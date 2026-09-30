# =============================================================================
#  NBA Contracts, Value & Performance Explorer  (single-file Shiny app: app.R)
# -----------------------------------------------------------------------------
#  Data
#    1. nba-contracts-contracts.csv  (Spotrac contract export, provided)
#    2. Spotrac "Best Value" rankings (scraped):
#         https://www.spotrac.com/nba/rankings/best-value/_/year/2025
#    3. Basketball-Reference regular-season per-game stats (scraped):
#         https://www.basketball-reference.com/leagues/NBA_2026_per_game.html
#
#  Tabs (the four insights + data notes)
#    Map 1  Contract Universe  - packed bubble chart: League > Teams > Players (plotly)
#    Map 2  Salary vs Value    - AAV (x) vs Spotrac best value (y), team colours
#                                 (ggplot in plotOutput("myplot") + plotly version)
#    Map 3  Build a Lineup     - 5 player slots -> reactable (AAV, <=5 stats,
#                                 cumulative AAV, averaged stats) + radar chart
#    Map 4  Cap Commitments    - team x season committed-AAV heatmap (plotly) + gt
#    About the Data            - sourcing status and modifications
#
#  Deploying to Posit Connect Cloud
#    - Put app.R and nba-contracts-contracts.csv in the same folder / GitHub repo.
#    - Run the app locally ONCE: it scrapes both websites and caches the results
#      in data_cache/*.rds. Commit that folder so the cloud app never has to
#      scrape (both sites rate-limit or block cloud IPs).
#    - Every package below is on CRAN, so Connect Cloud's automatic dependency
#      scan can install them (no renv.lock / manifest.json required).
# =============================================================================

library(shiny)
options(shiny.sanitize.errors = FALSE)   # show real error messages in the browser

# Everything that runs at startup is wrapped in tryCatch(): if anything fails,
# the app still starts and displays the error instead of "Unable to start".
startup_error <- NULL
startup_ok <- tryCatch({

suppressPackageStartupMessages({
  library(robotstxt)
  library(tidyverse)
  library(readxl)
  library(zoo)
  library(rvest)
  library(janitor)
  library(polite)
  library(reactable)
  library(gt)
  library(plotly)
  library(RColorBrewer)
  library(slider)
  # ggradar (GitHub-only), Lahman and fastRhockey were removed so the app can be
  # installed from CRAN on Posit Connect Cloud. radar_plot() below replaces ggradar.
})

# ---- Settings ---------------------------------------------------------------
CSV_PATH      <- list.files(".", pattern = "nba.contracts.*\\.csv$", ignore.case = TRUE,
                            recursive = TRUE, full.names = TRUE)[1]
if (is.na(CSV_PATH)) stop("Cannot find nba-contracts-contracts.csv. Files deployed: ",
                          paste(list.files(".", recursive = TRUE), collapse = ", "))
message("[startup] reading ", CSV_PATH)
httr::set_config(httr::timeout(20))   # never let a blocked website hang startup
SEASON_REF    <- 2026   # contract season in effect (2026-27 season)
STATS_SEASON  <- 2026   # Basketball-Reference NBA_2026 = 2025-26 regular season
SPOTRAC_URL   <- "https://www.spotrac.com/nba/rankings/best-value/_/year/2025"
BBREF_URL     <- sprintf("https://www.basketball-reference.com/leagues/NBA_%d_per_game.html",
                         STATS_SEASON)
cache_files   <- list.files(".", pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
CACHE_DIR     <- if (dir.exists("data_cache")) "data_cache" else
                 if (length(cache_files)) dirname(cache_files[1]) else "data_cache"
USER_AGENT    <- "NBA contracts class project (R polite package)"

# ---- NBA team colours (primary / secondary) ---------------------------------
team_colors <- tribble(
  ~team, ~team_name,                ~primary,  ~secondary,
  "ATL", "Atlanta Hawks",           "#E03A3E", "#C1D32F",
  "BOS", "Boston Celtics",          "#007A33", "#BA9653",
  "BKN", "Brooklyn Nets",           "#000000", "#AAAAAA",
  "CHA", "Charlotte Hornets",       "#1D1160", "#00788C",
  "CHI", "Chicago Bulls",           "#CE1141", "#000000",
  "CLE", "Cleveland Cavaliers",     "#860038", "#FDBB30",
  "DAL", "Dallas Mavericks",        "#00538C", "#B8C4CA",
  "DEN", "Denver Nuggets",          "#0E2240", "#FEC524",
  "DET", "Detroit Pistons",         "#C8102E", "#1D42BA",
  "GSW", "Golden State Warriors",   "#1D428A", "#FFC72C",
  "HOU", "Houston Rockets",         "#CE1141", "#C4CED4",
  "IND", "Indiana Pacers",          "#002D62", "#FDBB30",
  "LAC", "LA Clippers",             "#C8102E", "#1D428A",
  "LAL", "Los Angeles Lakers",      "#552583", "#FDB927",
  "MEM", "Memphis Grizzlies",       "#5D76A9", "#12173F",
  "MIA", "Miami Heat",              "#98002E", "#F9A01B",
  "MIL", "Milwaukee Bucks",         "#00471B", "#EEE1C6",
  "MIN", "Minnesota Timberwolves",  "#0C2340", "#236192",
  "NOP", "New Orleans Pelicans",    "#0C2340", "#C8102E",
  "NYK", "New York Knicks",         "#006BB6", "#F58426",
  "OKC", "Oklahoma City Thunder",   "#007AC1", "#EF3B24",
  "ORL", "Orlando Magic",           "#0077C0", "#C4CED4",
  "PHI", "Philadelphia 76ers",      "#006BB6", "#ED174C",
  "PHX", "Phoenix Suns",            "#1D1160", "#E56020",
  "POR", "Portland Trail Blazers",  "#E03A3E", "#000000",
  "SAC", "Sacramento Kings",        "#5A2D81", "#63727A",
  "SAS", "San Antonio Spurs",       "#8A8D8F", "#000000",
  "TOR", "Toronto Raptors",         "#CE1141", "#000000",
  "UTA", "Utah Jazz",               "#002B5C", "#F9A01B",
  "WAS", "Washington Wizards",      "#002B5C", "#E31837"
)
team_pal <- setNames(team_colors$primary, team_colors$team)

# ---- Helpers ----------------------------------------------------------------
# Name key used to join the three sources (accents, punctuation, suffixes)
name_key <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_remove_all("[\\.'\u2019,*]") |>
    str_replace_all("-", " ") |>
    str_squish() |>
    str_remove("\\s+(jr|sr|ii|iii|iv|v)$")
}

money_m <- function(x, digits = 1) {
  ifelse(is.na(x), "-", paste0(ifelse(x < 0, "-$", "$"),
                               formatC(abs(x) / 1e6, format = "f", digits = digits), "M"))
}

hex_alpha <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1, ], rgb[2, ], rgb[3, ], alpha)
}

# Scrape once, then reuse data_cache/<name>.rds
cached_scrape <- function(name, scrape_fun) {
  path <- file.path(CACHE_DIR, paste0(name, ".rds"))
  if (file.exists(path)) {
    return(list(data = readRDS(path), status = "loaded from cache"))
  }
  err <- NULL
  res <- tryCatch(scrape_fun(), error = function(e) { err <<- conditionMessage(e); NULL })
  if (!is.null(res) && nrow(res) > 0) {
    try({ dir.create(CACHE_DIR, showWarnings = FALSE); saveRDS(res, path) }, silent = TRUE)
    return(list(data = res, status = "scraped live"))
  }
  list(data = NULL, status = paste("scrape failed:", err %||% "no rows returned"))
}

check_robots <- function(url) {
  ok <- tryCatch(suppressMessages(robotstxt::paths_allowed(url)), error = function(e) NA)
  if (isFALSE(ok)) stop("robots.txt disallows ", url)
  invisible(TRUE)
}

# Radar chart in plain ggplot2 (drop-in replacement for ggradar)
#   df: first column = group name, other columns = values scaled 0-1
radar_plot <- function(df, colours, title = NULL) {
  vars <- names(df)[-1]; k <- length(vars)
  ang  <- pi / 2 - 2 * pi * (seq_len(k) - 1) / k
  long <- df |>
    rename(group = 1) |>
    pivot_longer(-group, names_to = "stat", values_to = "v") |>
    mutate(i = match(stat, vars), x = v * cos(ang[i]), y = v * sin(ang[i])) |>
    arrange(group, i)
  closed <- bind_rows(long, long |> group_by(group) |> slice(1) |> ungroup())
  grid   <- expand_grid(level = c(0.25, 0.5, 0.75, 1), i = c(seq_len(k), 1)) |>
    mutate(x = level * cos(ang[i]), y = level * sin(ang[i]))
  ggplot() +
    geom_path(data = grid, aes(x, y, group = level), colour = "grey82") +
    geom_segment(data = tibble(x = cos(ang), y = sin(ang)),
                 aes(x = 0, y = 0, xend = x, yend = y), colour = "grey82") +
    geom_polygon(data = closed, aes(x, y, group = group, fill = group), alpha = 0.12) +
    geom_path(data = closed, aes(x, y, group = group, colour = group), linewidth = 1.2) +
    geom_point(data = long, aes(x, y, colour = group), size = 3) +
    geom_text(data = tibble(stat = vars, x = 1.2 * cos(ang), y = 1.2 * sin(ang)),
              aes(x, y, label = stat), size = 5, fontface = "bold") +
    annotate("text", x = 0.03, y = c(0.5, 1), label = c("50%", "league max"),
             hjust = 0, size = 3.5, colour = "grey50") +
    scale_colour_manual(values = colours) + scale_fill_manual(values = colours) +
    guides(colour = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
    coord_equal(clip = "off") +
    labs(title = title, colour = NULL, fill = NULL) +
    theme_void(base_size = 14) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
          plot.margin = margin(20, 40, 10, 40))
}

# ---- 1. Contracts CSV -------------------------------------------------------
contracts_all <- read_csv(CSV_PATH, show_col_types = FALSE) |>
  clean_names() |>
  rename(team_raw       = starts_with("team"),
         age_at_signing = starts_with("age"),
         cash_2yr       = matches("2_year"),
         cash_3yr       = matches("3_year")) |>
  transmute(
    player         = str_squish(player),
    pos            = str_squish(pos),
    team           = word(str_squish(team_raw), 1),
    age_at_signing = as.numeric(age_at_signing),
    start, end, yrs,
    total_value    = parse_number(as.character(value)),
    aav_listed     = parse_number(as.character(aav)),
    cash_2yr       = parse_number(as.character(cash_2yr))
  ) |>
  mutate(
    aav_imputed = is.na(aav_listed) & !is.na(cash_2yr),
    aav         = coalesce(aav_listed, cash_2yr / pmin(yrs, 2)),
    total_value = coalesce(total_value, aav * yrs)
  )

# One row per player: the contract covering SEASON_REF, else the newest one
contracts <- contracts_all |>
  mutate(active_ref = start <= SEASON_REF & end >= SEASON_REF) |>
  arrange(player, desc(active_ref), desc(start)) |>
  distinct(player, .keep_all = TRUE) |>
  filter(!is.na(aav), aav > 0) |>
  left_join(team_colors, by = "team") |>
  mutate(
    key       = name_key(player),
    team_name = coalesce(team_name, team),
    primary   = coalesce(primary, "#888888"),
    secondary = coalesce(secondary, "#CCCCCC")
  )

# ---- 2. Basketball-Reference per-game stats --------------------------------
STAT_CHOICES <- c("Points" = "pts", "Rebounds" = "trb", "Assists" = "ast",
                  "Steals" = "stl", "Blocks" = "blk", "Minutes" = "mp",
                  "3PT made" = "x3p", "Off. rebounds" = "orb", "Turnovers" = "tov")

scrape_bbref_stats <- function() {
  check_robots(BBREF_URL)
  page <- polite::bow(BBREF_URL, user_agent = USER_AGENT, delay = 5) |> polite::scrape()
  if (is.null(page)) stop("empty response from basketball-reference")
  node <- html_element(page, "table#per_game_stats")
  if (inherits(node, "xml_missing")) node <- html_element(page, "table")
  tbl <- html_table(node, convert = FALSE) |> clean_names()
  if ("tm" %in% names(tbl)) tbl <- rename(tbl, team = tm)
  num_cols <- intersect(c("age", "g", "gs", unname(STAT_CHOICES)), names(tbl))
  tbl |>
    filter(player != "Player", player != "", !str_detect(player, "League Average")) |>
    mutate(player = str_remove(player, "\\*$"),
           across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x)))) |>
    distinct(player, .keep_all = TRUE) |>        # 1st row = season total for traded players
    select(player, bbref_team = team, all_of(num_cols)) |>
    mutate(key = name_key(player)) |>
    distinct(key, .keep_all = TRUE)
}

bbref <- cached_scrape(paste0("bbref_per_game_", STATS_SEASON), scrape_bbref_stats)
stats_ok <- !is.null(bbref$data)

stats_df <- if (stats_ok) {
  contracts |> inner_join(select(bbref$data, -player), by = "key")
} else {
  contracts |> mutate(!!!setNames(rep(list(NA_real_), length(STAT_CHOICES) + 1),
                                  c(unname(STAT_CHOICES), "g")))
}

# ---- 3. Spotrac best-value rankings ----------------------------------------
extract_money <- function(s) {
  m <- str_extract_all(s, "-?\\$\\s?-?[0-9][0-9,]*(\\.[0-9]+)?\\s?[MmKk]?")[[1]]
  if (!length(m)) m <- str_extract_all(s, "-?[0-9][0-9,]*\\.[0-9]+")[[1]]
  if (!length(m)) return(NA_real_)
  v    <- tail(m, 1)
  mult <- if (str_detect(v, "[Mm]$")) 1e6 else if (str_detect(v, "[Kk]$")) 1e3 else 1
  sgn  <- if (str_detect(v, "^-|\\$\\s?-")) -1 else 1
  sgn * readr::parse_number(str_remove_all(v, "[^0-9.,]")) * mult
}

scrape_spotrac_value <- function() {
  check_robots(SPOTRAC_URL)
  page <- polite::bow(SPOTRAC_URL, user_agent = USER_AGENT, delay = 5) |> polite::scrape()
  if (is.null(page)) stop("empty response from spotrac")

  out <- NULL
  # (a) HTML table layout
  for (t in html_table(page)) {
    t <- clean_names(t)
    pcol <- names(t)[str_detect(names(t), "player|name")][1]
    if (is.na(pcol)) next
    numeric_share <- map_dbl(t, ~ mean(!is.na(suppressWarnings(parse_number(as.character(.x))))))
    vcol <- tail(setdiff(names(t)[numeric_share > 0.8], c("rank", "rk", "x", pcol)), 1)
    if (!length(vcol)) next
    out <- tibble(player = as.character(t[[pcol]]),
                  spotrac_value = map_dbl(as.character(t[[vcol]]), extract_money))
    break
  }
  # (b) ranking list layout
  if (is.null(out)) {
    items <- html_elements(page, "li.list-group-item")
    if (!length(items)) items <- html_elements(page, ".list-group-item, .ranking-item")
    out <- tibble(
      player        = items |> html_element("a") |> html_text2(),
      spotrac_value = map_dbl(html_text2(items), extract_money)
    )
  }
  out <- out |>
    mutate(player = str_squish(str_remove(player, "\\n.*$"))) |>
    filter(!is.na(player), player != "", !is.na(spotrac_value)) |>
    mutate(key = name_key(player)) |>
    distinct(key, .keep_all = TRUE)
  if (nrow(out) < 5) stop("could not parse the ranking list")
  out
}

spotrac <- cached_scrape("spotrac_best_value_2025", scrape_spotrac_value)

# Fallback when Spotrac cannot be scraped: production-based surplus value
#   production = (PTS + 1.2 REB + 1.5 AST + 3 STL + 3 BLK) x games
#   value      = player's share of league production x league AAV pool - AAV
production_value <- function(df) {
  df |>
    mutate(prod = (pts + 1.2 * trb + 1.5 * ast + 3 * stl + 3 * blk) * g) |>
    filter(!is.na(prod)) |>
    mutate(best_value = prod / sum(prod) * sum(aav) - aav) |>
    select(-prod)
}

if (!is.null(spotrac$data)) {
  value_df <- contracts |>
    inner_join(select(spotrac$data, key, best_value = spotrac_value), by = "key")
  value_source <- "Spotrac best-value ranking (2025)"
} else if (stats_ok) {
  value_df     <- production_value(stats_df)
  value_source <- "Estimated surplus value (production share x league AAV pool - AAV)"
} else {
  value_df     <- contracts |> mutate(best_value = NA_real_) |> filter(FALSE)
  value_source <- "unavailable (both scrapes failed)"
}
value_df <- value_df |>
  arrange(desc(best_value)) |>
  mutate(value_rank = row_number(),
         hover = paste0("<b>", player, "</b> (", team, ", ", pos, ")",
                        "<br>AAV: ", money_m(aav),
                        "<br>Best value: ", money_m(best_value),
                        "<br>Value rank: #", value_rank))

# ---- 4. Circle packing for Map 1 (pure R, greedy tangent placement) ---------
pack_circles <- function(r, gap = 0, n_angles = 72) {
  n <- length(r); x <- y <- rep(NA_real_, n)
  theta  <- seq(0, 2 * pi, length.out = n_angles + 1)[-1]
  placed <- integer(0)
  for (i in order(r, decreasing = TRUE)) {
    if (length(placed) == 0) {
      x[i] <- 0; y[i] <- 0
    } else {
      need <- r[placed] + r[i] + gap
      cx <- as.vector(outer(cos(theta), need) + rep(x[placed], each = n_angles))
      cy <- as.vector(outer(sin(theta), need) + rep(y[placed], each = n_angles))
      d2 <- outer(cx, x[placed], "-")^2 + outer(cy, y[placed], "-")^2
      clash <- sweep(d2, 2, (need - 1e-6)^2, "<")
      score <- sqrt(cx^2 + cy^2)
      score[rowSums(clash) > 0] <- Inf
      b <- which.min(score)
      x[i] <- cx[b]; y[i] <- cy[b]
    }
    placed <- c(placed, i)
  }
  x0 <- (max(x + r) + min(x - r)) / 2
  y0 <- (max(y + r) + min(y - r)) / 2
  x <- x - x0; y <- y - y0
  list(x = x, y = y, R = max(sqrt(x^2 + y^2) + r))
}

circle_path <- function(x, y, r, n = 64) {
  t <- c(seq(0, 2 * pi, length.out = n), NA)
  tibble(x = as.vector(outer(cos(t), r) + rep(x, each = n + 1)),
         y = as.vector(outer(sin(t), r) + rep(y, each = n + 1)))
}

build_pack <- function(df) {
  players <- df |>
    mutate(r = sqrt(aav / 1e6)) |>             # bubble AREA proportional to AAV
    group_by(team) |>
    group_modify(function(d, k) {
      p <- pack_circles(d$r, gap = 0.04)
      mutate(d, lx = p$x, ly = p$y, inner_R = p$R)
    }) |>
    ungroup()
  teams <- players |>
    group_by(team, team_name, primary, secondary) |>
    summarise(R = first(inner_R) * 1.05 + 0.2, payroll = sum(aav), n = n(),
              top_player = player[which.max(aav)], .groups = "drop")
  tp <- pack_circles(teams$R, gap = 0.6)
  teams <- mutate(teams, tx = tp$x, ty = tp$y)
  players <- players |>
    left_join(select(teams, team, tx, ty), by = "team") |>
    mutate(x = tx + lx, y = ty + ly,
           hover = paste0("<b>", player, "</b> - ", team_name,
                          "<br>", pos, " | ", start, "-", end, " (", yrs, " yrs)",
                          "<br>AAV: ", money_m(aav, 2),
                          ifelse(aav_imputed, " (est.)", ""),
                          "<br>Total value: ", money_m(total_value, 1),
                          "<br><i>click to open the ", team, " bubble</i>"))
  list(players = players, teams = teams, outer_R = tp$R + 1.2)
}

pack <- build_pack(contracts)

bubble_plot <- function(pk, selected = NULL) {
  p <- plot_ly(source = "bubble") |>
    add_trace(data = circle_path(0, 0, pk$outer_R, 240), x = ~x, y = ~y,
              type = "scatter", mode = "lines", fill = "toself",
              fillcolor = "rgba(120,120,120,0.08)",
              line = list(color = "#555555", width = 2),
              hoverinfo = "skip", showlegend = FALSE)
  for (i in seq_len(nrow(pk$teams))) {
    tm  <- pk$teams[i, ]
    dim <- !is.null(selected) && tm$team != selected
    p <- p |>
      add_trace(data = circle_path(tm$tx, tm$ty, tm$R, 90), x = ~x, y = ~y,
                type = "scatter", mode = "lines", fill = "toself",
                fillcolor = hex_alpha(tm$primary, if (dim) 0.25 else 0.9),
                line = list(color = tm$secondary, width = if (dim) 1 else 2.5),
                hoverinfo = "skip", showlegend = FALSE)
  }
  pl <- pk$players
  p |>
    add_trace(data = circle_path(pl$x, pl$y, pl$r, 36), x = ~x, y = ~y,
              type = "scatter", mode = "lines", fill = "toself",
              fillcolor = "rgba(255,255,255,0.30)",
              line = list(color = "rgba(255,255,255,0.85)", width = 0.8),
              hoverinfo = "skip", showlegend = FALSE) |>
    add_trace(data = pl, x = ~x, y = ~y, type = "scatter", mode = "markers",
              marker = list(size = 8, opacity = 0), customdata = ~team,
              text = ~hover, hoverinfo = "text",
              hoverlabel = list(bgcolor = pl$primary, font = list(color = "white")),
              showlegend = FALSE) |>
    add_trace(data = pk$teams, x = ~tx, y = ~ty + R + 0.55, type = "scatter",
              mode = "text", text = ~team, hoverinfo = "skip", showlegend = FALSE,
              textfont = list(color = pk$teams$primary, size = 12, family = "Arial Black")) |>
    layout(
      xaxis = list(visible = FALSE, scaleanchor = "y"),
      yaxis = list(visible = FALSE),
      hovermode = "closest", hoverdistance = -1,
      margin = list(l = 0, r = 0, t = 40, b = 0),
      title = list(text = sprintf("NBA: %s in AAV across %d players (bubble area = AAV)",
                                  money_m(sum(pl$aav) * 1e-3, 2) |> str_replace("M$", "B"),
                                  nrow(pl)), font = list(size = 15)),
      plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)"
    ) |>
    config(displaylogo = FALSE) |>
    event_register("plotly_click")
}

team_bubble_plot <- function(pk, tm_code) {
  pl <- filter(pk$players, team == tm_code)
  tm <- filter(pk$teams, team == tm_code)
  lab <- ifelse(pl$r > 2.2, paste0(word(pl$player, -1), "<br>", money_m(pl$aav)),
                ifelse(pl$r > 1.3, word(pl$player, -1), ""))
  plot_ly() |>
    add_trace(data = circle_path(0, 0, tm$R, 120), x = ~x, y = ~y, type = "scatter",
              mode = "lines", fill = "toself", fillcolor = hex_alpha(tm$primary, 0.95),
              line = list(color = tm$secondary, width = 4), hoverinfo = "skip") |>
    add_trace(data = circle_path(pl$lx, pl$ly, pl$r, 48), x = ~x, y = ~y,
              type = "scatter", mode = "lines", fill = "toself",
              fillcolor = hex_alpha(tm$secondary, 0.55),
              line = list(color = "white", width = 1.2), hoverinfo = "skip") |>
    add_trace(data = pl, x = ~lx, y = ~ly, type = "scatter", mode = "markers+text",
              text = lab, textfont = list(color = "white", size = 11),
              hovertext = ~hover, hoverinfo = "text",
              marker = list(size = 8, opacity = 0)) |>
    layout(showlegend = FALSE,
           xaxis = list(visible = FALSE, scaleanchor = "y"),
           yaxis = list(visible = FALSE),
           hovermode = "closest", margin = list(l = 0, r = 0, t = 30, b = 0),
           title = list(text = paste0(tm$team_name, " - ", money_m(tm$payroll), " total AAV"),
                        font = list(size = 13))) |>
    config(displaylogo = FALSE)
}

# ---- 5. Cap commitments for Map 4 (uses ALL contracts incl. extensions) -----
commit_seasons <- SEASON_REF:(SEASON_REF + 4)
commitments <- contracts_all |>
  filter(!is.na(aav)) |>
  crossing(season = commit_seasons) |>
  filter(start <= season, end >= season) |>
  group_by(player, season) |>
  slice_max(start, n = 1, with_ties = FALSE) |>      # extension replaces old deal
  ungroup() |>
  group_by(team, season) |>
  summarise(committed = sum(aav), n_players = n(), .groups = "drop") |>
  complete(team = team_colors$team, season = commit_seasons,
           fill = list(committed = 0, n_players = 0))

# ---- Text for "About the data" ---------------------------------------------
n_dupe_players <- contracts_all |> count(player) |> filter(n > 1) |> nrow()
n_dropped      <- n_distinct(contracts_all$player) - nrow(contracts)
n_imputed      <- sum(contracts$aav_imputed)
match_stats    <- if (stats_ok) sum(contracts$key %in% bbref$data$key) else 0
match_spotrac  <- if (!is.null(spotrac$data)) sum(contracts$key %in% spotrac$data$key) else 0

lineup_choices  <- sort(unique(if (stats_ok) stats_df$player else contracts$player))
default_lineup  <- (if (stats_ok) stats_df else contracts) |>
  arrange(desc(aav)) |> slice_head(n = 5) |> pull(player)

insight_box <- function(...) {
  div(style = "background:#f4f6fa;border-left:5px solid #1D428A;padding:10px 14px;margin:8px 0 14px 0;",
      ...)
}


message("[startup] data ready: ", nrow(contracts), " players | stats: ", bbref$status,
        " | spotrac: ", spotrac$status)
TRUE
}, error = function(e) {
  startup_error <<- conditionMessage(e)
  message("[startup] ERROR: ", startup_error)
  FALSE
})

# =============================================================================
#  UI
# =============================================================================
ui <- function(req) navbarPage(
  title = "NBA Contracts & Value Explorer",
  header = tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Arial, sans-serif; }
    .navbar-default { background:#17408B; border-color:#17408B; }
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a { color:#fff !important; }
    .navbar-default .navbar-nav > .active > a { background:#C9082A !important; }
    .player-card { border:2px solid #ddd; border-radius:10px; padding:10px; margin-top:10px; }
  "))),

  # ---- Map 1 ----------------------------------------------------------------
  tabPanel("1 · Contract Universe",
    fluidPage(
      insight_box(strong("Insight 1 - "), textOutput("insight1", inline = TRUE)),
      fluidRow(
        column(7, plotlyOutput("bubble", height = "760px")),
        column(5,
          h4(textOutput("team_title")),
          actionButton("reset_team", "Show all teams", icon = icon("rotate-left")),
          br(), br(),
          conditionalPanel("output.team_selected == true",
                           plotlyOutput("team_bubble", height = "380px")),
          reactableOutput("team_table")
        )
      )
    )
  ),

  # ---- Map 2 ----------------------------------------------------------------
  tabPanel("2 · Salary vs Value",
    sidebarLayout(
      sidebarPanel(width = 3,
        sliderInput("num", "Choose a number",
                    min = 0, max = 1000000000, value = 100),
        helpText("Minimum AAV in US$: players paid less than this are removed."),
        hr(),
        sliderInput("n_players", "Present players one at a time (best value first)",
                    min = 1, max = max(1, nrow(value_df)), value = min(15, max(1, nrow(value_df))),
                    step = 1, animate = animationOptions(interval = 900)),
        helpText("Press \u25B6 to add the next-best-value player to the chart."),
        uiOutput("newest_player")
      ),
      mainPanel(width = 9,
        insight_box(strong("Insight 2 - "), textOutput("insight2", inline = TRUE)),
        plotOutput("myplot", height = "480px"),
        h4("Interactive version (all players above the AAV threshold)"),
        plotlyOutput("value_plotly", height = "560px")
      )
    )
  ),

  # ---- Map 3 ----------------------------------------------------------------
  tabPanel("3 · Build a Lineup",
    fluidPage(
      insight_box(strong("Insight 3 - "),
        "Stack any five players and see what the lineup costs against what it produced.
         The table adds up the contract AAVs slot by slot and averages the regular-season stats."),
      fluidRow(
        lapply(1:5, function(i) column(2,
          selectizeInput(paste0("slot", i), paste("Slot", i),
                         choices = c("", lineup_choices),
                         selected = default_lineup[i] %||% "",
                         options = list(placeholder = "Pick a player")))),
        column(2, selectizeInput("stat_vars", "Stats (max 5)",
                                 choices = STAT_CHOICES,
                                 selected = c("pts", "trb", "ast", "stl", "blk"),
                                 multiple = TRUE, options = list(maxItems = 5)))
      ),
      reactableOutput("lineup_table"),
      br(),
      fluidRow(
        column(7, plotOutput("radar", height = "470px")),
        column(5, uiOutput("lineup_summary"))
      )
    )
  ),

  # ---- Map 4 ----------------------------------------------------------------
  tabPanel("4 · Cap Commitments",
    fluidPage(
      insight_box(strong("Insight 4 - "), textOutput("insight4", inline = TRUE)),
      plotlyOutput("commit_heat", height = "720px"),
      br(),
      gt_output("value_gt")
    )
  ),

  # ---- About ----------------------------------------------------------------
  tabPanel("About the Data", fluidPage(uiOutput("about")))
)

# =============================================================================
#  Server
# =============================================================================
server <- function(input, output, session) {

  # ---- Map 1 ----------------------------------------------------------------
  selected_team <- reactiveVal(NULL)

  observeEvent(event_data("plotly_click", source = "bubble"), {
    d <- event_data("plotly_click", source = "bubble")
    if (!is.null(d$customdata)) selected_team(as.character(d$customdata[[1]]))
  })
  observeEvent(input$team_pick,  selected_team(input$team_pick))
  observeEvent(input$reset_team, selected_team(NULL))

  output$team_selected <- reactive(!is.null(selected_team()))
  outputOptions(output, "team_selected", suspendWhenHidden = FALSE)

  output$insight1 <- renderText({
    top <- arrange(pack$teams, desc(payroll))
    sprintf("%s carry the biggest bubble (%s across %d contracts) while %s is the smallest (%s).
             The top 10%% of contracts account for %.0f%% of all AAV dollars - salary is highly concentrated.",
            top$team_name[1], money_m(top$payroll[1]), top$n[1],
            last(top$team_name), money_m(last(top$payroll)),
            100 * sum(sort(contracts$aav, decreasing = TRUE)[1:ceiling(nrow(contracts) / 10)]) /
              sum(contracts$aav))
  })

  output$bubble <- renderPlotly(bubble_plot(pack, selected_team()))

  output$team_title <- renderText({
    if (is.null(selected_team())) "All teams - click a bubble or a row to open a team"
    else paste0(filter(pack$teams, team == selected_team())$team_name, " roster contracts")
  })

  output$team_bubble <- renderPlotly({
    req(selected_team())
    team_bubble_plot(pack, selected_team())
  })

  output$team_table <- renderReactable({
    if (is.null(selected_team())) {
      pack$teams |>
        arrange(desc(payroll)) |>
        transmute(team, team_name, contracts = n, payroll, avg_aav = payroll / n, top_player) |>
        reactable(
          compact = TRUE, highlight = TRUE, defaultPageSize = 15, searchable = TRUE,
          onClick = htmlwidgets::JS(
            "function(rowInfo) { Shiny.setInputValue('team_pick', rowInfo.row.team, {priority: 'event'}); }"),
          rowStyle = list(cursor = "pointer"),
          columns = list(
            team = colDef(name = "", width = 60, style = function(v) list(
              background = team_pal[[v]], color = "white", fontWeight = "bold")),
            team_name  = colDef(name = "Team"),
            contracts  = colDef(name = "#", width = 45),
            payroll    = colDef(name = "Total AAV", format = colFormat(currency = "USD", separators = TRUE, digits = 0)),
            avg_aav    = colDef(name = "Avg AAV", format = colFormat(currency = "USD", separators = TRUE, digits = 0)),
            top_player = colDef(name = "Top contract")
          ))
    } else {
      tm  <- selected_team()
      col <- team_pal[[tm]] %||% "#888888"
      d   <- pack$players |> filter(team == tm) |> arrange(desc(aav)) |>
        transmute(player, pos, age_at_signing, term = paste0(start, "-", end), yrs, aav, total_value)
      mx  <- max(d$aav)
      reactable(d, compact = TRUE, highlight = TRUE, defaultPageSize = 20,
        columns = list(
          player = colDef(name = "Player", minWidth = 140),
          pos = colDef(name = "Pos", width = 50),
          age_at_signing = colDef(name = "Age@sign", width = 75),
          term = colDef(name = "Term", width = 90),
          yrs = colDef(name = "Yrs", width = 45),
          aav = colDef(name = "AAV", minWidth = 150, cell = function(v) {
            div(style = "display:flex;align-items:center;gap:6px;",
                div(style = sprintf("height:12px;width:%.0f%%;background:%s;border-radius:2px;",
                                    100 * v / mx, col)),
                span(money_m(v, 1)))
          }),
          total_value = colDef(name = "Total value",
                               format = colFormat(currency = "USD", separators = TRUE, digits = 0))
        ))
    }
  })

  # ---- Map 2 ----------------------------------------------------------------
  filtered_value <- reactive({ filter(value_df, aav >= input$num) })

  observeEvent(filtered_value(), {
    n <- max(1, nrow(filtered_value()))
    updateSliderInput(session, "n_players", max = n, value = min(input$n_players, n))
  })

  shown <- reactive({
    f <- filtered_value()
    validate(need(nrow(f) > 0, "No players with value data above this AAV threshold."))
    f |> arrange(value_rank) |> slice_head(n = min(input$n_players, nrow(f)))
  })

  output$insight2 <- renderText({
    validate(need(nrow(value_df) > 0, "Value data unavailable - see 'About the Data'."))
    r <- suppressWarnings(cor(value_df$aav, value_df$best_value, method = "spearman"))
    b <- value_df |> filter(aav < median(aav)) |> slice_max(best_value, n = 1)
    sprintf("Salary and value are only loosely related (Spearman rho = %.2f). The best bargain below
             the median salary is %s (%s AAV, %s value). Y-axis: %s.",
            r, b$player, money_m(b$aav), money_m(b$best_value), value_source)
  })

  output$myplot <- renderPlot({
    s      <- shown()
    newest <- slice_tail(s, n = 1)
    trend  <- filtered_value() |> arrange(aav) |>
      mutate(roll = slider::slide_dbl(best_value, median, .before = 12, .after = 12))
    ggplot(s, aes(aav, best_value)) +
      geom_hline(yintercept = 0, colour = "grey70") +
      geom_line(data = trend, aes(y = roll), linetype = "dashed", colour = "grey45", linewidth = 0.8) +
      geom_point(aes(colour = primary), size = 3.5, alpha = 0.85) +
      geom_point(data = newest, aes(fill = primary, colour = secondary),
                 shape = 21, size = 9, stroke = 2.5) +
      geom_label(data = newest, aes(label = paste0("#", value_rank, " ", player, " (", team, ")")),
                 vjust = -1.1, size = 4.5, fontface = "bold") +
      scale_colour_identity() + scale_fill_identity() +
      scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
      scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M"),
                         expand = expansion(mult = c(0.05, 0.18))) +
      labs(title = sprintf("Top %d value players (AAV >= %s)", nrow(s), scales::dollar(input$num)),
           subtitle = "Colour = team primary colour | dashed line = rolling median value (slider::slide_dbl, +/-12 players)",
           x = "Contract Average Annual Value (AAV)", y = value_source) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })

  output$newest_player <- renderUI({
    n <- slice_tail(shown(), n = 1)
    div(class = "player-card", style = sprintf("border-color:%s;", n$primary),
        h4(style = sprintf("color:%s;margin-top:0;", n$primary), n$player),
        p(n$team_name, " - ", n$pos),
        p(strong("AAV: "), money_m(n$aav, 2)), p(strong("Value: "), money_m(n$best_value, 2)),
        p(strong("Value rank: "), paste0("#", n$value_rank)))
  })

  output$value_plotly <- renderPlotly({
    f <- filtered_value()
    validate(need(nrow(f) > 0, "No data above this threshold."))
    mx <- median(f$aav); my <- median(f$best_value)
    rx <- range(f$aav);  ry <- range(f$best_value)
    plot_ly(f, x = ~aav, y = ~best_value, color = ~team, colors = team_pal,
            type = "scatter", mode = "markers", text = ~hover, hoverinfo = "text",
            marker = list(size = 11, line = list(color = "white", width = 1), opacity = 0.85)) |>
      layout(
        xaxis = list(title = "AAV (US$)", tickformat = "$.2s"),
        yaxis = list(title = value_source, tickformat = "$.2s"),
        shapes = list(
          list(type = "line", x0 = mx, x1 = mx, y0 = ry[1], y1 = ry[2], line = list(dash = "dot", color = "grey")),
          list(type = "line", x0 = rx[1], x1 = rx[2], y0 = my, y1 = my, line = list(dash = "dot", color = "grey"))),
        annotations = list(
          list(x = rx[1], y = ry[2], text = "<b>BARGAINS</b><br>cheap + high value", showarrow = FALSE, xanchor = "left", font = list(color = "#007A33")),
          list(x = rx[2], y = ry[2], text = "<b>WORTH IT</b><br>paid + high value", showarrow = FALSE, xanchor = "right", font = list(color = "#1D428A")),
          list(x = rx[2], y = ry[1], text = "<b>OVERPAID</b>", showarrow = FALSE, xanchor = "right", font = list(color = "#C8102E"))),
        legend = list(title = list(text = "Team"))
      ) |>
      config(displaylogo = FALSE)
  })

  # ---- Map 3 ----------------------------------------------------------------
  lineup <- reactive({
    picks <- map_chr(1:5, ~ input[[paste0("slot", .x)]] %||% "")
    tibble(slot = 1:5, player = picks) |>
      filter(player != "") |>
      left_join(stats_df, by = "player")
  })

  stat_vars <- reactive({
    v <- input$stat_vars
    validate(need(length(v) >= 1, "Pick at least one statistic."))
    v
  })

  output$lineup_table <- renderReactable({
    lu <- lineup(); sv <- stat_vars()
    validate(need(nrow(lu) > 0, "Add a player to a slot to build the table."))
    lbl <- setNames(names(STAT_CHOICES), STAT_CHOICES)
    tbl <- lu |>
      transmute(slot, player, team, aav, across(all_of(sv))) |>
      mutate(stat_avg = rowMeans(across(all_of(sv))),
             cum_aav  = cumsum(aav))
    usd <- colFormat(currency = "USD", separators = TRUE, digits = 0)
    stat_cols <- map(sv, function(s) colDef(
      name = lbl[[s]], format = colFormat(digits = 1),
      footer = function(values) sprintf("avg %.1f", mean(values, na.rm = TRUE))))
    names(stat_cols) <- sv
    reactable(
      tbl, compact = FALSE, highlight = TRUE, bordered = TRUE, pagination = FALSE,
      defaultColDef = colDef(footerStyle = list(fontWeight = "bold", background = "#f4f6fa")),
      columns = c(list(
        slot   = colDef(name = "Slot", width = 55, footer = ""),
        player = colDef(name = "Player", minWidth = 160, footer = "Lineup"),
        team   = colDef(name = "Team", width = 70, style = function(v) list(
          background = team_pal[[v]] %||% "#888", color = "white", fontWeight = "bold")),
        aav    = colDef(name = "AAV", format = usd, minWidth = 120,
                        footer = function(v) paste("total", money_m(sum(v), 2)))),
        stat_cols,
        list(
          stat_avg = colDef(name = "Avg of selected stats", format = colFormat(digits = 2),
                            minWidth = 120,
                            footer = function(v) sprintf("avg %.2f", mean(v, na.rm = TRUE))),
          cum_aav  = colDef(name = "Added contract value (cumulative AAV)", format = usd,
                            minWidth = 170,
                            footer = function(v) money_m(max(v), 2)))
      )
    )
  })

  output$radar <- renderPlot({
    lu <- distinct(lineup(), player, .keep_all = TRUE); sv <- stat_vars()
    validate(need(stats_ok, "Basketball-Reference stats unavailable."),
             need(length(sv) >= 3, "Pick at least 3 stats to draw the radar."),
             need(nrow(lu) >= 1, "Add players to compare."))
    lbl <- setNames(names(STAT_CHOICES), STAT_CHOICES)
    league_max <- map_dbl(sv, ~ max(stats_df[[.x]], na.rm = TRUE))
    radar_df <- lu |>
      select(player, all_of(sv)) |>
      mutate(across(all_of(sv), ~ replace_na(.x, 0))) |>
      mutate(across(all_of(sv), ~ .x / league_max[match(cur_column(), sv)])) |>
      rename_with(~ unname(lbl[.x]), all_of(sv))
    radar_plot(radar_df, colours = setNames(lu$primary, lu$player),
               title = "Lineup profile (each stat scaled to the league leader)")
  })

  output$lineup_summary <- renderUI({
    lu <- lineup(); sv <- stat_vars()
    req(nrow(lu) > 0)
    total <- sum(lu$aav, na.rm = TRUE)
    pts   <- sum(lu$pts, na.rm = TRUE)
    tagList(
      h3("Lineup economics"),
      p(strong("Total AAV: "), money_m(total, 2)),
      p(strong("Share of league AAV pool: "), sprintf("%.1f%%", 100 * total / sum(contracts$aav))),
      p(strong("Combined points per game: "), sprintf("%.1f", pts)),
      p(strong("Cost per point per game: "), if (pts > 0) money_m(total / pts, 2) else "-"),
      p(strong("Average of selected stats: "),
        sprintf("%.2f", mean(as.matrix(select(lu, all_of(sv))), na.rm = TRUE))),
      helpText("Stats: ", STATS_SEASON - 1, "-", substr(STATS_SEASON, 3, 4),
               " regular season per game, Basketball-Reference.")
    )
  })

  # ---- Map 4 ----------------------------------------------------------------
  output$insight4 <- renderText({
    cur  <- filter(commitments, season == SEASON_REF) |> arrange(desc(committed))
    far  <- filter(commitments, season == max(commit_seasons)) |> arrange(desc(committed))
    sprintf("%s have the most money committed for %d-%s (%s). Looking %d seasons out, %s are
             still locked in for %s - long extensions shape flexibility more than today's payroll.",
            filter(team_colors, team == cur$team[1])$team_name, SEASON_REF,
            substr(SEASON_REF + 1, 3, 4), money_m(cur$committed[1]),
            max(commit_seasons) - SEASON_REF,
            filter(team_colors, team == far$team[1])$team_name, money_m(far$committed[1]))
  })

  output$commit_heat <- renderPlotly({
    ord <- commitments |> filter(season == SEASON_REF) |> arrange(committed) |> pull(team)
    m   <- commitments |>
      mutate(team = factor(team, levels = ord),
             season_lab = paste0(season, "-", substr(season + 1, 3, 4)),
             txt = paste0("<b>", team, " ", season_lab, "</b><br>Committed: ",
                          money_m(committed), "<br>Players under contract: ", n_players))
    plot_ly(m, x = ~season_lab, y = ~team, z = ~committed / 1e6, type = "heatmap",
            text = ~txt, hoverinfo = "text",
            colors = colorRampPalette(brewer.pal(9, "YlOrRd"))(100),
            colorbar = list(title = "Committed<br>AAV ($M)")) |>
      add_annotations(data = m, x = ~season_lab, y = ~team,
                      text = ~ifelse(committed > 0, round(committed / 1e6), ""),
                      showarrow = FALSE, font = list(size = 10, color = "black")) |>
      layout(xaxis = list(title = "Season"), yaxis = list(title = ""),
             title = "Guaranteed-contract AAV already on the books, by team and season") |>
      config(displaylogo = FALSE)
  })

  output$value_gt <- render_gt({
    validate(need(stats_ok, "Basketball-Reference stats unavailable - value table skipped."))
    stats_df |>
      filter(g >= 40, mp >= 20) |>
      mutate(pra = pts + trb + ast, cost_per_pra = aav / pra) |>
      arrange(cost_per_pra) |>
      slice_head(n = 12) |>
      select(player, team, aav, g, pts, trb, ast, pra, cost_per_pra) |>
      gt() |>
      tab_header(title = md("**Best bang for the buck**"),
                 subtitle = "Lowest AAV per (points + rebounds + assists) per game - min. 40 games & 20 MPG") |>
      fmt_currency(columns = c(aav, cost_per_pra), decimals = 0) |>
      fmt_number(columns = c(pts, trb, ast, pra), decimals = 1) |>
      data_color(columns = cost_per_pra, palette = brewer.pal(9, "Greens"), reverse = TRUE) |>
      cols_label(player = "Player", team = "Team", aav = "AAV", g = "G", pts = "PTS",
                 trb = "REB", ast = "AST", pra = "PTS+REB+AST", cost_per_pra = "$ per PRA") |>
      tab_source_note("Sources: Spotrac contract export; Basketball-Reference per-game stats.")
  })

  # ---- About ----------------------------------------------------------------
  output$about <- renderUI({
    tagList(
      h2("Is the data completely sourced?"),
      p("Contracts come from the provided Spotrac export. Performance comes from Basketball-Reference and
         value rankings from Spotrac, both scraped with ", code("polite"), " (after a ",
        code("robotstxt"), " check) and cached in ", code(CACHE_DIR), "."),
      tags$table(class = "table table-condensed", style = "max-width:900px;",
        tags$tr(tags$th("Source"), tags$th("Status"), tags$th("Rows / matches")),
        tags$tr(tags$td("Contracts CSV"), tags$td("loaded"),
                tags$td(sprintf("%d contract rows -> %d players", nrow(contracts_all), nrow(contracts)))),
        tags$tr(tags$td("Basketball-Reference ", STATS_SEASON - 1, "-", substr(STATS_SEASON, 3, 4)),
                tags$td(bbref$status),
                tags$td(sprintf("%d of %d contract players matched", match_stats, nrow(contracts)))),
        tags$tr(tags$td("Spotrac best value 2025"), tags$td(spotrac$status),
                tags$td(sprintf("%d of %d contract players matched", match_spotrac, nrow(contracts))))
      ),
      h3("Modifications to the original proposal"),
      tags$ul(
        tags$li(sprintf("%d players appear twice (current deal + extension). Map 1-3 keep the contract
                         covering the %d-%s season (or the newest one); Map 4 uses every contract.",
                        n_dupe_players, SEASON_REF, substr(SEASON_REF + 1, 3, 4))),
        tags$li(sprintf("%d two-way / exhibit deals had no AAV; AAV was estimated from 2-year cash
                         divided by min(years, 2). %d players with no salary data were dropped.",
                        n_imputed, n_dropped)),
        tags$li("Player names were normalised (accents, punctuation, Jr./III suffixes) to join the three sources."),
        tags$li("Traded players use their full-season combined Basketball-Reference line."),
        tags$li(HTML(paste0("Map 2 y-axis source: <b>", value_source, "</b>. If Spotrac blocks scraping,
                 the app falls back to a production-based surplus value computed from Basketball-Reference.")))
      ),
      h3("How these insights differ from the class examples"),
      tags$ul(
        tags$li("Map 1 is a hand-built three-level circle packing (league > team > player) with drill-down, not a standard scatter bubble chart."),
        tags$li("Map 2 adds a play-through slider that reveals players one at a time, a rolling-median trend (slider) and value quadrants."),
        tags$li("Map 3 is a user-built lineup: reactable with cumulative salary, averaged stats, lineup footer and a radar profile."),
        tags$li("Map 4 projects future cap commitments by combining current deals with signed extensions.")
      )
    )
  })
}

if (isTRUE(startup_ok)) {
  shinyApp(ui, server)
} else {
  shinyApp(
    ui = fluidPage(
      h2("The app could not load its data"),
      p("Error message:"), pre(style = "color:#b00; white-space:pre-wrap;", startup_error),
      h4("Common causes"),
      tags$ul(
        tags$li("nba-contracts-contracts.csv is not committed to the GitHub repo next to app.R."),
        tags$li("A package failed to load - check the Logs tab in Posit Connect Cloud."),
        tags$li("data_cache/ was not committed, so the app tried to scrape and was blocked.")
      )
    ),
    server = function(input, output, session) {}
  )
}