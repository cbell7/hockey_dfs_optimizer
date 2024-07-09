options(scipen = 9999999)
# Load dependencies and source scraper ------------------------------------

library(tidyverse)
library(lubridate)
library(stringi)
library(RCurl)
library(xml2)
library(rvest)
library(jsonlite)
library(foreach)
library(odbc)

# devtools::source_url("https://raw.githubusercontent.com/evolvingwild/evolving-hockey/master/EH_scrape_functions.R")
# Load NHL scraper file (originally from Evolving-Hockey)
source("nhl_scraper.R")


# Database ----------------------------------------------------------------

# Obfuscated ODBC connection details for MySQL database
hockey_db <- NULL

# Most recent date of data
recent_games_q <- "
SELECT MAX(g.game_date) max_date
FROM skater_dfs s
JOIN games g
  ON s.game_id = g.game_id
"
max_date <- tbl(hockey_db, sql(recent_games_q)) %>% pull()

# Scrape game data --------------------------------------------------------

# Get game IDs
# season_start <- ymd("2023-10-10")
# season_end <- ymd("2023-10-18")
(season_start <- max_date + days(1))
(season_end <- today() - days(1))
games <- sc.scrape_schedule(season_start, season_end, print_sched = F)

# Filter to regular season
games <- games %>% filter(session == 'R')
games %>% select(-c(game_datetime, EST_time_convert)) %>% head()

# Scrape games and pull event summary
scrape <- sc.scrape_pbp(games$game_id)

# Scrape failures
scrape_fail <- games %>%
  select(game_id, season, session) %>%
  anti_join(scrape$game_info_df, by = "game_id")

write_csv(scrape_fail, "scrape_fail_log.csv", append = TRUE)

# Add scores to games
game_scores <- scrape$game_info_df %>%
  select(game_id, home_score, away_score, attendance)

game_data <- as_tibble(games) %>%
  rename(game_time = EST_time_convert) %>%
  mutate(game_date = EST_date) %>%
  filter(session == "R") %>%
  select(-c(game_datetime, EST_date)) %>%
  inner_join(game_scores, by = "game_id")

# Clean event summary data ------------------------------------------------

fix_names <- function(players) {
  x <- stri_replace_last_regex(players, "\\.", " ")
  x <- str_to_title(x)
  gsub(x, pattern = "\\.([a-z])\\.", replacement = "\\.\\U\\1\\.", perl = TRUE)
}

summary <- as_tibble(scrape$events_summary_df) %>%
  select(player, position, game_id, team:is_home, toi_all, toi_pp, g, a, s, bs) %>%
  mutate(
    player = fix_names(player),
    hat_trick = g >= 3,
    shot_bns = s >= 5,
    blk_bonus = bs >= 3,
    pts_bonus = (g + a) >= 3
  )

# Pull out goalies
goalies <- summary %>%
  filter(position == "G") %>%
  select(player, team, opponent, game_id, is_home)

summary <- summary %>%
  filter(position != "G")

# Get shootout goals and shorthanded points -------------------------------
pbp <- as_tibble(scrape$pbp_base)

# Shootout: regular season period 5
# Alternative: game strength state = 1v0 or 0v1
shootout <- pbp %>%
  filter(session == "R", game_period == 5, event_type == "GOAL") %>%
  mutate(
    player = fix_names(event_player_1),
    sog = 1
  ) %>%
  select(game_id, player, sog)

summary_so <- summary %>%
  left_join(shootout, by = c("player", "game_id"))

# Shorthanded goal
shorthanded <- pbp %>%
  filter(
    game_strength_state %in% c("4v5", "3v5", "3v4"),
    event_type == "GOAL"
  ) %>%
  mutate(across(event_player_1:event_player_3, fix_names)) %>%
  select(game_id, event_player_1:event_player_3, game_strength_state) %>%
  pivot_longer(cols = c(event_player_1:event_player_3)) %>%
  mutate(pts = 1) %>%
  group_by(game_id, player = value) %>%
  summarize(shp = sum(pts)) %>%
  ungroup()

summary_full <- summary_so %>%
  left_join(shorthanded, by = c("player", "game_id")) %>%
  mutate(
    sog = replace_na(sog, 0),
    shp = replace_na(shp, 0)
  )


# Calculate skater fantasy points -----------------------------------------

# DraftKings point values
g_pts <- 8.5
a_pts <- 5
s_pts <- 1.5
b_pts <- 1.3
shp_pts <- 2
sog_pts <- 1.5
hat_pts <- 3
s_bns_pts <- 3
b_bns_pts <- 3
p_bns_pts <- 3

skater_dfs <- summary_full %>%
  mutate(fpts = 
    g * g_pts + a * a_pts + s * s_pts + bs * b_pts + 
    shp * shp_pts + sog * sog_pts + hat_trick * hat_pts + 
    shot_bns * s_bns_pts + blk_bonus * b_bns_pts + pts_bonus * p_bns_pts
  ) %>%
  select(-c(hat_trick:pts_bonus))

# Goalie stats ------------------------------------------------------------

# Saves and goals against
goalie_sv_ga <- pbp %>%
  filter(
    event_type %in% c("SHOT", "GOAL"),
    game_period < 5 # shootout saves/GA don't count
  ) %>%
  mutate(
    across(c(home_goalie, away_goalie), fix_names),
    player = ifelse(event_team == home_team, away_goalie, home_goalie),
    event_tally = 1
  ) %>%
  pivot_wider(names_from = event_type, values_from = event_tally) %>%
  group_by(game_id, player) %>%
  summarize( # NAs are due to empty net goals
    sv = sum(SHOT, na.rm = TRUE),
    ga = sum(GOAL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(sv_bns = sv >= 35) %>%
  filter(!is.na(player)) %>%
  left_join(goalies, by = c("player", "game_id")) # home?

# Wins, OTL, and shutouts
endgame <- pbp %>%
  filter(event_type %in% c("GEND")) %>% # not including shootouts
  # mutate(shootout = event_type == "SOC") %>%
  distinct(game_id, .keep_all = TRUE) %>%
  mutate(OT = game_period > 3) %>%
  select(game_id, game_period, OT) %>%
  left_join(game_data %>% select(game_id, home_score, away_score))

g_summary_full <- goalie_sv_ga %>%
  left_join(endgame, by = "game_id") %>%
  mutate(
    is_home = as.logical(is_home),
    sv_bns = as.numeric(sv_bns),
    home_win = home_score > away_score,
    win = as.numeric(is_home & home_win | !is_home & !home_win),
    shutout = as.numeric(win & (home_score == 0 | away_score == 0)),
    otl = as.numeric(!win & OT)
  ) %>%
  select(-c(is_home:home_win))


# Calculate goalie fantasy points -----------------------------------------

# DraftKings point values
win_pts <- 6
sv_pts <- 0.7
ga_pts <- -3.5
so_pts <- 4
otl_pts <- 2
sv_bns_pts <- 3

goalie_dfs <- g_summary_full %>%
  mutate(fpts =
    sv * sv_pts + ga * ga_pts + sv_bns * sv_bns_pts +
    win * win_pts + shutout * so_pts + otl * otl_pts
  ) %>%
  select(game_id, player, team, opponent, sv:fpts) %>%
  filter(game_id %in% pull(distinct(game_data, game_id)))


# Write to database -------------------------------------------------------

game_data
skater_dfs
nrow(skater_dfs) - nrow(game_data) * 2 * 20
goalie_dfs

dbWriteTable(hockey_db, "games", game_data, overwrite = FALSE, append = TRUE)
dbWriteTable(hockey_db, "skater_dfs", skater_dfs, overwrite = FALSE, append = TRUE)
dbWriteTable(hockey_db, "goalie_dfs", goalie_dfs, overwrite = FALSE, append = TRUE)
# write_rds(scrape, "2008_pbp.Rds")
