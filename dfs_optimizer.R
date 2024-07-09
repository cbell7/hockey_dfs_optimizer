library(tidyverse)
library(rvest)
library(odbc)
library(lpSolve)

# NHL ---------------------------------------------------------------------

# Load data

dk_file <- tibble(
  file = list.files("Data/DraftKings", pattern = "DKSalaries", full.names = T),
  date = file.info(file)$ctime
) %>% filter(date == max(date)) %>% pull(file)
print(stringr::str_c(dk_file, " | ", as.character(file.info(dk_file)$ctime)))
dk_schedule_nhl <- read_csv(dk_file)

# Player lookup - may need to edit when player info is missing
player_lookup <- read_csv("Data/DraftKings/dk_player_lookup.csv")

# Player blacklist due to duplicate names
blacklist <- tribble(
  ~ID, ~player, ~Team,
  30342576, "Sebastian Aho", "Islanders"
  # DTD
  
)

# Clean data
nhl_start <- dk_schedule_nhl %>%
  rename(name_id = `Name + ID`,
         roster_position = `Roster Position`,
         game_info = `Game Info`,
         team = TeamAbbrev,
         dk_pts = AvgPointsPerGame
  ) %>%
  anti_join(blacklist) %>% 
  mutate(c = str_detect(roster_position, "C"),
         w = str_detect(roster_position, "W"),
         d = str_detect(roster_position, "D"),
         g = str_detect(roster_position, "G"),
         util = str_detect(roster_position, "UTIL"),
         Salary = as.integer(Salary),
         ID = as.character(ID),
         teams = str_extract(game_info, "\\w+@\\w+"),
         away = str_split(teams, "@", simplify = TRUE)[,1],
         home = str_split(teams, "@", simplify = TRUE)[,2],
         opp = ifelse(team == home, away, home)
  ) %>%
  select(ID, Name, Salary, team, opp, dk_pts, c:util)


# Process data with filtered lineups --------------------------------------

# NHL Lineups
lineup_url <- "https://www.nhl.com/news/nhl-projected-lineup-projections"

# Do we have the most recent lineup data?
lineup_time <- read_html(lineup_url) %>%
  html_nodes(".nhl-c-article__date time") %>%
  html_text()
lineup_time <- mdy(lineup_time)

if (lineup_time == today()) {
  lineup_data <- TRUE
  print("Using lineups from nhl.com")
} else {
  lineup_data <- FALSE
  
  # Check for rotogrinders lineups
  rg_url <- "https://rotogrinders.com/lineups/nhl"
  rg_time <- read_html(rg_url) %>%
    html_nodes("h1 span") %>% 
    html_text()
  rg_time <- str_remove(rg_time, "\\(.+\\)") %>% mdy()
  if (rg_time != today()) {
    stop("No lineup data at all for today. Do not proceed!")
  }
  warning("No NHL lineup data for today. Using RotoGrinders")
}

if(lineup_data) {
  
  ### Extract lineups from NHL
  lineup_url <- "https://www.nhl.com/news/nhl-projected-lineup-projections"
  
  lineup_raw <- read_html(lineup_url) %>%
    html_nodes("p") %>%
    html_text()
  
  lineups_nested <- tibble(players = str_split(lineup_raw, " -- | ?-- ?| - | –|- |")) %>%
    rowwise() %>% 
    # 3 Players for Forward Lines, 2 for D-Pair, 1 for goalie
    mutate(pos = case_when(
      length(players) == 3 ~ 'F',
      length(players) == 2 ~ 'D',
      length(players) == 1 ~ 'G'
    ),
    header = players[1] # Extract first element of list for filtering / grouping
    ) %>%
    ungroup() %>%
    filter(
      !str_detect(header, "[0-9]|NHL|Scratched|Injured|Status [Rr]eport|Suspended|Listen|Watch|Conference---"), # Ignore rows without rostered players
      str_trim(header) != '' # Ignore blank rows
    ) %>%
    mutate(
      # Pick out team from row header, then put NAs for lineups since nothing to extract
      is_team = str_detect(header, "[Pp]rojected|[Ll]ineup"),
      team = ifelse(is_team, str_remove(header, " [Pp]rojected [Ll]ineup"), NA_character_)
    ) %>%
    fill(team, .direction = "down") %>% # Fill in NAs with above team
    filter(
      !is_team,
      !str_detect(team, 'Below'),
      !is.na(team)
    ) %>% 
    group_by(team, pos) %>%
    mutate(line = row_number()) %>% # Rank lines by team
    ungroup() %>% 
    select(players, team, pos, line)
  
  lineups <- lineups_nested %>% unnest(players) %>% mutate(players = str_trim(players))
  
  lineup_check <- lineups_nested %>% 
    count(team, pos) %>%
    filter(
      pos == 'F' & n < 4 |
      pos == 'D' & n < 3 |
      pos == 'G' & n < 2 
    )
  
  nhl <- lineups %>%
    left_join(player_lookup %>% select(player, dk_name), by = c("players" = "player")) %>%
    mutate(players = ifelse(is.na(dk_name), players, dk_name)) %>% 
    select(-team, -dk_name) %>% 
    left_join(nhl_start, by = c("players" = "Name")) %>% 
    anti_join(blacklist %>% mutate(ID = as.character(ID)))
  
  if(nrow(lineup_check) > 0) print(lineup_check)
  
} else {
  
  ### Extract rostered player list from rotogrinders
  rg_url <- "https://rotogrinders.com/lineups/nhl"
  
  lineup_rg_raw <- read_html(rg_url) %>%
    html_nodes(".pname, .name") %>% 
    html_text()
  
  roster_filter <- tibble(raw = lineup_rg_raw) %>% 
    mutate(
      f_last = str_extract(raw, "[A-Z]. \\w+ ?'?\\w*"),
      first = str_extract(f_last, "\\w"),
      last = str_remove(f_last, "[A-Z]. "),
      last7 = str_sub(last, 1, 7)
    ) %>%
    filter(!is.na(f_last)) %>% 
    select(-raw)
  
  nhl_join <- nhl_start %>%
    mutate(
      first = str_extract(Name, "\\w"),
      last = str_extract(Name, "\\s\\w+ ?'?\\w*"),
      last = str_trim(last),
      last7 = str_sub(last, 1, 7)
    )
  
  # Check for multiple players with F, last7 with multiple matches
  nhl_multi <- nhl_join %>%
    filter(Salary > 2500) %>%
    count(first, last7) %>% 
    filter(n > 1) %>% 
    arrange(desc(n))
  
  nhl <- semi_join(nhl_join, roster_filter, by = c("first", "last7")) %>%
    mutate(pos = NA_character_, line = NA_character_) %>%
    select(players = Name, pos, line, ID:util)
  
  if(nrow(nhl_multi) > 0) print(nhl_multi)
  
}

# If player lineups are available
nhl %>% filter(is.na(line))

### Comment out this section if all games are included
teams_not_included <- NULL
# teams_not_included <- c(
#   'Maple Leafs', 'Capitals',
#   'Sabres', 'Senators',
#   'Ducks', 'Blue Jackets'
# )
if (lineup_data) {
  players_not_included <- lineups %>% 
  filter(team %in% teams_not_included) %>%
  left_join(player_lookup 
            %>% select(player, dk_name), 
            by = c("players" = "player")) %>%
  mutate(players = coalesce(dk_name, players))
} else players_not_included <- data.frame()

###
if (lineup_data) {
  nhl %>% 
    # If game is late
    anti_join(players_not_included, by = "players") %>%
    filter(is.na(ID)) %>% 
    select(players, pos, line)
}

# Check for correct number of players
n_games <- n_distinct(dk_schedule_nhl$`Game Info`)
nrow(nhl) - n_games * 2 * 20 - lineup_data * nrow(players_not_included)  # 2 teams with 20 players per lineup

# If player lineups aren't available
anti_join(roster_filter, nhl_join, by = c("first", "last7")) # check for matches

# Check for duplicates
nhl %>% group_by(players, ID) %>% count() %>% filter(n > 1)

# Create predictions for feasible players ---------------------------------

# Obfuscated ODBC connection details for MySQL database
hockey_db <- NULL

# Skaters
query <- "SELECT s.player, s.game_id, g.game_date, s.fpts,
    ROW_NUMBER() OVER (PARTITION BY s.player ORDER BY g.game_date DESC) AS gm
  FROM skater_dfs AS s
  LEFT JOIN games AS g ON s.game_id = g.game_id
  WHERE session = 'R' AND game_date >= '2021-10-01'"
player <- tbl(hockey_db, sql(query)) %>% 
  collect() %>%
  mutate(gm = as.integer(gm))

# Goalies
query <- "SELECT go.player, go.game_id, g.game_date, go.fpts,
    ROW_NUMBER() OVER (PARTITION BY go.player ORDER BY g.game_date DESC) AS gm
  FROM goalie_dfs AS go
  LEFT JOIN games AS g ON go.game_id = g.game_id
  WHERE session = 'R' AND season >= 20222023"
goalie <- tbl(hockey_db, sql(query)) %>% 
  collect() %>%
  mutate(gm = as.integer(gm))

recent_games_skaters <- player %>% 
  filter(gm <= 20) %>%
  group_by(player) %>%
  mutate(
    n_games = n(),
    avg_fpts = mean(fpts),
    max_fpts = max(fpts),
    sd_pct = sd(fpts) / avg_fpts
  ) %>%
  filter(n_games == 20, gm == 1) %>%
  ungroup()

recent_games_goalies <- goalie %>% 
  filter(gm <= 15) %>%
  group_by(player) %>%
  mutate(
    n_games = n(),
    avg_fpts = mean(fpts),
    max_fpts = max(fpts),
    sd_pct = sd(fpts) / avg_fpts
  ) %>%
  filter(n_games == 15, gm == 1) %>%
  ungroup()

recent_games <- bind_rows(recent_games_skaters, recent_games_goalies)

players_with_data <- recent_games %>%
  distinct(player, avg_fpts, max_fpts, sd_pct) %>%
  left_join(player_lookup %>% select(player, dk_name), by = "player") %>%
  mutate(join_name = ifelse(is.na(dk_name), player, dk_name)) %>%
  select(player, Name = join_name, avg_fpts, max_fpts, sd_pct)

nhl_final <- nhl %>% 
  filter(!is.na(ID)) %>% 
  left_join(players_with_data, by = c("players" =  "Name")) %>%
  #mutate(pred = ifelse(!is.na(avg_fpts), avg_fpts, dk_pts)) %>%
  mutate(pred = avg_fpts) %>% 
  select(-player) %>% 
  distinct(players, .keep_all = TRUE)

# Check for players with missing data
nhl_final %>% filter(is.na(avg_fpts), !g, Salary > 2500) %>% select(players, team, dk_pts)

# Check for players with multiple data points
multi_players <- nhl_final %>% count(players, ID) %>% filter(n > 1)
has_multi <- !is.na(max(multi_players$n)) & max(multi_players$n) > 1

if (has_multi) {
  remove_data <- multi_players %>% 
    left_join(players_with_data, by = c('players' = 'Name')) %>% 
    left_join(recent_games, by = c('player' = 'player')) %>%
    group_by(ID) %>% 
    filter(game_date < max(game_date)) %>%
    ungroup() %>%
    select(ID, avg_fpts = avg_fpts.x)
  
  nhl_final <- nhl_final %>% anti_join(remove_data, by = c('ID', 'avg_fpts'))
}
nhl_final

# Optimize ----------------------------------------------------------------

goalies <- nhl_final %>% 
  filter(g) %>%
  mutate(pos = 'G')

generate_lineups <- function(goalie) {
  
  # Objective function: maximize avg. of my prediction and DK prediction
  nhl <- nhl_final %>% 
    mutate(pred = (pred + dk_pts) / 2) %>% 
    filter(
      !is.na(pred),
      !g,
      team != goalie$opp,
      !(team == "NYI" & players == 'Sebastian Aho')
    )
  
  objective <- nhl$pred
  direction <- "max"
  
  # Constraints
  salary_cap <- 50000 - goalie$Salary
  num_players <- 8
  centers <- 2
  wings <- 3
  defense <- 2
  
  const_lhs <- rbind(
    nhl$Salary,
    nhl$c,
    nhl$w,
    nhl$d,
    nhl$util
  )
  const_rhs <- c(salary_cap, centers, wings, defense, num_players) # removed goalies
  const_dir <- c(      "<=",    ">=",  ">=",    ">=",         "=")
  
  # Solve for optimal lineup, decision variable: binary - whether a player is on the roster 
  nhl_solve <- lp(direction, objective, const_lhs, const_dir, const_rhs,
                  all.bin = TRUE)
  
  # Lineup
  lineup <- nhl[as.logical(nhl_solve$solution), ] %>%
    bind_rows(goalie) %>%
    mutate(pred = ifelse(!g, avg_fpts, dk_pts))
  
  # You need skaters from 3 different teams
  skater_teams <- lineup %>%
    filter(!g) %>%
    summarize(teams = n_distinct(team)) %>% 
    pull()
  
  lineup_summary <- lineup %>%
    group_by(team, line) %>%
    mutate(stack = n()) %>%
    ungroup() %>%
    summarize(
      salary = sum(Salary),
      dk_pts = sum(dk_pts),
      pred = sum(pred),
      max = sum(max_fpts, na.rm = TRUE),
      stack_score = mean(stack),
      risk = mean(sd_pct, na.rm = TRUE)
    ) %>%
    mutate(n_teams = skater_teams)
  
  tibble(
    Goalie = goalie$players,
    team = goalie$team,
    opp = goalie$opp,
    lineup = list(lineup),
  ) %>% 
    bind_cols(lineup_summary)
}

dfs_lineups <- tibble()

for (i in 1:nrow(goalies)) {
  dfs_lineups <- bind_rows(dfs_lineups, generate_lineups(goalies[i,]))
}

dfs_lineups <- dfs_lineups %>% arrange(desc(pred), desc(max), desc(stack_score))
dfs_lineups

dfs_lineups$lineup[[1]] %>% 
  select(Salary, player = players, team, line, c:util, pred) %>%
  arrange(g, desc(c), desc(w), desc(d)) %>% 
  view()

