###############################################
# Class 10 Lab Dataset: 2024-25 MBB Players
# Source: hoopR (ESPN)
# Note: all "*_pct" and `usage` are decimals (0-1)
#       Additional *_pctile_pos columns are
#       weighted percentiles within position groups.
###############################################

# 0. Install packages (run once if needed)
# install.packages("hoopR")
# install.packages("dplyr")
# install.packages("readr")

library(hoopR)
library(dplyr)
library(readr)

###############################################
# Helper: weighted percentile function
###############################################

weighted_percentile <- function(x, w) {
  # Returns values in [0,1] representing the
  # weighted cumulative distribution position of each x
  if (length(x) == 0) return(numeric(0))
  
  w[is.na(w)] <- 0
  x_na <- is.na(x)
  
  if (all(x_na) || sum(w[!x_na]) <= 0) {
    return(rep(NA_real_, length(x)))
  }
  
  x2 <- x[!x_na]
  w2 <- w[!x_na]
  
  ord <- order(x2)
  x_sorted <- x2[ord]
  w_sorted <- w2[ord]
  
  cw <- cumsum(w_sorted)
  total_w <- sum(w_sorted)
  
  p_sorted <- cw / total_w
  
  p2 <- numeric(length(x2))
  p2[ord] <- p_sorted
  
  out <- rep(NA_real_, length(x))
  out[!x_na] <- p2
  out
}

###############################################
# 1. Pull ALL game-level player box data (2024-25)
###############################################

# hoopR uses the END year of the season -> 2025 for 2024-25
mbb_player_box_2025 <- load_mbb_player_box(seasons = 2025)

# (Optional) Save raw game-level data
# write_csv(
#   mbb_player_box_2025,
#   file = "~/Desktop/mbb_player_box_2025_raw.csv"
# )

###############################################
# 2. Clean out DNPs / no-minutes rows
###############################################

mbb_player_box_2025_clean <- mbb_player_box_2025 %>%
  filter(
    !isTRUE(did_not_play),    # drop DNP rows
    !is.na(minutes),
    minutes > 0
  )

# If you ONLY want regular season games, you can add:
# mbb_player_box_2025_clean <- mbb_player_box_2025_clean %>%
#   filter(season_type == 2)  # often 2 = regular season; check with count()

###############################################
# 3. Team-game totals (for team + opponent season stats)
###############################################

team_game_totals_2025 <- mbb_player_box_2025_clean %>%
  group_by(
    season,
    game_id,
    team_id,
    team_display_name,
    team_short_display_name,
    team_abbreviation
  ) %>%
  summarise(
    team_minutes = sum(minutes, na.rm = TRUE),
    team_fgm     = sum(field_goals_made, na.rm = TRUE),
    team_fga     = sum(field_goals_attempted, na.rm = TRUE),
    team_fg3a    = sum(three_point_field_goals_attempted, na.rm = TRUE),
    team_ftm     = sum(free_throws_made, na.rm = TRUE),
    team_fta     = sum(free_throws_attempted, na.rm = TRUE),
    team_oreb    = sum(offensive_rebounds, na.rm = TRUE),
    team_dreb    = sum(defensive_rebounds, na.rm = TRUE),
    team_reb     = sum(rebounds, na.rm = TRUE),
    team_tov     = sum(turnovers, na.rm = TRUE),
    .groups = "drop"
  )

# Attach opponent team stats for each team-game
team_game_w_opp_2025 <- team_game_totals_2025 %>%
  left_join(
    team_game_totals_2025,
    by = c("season", "game_id"),
    suffix = c("", "_opp")
  ) %>%
  filter(team_id != team_id_opp)

###############################################
# 4. Team-season totals (team + opponent)
###############################################

team_season_totals_2025 <- team_game_w_opp_2025 %>%
  group_by(season, team_id) %>%
  summarise(
    team_minutes_total = sum(team_minutes, na.rm = TRUE),
    team_fgm_total     = sum(team_fgm, na.rm = TRUE),
    team_fga_total     = sum(team_fga, na.rm = TRUE),
    team_fg3a_total    = sum(team_fg3a, na.rm = TRUE),
    team_ftm_total     = sum(team_ftm, na.rm = TRUE),
    team_fta_total     = sum(team_fta, na.rm = TRUE),
    team_oreb_total    = sum(team_oreb, na.rm = TRUE),
    team_dreb_total    = sum(team_dreb, na.rm = TRUE),
    team_reb_total     = sum(team_reb, na.rm = TRUE),
    team_tov_total     = sum(team_tov, na.rm = TRUE),
    
    opp_oreb_total     = sum(team_oreb_opp, na.rm = TRUE),
    opp_dreb_total     = sum(team_dreb_opp, na.rm = TRUE),
    opp_reb_total      = sum(team_reb_opp, na.rm = TRUE),
    .groups = "drop"
  )

###############################################
# 5. Player-season totals + advanced metrics
###############################################

mbb_player_season_2025 <- mbb_player_box_2025_clean %>%
  group_by(
    season,
    athlete_id,
    athlete_display_name,
    athlete_short_name,
    athlete_jersey,
    athlete_position_name,
    athlete_position_abbreviation,
    team_id,
    team_display_name,
    team_short_display_name,
    team_abbreviation
  ) %>%
  summarise(
    games_played   = n_distinct(game_id),
    games_started  = sum(starter, na.rm = TRUE),
    minutes_total  = sum(minutes, na.rm = TRUE),
    
    pts_total      = sum(points, na.rm = TRUE),
    fgm_total      = sum(field_goals_made, na.rm = TRUE),
    fga_total      = sum(field_goals_attempted, na.rm = TRUE),
    fg3m_total     = sum(three_point_field_goals_made, na.rm = TRUE),
    fg3a_total     = sum(three_point_field_goals_attempted, na.rm = TRUE),
    ftm_total      = sum(free_throws_made, na.rm = TRUE),
    fta_total      = sum(free_throws_attempted, na.rm = TRUE),
    
    oreb_total     = sum(offensive_rebounds, na.rm = TRUE),
    dreb_total     = sum(defensive_rebounds, na.rm = TRUE),
    reb_total      = sum(rebounds, na.rm = TRUE),
    ast_total      = sum(assists, na.rm = TRUE),
    stl_total      = sum(steals, na.rm = TRUE),
    blk_total      = sum(blocks, na.rm = TRUE),
    tov_total      = sum(turnovers, na.rm = TRUE),
    pf_total       = sum(fouls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Attach team + opponent season totals (for usage, reb%, AST%)
  left_join(team_season_totals_2025, by = c("season", "team_id")) %>%
  mutate(
    # Per-game counting stats
    mpg   = minutes_total / games_played,
    ppg   = pts_total / games_played,
    rpg   = reb_total / games_played,
    apg   = ast_total / games_played,
    spg   = stl_total / games_played,
    bpg   = blk_total / games_played,
    tovpg = tov_total / games_played,
    
    oreb_pg = oreb_total / games_played,
    dreb_pg = dreb_total / games_played,
    
    # Shooting percentages (decimals)
    fg_pct  = if_else(fga_total  > 0, fgm_total  / fga_total,  NA_real_),
    fg3_pct = if_else(fg3a_total > 0, fg3m_total / fg3a_total, NA_real_),
    ft_pct  = if_else(fta_total  > 0, ftm_total  / fta_total,  NA_real_),
    
    # Advanced shooting
    efg_pct = if_else(
      fga_total > 0,
      (fgm_total + 0.5 * fg3m_total) / fga_total,
      NA_real_
    ),
    ts_pct = if_else(
      (fga_total + 0.44 * fta_total) > 0,
      pts_total / (2 * (fga_total + 0.44 * fta_total)),
      NA_real_
    ),
    
    # Per-40 rates
    pts_per_40  = if_else(minutes_total > 0, pts_total  * 40 / minutes_total, NA_real_),
    reb_per_40  = if_else(minutes_total > 0, reb_total  * 40 / minutes_total, NA_real_),
    ast_per_40  = if_else(minutes_total > 0, ast_total  * 40 / minutes_total, NA_real_),
    stl_per_40  = if_else(minutes_total > 0, stl_total  * 40 / minutes_total, NA_real_),
    blk_per_40  = if_else(minutes_total > 0, blk_total  * 40 / minutes_total, NA_real_),
    tov_per_40  = if_else(minutes_total > 0, tov_total  * 40 / minutes_total, NA_real_),
    
    oreb_per_40 = if_else(minutes_total > 0, oreb_total * 40 / minutes_total, NA_real_),
    dreb_per_40 = if_else(minutes_total > 0, dreb_total * 40 / minutes_total, NA_real_),
    
    # Usage (decimal, not percent)
    # We'll also define usg_weight as the player's "possessions used":
    usg_weight = pmax(fga_total + 0.44 * fta_total + tov_total, 0),
    
    usage = if_else(
      minutes_total > 0 &
        (team_fga_total + 0.44 * team_fta_total + team_tov_total) > 0,
      ((fga_total + 0.44 * fta_total + tov_total) * (team_minutes_total / 5)) /
        (minutes_total * (team_fga_total + 0.44 * team_fta_total + team_tov_total)),
      NA_real_
    ),
    
    # Assist opportunities (denominator for AST%)
    ast_denom = (
      (minutes_total / (team_minutes_total / 5)) * team_fgm_total - fgm_total
    ),
    
    ast_pct = if_else(
      minutes_total > 0 &
        team_minutes_total > 0 &
        ast_denom > 0,
      ast_total / ast_denom,
      NA_real_
    ),
    
    # Turnover Percentage (decimal)
    tov_denom = fga_total + 0.44 * fta_total + tov_total,
    tov_pct = if_else(
      tov_denom > 0,
      tov_total / tov_denom,
      NA_real_
    ),
    
    # Offensive Rebound Percentage (decimal)
    oreb_chances = (minutes_total / (team_minutes_total / 5)) *
      (team_oreb_total + opp_dreb_total),
    
    oreb_pct = if_else(
      minutes_total > 0 &
        team_minutes_total > 0 &
        (team_oreb_total + opp_dreb_total) > 0 &
        oreb_chances > 0,
      oreb_total / oreb_chances,
      NA_real_
    ),
    
    # Defensive Rebound Percentage (decimal)
    dreb_chances = (minutes_total / (team_minutes_total / 5)) *
      (team_dreb_total + opp_oreb_total),
    
    dreb_pct = if_else(
      minutes_total > 0 &
        team_minutes_total > 0 &
        (team_dreb_total + opp_oreb_total) > 0 &
        dreb_chances > 0,
      dreb_total / dreb_chances,
      NA_real_
    ),
    
    # 3PA Rate (3PAr) and FTA Rate
    threepar = if_else(
      fga_total > 0,
      fg3a_total / fga_total,
      NA_real_
    ),
    fta_rate = if_else(
      fga_total > 0,
      fta_total / fga_total,
      NA_real_
    ),
    
    # Weights for weighted percentiles (sample-size proxies)
    ts_weight    = pmax(fga_total + 0.44 * fta_total, 0),
    efg_weight   = pmax(fga_total, 0),
    ast_weight   = pmax(ast_denom, 0),
    tov_weight   = pmax(tov_denom, 0),
    oreb_weight  = pmax(oreb_chances, 0),
    dreb_weight  = pmax(dreb_chances, 0),
    fg3_weight   = pmax(fg3a_total, 0),
    ft_weight    = pmax(fta_total, 0),
    threepar_weight = pmax(fga_total, 0),
    fta_rate_weight = pmax(fga_total, 0)
  ) %>%
  # Position-group weighted percentiles
  group_by(athlete_position_abbreviation) %>%
  mutate(
    usage_pctile_pos    = weighted_percentile(usage,    usg_weight),
    ts_pctile_pos       = weighted_percentile(ts_pct,   ts_weight),
    efg_pctile_pos      = weighted_percentile(efg_pct,  efg_weight),
    ast_pctile_pos      = weighted_percentile(ast_pct,  ast_weight),
    tov_pctile_pos      = weighted_percentile(tov_pct,  tov_weight),
    oreb_pctile_pos     = weighted_percentile(oreb_pct, oreb_weight),
    dreb_pctile_pos     = weighted_percentile(dreb_pct, dreb_weight),
    fg3_pctile_pos      = weighted_percentile(fg3_pct,  fg3_weight),
    ft_pctile_pos       = weighted_percentile(ft_pct,   ft_weight),
    threepar_pctile_pos = weighted_percentile(threepar, threepar_weight),
    fta_rate_pctile_pos = weighted_percentile(fta_rate, fta_rate_weight)
  ) %>%
  ungroup()

###############################################
# 6. Trim to a “lab-friendly” set of columns
###############################################

mbb_player_season_2025_lab <- mbb_player_season_2025 %>%
  select(
    season,
    athlete_id,
    player    = athlete_display_name,
    team      = team_short_display_name,
    position  = athlete_position_abbreviation,
    
    games_played,
    games_started,
    minutes_total,
    mpg,
    
    pts_total,
    ppg,
    pts_per_40,
    
    reb_total,
    rpg,
    reb_per_40,
    
    oreb_total,
    oreb_pg,
    oreb_per_40,
    
    dreb_total,
    dreb_pg,
    dreb_per_40,
    
    ast_total,
    apg,
    ast_per_40,
    
    stl_total,
    spg,
    stl_per_40,
    
    blk_total,
    bpg,
    blk_per_40,
    
    tov_total,
    tovpg,
    tov_per_40,
    
    fg_pct,
    fg3_pct,
    threepar,
    ft_pct,
    fta_rate,
    efg_pct,
    ts_pct,
    usage,
    ast_pct,
    tov_pct,
    oreb_pct,
    dreb_pct,
    
    usage_pctile_pos,
    ts_pctile_pos,
    efg_pctile_pos,
    ast_pctile_pos,
    tov_pctile_pos,
    oreb_pctile_pos,
    dreb_pctile_pos,
    fg3_pctile_pos,
    ft_pctile_pos,
    threepar_pctile_pos,
    fta_rate_pctile_pos
  )

###############################################
# 7. Write CSVs to disk
###############################################

# Full season-level dataset (all columns)
write_csv(
  mbb_player_season_2025,
  file = "~/Desktop/mbb_player_season_2025_full.csv"
)

# Trimmed “lab” dataset (cleaner for students)
write_csv(
  mbb_player_season_2025_lab,
  file = "~/Desktop/mbb_player_season_2025_lab.csv"
)

###############################################
# End of script
###############################################
