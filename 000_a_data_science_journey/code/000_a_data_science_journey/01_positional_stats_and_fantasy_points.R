

library(tidyverse)
library(arrow)


rm(list = ls())

# Fantasy league settings
fantasy_teams_val <- 10 


# Functions 
fantasy_qb_points <- 
  function(df) {
    df <- 
      df %>% 
      mutate(
        fantasy_points_dk = (
          passing_tds * 4 +
            passing_yards * .04 + 
            passing_yards300 * 3 + 
            passing_interceptions * 1 + 
            rushing_tds * 6 + 
            rushing_yards * .1 +  
            rushing_yards100 * 3 + 
            rushing_fumbles * 1), 
        fantasy_points_espn = (
          passing_tds * 4 +
            passing_yards * .04 + 
            passing_interceptions * 1 + 
            rushing_tds * 6 + 
            rushing_yards * .1 +  
            rushing_fumbles * 1))
  }

fantasy_rb_te_wr_points <- 
  function(df) {
    df <- 
      df %>% 
      mutate(
        fantasy_points_dk = (
          rushing_tds * 6 +
            rushing_yards * .1 + 
            rushing_yards100 * 3 +  
            rushing_fumbles * 1 + 
            
            receiving_receptions + 
            receiving_yards * .1 + 
            receiving_yards100 * 3 + 
            receiving_tds * 6 + 
            receiving_fumbles * 1),
        fantasy_points_espn = (
          rushing_tds * 6 +
            rushing_yards * .1 + 
            rushing_fumbles * 1 + 
            
            receiving_yards * .1 + 
            receiving_tds * 6 + 
            receiving_fumbles * 1))
  }

fantasy_points_next <- 
  function(df) {
    df <- 
      df %>% 
      arrange(
        player_id, 
        season_type, 
        season) %>% 
      group_by(
        player_id, 
        season_type) %>%
      mutate(
        fantasy_points_dk_next = lead(fantasy_points_dk, 1),
        fantasy_points_espn_next = lead(fantasy_points_espn, 1),
        fantasy_points_dk_above_rplcmnt_next = lead(fantasy_points_dk_above_rplcmnt, 1),
        fantasy_points_espn_above_rplcmnt_next = lead(fantasy_points_espn_above_rplcmnt, 1)
      ) %>%
      ungroup() %>% 
      select(
        season_type, 
        season, 
        player_id, 
        player_name, 
        position, 
        team, 
        games, 
        fantasy_points_dk_next, 
        fantasy_points_espn_next, 
        fantasy_points_dk_above_rplcmnt_next, 
        fantasy_points_espn_above_rplcmnt_next, 
        everything())
  }

# Weekly player stats 
player_stats <- 
  read_parquet('000_a_data_science_journey/data/player_stats.parquet') %>% 
  mutate(
    fantasy_teams = fantasy_teams_val, 
    player_name = str_replace(player_name, '. ', '.'), 
    air_yards_share = if_else(is.na(air_yards_share) == TRUE, 0, air_yards_share)) %>% 
  mutate(
    player_name = case_when( # Eventually move to function
      player_id == '00-0030151' ~ 'T.Brady',
      player_id == '00-0034857' ~ 'J.Allen',
      player_id == '00-0023459' ~ 'A.Rodgers',
      player_id == '00-0029665' ~ 'R.Griffin',
      player_id == '00-0026993' ~ 'J.Freeman',
      player_id == '00-0023436' ~ 'A.Smith',
      player_id == '00-0028118' ~ 'T.Taylor',
      player_id == '00-0020679' ~ 'S.Hill',
      player_id == '00-0025708' ~ 'M.Moore',
      player_id == '00-0028825' ~ 'T.Pryor',
      player_id == '00-0035289' ~ 'G.Minshew',
      player_id == '00-0016838' ~ 'A.Va.Pelt',
      player_id == '00-0013338' ~ 'J.Quinn',
      player_id == '00-0019192' ~ 'T.Brown',
      player_id == '00-0027796' ~ 'J.Webb',
      player_id == '00-0021254' ~ 'R.Fasani',

      # RBs
      player_id == '00-0032187' ~ 'D.Johnson',
      player_id == '00-0020407' ~ 'R.Johnson',
      player_id == '00-0027966' ~ 'M.Ingram',
      player_id == '00-0032780' ~ 'J.Howard',
      player_id == '00-0027864' ~ 'R.Mathews',
      player_id == '00-0032187' ~ 'D.Johnson',
      player_id == '00-0020403' ~ 'A.Thomas',
      player_id == '00-0026204' ~ 'K.Smith',
      player_id == '00-0034816' ~ 'R.Jones',
      player_id == '00-0034301' ~ 'D.Williams',
      player_id == '00-0031390' ~ 'C.Sims',
      player_id == '00-0024242' ~ 'D.Williams',
      player_id == '00-0030874' ~ 'D.Williams',
      player_id == '00-0036924' ~ 'M.Carter',
      player_id == '00-0028083' ~ 'J.Rodgers',
      player_id == '00-0032152' ~ 'K.Williams',
      player_id == '00-0035537' ~ 'T.Johnson',
      player_id == '00-0028000' ~ 'D.Thomas',
      player_id == '00-0033950' ~ 'W.Gallman',
      player_id == '00-0034457' ~ 'J.Adams',
      player_id == '00-0035217' ~ 'B.Snell',
      player_id == '00-0032257' ~ 'D.Johnson',
      player_id == '00-0025535' ~ 'K.Smith',
      player_id == '00-0020964' ~ 'T.Fisher',
      player_id == '00-0022130' ~ 'M.Smith',
      player_id == '00-0002713' ~ 'T.Carter',
      player_id == '00-0022141' ~ 'J.Johnson',
      player_id == '00-0017896' ~ 'N.Luchey',
      player_id == '00-0028841' ~ 'W.Johnson',
      player_id == '00-0027346' ~ 'A.Smith',
      player_id == '00-0022883' ~ 'A.Echemandu',
      player_id == '00-0025568' ~ 'R.Mauia',
      player_id == '00-0028706' ~ 'A.Smith',
      player_id == '00-0033838' ~ 'T.Edmunds',
      player_id == '00-0034449' ~ 'B.Howell',
      player_id == '00-0035726' ~ 'J.Johnson',

      # WRs
      player_id == '00-0027944' ~ 'J.Jones',
      player_id == '00-0020337' ~ 'S.Smith',
      player_id == '00-0027874' ~ 'D.Thomas',
      player_id == '00-0031428' ~ 'A.Robinson',
      player_id == '00-0031235' ~ 'O.Beckham',
      player_id == '00-0020397' ~ 'C.Johnson',
      player_id == '00-0015218' ~ 'J.Smith',
      player_id == '00-0035640' ~ 'D.Metcalf',
      player_id == '00-0022909' ~ 'R.Williams',
      player_id == '00-0026364' ~ 'S.Johnson',
      player_id == '00-0035216' ~ 'D.Johnson',
      player_id == '00-0027702' ~ 'M.Williams',
      player_id == '00-0027891' ~ 'G.Tate',
      player_id == '00-0002068' ~ 'T.Brown',
      player_id == '00-0029293' ~ 'M.Jones',
      player_id == '00-0025465' ~ 'J.Jones',
      player_id == '00-0032160' ~ 'T.Williams',
      player_id == '00-0034827' ~ 'D.Moore',
      player_id == '00-0031051' ~ 'J.Brown',
      player_id == '00-0028052' ~ 'C.Shorts',
      player_id == '00-0032688' ~ 'R.Anderson',
      player_id == '00-0033127' ~ 'W.Fuller',
      player_id == '00-0025466' ~ 'M.Sims-Walker',
      player_id == '00-0030663' ~ 'W.Snead',
      player_id == '00-0029632' ~ 'M.Sanu',
      player_id == '00-0022156' ~ 'B.Johnson',
      player_id == '00-0033681' ~ 'K.Cole',
      player_id == '00-0027893' ~ 'D.Williams',
      player_id == '00-0021190' ~ 'A.Randl.El',
      player_id == '00-0034777' ~ 'D.Chark',
      player_id == '00-0036268' ~ 'L.Shenault',
      player_id == '00-0021175' ~ 'A.Davis',
      player_id == '00-0021166' ~ 'J.Reed',
      player_id == '00-0036182' ~ 'N.Westbrook-Ikhine',
      player_id == '00-0030300' ~ 'Ja.Brown',
      player_id == '00-0033460' ~ 'J.Ross',
      player_id == '00-0032208' ~ 'P.Dorsett',
      player_id == '00-0013512' ~ 'J.Reed',
      player_id == '00-0030113' ~ 'Ch.Johnson',
      player_id == '00-0028116' ~ 'A.Robinson',
      player_id == '00-0023442' ~ 'T.Williamson',
      player_id == '00-0021330' ~ 'D.Jones',
      player_id == '00-0021993' ~ 'B.Johnson',
      player_id == '00-0027356' ~ 'M.Moore',
      player_id == '00-0028307' ~ 'D.Davis',
      player_id == '00-0034286' ~ 'R.James',
      player_id == '00-0035457' ~ 'T.Johnson',
      player_id == '00-0036382' ~ 'K.Hill',

      # TEs
      player_id == '00-0024389' ~ 'D.Walker',
      player_id == '00-0020171' ~ 'B.Williams',
      player_id == '00-0023506' ~ 'A.Smith',
      player_id == '00-0020171' ~ 'E.Williams',
      player_id == '00-0035329' ~ 'D.Parham',
      player_id == '00-0019919' ~ 'R.Williams',
      player_id == '00-0021268' ~ 'T.Jones',
      player_id == '00-0022051' ~ 'T.Johnson',

      TRUE ~ player_name)
    )


# Roster info for player position
roster <- 
  read_parquet('000_a_data_science_journey/data/roster.parquet') %>% 
  distinct(season, position, gsis_id)

player_stats <- 
  player_stats %>% 
  inner_join(
    y = roster, 
    by = c("season", "player_id" = "gsis_id")) %>% 
  mutate(
    passing_yards300 = if_else(passing_yards >= 300, 1, 0),
    rushing_yards100 = if_else(rushing_yards >= 100, 1, 0),
    receiving_yards100 = if_else(receiving_yards >= 100, 1, 0)) %>% 
  select(
    season_type, 
    season, 
    week, 
    player_id, 
    player_name, 
    position, 
    team = recent_team, 
    everything())

rm(roster)


# Team passing, rushing, and receiving stats
team_summary_by_week_passing <- 
  player_stats %>% 
  group_by(
    season_type, 
    season, 
    week, 
    team, 
    fantasy_teams) %>% 
  summarise(
    completions = sum(completions, na.rm = TRUE), 
    attempts = sum(attempts, na.rm = TRUE), 
    passing_yards = sum(passing_yards, na.rm = TRUE), 
    passing_tds = sum(passing_tds, na.rm = TRUE), 
    interceptions = sum(interceptions, na.rm = TRUE), 
    sacks = sum(sacks, na.rm = TRUE), 
    sack_yards = sum(sack_yards, na.rm = TRUE), 
    sack_fumbles = sum(sack_fumbles, na.rm = TRUE), 
    sack_fumbles_lost = sum(sack_fumbles_lost, na.rm = TRUE), 
    passing_air_yards = sum(passing_air_yards, na.rm = TRUE), 
    passing_yards_after_catch = sum(passing_yards_after_catch, na.rm = TRUE), 
    passing_first_downs = sum(passing_first_downs, na.rm = TRUE), 
    passing_epa = sum(passing_epa, na.rm = TRUE), 
    passing_2pt_conversions = sum(passing_2pt_conversions, na.rm = TRUE)) %>% 
  ungroup()

team_summary_by_week_rushing <- 
  player_stats %>% 
  group_by(
    season_type, 
    season, 
    week, 
    team, 
    fantasy_teams) %>% 
  summarise(
    carries = sum(carries, na.rm = TRUE), 
    rushing_yards = sum(rushing_yards, na.rm = TRUE), 
    rushing_tds = sum(rushing_tds, na.rm = TRUE), 
    rushing_fumbles = sum(rushing_fumbles, na.rm = TRUE), 
    rushing_fumbles_lost = sum(rushing_fumbles_lost, na.rm = TRUE), 
    rushing_first_downs = sum(rushing_first_downs, na.rm = TRUE), 
    rushing_epa = sum(rushing_epa, na.rm = TRUE), 
    rushing_2pt_conversions = sum(rushing_2pt_conversions, na.rm = TRUE)) %>% 
  ungroup()

team_summary_by_week_receiving <- 
  player_stats %>% 
  group_by(
    season_type, 
    season, 
    week, 
    team, 
    fantasy_teams) %>% 
  summarise(
    receptions = sum(receptions, na.rm = TRUE), 
    targets = sum(targets, na.rm = TRUE), 
    receiving_yards = sum(receiving_yards, na.rm = TRUE), 
    receiving_tds = sum(receiving_tds, na.rm = TRUE), 
    receiving_fumbles = sum(receiving_fumbles, na.rm = TRUE), 
    receiving_fumbles_lost = sum(receiving_fumbles_lost, na.rm = TRUE), 
    receiving_air_yards = sum(receiving_air_yards, na.rm = TRUE), 
    receiving_yards_after_catch = sum(receiving_yards_after_catch, na.rm = TRUE), 
    receiving_first_downs = sum(receiving_first_downs, na.rm = TRUE), 
    receiving_epa = sum(receiving_epa, na.rm = TRUE), 
    receiving_2pt_conversions = sum(receiving_2pt_conversions, na.rm = TRUE)) %>% 
  ungroup()

team_summary_by_week <- 
  player_stats %>% 
  distinct(
    season_type, 
    season, 
    week, 
    team, 
    fantasy_teams) %>% 
  inner_join(team_summary_by_week_passing) %>% 
  inner_join(team_summary_by_week_rushing) %>% 
  inner_join(team_summary_by_week_receiving) %>% 
  mutate(
    team_plays = attempts + carries, 
    team_rush_pct = round(attempts / team_plays, 5), 
    team_pass_pct = round(1 - team_rush_pct, 5)) %>% 
  select(
    season_type, 
    season, 
    week, 
    team, 
    fantasy_teams, 
    team_plays, 
    team_rush_pct, 
    team_pass_pct, 
    team_passing_attempts = attempts, 
    team_rushing_attempts = carries, 
    team_receiving_targets = targets, 
    team_receiving_receptions = receptions)

team_summary_by_season <- 
  team_summary_by_week %>% 
  group_by(
    season_type, 
      season, 
      team, 
    fantasy_teams) %>% 
  summarise(
    team_passing_attempts = sum(team_passing_attempts, na.rm = FALSE), 
    team_rushing_attempts = sum(team_rushing_attempts, na.rm = FALSE), 
    team_receiving_targets = sum(team_receiving_targets, na.rm = FALSE), 
    team_receiving_receptions = sum(team_receiving_receptions, na.rm = FALSE)) %>% 
  ungroup() %>% 
  mutate(
    team_plays = team_passing_attempts + team_rushing_attempts, 
    team_rush_pct = round(team_rushing_attempts / team_plays, 5), 
    team_pass_pct = round(1 - team_rush_pct, 5)) %>% 
  select(
    season_type, 
    season, 
    team, 
    fantasy_teams, 
    team_plays, 
    team_rush_pct, 
    team_pass_pct, 
    team_passing_attempts, 
    team_rushing_attempts, 
    team_receiving_targets, 
    team_receiving_receptions)
  

rm(list = c('team_summary_by_week_passing', 'team_summary_by_week_rushing', 'team_summary_by_week_receiving'))

player_stats <- 
  player_stats %>% 
  inner_join(
    y = team_summary_by_week, 
    by = c('season_type', 'season', 'week', 'team', 'fantasy_teams')) %>% 
  mutate(
    passing_share = round(attempts / team_passing_attempts, 5), 
    receiving_target_share = round(targets / team_receiving_targets, 5), 
    rushing_share = round(carries / team_rushing_attempts, 5), 
    passing_yards_after_catch_pct = if_else(
      passing_yards_after_catch == 0, NA_real_, 
      round(passing_yards_after_catch / passing_yards, 5))) %>% 
  mutate(
    passing_epa = replace_na(passing_epa, 0), 
    rushing_epa = replace_na(rushing_epa, 0)) %>% 
  select(
    -sack_yards, 
    -pacr, 
    -dakota, 
    -racr, 
    -wopr, 
    -fantasy_points, 
    -fantasy_points_ppr) %>% 
  mutate(
    passing_completion_pct = if_else(
      is.na(attempts) == TRUE, NA_real_, 
      round(completions / attempts, 5))) %>% 
  select(
    season_type, 
    season, 
    week, 
    player_id, 
    player_name, 
    position, 
    team, 
    fantasy_teams, 
    team_plays, 
    team_rush_pct, 
    team_pass_pct, 
    
    passing_share, 
    passing_completions = completions, 
    passing_attempts = attempts, 
    passing_completion_pct, 
    passing_yards, 
    passing_yards300, 
    passing_tds, 
    passing_interceptions = interceptions, 
    passing_sacks = sacks, 
    passing_sack_fumbles = sack_fumbles,
    passing_sack_fumbles_lost = sack_fumbles_lost,
    passing_air_yards, 
    passing_yards_after_catch, 
    passing_yards_after_catch_pct, 
    passing_first_downs, 
    passing_epa, 
    passing_2pt_conversions, 
    
    rushing_share, 
    rushing_attempts = carries, 
    rushing_yards, 
    rushing_yards100, 
    rushing_tds, 
    rushing_fumbles, 
    rushing_fumbles_lost, 
    rushing_first_downs, 
    rushing_epa, 
    rushing_2pt_conversions, 
    
    receiving_target_share, 
    receiving_air_yards_share = air_yards_share, 
    receiving_receptions = receptions, 
    receiving_target = targets, 
    receiving_yards, 
    receiving_yards100, 
    receiving_tds, 
    receiving_fumbles, 
    receiving_fumbles_lost, 
    receiving_air_yards, 
    receiving_yards_after_catch, 
    receiving_first_downs, 
    receiving_2pt_conversions) %>% 
  distinct()

rm(team_summary_by_week)


# Positional stats and fantasy points by week 
qbs_by_week <- 
  player_stats %>% 
  filter(position == 'QB') %>% 
  mutate(
    season_type = as_factor(season_type), 
    player_id = as_factor(player_id), 
    player_name = as_factor(player_name), 
    position = as_factor(position), 
    team = as_factor(team)) %>% 
  fantasy_qb_points() %>% 
  group_by(
    season_type, 
    season, 
    week, 
    position) %>% 
  mutate(
    fantasy_points_dk_rank = min_rank(-fantasy_points_dk), 
    fantasy_points_espn_rank = min_rank(-fantasy_points_espn)) %>% 
  ungroup() %>%  
  arrange(season, week) %>% 
  group_by(player_id, season_type) %>% 
  mutate(
    week_n = 1:n(), 
    week_last_n = n():1) %>% 
  ungroup() %>% 
  select(
    season_type, 
    season, 
    week, 
    player_id, 
    player_name, 
    position, 
    team, 
    fantasy_teams, 
    week_n, 
    week_last_n, 
    fantasy_points_dk, 
    fantasy_points_espn, 
    fantasy_points_dk_rank, 
    fantasy_points_espn_rank, 
    team_plays, 
    team_pass_pct, 
    starts_with('passing_'), 
    starts_with('rushing_')) 


rbs_by_week <- 
  player_stats %>% 
  filter(position %in% c('RB', 'FB')) %>% 
  mutate(
    fullback = if_else(position == 'FB', 'Y', 'N'), 
    position = 'RB') %>% 
  mutate(
    season_type = as_factor(season_type), 
    player_id = as_factor(player_id), 
    player_name = as_factor(player_name), 
    position = as_factor(position), 
    team = as_factor(team)) %>% 
  fantasy_rb_te_wr_points() %>% 
  group_by(
    season_type, 
    season, 
    week, 
    position) %>% 
  mutate(
    fantasy_points_dk_rank = min_rank(-fantasy_points_dk), 
    fantasy_points_espn_rank = min_rank(-fantasy_points_espn)) %>% 
  ungroup() %>% 
  arrange(season, week) %>% 
  group_by(player_id, season_type) %>% 
  mutate(
    week_n = 1:n(), 
    week_last_n = n():1) %>% 
  ungroup() %>% 
  select(
    season_type, 
    season, 
    week, 
    player_id, 
    player_name, 
    position, 
    team, 
    fantasy_teams, 
    week_n, 
    week_last_n, 
    fantasy_points_dk, 
    fantasy_points_espn, 
    fantasy_points_dk_rank, 
    fantasy_points_espn_rank, 
    team_plays, 
    team_rush_pct, 
    starts_with('rushing_'), 
    starts_with('receiving_'))



wrs_by_week <- 
  player_stats %>% 
  filter(position == 'WR') %>% 
  mutate(
    season_type = as_factor(season_type), 
    player_id = as_factor(player_id), 
    player_name = as_factor(player_name), 
    position = as_factor(position), 
    team = as_factor(team)) %>% 
  fantasy_rb_te_wr_points() %>% 
  group_by(
    season_type, 
    season, 
    week, 
    position) %>% 
  mutate(
    fantasy_points_dk_rank = min_rank(-fantasy_points_dk), 
    fantasy_points_espn_rank = min_rank(-fantasy_points_espn)) %>% 
  ungroup() %>% 
  arrange(season, week) %>% 
  group_by(
    player_id, 
    season_type) %>% 
  mutate(
    week_n = 1:n(), 
    week_last_n = n():1) %>% 
  ungroup() %>% 
  select(
    season_type, 
    season, 
    week, 
    player_id, 
    player_name, 
    position, 
    team,
    fantasy_teams,  
    week_n, 
    week_last_n, 
    fantasy_points_dk, 
    fantasy_points_espn, 
    fantasy_points_dk_rank, 
    fantasy_points_espn_rank, 
    team_plays, 
    team_pass_pct, 
    starts_with('receiving_'), 
    starts_with('rushing_'))

tes_by_week <- 
  player_stats %>% 
  filter(position == 'TE') %>% 
  mutate(
    season_type = as_factor(season_type), 
    player_id = as_factor(player_id), 
    player_name = as_factor(player_name), 
    position = as_factor(position), 
    team = as_factor(team)) %>% 
  fantasy_rb_te_wr_points() %>% 
  group_by(
    season_type, 
    season, 
    week, 
    position) %>% 
  mutate(
    fantasy_points_dk_rank = min_rank(-fantasy_points_dk), 
    fantasy_points_espn_rank = min_rank(-fantasy_points_espn)) %>% 
  ungroup() %>% 
  arrange(season, week) %>% 
  group_by(player_id, season_type) %>% 
  mutate(
    week_n = 1:n(), 
    week_last_n = n():1) %>% 
  ungroup() %>% 
  select(
    season_type, 
    season, 
    week, 
    player_id, 
    player_name, 
    position, 
    team, 
    fantasy_teams, 
    week_n, 
    week_last_n, 
    fantasy_points_dk, 
    fantasy_points_espn, 
    fantasy_points_dk_rank, 
    fantasy_points_espn_rank, 
    team_plays, 
    team_pass_pct, 
    starts_with('receiving_'), 
    starts_with('rushing_'))

write_parquet(qbs_by_week, '000_a_data_science_journey/results/qbs_by_week.parquet')
write_parquet(rbs_by_week, '000_a_data_science_journey/results/rbs_by_week.parquet')
write_parquet(wrs_by_week, '000_a_data_science_journey/results/wrs_by_week.parquet')
write_parquet(tes_by_week, '000_a_data_science_journey/results/tes_by_week.parquet')


# Positional stats and fantasy points by season
qbs_by_season <- 
  qbs_by_week %>% 
  group_by(
    season_type, 
    season, 
    player_id, 
    player_name, 
    position, 
    team,
    fantasy_teams) %>% 
  summarise(
    games = sum(n()), 
    fantasy_points_dk = sum(fantasy_points_dk, na.rm = TRUE), 
    fantasy_points_espn = sum(fantasy_points_espn, na.rm = TRUE), 
    fantasy_points_dk_rank_avg = mean(fantasy_points_dk_rank, na.rm = TRUE), 
    fantasy_points_espn_rank_avg = mean(fantasy_points_espn_rank, na.rm = TRUE), 
    team_plays_avg = mean(team_plays, na.rm = TRUE),
    team_pass_pct_avg = mean(team_pass_pct, na.rm = TRUE), 
    passing_share = mean(passing_share, na.rm = TRUE),
    passing_completions = sum(passing_completions, na.rm = TRUE), 
    passing_attempts = sum(passing_attempts, na.rm = TRUE), 
    passing_completion_pct = if_else(passing_attempts <= 0, 0, round(passing_completions / passing_attempts, 5)), 
    passing_yards = sum(passing_yards, na.rm = TRUE), 
    passing_yards300 = sum(passing_yards300, na.rm = TRUE), 
    passing_tds = sum(passing_tds, na.rm = TRUE), 
    passing_interceptions = sum(passing_interceptions, na.rm = TRUE), 
    passing_sacks = sum(passing_sacks, na.rm = TRUE), 
    passing_sack_fumbles = sum(passing_sack_fumbles, na.rm = TRUE), 
    passing_sack_fumbles_lost = sum(passing_sack_fumbles_lost, na.rm = TRUE), 
    passing_air_yards = sum(passing_air_yards, na.rm = TRUE), 
    passing_yards_after_catch = sum(passing_yards_after_catch, na.rm = TRUE), 
    passing_yards_after_catch_pct = if_else(passing_yards <= 0, 0, round(passing_yards_after_catch / passing_yards, 5)), 
    passing_first_downs = sum(passing_first_downs, na.rm = TRUE), 
    passing_epa = sum(passing_epa, na.rm = TRUE), 
    passing_2pt_conversions = sum(passing_2pt_conversions, na.rm = TRUE), 
    rushing_share = mean(rushing_share, na.rm = TRUE), 
    rushing_attempts = sum(rushing_attempts, na.rm = TRUE), 
    rushing_yards = sum(rushing_yards, na.rm = TRUE), 
    rushing_yards100 = sum(rushing_yards100, na.rm = TRUE), 
    rushing_tds = sum(rushing_tds, na.rm = TRUE), 
    rushing_fumbles = sum(rushing_fumbles, na.rm = TRUE), 
    rushing_fumbles_lost = sum(rushing_fumbles_lost, na.rm = TRUE), 
    rushing_first_downs = sum(rushing_first_downs, na.rm = TRUE), 
    rushing_epa = sum(rushing_epa, na.rm = TRUE), 
    rushing_2pt_conversions = sum(rushing_2pt_conversions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(season) %>% 
  group_by(
    player_id, 
    season_type) %>% 
  mutate(
    season_n = 1:n(), 
    season_last_n = n():1) %>% 
  ungroup() %>% 
  group_by(
    season_type, 
    season,
    position) %>% 
  mutate(
    fantasy_points_dk_rank = min_rank(-fantasy_points_dk), 
    fantasy_points_espn_rank = min_rank(-fantasy_points_espn)) %>% 
  ungroup() 

rbs_by_season <- 
  rbs_by_week %>% 
  group_by(
    season_type, 
    season, 
    player_id, 
    player_name, 
    position, 
    team,
    fantasy_teams) %>% 
  summarise(
    games = sum(n()), 
    fantasy_points_dk = sum(fantasy_points_dk, na.rm = TRUE), 
    fantasy_points_espn = sum(fantasy_points_espn, na.rm = TRUE),  
    fantasy_points_dk_rank_avg = mean(fantasy_points_dk_rank, na.rm = TRUE), 
    fantasy_points_espn_rank_avg = mean(fantasy_points_espn_rank, na.rm = TRUE), 
    team_plays_avg = mean(team_plays, na.rm = TRUE), 
    rushing_share = mean(rushing_share, na.rm = TRUE), 
    rushing_attempts = sum(rushing_attempts, na.rm = TRUE), 
    rushing_yards = sum(rushing_yards, na.rm = TRUE), 
    rushing_yards100 = sum(rushing_yards100, na.rm = TRUE), 
    rushing_tds = sum(rushing_tds, na.rm = TRUE), 
    rushing_fumbles = sum(rushing_fumbles, na.rm = TRUE), 
    rushing_fumbles_lost = sum(rushing_fumbles_lost, na.rm = TRUE), 
    rushing_first_downs = sum(rushing_first_downs, na.rm = TRUE), 
    rushing_epa = sum(rushing_epa, na.rm = TRUE), 
    rushing_2pt_conversions = sum(rushing_2pt_conversions, na.rm = TRUE), 
    receiving_target_share = mean(receiving_target_share, na.rm = TRUE), 
    receiving_air_yards_share = mean(receiving_air_yards_share, na.rm = TRUE), 
    receiving_receptions = sum(receiving_receptions, na.rm = TRUE), 
    receiving_target = sum(receiving_target, na.rm = TRUE), 
    receiving_yards = sum(receiving_yards, na.rm = TRUE), 
    receiving_yards100 = sum(receiving_yards100, na.rm = TRUE), 
    receiving_tds = sum(receiving_tds, na.rm = TRUE), 
    receiving_fumbles = sum(receiving_fumbles, na.rm = TRUE), 
    receiving_fumbles_lost = sum(receiving_fumbles_lost, na.rm = TRUE), 
    receiving_air_yards = sum(receiving_air_yards, na.rm = TRUE), 
    receiving_yards_after_catch = sum(receiving_yards_after_catch, na.rm = TRUE), 
    receiving_first_downs = sum(receiving_first_downs, na.rm = TRUE), 
    receiving_2pt_conversions = sum(receiving_2pt_conversions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(season) %>% 
  group_by(
    player_id, 
    season_type) %>% 
  mutate(
    season_n = 1:n(), 
    season_last_n = n():1) %>% 
  ungroup() %>% 
  group_by(
    season_type, 
    season,
    position) %>% 
  mutate(
    fantasy_points_dk_rank = min_rank(-fantasy_points_dk), 
    fantasy_points_espn_rank = min_rank(-fantasy_points_espn)) %>% 
  ungroup() 

wrs_by_season <- 
  wrs_by_week %>% 
  group_by(
    season_type, 
    season, 
    player_id, 
    player_name, 
    position, 
    team,
    fantasy_teams) %>% 
  summarise(
    games = sum(n()), 
    fantasy_points_dk = sum(fantasy_points_dk, na.rm = TRUE), 
    fantasy_points_espn = sum(fantasy_points_espn, na.rm = TRUE), 
    fantasy_points_dk_rank_avg = mean(fantasy_points_dk_rank, na.rm = TRUE), 
    fantasy_points_espn_rank_avg = mean(fantasy_points_espn_rank, na.rm = TRUE), 
    team_plays_avg = mean(team_plays, na.rm = TRUE), 
    receiving_target_share = mean(receiving_target_share, na.rm = TRUE), 
    receiving_air_yards_share = mean(receiving_air_yards_share, na.rm = TRUE), 
    receiving_receptions = sum(receiving_receptions, na.rm = TRUE), 
    receiving_target = sum(receiving_target, na.rm = TRUE), 
    receiving_yards = sum(receiving_yards, na.rm = TRUE), 
    receiving_yards100 = sum(receiving_yards100, na.rm = TRUE), 
    receiving_tds = sum(receiving_tds, na.rm = TRUE), 
    receiving_fumbles = sum(receiving_fumbles, na.rm = TRUE), 
    receiving_fumbles_lost = sum(receiving_fumbles_lost, na.rm = TRUE), 
    receiving_air_yards = sum(receiving_air_yards, na.rm = TRUE), 
    receiving_yards_after_catch = sum(receiving_yards_after_catch, na.rm = TRUE), 
    receiving_first_downs = sum(receiving_first_downs, na.rm = TRUE), 
    receiving_2pt_conversions = sum(receiving_2pt_conversions, na.rm = TRUE), 
    rushing_share = mean(rushing_share, na.rm = TRUE), 
    rushing_attempts = sum(rushing_attempts, na.rm = TRUE), 
    rushing_yards = sum(rushing_yards, na.rm = TRUE), 
    rushing_yards100 = sum(rushing_yards100, na.rm = TRUE), 
    rushing_tds = sum(rushing_tds, na.rm = TRUE), 
    rushing_fumbles = sum(rushing_fumbles, na.rm = TRUE), 
    rushing_fumbles_lost = sum(rushing_fumbles_lost, na.rm = TRUE), 
    rushing_first_downs = sum(rushing_first_downs, na.rm = TRUE), 
    rushing_epa = sum(rushing_epa, na.rm = TRUE), 
    rushing_2pt_conversions = sum(rushing_2pt_conversions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(season) %>% 
  group_by(
    player_id, 
    season_type) %>% 
  mutate(
    season_n = 1:n(), 
    season_last_n = n():1) %>% 
  ungroup() %>% 
  group_by(
    season_type, 
    season,
    position) %>% 
  mutate(
    fantasy_points_dk_rank = min_rank(-fantasy_points_dk), 
    fantasy_points_espn_rank = min_rank(-fantasy_points_espn)) %>% 
  ungroup() 

tes_by_season <- 
  tes_by_week %>% 
  group_by(
    season_type, 
    season, 
    player_id, 
    player_name, 
    position, 
    team,
    fantasy_teams) %>% 
  summarise(
    games = sum(n()), 
    fantasy_points_dk = sum(fantasy_points_dk, na.rm = TRUE), 
    fantasy_points_espn = sum(fantasy_points_espn, na.rm = TRUE), 
    fantasy_points_dk_rank_avg = mean(fantasy_points_dk_rank, na.rm = TRUE), 
    fantasy_points_espn_rank_avg = mean(fantasy_points_espn_rank, na.rm = TRUE), 
    team_plays_avg = mean(team_plays, na.rm = TRUE), 
    receiving_target_share = mean(receiving_target_share, na.rm = TRUE), 
    receiving_air_yards_share = mean(receiving_air_yards_share, na.rm = TRUE), 
    receiving_receptions = sum(receiving_receptions, na.rm = TRUE), 
    receiving_target = sum(receiving_target, na.rm = TRUE), 
    receiving_yards = sum(receiving_yards, na.rm = TRUE), 
    receiving_yards100 = sum(receiving_yards100, na.rm = TRUE), 
    receiving_tds = sum(receiving_tds, na.rm = TRUE), 
    receiving_fumbles = sum(receiving_fumbles, na.rm = TRUE), 
    receiving_fumbles_lost = sum(receiving_fumbles_lost, na.rm = TRUE), 
    receiving_air_yards = sum(receiving_air_yards, na.rm = TRUE), 
    receiving_yards_after_catch = sum(receiving_yards_after_catch, na.rm = TRUE), 
    receiving_first_downs = sum(receiving_first_downs, na.rm = TRUE), 
    receiving_2pt_conversions = sum(receiving_2pt_conversions, na.rm = TRUE), 
    rushing_share = mean(rushing_share, na.rm = TRUE), 
    rushing_attempts = sum(rushing_attempts, na.rm = TRUE), 
    rushing_yards = sum(rushing_yards, na.rm = TRUE), 
    rushing_yards100 = sum(rushing_yards100, na.rm = TRUE), 
    rushing_tds = sum(rushing_tds, na.rm = TRUE), 
    rushing_fumbles = sum(rushing_fumbles, na.rm = TRUE), 
    rushing_fumbles_lost = sum(rushing_fumbles_lost, na.rm = TRUE), 
    rushing_first_downs = sum(rushing_first_downs, na.rm = TRUE), 
    rushing_epa = sum(rushing_epa, na.rm = TRUE), 
    rushing_2pt_conversions = sum(rushing_2pt_conversions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(season) %>% 
  group_by(
    player_id, 
    season_type) %>% 
  mutate(
    season_n = 1:n(), 
    season_last_n = n():1) %>% 
  ungroup() %>% 
  group_by(
    season_type, 
    season,
    position) %>% 
  mutate(
    fantasy_points_dk_rank = min_rank(-fantasy_points_dk), 
    fantasy_points_espn_rank = min_rank(-fantasy_points_espn)) %>% 
  ungroup() 


# Replacement values
qbs_by_season <- 
  qbs_by_season %>% 
  inner_join(
    qbs_by_season %>% 
      filter(fantasy_points_dk_rank == fantasy_teams_val) %>% 
      select(season_type, season, replacement_qb_dk = fantasy_points_dk)) %>% 
  inner_join(
    qbs_by_season %>% 
      filter(fantasy_points_espn_rank == fantasy_teams_val) %>% 
      select(season_type, season, replacement_qb_espn = fantasy_points_espn)) %>% 
  mutate(
    fantasy_points_dk_above_rplcmnt = fantasy_points_dk - replacement_qb_dk, 
    fantasy_points_espn_above_rplcmnt = fantasy_points_espn - replacement_qb_espn) %>% 
  select(
    -replacement_qb_dk, 
    -replacement_qb_espn)

rbs_by_season <- 
  rbs_by_season %>% 
  inner_join(
    rbs_by_season %>% 
      filter(fantasy_points_dk_rank == fantasy_teams_val) %>% 
      select(season_type, season, replacement_rb_dk = fantasy_points_dk)) %>% 
  inner_join(
    rbs_by_season %>% 
      filter(fantasy_points_espn_rank == fantasy_teams_val) %>% 
      select(season_type, season, replacement_rb_espn = fantasy_points_espn)) %>% 
  mutate(
    fantasy_points_dk_above_rplcmnt = fantasy_points_dk - replacement_rb_dk, 
    fantasy_points_espn_above_rplcmnt = fantasy_points_espn - replacement_rb_espn) %>% 
  select(
    -replacement_rb_dk, 
    -replacement_rb_espn)

wrs_by_season <- 
  wrs_by_season %>% 
  inner_join(
    wrs_by_season %>% 
      filter(fantasy_points_dk_rank == fantasy_teams_val) %>% 
      select(season_type, season, replacement_wr_dk = fantasy_points_dk)) %>% 
  inner_join(
    wrs_by_season %>% 
      filter(fantasy_points_espn_rank == fantasy_teams_val) %>% 
      select(season_type, season, replacement_wr_espn = fantasy_points_espn)) %>% 
  mutate(
    fantasy_points_dk_above_rplcmnt = fantasy_points_dk - replacement_wr_dk, 
    fantasy_points_espn_above_rplcmnt = fantasy_points_espn - replacement_wr_espn) %>% 
  select(
    -replacement_wr_dk, 
    -replacement_wr_espn)

tes_by_season <- 
  tes_by_season %>% 
  inner_join(
    tes_by_season %>% 
      filter(fantasy_points_dk_rank == fantasy_teams_val) %>% 
      select(season_type, season, replacement_te_dk = fantasy_points_dk)) %>% 
  inner_join(
    tes_by_season %>% 
      filter(fantasy_points_espn_rank == fantasy_teams_val) %>% 
      select(season_type, season, replacement_te_espn = fantasy_points_espn)) %>% 
  mutate(
    fantasy_points_dk_above_rplcmnt = fantasy_points_dk - replacement_te_dk, 
    fantasy_points_espn_above_rplcmnt = fantasy_points_espn - replacement_te_espn) %>% 
  select(
    -replacement_te_dk, 
    -replacement_te_espn)


# Add next season's fantasy points 
qbs_by_season <- 
  qbs_by_season %>% 
  fantasy_points_next()

rbs_by_season <- 
  rbs_by_season %>% 
  fantasy_points_next()

wrs_by_season <- 
  wrs_by_season %>% 
  fantasy_points_next()

tes_by_season <- 
  tes_by_season %>% 
  fantasy_points_next()


# Write to disk 
write_parquet(qbs_by_season, '000_a_data_science_journey/results/qbs_by_season.parquet')
write_parquet(rbs_by_season, '000_a_data_science_journey/results/rbs_by_season.parquet')
write_parquet(wrs_by_season, '000_a_data_science_journey/results/wrs_by_season.parquet')
write_parquet(tes_by_season, '000_a_data_science_journey/results/tes_by_season.parquet')


rm(list = ls())

# # Finding players with multiple names 
# 
# players_multiple_names <- 
#   player_stats %>% 
#   distinct(player_id, player_name) %>% 
#   group_by(player_id) %>% 
#   count(player_id) %>% 
#   filter(n > 1)
# 
# multiple_names <- 
#   wrs_by_season %>% 
#   select(player_id, player_name, fantasy_points_espn) %>% 
#   inner_join(players_multiple_names) %>% 
#   arrange(-fantasy_points_espn)


