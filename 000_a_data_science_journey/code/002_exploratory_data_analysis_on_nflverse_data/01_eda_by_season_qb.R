

library(tidyverse)
library(arrow)
library(DataExplorer)
library(correlationfunnel)

rm(list = ls())


# normalize <- function(x) {
#   (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# }
# 
# qbs_by_season <- read_parquet('000_a_data_science_journey/results/qbs_by_season.parquet')
# 
# qualified_players <- 
#   qbs_by_season %>% 
#   select(
#     season_type, 
#     season, 
#     player_id, 
#     player_name, 
#     fantasy_points_dk, 
#     fantasy_points_espn, 
#     passing_attempts) %>% 
#   group_by(
#     season_type, 
#     season) %>% 
#   mutate(across(fantasy_points_dk:passing_attempts, normalize)) %>% #normalize to adjust for "era"
#   mutate(
#     modeling_observation = case_when(
#       fantasy_points_dk >= median(fantasy_points_dk) ~ 'Y',
#       fantasy_points_espn >= median(fantasy_points_espn) ~ 'Y',
#       passing_attempts >= median(passing_attempts) ~ 'Y',
#       TRUE ~ 'N')) %>% 
#   ungroup() %>%
#   filter(modeling_observation == 'Y') %>% 
#   distinct( 
#     season_type, 
#     season, 
#     player_id, 
#     player_name) 
# 
# qbs_by_season_qualified <- 
#   qbs_by_season %>% 
#   inner_join(qualified_players)
# 
# rm(qualified_players)
# 
# View(
#   qbs_by_season_qualified %>% 
#     count(season))
# 
# write_parquet(qbs_by_season_qualified, '000_a_data_science_journey/results/qbs_by_season_qualified.parquet')

qbs_by_season_qualified <- read_parquet('000_a_data_science_journey/results/qbs_by_season_qualified.parquet')


qbs_by_season_qualified %>%
  drop_na(fantasy_points_dk_next) %>% 
  select(
    fantasy_points_dk_next, 
    fantasy_points_dk, 
    starts_with('team_'), 
    starts_with('passing_'), 
    starts_with('rushing_')) %>% 
  correlate(target = fantasy_points_dk_next) %>% 
  filter(feature != 'fantasy_points_dk_next') %>% 
  # plot_correlation_funnel()
  ggplot(aes(x = correlation, y = feature)) +
  geom_jitter() +
  xlim(-1,1)

create_report(
  data = 
    qbs_by_season_qualified %>% 
    select(
      fantasy_points_dk_next, 
      fantasy_points_dk, 
      starts_with('team_'), 
      starts_with('passing_'), 
      starts_with('rushing_')), 
  output_file = 'qbs_by_season_qualified.html', 
  output_dir = 'results/')





