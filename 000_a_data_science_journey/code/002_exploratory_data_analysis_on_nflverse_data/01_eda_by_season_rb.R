

library(tidyverse)
library(arrow)
library(DataExplorer)
library(correlationfunnel)

rm(list = ls())


# normalize <- function(x) {
#   (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# }
# 
# rbs_by_season <- read_parquet('000_a_data_science_journey/results/rbs_by_season.parquet')
# 
# qualified_players <-
#   rbs_by_season %>%
#   select(
#     season_type,
#     season,
#     player_id,
#     player_name,
#     fantasy_points_dk,
#     fantasy_points_espn,
#     rushing_attempts) %>%
#   group_by(
#     season_type,
#     season) %>%
#   mutate(across(fantasy_points_dk:rushing_attempts, normalize)) %>% #normalize to adjust for "era"
#   mutate(
#     modeling_observation = case_when(
#       fantasy_points_dk >= median(fantasy_points_dk) ~ 'Y',
#       fantasy_points_espn >= median(fantasy_points_espn) ~ 'Y',
#       rushing_attempts >= median(rushing_attempts) ~ 'Y',
#       TRUE ~ 'N')) %>%
#   ungroup() %>%
#   filter(modeling_observation == 'Y') %>%
#   distinct(
#     season_type,
#     season,
#     player_id,
#     player_name)
# 
# rbs_by_season_qualified <-
#   rbs_by_season %>%
#   inner_join(qualified_players)
# 
# rm(qualified_players)
# 
# View(
#   rbs_by_season_qualified %>%
#     count(season))
# 
# write_parquet(rbs_by_season_qualified, '000_a_data_science_journey/results/rbs_by_season_qualified.parquet')

rbs_by_season_qualified <- read_parquet('000_a_data_science_journey/results/rbs_by_season_qualified.parquet')


rbs_by_season_qualified %>%
  drop_na(fantasy_points_dk_next) %>% 
  select(
    fantasy_points_dk_next, 
    fantasy_points_dk, 
    starts_with('team_'), 
    starts_with('rushing_'), 
    starts_with('receiving_')) %>% 
  correlate(target = fantasy_points_dk_next) %>% 
  filter(feature != 'fantasy_points_dk_next') %>% 
  # plot_correlation_funnel()
  ggplot(aes(x = correlation, y = feature)) +
  geom_jitter() +
  xlim(-1,1)

create_report(
  data = 
    rbs_by_season_qualified %>% 
    select(
      fantasy_points_dk_next, 
      fantasy_points_dk, 
      starts_with('team_'), 
      starts_with('rushing_'), 
      starts_with('receiving_')), 
  output_file = 'rbs_by_season_qualified.html', 
  output_dir = 'results/')





