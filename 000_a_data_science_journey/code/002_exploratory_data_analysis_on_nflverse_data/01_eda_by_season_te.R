

library(tidyverse)
library(arrow)
library(DataExplorer)
library(correlationfunnel)

rm(list = ls())


# normalize <- function(x) {
#   (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# }
# 
# tes_by_season <- read_parquet('000_a_data_science_journey/results/tes_by_season.parquet')
# 
# qualified_players <-
#   tes_by_season %>%
#   select(
#     season_type,
#     season,
#     player_id,
#     player_name,
#     fantasy_points_dk,
#     fantasy_points_espn,
#     receiving_target) %>%
#   group_by(
#     season_type,
#     season) %>%
#   mutate(across(fantasy_points_dk:receiving_target, normalize)) %>% #normalize to adjust for "era"
#   mutate(
#     modeling_observation = case_when(
#       fantasy_points_dk >= median(fantasy_points_dk) ~ 'Y',
#       fantasy_points_espn >= median(fantasy_points_espn) ~ 'Y',
#       receiving_target >= median(receiving_target) ~ 'Y',
#       TRUE ~ 'N')) %>%
#   ungroup() %>%
#   filter(modeling_observation == 'Y') %>%
#   distinct(
#     season_type,
#     season,
#     player_id,
#     player_name)
# 
# tes_by_season_qualified <-
#   tes_by_season %>%
#   inner_join(qualified_players)
# 
# rm(qualified_players)
# 
# View(
#   tes_by_season_qualified %>%
#     count(season))
# 
# write_parquet(tes_by_season_qualified, '000_a_data_science_journey/results/tes_by_season_qualified.parquet')

tes_by_season_qualified <- read_parquet('000_a_data_science_journey/results/tes_by_season_qualified.parquet')


tes_by_season_qualified %>%
  drop_na(fantasy_points_dk_next) %>% 
  select(
    fantasy_points_dk_next, 
    fantasy_points_dk, 
    starts_with('team_'), 
    starts_with('receiving_'), 
    starts_with('rushing_')) %>% 
  correlate(target = fantasy_points_dk_next) %>% 
  filter(feature != 'fantasy_points_dk_next') %>% 
  # plot_correlation_funnel()
  ggplot(aes(x = correlation, y = feature)) +
  geom_jitter() +
  xlim(-1,1)

create_report(
  data = 
    tes_by_season_qualified %>% 
    select(
      fantasy_points_dk_next, 
      fantasy_points_dk, 
      starts_with('team_'), 
      starts_with('receiving_'), 
      starts_with('rushing_')), 
  output_file = 'tes_by_season_qualified.html', 
  output_dir = 'results/')





