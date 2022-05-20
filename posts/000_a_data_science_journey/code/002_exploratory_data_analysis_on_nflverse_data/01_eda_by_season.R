

library(tidyverse)
library(arrow)
library(DataExplorer)

rm(list = ls())

# https://bookdown.org/rdpeng/exdata/k-means-clustering.html#using-the-kmeans-function

# Fantasy league settings
fantasy_teams <- 10 


# Read in data and add next season's fantasy points 
qbs_by_season <- 
  read_parquet('posts/000_a_data_science_journey/results/qbs_by_season.parquet') %>% 
  filter(season_type == 'REG') %>% 
  filter(fantasy_points_dk_rank <= fantasy_teams * 3)
  
rbs_by_season <- 
  read_parquet('posts/000_a_data_science_journey/results/rbs_by_season.parquet') %>% 
  filter(season_type == 'REG') %>% 
  filter(fantasy_points_dk_rank <= fantasy_teams * 6)

wrs_by_season <- 
  read_parquet('posts/000_a_data_science_journey/results/wrs_by_season.parquet') %>% 
  filter(season_type == 'REG') %>% 
  filter(fantasy_points_dk_rank <= fantasy_teams * 6)

tes_by_season <- 
  read_parquet('posts/000_a_data_science_journey/results/tes_by_season.parquet') %>% 
  filter(season_type == 'REG') %>% 
  filter(fantasy_points_dk_rank <= fantasy_teams * 3)


# # Create EDA reports
# create_report(qbs_by_season, output_file = 'eda_qbs_by_season', output_dir = 'posts/000_a_data_science_journey/results/')
# create_report(rbs_by_season, output_file = 'eda_rbs_by_season', output_dir = 'posts/000_a_data_science_journey/results/')
# create_report(wrs_by_season, output_file = 'eda_wrs_by_season', output_dir = 'posts/000_a_data_science_journey/results/')
# create_report(tes_by_season, output_file = 'eda_tes_by_season', output_dir = 'posts/000_a_data_science_journey/results/')


# QBs 


colnames(qbs_by_season)

qbs_by_season_qualified_dk <- 
  qbs_by_season %>% 
  # drop_na() %>%
  filter(qualified_dk == 'Y')

qbs_by_season_qualified_dk_hclust <- 
  qbs_by_season_qualified_dk %>% 
  select(
  -season_type, 
  -season, 
  -player_id, 
  -player_name, 
  -position, 
  -team, 
  -fantasy_points_dk_next,
  -fantasy_points_espn_next,
  -fantasy_points_dk_above_rplcmnt_next,
  -fantasy_points_espn_above_rplcmnt_next)

qbs_by_season_qualified_dk$cluster <- as.factor(kmeans(qbs_by_season_qualified_dk_hclust, centers = 5)$cluster)

qbs_by_season_qualified_espn <- 
  qbs_by_season %>% 
  # drop_na() %>% 
  filter(qualified_espn == 'Y')

qbs_by_season_qualified_dk %>% 
  ggplot(aes(x = fantasy_points_dk, y = fantasy_points_dk_next)) + 
  xlim(100, 500) +
  ylim(100, 500) +
  geom_jitter()

qbs_by_season_qualified_dk %>% 
  ggplot(aes(x = fantasy_points_dk)) + 
  # geom_histogram(bins = 100)
  geom_density()

qbs_by_season_qualified_espn %>% 
  ggplot(aes(x = fantasy_points_espn, y = fantasy_points_espn_next)) + 
  xlim(100, 500) +
  ylim(100, 500) +
  geom_jitter()

qbs_by_season_qualified_dk %>% 
  ggplot(aes(x = passing_attempts, y = fantasy_points_dk_next)) + 
  # xlim(100, 500) +
  # ylim(0, 500) +
  geom_jitter()


quantile(qbs_by_season$fantasy_points_dk, c(0.1, 0.15, 0.2, 0.25, 0.33, 0.5, 0.66, 0.75, 0.9))

summary(lm(fantasy_points_dk_next ~ fantasy_points_dk + sqrt(fantasy_points_dk) + passing_attempts + sqrt(passing_attempts), qbs_by_season_qualified_dk))
summary(lm(fantasy_points_espn_next ~ fantasy_points_espn, qbs_by_season_qualified_espn %>% drop_na()))

summary(lm(fantasy_points_dk_next ~ passing_attempts, qbs_by_season_qualified_dk %>% drop_na()))



# sample_qbs <- 
#   qbs_by_season %>% 
#   filter(season == 2021) %>% 
#   distinct(player_id)
# 
# qbs_by_season_sample <-
#   qbs_by_season %>%
#   # inner_join(sample_qbs) %>% # %>% filter(player_name == 'M.Stafford')
#   filter(fantasy_points_dk >= 150) %>% 
#   group_by(season_type, season) %>% 
#   mutate(
#     # fjksdh = fantasy_points_dk - median(fantasy_points_dk)
#     filter(fantasy_points_dk_replacement = 
#     )


qbs_by_season_sample %>% 
  filter(fantasy_points_dk >= 150) %>% # 16 * 10 ~ 150 
  ggplot(aes(x = fantasy_points_dk)) + 
  # geom_histogram(bins = 100)
  geom_density()


quantile(round(qbs_by_season_sample$rushing_attempts / qbs_by_season_sample$games, 1), c(0.70, 0.75, 0.8, 0.9))

qbs_by_season_sample %>% filter(rushing_yards >= 175) %>% distinct(player_name) 

qbs_by_season_sample %>% 
  filter(fantasy_points_dk >= 150) %>% # 16 * 10 ~ 150 
  mutate(rushing_attempts_per_game = round(rushing_attempts / games, 1)) %>% 
  ggplot(aes(x = fantasy_points_dk, fill = rushing_attempts_per_game)) + 
  geom_histogram(bins = 100)
  # geom_jitter()







