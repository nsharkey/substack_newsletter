

library(tidyverse)
library(arrow)
library(tictoc)


rm(list = ls())

qbs_by_week_qualified <- 
  read_parquet('000_a_data_science_journey/results/qbs_by_week.parquet') %>% # Need too qualify? 
  filter(season_type == 'REG') %>% 
  filter(player_id == '00-0034857') # Josh Allen


qbs_by_week_qualified %>% 
  select(fantasy_points_dk) %>% 
  sample_n(16, replace = TRUE) %>% 
  summarize(fantasy_points_dk = mean(fantasy_points_dk))



