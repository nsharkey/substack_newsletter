

library(tidyverse)
library(arrow)


rm(list = ls())

normalize <- 
  function(x, na.rm = TRUE) {
    round((x- min(x)) /(max(x)-min(x)), 5)
  }

qbs_by_week <- read_parquet('posts/000_first_post/results/qbs_by_week.parquet')
rbs_by_week <- read_parquet('posts/000_first_post/results/rbs_by_week.parquet')
wrs_by_week <- read_parquet('posts/000_first_post/results/wrs_by_week.parquet')
tes_by_week <- read_parquet('posts/000_first_post/results/tes_by_week.parquet')

colnames(qbs_by_week)


# Replacement level

qbs_by_week1 <- 
  qbs_by_week %>% 
  mutate(
    replacement_dk = case_when(
      position == 'QB' & fantasy_points_dk_rank == 20 ~ 'Y', 
      
      TRUE ~ 'N'
    )
  )




qbs_by_week1 <- 
  qbs_by_week %>% 
  group_by(
    season_type, 
    season, 
    position) %>% 
  mutate(
    fantasy_points_dk = normalize(fantasy_points_dk)
  )

qbs_by_week1 %>% 
  ggplot(aes(x = fantasy_points_dk, y = ))