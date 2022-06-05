

library(tidyverse)
library(arrow)


rm(list = ls())


# Current Season
season <- 2021


# # Run if you need historic seasons
# source('000_a_data_science_journey/code/00_data_collection_historic.R')
# rm(list = setdiff(ls(), 'season'))


# Play-by-Play Data
write_parquet(
  bind_rows(
    read_rds(paste0('https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_', season, '.rds')), 
    read_parquet('000_a_data_science_journey/data/historic/pbp_historic.parquet')), 
  '000_a_data_science_journey/data/play_by_play.parquet')



# Player Stats by Week 
write_parquet(
  read_rds('https://github.com/nflverse/nflverse-data/releases/download/player_stats/player_stats.rds'), 
  '000_a_data_science_journey/data/player_stats.parquet')


# Rosters 
write_parquet(
  bind_rows(
    read_rds(paste0('https://github.com/nflverse/nflverse-data/releases/download/rosters/roster_', season, '.rds')), 
    read_parquet('000_a_data_science_journey/data/historic/roster_historic.parquet')), 
  '000_a_data_science_journey/data/roster.parquet')


# Snap Counts 
write_parquet(
  bind_rows(
    read_rds(paste0('https://github.com/nflverse/nflverse-data/releases/download/snap_counts/snap_counts_', season, '.rds')), 
    read_parquet('000_a_data_science_journey/data/historic/snap_counts_historic.parquet')), 
  '000_a_data_science_journey/data/snap_counts.parquet')


# Injuries
write_parquet(
  bind_rows(
    read_rds(paste0('https://github.com/nflverse/nflverse-data/releases/download/injuries/injuries_', season, '.rds')), 
    read_parquet('000_a_data_science_journey/data/historic/injuries_historic.parquet')), 
  '000_a_data_science_journey/data/injuries.parquet')


rm(list = ls())


