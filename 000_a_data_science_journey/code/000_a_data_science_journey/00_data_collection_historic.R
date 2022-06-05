

library(tidyverse)
library(arrow)


rm(list = ls())


# Historical Seasons
seasons <- 1999:2020


# Play-by-Play Data 
pbp <- NULL 
print("Play-by-Play Data")
for (s in 1:length(seasons)) {
  
  print(seasons[s])
  
  df <- read_rds(paste0('https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_', seasons[s], '.rds'))
  pbp <- bind_rows(pbp, df) 
  
  rm(list = c('df', 's'))
  
}

write_parquet(pbp, '000_a_data_science_journey/data/historic/pbp_historic.parquet')


# Player Stats by Week 
# # Pulled every week 


# Rosters 
roster <- NULL 
print("Rosters Data")
for (s in 1:length(seasons)) {
  
  print(seasons[s])
  
  df <- read_rds(paste0('https://github.com/nflverse/nflverse-data/releases/download/rosters/roster_', seasons[s], '.rds'))
  roster <- bind_rows(roster, df) 
  
  rm(list = c('df', 's'))
  
}

write_parquet(roster, '000_a_data_science_journey/data/historic/roster_historic.parquet')


# Snap Counts 
snap_counts <- NULL 
print("Snap Counts Data")
for (s in 1:length(seasons)) {
  
  if (seasons[s] >= 2012){ # Data only goes back to 2012
    
    print(seasons[s])
    
    df <- read_rds(paste0('https://github.com/nflverse/nflverse-data/releases/download/snap_counts/snap_counts_', seasons[s], '.rds'))
    snap_counts <- bind_rows(snap_counts, df)

    rm(list = c('df', 's'))
    
  }
}

write_parquet(snap_counts, '000_a_data_science_journey/data/historic/snap_counts_historic.parquet')


# Draft Picks 
print("Draft Picks Data")
write_parquet(
  read_rds('https://github.com/nflverse/nflverse-data/releases/download/draft_picks/draft_picks.rds'), 
  '000_a_data_science_journey/data/historic/draft_picks.parquet')


# Injuries
injuries <- NULL 
print("Injuries Data")
for (s in 1:length(seasons)) {
  
  if (seasons[s] >= 2009){ # Data only goes back to 2012
    
    print(seasons[s])
    
    df <- read_rds(paste0('https://github.com/nflverse/nflverse-data/releases/download/injuries/injuries_', seasons[s], '.rds'))
    injuries <- bind_rows(injuries, df)
    
    rm(list = c('df', 's'))
    
  }
}

write_parquet(injuries, '000_a_data_science_journey/data/historic/injuries_historic.parquet')


# Combine
print("Combine Data")
write_parquet(
  read_rds('https://github.com/nflverse/nflverse-data/releases/download/combine/combine.rds'), 
  '000_a_data_science_journey/data/historic/combine.parquet')


# Contracts
print("Contracts Data")
contracts <- read_rds('https://github.com/nflverse/nflverse-data/releases/download/contracts/historical_contracts.rds')

# # Summary 
contracts_summary <- contracts %>% select(-season_history)
write_parquet(contracts_summary, '000_a_data_science_journey/data/historic/contracts_summary.parquet')


rm(list = ls())



# # # Detail 
# contracts_detail <- NULL
# for (p in 1:nrow(contracts)) {
# 
#   df <-
#     cbind(
#       player = contracts$player[[p]],
#       position = contracts$position[[p]],
#       contracts$season_history[[p]])
# 
#   contracts_detail <- bind_rows(contracts_detail, df)
# 
#   rm(list = c('df', 'p'))
# 
# }
# 
# write_parquet(contracts_detail, '000_a_data_science_journey/data/historic/contracts_detail.parquet')


