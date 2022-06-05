

library(tidyverse)
library(arrow)

rm(list = ls())

# https://www.kaggle.com/competitions/nfl-big-data-bowl-2022/data


tracking <- read_parquet('000_a_data_science_journey/data/bdb2021/tracking_data2018_2020.parquet')
games <- read_parquet('000_a_data_science_journey/data/bdb2021/games.parquet')
plays <- read_parquet('000_a_data_science_journey/data/bdb2021/plays.parquet')
players <- read_parquet('000_a_data_science_journey/data/bdb2021/players.parquet')
PFFScoutingData <- read_parquet('000_a_data_science_journey/data/bdb2021/PFFScoutingData.parquet')








# colnames(tracking)



tracking_players <- 
  tracking %>% 
  distinct(
    team, 
    gameId, 
    playId, 
    nflId) 

tracking_players1 <- 
  tracking_players %>% 
  inner_join(
    y = players, 
    by = 'nflId'
  )





# Tracking data
# 
# Files tracking[season].csv contains player tracking data from season [season].
# 
# time: Time stamp of play (time, yyyy-mm-dd, hh:mm:ss)
# x: Player position along the long axis of the field, 0 - 120 yards. See Figure 1 below. (numeric)
# y: Player position along the short axis of the field, 0 - 53.3 yards. See Figure 1 below. (numeric)
# s: Speed in yards/second (numeric)
# a: Acceleration in yards/second^2 (numeric)
# dis: Distance traveled from prior time point, in yards (numeric)
# o: Player orientation (deg), 0 - 360 degrees (numeric)
# dir: Angle of player motion (deg), 0 - 360 degrees (numeric)
# event: Tagged play details, including moment of ball snap, pass release, pass catch, tackle, etc (text)
# nflId: Player identification number, unique across players (numeric)
# displayName: Player name (text)
# jerseyNumber: Jersey number of player (numeric)
# position: Player position group (text)
# team: Team (away or home) of corresponding player (text)
# frameId: Frame identifier for each play, starting at 1 (numeric)
# gameId: Game identifier, unique (numeric)
# playId: Play identifier, not unique across games (numeric)
# playDirection: Direction that the offense is moving (left or right)
# 
# 
# Game data
# 
# gameId: Game identifier, unique (numeric)
# season: Season of game
# week: Week of game
# gameDate: Game Date (time, mm/dd/yyyy)
# gameTimeEastern: Start time of game (time, HH:MM:SS, EST)
# homeTeamAbbr: Home team three-letter code (text)
# visitorTeamAbbr: Visiting team three-letter code (text)
# Play data
# 
# gameId: Game identifier, unique (numeric)
# playId: Play identifier, not unique across games (numeric)
# playDescription: Description of play (text)
# quarter: Game quarter (numeric)
# down: Down (numeric)
# yardsToGo: Distance needed for a first down (numeric)
# possessionTeam: Team punting, placekicking or kicking off the ball (text)
# specialTeamsPlayType: Formation of play: Extra Point, Field Goal, Kickoff or Punt (text)
# specialTeamsResult: Special Teams outcome of play dependent on play type: Blocked Kick Attempt, Blocked Punt, Downed, Fair Catch, Kick Attempt Good, Kick Attempt No Good, Kickoff Team Recovery, Muffed, Non-Special Teams Result, Out of Bounds, Return or Touchback (text)
# kickerId: nflId of placekicker, punter or kickoff specialist on play (numeric)
# returnerId: nflId(s) of returner(s) on play if there was a special teams return. Multiple returners on a play are separated by a ; (text)
# kickBlockerId: nflId of blocker of kick on play if there was a blocked field goal or blocked punt (numeric)
# yardlineSide: 3-letter team code corresponding to line-of-scrimmage (text)
# yardlineNumber: Yard line at line-of-scrimmage (numeric)
# gameClock: Time on clock of play (MM:SS)
# penaltyCodes: NFL categorization of the penalties that occurred on the play. A standard penalty code followed by a d means the penalty was on the defense. Multiple penalties on a play are separated by a ; (text)
# penaltyJerseyNumber: Jersey number and team code of the player committing each penalty. Multiple penalties on a play are separated by a ; (text)
# penaltyYards: yards gained by possessionTeam by penalty (numeric)
# preSnapHomeScore: Home score prior to the play (numeric)
# preSnapVisitorScore: Visiting team score prior to the play (numeric)
# passResult: Scrimmage outcome of the play if specialTeamsPlayResult is "Non-Special Teams Result" (C: Complete pass, I: Incomplete pass, S: Quarterback sack, IN: Intercepted pass, R: Scramble, ' ': Designed Rush, text)
# kickLength: Kick length in air of kickoff, field goal or punt (numeric)
# kickReturnYardage: Yards gained by return team if there was a return on a kickoff or punt (numeric)
# playResult: Net yards gained by the kicking team, including penalty yardage (numeric)
# absoluteYardlineNumber: Location of ball downfield in tracking data coordinates (numeric)
# Player data
# 
# nflId: Player identification number, unique across players (numeric)
# Height: Player height (text)
# Weight: Player weight (numeric)
# birthDate: Date of birth (YYYY-MM-DD)
# collegeName: Player college (text)
# Position: Player position (text)
# displayName: Player name (text)
# 
# 
# PFF Scouting data
# 
# gameId: Game identifier, unique (numeric)
# playId: Play identifier, not unique across games (numeric)
# snapDetail: On Punts, whether the snap was on target and if not, provides detail (H: High, L: Low, <: Left, >: Right, OK: Accurate Snap, text)
# operationTime: Timing from snap to kick on punt plays in seconds: (numeric)
# hangTime: Hangtime of player's punt or kickoff attempt in seconds. Timing is taken from impact with foot to impact with the ground or a player. (numeric)
# kickType: Kickoff or Punt Type (text).
# Possible values for kickoff plays:
# D: Deep - your normal deep kick with decent hang time
# F: Flat - different than a Squib in that it will have some hang time and no roll but has a lower trajectory and hang time than a Deep kick off
# K: Free Kick - Kick after a safety
# O: Obvious Onside - score and situation dictates the need to regain possession. Also the hands team is on for the returning team
# P: Pooch kick - high for hangtime but not a lot of distance - usually targeting an upman
# Q: Squib - low-line drive kick that bounces or rolls considerably, with virtually no hang time
# S: Surprise Onside - accounting for score and situation an onsides kick that the returning team doesn’t expect. Hands teams probably aren't on the field
# B: Deep Direct OOB - Kickoff that is aimed deep (regular kickoff) that goes OOB directly (doesn't bounce)
# Possible values for punt plays:
# N: Normal - standard punt style
# R: Rugby style punt
# A: Nose down or Aussie-style punts
# kickDirectionIntended: Intended kick direction from the kicking team's perspective - based on how coverage unit sets up and other factors (L: Left, R: Right, C: Center, text).
#                                                                                           kickDirectionActual: Actual kick direction from the kicking team's perspective (L: Left, R: Right, C: Center, text).
# returnDirectionIntended: The return direction the punt return or kick off return unit is set up for from the return team's perspective (L: Left, R: Right, C: Center, text).
#                                                                                           returnDirectionActual: Actual return direction from the return team's perspective (L: Left, R: Right, C: Center, text).
# missedTacklers: Jersey number and team code of player(s) charged with a missed tackle on the play. It will be reasonable to assume that he should have brought down the ball carrier and failed to do so. This situation does not have to entail contact, but it most frequently does. Missed tackles on a QB by a pass rusher are also included here. Multiple missed tacklers on a play are separated by a ; (text).
# assistTacklers: Jersey number and team code of player(s) assisting on the tackle. Multiple assist tacklers on a play are separated by a ; (text).
# tacklers: Jersey number and team code of player making the tackle (text).
# kickoffReturnFormation: 3 digit code indicating the number of players in the Front Wall, Mid Wall and Back Wall (text).
# gunners: Jersey number and team code of player(s) lined up as gunner on punt unit. Multiple gunners on a play are separated by a ; (text).
# puntRushers: Jersey number and team code of player(s) on the punt return unit with "Punt Rush" role for actively trying to block the punt. Does not include players crossing the line of scrimmage to engage in punt coverage players in a "Hold Up" role. Multiple punt rushers on a play are separated by a ; (text).
# specialTeamsSafeties: Jersey number and team code for player(s) with "Safety" roles on kickoff coverage and field goal/extra point block units - and those not actively advancing towards the line of scrimmage on the punt return unit. Multiple special teams safeties on a play are separated by a ; (text).
# vises: Jersey number and team code for player(s) with a "Vise" role on the punt return unit. Multiple vises on a play are separated by a ; (text).
# kickContactType: Detail on how a punt was fielded, or what happened when it wasn't fielded (text).
# Possible values:
# BB: Bounced Backwards
# BC: Bobbled Catch from Air
# BF: Bounced Forwards
# BOG: Bobbled on Ground
# CC: Clean Catch from Air
# CFFG: Clean Field From Ground
# DEZ: Direct to Endzone
# ICC: Incidental Coverage Team Contact
# KTB: Kick Team Knocked Back
# KTC: Kick Team Catch
# KTF: Kick Team Knocked Forward
# MBC: Muffed by Contact with Non-Designated Returner
# MBDR: Muffed by Designated Returner
# OOB: Directly Out Of Bounds
# 
