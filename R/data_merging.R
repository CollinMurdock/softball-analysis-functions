
#############################################################
#### File containing functions for data merging/cleaning
#############################################################

# mergeGameBatterRoster
# a function that will merge game data with roster data
# inputs
#   game - df of the game data
#     required columns
#       batter - the batter number
#       pitcher - the pitcher number
#   homeRoster - df of the home team's roster
#     required columns
#       'Player Number' - the number of the player
#   homeCode - the team code for the home team
#   awayRoster - df of the home team's roster
#     required columns
#       'Player Number' - the number of the player
#   awayCode - the team code for the home team

mergeGameBatterRoster <- function(game, homeRoster, homeCode, awayRoster, awayCode) {
  # need to split the game into teams at bat
  home <- left_join(
    filter(game, game$at_bat == homeCode),
    homeRoster,
    by=c("batter" = "Player Number")
  )
  away <- left_join(
    filter(game, game$at_bat == awayCode),
    awayRoster,
    by=c("batter" = "Player Number")
  )
  rbind(home, away)
}

