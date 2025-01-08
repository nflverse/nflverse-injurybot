#' @export
compute_post_text <- function(game_data){
  if (game_data$season_type == "POST"){
    paste0(
      "Injury report ahead of the #NFL ",
      compute_playoff_string(game_data$week_type),
      " #",
      teams$team_nick[teams$team_abbr == game_data$away_team],
      ifelse(game_data$neutral_site, "  vs.  ", "  @  "),
      "#",
      teams$team_nick[teams$team_abbr == game_data$home_team],
      " matchup"
    )
  } else {
    paste0(
      "Injury report ahead of the #NFL week ",
      game_data$week,
      " #",
      teams$team_nick[teams$team_abbr == game_data$away_team],
      ifelse(game_data$neutral_site, "  vs.  ", "  @  "),
      "#",
      teams$team_nick[teams$team_abbr == game_data$home_team],
      " matchup"
    )
  }
}

#' @export
compute_alt_text <- function(){
  paste0(
    "A table shows player names of two NFL teams and their injury status during the practice week."
  )
}
