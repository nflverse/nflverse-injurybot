#' @export
compute_post_text <- function(game_data){
  paste0(
    "Today's #NFL injury report ahead of week ",
    game_data$week,
    " #",
    teams$team_nick[teams$team_abbr == game_data$away_team],
    ifelse(game_data$neutral_site, "  vs.  ", "  @  "),
    "#",
    teams$team_nick[teams$team_abbr == game_data$home_team],
    " matchup"
  )
}

#' @export
compute_alt_text <- function(){
  paste0(
    "A table shows player names of two NFL teams and their injury status during the practice week."
  )
}
