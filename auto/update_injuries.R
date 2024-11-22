season <- nflreadr::get_current_season(roster = FALSE)

# Fetch All Game Data
all_season_games <- nflverse.injurybot::fetch_games(season)

# Game Data of current week's games
week_games <- all_season_games |>
  dplyr::filter(!grepl("FINAL", status)) |>
  dplyr::filter(week == min(week))

# Extract week from this week's games and resolve the week number for api
week <- unique(week_games$week)
season_type <- unique(week_games$season_type)
max_reg <- all_season_games |>
  dplyr::filter(season_type == "REG") |>
  dplyr::pull(week) |>
  max()
week <- if (week > max_reg) week - max_reg else week

# Fetch Injury Data
injury_data <- nflverse.injurybot::fetch_injuries(
  season = season,
  season_type = season_type,
  week = week
)

# Process every game of current week
for (game_id in week_games$nflverse_id) {
  current_game <- nflverse.injurybot::evaluate_game(game_id, week_games, injury_data)
  last_run <- nflverse.injurybot::fetch_from_release(game_id)
  game_data <- week_games[week_games$nflverse_id == game_id,]

  # compare to last run if the file is available. If not, it's likely the first run
  if (identical(current_game, last_run)){
    cli::cli_alert_info("No new data for {.val {game_id}}")
    next
  }

  # If we reach this point, data has changed and we would like to post an update
  tbl <- nflverse.injurybot::compute_game_table(current_game, game_data)
  file_name <- gt::gtsave(tbl, paste0("auto/", game_id, ".png"), zoom = 4, quiet = TRUE)

  # Just in case the code is running too fast,
  # we take a nap here to protect bsky servers
  Sys.sleep(3)
  post <- bskyr::bs_post(
    text = nflverse.injurybot::compute_post_text(game_data),
    images = file_name,
    images_alt = nflverse.injurybot::compute_alt_text()
  )
  cli::cli_alert_success("Posted update for {.val {game_id}}")

  # upload new data if everything worked fine until this point
  saveRDS(current_game, paste0("auto/", game_id, ".rds"))
  nflversedata:::gh_cli_release_upload(
    files = paste0("auto/", game_id, ".rds"),
    tag = paste0("injuries_", season),
    repo = "nflverse/nflverse-injurybot"
  )
}

