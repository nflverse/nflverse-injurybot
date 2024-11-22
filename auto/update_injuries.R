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

one_game <- nflverse.injurybot::evaluate_game("2024_12_KC_CAR", week_games, injury_data)
nflverse.injurybot::compute_game_table(one_game, week_games[week_games$nflverse_id == "2024_12_KC_CAR",])

  # bskyr::bs_post(
  #   text = compute_post_text(game_data),
  #   images = file_name,
  #   images_alt = compute_alt_text()
  # )
