#' @export
fetch_injuries <- function(season = nflreadr::most_recent_season(),
                           season_type = c("REG", "POST"),
                           week = nflreadr::get_current_week()){
  season_type <- rlang::arg_match(season_type)
  nfldx::nfldx_injuries(
    season = season,
    season_type = season_type,
    weeks = week
  ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(practice1, practice2, injury1, injury2),
        .fns = function(x) {
          stringr::str_remove_all(
            x, stringr::regex("not injury related - ", ignore_case = TRUE)
          ) |>
            stringr::str_to_title()
        }
      ),
      ps = compute_status_string(practices_status, practice1, practice2),
      gs = compute_status_string(injury_status, injury1, injury2)
    ) |>
    dplyr::select(
      season, game_type, team = club_code, week, position, full_name,
      date = practices_date, ps, gs
    )
}

#' @export
fetch_games <- function(season = nflreadr::most_recent_season()){
  nflapi::nflapi_games(season = season) |>
    nflapi::nflapi_parse_games() |>
    dplyr::mutate(
      dplyr::across(c(away_team, home_team), ~ nflreadr::clean_team_abbrs(.x)),
      game_time = lubridate::as_datetime(time),
      game_day = format(game_time, "%Y-%m-%d", tz = "America/New_York")
    )
}

#' @export
fetch_from_release <- function(game_id){
  to_load <- paste0(
    "https://github.com/nflverse/nflverse-injurybot/releases/download/injuries_",
    substr(game_id, 1, 4), "/", game_id, ".rds"
  )
  tmp_file <- tempfile(game_id, fileext = ".rds")
  on.exit(unlink(tmp_file))
  download <- curl::curl_fetch_disk(to_load, tmp_file)
  if (download$status_code != 200) return(data.frame())
  readRDS(tmp_file)
}
