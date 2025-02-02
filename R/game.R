#' @export
evaluate_game <- function(game_id, schedule, injuries){
  # game_id <- "2024_12_PIT_CLE"

  game <- schedule[schedule$nflverse_id == game_id, ]

  game_injuries <- injuries |>
    dplyr::filter(team %in% c(game$away_team, game$home_team))

  status_dates <- c(sort(unique(game_injuries$date)), game$game_day)
  status_days <- lubridate::wday(status_dates, label = TRUE, week_start = 1, abbr = FALSE, locale = "C") |>
    as.character() |>
    rlang::set_names(status_dates)

  game_injuries |>
    dplyr::mutate(
      dplyr::across(c(ps, gs), ~ stringr::str_wrap(.x, width = 65)),
      dplyr::across(c(ps, gs), ~ stringr::str_replace_all(.x, "\\n", "<br>"))
    ) |>
    tidyr::pivot_wider(
      names_from = date,
      values_from = ps,
      names_sort = TRUE
    ) |>
    dplyr::relocate(gs, .after = dplyr::last_col()) |>
    dplyr::rename_with(
      .fn = ~ status_days[.x],
      .cols = dplyr::any_of(names(status_days))
    ) |>
    dplyr::select(-c(season, game_type, week))
}

#' @export
compute_game_table <- function(injury_data, game_data){
  injury_teams <- unique(injury_data$team)
  row_group_order <- c(game_data$away_team, game_data$home_team)
  row_group_order <- row_group_order[row_group_order %in% injury_teams]
  gt::gt(injury_data, groupname_col = "team") |>
    gt::sub_missing(missing_text = "") |>
    injury_table_theme() |>
    gt::cols_label(
      position = "pos",
      full_name = "player",
      gs = "Game Status"
    ) |>
    gt::row_group_order(row_group_order) |>
    gt::tab_header(
      title = compute_title_string(game_data),
      subtitle = compute_subtitle_string(game_data)
    ) |>
    nflplotR::gt_nfl_wordmarks(locations = gt::cells_row_groups()) |>
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_row_groups()
    ) |>
    gt::fmt_markdown() |>
    gt::data_color(
      columns = gs,
      fn = function(x){
        designation <- stringr::str_extract(x, "OUT|QST|DBT")
        dplyr::case_when(
          designation == "OUT" ~ "#D91656",
          designation == "DBT" ~ "#640D5F",
          designation == "QST" ~ "#FFF176FF",
          TRUE ~ "transparent"
        )
      },
      target_columns = gt::any_of(c("full_name", "gs"))
    ) |>
    gt::tab_footnote(
      gt::html(paste0(
        paste(
          paste(paste0("<b>", status_abbr[[1]], "</b>"), names(status_abbr)[[1]], sep = " = "),
          paste(paste0("<b>", status_abbr[[2]], "</b>"), names(status_abbr)[[2]], sep = " = "),
          paste(paste0("<b>", status_abbr[[3]], "</b>"), names(status_abbr)[[3]], sep = " = "),
          sep = " // "
        ),
        "<br>",
        paste(
          paste(paste0("<b>", game_designation[[1]], "</b>"), names(game_designation)[[1]], sep = " = "),
          paste(paste0("<b>", game_designation[[2]], "</b>"), names(game_designation)[[2]], sep = " = "),
          paste(paste0("<b>", game_designation[[3]], "</b>"), names(game_designation)[[3]], sep = " = "),
          sep = " // "
        ))
      )
    )
}
