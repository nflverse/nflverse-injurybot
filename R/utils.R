status_abbr <- c(
  "Did Not Participate In Practice" = "DNP",
  "Limited Participation in Practice" = "LTD",
  "Full Participation in Practice" = "FULL",
  "Questionable" = "QST",
  "Doubtful" = "DBT",
  "Out" = "OUT"
)

game_designation <- c(
  "Questionable (uncertain as to whether the player will play)" = "QST",
  "Doubtful (unlikely to play)" = "DBT",
  "Out (will not play)" = "OUT"
)

compute_status_string <- function(status, reason1, reason2){
  if (all(is.na(status))) return("")
  stat_chars <- max(nchar(status_abbr[status]), na.rm = TRUE)
  paste0(
    paste0("<span style='width:", stat_chars + 1, "ch;float:left'>"),
    data.table::fifelse(!is.na(status), status_abbr[status], ""),
    "</span>",
    data.table::fifelse(!is.na(reason1), "| ", ""),
    data.table::fifelse(!is.na(reason1), reason1, ""),
    data.table::fifelse(!is.na(reason2), "/", ""),
    data.table::fifelse(!is.na(reason2), reason2, "")
  ) |>
    dplyr::na_if("")
}

compute_title_string <- function(g){
  paste0(
    teams$team_name[teams$team_abbr == g$away_team],
    ifelse(g$neutral_site, "  vs.  ", "  @  "),
    teams$team_name[teams$team_abbr == g$home_team]
  )
}

compute_subtitle_string <- function(g){
  ko <- withr::with_locale(
    new = c("LC_TIME" = "C"),
    code = {
      g$time |>
        lubridate::as_datetime() |>
        format(
          format = "%a, %b %e, %Y  %I:%M %p",
          tz = "America/New_York",
          usetz = TRUE
        )
    }
  )
  if (g$season_type == "POST"){
    paste0(
      "Injury Report, ",
      g$season,
      ", ",
      compute_playoff_string(g$week_type),
      ".  Kickoff: ",
      ko
    )
  } else {
    paste0(
      "Injury Report, ",
      g$season,
      ", Week ",
      g$week,
      ".  Kickoff: ",
      ko
    )
  }
}

compute_playoff_string <- function(game_type){
  switch (game_type,
    "WC" = "Wild Card",
    "DIV" = "Divisional Round",
    "CON" = "Championship Game",
    "SB" = "Super Bowl",
  )
}

injury_table_theme <- function(gt_object, ...){
  gt_object |>
    gt::opt_all_caps()  |>
    gt::opt_table_font(
      font = list(
        gt::google_font("Roboto Condensed"),
        gt::default_fonts()
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top", color = "black", weight = gt::px(0)
      ),
      locations = gt::cells_column_labels(
        columns = gt::everything()
      )
    ) |>
    gt::tab_style(
      locations = gt::cells_title(groups = "title"),
      style = gt::cell_text(
        weight = "bold",
        font = list(
          gt::google_font("Avenir Next"),
          gt::default_fonts()
        ))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom", color = "black", weight = gt::px(1)
      ),
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_options(
      column_labels.background.color = "white",
      heading.border.bottom.style = "none",
      table.border.top.width = gt::px(3),
      table.border.top.style = "none", #transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "normal",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = gt::px(1),
      row_group.border.bottom.color = "white",
      stub.border.color = "white",
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(3),
      source_notes.border.lr.style = "none",
      source_notes.background.color = "gray30",
      footnotes.background.color = "gray30",
      table.font.size = 9,
      heading.title.font.size = 18,
      heading.subtitle.font.size = 11,
      column_labels.font.size = 8,
      footnotes.font.size = 8,
      heading.align = "center",
      heading.background.color = "gray30"
    ) |>
    gt::tab_options(...)
}
