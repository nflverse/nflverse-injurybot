teams <- nflreadr::load_teams(current = FALSE)
usethis::use_data(teams, overwrite = TRUE, internal = TRUE)
