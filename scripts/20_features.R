# 020_features.R

# __ Clean environment _________________________________________________________
to_keep <- c("raw_games", "games", "all_teams")
rm(list = setdiff(ls(), to_keep))

# __ Set up random framework ___________________________________________________
N <- 10000
Seed <- 1807

# __ Useful functions __________________________________________________________
prepare_data <- function (df, team_ref) {
  df |>
    mutate(
      team_home = factor(team_home, levels = all_teams),
      team_away = factor(team_away, levels = all_teams),
      team_home = relevel(team_home, ref = team_ref),
      team_away = relevel(team_away, ref = team_ref)
    )
}

mean_points <- function (train) {
  mean_points <- left_join(
    train |>
      group_by(team = team_home) |>
      summarise(mean_diff_home = mean(score_diff)),
    train |>
      group_by(team = team_away) |>
      summarise(mean_diff_away = mean(-score_diff)),
    by = "team"
  ) |>
    mutate(mean_diff = (mean_diff_home + mean_diff_away) / 2) |>
    arrange(desc(mean_diff))
  return(head(mean_points, 4))
}