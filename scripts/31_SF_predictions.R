# 31_SF_predictions.R

# __ Clean environment _________________________________________________________
to_keep <- c(
  "raw_games", "all_teams",
  "games", "N", "Seed",
  "prepare_data", "plot_SF_heatmaps"
)
rm(list = setdiff(ls(), to_keep))

# __ Prepare data ______________________________________________________________
train <- prepare_data(
  df = bind_rows(
    games$regular$`26`,
    games$playoffs$`26`
  ),
  team_ref = "OLY"
) |>
  arrange(date)

train_with25 <- prepare_data(
  df = bind_rows(
    games$regular$`25`,
    games$playoffs$`25`,
    games$regular$`26`,
    games$playoffs$`26`
  ),
  team_ref = "OLY"
) |>
  arrange(date)

# __ Train models ______________________________________________________________
fit <- lm(
  score_diff ~ team_home + team_away,
  contrasts = list(
    team_home = contr.sum,
    team_away = contr.sum
  ),
  data = train
)

fit_with25 <- lm(
  score_diff ~ team_home + team_away,
  contrasts = list(
    team_home = contr.sum,
    team_away = contr.sum
  ),
  data = train_with25
)

# __ Simulation best-of-3 ______________________________________________________

# Simulate a BO3 series between team_A and team_B following the format :
# (home, away, home) for team_A. Return the qualification probabilities and the
# games summaries.

simulate_bo3 <- function(n, seed, model, serie_name, team_A, team_B) {
  
  set.seed(seed)
  
  sigma <- summary(model)$sigma
  
  # __ Predict score_diff for every configuration home/away ____________________
  # make_newdata <- function(model, team_home, team_away) {
  #   tibble(
  #     team_home = factor(team_home, levels = levels(model$model$team_home)),
  #     team_away = factor(team_away, levels = levels(model$model$team_away))
  #   )
  # }
  # 
  # mu_m1     <- predict(model, newdata = make_newdata(model, team_A, team_B))
  # mu_m2_raw <- predict(model, newdata = make_newdata(model, team_B, team_A))
  # mu_m2     <- -mu_m2_raw
  # mu_m3     <- mu_m1
  
  # game 1 : team_A @ home, team_B @ away
  newdata_m1 <- prepare_data(
    tibble(
      date = as.Date(NA),
      playoff = TRUE, serie = NA_character_,
      team_home = team_A, pts_home = NA_real_,
      team_away = team_B, pts_away = NA_real_,
      score_diff = NA_real_,
      wins_A = 0L, wins_B = 0L
    ),
    team_ref = levels(model$model$team_home)[1]
  )
  mu_m1 <- predict(model, newdata = newdata_m1)   # score_diff = pts_A - pts_B

  # game 2 : team_B @ home, team_A @ away  → score_diff = pts_B - pts_A
  newdata_m2 <- prepare_data(
    tibble(
      date = as.Date(NA),
      playoff = TRUE, serie = NA_character_,
      team_home = team_B, pts_home = NA_real_,
      team_away = team_A, pts_away = NA_real_,
      score_diff = NA_real_,
      wins_A = 0L, wins_B = 0L
    ),
    team_ref = levels(model$model$team_home)[1]
  )
  mu_m2_raw <- predict(model, newdata = newdata_m2)  # pts_B - pts_A
  mu_m2 <- -mu_m2_raw                                # pts_A - pts_B

  # game 3 : team_A @ home, team_B @ away (as game 1)
  mu_m3 <- mu_m1
  
  # __ Monte Carlo _____________________________________________________________
  # For each game, draw randomly score_diff
  score_diff_m1 <- rnorm(n, mean = mu_m1, sd = sigma)
  score_diff_m2 <- rnorm(n, mean = mu_m2, sd = sigma)
  score_diff_m3 <- rnorm(n, mean = mu_m3, sd = sigma)
  
  # Predicted outcome of each game team_A point of view
  win_A_m1 <- score_diff_m1 > 0
  win_A_m2 <- score_diff_m2 > 0
  win_A_m3 <- score_diff_m3 > 0
  
  # Deduce series winner
  wins_A_after2 <- as.integer(win_A_m1) + as.integer(win_A_m2)
  
  qualified_A <- (wins_A_after2 == 2) | (wins_A_after2 == 1 & win_A_m3)
  qualified_B <- !qualified_A
  
  # Number of games played in the series
  n_games_played <- ifelse(wins_A_after2 %in% c(0, 2), 2L, 3L)
  
  # __ Per game summary ________________________________________________________
  played_m3 <- n_games_played == 3
  
  summary_games <- tibble(
    Serie = serie_name,
    Game = c(1L, 2L, 3L),
    Home = c(team_A, team_B, team_A),
    Away = c(team_B, team_A, team_B),
    Pred_score_diff = round(c(mu_m1, mu_m2, mu_m3), 2),
    P_home_win = round(c(
      mean(score_diff_m1 > 0),
      mean(-score_diff_m2 > 0),
      mean(score_diff_m3[played_m3] > 0)
    ), 3),
    P_played  = round(c(1, 1, mean(played_m3)), 3)
  )
  
  # __ Résumé de la série ______________________________________________________
  summary_serie <- tibble(
    Serie = serie_name,
    Team_A = team_A,
    Team_B = team_B,
    P_A_wins = round(mean(qualified_A), 3),
    P_B_wins = round(mean(qualified_B), 3),
    P_A_wins_after2 = round(mean(qualified_A & n_games_played == 2), 3),
    P_B_wins_after2 = round(mean(qualified_B & n_games_played == 2), 3),
    P_3games = round(mean(n_games_played == 3), 3)
  )
  
  return(list(
    summary_serie   = summary_serie,
    summary_games = summary_games,
    sim             = tibble(
      score_diff_m1, score_diff_m2, score_diff_m3,
      win_A_m1, win_A_m2, win_A_m3,
      n_games_played,
      qualified_A
    )
  ))
}

# __ Predictions — only 2026 model _____________________________________________
SF1_fit      <- simulate_bo3(N, Seed, fit, "SF1 (OLY-AEK)", "OLY", "AEK")
SF2_fit      <- simulate_bo3(N, Seed, fit, "SF2 (PAO-POK)", "PAO", "POK")

# __ Predictions — 25-26model __________________________________________________
SF1_fit25    <- simulate_bo3(N, Seed, fit_with25, "SF1 (OLY-AEK)", "OLY", "AEK")
SF2_fit25    <- simulate_bo3(N, Seed, fit_with25, "SF2 (PAO-POK)", "PAO", "POK")

# __ Affichage des résultats ___________________________________________________
cat("=== Only 26 season model ===\n\n")

cat("-- Series summary --\n")
print(as.data.frame(bind_rows(SF1_fit$summary_serie, SF2_fit$summary_serie)))

cat("\n-- Per game summary --\n")
print(as.data.frame(bind_rows(SF1_fit$summary_games, SF2_fit$summary_games)))

cat("\n=== 25 & 26 seasons model ===\n\n")

cat("-- Series summary --\n")
print(as.data.frame(bind_rows(SF1_fit25$summary_serie, SF2_fit25$summary_serie)))

cat("\n-- Per game summary --\n")
print(as.data.frame(bind_rows(SF1_fit25$summary_games, SF2_fit25$summary_games)))

# __ Check predictions coherence _______________________________________________
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
head(mean_points, 4)

mean_points_with25 <- left_join(
  train_with25 |>
    group_by(team = team_home) |>
    summarise(mean_diff_home = mean(score_diff)),
  train_with25 |>
    group_by(team = team_away) |>
    summarise(mean_diff_away = mean(-score_diff)),
  by = "team"
) |>
  mutate(mean_diff = (mean_diff_home + mean_diff_away) / 2) |>
  arrange(desc(mean_diff))
head(mean_points_with25, 4)

# __ Export résultats __________________________________________________________
# write_csv(
#   bind_rows(SF1_fit$summary_serie, SF2_fit$summary_serie),
#   "outputs/SF_predictions/SF_series_fit.csv"
# )
# write_csv(
#   bind_rows(SF1_fit25$summary_serie, SF2_fit25$summary_serie),
#   "outputs/SF_predictions/SF_series_fit25.csv"
# )
# write_csv(
#   bind_rows(SF1_fit$summary_games, SF2_fit$summary_games),
#   "outputs/SF_predictions/SF_games_fit.csv"
# )
# write_csv(
#   bind_rows(SF1_fit25$summary_games, SF2_fit25$summary_games),
#   "outputs/SF_predictions/SF_games_fit25.csv"
# )