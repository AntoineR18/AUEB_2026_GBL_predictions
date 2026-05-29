# 40_Final_pred_with_update.R

# __ Clean environment _________________________________________________________
to_keep <- c(
  "raw_games", "all_teams",
  "games", "N", "Seed",
  "prepare_data", "mean_points",
  "SF1_fit", "SF1_fit25", "SF2_fit", "SF2_fit25",
  "SF", "SF_with25"
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

simulate_bo5 <- function(n = N, seed = Seed, model, train) {
  
  set.seed(seed)

  dates = c(
      as.Date("2026-06-03"),
      as.Date("2026-06-05"),
      as.Date("2026-06-08"),
      as.Date("2026-06-10"),
      as.Date("2026-06-13")
  )
  
  all_wins <- vector("list", n)
  all_means <- matrix(NA_real_, nrow = n, ncol = 5)
  all_scores <- matrix(NA_real_, nrow = n, ncol = 5)
  
  for (i in 1:n) {
    
    train_rolling <- train
    model_rolling <- model
    
    wins <- c(0,0)
    
    for (g in 1:5) {
      
      # Test if a team has already won
      if (max(wins) == 3) next
      
      # Prepare current game
      newgame <- tibble(
        team_home = factor(
          ifelse(g %in% c(1,3,5), "OLY", "PAO"),
          levels = levels(model_rolling$model$team_home)
        ),
        team_away = factor(
          ifelse(g %in% c(1,3,5), "PAO", "OLY"),
          levels = levels(model_rolling$model$team_away)
        )
      )
      
      # Predict score_diff according to updated model
      sigma <- summary(model_rolling)$sigma
      mean_score_diff <- predict(model_rolling, newdata = newgame)
      score_diff <- rnorm(1, mean = mean_score_diff, sd = sigma)
      
      # Update wins
      win_A <- (g %in% c(1,3,5)) == (score_diff > 0)
      wins[ifelse(win_A, 1, 2)] <- wins[ifelse(win_A, 1, 2)] + 1
      
      # Update current game
      newgame <- newgame |>
        mutate(score_diff = score_diff)
      
      # Update model
      train_rolling <- bind_rows(train_rolling, newgame)
      model_rolling <- lm(
        score_diff ~ team_home + team_away,
        contrasts = list(
          team_home = contr.sum,
          team_away = contr.sum
        ),
        data = train_rolling
      )
      
      all_means[i, g] <- mean_score_diff
      all_scores[i, g] <- score_diff
    }
    
    all_wins[[i]] <- wins
  }
  
  # __ Per game summary ________________________________________________________
  summary_games <- tibble(
    Serie = "Final",
    Game = seq(1:5),
    Home = c("OLY", "PAO", "OLY", "PAO", "OLY"),
    Away = c("PAO", "OLY", "PAO", "OLY", "PAO"),
    Pred_score_diff = round(colMeans(all_means, na.rm = TRUE), 2),
    P_home_win = round(colMeans(all_scores > 0, na.rm = TRUE), 3),
    P_played = round(colMeans(!is.na(all_scores)), 3)
  )

  # __ Series summary __________________________________________________________
  summary_serie <- tibble(
    Serie = "Final",
    Team_A = "OLY",
    Team_B = "PAO",
    P_A_wins = round(
      mean(sapply(all_wins, function(g) {
        g[1] == 3
      })),
      3
    ),
    P_B_wins = round(
      mean(sapply(all_wins, function(g) {
        g[2] == 3
      })),
      3
    ),
    P_A_wins_after3 = round(
      mean(sapply(all_wins, function(g) {
        g[1] == 3 & sum(g) == 3
      })),
      3
    ),
    P_B_wins_after3 = round(
      mean(sapply(all_wins, function(g) {
        g[2] == 3 & sum(g) == 3
        })),
      3
    ),
    P_5games = round(
      mean(sapply(all_wins, function(g) {
        sum(g) == 5
        })),
      3
    ),
  )
  
  return(list(
    summary_serie = summary_serie,
    summary_games = summary_games,
    sim = list(
      scores = all_scores,
      wins = all_wins
    )
  ))
}

# __ Predictions _______________________________________________________________
Final <- simulate_bo5(model = fit, train = train)
Final_with25 <- simulate_bo5(model = fit_with25, train = train_with25)

# __ Arrange outputs ___________________________________________________________
Final_games <- as.data.frame(
  Final$summary_games |>
    mutate(P_away_win = 1 - P_home_win) |>
    select(Game, Home, P_home_win, P_away_win, Pred_score_diff, P_played)
)
Final_with25_games <- as.data.frame(
  Final_with25$summary_games |>
    mutate(P_away_win = 1 - P_home_win) |>
    select(Game, Home, P_home_win, P_away_win, Pred_score_diff, P_played)
)

# __ Show outputs  _____________________________________________________________
cat("=== Only 26 season model ===\n\n")

cat("-- Series summary --\n")
print(as.data.frame(Final$summary_serie))

cat("\n-- Per game summary --\n")
print(Final_games)

cat("\n=== 25 & 26 seasons model ===\n\n")

cat("-- Series summary --\n")
print(as.data.frame(Final_with25$summary_serie))

cat("\n-- Per game summary --\n")
print(Final_with25_games)

# __ Export results __________________________________________________________
write_csv(
  Final$summary_serie,
  "outputs/Final_predictions/Final.csv"
)
write_csv(
  Final_games,
  "outputs/Final_predictions/Final_games.csv"
)
write_csv(
  Final_with25$summary_serie,
  "outputs/Final_predictions/Final_with25.csv"
)
write_csv(
  Final_with25_games,
  "outputs/Final_predictions/Final_with25_games.csv"
)

# print(
#   xtable(Final$summary_serie),
#   include.rownames = FALSE,
#   file = "outputs/Final_predictions/Final.tex"
# )
# print(
#   xtable(Final_games, digits = c(0, 0, 0, 0, 2, 3, 3)),
#   include.rownames = FALSE,
#   file = "outputs/Final_predictions/Final_games.tex"
# )
# print(
#   xtable(Final_with25$summary_serie),
#   include.rownames = FALSE,
#   file = "outputs/Final_predictions/Final_with25.tex"
# )
# print(
#   xtable(Final_with25_games, digits = c(0, 0, 0, 0, 2, 3, 3)),
#   include.rownames = FALSE,
#   file = "outputs/Final_predictions/Final_with25_games.tex"
# )
