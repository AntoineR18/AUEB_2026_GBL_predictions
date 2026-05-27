# 32_SF_predictions_with_update.R

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

simulate_bo3 <- function(n = N, seed = Seed, model, train) {
  
  set.seed(seed)
  
  series <- c("SF1", "SF2")
  team_A <- c("SF1" = "OLY", "SF2" = "PAO")
  
  SF_games <- tibble(
    date = rep(
      c(as.Date("2026-05-28"), as.Date("2026-05-30"), as.Date("2026-06-01")),
      each = 2
    ),
    serie = rep(series, 3),
    team_home = c("OLY", "PAO", "AEK", "POK", "OLY", "PAO"),
    team_away = c("AEK", "POK", "OLY", "PAO", "AEK", "POK")
  )
  
  all_wins <- vector("list", n)
  all_means <- matrix(NA_real_, nrow = n, ncol = 6)
  all_scores <- matrix(NA_real_, nrow = n, ncol = 6)
  
  for (i in 1:n) {
    
    train_rolling <- train
    model_rolling <- model
    
    wins <- list("SF1" = c(0, 0), "SF2" = c(0, 0))
    
    for (g in 1:6) {
      
      game <- SF_games[g, ]
      
      # Test if a team has already won
      serie <- game$serie
      if (max(wins[[serie]]) == 2) next
      
      # Prepare current game
      newgame <- prepare_data(
        tibble(
          date = game$date,
          playoff = TRUE,
          serie = serie,
          team_home = game$team_home, pts_home = NA_integer_,
          team_away = game$team_away, pts_away = NA_integer_,
          score_diff = NA_integer_,
          wins_A = 0L, wins_B = 0L
        ),
        team_ref = "OLY"
      )
      
      # Predict score_diff according to updated model
      sigma <- summary(model_rolling)$sigma
      mean_score_diff <- predict(model_rolling, newdata = newgame)
      score_diff <- rnorm(1, mean = mean_score_diff, sd = sigma)
      
      # Update wins
      win_A <- (game$team_home == team_A[[serie]]) == (score_diff > 0)
      wins[[serie]][ifelse(win_A, 1, 2)] <- wins[[serie]][ifelse(win_A, 1, 2)] + 1
      
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
  summary_games <- SF_games |>
    select(!date) |>
    rename(
      Serie = serie,
      Home = team_home,
      Away = team_away
    ) |>
    mutate(
      Game = rep(1:3, each = 2),
      Pred_score_diff = round(colMeans(all_means, na.rm = TRUE), 2),
      P_home_win = round(colMeans(all_scores > 0, na.rm = TRUE), 3),
      P_played = round(colMeans(!is.na(all_scores)), 3)
    ) |> 
    select(Serie, Game, Home, Away, Pred_score_diff, P_home_win, P_played)
  
  # __ Series summary __________________________________________________________
  summary_serie <- tibble(
    Serie = series,
    Team_A = c("OLY", "PAO"),
    Team_B = c("AEK", "POK"),
    P_A_wins = round(
      sapply(series, function(s) {
        mean(sapply(all_wins, function(g) {
          g[[s]][1] == 2
        }))
      }),
      3
    ),
    P_B_wins = round(
      sapply(series, function(s) {
        mean(sapply(all_wins, function(g) {
          g[[s]][2] == 2
        }))
      }),
      3
    ),
    P_A_wins_after2 = round(
      sapply(series, function(s) {
        mean(sapply(all_wins, function(g) {
          g[[s]][1] == 2 & sum(g[[s]]) == 2
        }))
      }),
      3
    ),
    P_B_wins_after2 = round(
      sapply(series, function(s) {
        mean(sapply(all_wins, function(g) {
          g[[s]][2] == 2 & sum(g[[s]]) == 2
        }))
      }),
      3
    ),
    P_3games = round(
      sapply(series, function(s) {
        mean(sapply(all_wins, function(g) {
          sum(g[[s]]) == 3
        }))
      }),
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

SF <- simulate_bo3(model = fit, train = train)
SF_with25 <- simulate_bo3(model = fit_with25, train = train_with25)

# __ Show outputs  _____________________________________________________________
cat("=== Only 26 season model ===\n\n")

cat("-- Series summary --\n")
print(as.data.frame(SF$summary_serie))

cat("\n-- Per game summary --\n")
print(as.data.frame(SF$summary_games))

cat("\n=== 25 & 26 seasons model ===\n\n")

cat("-- Series summary --\n")
print(as.data.frame(SF_with25$summary_serie))

cat("\n-- Per game summary --\n")
print(as.data.frame(SF_with25$summary_games))