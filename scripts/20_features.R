# 020_features.R

# __ Clean environment _________________________________________________________
to_keep <- c("raw_games", "games", "all_teams")
rm(list = setdiff(ls(), to_keep))

# __ Set up random framework ___________________________________________________
N <- 100
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

plot_SF_heatmaps <- function(SF1_without, SF2_without, SF1_with) {
  
  get_max_prob <- function(mc) {
    tibble(diff = round(mc)) |>
      filter(diff >= -15, diff <= 15) |>
      count(diff) |>
      mutate(prob = n / sum(n)) |>
      pull(prob) |>
      max()
  }
  
  max_prob <- max(
    get_max_prob(SF1_without$MC),
    get_max_prob(SF2_without$MC),
    get_max_prob(SF1_with$MC)
  )
  
  plot_MC_heatmap <- function(mc, team_A, team_B, title, pred_diff) {
    tibble(diff = round(mc)) |>
      filter(diff >= -15, diff <= 15) |>
      count(diff) |>
      mutate(prob = n / sum(n)) |>
      ggplot(aes(x = diff, y = 1, fill = prob)) +
      geom_tile() +
      scale_fill_gradient(
        low = "white",
        high = "darkblue",
        limits = c(0, max_prob)
      ) +
      scale_x_continuous(breaks = seq(-15, 15, by = 5)) +
      labs(
        title = title,
        x = paste0("Score difference (", team_A, " - ", team_B, ")"),
        y = NULL,
        fill = "Probability"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_blank()) +
      geom_text(
        aes(
          label = ifelse(
            prob == max(prob),
            paste0(diff, "\n(", round(prob*100, 1), "%)"),
            ""
          )
        ),
        color = "white", size = 3
      ) +
      geom_vline(
        aes(xintercept = round(pred_diff), color = "Predicted diff"),
        linetype = "dashed"
      ) +
      scale_color_manual(values = c("Predicted diff" = "red"), name = NULL)
  }
  
  p1 <- plot_MC_heatmap(
    mc = SF1_without$MC,
    team_A = SF1_without$summary$Team_A,
    team_B = SF1_without$summary$Team_B,
    title = "SF1 — Without home effect",
    pred_diff = SF1_without$summary$Score_diff_pred
  )
  p2 <- plot_MC_heatmap(
    mc = SF2_without$MC,
    team_A = SF2_without$summary$Team_A,
    team_B = SF2_without$summary$Team_B,
    title = "SF2 — Without home effect",
    pred_diff = SF2_without$summary$Score_diff_pred
  )
  p3 <- plot_MC_heatmap(
    mc = SF1_with$MC,
    team_A = SF1_with$summary$Team_A,
    team_B = SF1_with$summary$Team_B,
    title = "SF1 — With home effect for OLY",
    pred_diff = SF1_with$summary$Score_diff_pred
  )
  
  (p1 + p3) / p2 +
    plot_layout(guides = "collect")
}