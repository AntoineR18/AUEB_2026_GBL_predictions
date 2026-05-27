# 12_data_preprocess.R

# __ Clean environment _________________________________________________________
to_keep <- c("raw_games")
rm(list = setdiff(ls(), to_keep))

# __ Harmonize team names ______________________________________________________
create_team_codes <- function () {
  return(c(
    "AEK Athens" = "AEK",
    "Apollon Patras" = "APO",
    "Apollon Patras Carna" = "APO",
    "Aris" = "ARI",
    "Aris Midea" = "ARI",
    "ASK Karditsas" = "KAR",
    "ASK Karditsas Iaponiki" = "KAR",
    "Ionikos" = "ION",
    "Ionikos Hellenic Coin" = "ION",
    "Iraklis" = "IRA",
    "Iraklis 2022" = "IRA",
    "Kolossos H Hotels" = "KOL",
    "Larisa" = "LAR",
    "Larisa Bread factory" = "LAR",
    "Lavrio Megabolt" = "LAV",
    "Maroussi" = "MAR",
    "Messolonghi BAXI" = "MES",
    "Mykonos Betsson" = "MYK",
    "Olympiacos" = "OLY",
    "Panathinaikos" = "PAO",
    "Panathinaikos OPAP" = "PAO",
    "Panionios" = "PNN",
    "PAOK" = "POK",
    "PAOK mateco" = "POK",
    "Peristeri" = "PER",
    "Peristeri bwin" = "PER",
    "Promitheas Patras" = "PRO"
  ))
}
team_codes <- create_team_codes()
all_teams <- unique(unlist(team_codes))

# __ Preprocess data ___________________________________________________________
clean_games <- function(df, phase, season) {
  
  Sys.setlocale("LC_TIME", "C")
  
  df_clean <- df |>
    
    # Rename existing columns
    rename(
      date = Date,
      team_home = Opp,
      pts_home = `PTS...5`,
      team_away = Team,
      pts_away = `PTS...3`
    ) |>
    
    # Delete postponed & canceled games
    filter(!is.na(pts_home)) |>
    
    # Update existing variables and create new ones
    mutate(
      
      date = as.Date(date, "%a %b %d %Y"),
      team_home = recode(team_home, !!!team_codes),
      team_away = recode(team_away, !!!team_codes),
      
      score_diff = pts_home - pts_away,
      playoff = phase == "playoffs",
      serie = NA_character_,
      wins_A = 0,
      wins_B = 0,
    )
  
  # Rearrange columns
  df_clean <- df_clean[c(
    "date",
    "playoff", "serie",
    "team_home", "pts_home", "team_away", "pts_away",
    "score_diff",
    "wins_A", "wins_B"
  )]
  
  return(df_clean)
}
games <- imap(raw_games, function(phase_list, phase) {
  imap(phase_list, function(df, season) {
    clean_games(df, phase, season)
  })
})

compute_po_series <- function (df, reset_date = NULL) {
  
  df <- df |>
    mutate(
      serie = paste(
        pmin(team_home, team_away),
        pmax(team_home, team_away),
        sep = "-"
      )
    )
  
  if (!is.null(reset_date)) {
    df <- df |>
      mutate(serie_phase = ifelse(date >= as.Date(reset_date), 2, 1))
  } else {
    df <- df |>
      mutate(serie_phase = 1)
  }
  
  df <- df |>
    group_by(serie, serie_phase) |>
    mutate(
      wins_A = lag(
        cumsum(
          team_home == pmin(team_home, team_away) & score_diff > 0 |
            team_away == pmin(team_home, team_away) & score_diff < 0
        ),
        default = 0
      ),
      wins_B = lag(
        cumsum(
          team_home == pmax(team_home, team_away) & score_diff > 0 |
            team_away == pmax(team_home, team_away) & score_diff < 0
        ),
        default = 0
      )
    ) |>
    ungroup() |>
    select(-serie_phase)
  
  return(df)
}
games$playoffs <- imap(games$playoffs, function(df, season) {
  compute_po_series(df)
})

# __ Special treatment for 24 season ___________________________________________
handle_24 <- function (df) {
  
  df[2, "wins_B"] <- 0
  df[4, "wins_B"] <- 0
  df[7, "wins_B"] <- 1
  df[8, "wins_B"] <- 1
  df[9, "wins_B"] <- 1
  df[10, "wins_A"] <- 0
  df[11, "wins_B"] <- 0
  df[12, "wins_A"] <- 1
  df[13, "wins_B"] <- 1
  df[15, "wins_A"] <- 0
  df[14, "wins_B"] <- 0
  df[16, "wins_B"] <- 0
  df[17, "wins_B"] <- 0
  df[18, "wins_B"] <- 1
  df[19, "wins_B"] <- 2
  
  return(df)
}
games$playoffs$`24` <- bind_rows(
    games$playoffs$`24`[1:30, ],
    handle_24(games$playoffs$`24`[31:nrow(games$playoffs$`24`), ])
  )
