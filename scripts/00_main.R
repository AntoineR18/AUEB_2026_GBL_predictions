# 00_main.R

library(tidyverse)
library(patchwork)
library(xtable)

# __ Preprocess data ___________________________________________________________
source("scripts/11_data_load.R")

source("scripts/12_data_preprocess.R")
source("scripts/20_features.R")

# __ Predictions _______________________________________________________________
source("scripts/31_SF_predictions.R")
source("scripts/32_SF_predictions_with_update.R")
# source("scripts/50_final_predictions.R")
# 
# # __ Validation ________________________________________________________________
# source("scripts/41_validation.R")
# source("scripts/42_validation_plots.R")

# __ Clean environment _________________________________________________________
to_keep <- c(
  "raw_games"
)
rm(list = setdiff(ls(), to_keep))
