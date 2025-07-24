#!/usr/bin/env Rscript
# 03_shap_recent30.R
# ──────────────────────────────────────────────────────────────────────────────
# Load saved RF2 models & retained features, then compute SHAP on recent30.
# Usage:
#   Rscript 03_shap_recent30.R
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load packages & clear
librarian::shelf(dplyr, tidyr, randomForest, fastshap, tibble)
rm(list=ls()); set.seed(666)

# 1) Paths
models_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"
data_dir   <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files"

# 2) Load retained‐features table
feat_df <- read.csv(
  file.path(models_dir, "Retained_Variables_Per_Model.csv"),
  stringsAsFactors = FALSE
)
# Expect columns: response, kept_vars (semicolon‑separated)

# 3) Load recent30 data
setwd(data_dir)
rl_cols <- grep("^(land_|rocks_)", names(read.csv("AllDrivers_Harmonized_Yearly_filtered_5_years_uncleaned.csv")), value=TRUE)
df_recent30 <- read.csv("AllDrivers_cc_recent30.csv") %>%
  mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  select(-contains("Gen"), -contains("major"), -Max_Daylength, -Q, -drainage_area) %>%
  mutate(greenup_day = as.numeric(greenup_day))

# 4) Compute SHAP for each response (no progress bar)
for(resp in feat_df$response) {
  # parse feature names
  kept <- strsplit(feat_df$kept_vars[feat_df$response == resp], ";\\s*")[[1]]
  
  # load the RF2 model
  load(file.path(models_dir, sprintf("%s_RF2.RData", resp)))  # loads 'rf2'
  
  # subset recent30 to rows with no NA in resp or kept
  df_te <- df_recent30 %>% drop_na(all_of(c(resp, kept)))
  X     <- df_te[, kept, drop=FALSE]
  
  # SHAP compute (nsim = 30; parallel within fastshap)
  wrap <- function(object, newdata) predict(object, newdata = newdata)
  shap_vals <- fastshap::explain(
    object       = rf2,
    X            = X,
    pred_wrapper = wrap,
    nsim         = 30,
    .parallel    = TRUE
  )
  
  # save to CSV
  out_file <- file.path(models_dir, sprintf("%s_recent30_shap.csv", resp))
  write.csv(shap_vals, out_file, row.names = FALSE)
  message("Saved SHAP for ", resp, " -> ", out_file)
}

message("All SHAP files written to ", models_dir)
