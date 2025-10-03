# #############################################################################
# Re-run RF predictions for UPDATED Cross-Validation (unseen10) data - FNConc
# #############################################################################
# Purpose:
#   The cross-validation dataset has been updated with new stratified sampling.
#   This script re-runs predictions ONLY for the unseen10 dataset using the
#   existing trained RF2 model, then updates the full predictions CSV.
#
# Required inputs:
#   1) <drv_dir>/AllDrivers_unseen10_not_split.csv  (UPDATED cross-val data)
#   2) <out_dir>/FNConc_RF2_model_and_settings_split.RData (existing RF2 model)
#   3) <out_dir>/Predictions_FNConc_split.csv (existing predictions)
#
# Outputs updated:
#   A) <out_dir>/Predictions_FNConc_split.csv (unseen10 rows replaced)
# #############################################################################

# 0) Load packages
librarian::shelf(randomForest, dplyr, tidyr)

# 1) Paths
drv_dir    <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files/inputs"
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"

# 2) Load existing RF2 model and feature list
load(file.path(output_dir, "FNConc_RF2_model_and_settings_split.RData"))
# This loads: rf2_FNConc, nt2_FNConc, mtry2_FNConc, feats_FNConc

# 3) Load UPDATED unseen10 data (using same preprocessing as Step 2.1)
# First read to get column names
df_temp <- read.csv(file.path(drv_dir, "AllDrivers_unseen10_not_split.csv"),
                    stringsAsFactors = FALSE)
rl_cols <- grep("^(land_|rocks_)", names(df_temp), value = TRUE)

# Apply same load_split function from original Step 2.1
df_unseen10 <- df_temp %>%
  mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  select(-contains("Gen"), -contains("major"), -Q, -drainage_area) %>%
  mutate(greenup_day = as.numeric(greenup_day))

# 4) Generate NEW predictions for unseen10
cat("Generating new predictions for updated unseen10 dataset...\n")

df_unseen10_clean <- df_unseen10 %>%
  drop_na(all_of(c("FNConc", feats_FNConc)))

new_unseen10_preds <- tibble(
  subset    = "unseen10",
  observed  = df_unseen10_clean$FNConc,
  predicted = predict(rf2_FNConc, df_unseen10_clean)
)

cat(sprintf("  - Generated %d predictions for unseen10\n", nrow(new_unseen10_preds)))

# 5) Load existing predictions, replace unseen10, and save
pred_df_old <- read.csv(file.path(output_dir, "Predictions_FNConc_split.csv"))

cat("Replacing unseen10 predictions in full predictions file...\n")
cat(sprintf("  - Old unseen10 rows: %d\n", sum(pred_df_old$subset == "unseen10")))
cat(sprintf("  - New unseen10 rows: %d\n", nrow(new_unseen10_preds)))

# Keep older70 and recent30, replace unseen10
pred_df_updated <- pred_df_old %>%
  filter(subset != "unseen10") %>%
  bind_rows(new_unseen10_preds)

# 6) Save updated predictions
write.csv(pred_df_updated,
          file      = file.path(output_dir, "Predictions_FNConc_split.csv"),
          row.names = FALSE)

cat("\n✓ Successfully updated Predictions_FNConc_split.csv\n")
cat(sprintf("  Final row counts: older70=%d, recent30=%d, unseen10=%d\n",
            sum(pred_df_updated$subset == "older70"),
            sum(pred_df_updated$subset == "recent30"),
            sum(pred_df_updated$subset == "unseen10")))

# 7) Calculate and display metrics for new unseen10
R2    <- cor(new_unseen10_preds$observed, new_unseen10_preds$predicted)^2
RMSE  <- sqrt(mean((new_unseen10_preds$observed - new_unseen10_preds$predicted)^2))
pRMSE <- 100 * RMSE / mean(new_unseen10_preds$observed)

cat("\nNew Cross-Validation (unseen10) Performance:\n")
cat(sprintf("  R² = %.3f\n", R2))
cat(sprintf("  RMSE = %.2f\n", RMSE))
cat(sprintf("  %%RMSE = %.1f%%\n", pRMSE))

#---- End of Script ----
