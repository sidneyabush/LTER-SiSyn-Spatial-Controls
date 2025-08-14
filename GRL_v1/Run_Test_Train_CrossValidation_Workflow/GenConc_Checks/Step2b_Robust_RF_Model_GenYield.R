# =============================================================================
# Train RF for GenYield: older70 (training) → RF1 → stability → RF2 → predict on recent30 & unseen10
# =============================================================================

# 0) Load packages & clear
librarian::shelf(
  remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot, mlbench,
  pROC, tree, dplyr, plot.matrix, reshape2, rcartocolor, arsenal,
  googledrive, data.table, ggplot2, corrplot, pdp,
  iml, tidyr, viridis, parallel, doParallel, foreach
)
rm(list = ls())
set.seed(666)

# 1) Setup parallel backend once
n_cores <- parallel::detectCores() - 1
cl      <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# 2) Paths
drv_dir    <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files"
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"
setwd(drv_dir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 3) Utility functions
save_correlation_plot <- function(driver_cor, output_dir, name) {
  png(sprintf("%s/%s_Yearly_5yrs_corrplot.png", output_dir, name),
      width = 2500, height = 2500, res = 300)
  corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
  title(sprintf("All Data Yearly %s", name))
  dev.off()
}

save_rf_importance_plot <- function(rf_model, output_dir, name) {
  png(sprintf("%s/RF_variable_importance_%s_Yearly_5_years.png", output_dir, name),
      width = 10, height = 10, units = "in", res = 300)
  randomForest::varImpPlot(rf_model,
                           main = sprintf("rf_model2 - Yearly %s", name),
                           col  = "darkblue")
  dev.off()
}

save_lm_plot <- function(rf_model2, observed, output_dir, name) {
  preds <- rf_model2$predicted
  rmse  <- sqrt(mean((preds - observed)^2))
  rsq   <- mean(rf_model2$rsq)
  png(sprintf("%s/RF2_lm_plot_%s_Yearly_5_years.png", output_dir, name),
      width = 1500, height = 1500, res = 300)
  plot(preds, observed,
       pch = 16, cex = 1.5,
       xlab = "Predicted", ylab = "Observed",
       main = sprintf("RF Model 2 Full Data Ave %s", name),
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  abline(0, 1, col = "#6699CC", lwd = 3, lty = 2)
  legend("topleft",  bty = "n", cex = 1.5, legend = sprintf("R² = %.3f", rsq))
  legend("bottomright", bty = "n", cex = 1.5, legend = sprintf("RMSE = %.2f", rmse))
  dev.off()
}

save_rf2_all_subsets_plot <- function(pred_df, name, output_dir) {
  library(dplyr); library(ggplot2)
  pred_df <- pred_df %>%
    mutate(subset = factor(subset,
                           levels = c("older70","recent30","unseen10"),
                           labels = c("Train","Test","Cross‑Val")))
  x_min   <- min(pred_df$predicted, na.rm = TRUE)
  x_range <- diff(range(pred_df$predicted, na.rm = TRUE))
  y_max   <- max(pred_df$observed,  na.rm = TRUE)
  y_range <- diff(range(pred_df$observed,  na.rm = TRUE))
  metrics_df <- pred_df %>%
    group_by(subset) %>%
    summarise(
      R2    = cor(observed, predicted, use = "complete.obs")^2,
      RMSE  = sqrt(mean((observed - predicted)^2, na.rm = TRUE)),
      pRMSE = 100 * RMSE / mean(observed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = sprintf("R² = %.3f\nRMSE = %.2f\n%%RMSE = %.1f%%", R2, RMSE, pRMSE),
      x = x_min + 0.02 * x_range,
      y = y_max - (as.numeric(subset) - 1) * 0.15 * y_range
    )
  p <- ggplot(pred_df, aes(x = predicted, y = observed, color = subset)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_text(data = metrics_df,
              aes(x = x, y = y, label = label, color = subset),
              inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3.5, show.legend = FALSE) +
    scale_color_manual(values = c("Train"="#1b9e77","Test"="#d95f02","Cross‑Val"="#7570b3")) +
    theme_bw() +
    labs(title = paste("RF Model 2 Predictions for", name),
         x = "Predicted", y = "Observed", color = "Subset")
  ggsave(sprintf("%s/RF2_all_subsets_%s_pred_vs_obs.png", output_dir, name),
         plot = p, width = 10, height = 6, dpi = 300)
}

# 4) RandomForest helpers
test_numtree_parallel <- function(ntree_list, formula, data) {
  foreach(nt = ntree_list, .combine = 'c', .packages = 'randomForest') %dopar% {
    set.seed(666)
    rf_model <- randomForest(formula, data = data,
                             importance = TRUE, proximity = TRUE, ntree = nt)
    mean(rf_model$mse)
  }
}

rf_stability_selection_parallel <- function(x, y, n_bootstrap = 500, threshold = 0.8,
                                            ntree, mtry, importance_threshold) {
  sel_mse <- foreach(i = 1:n_bootstrap, .combine = rbind, .packages = 'randomForest') %dopar% {
    set.seed(123 + i)
    idx <- sample(nrow(x), replace = TRUE)
    rf_model <- randomForest(x[idx, ], y[idx],
                             ntree      = ntree,
                             mtry       = mtry,
                             importance = TRUE)
    imp_scores <- importance(rf_model)[, "%IncMSE"]
    selected   <- as.integer(imp_scores > importance_threshold)
    c(selected, mean(rf_model$mse))
  }
  sel_mat <- sel_mse[,1:ncol(x)]
  mse_vec <- sel_mse[,ncol(sel_mse)]
  freqs   <- colMeans(sel_mat); names(freqs) <- colnames(x)
  stable_feats <- names(freqs[freqs >= threshold])
  list(features = stable_feats, frequencies = freqs, mse_vec = mse_vec)
}

# 5) Read & split data
rec_len <- 5
drv_all <- read.csv(sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years.csv", rec_len))
vars       <- c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
                "snow_cover","permafrost","elevation","basin_slope","RBI",
                "recession_slope", grep("^land_|^rocks_", names(drv_all), value = TRUE))
predictors <- intersect(vars, names(drv_all))
rl_cols    <- grep("^(land_|rocks_)", names(drv_all), value = TRUE)
load_split <- function(path) {
  read.csv(path, stringsAsFactors = FALSE) %>%
    mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
    select(-contains("FN"), -contains("major"), -Q, -drainage_area) %>%
    mutate(greenup_day = as.numeric(greenup_day))
}
df_train    <- load_split("AllDrivers_cc_older70.csv")
df_recent30 <- load_split("AllDrivers_cc_recent30.csv")
df_unseen10 <- load_split("AllDrivers_cc_unseen10.csv")

# ──────────────────────────────────────────────────────────────────────────────
# Define tree grid for RF1 & RF2
tree_grid <- seq(100, 2000, by = 100)
# ──────────────────────────────────────────────────────────────────────────────

# =============================================================================
# a) RF1 tuning for GenYield
# =============================================================================
df_tr_full <- df_train %>%
  drop_na(all_of(c("GenYield", predictors)))
x1 <- df_tr_full[predictors]
y1 <- df_tr_full$GenYield

# scan ntree
mse1 <- test_numtree_parallel(tree_grid, as.formula("GenYield ~ ."), df_tr_full)
nt1  <- tree_grid[which.min(mse1)]

# tune mtry
t1   <- randomForest::tuneRF(x1, y1,
                             ntreeTry   = nt1,
                             stepFactor = 1.5,
                             improve    = 0.01,
                             plot       = FALSE)
mtry1 <- t1[which.min(t1[, 2]), 1]

# train RF1
rf1 <- randomForest(x = x1, y = y1,
                    ntree      = nt1,
                    mtry       = mtry1,
                    importance = TRUE)

# cache RF1 & importances, rename for stability
rf1_GenYield  <- rf1
imps_GenYield <- randomForest::importance(rf1)[, "%IncMSE"]
save(rf1_GenYield, imps_GenYield,
     file = file.path(output_dir, "GenYield_RF1.RData"))

# =============================================================================
# b) Stability selection
# =============================================================================
imp_thr_GenYield  <- quantile(imps_GenYield, 0.50)
freq_thr_GenYield <- 0.80

stab_GenYield <- rf_stability_selection_parallel(
  x                     = df_train[predictors],
  y                     = df_train$GenYield,
  n_bootstrap           = 500,
  threshold             = freq_thr_GenYield,
  ntree                 = rf1_GenYield$ntree,
  mtry                  = rf1_GenYield$mtry,
  importance_threshold  = imp_thr_GenYield
)
feats_GenYield <- stab_GenYield$features

# save stability outputs
save(stab_GenYield, feats_GenYield, imp_thr_GenYield, freq_thr_GenYield,
     file = file.path(output_dir, "GenYield_stability_selection.RData"))
write.csv(
  tibble(variable = names(stab_GenYield$frequencies),
         frequency = stab_GenYield$frequencies),
  file      = file.path(output_dir, "GenYield_stability_frequencies.csv"),
  row.names = FALSE
)

# =============================================================================
# c) RF2 tuning on selected features
# =============================================================================
df2_GenYield <- df_train %>%
  drop_na(all_of(c("GenYield", feats_GenYield))) %>%
  select(GenYield, all_of(feats_GenYield))

mse2_GenYield <- test_numtree_parallel(tree_grid, as.formula("GenYield ~ ."), df2_GenYield)
nt2_GenYield  <- tree_grid[which.min(mse2_GenYield)]

t2_GenYield   <- randomForest::tuneRF(
  x        = df2_GenYield[feats_GenYield],
  y        = df2_GenYield$GenYield,
  ntreeTry = nt2_GenYield,
  stepFactor = 1.5,
  improve    = 0.01,
  plot       = FALSE
)
mtry2_GenYield <- t2_GenYield[which.min(t2_GenYield[,2]),1]

rf2_GenYield   <- randomForest(
  x          = df2_GenYield[feats_GenYield],
  y          = df2_GenYield$GenYield,
  ntree      = nt2_GenYield,
  mtry       = mtry2_GenYield,
  importance = TRUE
)

save(rf2_GenYield, nt2_GenYield, mtry2_GenYield, feats_GenYield,
     file = file.path(output_dir, "GenYield_RF2_model_and_settings.RData"))

save(rf2_GenYield, 
     file = file.path(output_dir, "GenYield_Yearly_rf_model2.RData"))

# =============================================================================
# d) Export kept_drivers for SHAP
# =============================================================================
kept_drivers_GenYield <- df_recent30 %>%
  drop_na(all_of(c("GenYield", feats_GenYield))) %>%
  select(all_of(feats_GenYield))

save(kept_drivers_GenYield,
     file = file.path(output_dir, "GenYield_Yearly_kept_drivers.RData"))

# =============================================================================
# e) Diagnostics & plots
# =============================================================================
save_correlation_plot(cor(df_train[predictors]), output_dir, "GenYield")
save_rf_importance_plot(rf2_GenYield, output_dir, "GenYield")
save_lm_plot(rf2_GenYield, df2_GenYield$GenYield, output_dir, "GenYield")

pred_list_GenYield <- list(
  older70 = tibble(subset   = "older70",
                   observed = df2_GenYield$GenYield,
                   predicted= predict(rf2_GenYield, df2_GenYield)),
  recent30 = tibble(subset   = "recent30",
                    observed = df_recent30$GenYield,
                    predicted= predict(rf2_GenYield,
                                       df_recent30 %>%
                                         drop_na(all_of(c("GenYield", feats_GenYield))))),
  unseen10 = tibble(subset   = "unseen10",
                    observed = df_unseen10$GenYield,
                    predicted= predict(rf2_GenYield,
                                       df_unseen10 %>%
                                         drop_na(all_of(c("GenYield", feats_GenYield)))))
)
pred_df_GenYield <- bind_rows(pred_list_GenYield)

write.csv(pred_df_GenYield,
          file      = file.path(output_dir, "Predictions_GenYield.csv"),
          row.names = FALSE)

save_rf2_all_subsets_plot(pred_df_GenYield, "GenYield", output_dir)

# =============================================================================
# f) Save stability + importance summary (with header comment)
# =============================================================================
stab_df <- tibble(
  variable  = names(stab_GenYield$frequencies),
  frequency = stab_GenYield$frequencies,
  incMSE    = imps_GenYield[names(stab_GenYield$frequencies)],
  selected  = names(stab_GenYield$frequencies) %in% feats_GenYield
)

stab_out <- file.path(output_dir, "GenYield_08_Feature_Stability_and_medianImportance.csv")
writeLines(
  sprintf("# Importance threshold (median %%IncMSE from RF1) = %.5f", imp_thr_GenYield),
  con = stab_out
)
write.table(
  stab_df,
  file      = stab_out,
  sep       = ",",
  row.names = FALSE,
  append    = TRUE
)

# =============================================================================
# g) Stop parallel backend
# =============================================================================
parallel::stopCluster(cl)
