# Train RF for FNConc: older70 (training) → RF1 → stability → RF2 → predict on recent30 (testing) & unseen10 (cross-validation)

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
save_correlation_plot <- function(driver_cor, output_dir) {
  png(sprintf("%s/FNConc_Yearly_5yrs_corrplot.png", output_dir),
      width = 2500, height = 2500, res = 300)
  corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
  title("All Data Yearly FNConc")
  dev.off()
}

ave_rf_importance_plot <- function(rf_model, output_dir) {
  png(
    filename = sprintf("%s/RF_variable_importance_FNYield_Yearly_5_years.png", output_dir),
    width    = 10,        # inches
    height   = 10,           # inches
    units    = "in",        # specify inches
    res      = 300          # dpi
  )
  randomForest::varImpPlot(rf_model,
                           main = "rf_model2 - Yearly FNYield",
                           col  = "darkblue")
  dev.off()
}

save_lm_plot <- function(rf_model2, observed, output_dir) {
  preds <- rf_model2$predicted
  rmse  <- sqrt(mean((preds - observed)^2))
  rsq   <- mean(rf_model2$rsq)
  png(sprintf("%s/RF2_lm_plot_FNConc_Yearly_5_years.png", output_dir),
      width = 1500, height = 1500, res = 300)
  plot(preds, observed,
       pch = 16, cex = 1.5, xlab = "Predicted", ylab = "Observed",
       main = "RF Model 2 Full Data Ave FNConc",
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  abline(0, 1, col = "#6699CC", lwd = 3, lty = 2)
  legend("topleft", bty = "n", cex = 1.5,
         legend = paste("R² =", format(rsq, digits = 3)))
  legend("bottomright", bty = "n", cex = 1.5,
         legend = paste("RMSE =", format(rmse, digits = 3)))
  dev.off()
}

save_rf2_all_subsets_plot <- function(pred_df, resp, output_dir) {
  library(dplyr)
  library(ggplot2)
  
  # 1) Recode and factor the subset column
  pred_df <- pred_df %>%
    mutate(subset = factor(subset,
                           levels = c("older70","recent30","unseen10"),
                           labels = c("Train","Test","Cross‑Val")))
  
  # 2) Precompute global plot ranges for annotation offsets
  x_min   <- min(pred_df$predicted, na.rm = TRUE)
  x_range <- diff(range(pred_df$predicted, na.rm = TRUE))
  y_max   <- max(pred_df$observed,  na.rm = TRUE)
  y_range <- diff(range(pred_df$observed,  na.rm = TRUE))
  
  # 3) Compute metrics per subset and set x,y positions
  metrics_df <- pred_df %>%
    group_by(subset) %>%
    summarise(
      R2    = cor(observed, predicted, use = "complete.obs")^2,
      RMSE  = sqrt(mean((observed - predicted)^2, na.rm = TRUE)),
      pRMSE = 100 * RMSE / mean(observed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = sprintf("R² = %.3f\nRMSE = %.2f\n%%RMSE = %.1f%%",
                      R2, RMSE, pRMSE),
      x = x_min + 0.02 * x_range,                          # 2% in from left
      y = y_max - (as.numeric(subset) - 1) * 0.15 * y_range # stagger by 15% of height
    )
  
  # 4) Build the scatter + annotation plot
  p <- ggplot(pred_df, aes(x = predicted, y = observed, color = subset)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_text(data = metrics_df,
              aes(x = x, y = y, label = label, color = subset),
              inherit.aes = FALSE,
              hjust = 0, vjust = 1,
              size = 3.5,
              show.legend = FALSE) +
    scale_color_manual(values = c("Train"     = "#1b9e77",
                                  "Test"      = "#d95f02",
                                  "Cross‑Val" = "#7570b3")) +
    theme_bw() +
    labs(
      title = paste("RF Model 2 Predictions for", resp),
      x     = "Predicted",
      y     = "Observed",
      color = "Subset"
    )
  
  # 5) Save to file
  ggsave(
    filename = file.path(output_dir,
                         paste0("RF2_all_subsets_", resp, "_pred_vs_obs.png")),
    plot   = p,
    width  = 10,
    height = 6,
    dpi    = 300
  )
}

# 4) ntree scan (uses existing cluster)
test_numtree_parallel <- function(ntree_list, formula, data) {
  foreach(nt = ntree_list, .combine = 'c', .packages = 'randomForest') %dopar% {
    set.seed(666)
    rf_model <- randomForest(formula,
                             data       = data,
                             importance = TRUE,
                             proximity  = TRUE,
                             ntree      = nt)
    mean(rf_model$mse)
  }
}

# 5) Stability selection (OOB MSE tracking)
rf_stability_selection_parallel <- function(x, y, 
                                            n_bootstrap = 500,        # robust workflow
                                            # n_bootstrap = 10,         # test workflow
                                            threshold = 0.8,
                                            ntree, mtry,
                                            importance_threshold) {
  sel_mse <- foreach(i = 1:n_bootstrap,
                     .combine = rbind,
                     .packages = 'randomForest') %dopar% {
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
  sel_mat <- sel_mse[, 1:ncol(x)]
  mse_vec <- sel_mse[, ncol(sel_mse)]
  freqs   <- colMeans(sel_mat)
  names(freqs) <- colnames(x)
  stable_feats <- names(freqs[freqs >= threshold])
  list(features   = stable_feats,
       frequencies = freqs,
       mse_vec     = mse_vec)
}

# 6) Read & split data
rec_len <- 5
drv_all <- read.csv(
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years.csv", rec_len)
)
vars       <- c("NOx","P","npp","evapotrans","greenup_day",
                "precip","temp","snow_cover","permafrost",
                "elevation","basin_slope","RBI","recession_slope",
                grep("^land_|^rocks_", names(drv_all), value = TRUE))
predictors <- intersect(vars, names(drv_all))
rl_cols    <- grep("^(land_|rocks_)", names(drv_all), value = TRUE)

load_split <- function(path) {
  read.csv(path) %>%
    mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
    select(-contains("Gen"), -contains("major"),
           -Max_Daylength, -Q, -drainage_area) %>%
    mutate(greenup_day = as.numeric(greenup_day))
}

df_train    <- load_split("AllDrivers_cc_older70.csv")
df_recent30 <- load_split("AllDrivers_cc_recent30.csv")
df_unseen10 <- load_split("AllDrivers_cc_unseen10.csv")

# 7) Core pipeline for FNConc
resp <- "FNConc"
message("Processing ", resp)

# tree_grid <- seq(100, 800, by = 100)  # Testing grid
tree_grid <- seq(100, 2000, by = 100) # Robust grid

# a) RF1 tuning (comment out once done)
df_tr_full <- df_train %>%
  drop_na(all_of(c(resp, predictors)))
x1 <- df_tr_full[predictors]
y1 <- df_tr_full[[resp]]

# scan ntree for RF1
mse1 <- test_numtree_parallel(tree_grid,
                              as.formula(paste(resp, "~ .")),
                              df_tr_full)
nt1 <- tree_grid[which.min(mse1)]

# tune mtry
t1   <- randomForest::tuneRF(x1, y1,
                             ntreeTry  = nt1,
                             stepFactor = 1.5,
                             improve    = 0.01,
                             plot       = FALSE)
mtry1 <- t1[which.min(t1[, 2]), 1]

# train RF1
rf1   <- randomForest(x = x1, y = y1,
                      ntree      = nt1,
                      mtry       = mtry1,
                      importance = TRUE)

# cache RF1 and importances
imps <- importance(rf1)[, "%IncMSE"]
save(rf1, imps,
     file = file.path(output_dir, sprintf("%s_RF1.RData", resp)))

# b) Load cached RF1 (if restarting here)
load(file.path(output_dir, sprintf("%s_RF1.RData", resp)))  # loads rf1, imps

# c) Stability selection
imp_thr  <- quantile(imps, 0.50)
freq_thr <- 0.80
message("Starting stability selection…")
start <- Sys.time()

stab <- rf_stability_selection_parallel(
  x                     = df_train[predictors],
  y                     = df_train[[resp]],
  n_bootstrap           = 500,      # Robust
  #n_bootstrap           = 10,      # Testing
  threshold             = freq_thr,
  ntree                 = rf1$ntree,
  mtry                  = rf1$mtry,
  importance_threshold  = imp_thr
)

end <- Sys.time()
message("Stability selection took ", round(end - start, 2),
        " ", units(end - start))

feats <- stab$features
# if (length(feats) < 5) {
#   message("Low feature count; falling back to top 5 by frequency.")
#   feats <- names(sort(stab$frequencies, decreasing = TRUE))[1:5]
# }

# d) RF2 on selected features
message("Starting RF2 tuning…")
start <- Sys.time()

formula2 <- as.formula(paste(resp, "~ ."))
df2      <- df_train %>%
  drop_na(all_of(c(resp, feats))) %>%
  select(all_of(c(resp, feats)))

mse2 <- test_numtree_parallel(tree_grid, formula2, df2)
nt2  <- tree_grid[which.min(mse2)]

t2 <- randomForest::tuneRF(
  x        = df2[feats], 
  y        = df2[[resp]], 
  ntreeTry = nt2, 
  stepFactor = 1.5, 
  improve    = 0.01,
  plot       = FALSE
)

mtry2 <- t2[which.min(t2[, 2]), 1]

rf2 <- randomForest(x         = df2[feats],
                    y         = df2[[resp]],
                    ntree     = nt2,
                    mtry      = mtry2,
                    importance = TRUE)

end <- Sys.time()
message("RF2 tuning took ", round(end - start, 2),
        " ", units(end - start))

# e) Save RF2 & drivers for SHAP
save(rf2,
     file = file.path(output_dir,
                      sprintf("%s_Yearly_rf_model2.RData", resp)))
kept_drivers <- df_recent30 %>%
  drop_na(all_of(c(resp, feats))) %>%
  select(all_of(feats))
save(kept_drivers,
     file = file.path(output_dir,
                      sprintf("%s_Yearly_kept_drivers.RData", resp)))

# f) Diagnostics for RF2 (OOB on training)
save_rf_importance_plot(rf2, output_dir)
save_lm_plot(rf2, df2[[resp]], output_dir)

# g) Evaluate on recent30 & unseen10 + save metrics
evaluate_sub <- function(df_sub, name) {
  df_sub <- df_sub %>% drop_na(all_of(c(resp, feats)))
  preds  <- predict(rf2, df_sub)
  obs    <- df_sub[[resp]]
  tibble(subset = name,
         R2    = cor(obs, preds)^2,
         RMSE  = sqrt(mean((obs - preds)^2)),
         pRMSE = 100 * RMSE / mean(obs))
}

metrics <- bind_rows(
  evaluate_sub(df_recent30, "recent30"),
  evaluate_sub(df_unseen10, "unseen10")
)
write.csv(metrics,
          file.path(output_dir, sprintf("Metrics_%s.csv", resp)),
          row.names = FALSE)

# h) Save retained-feature list
tibble(response = resp,
       kept_vars = paste(feats, collapse = "; ")) %>%
  write.csv(file.path(output_dir,
                      paste0("Retained_Variables_", resp, ".csv")),
            row.names = FALSE)

# i) Gather predictions for train, test, and CV + save CSV
pred_list <- list(
  older70 = tibble(subset    = "older70",
                   observed  = df2[[resp]],
                   predicted = predict(rf2, df2)),
  recent30 = tibble(subset    = "recent30",
                    observed  = df_recent30[[resp]],
                    predicted = predict(rf2,
                                        df_recent30 %>%
                                          drop_na(all_of(c(resp, feats))))),
  unseen10 = tibble(subset    = "unseen10",
                    observed  = df_unseen10[[resp]],
                    predicted = predict(rf2,
                                        df_unseen10 %>%
                                          drop_na(all_of(c(resp, feats)))))
)
pred_df <- bind_rows(pred_list)
write.csv(pred_df,
          file.path(output_dir,
                    paste0("Predictions_", resp, ".csv")),
          row.names = FALSE)

# j) Quick diagnostics: corr and varImp
save_correlation_plot(cor(df_train[predictors]), output_dir)
save_rf_importance_plot(rf2, output_dir)

# k) Single Pred vs Obs plot with colored subsets
save_rf2_all_subsets_plot(pred_df, resp, output_dir)

# 8) Stop parallel backend
parallel::stopCluster(cl)
