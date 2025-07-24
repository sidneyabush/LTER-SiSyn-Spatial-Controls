#!/usr/bin/env Rscript
# 02_final_rf_pipeline_robust.R
# ──────────────────────────────────────────────────────────────────────────────
# Train final RF models on older70, stability‑select features with robust parameters,
# then predict on recent30 & unseen10, and compute SHAP values on older70.
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load needed packages & clear environment
librarian::shelf(
  remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot,
  mlbench, pROC, tree, dplyr, plot.matrix, reshape2, rcartocolor,
  arsenal, googledrive, data.table, ggplot2, corrplot, pdp,
  iml, tidyr, viridis, parallel, doParallel, foreach, fastshap
)
rm(list = ls()); set.seed(666)

# 1) Set working & output directories
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 2) Utility functions
save_correlation_plot <- function(driver_cor, name) {
  png(sprintf("%s/%s_corrplot.png", output_dir, name), width = 2500, height = 2500, res = 300)
  corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
  title(name); dev.off()
}

save_rf_importance_plot <- function(model, name) {
  png(sprintf("%s/%s_varImp.png", output_dir, name), width = 1600, height = 1200, res = 300)
  randomForest::varImpPlot(model, main = name); dev.off()
}

save_lm_plot <- function(preds, obs, name) {
  png(sprintf("%s/%s_pred_vs_obs.png", output_dir, name), width = 1500, height = 1500, res = 300)
  plot(preds, obs, pch = 16, cex = 1.5,
       xlab = "Predicted", ylab = "Observed", main = name,
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  abline(0, 1, col = "#6699CC", lwd = 3, lty = 2); dev.off()
}

test_numtree_parallel <- function(ntree_list, formula, data) {
  cores <- detectCores() - 1; cl <- makeCluster(cores); registerDoParallel(cl)
  MSE <- foreach(ntree = ntree_list, .combine = 'c', .packages = 'randomForest') %dopar% {
    set.seed(666)
    rf_model <- randomForest(formula, data = data, importance = TRUE, proximity = TRUE, ntree = ntree)
    mean(rf_model$mse)
  }
  stopCluster(cl); MSE
}

rf_stability_selection_parallel <- function(x, y,
                                            n_bootstrap = 500,
                                            threshold    = 0.8,
                                            ntree        = 500,
                                            mtry         = NULL,
                                            importance_threshold = 0) {
  cores <- detectCores() - 1; cl <- makeCluster(cores); registerDoParallel(cl)
  sel_mse <- foreach(i = 1:n_bootstrap, .combine = rbind, .packages = 'randomForest') %dopar% {
    set.seed(123 + i)
    idx <- sample(nrow(x), replace = TRUE)
    rf_model <- randomForest(x[idx, ], y[idx], ntree = ntree, mtry = mtry, importance = TRUE)
    imps <- randomForest::importance(rf_model)[, '%IncMSE']
    c(as.numeric(imps > importance_threshold), mean(rf_model$mse))
  }
  stopCluster(cl)
  sel_mat <- sel_mse[, 1:ncol(x)]; freqs <- colMeans(sel_mat)
  features <- names(freqs[freqs >= threshold])
  if(length(features) < 5) features <- names(sort(freqs, decreasing = TRUE))[1:5]
  list(features = features, frequencies = freqs)
}

# 3) Read data & define predictors
tot_si <- read.csv(sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", 5), stringsAsFactors = FALSE)
var_order <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI","recession_slope",
  grep("^land_|^rocks_", names(tot_si), value = TRUE)
)
predictors <- intersect(var_order, names(tot_si))
rl_cols <- grep("^(land_|rocks_)", names(tot_si), value = TRUE)

# 4) Load splits
load_split <- function(path) {
  read.csv(path, stringsAsFactors = FALSE) %>%
    mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
    select(-contains("Gen"), -contains("major"), -Max_Daylength, -Q, -drainage_area) %>%
    mutate(greenup_day = as.numeric(greenup_day))
}
df_train   <- load_split("AllDrivers_cc_older70.csv")
df_recent30<- load_split("AllDrivers_cc_recent30.csv")
df_unseen10<- load_split("AllDrivers_cc_unseen10.csv")

# 5) Loop responses: tune RF1, stability, tune RF2, evaluate
ntree_vals <- seq(100, 2000, by = 100)
metrics_list <- list(); preds_list <- list(); features_list <- list()
for(resp in c("FNConc","FNYield")) {
  message("--- Processing ", resp)
  df_tr <- df_train %>% drop_na(all_of(c(resp, predictors)))
  x <- df_tr %>% select(all_of(predictors)); y <- df_tr[[resp]]
  # RF1 tuning
  MSE1 <- test_numtree_parallel(ntree_vals, as.formula(paste(resp, "~ .")), df_tr)
  best_nt1 <- ntree_vals[which.min(MSE1)]
  tune1 <- randomForest::tuneRF(x, y, ntreeTry = best_nt1, stepFactor = 1.5, improve = 0.01, plot = FALSE)
  best_mtry1 <- tune1[which.min(tune1[, "OOBError"]), "mtry"]
  rf1 <- randomForest(x, y, ntree = best_nt1, mtry = best_mtry1, importance = TRUE)
  imps <- randomForest::importance(rf1)[, '%IncMSE']
  thr_imp <- quantile(imps[imps > 0], 0.50)
  # Stability selection
  stab <- rf_stability_selection_parallel(x, y, n_bootstrap = 500,
                                          threshold = 0.8,
                                          ntree     = best_nt1,
                                          mtry      = best_mtry1,
                                          importance_threshold = thr_imp)
  feats <- stab$features; features_list[[resp]] <- feats
  # RF2 tuning
  df2 <- df_tr[, c(resp, feats)];
  MSE2 <- test_numtree_parallel_optimized(ntree_vals, as.formula(paste(resp, "~ .")), df2)
  best_nt2 <- ntree_vals[which.min(MSE2)]
  tune2 <- randomForest::tuneRF(df2[feats], df2[[resp]], ntreeTry = best_nt2, stepFactor = 1.5, improve = 0.01, plot = FALSE)
  best_mtry2 <- tune2[which.min(tune2[, "OOBError"]), "mtry"]
  rf2 <- randomForest(as.formula(paste(resp, "~ .")), data = df2,
                      ntree = best_nt2, mtry = best_mtry2, importance = TRUE)
  # Evaluate on recent30 & unseen10
  for(sub in c("recent30","unseen10")) {
    df_te <- get(paste0("df_", sub)) %>% drop_na(all_of(c(resp, feats)))
    if(nrow(df_te) < 5) next
    pred <- predict(rf2, df_te); obs <- df_te[[resp]]
    key <- paste(sub, resp, sep = "_")
    metrics_list[[key]] <- tibble(
      subset = sub, response = resp,
      R2 = cor(obs, pred)^2,
      RMSE = sqrt(mean((obs - pred)^2)),
      pRMSE = 100 * RMSE / mean(obs)
    )
    preds_list[[key]] <- tibble(subset = sub, response = resp, observed = obs, predicted = pred)
  }
  # Diagnostics
  save_correlation_plot(cor(x), paste0(resp, "_older70"))
  save_rf_importance_plot(rf2, paste0(resp, "_RF2"))
  save_lm_plot(predict(rf2, df_tr), df_tr[[resp]], paste0(resp, "_train"))
}
# 6) Save outputs
write.csv(tibble(response = names(features_list), kept_vars = map_chr(features_list, paste, collapse = "; ")),
          file.path(output_dir, "Retained_Variables_Per_Model.csv"), row.names = FALSE)
write.csv(bind_rows(metrics_list), file.path(output_dir, "Metrics_Final.csv"), row.names = FALSE)
write.csv(bind_rows(preds_list),   file.path(output_dir, "Predictions_Final.csv"), row.names = FALSE)

# 7) Compute SHAP on older70
for(resp in names(features_list)) {
  feats <- features_list[[resp]]
  load(file.path(output_dir, sprintf("%s_RF2.RData", resp)))  # rf2
  df_tr <- df_train %>% drop_na(all_of(c(resp, feats)))
  X <- df_tr[, feats, drop = FALSE]; y <- df_tr[[resp]]
  pred_wrapper <- function(object, newdata) predict(object, newdata = newdata)
  shap_vals <- fastshap::explain(rf2, X = X, pred_wrapper = pred_wrapper, nsim = 30, .parallel = TRUE)
  write.csv(shap_vals, file.path(output_dir, sprintf("%s_older70_shap.csv", resp)), row.names = FALSE)
}
# End pipeline
