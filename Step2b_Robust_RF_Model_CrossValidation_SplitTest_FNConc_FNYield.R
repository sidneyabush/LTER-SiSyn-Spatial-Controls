#!/usr/bin/env Rscript
# 02_final_rf_pipeline.R
# ──────────────────────────────────────────────────────────────────────────────
# Train RF models on older70, stability‐select features, then predict on recent30 & unseen10
# for both FNConc and FNYield, using the robust workflow.
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load packages, clear environment, set seed
librarian::shelf(
  remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot,
  mlbench, pROC, tree, dplyr, plot.matrix, reshape2, rcartocolor,
  arsenal, googledrive, data.table, ggplot2, corrplot, pdp,
  iml, tidyr, viridis, parallel, doParallel, foreach
)
rm(list = ls())
set.seed(666)

# 1) Output directory
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 2) Utility functions
save_correlation_plot <- function(driver_cor, name) {
  png(sprintf("%s/%s_corrplot.png", output_dir, name), width=2500, height=2500, res=300)
  corrplot(driver_cor, type="lower", pch.col="black", tl.col="black", diag=FALSE)
  title(sprintf("All Data Yearly %s", name))
  dev.off()
}

save_rf_importance_plot <- function(model, name) {
  png(sprintf("%s/%s_varImp.png", output_dir, name), width=1600, height=1200, res=300)
  randomForest::varImpPlot(model, main=sprintf("RF2 - %s" ,name))
  dev.off()
}

save_lm_plot <- function(preds, obs, name) {
  png(sprintf("%s/%s_pred_vs_obs.png", output_dir, name), width=1500, height=1500, res=300)
  plot(preds, obs,
       pch=16, cex=1.5,
       xlab="Predicted", ylab="Observed",
       main=sprintf("RF2 %s Predicted vs Observed", name),
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
  abline(0,1, col="#6699CC", lwd=3, lty=2)
  dev.off()
}

# Parallel test for ntree choices
test_numtree_parallel <- function(ntree_list, formula, data) {
  cores <- detectCores()-1; cl <- makeCluster(cores); registerDoParallel(cl)
  MSE <- foreach(nt=ntree_list, .combine='c', .packages='randomForest') %dopar% {
    set.seed(666); mean(randomForest(formula, data=data, ntree=nt, importance=TRUE)$mse)
  }
  stopCluster(cl); MSE
}

test_numtree_parallel_optimized <- function(ntree_list, formula, data) {
  cores <- detectCores()-1; cl <- makeCluster(cores); registerDoParallel(cl)
  MSE <- foreach(nt=ntree_list, .combine='c', .packages='randomForest') %dopar% {
    set.seed(666); mean(randomForest(formula, data=data, ntree=nt, importance=TRUE, proximity=TRUE)$mse)
  }
  stopCluster(cl); MSE
}

# RF stability selection with MSE tracking
rf_stability_selection_parallel <- function(x, y,
                                            n_bootstrap=500,
                                            threshold=0.3,
                                            ntree=500,
                                            mtry=NULL,
                                            importance_threshold=0) {
  cores <- detectCores()-1; cl <- makeCluster(cores); registerDoParallel(cl)
  sel_and_mse <- foreach(i=1:n_bootstrap, .combine=rbind, .packages='randomForest') %dopar% {
    set.seed(123+i)
    idx <- sample(nrow(x), replace=TRUE)
    m <- randomForest(x[idx,], y[idx], ntree=ntree, mtry=mtry, importance=TRUE)
    imps <- importance(m)[, '%IncMSE']
    c(as.numeric(imps > importance_threshold), mean(m$mse))
  }
  stopCluster(cl)
  sel_mat <- sel_and_mse[, 1:ncol(x)]; freqs <- colMeans(sel_mat)
  features <- names(freqs[freqs >= threshold])
  # fallback to top 5 if needed
  if(length(features)<5) features <- names(sort(freqs, decreasing=TRUE))[1:5]
  list(features=features, freqs=freqs, mse_vec=sel_and_mse[, ncol(sel_and_mse)])
}

# 3) Read thresholds & harmonized drivers
best_thresh <- read.csv("best_thresholds.csv", stringsAsFactors=FALSE)
record_length <- 5
harm_file <- sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length)
drv_all <- read.csv(harm_file, stringsAsFactors=FALSE)
vars_all <- c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
              "snow_cover","permafrost","elevation","basin_slope","RBI","recession_slope",
              grep("^land_|^rocks_", names(drv_all), value=TRUE))
vars <- intersect(vars_all, names(drv_all))
rl_cols <- grep("^(land_|rocks_)", names(drv_all), value=TRUE)
drv_all <- drv_all %>% mutate(across(all_of(rl_cols), ~ replace_na(., 0)))

# 4) Load splits
load_split <- function(path) {
  read.csv(path, stringsAsFactors=FALSE) %>%
    mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
    select(-contains("Gen"), -contains("major"), -Max_Daylength, -Q, -drainage_area) %>%
    mutate(greenup_day = as.numeric(greenup_day))
}
df_train   <- load_split("AllDrivers_cc_older70.csv")
df_recent30 <- load_split("AllDrivers_cc_recent30.csv")
df_unseen10 <- load_split("AllDrivers_cc_unseen10.csv")

# 5) Loop over responses and run pipeline
metrics_list <- list(); preds_list <- list(); features_list <- list()
for(resp in c("FNConc","FNYield")) {
  message("--- Processing ", resp)
  # threshold settings
  thr_imp  <- best_thresh %>% filter(response==resp) %>% pull(importance)
  thr_freq <- best_thresh %>% filter(response==resp) %>% pull(stability)
  
  # prepare train data
  train_cc <- df_train %>% drop_na(c(resp, vars))
  x_train <- train_cc %>% select(all_of(vars)); y_train <- train_cc[[resp]]
  
  # 5a) RF1 tuning on older70
  ntree_vals <- seq(100, 2000, by=100)
  mse_rf1   <- test_numtree_parallel(ntree_vals, as.formula(paste(resp, "~ .")), train_cc)
  best_nt1  <- ntree_vals[which.min(mse_rf1)]
  tune_mtry <- tuneRF(x_train, y_train, ntreeTry=best_nt1, stepFactor=1.5, improve=0.01, plot=FALSE)
  best_mtry1<- tune_mtry[which.min(tune_mtry[,2]),1]
  rf1       <- randomForest(x_train, y_train, ntree=best_nt1, mtry=best_mtry1, importance=TRUE)
  
  # importance cutoff
  imps      <- importance(rf1)[, '%IncMSE']
  thr_imp   <- quantile(imps[imps>0], thr_imp)
  
  # 5b) Stability selection
  stab_res  <- rf_stability_selection_parallel(
    x=x_train, y=y_train,
    n_bootstrap=500, threshold=thr_freq,
    ntree=best_nt1, mtry=best_mtry1,
    importance_threshold=thr_imp
  )
  feats     <- stab_res$features; features_list[[resp]] <- feats
  
  # 5c) RF2 tuning on older70
  df2       <- train_cc[, c(resp, feats)];
  mse_rf2   <- test_numtree_parallel_optimized(ntree_vals,
                                               as.formula(paste(resp, "~ .")), df2)
  best_nt2  <- ntree_vals[which.min(mse_rf2)]
  tune_mtry2<- tuneRF(df2[feats], df2[[resp]], ntreeTry=best_nt2, stepFactor=1.5, improve=0.01, plot=FALSE)
  best_mtry2<- tune_mtry2[which.min(tune_mtry2[,2]),1]
  rf2       <- randomForest(as.formula(paste(resp, "~ .")), data=df2,
                            ntree=best_nt2, mtry=best_mtry2, importance=TRUE)
  
  # 5d) Predict on recent30 & unseen10
  for(sub in c("recent30","unseen10")) {
    df_test <- get(paste0("df_", sub)) %>% drop_na(c(resp, feats))
    if(nrow(df_test)<5) next
    pred    <- predict(rf2, df_test); obs <- df_test[[resp]]
    key     <- paste(sub, resp, sep="_")
    metrics_list[[key]] <- tibble(
      subset=sub, response=resp,
      R2=cor(obs, pred)^2,
      RMSE=sqrt(mean((obs-pred)^2)),
      pRMSE=100*sqrt(mean((obs-pred)^2))/mean(obs)
    )
    preds_list[[key]]   <- tibble(subset=sub, response=resp, observed=obs, predicted=pred)
  }
  
  # diagnostic plots
  save_correlation_plot(cor(x_train), paste0(resp, "_older70"))
  save_rf_importance_plot(rf2, paste0(resp, "_RF2"))
  save_lm_plot(predict(rf2, train_cc), y_train, paste0(resp, "_train"))
}

# 6) Save outputs
metrics_df  <- bind_rows(metrics_list); write.csv(metrics_df, file.path(output_dir, "metrics_final.csv"), row.names=FALSE)
preds_df    <- bind_rows(preds_list);   write.csv(preds_df,   file.path(output_dir, "predictions_final.csv"), row.names=FALSE)
features_df <- tibble(response=names(features_list), kept_vars=map_chr(features_list, paste, collapse='; '))
write.csv(features_df, file.path(output_dir, "Retained_Variables_Per_Model.csv"), row.names=FALSE)

# 7) Generate SHAP values on older70 models
# Load feature list and SHAP dependencies
library(iml)
feat_df <- read.csv(file.path(output_dir, "Retained_Variables_Per_Model.csv"), stringsAsFactors=FALSE)
for(resp in feat_df$response) {
  kept <- strsplit(feat_df$kept_vars[feat_df$response == resp], "; ")[[1]]
  # Load tuned RF2 model for this response
  model_file <- file.path(output_dir, sprintf("%s_RF2_model.RData", resp))
  load(model_file)  # should load object `rf2`
  # Prepare data for SHAP (older70)
  train_cc <- df_train %>% drop_na(c(resp, kept))
  X_train  <- train_cc[, kept, drop=FALSE]
  y_train  <- train_cc[[resp]]
  # Create predictor and compute SHAP
  predictor <- Predictor$new(rf2, data = X_train, y = y_train, type = "regression")
  shap <- Shapley$new(predictor, sample.size = nrow(X_train))
  # Save SHAP results
  out_file <- file.path(output_dir, sprintf("%s_older70_shap.csv", resp))
  write.csv(shap$results, out_file, row.names = FALSE)
  message("Saved SHAP for ", resp, " to ", out_file)
}

# End of pipeline
