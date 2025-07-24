#!/usr/bin/env Rscript
# 02_final_rf_pipeline_robust.R
# ──────────────────────────────────────────────────────────────────────────────
# Train final RF models on older70, stability‑select features with robust parameters,
# then predict on recent30 & unseen10, and compute SHAP values on older70.
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load needed packages & clear environment
librarian::shelf(
  randomForest, dplyr, tidyr, purrr, tibble,
  parallel, doParallel, foreach, corrplot, ggplot2, fastshap
)
rm(list = ls()); set.seed(666)

# 1) Set working & output directories
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 2) Utility functions
save_correlation_plot <- function(driver_cor, name) {
  png(file.path(output_dir, paste0(name, "_corrplot.png")), width=2500, height=2500, res=300)
  corrplot(driver_cor, type="lower", tl.col="black", diag=FALSE)
  title(name); dev.off()
}

save_rf_importance_plot <- function(model, name) {
  png(file.path(output_dir, paste0(name, "_varImp.png")), width=1600, height=1200, res=300)
  randomForest::varImpPlot(model, main=name); dev.off()
}

save_lm_plot <- function(preds, obs, name) {
  png(file.path(output_dir, paste0(name, "_pred_vs_obs.png")), width=1500, height=1500, res=300)
  plot(preds, obs, pch=16, cex=1.5, xlab="Predicted", ylab="Observed", main=name)
  abline(0,1,lty=2); dev.off()
}

# 3) Parallel ntree testing functions
test_numtree_parallel <- function(ntree_list, formula, df) {
  cores <- detectCores() - 1; cl <- makeCluster(cores); registerDoParallel(cl)
  MSE <- foreach(nt=ntree_list, .combine='c', .packages='randomForest') %dopar% {
    set.seed(666)
    rf <- randomForest::randomForest(formula, data=df, importance=TRUE, proximity=TRUE, ntree=nt)
    mean(rf$mse)
  }
  stopCluster(cl); MSE
}

test_numtree_parallel_optimized <- test_numtree_parallel  # same as above for consistency

# 4) Stability selection
auto_stability <- function(x, y, ntree, mtry, thr_imp, thr_freq, n_boot=500) {
  cores <- detectCores() - 1; cl <- makeCluster(cores); registerDoParallel(cl)
  sel_mat <- foreach(i=1:n_boot, .combine=rbind, .packages='randomForest') %dopar% {
    set.seed(123+i)
    idx <- sample(nrow(x), replace=TRUE)
    rf <- randomForest::randomForest(x[idx,], y[idx], ntree=ntree, mtry=mtry, importance=TRUE)
    imps <- randomForest::importance(rf)[,'%IncMSE']
    c(as.integer(imps > thr_imp), mean(rf$mse))
  }
  stopCluster(cl)
  freqs <- colMeans(sel_mat[,1:ncol(x)])
  feats <- names(freqs[freqs >= thr_freq])
  if(length(feats) < 5) feats <- names(sort(freqs, decreasing=TRUE))[1:5]
  list(features=feats, frequencies=freqs)
}

# 5) Read data & define predictors
record_length <- 5
drv_file <- sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length)
drv_all <- read.csv(drv_file)
vars <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI","recession_slope",
  grep("^land_|^rocks_", names(drv_all), value=TRUE)
)
predictors <- intersect(vars, names(drv_all))
rl_cols <- grep("^(land_|rocks_)", names(drv_all), value=TRUE)

# 6) Load splits
load_split <- function(path) {
  read.csv(path) %>% mutate(across(all_of(rl_cols), ~ replace_na(.,0))) %>%
    select(-contains("Gen"), -contains("major"), -Max_Daylength, -Q, -drainage_area) %>%
    mutate(greenup_day=as.numeric(greenup_day))
}
df_train    <- load_split("AllDrivers_cc_older70.csv")
df_recent30 <- load_split("AllDrivers_cc_recent30.csv")
df_unseen10 <- load_split("AllDrivers_cc_unseen10.csv")

# 7) Core pipeline
tree_grid <- seq(100,2000,100)
results_metrics <- list(); results_preds <- list(); results_feats <- list()

for(resp in c("FNConc","FNYield")) {
  message("Processing ", resp)
  df_tr <- df_train %>% drop_na(all_of(c(resp,predictors)))
  x <- df_tr[predictors]; y <- df_tr[[resp]]
  # RF1 tuning
  mse1 <- test_numtree_parallel(tree_grid, as.formula(paste(resp,"~.")), df_tr)
  nt1 <- tree_grid[which.min(mse1)]
  tr1 <- randomForest::tuneRF(x,y, ntreeTry=nt1, stepFactor=1.5, improve=0.01, plot=FALSE)
  mtry1 <- tr1[which.min(tr1[,2]),1]
  rf1 <- randomForest::randomForest(x,y, ntree=nt1, mtry=mtry1, importance=TRUE)
  # thresholds
  imp_thr <- quantile(randomForest::importance(rf1)[,'%IncMSE'], 0.5)
  freq_thr <- 0.8
  stab <- auto_stability(x,y, nt1, mtry1, imp_thr, freq_thr, n_boot=500)
  feats <- stab$features; results_feats[[resp]] <- feats
  # RF2 tuning
  df2 <- df_tr[c(resp,feats)];
  mse2 <- test_numtree_parallel_optimized(tree_grid, as.formula(paste(resp,"~.")), df2)
  nt2 <- tree_grid[which.min(mse2)]
  tr2 <- randomForest::tuneRF(df2[feats], df2[[resp]], ntreeTry=nt2, stepFactor=1.5, improve=0.01, plot=FALSE)
  mtry2 <- tr2[which.min(tr2[,2]),1]
  rf2 <- randomForest::randomForest(as.formula(paste(resp,"~.")), data=df2, ntree=nt2, mtry=mtry2, importance=TRUE)
  # Save RF2 model for downstream SHAP
  save(rf2, file = file.path(output_dir, sprintf("%s_RF2.RData", resp)))
  # Evaluate
  for(sub in c("recent30","unseen10")) {
    df_te <- get(paste0("df_",sub)) %>% drop_na(all_of(c(resp,feats)))
    if(nrow(df_te)<5) next
    pred <- predict(rf2, df_te); obs <- df_te[[resp]]
    key <- paste(sub,resp,sep="_")
    results_metrics[[key]] <- tibble(
      subset=sub, response=resp,
      R2=cor(obs,pred)^2,
      RMSE=sqrt(mean((obs-pred)^2)),
      pRMSE=100*RMSE/mean(obs)
    )
    results_preds[[key]] <- tibble(subset=sub,response=resp,observed=obs,predicted=pred)
  }
  # Diagnostics
  save_correlation_plot(cor(x), paste0(resp,"_older70"))
  save_rf_importance_plot(rf2, paste0(resp,"_RF2"))
  save_lm_plot(predict(rf2,df_tr), df_tr[[resp]], paste0(resp,"_train"))
}

# 8) Save outputs
tibble(response=names(results_feats), kept_vars=map_chr(results_feats,paste,collapse="; ")) %>%
  write.csv(file.path(output_dir,"Retained_Variables_Per_Model.csv"),row.names=FALSE)
bind_rows(results_metrics) %>%
  write.csv(file.path(output_dir,"Metrics_Final.csv"),row.names=FALSE)
bind_rows(results_preds) %>%
  write.csv(file.path(output_dir,"Predictions_Final.csv"),row.names=FALSE)

# 9) SHAP on older70
for(resp in names(results_feats)) {
  feats <- results_feats[[resp]]
  load(file.path(output_dir,sprintf("%s_RF2.RData",resp)))  # loads rf2
  df_tr <- df_train %>% drop_na(all_of(c(resp,feats)))
  X <- df_tr[,feats,drop=FALSE]
  wrap <- function(obj,newdata) predict(obj,newdata=newdata)
  shap <- fastshap::explain(rf2, X=X, pred_wrapper=wrap, nsim=30, .parallel=TRUE)
  write.csv(shap, file.path(output_dir,sprintf("%s_older70_shap.csv",resp)), row.names=FALSE)
}

# End of pipeline
