#!/usr/bin/env Rscript
# 02_final_rf_pipeline_robust.R
# ──────────────────────────────────────────────────────────────────────────────
# Train RF1 on older70, run stability selection, train RF2, predict on recent30 & unseen10
# with progress bars for ntree scan and bootstraps.
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load packages & clear
librarian::shelf(
  randomForest, dplyr, tidyr, purrr, tibble,
  parallel, doParallel, corrplot, ggplot2,
  pbmcapply, pbapply
)
rm(list=ls()); set.seed(666)

# 1) Paths
drv_dir    <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files"
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"
setwd(drv_dir)
dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)

# 2) Utility plots
save_corr <- function(mat,name){
  png(file.path(output_dir,paste0(name,"_corrplot.png")),2500,2500,res=300)
  corrplot(mat,type="lower",tl.col="black",diag=FALSE); title(name); dev.off()
}
save_varimp <- function(mod,name){
  png(file.path(output_dir,paste0(name,"_varImp.png")),1600,1200,300)
  randomForest::varImpPlot(mod,main=name); dev.off()
}
save_predobs <- function(pred,obs,name){
  png(file.path(output_dir,paste0(name,"_pred_vs_obs.png")),1500,1500,300)
  plot(pred,obs,pch=16,cex=1.5,main=name,xlab="Predicted",ylab="Observed")
  abline(0,1,lty=2); dev.off()
}

# 3) ntree scan via x/y interface w/ progress bar
test_numtree_parallel <- function(tree_grid, x, y) {
  ncores <- parallel::detectCores() - 1
  pbmcapply::pbmclapply(
    tree_grid,
    function(nt) {
      set.seed(666)
      rf <- randomForest::randomForest(
        x=x, y=y,
        importance=TRUE, proximity=TRUE,
        ntree=nt
      )
      mean(rf$mse)
    },
    mc.cores = ncores
  ) %>% unlist()
}

# 4) Stability selection via x/y interface w/ progress bar
auto_stability <- function(x, y, ntree, mtry, imp_thr, freq_thr, n_boot=500) {
  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  sel_mat <- pbapply::pbsapply(
    1:n_boot,
    function(i) {
      set.seed(123 + i)
      idx <- sample(nrow(x), replace=TRUE)
      rf  <- randomForest::randomForest(
        x = x[idx, , drop=FALSE],
        y = y[idx],
        ntree      = ntree,
        mtry       = mtry,
        importance = TRUE
      )
      imps <- randomForest::importance(rf)[, "%IncMSE"]
      as.integer(imps > imp_thr)
    },
    cl = cl
  )
  parallel::stopCluster(cl)
  freqs <- rowMeans(sel_mat)
  feats <- names(freqs[freqs >= freq_thr])
  if(length(feats) < 5) feats <- names(sort(freqs, decreasing=TRUE))[1:5]
  list(features = feats, frequencies = freqs)
}

# 5) Read data & splits
rec_len   <- 5
drv_all   <- read.csv(sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", rec_len))
vars      <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI","recession_slope",
  grep("^land_|^rocks_", names(drv_all), value=TRUE)
)
predictors <- intersect(vars, names(drv_all))
rl_cols    <- grep("^(land_|rocks_)", names(drv_all), value=TRUE)
load_split <- function(path) {
  read.csv(path) %>%
    mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
    select(-contains("Gen"), -contains("major"), -Max_Daylength, -Q, -drainage_area) %>%
    mutate(greenup_day = as.numeric(greenup_day))
}
df_train    <- load_split("AllDrivers_cc_older70.csv")
df_recent30 <- load_split("AllDrivers_cc_recent30.csv")
df_unseen10 <- load_split("AllDrivers_cc_unseen10.csv")

# 6) Core pipeline
tree_grid    <- seq(100, 2000, 100)
results_met  <- list()
results_pred <- list()
results_feats<- list()

for(resp in c("FNConc","FNYield")) {
  message("→ ", resp)
  tr <- df_train %>% drop_na(all_of(c(resp, predictors)))
  x  <- tr[predictors]
  y  <- tr[[resp]]
  
  # RF1 tuning
  mse1 <- test_numtree_parallel(tree_grid, x, y)
  nt1  <- tree_grid[which.min(mse1)]
  t1   <- randomForest::tuneRF(x, y, ntreeTry=nt1, stepFactor=1.5, improve=0.01, plot=FALSE)
  mtry1<- t1[which.min(t1[,2]),1]
  rf1  <- randomForest::randomForest(x, y, ntree=nt1, mtry=mtry1, importance=TRUE)
  
  # thresholds & stability
  imp_thr  <- quantile(randomForest::importance(rf1)[, "%IncMSE"], 0.50)
  freq_thr <- 0.80
  stab     <- auto_stability(x, y, nt1, mtry1, imp_thr, freq_thr, n_boot=500)
  feats    <- stab$features; results_feats[[resp]] <- feats
  
  # RF2 tuning
  x2   <- x[, feats, drop=FALSE]
  y2   <- y
  mse2 <- test_numtree_parallel(tree_grid, x2, y2)
  nt2  <- tree_grid[which.min(mse2)]
  t2   <- randomForest::tuneRF(x2, y2, ntreeTry=nt2, stepFactor=1.5, improve=0.01, plot=FALSE)
  mtry2<- t2[which.min(t2[,2]),1]
  rf2  <- randomForest::randomForest(x2, y2, ntree=nt2, mtry=mtry2, importance=TRUE)
  save(rf2, file=file.path(output_dir, paste0(resp,"_RF2.RData")))
  
  # Predict on recent30 & unseen10
  for(sub in c("recent30","unseen10")) {
    df_te <- get(paste0("df_",sub)) %>% drop_na(all_of(c(resp, feats)))
    if(nrow(df_te) < 5) next
    pr <- predict(rf2, df_te); ob <- df_te[[resp]]
    key <- paste(sub, resp, sep="_")
    results_met[[key]]  <- tibble(
      subset   = sub,
      response = resp,
      R2       = cor(ob, pr)^2,
      RMSE     = sqrt(mean((ob-pr)^2)),
      pRMSE    = 100 * RMSE / mean(ob)
    )
    results_pred[[key]] <- tibble(
      subset    = sub,
      response  = resp,
      observed  = ob,
      predicted = pr
    )
  }
  
  # Diagnostics
  save_corr(cor(x), paste0(resp,"_older70"))
  save_varimp(rf2, paste0(resp,"_RF2"))
  save_predobs(predict(rf2, tr), y, paste0(resp,"_train"))
}

# 7) Save outputs
tibble(
  response  = names(results_feats),
  kept_vars = map_chr(results_feats, paste, collapse="; ")
) %>% write.csv(file.path(output_dir,"Retained_Variables_Per_Model.csv"), row.names=FALSE)

bind_rows(results_met)  %>% write.csv(file.path(output_dir,"Metrics_Final.csv"), row.names=FALSE)
bind_rows(results_pred) %>% write.csv(file.path(output_dir,"Predictions_Final.csv"), row.names=FALSE)

message("Training & evaluation complete. Run Step3.")
