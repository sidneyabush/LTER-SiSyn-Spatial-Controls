# ──────────────────────────────────────────────────────────────────────────────
# RF Pipeline with fixed importance & lower stability threshold
# Subsets: unseen10, older70, recent30
# Responses: FNConc, FNYield
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load required packages & set seed ----------------------------------------
librarian::shelf(
  dplyr, tidyr, randomForest, corrplot,
  parallel, doParallel, foreach
)
set.seed(666)
record_length <- 5

# 1) Read harmonized data & define predictors --------------------------------
tot_si <- read.csv(
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length),
  stringsAsFactors = FALSE
)

var_order <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI",
  "recession_slope",
  grep("^land_|^rocks_", names(tot_si), value = TRUE)
)
var_order_present <- intersect(var_order, names(tot_si))

# zero‐fill land_/rocks_ so you don't lose rows
rl_cols <- grep("^(land_|rocks_)", names(tot_si), value = TRUE)
tot_si <- tot_si %>%
  mutate(across(all_of(rl_cols), ~ replace_na(., 0)))

# 2) Tune global importance thresholds ----------------------------------------

# 2a) FNConc
complete_conc <- tot_si %>%
  select(FNConc, all_of(var_order_present)) %>%
  drop_na()
if (nrow(complete_conc) == 0) stop("No data for FNConc tuning")

rf_full_conc <- randomForest(
  FNConc ~ .,
  data       = complete_conc,
  ntree      = 500,
  mtry       = floor(sqrt(ncol(complete_conc) - 1)),
  importance = TRUE
)
imp_vals_conc <- importance(rf_full_conc)[, "%IncMSE"]

thr_grid <- seq(0, max(imp_vals_conc, na.rm = TRUE), length.out = 10)
tune_conc <- data.frame(threshold = thr_grid,
                        n_features = 0,
                        oob_mse    = NA_real_)

for (i in seq_along(thr_grid)) {
  thr <- thr_grid[i]
  sel_mat <- replicate(200, {
    idx <- sample(nrow(complete_conc), replace = TRUE)
    m   <- randomForest(
      complete_conc[idx, var_order_present, drop = FALSE],
      complete_conc$FNConc[idx],
      ntree      = 500,
      mtry       = floor(sqrt(length(var_order_present))),
      importance = TRUE
    )
    as.numeric(importance(m)[, "%IncMSE"] > thr)
  })
  freqs <- rowMeans(sel_mat)
  feats <- names(freqs[freqs >= 0.8])
  tune_conc$n_features[i] <- length(feats)
  if (length(feats) > 0) {
    rf2 <- randomForest(
      FNConc ~ .,
      data       = complete_conc[, c("FNConc", feats), drop = FALSE],
      ntree      = 500,
      mtry       = floor(sqrt(length(feats))),
      importance = FALSE
    )
    tune_conc$oob_mse[i] <- tail(rf2$mse, 1)
  }
}
write.csv(tune_conc, "threshold_tuning_FNConc.csv", row.names = FALSE)
valid_conc <- subset(tune_conc, n_features > 0)
importance_threshold_conc <- if (nrow(valid_conc)) {
  valid_conc$threshold[which.min(valid_conc$oob_mse)]
} else 0
message("FNConc importance threshold = ", importance_threshold_conc)

# 2b) FNYield
complete_yield <- tot_si %>%
  select(FNYield, all_of(var_order_present)) %>%
  drop_na()
if (nrow(complete_yield) == 0) stop("No data for FNYield tuning")

rf_full_yield <- randomForest(
  FNYield ~ .,
  data       = complete_yield,
  ntree      = 500,
  mtry       = floor(sqrt(ncol(complete_yield) - 1)),
  importance = TRUE
)
imp_vals_yield <- importance(rf_full_yield)[, "%IncMSE"]

tune_yield <- data.frame(threshold = thr_grid,
                         n_features = 0,
                         oob_mse    = NA_real_)
for (i in seq_along(thr_grid)) {
  thr <- thr_grid[i]
  sel_mat <- replicate(200, {
    idx <- sample(nrow(complete_yield), replace = TRUE)
    m   <- randomForest(
      complete_yield[idx, var_order_present, drop = FALSE],
      complete_yield$FNYield[idx],
      ntree      = 500,
      mtry       = floor(sqrt(length(var_order_present))),
      importance = TRUE
    )
    as.numeric(importance(m)[, "%IncMSE"] > thr)
  })
  freqs <- rowMeans(sel_mat)
  feats <- names(freqs[freqs >= 0.8])
  tune_yield$n_features[i] <- length(feats)
  if (length(feats) > 0) {
    rf2 <- randomForest(
      FNYield ~ .,
      data       = complete_yield[, c("FNYield", feats), drop = FALSE],
      ntree      = 500,
      mtry       = floor(sqrt(length(feats))),
      importance = FALSE
    )
    tune_yield$oob_mse[i] <- tail(rf2$mse, 1)
  }
}
write.csv(tune_yield, "threshold_tuning_FNYield.csv", row.names = FALSE)
valid_yield <- subset(tune_yield, n_features > 0)
importance_threshold_yield <- if (nrow(valid_yield)) {
  valid_yield$threshold[which.min(valid_yield$oob_mse)]
} else 0
message("FNYield importance threshold = ", importance_threshold_yield)

# 3) Set a lower stability threshold ------------------------------------------
stability_threshold <- 0.6

rf_stability <- function(x, y, thr_imp, thr_freq = stability_threshold) {
  sel_mat <- replicate(500, {
    idx <- sample(nrow(x), replace = TRUE)
    m   <- randomForest(
      x[idx, , drop = FALSE],
      y[idx],
      ntree      = 500,
      mtry       = floor(sqrt(ncol(x))),
      importance = TRUE
    )
    as.numeric(importance(m)[, "%IncMSE"] > thr_imp)
  })
  freqs <- rowMeans(sel_mat)
  names(freqs[freqs >= thr_freq])
}

# 4) Utility functions --------------------------------------------------------
save_corr <- function(mat, dir) {
  png(file.path(dir, "corrplot.png"), width = 2000, height = 2000, res = 300)
  corrplot(mat, type="lower", tl.col="black", diag=FALSE)
  dev.off()
}
save_imp <- function(mod, dir, prefix) {
  png(file.path(dir, paste0(prefix, "_varImp.png")), width = 1600, height = 1200, res = 300)
  varImpPlot(mod, main = paste(prefix, "Variable Importance"))
  dev.off()
}
save_pred_obs <- function(mod, obs, dir, prefix) {
  preds <- mod$predicted
  rmse  <- sqrt(mean((preds - obs)^2, na.rm=TRUE))
  rsq   <- mean(mod$rsq, na.rm=TRUE)
  png(file.path(dir, paste0(prefix, "_pred_obs.png")), width = 1500, height = 1500, res = 300)
  plot(preds, obs, pch=16, cex=1.5, xlab="Predicted", ylab="Observed",
       main=paste(prefix, "Pred vs Obs"))
  abline(0,1,lty=2); legend("topleft", bty="n", legend=paste("R²=", round(rsq,3)))
  legend("bottomright", bty="n", legend=paste("RMSE=", round(rmse,3)))
  dev.off()
}
test_nt <- function(ntv, form, dat) {
  cores <- detectCores()-1; cl <- makeCluster(cores); registerDoParallel(cl)
  res <- foreach(nt=ntv, .combine='c', .packages='randomForest') %dopar% {
    set.seed(666)
    m <- randomForest(form, data=dat, ntree=nt)
    mean(m$mse, na.rm=TRUE)
  }
  stopCluster(cl)
  res
}

# 5) Run per‐subset RF pipelines ------------------------------------------------
subset_names <- c("unseen10","older70","recent30")
ntree_vals   <- seq(100, 2000, by=100)
output_root  <- "/Users/.../Final_Models"
dir.create(output_root, recursive=TRUE, showWarnings=FALSE)

for (sub in subset_names) {
  message("===== SUBSET: ", sub, " =====")
  df <- read.csv(sprintf("AllDrivers_cc_%s.csv", sub), stringsAsFactors=FALSE)
  df <- df %>% mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
    select(-contains("Gen"), -contains("major"),
           -Max_Daylength, -Q, -drainage_area) %>%
    mutate(greenup_day = as.numeric(greenup_day))
  
  for (resp in c("FNConc","FNYield")) {
    message("---- RESPONSE: ", resp, " ----")
    out_dir <- file.path(output_root, sub, resp)
    dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
    
    dat <- df %>%
      select(all_of(c(resp, var_order_present))) %>%
      mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
      na.omit()
    if (nrow(dat) < 5) {
      message("  insufficient data; skipping")
      next
    }
    
    # 5a) Correlation plot
    save_corr(cor(dat %>% select(-all_of(resp)), use="pairwise.complete.obs"), out_dir)
    
    # 5b) RF1 tuning & fit
    form1 <- as.formula(paste(resp, "~ ."))
    mse1  <- test_nt(ntree_vals, form1, dat)
    best_nt <- ntree_vals[which.min(mse1)]
    tune   <- randomForest::tuneRF(
      x         = dat %>% select(-all_of(resp)),
      y         = dat[[resp]],
      ntreeTry  = best_nt,
      stepFactor= 1,
      improve   = 0.5,
      plot      = FALSE
    )
    best_mtry <- tune[which.min(tune[,2]),1]
    set.seed(666)
    rf1 <- randomForest(form1, data=dat, ntree=best_nt, mtry=best_mtry, importance=TRUE)
    save_imp(rf1, out_dir, "RF1")
    
    # 5c) Stability & RF2
    thr_use <- if (resp=="FNConc") importance_threshold_conc else importance_threshold_yield
    x <- dat %>% select(-all_of(resp))
    y <- dat[[resp]]
    sel_feats <- rf_stability(x, y, thr_imp=thr_use)
    if (length(sel_feats)==0) {
      message("  no stable features; skipping RF2")
      next
    }
    form2 <- as.formula(paste(resp, "~", paste(sel_feats, collapse="+")))
    set.seed(666)
    rf2 <- randomForest(form2, data=dat, ntree=best_nt, mtry=max(1,floor(sqrt(length(sel_feats)))), importance=TRUE)
    save_imp(rf2, out_dir, "RF2")
    save_pred_obs(rf2, dat[[resp]], out_dir, "RF2")
    
    # 5d) Save stability frequencies
    freqs <- rowMeans(replicate(500, {
      idx <- sample(nrow(x), replace=TRUE)
      m   <- randomForest(x[idx, sel_feats, drop=FALSE], y[idx], ntree=500,
                          mtry=best_mtry, importance=TRUE)
      as.numeric(importance(m)[, "%IncMSE"] > thr_use)
    }))
    write.csv(data.frame(Feature=names(freqs), Frequency=freqs),
              file.path(out_dir, "stability_frequencies.csv"), row.names=FALSE)
  }
}
