# ──────────────────────────────────────────────────────────────────────────────
# 0) Setup ---------------------------------------------------------------------
librarian::shelf(
  dplyr, tidyr, randomForest, corrplot, parallel, doParallel, foreach
)

set.seed(666)
record_length <- 5

# 1) Read and prepare harmonized data -----------------------------------------
tot_si <- read.csv(
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length),
  stringsAsFactors = FALSE
)

# Identify your predictor variables
var_order <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI",
  "recession_slope",
  grep("^land_|^rocks_", names(tot_si), value = TRUE)
)
# Only keep those actually present
var_order_present <- intersect(var_order, names(tot_si))

# Zero‐fill any land_/rocks_ columns so they don’t drop entire rows
rl_cols <- grep("^(land_|rocks_)", names(tot_si), value = TRUE)
tot_si <- tot_si %>%
  dplyr::mutate(across(dplyr::all_of(rl_cols), ~ tidyr::replace_na(., 0)))

# 2) Tune a global importance threshold for FNConc ----------------------------

# Build complete‐case table for FNConc + predictors
complete_conc <- tot_si %>%
  dplyr::select(FNConc, dplyr::all_of(var_order_present)) %>%
  tidyr::drop_na()
if (nrow(complete_conc) == 0) stop("No complete cases for FNConc threshold tuning.")

# Fit a full RF to get raw %IncMSE
imp_rf_conc <- randomForest::randomForest(
  FNConc ~ .,
  data       = complete_conc,
  ntree      = 500,
  mtry       = floor(sqrt(ncol(complete_conc) - 1)),
  importance = TRUE
)
imp_vals_conc <- randomForest::importance(imp_rf_conc)[, "%IncMSE"]

# Grid search over importance thresholds
thr_grid      <- seq(0, max(imp_vals_conc, na.rm = TRUE), length.out = 10)
tune_conc     <- data.frame(
  threshold  = thr_grid,
  n_features = integer(length(thr_grid)),
  oob_mse    = numeric(length(thr_grid))
)
for (i in seq_along(thr_grid)) {
  thr <- thr_grid[i]
  # bootstrap stability selection
  sel_mat <- replicate(200, {
    idx <- sample(nrow(complete_conc), replace = TRUE)
    mod <- randomForest::randomForest(
      complete_conc[idx, var_order_present, drop=FALSE],
      complete_conc$FNConc[idx],
      ntree      = 500,
      mtry       = floor(sqrt(length(var_order_present))),
      importance = TRUE
    )
    as.numeric(randomForest::importance(mod)[, "%IncMSE"] > thr)
  })
  freqs <- rowMeans(sel_mat)
  feats <- names(freqs[freqs >= 0.8])
  tune_conc$n_features[i] <- length(feats)
  if (length(feats) > 0) {
    rf2 <- randomForest::randomForest(
      FNConc ~ .,
      data       = complete_conc[, c("FNConc", feats), drop=FALSE],
      ntree      = 500,
      mtry       = floor(sqrt(length(feats))),
      importance = FALSE
    )
    tune_conc$oob_mse[i] <- tail(rf2$mse, 1)
  }
}
write.csv(tune_conc, "threshold_tuning_FNConc.csv", row.names = FALSE)
valid_conc <- tune_conc[tune_conc$n_features > 0, ]
importance_threshold_conc <- if (nrow(valid_conc)) {
  valid_conc$threshold[which.min(valid_conc$oob_mse)]
} else 0
message("FNConc importance threshold = ", importance_threshold_conc)

# 3) Tune a global importance threshold for FNYield ---------------------------

complete_yield <- tot_si %>%
  dplyr::select(FNYield, dplyr::all_of(var_order_present)) %>%
  tidyr::drop_na()
if (nrow(complete_yield) == 0) stop("No complete cases for FNYield threshold tuning.")

imp_rf_yield <- randomForest::randomForest(
  FNYield ~ .,
  data       = complete_yield,
  ntree      = 500,
  mtry       = floor(sqrt(ncol(complete_yield) - 1)),
  importance = TRUE
)
imp_vals_yield <- randomForest::importance(imp_rf_yield)[, "%IncMSE"]

tune_yield     <- data.frame(
  threshold  = thr_grid,
  n_features = integer(length(thr_grid)),
  oob_mse    = numeric(length(thr_grid))
)
for (i in seq_along(thr_grid)) {
  thr <- thr_grid[i]
  sel_mat <- replicate(200, {
    idx <- sample(nrow(complete_yield), replace = TRUE)
    mod <- randomForest::randomForest(
      complete_yield[idx, var_order_present, drop=FALSE],
      complete_yield$FNYield[idx],
      ntree      = 500,
      mtry       = floor(sqrt(length(var_order_present))),
      importance = TRUE
    )
    as.numeric(randomForest::importance(mod)[, "%IncMSE"] > thr)
  })
  freqs <- rowMeans(sel_mat)
  feats <- names(freqs[freqs >= 0.8])
  tune_yield$n_features[i] <- length(feats)
  if (length(feats) > 0) {
    rf2 <- randomForest::randomForest(
      FNYield ~ .,
      data       = complete_yield[, c("FNYield", feats), drop=FALSE],
      ntree      = 500,
      mtry       = floor(sqrt(length(feats))),
      importance = FALSE
    )
    tune_yield$oob_mse[i] <- tail(rf2$mse, 1)
  }
}
write.csv(tune_yield, "threshold_tuning_FNYield.csv", row.names = FALSE)
valid_yield <- tune_yield[tune_yield$n_features > 0, ]
importance_threshold_yield <- if (nrow(valid_yield)) {
  valid_yield$threshold[which.min(valid_yield$oob_mse)]
} else 0
message("FNYield importance threshold = ", importance_threshold_yield)

# 4) Define utility functions and RF hyper‐tuning -----------------------------

save_corr <- function(mat, dir, name) {
  png(file.path(dir, paste0(name, "_corr.png")), w=2000,h=2000,res=300)
  corrplot::corrplot(mat, type="lower", tl.col="black", diag=FALSE)
  dev.off()
}
save_imp <- function(mod, dir, prefix) {
  png(file.path(dir, paste0(prefix, "_varImp.png")), w=1600,h=1200,res=300)
  randomForest::varImpPlot(mod, main=paste(prefix,"VarImp"))
  dev.off()
}
save_pred_obs <- function(mod, obs, dir, prefix) {
  preds <- mod$predicted
  rmse  <- sqrt(mean((preds-obs)^2))
  rsq   <- mean(mod$rsq)
  png(file.path(dir, paste0(prefix, "_pred_obs.png")), w=1500,h=1500,res=300)
  plot(preds, obs, pch=16, cex=1.5, xlab="Predicted", ylab="Observed",
       main=paste(prefix,"Pred vs Obs"))
  abline(0,1,lty=2); legend("topleft", bty="n", legend=paste("R²=",round(rsq,3)))
  legend("bottomright", bty="n", legend=paste("RMSE=",round(rmse,3)))
  dev.off()
}
test_nt <- function(ntv, form, dat) {
  cores <- parallel::detectCores()-1; cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  res <- foreach(nt=ntv, .combine='c', .packages='randomForest') %dopar% {
    set.seed(666); m <- randomForest::randomForest(form, data=dat, ntree=nt); mean(m$mse)
  }
  stopCluster(cl); res
}

# 5) Run per‐subset pipelines -------------------------------------------------

subset_names <- c("unseen10", "older70", "recent30")
ntree_vals   <- seq(100, 2000, 100)
output_root  <- "/Users/sidneybush/.../Final_Models"
dir.create(output_root, recursive=TRUE, showWarnings=FALSE)

for (sub in subset_names) {
  message("===== SUBSET: ", sub, " =====")
  df <- read.csv(sprintf("AllDrivers_cc_%s.csv", sub), stringsAsFactors=FALSE)
  
  # zero‐fill again
  df <- df %>% dplyr::mutate(across(dplyr::all_of(rl_cols), ~ replace_na(.,0)))
  df <- df %>% dplyr::select(
    -dplyr::contains("Yield"), -dplyr::contains("Gen"),
    -dplyr::contains("major"),
    -Max_Daylength, -Q, -drainage_area
  ) %>% dplyr::mutate(greenup_day = as.numeric(greenup_day))
  
  for (resp in c("FNConc","FNYield")) {
    message("---- RESPONSE: ", resp, " ----")
    out_dir <- file.path(output_root, sub, resp)
    dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
    
    # build numeric data
    dat <- df %>%
      dplyr::select(dplyr::all_of(c(resp, var_order_present))) %>%
      dplyr::mutate(across(dplyr::everything(), ~ as.numeric(as.character(.)))) %>%
      stats::na.omit()
    if (nrow(dat)==0 || ncol(dat)<2) {
      message("  insufficient data for ", resp); next
    }
    
    # correlation
    M <- cor(dat %>% dplyr::select(-dplyr::all_of(resp)), use="pairwise.complete.obs")
    save_corr(M, out_dir, "drivers")
    
    # RF1 tune and fit
    form <- as.formula(paste(resp, "~ ."))
    mse1 <- test_nt(ntree_vals, form, dat)
    best_nt <- ntree_vals[which.min(mse1)]
    tune <- randomForest::tuneRF(
      x         = dat %>% dplyr::select(-dplyr::all_of(resp)),
      y         = dat[[resp]],
      ntreeTry  = best_nt, stepFactor=1, improve=0.5, plot=FALSE
    )
    best_mtry <- tune[which.min(tune[,2]),1]
    set.seed(666)
    rf1 <- randomForest::randomForest(form, data=dat, ntree=best_nt, mtry=best_mtry, importance=TRUE)
    save_imp(rf1, out_dir, "RF1")
    
    # stability selection
    thr_use <- if (resp=="FNConc") importance_threshold_conc else importance_threshold_yield
    x <- dat %>% dplyr::select(-dplyr::all_of(resp))
    y <- dat[[resp]]
    sel_feats <- rf_stability(x, y, thr_use)
    if (length(sel_feats)==0) {
      message("  no stable features for ", resp); next
    }
    
    # RF2 fit
    form2 <- as.formula(paste(resp, "~", paste(sel_feats, collapse="+")))
    set.seed(666)
    rf2 <- randomForest::randomForest(form2, data=dat, ntree=best_nt, mtry=max(1,floor(sqrt(length(sel_feats)))), importance=TRUE)
    save_imp(rf2, out_dir, "RF2")
    save_pred_obs(rf2, dat[[resp]], out_dir, "RF2")
    
    # save frequencies
    freqs <- rowMeans(replicate(500, {
      idx <- sample(nrow(x), replace=TRUE)
      m <- randomForest::randomForest(x[idx, sel_feats, drop=FALSE], y[idx], ntree=500, mtry=best_mtry, importance=TRUE)
      as.numeric(randomForest::importance(m)[, "%IncMSE"] > thr_use)
    }))
    write.csv(data.frame(Feature=names(freqs), Frequency=freqs),
              file.path(out_dir, "stability_frequencies.csv"), row.names=FALSE)
  }
}
