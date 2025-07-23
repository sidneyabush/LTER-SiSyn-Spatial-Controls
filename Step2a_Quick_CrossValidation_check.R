#!/usr/bin/env Rscript
# 01_threshold_tuning_fast_with_median.R
# ──────────────────────────────────────────────────────────────────────────────
# Fast stability‐selection using the *median* positive importance threshold
# (i.e. the 50th percentile of all >0 %IncMSE), and a single stability cutoff.
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load packages & set seed -----------------------------------------------
librarian::shelf(
  dplyr, tidyr, randomForest, foreach, doParallel, tibble, purrr
)
set.seed(666)

# 1) Read harmonized data & define predictors --------------------------------
tot_si <- read.csv(
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", 5),
  stringsAsFactors = FALSE
)
var_order  <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI","recession_slope",
  grep("^land_|^rocks_", names(tot_si), value = TRUE)
)
predictors <- intersect(var_order, names(tot_si))

# 2) Prepare train & test30 sets --------------------------------------------
rl_cols <- grep("^(land_|rocks_)", names(tot_si), value = TRUE)

df_train  <- read.csv("AllDrivers_cc_older70.csv", stringsAsFactors = FALSE) %>%
  mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  dplyr::select(-contains("Gen"), -contains("major"), -Max_Daylength, -Q, -drainage_area) %>%
  mutate(greenup_day = as.numeric(greenup_day))

df_test30 <- read.csv("AllDrivers_cc_recent30.csv", stringsAsFactors = FALSE) %>%
  mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  dplyr::select(-contains("Gen"), -contains("major"), -Max_Daylength, -Q, -drainage_area) %>%
  mutate(greenup_day = as.numeric(greenup_day))

# 3) RF1‐tuning helper -------------------------------------------------------
tune_rf1 <- function(df, resp, preds) {
  x <- df %>% dplyr::select(all_of(preds))
  y <- df[[resp]]
  tr <- randomForest::tuneRF(x, y,
                             ntreeTry   = 200,
                             stepFactor = 1.5,
                             improve    = 0.01,
                             plot       = FALSE)
  best_mtry <- tr[which.min(tr[, 2]), 1]
  # pick ntree by OOB across a small grid
  mse_vec <- foreach(nt = seq(100, 500, by = 100), .combine = "c", .packages="randomForest") %dopar% {
    mean(randomForest(x, y, ntree = nt, mtry = best_mtry)$mse)
  }
  best_nt <- seq(100, 500, by=100)[which.min(mse_vec)]
  list(mtry = best_mtry, ntree = best_nt)
}

# 4) Stability‐selection helper ----------------------------------------------
stability_feats <- function(df, resp, preds,
                            mtry, ntree, thr_imp, thr_freq,
                            n_boot = 100) {
  x <- df %>% dplyr::select(all_of(preds))
  y <- df[[resp]]
  sel_mat <- replicate(n_boot, {
    idx <- sample(nrow(x), replace = TRUE)
    m   <- randomForest(
      x[idx, , drop = FALSE], y[idx],
      ntree      = ntree,
      mtry       = mtry,
      importance = TRUE
    )
    # count a feature if its IncMSE ≥ thr_imp
    as.numeric(randomForest::importance(m)[, "%IncMSE"] >= thr_imp)
  })
  freqs <- rowMeans(sel_mat)
  names(freqs[freqs >= thr_freq])
}

# 5) Cache RF1 tuning on the training set ------------------------------------
cores <- parallel::detectCores() - 1
cl    <- makeCluster(cores); registerDoParallel(cl)

rf1_params <- list(
  FNConc  = tune_rf1(df_train, "FNConc",  predictors),
  FNYield = tune_rf1(df_train, "FNYield", predictors)
)

# 6) Compute the *median‐importance* threshold per response -------------------
importance_thresholds <- map_dfr(
  c("FNConc","FNYield"),
  function(resp) {
    x <- df_train %>% dplyr::select(all_of(predictors))
    y <- df_train[[resp]]
    rf_full <- randomForest(x, y,
                            ntree      = rf1_params[[resp]]$ntree,
                            mtry       = rf1_params[[resp]]$mtry,
                            importance = TRUE)
    imp_vals <- randomForest::importance(rf_full)[, "%IncMSE"]
    median_pos <- quantile(imp_vals[imp_vals > 0], probs = 0.50, na.rm = TRUE)
    tibble(response = resp, thr_imp = median_pos)
  }
)

# choose a single stability‐frequency cutoff
stability_threshold <- 0.6

# 7) Run stability‐selection & RF2 fitting & test30 prediction ---------------
metrics_list <- list()
preds_list   <- list()
features_list<- list()

for(resp in c("FNConc","FNYield")) {
  message("** ",resp," **")
  params  <- rf1_params[[resp]]
  thr_imp <- importance_thresholds$thr_imp[importance_thresholds$response == resp]
  
  # a) stability selection on older70
  feats <- stability_feats(
    df_train, resp, predictors,
    mtry     = params$mtry,
    ntree    = params$ntree,
    thr_imp  = thr_imp,
    thr_freq = stability_threshold,
    n_boot   = 100
  )
  if (length(feats)==0) {
    message(" No stable features – falling back to all predictors")
    feats <- predictors
  }
  features_list[[resp]] <- feats
  
  # b) fit RF2 on just those features
  df2 <- df_train %>% dplyr::select(all_of(c(resp, feats)))
  rf2 <- randomForest(
    as.formula(paste(resp, "~ .")),
    data       = df2,
    ntree      = params$ntree,
    mtry       = max(1, floor(sqrt(length(feats)))),
    importance = TRUE
  )
  
  # c) predict on recent30
  test2 <- df_test30 %>%
    drop_na(all_of(c(resp, feats))) %>%
    dplyr::select(all_of(c(resp, feats)))
  preds <- predict(rf2, test2)
  obs   <- test2[[resp]]
  metrics_list[[resp]] <- tibble(
    subset   = "recent30",
    response = resp,
    R2       = cor(obs, preds, use="complete.obs")^2,
    RMSE     = sqrt(mean((obs - preds)^2)),
    pRMSE    = 100*RMSE/mean(obs)
  )
  preds_list[[resp]] <- tibble(
    subset    = "recent30",
    response  = resp,
    observed  = obs,
    predicted = preds
  )
}

stopCluster(cl)

# 8) Gather & save -----------------------------------------------------------
metrics_df <- bind_rows(metrics_list)
preds_df   <- bind_rows(preds_list)

print(metrics_df)
print(features_list)

write.csv(metrics_df, "metrics_recent30.csv", row.names=FALSE)
write.csv(
  tibble(
    response = names(features_list),
    kept_vars = map_chr(features_list, ~ paste(.x, collapse = "; "))
  ),
  "Retained_Features_By_Response.csv",
  row.names = FALSE
)
