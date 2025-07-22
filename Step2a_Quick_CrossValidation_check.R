#!/usr/bin/env Rscript
# 01_threshold_tuning.R
# ──────────────────────────────────────────────────────────────────────────────
# Grid‐search importance & stability thresholds on older70→recent30 split,
# saving the best pair for FNConc and FNYield.
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load packages & set seed
librarian::shelf(dplyr, tidyr, randomForest, foreach, doParallel, tibble, purrr)
set.seed(666)

# 1) Read harmonized data & define predictors
tot_si <- read.csv(
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", 5),
  stringsAsFactors = FALSE
)

var_order <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI","recession_slope",
  grep("^land_|^rocks_", names(tot_si), value = TRUE)
)
var_order_present <- intersect(var_order, names(tot_si))

# zero‐fill land_/rocks_
rl_cols <- grep("^(land_|rocks_)", names(tot_si), value = TRUE)
tot_si <- tot_si %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(., 0)))

# 2) Load train & local‐test sets
df_train   <- read.csv("AllDrivers_cc_older70.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day = as.numeric(greenup_day))

df_test30  <- read.csv("AllDrivers_cc_recent30.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day = as.numeric(greenup_day))

# 3) RF1‐tuning & stability‐selection helpers
tune_rf1 <- function(df, resp, preds,
                     ntree_try = 200,
                     ntree_grid = seq(100, 500, by = 100)) {
  x <- df %>% dplyr::select(all_of(preds))
  y <- df[[resp]]
  tr <- randomForest::tuneRF(x, y,
                             ntreeTry   = ntree_try,
                             stepFactor = 1.5,
                             improve    = 0.01,
                             plot       = FALSE)
  best_mtry <- tr[which.min(tr[,2]), 1]
  mse_vec <- foreach(nt = ntree_grid, .combine = "c", .packages = "randomForest") %dopar% {
    mean(randomForest(x, y, ntree = nt, mtry = best_mtry)$mse)
  }
  best_nt <- ntree_grid[which.min(mse_vec)]
  list(mtry = best_mtry, ntree = best_nt)
}

stability_feats <- function(df, resp, preds,
                            mtry, ntree, thr_imp, thr_freq,
                            n_boot = 100) {
  x <- df %>% dplyr::select(all_of(preds))
  y <- df[[resp]]
  sel_mat <- replicate(n_boot, {
    idx <- sample(nrow(x), replace = TRUE)
    m   <- randomForest(
      x[idx, , drop = FALSE],
      y[idx],
      ntree      = ntree,
      mtry       = mtry,
      importance = TRUE
    )
    as.numeric(randomForest::importance(m)[, "%IncMSE"] > thr_imp)
  })
  freqs <- rowMeans(sel_mat)
  names(freqs[freqs >= thr_freq])
}

# 4) Grid of candidate thresholds
threshold_grid <- expand.grid(
  importance = c(0, 1.5, 3),
  stability  = c(0.5, 0.6, 0.75),
  stringsAsFactors = FALSE
)

run_threshold_search <- function(resp) {
  train_cc   <- df_train   %>% tidyr::drop_na(all_of(c(resp, var_order_present)))
  test_cc    <- df_test30  %>% tidyr::drop_na(all_of(c(resp, var_order_present)))
  rf1_params <- tune_rf1(train_cc, resp, var_order_present)
  
  threshold_grid %>%
    mutate(response = resp) %>%
    mutate(results = pmap(
      list(importance, stability),
      function(imp, freq) {
        feats <- stability_feats(
          train_cc, resp, var_order_present,
          mtry     = rf1_params$mtry,
          ntree    = rf1_params$ntree,
          thr_imp  = imp,
          thr_freq = freq,
          n_boot   = 100
        )
        n_feats <- length(feats)
        if (n_feats < 3) {
          return(tibble(n_feats, R2 = NA_real_, pRMSE = NA_real_))
        }
        df2     <- train_cc   %>% dplyr::select(all_of(c(resp, feats)))
        params2 <- tune_rf1(df2, resp, feats)
        rf2     <- randomForest(as.formula(paste(resp, "~ .")),
                                data       = df2,
                                ntree      = params2$ntree,
                                mtry       = params2$mtry)
        test2   <- test_cc    %>% dplyr::select(all_of(c(resp, feats)))
        preds   <- predict(rf2, test2)
        obs     <- test2[[resp]]
        R2      <- cor(obs, preds, use = "complete.obs")^2
        RMSE    <- sqrt(mean((obs - preds)^2))
        pRMSE   <- 100 * RMSE / mean(obs)
        tibble(n_feats, R2, pRMSE)
      }
    )) %>%
    tidyr::unnest(results) %>%
    arrange(desc(R2), n_feats) %>%
    slice(1) %>%
    dplyr::select(importance, stability, n_feats, R2, pRMSE)
}

# 5) Search for both responses
cores <- parallel::detectCores() - 1
cl    <- parallel::makeCluster(cores); doParallel::registerDoParallel(cl)

best_FNConc  <- run_threshold_search("FNConc")
best_FNYield <- run_threshold_search("FNYield")

stopCluster(cl)

# 6) Save best thresholds
best_thresh <- tibble(
  response   = c("FNConc", "FNYield"),
  importance = c(best_FNConc$importance,  best_FNYield$importance),
  stability  = c(best_FNConc$stability,   best_FNYield$stability)
)

write.csv(best_thresh,
          "best_thresholds.csv",
          row.names = FALSE)

message("Best thresholds saved to best_thresholds.csv:\n",
        paste(capture.output(best_thresh), collapse = "\n"))
