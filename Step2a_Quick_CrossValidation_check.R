#!/usr/bin/env Rscript
# 01_threshold_tuning_simple.R
# ──────────────────────────────────────────────────────────────────────────────
# Simplified threshold tuning with median‐based importance cutoff:
#   • tune RF1 on training set
#   • compute median importance threshold from RF1
#   • stability selection on those median thresholds
#   • train RF2 and evaluate on recent30
#   • record if fallback to all predictors was needed
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load packages & set seed -----------------------------------------------
librarian::shelf(
  dplyr, tidyr, randomForest, foreach, doParallel, tibble, purrr
)
set.seed(666)

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files") 


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

# zero‑fill land_/rocks_ -----------------------------------------------------
rl_cols <- grep("^(land_|rocks_)", names(tot_si), value = TRUE)

# 2) Load train & local‑test sets --------------------------------------------
df_train <- read.csv("AllDrivers_cc_older70.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day = as.numeric(greenup_day))

df_test30 <- read.csv("AllDrivers_cc_recent30.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day = as.numeric(greenup_day))

# 3) Prepare output dir ------------------------------------------------------
output_dir <- "Final_Models"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 4) RF1‑tuning & stability‐selection helpers --------------------------------
tune_rf1 <- function(df, resp, preds,
                     ntree_try = 200,
                     ntree_grid = seq(100, 500, by = 100)) {
  x <- df %>% dplyr::select(all_of(preds))
  y <- df[[resp]]
  tr <- randomForest::tuneRF(
    x, y,
    ntreeTry   = ntree_try,
    stepFactor = 1.5,
    improve    = 0.01,
    plot       = FALSE
  )
  best_mtry <- tr[which.min(tr[,2]),1]
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

# 5) Cache RF1 tuning on the training set ------------------------------------
cores <- parallel::detectCores() - 1
cl    <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

rf1_params <- list(
  FNConc  = tune_rf1(df_train, "FNConc",  predictors),
  FNYield = tune_rf1(df_train, "FNYield", predictors)
)

# 6) Run median‐based threshold tuning & stability selection ----------------
features_list <- list()
fallback_list <- list()
metrics_list  <- list()
preds_list    <- list()

for (resp in c("FNConc","FNYield")) {
  params   <- rf1_params[[resp]]
  train_cc <- df_train %>% tidyr::drop_na(all_of(c(resp, predictors)))
  
  # 6a) Train RF1 on the training subset to get importances
  rf1_full <- randomForest(
    as.formula(paste(resp, "~ .")),
    data       = train_cc %>% dplyr::select(all_of(c(resp, predictors))),
    ntree      = params$ntree,
    mtry       = params$mtry,
    importance = TRUE
  )
  imp_vals <- randomForest::importance(rf1_full)[, "%IncMSE"]
  thr_imp <- quantile(imp_vals[imp_vals > 0], 0.25)
  thr_freq <- 0.5   # stability cutoff
  
  # 6b) Stability selection on older70 using median importance
  feats <- stability_feats(
    train_cc, resp, predictors,
    mtry     = params$mtry,
    ntree    = params$ntree,
    thr_imp  = thr_imp,
    thr_freq = thr_freq,
    n_boot   = 100
  )
  
  used_fallback <- FALSE
  if (length(feats) == 0) {
    message("⚠ No stable features for ", resp, "; using all predictors.")
    feats         <- predictors
    used_fallback <- TRUE
  }
  
  features_list[[resp]] <- feats
  fallback_list[[resp]] <- used_fallback
  
  # 6c) train final RF2 on those features
  df2 <- train_cc %>% dplyr::select(all_of(c(resp, feats)))
  rf2 <- randomForest(
    as.formula(paste(resp, "~ .")),
    data       = df2,
    ntree      = params$ntree,
    mtry       = max(1, floor(sqrt(length(feats)))),
    importance = TRUE
  )
  
  # 6d) evaluate on recent30
  test_cc <- df_test30 %>% tidyr::drop_na(all_of(c(resp, feats)))
  preds   <- predict(rf2, test_cc %>% dplyr::select(all_of(feats)))
  obs     <- test_cc[[resp]]
  metrics_list[[resp]] <- tibble::tibble(
    subset         = "recent30",
    response       = resp,
    threshold_imp  = thr_imp,
    threshold_freq = thr_freq,
    R2             = cor(obs, preds)^2,
    RMSE           = sqrt(mean((obs - preds)^2)),
    pRMSE          = 100 * sqrt(mean((obs - preds)^2)) / mean(obs)
  )
  preds_list[[resp]] <- tibble::tibble(
    subset    = "recent30",
    response  = resp,
    observed  = obs,
    predicted = preds
  )
}

stopCluster(cl)

# 7) Save retained features, fallback flags & metrics ------------------------
features_df <- tibble::tibble(
  response      = names(features_list),
  kept_vars     = purrr::map_chr(features_list, ~ paste(.x, collapse = "; ")),
  used_fallback = unlist(fallback_list)
)
write.csv(
  features_df,
  file.path(output_dir, "Retained_Variables_Per_Model.csv"),
  row.names = FALSE
)

metrics_df <- dplyr::bind_rows(metrics_list)
write.csv(
  metrics_df,
  file.path(output_dir, "Metrics_Recent30.csv"),
  row.names = FALSE
)

message("✅ Finished: saved retained features, fallback flags, and recent30 metrics to ", output_dir)
