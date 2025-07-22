#!/usr/bin/env Rscript
# 02_final_rf_pipeline.R
# ──────────────────────────────────────────────────────────────────────────────
# Train final RF2 on older70 with fixed thresholds, predict on recent30 & unseen10,
# record retained features per model.
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load packages & set seed
librarian::shelf(
  dplyr, tidyr, purrr, tibble,
  randomForest, parallel, doParallel, foreach,
  corrplot, ggplot2
)
set.seed(666)

# 1) Read best thresholds
best_thresh <- read.csv("best_thresholds.csv", stringsAsFactors = FALSE)

# 2) Read all splits & harmonized to define var_order
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
tot_si <- tot_si %>% dplyr::mutate(across(all_of(rl_cols), ~ replace_na(., 0)))

# 3) Load splits
df_train   <- read.csv("AllDrivers_cc_older70.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day = as.numeric(greenup_day))

df_recent30 <- read.csv("AllDrivers_cc_recent30.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day = as.numeric(greenup_day))

df_unseen10 <- read.csv("AllDrivers_cc_unseen10.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day = as.numeric(greenup_day))

# 4) RF1‐tuning & stability‐selection helpers (same as script 1)
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

# 5) Parallel setup
cores <- parallel::detectCores() - 1
cl    <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

# 6) Train RF2 on older70 & predict on recent30/unseen10
metrics_list  <- list()
preds_list    <- list()
features_list <- list()

for(resp in c("FNConc","FNYield")) {
  # pull thresholds
  thr_imp  <- best_thresh %>% filter(response == resp) %>% pull(importance)
  thr_freq <- best_thresh %>% filter(response == resp) %>% pull(stability)
  
  message("Training final RF2 on older70 for ", resp)
  train_cc <- df_train %>% tidyr::drop_na(all_of(c(resp, var_order_present)))
  
  # RF1 tuning
  params1 <- tune_rf1(train_cc, resp, var_order_present)
  
  # Stability selection
  feats <- stability_feats(
    train_cc, resp, var_order_present,
    mtry     = params1$mtry,
    ntree    = params1$ntree,
    thr_imp  = thr_imp,
    thr_freq = thr_freq,
    n_boot   = 200
  )
  if (length(feats) == 0) feats <- var_order_present
  features_list[[resp]] <- feats
  
  # RF2 tuning & fit
  df2     <- train_cc[, c(resp, feats), drop = FALSE]
  params2 <- tune_rf1(df2, resp, feats)
  rf2     <- randomForest(
    as.formula(paste(resp, "~ .")),
    data       = df2,
    ntree      = params2$ntree,
    mtry       = params2$mtry,
    importance = TRUE
  )
  
  # apply to recent30 & unseen10
  for(sub in c("recent30","unseen10")) {
    df_test <- get(paste0("df_", sub)) %>%
      tidyr::drop_na(all_of(c(resp, feats)))
    if (nrow(df_test) < 5) next
    
    preds <- predict(rf2, df_test)
    obs   <- df_test[[resp]]
    key   <- paste(sub, resp, sep = "_")
    
    metrics_list[[key]] <- tibble(
      subset   = sub,
      response = resp,
      R2       = cor(obs, preds, use = "complete.obs")^2,
      RMSE     = sqrt(mean((obs - preds)^2)),
      pRMSE    = 100 * sqrt(mean((obs - preds)^2)) / mean(obs)
    )
    
    preds_list[[key]] <- tibble(
      subset    = sub,
      response  = resp,
      observed  = obs,
      predicted = preds
    )
  }
}

# 7) Tear down parallel
stopCluster(cl)

# 8) Combine & save outputs
metrics_df  <- bind_rows(metrics_list)
preds_df    <- bind_rows(preds_list)
features_df <- tibble(
  response = names(features_list),
  kept_vars = map_chr(features_list, ~ paste(.x, collapse = "; "))
)

write.csv(metrics_df,  "metrics_final.csv",             row.names = FALSE)
write.csv(preds_df,    "predictions_final.csv",         row.names = FALSE)
write.csv(features_df, "Retained_Variables_Per_Model.csv", row.names = FALSE)

# 9) (Optional) quick plot
ggplot(preds_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ subset + response, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(x = "Observed", y = "Predicted") +
  geom_text(
    data = metrics_df,
    aes(x = Inf, y = -Inf, label = sprintf("R²=%.2f\npRMSE=%.1f%%", R2, pRMSE)),
    inherit.aes = FALSE, hjust = 1.1, vjust = -0.1, size = 3
  )
