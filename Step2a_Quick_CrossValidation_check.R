# ──────────────────────────────────────────────────────────────────────────────
# FULL RF PIPELINE WITH GRID‐SEARCHED THRESHOLDS, FEATURE‐TRACKING
#  Train on older70, apply to recent30 & unseen10, record retained features
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(randomForest)
library(parallel)
library(doParallel)
library(foreach)
library(corrplot)
library(ggplot2)

set.seed(666)

# 1) Read harmonized data & define predictors --------------------------------
record_length <- 5
tot_si <- read.csv(
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv",
          record_length),
  stringsAsFactors = FALSE
)

var_order  <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI","recession_slope",
  grep("^land_|^rocks_", names(tot_si), value = TRUE)
)
predictors <- intersect(var_order, names(tot_si))

# zero‐fill land_/rocks_
rl_cols <- grep("^(land_|rocks_)", names(tot_si), value = TRUE)
tot_si   <- tot_si %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(., 0)))

# 2) Read train & local‐test sets --------------------------------------------
df_older70 <- read.csv("AllDrivers_cc_older70.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(across(starts_with("land_"),  ~ replace_na(., 0))) %>%
  dplyr::mutate(across(starts_with("rocks_"), ~ replace_na(., 0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day = as.numeric(greenup_day))

df_recent30 <- read.csv("AllDrivers_cc_recent30.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(across(starts_with("land_"),  ~ replace_na(., 0))) %>%
  dplyr::mutate(across(starts_with("rocks_"), ~ replace_na(., 0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day = as.numeric(greenup_day))

df_unseen10 <- read.csv("AllDrivers_cc_unseen10.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(across(starts_with("land_"),  ~ replace_na(., 0))) %>%
  dplyr::mutate(across(starts_with("rocks_"), ~ replace_na(., 0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day = as.numeric(greenup_day))

# 3) Define RF1 / stability‐selection helpers --------------------------------
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
  best_mtry <- tr[which.min(tr[, 2]), 1]
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
    m   <- randomForest(x[idx, , drop = FALSE],
                        y[idx],
                        ntree      = ntree,
                        mtry       = mtry,
                        importance = TRUE)
    as.numeric(randomForest::importance(m)[, "%IncMSE"] > thr_imp)
  })
  freqs <- rowMeans(sel_mat)
  names(freqs[freqs >= thr_freq])
}

# 4) Grid‐search thresholds on older70 → evaluate on recent30 -----------------
threshold_grid <- expand.grid(
  importance = c(0, 1.5, 3),
  stability  = c(0.5, 0.6, 0.75),
  stringsAsFactors = FALSE
)

run_threshold_search <- function(resp) {
  train_cc   <- df_older70 %>% tidyr::drop_na(all_of(c(resp, predictors)))
  test_cc    <- df_recent30 %>% tidyr::drop_na(all_of(c(resp, predictors)))
  rf1_params <- tune_rf1(train_cc, resp, predictors)
  
  threshold_grid %>%
    mutate(response = resp) %>%
    mutate(results = pmap(
      list(importance, stability),
      function(imp, freq) {
        feats <- stability_feats(
          train_cc, resp, predictors,
          mtry     = rf1_params$mtry,
          ntree    = rf1_params$ntree,
          thr_imp  = imp,
          thr_freq = freq,
          n_boot   = 100
        )
        n_feats <- length(feats)
        if (n_feats < 3) return(tibble(n_feats, R2 = NA_real_, pRMSE = NA_real_))
        
        # train RF2 & predict
        df2     <- train_cc %>% dplyr::select(all_of(c(resp, feats)))
        params2 <- tune_rf1(df2, resp, feats)
        rf2     <- randomForest(
          as.formula(paste(resp, "~ .")),
          data       = df2,
          ntree      = params2$ntree,
          mtry       = params2$mtry
        )
        test2 <- test_cc %>% dplyr::select(all_of(c(resp, feats)))
        preds <- predict(rf2, test2)
        obs   <- test2[[resp]]
        R2    <- cor(obs, preds, use = "complete.obs")^2
        RMSE  <- sqrt(mean((obs - preds)^2))
        pRMSE <- 100 * RMSE / mean(obs)
        tibble(n_feats, R2, pRMSE)
      }
    )) %>%
    tidyr::unnest(results) %>%
    arrange(desc(R2), n_feats) %>%
    slice(1) %>%
    dplyr::select(importance, stability, n_feats, R2, pRMSE)
}

best_FNConc  <- run_threshold_search("FNConc")
best_FNYield <- run_threshold_search("FNYield")

message("Best thresholds for FNConc:\n", paste(capture.output(best_FNConc), collapse="\n"))
message("Best thresholds for FNYield:\n", paste(capture.output(best_FNYield), collapse="\n"))

# 5) Now train final models on older70 with those thresholds ------------------
cores <- parallel::detectCores() - 1
cl    <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

metrics_list  <- list()
preds_list    <- list()
features_list <- list()

for (resp in c("FNConc","FNYield")) {
  thr_imp  <- if (resp=="FNConc") best_FNConc$importance  else best_FNYield$importance
  thr_freq <- if (resp=="FNConc") best_FNConc$stability   else best_FNYield$stability
  
  # a) complete cases
  train_cc <- df_older70 %>% tidyr::drop_na(all_of(c(resp, predictors)))
  # b) RF1 tuning
  params1 <- tune_rf1(train_cc, resp, predictors)
  # c) stability selection with chosen thr
  feats <- stability_feats(
    train_cc, resp, predictors,
    mtry     = params1$mtry,
    ntree    = params1$ntree,
    thr_imp  = thr_imp,
    thr_freq = thr_freq,
    n_boot   = 200
  )
  if (length(feats)==0) feats <- predictors
  features_list[[resp]] <- feats
  
  # d) RF2 tuning & fit
  df2     <- train_cc %>% dplyr::select(all_of(c(resp, feats)))
  params2 <- tune_rf1(df2, resp, feats)
  rf2     <- randomForest(
    as.formula(paste(resp, "~ .")),
    data       = df2,
    ntree      = params2$ntree,
    mtry       = params2$mtry,
    importance = TRUE
  )
  
  # e) apply to recent30 & unseen10
  for (sub in c("recent30","unseen10")) {
    df_test <- get(paste0("df_", sub)) %>%
      dplyr::select(all_of(c(resp, feats))) %>%
      tidyr::drop_na()
    if (nrow(df_test) < 5) next
    preds <- predict(rf2, df_test)
    obs   <- df_test[[resp]]
    key   <- paste(sub, resp, sep="_")
    metrics_list[[key]] <- tibble(
      subset   = sub,
      response = resp,
      R2       = cor(obs,preds)^2,
      RMSE     = sqrt(mean((obs-preds)^2)),
      pRMSE    = 100*sqrt(mean((obs-preds)^2))/mean(obs)
    )
    preds_list[[key]] <- tibble(
      subset    = sub,
      response  = resp,
      observed  = obs,
      predicted = preds
    )
  }
}

stopCluster(cl)

# 6) Combine & plot -----------------------------------------------------------
metrics_df <- bind_rows(metrics_list)
preds_df   <- bind_rows(preds_list)

print(metrics_df)

ggplot(preds_df, aes(x=observed,y=predicted)) +
  geom_point(alpha=0.6) +
  geom_abline(slope=1,intercept=0,linetype="dashed") +
  facet_wrap(~ subset + response, scales="free", ncol=2) +
  theme_minimal() +
  labs(x="Observed", y="Predicted") +
  geom_text(
    data=metrics_df,
    aes(x=Inf,y=-Inf,
        label=sprintf("R²=%.2f\npRMSE=%.1f%%", R2, pRMSE)),
    inherit.aes=FALSE, hjust=1.1, vjust=-0.1, size=3
  )

# 7) Save retained features --------------------------------------------------
features_df <- tibble(
  response = names(features_list),
  kept_vars = map_chr(features_list, ~ paste(.x, collapse="; "))
)

print(features_df)
write.csv(features_df, "Retained_Variables_Per_Model.csv", row.names=FALSE)
