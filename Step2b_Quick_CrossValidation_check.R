# ──────────────────────────────────────────────────────────────────────────────
# FULL RF PIPELINE WITH SHARED THRESHOLDS, pRMSE, FALLBACK & FACETED PLOTS
#  - single importance_threshold for both FNConc & FNYield
#  - normalized RMSE (pRMSE) for unitless comparison
#  - tuneRF → stability → tuneRF per subset & response
#  - FALLBACK to all predictors if no stable features found
#  - collects R², RMSE, pRMSE and plots observed vs predicted in a 3×2 grid
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load required packages & set seed ----------------------------------------
library(dplyr)
library(tidyr)
library(randomForest)
library(parallel)
library(doParallel)
library(foreach)
library(corrplot)
library(ggplot2)
library(tibble)

set.seed(666)

# 1) Read harmonized data & define predictors --------------------------------
record_length <- 5
tot_si <- read.csv(
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv",
          record_length),
  stringsAsFactors = FALSE
)

var_order <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI","recession_slope",
  grep("^land_|^rocks_", names(tot_si), value = TRUE)
)
predictors <- intersect(var_order, names(tot_si))

# zero‑fill land_/rocks_ so you don't lose rows
rl_cols <- grep("^(land_|rocks_)", names(tot_si), value = TRUE)
tot_si   <- tot_si %>% mutate(across(all_of(rl_cols), ~ replace_na(., 0)))

# 2) Set shared importance & stability thresholds -----------------------------
importance_threshold <- 1.5   # %IncMSE bar, same for FNConc & FNYield
stability_threshold  <- 0.6   # must exceed bar in ≥60% of bootstraps

# 3) Prepare storage for metrics & predictions --------------------------------
metrics_list <- list()
preds_list   <- list()

# 4) Helper: tune mtry & ntree for RF1 -----------------------------------------
tune_rf1 <- function(df, resp, preds,
                     ntree_try = 200,
                     ntree_grid = seq(100, 500, by = 100)) {
  x <- df %>% dplyr::select(dplyr::all_of(preds))
  y <- df[[resp]]
  tr <- randomForest::tuneRF(
    x, y,
    ntreeTry   = ntree_try,
    stepFactor = 1.5,
    improve    = 0.01,
    plot       = FALSE
  )
  best_mtry <- tr[which.min(tr[, 2]), 1]
  mse_vec <- foreach(nt = ntree_grid, .combine = "c", .packages = "randomForest") %dopar% {
    mean(randomForest(x, y, ntree = nt, mtry = best_mtry)$mse)
  }
  best_nt <- ntree_grid[which.min(mse_vec)]
  list(mtry = best_mtry, ntree = best_nt)
}

# 5) Helper: stability selection ------------------------------------------------
stability_feats <- function(df, resp, preds,
                            mtry, ntree, thr_imp, thr_freq,
                            n_boot = 100) {
  x <- df %>% dplyr::select(dplyr::all_of(preds))
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

# 6) Start parallel backend ---------------------------------------------------
cores <- parallel::detectCores() - 1
cl    <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

# 7) Loop over subsets & responses --------------------------------------------
subset_names <- c("unseen10", "older70", "recent30")
responses    <- c("FNConc", "FNYield")

for (sub in subset_names) {
  message("==== SUBSET: ", sub, " ====")
  df_sub <- read.csv(
    sprintf("AllDrivers_cc_%s.csv", sub),
    stringsAsFactors = FALSE
  ) %>%
    mutate(across(starts_with("land_"),  ~ replace_na(., 0))) %>%
    mutate(across(starts_with("rocks_"), ~ replace_na(., 0))) %>%
    dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                  -Max_Daylength, -Q, -drainage_area) %>%
    mutate(greenup_day = as.numeric(greenup_day))
  
  for (resp in responses) {
    message(">> RESPONSE: ", resp)
    
    # 7a) Complete cases for this response
    df_cc <- df_sub %>%
      dplyr::select(dplyr::all_of(c(resp, predictors))) %>%
      tidyr::drop_na()
    if (nrow(df_cc) < 10) {
      message("   insufficient data; skipping.")
      next
    }
    
    # 7b) RF1 tuning
    params1 <- tune_rf1(df_cc, resp, predictors)
    
    # 7c) Stability selection
    feats <- stability_feats(
      df_cc, resp, predictors,
      mtry     = params1$mtry,
      ntree    = params1$ntree,
      thr_imp  = importance_threshold,
      thr_freq = stability_threshold,
      n_boot   = 100
    )
    if (length(feats) == 0) {
      message("   no stable features for ", sub, "/", resp, "; falling back to all predictors.")
      feats <- predictors
    }
    
    # 7d) RF2 tuning on selected features
    df2     <- df_cc[, c(resp, feats), drop = FALSE]
    params2 <- tune_rf1(df2, resp, feats)
    
    # 7e) Final RF2 model
    rf2 <- randomForest(
      as.formula(paste(resp, "~ .")),
      data       = df2,
      ntree      = params2$ntree,
      mtry       = params2$mtry,
      importance = TRUE
    )
    
    # 7f) Save Corrplot & VarImp
    out_dir <- file.path("Final_Models", sub, resp)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    png(file.path(out_dir, "corrplot.png"), width = 800, height = 800, res = 150)
    corrplot(
      cor(df_cc[, predictors], use = "pairwise.complete.obs"),
      type = "lower", diag = FALSE, tl.col = "black"
    )
    dev.off()
    png(file.path(out_dir, "RF2_varImp.png"), width = 1000, height = 600, res = 150)
    varImpPlot(rf2, main = paste(sub, resp, "RF2 Variable Importance"))
    dev.off()
    
    # 7g) Collect predictions & metrics (including pRMSE)
    preds <- predict(rf2, df2)
    obs   <- df2[[resp]]
    R2    <- cor(obs, preds, use = "complete.obs")^2
    RMSE  <- sqrt(mean((obs - preds)^2, na.rm = TRUE))
    pRMSE <- 100 * RMSE / mean(obs, na.rm = TRUE)
    
    key <- paste(sub, resp, sep = "_")
    metrics_list[[key]] <- tibble(
      subset   = sub,
      response = resp,
      R2       = R2,
      RMSE     = RMSE,
      pRMSE    = pRMSE
    )
    preds_list[[key]] <- tibble(
      subset    = sub,
      response  = resp,
      observed  = obs,
      predicted = preds
    )
  }
}

# 8) Stop parallel ------------------------------------------------------------
stopCluster(cl)

# 9) Combine & plot outside of loops ------------------------------------------
if (length(preds_list) == 0) {
  stop("No predictions collected. Check thresholds or data availability.")
}

metrics_df <- bind_rows(metrics_list)
preds_df   <- bind_rows(preds_list)

# 9a) Print metrics
print(metrics_df)

# 9b) Faceted observed vs predicted with R² & pRMSE
ggplot() +
  geom_point(
    data = preds_df,
    aes(x = observed, y = predicted),
    alpha = 0.6
  ) +
  geom_abline(
    slope     = 1,
    intercept = 0,
    linetype  = "dashed"
  ) +
  facet_grid(subset ~ response) +
  theme_minimal() +
  labs(x = "Observed", y = "Predicted") +
  geom_text(
    data = metrics_df,
    aes(
      x     = Inf,
      y     = -Inf,
      label = paste0("R²=", round(R2,2),
                     "\npRMSE=", round(pRMSE,1), "%")
    ),
    hjust = 1.1, vjust = -0.1, size = 3,
    inherit.aes = FALSE
  )
