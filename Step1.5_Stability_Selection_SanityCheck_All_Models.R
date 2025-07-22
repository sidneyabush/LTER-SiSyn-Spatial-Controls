# ──────────────────────────────────────────────────────────────────────────────
# QUICK‐CHECK: Stability‐Selection Sanity Test with Shared Thresholds
# Reports how many (and which) predictors survive a single importance threshold
# ──────────────────────────────────────────────────────────────────────────────

# 0) Load packages & set seed
library(dplyr)
library(tidyr)
library(randomForest)

set.seed(666)

# 1) Read harmonized data & define your predictors
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
var_order_present <- intersect(var_order, names(tot_si))

# 2) Set a single shared importance threshold + stability threshold
importance_threshold <- 1    # %IncMSE bar for both FNConc & FNYield
stability_threshold  <- 0.5  # feature must appear in ≥50% of bootstraps

# 3) Quick‐check function returning feature names
quick_check_features <- function(df, resp, predictors,
                                 thr_imp, thr_freq = stability_threshold,
                                 n_boot = 50, ntree = 100) {
  # zero‑fill any land_/rocks_ columns
  rl_cols <- grep("^(land_|rocks_)", names(df), value = TRUE)
  if (length(rl_cols) > 0) {
    df <- df %>%
      dplyr::mutate(across(dplyr::all_of(rl_cols), ~ replace_na(., 0)))
  }
  
  # build complete‑case numeric data.frame
  dat <- df %>%
    dplyr::select(dplyr::all_of(c(resp, predictors))) %>%
    dplyr::mutate(across(dplyr::everything(), as.numeric)) %>%
    tidyr::drop_na()
  
  if (nrow(dat) < 10) {
    return(character(0))
  }
  
  x <- dat %>% dplyr::select(-dplyr::all_of(resp))
  y <- dat[[resp]]
  
  # bootstrap stability‑selection
  sel_mat <- replicate(n_boot, {
    idx <- sample(nrow(x), replace = TRUE)
    m   <- randomForest(
      x[idx, , drop = FALSE],
      y[idx],
      ntree      = ntree,
      mtry       = floor(sqrt(ncol(x))),
      importance = TRUE
    )
    as.numeric(randomForest::importance(m)[, "%IncMSE"] > thr_imp)
  })
  
  freqs <- rowMeans(sel_mat)
  names(freqs)[freqs >= thr_freq]
}

# 4) Run the quick‐check for each subset & response
subset_names <- c("unseen10", "older70", "recent30")
responses    <- c("FNConc", "FNYield")
input_prefix <- "AllDrivers_cc"

for (sub in subset_names) {
  df <- read.csv(
    sprintf("%s_%s.csv", input_prefix, sub),
    stringsAsFactors = FALSE
  )
  
  for (resp in responses) {
    feats   <- quick_check_features(
      df         = df,
      resp       = resp,
      predictors = var_order_present,
      thr_imp    = importance_threshold,
      thr_freq   = stability_threshold,
      n_boot     = 50,
      ntree      = 100
    )
    n_feats <- length(feats)
    
    if (n_feats > 0) {
      message(
        sub, "/", resp, ": ✔ ", n_feats,
        " features → ", paste(feats, collapse = ", ")
      )
    } else {
      message(sub, "/", resp, ": ✘ none")
    }
  }
}
