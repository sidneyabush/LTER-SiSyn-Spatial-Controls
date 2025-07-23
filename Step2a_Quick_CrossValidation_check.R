#!/usr/bin/env Rscript
# 01_threshold_tuning_simple.R
# ──────────────────────────────────────────────────────────────────────────────
# Simplified threshold tuning with lower‐quartile importance cutoff and
# enforced top‐5 fallback:
#   • tune RF1 on older70
#   • compute 25th‐percentile importance threshold
#   • compute per‐feature selection freq over 100 bootstraps
#   • keep those ≥30%; if <5, take top‐5 by freq
#   • train RF2 & evaluate on recent30
#   • record fallback flag
# ──────────────────────────────────────────────────────────────────────────────

librarian::shelf(dplyr, tidyr, randomForest, foreach, doParallel, tibble, purrr)
set.seed(666)

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

# 1) Read data & define predictors
tot_si <- read.csv("AllDrivers_Harmonized_Yearly_filtered_5_years_uncleaned.csv", stringsAsFactors=FALSE)
var_order  <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI","recession_slope",
  grep("^land_|^rocks_", names(tot_si), value=TRUE)
)
predictors <- intersect(var_order, names(tot_si))
rl_cols    <- grep("^(land_|rocks_)", names(tot_si), value=TRUE)

# 2) Load splits
df_train  <- read.csv("AllDrivers_cc_older70.csv", stringsAsFactors=FALSE) %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(.,0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day=as.numeric(greenup_day))
df_test30 <- read.csv("AllDrivers_cc_recent30.csv", stringsAsFactors=FALSE) %>%
  dplyr::mutate(across(all_of(rl_cols), ~ replace_na(.,0))) %>%
  dplyr::select(-dplyr::contains("Gen"), -dplyr::contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate(greenup_day=as.numeric(greenup_day))

# 3) Output dir
output_dir <- "Final_Models"
dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)

# 4) Helpers
tune_rf1 <- function(df, resp, preds) {
  x <- df %>% dplyr::select(all_of(preds)); y <- df[[resp]]
  tr <- randomForest::tuneRF(x,y, ntreeTry=200, stepFactor=1.5, improve=0.01, plot=FALSE)
  mtry <- tr[which.min(tr[,"OOBError"]), "mtry"]
  # pick ntree by quick OOB‐MSE scan
  nt_grid <- seq(100,500,100)
  mse_vec <- sapply(nt_grid, function(nt) mean(randomForest(x,y, ntree=nt, mtry=mtry)$mse))
  list(mtry=mtry, ntree=nt_grid[which.min(mse_vec)])
}

selection_freqs <- function(df, resp, preds, mtry, ntree, thr_imp, n_boot=100) {
  x <- df %>% dplyr::select(all_of(preds)); y <- df[[resp]]
  sel <- replicate(n_boot, {
    ii <- sample(nrow(x), replace=TRUE)
    m  <- randomForest(x[ii,], y[ii], ntree=ntree, mtry=mtry, importance=TRUE)
    randomForest::importance(m)[, "%IncMSE"] > thr_imp
  })
  rowMeans(sel)
}

# 5) Parallel RF1 tuning
cores <- parallel::detectCores()-1
cl <- parallel::makeCluster(cores); doParallel::registerDoParallel(cl)
rf1_params <- list(
  FNConc  = tune_rf1(df_train, "FNConc",  predictors),
  FNYield = tune_rf1(df_train, "FNYield", predictors)
)

# 6) Threshold + stability + RF2 + evaluation
features_list <- list(); fallback_list <- list()
metrics_list  <- list(); preds_list <- list()

for(resp in c("FNConc","FNYield")) {
  pars     <- rf1_params[[resp]]
  train_cc <- df_train %>% tidyr::drop_na(all_of(c(resp,predictors)))
  # 6a) RF1 importances
  rf1_full <- randomForest(
    as.formula(paste(resp,"~ .")),
    data = train_cc %>% dplyr::select(all_of(c(resp,predictors))),
    ntree=pars$ntree, mtry=pars$mtry, importance=TRUE
  )
  imp_vals <- randomForest::importance(rf1_full)[,"%IncMSE"]
  thr_imp  <- quantile(imp_vals[imp_vals > 0], 0.10, na.rm = TRUE)
  # 6b) bootstrap freqs
  freqs    <- selection_freqs(train_cc, resp, predictors, pars$mtry, pars$ntree, thr_imp, n_boot=100)
  sel      <- names(freqs[freqs>=0.20])
  # 6c) fallback to top-5 if needed
  if(length(sel)<5) {
    sel <- names(sort(freqs, decreasing=TRUE))[1:5]
    fb  <- TRUE
  } else fb <- FALSE
  
  features_list[[resp]] <- sel
  fallback_list[[resp]]  <- fb
  
  # 6d) RF2 on sel features
  df2 <- train_cc %>% dplyr::select(all_of(c(resp, sel)))
  rf2 <- randomForest(
    as.formula(paste(resp,"~ .")), data=df2,
    ntree=pars$ntree, mtry=max(1,floor(sqrt(length(sel)))), importance=TRUE
  )
  # 6e) evaluate
  test_cc <- df_test30 %>% tidyr::drop_na(all_of(c(resp,sel)))
  pred   <- predict(rf2, test_cc %>% dplyr::select(all_of(sel)))
  obs    <- test_cc[[resp]]
  
  metrics_list[[resp]] <- tibble::tibble(
       subset        = "recent30",
       response      = resp,
       thr_imp       = thr_imp,
       thr_freq      = thr_freq,  # use the variable you set above
       used_fallback = fb,
       R2            = cor(obs, pred)^2,
       RMSE          = sqrt(mean((obs - pred)^2)),
       pRMSE         = 100 * RMSE / mean(obs)
    )
  preds_list[[resp]] <- tibble::tibble(
    subset="recent30", response=resp, observed=obs, predicted=pred
  )
}

stopCluster(cl)

# 7) Save outputs
tibble::tibble(
  response=names(features_list),
  kept_vars=map_chr(features_list, ~paste(.x,collapse=";")),
  used_fallback=unlist(fallback_list)
) %>%
  write.csv(file.path(output_dir,"Retained_Variables_Per_Model.csv"),row.names=FALSE)

dplyr::bind_rows(metrics_list) %>%
  write.csv(file.path(output_dir,"Metrics_Recent30.csv"),row.names=FALSE)

message("✅ Done: features, fallback flags & metrics written to ",output_dir)
