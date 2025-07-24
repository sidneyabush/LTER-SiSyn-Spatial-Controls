# ──────────────────────────────────────────────────────────────────────────────
# TRAIN ON older70, TEST ON recent30 & unseen10
# ──────────────────────────────────────────────────────────────────────────────

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
# 0) thresholds & predictors ---------------------------------------------------
importance_threshold <- 0    # keep all (zero cut)
stability_threshold  <- 0.5  # ≥50% bootstrap freq

# read full harmonized (just to get column names)
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

# helper to zero‐fill
zero_fill <- function(df){
  rl <- grep("^(land_|rocks_)", names(df), value=TRUE)
  df %>% mutate(across(all_of(rl), ~ replace_na(.,0)))
}

# 1) RF1 tuning helper ---------------------------------------------------------
tune_rf1 <- function(df, resp, preds,
                     ntree_try  = 200,
                     ntree_grid = seq(100,500,100)){
  x <- df %>% dplyr::select(all_of(preds))
  y <- df[[resp]]
  tr <- randomForest::tuneRF(x,y,
                             ntreeTry   = ntree_try,
                             stepFactor = 1.5,
                             improve    = 0.01,
                             plot       = FALSE
  )
  best_mtry <- tr[which.min(tr[,2]),1]
  mse_vec <- foreach(nt=ntree_grid, .combine='c', .packages='randomForest') %dopar% {
    mean(randomForest(x,y,ntree=nt,mtry=best_mtry)$mse)
  }
  best_nt <- ntree_grid[which.min(mse_vec)]
  list(mtry=best_mtry, ntree=best_nt)
}

# 2) stability‐selection helper ------------------------------------------------
stability_feats <- function(df, resp, preds,
                            mtry, ntree,
                            thr_imp, thr_freq,
                            n_boot = 100){
  x <- df %>% dplyr::select(all_of(preds))
  y <- df[[resp]]
  sel_mat <- replicate(n_boot, {
    idx <- sample(nrow(x),replace=TRUE)
    m   <- randomForest(x[idx,], y[idx],
                        ntree      = ntree,
                        mtry       = mtry,
                        importance = TRUE)
    as.numeric(randomForest::importance(m)[, "%IncMSE"] > thr_imp)
  })
  freqs <- rowMeans(sel_mat)
  names(freqs[freqs >= thr_freq])
}

# 3) set up parallel -----------------------------------------------------------
cores <- detectCores()-1
cl    <- makeCluster(cores); registerDoParallel(cl)

# 4) read & prep older70 (the single training set) ----------------------------
train70 <- read.csv("AllDrivers_cc_older70.csv", stringsAsFactors=FALSE) %>%
  zero_fill() %>%
  dplyr::select(-contains("Gen"), -contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  mutate(greenup_day = as.numeric(greenup_day))

metrics_list <- list()
preds_list   <- list()

for(resp in c("FNConc","FNYield")){
  
  message(">>> TRAINING on older70 for ", resp)
  
  # 4a) complete‐cases for training
  train_cc <- train70 %>%
    dplyr::select(all_of(c(resp, predictors))) %>%
    tidyr::drop_na()
  if(nrow(train_cc)<10){
    stop("Not enough training data for ",resp)
  }
  
  # 4b) RF1 tuning on older70
  params1 <- tune_rf1(train_cc, resp, predictors)
  
  # 4c) stability‐select on older70
  feats <- stability_feats(
    train_cc, resp, predictors,
    mtry        = params1$mtry,
    ntree       = params1$ntree,
    thr_imp     = importance_threshold,
    thr_freq    = stability_threshold,
    n_boot      = 200
  )
  if(length(feats)==0){
    message("  → no stable features, falling back to all predictors")
    feats <- predictors
  }
  
  # 4d) RF2 tuning (re‐use tune_rf1) on older70 with only `feats`
  train2 <- train_cc[, c(resp, feats), drop=FALSE]
  params2 <- tune_rf1(train2, resp, feats)
  
  # 4e) final RF2 fit on older70
  rf2 <- randomForest(
    as.formula(paste(resp,"~ .")),
    data       = train2,
    ntree      = params2$ntree,
    mtry       = params2$mtry,
    importance = TRUE
  )
  
  # 4f) store TRAINING‐set OOB metrics & varImp
  oob_R2  <- tail(rf2$rsq,1)
  oob_RMSE<- sqrt(tail(rf2$mse,1))
  metrics_list[[paste0("older70_",resp)]] <- tibble(
    subset   = "older70",
    response = resp,
    R2       = oob_R2,
    RMSE     = oob_RMSE,
    pRMSE    = 100*oob_RMSE/mean(train2[[resp]]),
    n        = nrow(train2)
  )
  preds_list[[paste0("older70_",resp)]] <- tibble(
    subset    = "older70",
    response  = resp,
    observed  = train2[[resp]],
    predicted = predict(rf2, train2)
  )
  
  # 4g) now APPLY to each test set
  for(sub in c("recent30","unseen10")){
    message("    → TESTING on ",sub)
    test_df <- read.csv(sprintf("AllDrivers_cc_%s.csv",sub), stringsAsFactors=FALSE) %>%
      zero_fill() %>%
      dplyr::select(-contains("Gen"), -contains("major"),
                    -Max_Daylength, -Q, -drainage_area) %>%
      mutate(greenup_day = as.numeric(greenup_day))
    
    test_cc <- test_df %>%
      dplyr::select(all_of(c(resp, feats))) %>%
      tidyr::drop_na()
    if(nrow(test_cc)<5){
      message("       insufficient test cases for ",sub,"/",resp); next
    }
    
    preds <- predict(rf2, test_cc)
    obs   <- test_cc[[resp]]
    R2_t  <- cor(obs,preds)^2
    RMSE_t<- sqrt(mean((obs-preds)^2))
    
    metrics_list[[paste0(sub,"_",resp)]] <- tibble(
      subset   = sub,
      response = resp,
      R2       = R2_t,
      RMSE     = RMSE_t,
      pRMSE    = 100*RMSE_t/mean(obs),
      n        = nrow(test_cc)
    )
    preds_list[[paste0(sub,"_",resp)]] <- tibble(
      subset    = sub,
      response  = resp,
      observed  = obs,
      predicted = preds
    )
  }
}

stopCluster(cl)

# 5) combine and plot ---------------------------------------------------------
metrics_df <- bind_rows(metrics_list)
preds_df   <- bind_rows(preds_list)

# a) print metrics
print(metrics_df)

# b) build the plot and save it
p_obs_pred <- ggplot(preds_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ subset + response, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(x = "Observed", y = "Predicted") +
  geom_text(
    data = metrics_df,
    aes(
      x     = Inf,
      y     = -Inf,
      label = sprintf("R²=%.3f\nn=%d", R2, n)
    ),
    inherit.aes = FALSE,
    hjust = 1.1,
    vjust = -0.1,
    size = 3
  )

print(p_obs_pred)

# c) save to disk
ggsave(
  filename = "Final_Models/obs_vs_predicted.png",
  plot     = p_obs_pred,
  width    = 12,
  height   = 8,
  units    = "in",
  dpi      = 300
)
