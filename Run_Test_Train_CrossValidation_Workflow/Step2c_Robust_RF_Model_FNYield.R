#!/usr/bin/env Rscript
# 02_rf_FNYield.R
# Train RF for FNYield: older70 → RF1 → stability → RF2 → predict on recent30 & unseen10

# 0) Load packages & clear
librarian::shelf(
  randomForest, dplyr, tidyr, purrr, tibble,
  parallel, doParallel, foreach, corrplot, ggplot2
)
rm(list=ls()); set.seed(666)

# 1) Paths
drv_dir    <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files"
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"
setwd(drv_dir)
dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)

# 2) Utility plots
save_corr <- function(mat, name) {
  png(file.path(output_dir, paste0(name, "_corrplot.png")), 2500, 2500, res=300)
  corrplot(mat, type="lower", tl.col="black", diag=FALSE)
  title(name)
  dev.off()
}

save_varimp <- function(mod, name) {
  png(file.path(output_dir, paste0(name, "_varImp.png")), 1600, 1200, res=300)
  randomForest::varImpPlot(mod, main=name)
  dev.off()
}

save_predobs <- function(pred, obs, name) {
  png(file.path(output_dir, paste0(name, "_pred_vs_obs.png")), 1500, 1500, res=300)
  plot(pred, obs, pch=16, cex=1.5, main=name, xlab="Predicted", ylab="Observed")
  abline(0, 1, lty=2)
  dev.off()
}

# 3) ntree scan via foreach + randomForest
test_numtree_parallel <- function(tree_grid, x, y) {
  cores <- parallel::detectCores() - 1
  cl    <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  mse <- foreach(nt = tree_grid, .combine='c', .packages='randomForest') %dopar% {
    set.seed(666)
    rf <- randomForest::randomForest(x = x, y = y, ntree = nt, importance = TRUE)
    mean(rf$mse)
  }
  parallel::stopCluster(cl)
  mse
}

# 4) stability‑selection via foreach
auto_stability <- function(x, y, ntree, mtry, imp_thr, freq_thr, n_boot = 500) {
  cores <- parallel::detectCores() - 1
  cl    <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  sel_mat <- foreach(i = 1:n_boot, .combine=rbind, .packages='randomForest') %dopar% {
    set.seed(123 + i)
    idx <- sample(nrow(x), replace=TRUE)
    rf  <- randomForest::randomForest(x[idx,], y[idx], ntree=ntree, mtry=mtry, importance=TRUE)
    imps <- randomForest::importance(rf)[, "%IncMSE"]
    c(as.integer(imps > imp_thr), mean(rf$mse))
  }
  parallel::stopCluster(cl)
  freqs <- colMeans(sel_mat[, 1:ncol(x)])
  feats <- names(freqs[freqs >= freq_thr])
  if (length(feats) < 5) feats <- names(sort(freqs, decreasing=TRUE))[1:5]
  list(features = feats, frequencies = freqs)
}

# 5) Read data & splits
rec_len <- 5
drv_all <- read.csv(sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", rec_len))
vars <- c(
  "NOx", "P", "npp", "evapotrans", "greenup_day", "precip", "temp",
  "snow_cover", "permafrost", "elevation", "basin_slope", "RBI", "recession_slope",
  grep("^land_|^rocks_", names(drv_all), value=TRUE)
)

predictors <- intersect(vars, names(drv_all))
rl_cols <- grep("^(land_|rocks_)", names(drv_all), value=TRUE)

load_split <- function(path) {
  read.csv(path) %>%
    mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
    select(-contains("Gen"), -contains("major"), -Max_Daylength, -Q, -drainage_area) %>%
    mutate(greenup_day = as.numeric(greenup_day))
}

df_train    <- load_split("AllDrivers_cc_older70.csv")
df_recent30 <- load_split("AllDrivers_cc_recent30.csv")
df_unseen10 <- load_split("AllDrivers_cc_unseen10.csv")

# 6) Core pipeline for FNYield
resp <- "FNYield"
message("Processing ", resp)

tree_grid    <- seq(100, 2000, 100)
results_met  <- list()
results_pred <- list()
results_feats<- list()

df_tr <- df_train %>% drop_na(all_of(c(resp, predictors)))
x     <- df_tr[predictors]
y     <- df_tr[[resp]]

# RF1 tuning
mse1     <- test_numtree_parallel(tree_grid, x, y)
nt1      <- tree_grid[which.min(mse1)]
t1       <- randomForest::tuneRF(x, y, ntreeTry=nt1, stepFactor=1.5, improve=0.01, plot=FALSE)
mtry1    <- t1[which.min(t1[,2]), 1]
rf1      <- randomForest::randomForest(x, y, ntree=nt1, mtry=mtry1, importance=TRUE)

# Stability selection
imp_thr  <- quantile(randomForest::importance(rf1)[, "%IncMSE"], 0.50)
freq_thr <- 0.80
stab     <- auto_stability(x, y, nt1, mtry1, imp_thr, freq_thr)
feats    <- stab$features
results_feats[[resp]] <- feats

# RF2 tuning
df2     <- df_tr[, c(resp, feats)]
mse2    <- test_numtree_parallel(tree_grid, df2[feats], df2[[resp]])
nt2     <- tree_grid[which.min(mse2)]
t2      <- randomForest::tuneRF(df2[feats], df2[[resp]], ntreeTry=nt2, stepFactor=1.5, improve=0.01, plot=FALSE)
mtry2   <- t2[which.min(t2[,2]), 1]
rf2     <- randomForest::randomForest(x=df2[feats], y=df2[[resp]], ntree=nt2, mtry=mtry2, importance=TRUE)

save(rf2, file=file.path(output_dir, paste0(resp, "_RF2.RData")))

# Evaluate on recent30 & unseen10
for (sub in c("recent30", "unseen10")) {
  df_te <- get(paste0("df_", sub)) %>% drop_na(all_of(c(resp, feats)))
  if (nrow(df_te) < 5) next
  pr <- predict(rf2, df_te)
  ob <- df_te[[resp]]
  key <- paste(sub, resp, sep = "_")
  results_met[[key]] <- tibble(
    subset   = sub,
    response = resp,
    R2       = cor(ob, pr)^2,
    RMSE     = sqrt(mean((ob - pr)^2)),
    pRMSE    = 100 * RMSE / mean(ob)
  )
  results_pred[[key]] <- tibble(
    subset    = sub,
    response  = resp,
    observed  = ob,
    predicted = pr
  )
}

# Diagnostics
save_corr(cor(x), paste0(resp, "_older70"))
save_varimp(rf2, paste0(resp, "_RF2"))
save_predobs(predict(rf2, df_tr), y, paste0(resp, "_train"))

# Save outputs
tibble(response = resp, kept_vars = paste(feats, collapse = "; ")) %>%
  write.csv(file.path(output_dir, paste0("Retained_Variables_", resp, ".csv")), row.names=FALSE)

bind_rows(results_met) %>%
  write.csv(file.path(output_dir, paste0("Metrics_", resp, ".csv")), row.names=FALSE)

bind_rows(results_pred) %>%
  write.csv(file.path(output_dir, paste0("Predictions_", resp, ".csv")), row.names=FALSE)

# Save SHAP-ready RF2 model and input matrix for FNYield
rf_model2       <- rf2
kept_drivers    <- df2[feats]
drivers_numeric <- df2  # Includes response + features for reference

save(rf_model2, kept_drivers, drivers_numeric,
     file = file.path(output_dir, "FNYield_Yearly_rf_model2.RData"))

# Create quick plot to compare each subset: 
# Load predictions
pred_df <- read_csv("Final_Models/Predictions_FNYield.csv")

# Rename subset labels
pred_df <- pred_df %>%
  mutate(subset = recode(subset,
                         "older70"  = "Train",
                         "recent30" = "Test",
                         "unseen10" = "Cross-Validation"))

# Compute R², RMSE, pRMSE per subset
metrics_df <- pred_df %>%
  group_by(subset) %>%
  summarise(
    R2    = round(cor(observed, predicted)^2, 2),
    RMSE  = round(sqrt(mean((observed - predicted)^2, na.rm = TRUE)), 2),
    pRMSE = round(100 * RMSE / mean(observed, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("R² = ", R2, "\nRMSE = ", RMSE, "\n%RMSE = ", pRMSE, "%")
  )

# Create plot with annotations
p <- ggplot(pred_df, aes(x = predicted, y = observed, color = subset)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c("Train" = "#1b9e77", "Test" = "#d95f02", "Cross-Validation" = "#7570b3")
  ) +
  geom_text(
    data = metrics_df,
    aes(x = Inf, y = -Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.1, vjust = -0.3,
    size = 4,
    color = "black"
  ) +
  facet_wrap(~subset) +
  theme_bw() +
  labs(
    title = "Predicted vs Observed: FNYield",
    x = "Predicted",
    y = "Observed",
    color = "Dataset"
  )

# Save the plot
ggsave("Final_Models/Fig_Predicted_vs_Observed_FNYield_annotated.png", p, width = 10, height = 6, dpi = 300)
