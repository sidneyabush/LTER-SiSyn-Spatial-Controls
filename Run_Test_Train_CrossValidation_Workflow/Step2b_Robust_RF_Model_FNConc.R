# Train RF for FNConc: older70 (training) → RF1 → stability → RF2 → predict on recent30 (testing) & unseen10 (cross-validation)

# 0) Load packages & clear
librarian::shelf(
  remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot, mlbench,
  pROC, tree, dplyr, plot.matrix, reshape2, rcartocolor, arsenal,
  googledrive, data.table, ggplot2, corrplot, pdp,
  iml, tidyr, viridis, parallel, doParallel, foreach
)
rm(list=ls())
set.seed(666)

# 1) Setup parallel backend once
n_cores <- parallel::detectCores() - 1
cl      <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# 2) Paths
drv_dir    <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files"
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"
setwd(drv_dir)
dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)

# 3) Utility functions
save_correlation_plot <- function(driver_cor, output_dir) {
  png(sprintf("%s/FNConc_Yearly_5yrs_corrplot.png", output_dir), width=2500, height=2500, res=300)
  corrplot(driver_cor, type="lower", pch.col="black", tl.col="black", diag=FALSE)
  title("All Data Yearly FNConc")
  dev.off()
}

save_rf_importance_plot <- function(rf_model, output_dir) {
  png(sprintf("%s/RF_variable_importance_FNConc_Yearly_5_years.png", output_dir), width=1600, height=1200, res=300)
  randomForest::varImpPlot(rf_model, main="rf_model2 - Yearly FNConc", col="darkblue")
  dev.off()
}

save_lm_plot <- function(rf_model2, observed, output_dir) {
  preds <- rf_model2$predicted
  rmse  <- sqrt(mean((preds - observed)^2))
  rsq   <- mean(rf_model2$rsq)
  png(sprintf("%s/RF2_lm_plot_FNConc_Yearly_5_years.png", output_dir), width=1500, height=1500, res=300)
  plot(preds, observed,
       pch=16, cex=1.5, xlab="Predicted", ylab="Observed",
       main="RF Model 2 Full Data Ave FNConc", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
  abline(0,1,col="#6699CC",lwd=3,lty=2)
  legend("topleft", bty="n", cex=1.5, legend=paste("R² =", format(rsq,digits=3)))
  legend("bottomright", bty="n", cex=1.5, legend=paste("RMSE =", format(rmse,digits=3)))
  dev.off()
}

# 4) ntree scan (uses existing cluster)
test_numtree_parallel <- function(ntree_list, formula, data) {
  foreach(nt=ntree_list, .combine='c', .packages='randomForest') %dopar% {
    set.seed(666)
    rf_model <- randomForest(formula, data=data, importance=TRUE, proximity=TRUE, ntree=nt)
    mean(rf_model$mse)
  }
}

# 5) Stability selection (OOB MSE tracking, uses existing cluster)
rf_stability_selection_parallel <- function(x, y, n_bootstrap=500, threshold=0.8,
                                            ntree, mtry, importance_threshold) {
  sel_mse <- foreach(i=1:n_bootstrap, .combine=rbind, .packages='randomForest') %dopar% {
    set.seed(123 + i)
    idx <- sample(nrow(x), replace=TRUE)
    rf_model <- randomForest(x[idx,], y[idx], ntree=ntree, mtry=mtry, importance=TRUE)
    imp_scores <- importance(rf_model)[, "%IncMSE"]
    selected   <- as.integer(imp_scores > importance_threshold)
    c(selected, mean(rf_model$mse))
  }
  sel_mat <- sel_mse[, 1:ncol(x)]
  mse_vec <- sel_mse[, ncol(sel_mse)]
  freqs   <- colMeans(sel_mat)
  names(freqs) <- colnames(x)
  stable_feats <- names(freqs[freqs >= threshold])
  list(features=stable_feats, frequencies=freqs, mse_vec=mse_vec)
}

# 6) Read & split data
rec_len <- 5
drv_all <- read.csv(sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", rec_len))
vars    <- c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
             "snow_cover","permafrost","elevation","basin_slope","RBI","recession_slope",
             grep("^land_|^rocks_", names(drv_all), value=TRUE))
predictors <- intersect(vars, names(drv_all))
rl_cols    <- grep("^(land_|rocks_)", names(drv_all), value=TRUE)
load_split <- function(path) {
  read.csv(path) %>%
    mutate(across(all_of(rl_cols), ~ replace_na(., 0))) %>%
    select(-contains("Gen"), -contains("major"), -Max_Daylength, -Q, -drainage_area) %>%
    mutate(greenup_day = as.numeric(greenup_day))
}
df_train    <- load_split("AllDrivers_cc_older70.csv")
df_recent30 <- load_split("AllDrivers_cc_recent30.csv")
df_unseen10 <- load_split("AllDrivers_cc_unseen10.csv")

# 7) Core pipeline for FNConc
resp <- "FNConc"; message("Processing ", resp)
# tree_grid <- seq(100, 2000, 100) # Robust
tree_grid <- seq(100, 800, 100)    # Testing

# a) RF1 tuning (comment out once run once)
# mse1  <- test_numtree_parallel(tree_grid, FNConc ~ ., drivers_numeric)
# nt1   <- tree_grid[which.min(mse1)]
# t1    <- randomForest::tuneRF(x,y, ntreeTry=nt1, stepFactor=1.5,...)
# mtry1 <- t1[which.min(t1[,2]),1]
# rf1   <- randomForest(x, y, ntree=nt1, mtry=mtry1, importance=TRUE)

# b) Load cached RF1
load(file.path(output_dir, sprintf("%s_RF1.RData", resp)))  # loads rf1, imps

# c) Stability selection
imp_thr  <- quantile(imps, 0.50)
freq_thr <- 0.80
message("Starting stability selection…")
start <- Sys.time()

stab <- rf_stability_selection_parallel(
  x = df_train[predictors], 
  y = df_train[[resp]],
  n_bootstrap = 10,      # Testing
  threshold   = freq_thr,
  ntree       = rf1$ntree, 
  mtry        = rf1$mtry, 
  importance_threshold = imp_thr
)

end <- Sys.time()
message("Stability selection took ", round(end - start, 2), " ", units(end - start))

feats <- stab$features
if (length(feats) < 5) {
  message("Low feature count; falling back to top 5 by frequency.")
  feats <- names(sort(stab$frequencies, decreasing=TRUE))[1:5]
}

# d) RF2 on selected features
message("Starting RF2 tuning…")
start <- Sys.time()

formula2 <- as.formula(paste(resp, "~ ."))
df2 <- df_train %>%
  drop_na(all_of(c(resp, feats))) %>%
  select(all_of(c(resp, feats)))

mse2 <- test_numtree_parallel(tree_grid, formula2, df2)
nt2  <- tree_grid[which.min(mse2)]

t2   <- randomForest::tuneRF(
  x         = df2[feats],
  y         = df2[[resp]],
  ntreeTry  = nt2,
  stepFactor= 1.5,
  improve   = 0.01,
  plot      = FALSE
)
mtry2 <- t2[which.min(t2[,2]), 1]

rf2 <- randomForest(
  x         = df2[feats],
  y         = df2[[resp]],
  ntree     = nt2,
  mtry      = mtry2,
  importance= TRUE
)

end <- Sys.time()
message("RF2 tuning took ", round(end - start, 2), " ", units(end - start))

# e) Save RF2 & drivers for SHAP
save(rf2, file=file.path(output_dir, sprintf("%s_Yearly_rf_model2.RData", resp)))
kept_drivers <- df_recent30 %>% drop_na(all_of(c(resp, feats))) %>% select(all_of(feats))
save(kept_drivers, file=file.path(output_dir, sprintf("%s_Yearly_kept_drivers.RData", resp)))

# f) Diagnostics for RF2
save_rf_importance_plot(rf2, output_dir)
save_lm_plot(rf2, df2[[resp]], output_dir)

# g) Evaluate on recent30 & unseen10
evaluate_sub <- function(df_sub, name) {
  df_sub <- df_sub %>% drop_na(all_of(c(resp, feats)))
  preds  <- predict(rf2, df_sub)
  obs    <- df_sub[[resp]]
  tibble(subset=name,
         R2    = cor(obs,preds)^2,
         RMSE  = sqrt(mean((obs-preds)^2)),
         pRMSE = 100*RMSE/mean(obs))
}
metrics <- bind_rows(
  evaluate_sub(df_recent30, "recent30"),
  evaluate_sub(df_unseen10, "unseen10")
)
write.csv(metrics, file.path(output_dir, sprintf("Metrics_%s.csv", resp)), row.names=FALSE)

# h) Save retained-feature list
tibble(response = resp,
       kept_vars = paste(feats, collapse = "; ")) %>%
  write.csv(file.path(output_dir, paste0("Retained_Variables_", resp, ".csv")),
            row.names = FALSE)

# i) Gather predictions for train, test, and CV
pred_list <- list()

# – Train (older70)
df_tr <- df_train %>% drop_na(all_of(c(resp, feats)))
pred_list[["older70"]] <- tibble(
  subset    = "older70",
  observed  = df_tr[[resp]],
  predicted = predict(rf2, df_tr)
)

# – recent30 & unseen10
for(sub in c("recent30", "unseen10")) {
  df_te <- get(paste0("df_", sub)) %>%
    drop_na(all_of(c(resp, feats)))
  pred_list[[sub]] <- tibble(
    subset    = sub,
    observed  = df_te[[resp]],
    predicted = predict(rf2, df_te)
  )
}

pred_df <- bind_rows(pred_list)
write.csv(pred_df,
          file.path(output_dir, paste0("Predictions_", resp, ".csv")),
          row.names = FALSE)

# j) Quick diagnostics: corr, varImp, pred vs obs
save_correlation_plot(cor(df_train[predictors]), output_dir)
save_rf_importance_plot(rf2, output_dir)
save_lm_plot(rf2, pred_df$observed, output_dir)

# k) Faceted Predicted vs Observed plot
# (ggplot2 is already loaded)
pred_df <- pred_df %>%
  mutate(subset = recode(subset,
                         "older70"  = "Train",
                         "recent30" = "Test",
                         "unseen10" = "Cross-Validation"))

metrics_df <- pred_df %>%
  group_by(subset) %>%
  summarise(
    R2    = round(cor(observed, predicted)^2, 2),
    RMSE  = round(sqrt(mean((observed - predicted)^2)), 2),
    pRMSE = round(100 * RMSE / mean(observed), 1),
    .groups = "drop"
  ) %>%
  mutate(label = paste0("R² = ", R2, "\nRMSE = ", RMSE, "\n%RMSE = ", pRMSE, "%"))

p <- ggplot(pred_df, aes(x = predicted, y = observed, color = subset)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Train" = "#1b9e77",
                                "Test"  = "#d95f02",
                                "Cross-Validation" = "#7570b3")) +
  geom_text(data = metrics_df,
            aes(x = Inf, y = -Inf, label = label),
            inherit.aes = FALSE,
            hjust = 1.1, vjust = -0.3,
            size = 4) +
  facet_wrap(~subset) +
  theme_bw() +
  labs(
    title = paste("Predicted vs Observed:", resp),
    x = "Predicted", y = "Observed", color = "Dataset"
  )

ggsave(
  file.path(output_dir,
            paste0("Fig_Predicted_vs_Observed_", resp, "_annotated.png")),
  p, width = 10, height = 6, dpi = 300
)

# 8) Stop parallel backend
parallel::stopCluster(cl)
