# Load needed packages
librarian::shelf(
  remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot,
  mlbench, pROC, tree, dplyr, plot.matrix, reshape2, rcartocolor,
  arsenal, googledrive, data.table, ggplot2, corrplot, pdp,
  iml, tidyr, viridis, parallel, doParallel, foreach
)

# Clear environment
rm(list = ls())

# Global seed setting to ensure consistency
set.seed(666)

# ─── Utility Functions ───────────────────────────────────────────────────────

save_correlation_plot <- function(driver_cor, output_dir) {
  png(
    filename = file.path(output_dir, "FNConc_Train_5yrs_corrplot.png"),
    width    = 2500, height = 2500, res = 300
  )
  corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
  title("Training Data Yearly FNConc Correlations")
  dev.off()
}

save_rf_importance_plot <- function(rf_model, output_dir, prefix = "RF") {
  png(
    filename = file.path(output_dir, sprintf("%s_variable_importance_Train.png", prefix)),
    width    = 1600, height = 1200, res = 300
  )
  randomForest::varImpPlot(rf_model, main = paste0(prefix, " - Yearly FNConc (Train)"), col = "darkblue")
  dev.off()
}

save_lm_plot <- function(preds, observed, output_dir, prefix = "RF") {
  rmse <- sqrt(mean((preds - observed)^2))
  rsq  <- cor(preds, observed)^2
  
  png(
    filename = file.path(output_dir, sprintf("%s_lm_plot_Train.png", prefix)),
    width    = 1500, height = 1500, res = 300
  )
  plot(
    preds, observed,
    pch   = 16, cex = 1.5,
    xlab  = "Predicted", ylab = "Observed",
    main  = paste0(prefix, " Model - Train"),
    cex.lab  = 1.5, cex.axis = 1.5, cex.main = 1.5
  )
  abline(0, 1, col = "#6699CC", lwd = 3, lty = 2)
  legend("topleft",  bty = "n", cex = 1.5, legend = paste0("R² = ", format(rsq, digits = 3)))
  legend("bottomright", bty = "n", cex = 1.5, legend = paste0("RMSE = ", format(rmse, digits = 3)))
  dev.off()
}

# ─── Parallel ntree tester ───────────────────────────────────────────────────

test_numtree_parallel <- function(ntree_list, formula, data) {
  num_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(num_cores)
  registerDoParallel(cl)
  
  MSE <- foreach(ntree = ntree_list, .combine = 'c', .packages = 'randomForest') %dopar% {
    set.seed(666)
    rf_model <- randomForest(formula, data = data, importance = TRUE, proximity = TRUE, ntree = ntree)
    mean(rf_model$mse)
  }
  
  stopCluster(cl)
  MSE
}

# ─── RF Stability Selection ──────────────────────────────────────────────────

rf_stability_selection_parallel <- function(x, y, n_bootstrap = 100, threshold = 0.7,
                                            ntree = 500, mtry = NULL, importance_threshold = 0) {
  feature_scores <- rep(0, ncol(x))
  names(feature_scores) <- colnames(x)
  
  num_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(num_cores)
  registerDoParallel(cl)
  
  selection_and_mse <- foreach(i = 1:n_bootstrap, .combine = rbind, .packages = 'randomForest') %dopar% {
    set.seed(123 + i)
    boot_i <- sample(nrow(x), replace = TRUE)
    rf_i  <- randomForest(x[boot_i, ], y[boot_i], ntree = ntree, mtry = mtry, importance = TRUE)
    imp   <- importance(rf_i)[, "%IncMSE"]
    sel   <- as.numeric(imp > importance_threshold)
    c(sel, mean(rf_i$mse))
  }
  
  stopCluster(cl)
  
  sel_mat   <- selection_and_mse[, 1:ncol(x)]
  mse_vec   <- selection_and_mse[, ncol(selection_and_mse)]
  freqs     <- colMeans(sel_mat)
  stable    <- names(freqs[freqs >= threshold])
  
  list(
    features          = stable,
    frequencies       = freqs,
    sorted_frequencies = sort(freqs, decreasing = TRUE),
    mse_vec           = mse_vec
  )
}

# ─── Setup output directory ──────────────────────────────────────────────────

output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ─── Read & tidy data ────────────────────────────────────────────────────────

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")
drivers_df <- read.csv(sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", 5)) %>%
  dplyr::select(-contains("Yield"), -contains("Gen"), -contains("major"),
                -Max_Daylength, -Q, -drainage_area) %>%
  mutate(greenup_day = as.numeric(greenup_day))

drivers_numeric <- drivers_df %>% select(-Stream_ID, -Year)

# ─── Split into Train/Test ───────────────────────────────────────────────────

set.seed(666)
train_idx  <- caret::createDataPartition(drivers_numeric$FNConc, p = 0.75, list = FALSE)
train_data <- drivers_numeric[train_idx, ]
test_data  <- drivers_numeric[-train_idx, ]

# ─── Correlation plot (Train only) ───────────────────────────────────────────

cor_train <- cor(train_data[, -1])
save_correlation_plot(cor_train, output_dir)

# ─── Initial RF on Train ─────────────────────────────────────────────────────

ntree_values <- seq(100, 2000, by = 100)
MSE_rf1 <- test_numtree_parallel(ntree_values, FNConc ~ ., train_data)

# visualize MSE vs ntree if desired:
MSE_df1 <- data.frame(ntree = ntree_values, mean_MSE = MSE_rf1)
ggplot(MSE_df1, aes(ntree, mean_MSE)) +
  geom_point() + geom_line() + theme_classic() +
  scale_x_continuous(breaks = ntree_values) + theme(text = element_text(size = 20))

# manual picks
manual_ntree_rf1 <- 1200
tuneRF(
  x           = train_data[, -1],
  y           = train_data$FNConc,
  ntreeTry    = manual_ntree_rf1,
  stepFactor  = 1,
  improve     = 0.5,
  plot        = TRUE
)
manual_mtry_rf1 <- 9

set.seed(666)
rf_model1 <- randomForest(
  FNConc ~ ., data = train_data,
  importance = TRUE, proximity = TRUE,
  ntree      = manual_ntree_rf1,
  mtry       = manual_mtry_rf1
)

# Save importance + LM (Train)
save_rf_importance_plot(rf_model1, output_dir, prefix = "RF1")
save_lm_plot(rf_model1$predicted, train_data$FNConc, output_dir, prefix = "RF1")

# ─── Evaluate RF1 on Test ─────────────────────────────────────────────────────

pred_test1 <- predict(rf_model1, newdata = test_data)
obs_test1  <- test_data$FNConc
rmse_t1    <- sqrt(mean((pred_test1 - obs_test1)^2))
rsq_t1     <- cor(pred_test1, obs_test1)^2

png(
  filename = file.path(output_dir, "RF1_lm_plot_Test.png"),
  width    = 1500, height = 1500, res = 300
)
plot(
  pred_test1, obs_test1, pch = 16, cex = 1.5,
  xlab = "Predicted", ylab = "Observed",
  main = "RF1 Model - Test",
  cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5
)
abline(0, 1, col = "#6699CC", lwd = 3, lty = 2)
legend("topleft",    bty = "n", cex = 1.5, legend = paste0("R² = ", format(rsq_t1, digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste0("RMSE = ", format(rmse_t1, digits = 3)))
dev.off()

# ─── Stability Selection (Train only) ────────────────────────────────────────

x_train <- train_data[, -1]
y_train <- train_data$FNConc

rf0     <- randomForest(x_train, y_train, ntree = rf_model1$ntree, mtry = rf_model1$mtry, importance = TRUE)
imp0    <- importance(rf0)[, "%IncMSE"]
thr_75  <- quantile(imp0[imp0 > 0], 0.5)

set.seed(666)
stab_res <- rf_stability_selection_parallel(
  x                   = x_train,
  y                   = y_train,
  n_bootstrap         = 500,
  threshold           = 0.8,
  ntree               = rf_model1$ntree,
  mtry                = rf_model1$mtry,
  importance_threshold = thr_75
)

# ─── Optimized RF2 on Train (selected features) ─────────────────────────────

feat_list <- stab_res$features
rf2_formula <- as.formula(paste("FNConc ~", paste(feat_list, collapse = "+")))

# re-tune mtry on the reduced set
set.seed(666)
tuneRF(
  x         = train_data[, feat_list],
  y         = train_data$FNConc,
  ntreeTry  = 1000,
  stepFactor= 1,
  improve   = 0.5,
  plot      = FALSE
)

set.seed(666)
rf_model2 <- randomForest(
  rf2_formula, data = train_data,
  importance = TRUE, proximity = TRUE,
  ntree      = 1000,  mtry = 4
)

# Save importance + LM (Train)
save_rf_importance_plot(rf_model2, output_dir, prefix = "RF2")
save_lm_plot(rf_model2$predicted, train_data$FNConc, output_dir, prefix = "RF2")

# ─── Evaluate RF2 on Test ─────────────────────────────────────────────────────

pred_test2 <- predict(rf_model2, newdata = test_data)
obs_test2  <- test_data$FNConc
rmse_t2    <- sqrt(mean((pred_test2 - obs_test2)^2))
rsq_t2     <- cor(pred_test2, obs_test2)^2

png(
  filename = file.path(output_dir, "RF2_lm_plot_Test.png"),
  width    = 1500, height = 1500, res = 300
)
plot(
  pred_test2, obs_test2, pch = 16, cex = 1.5,
  xlab = "Predicted", ylab = "Observed",
  main = "RF2 Model (Stability-Selected) - Test",
  cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5
)
abline(0, 1, col = "#6699CC", lwd = 3, lty = 2)
legend("topleft",    bty = "n", cex = 1.5, legend = paste0("R² = ", format(rsq_t2, digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste0("RMSE = ", format(rmse_t2, digits = 3)))
dev.off()

# ─── Save final objects ───────────────────────────────────────────────────────

write.csv(
  data.frame(Feature = names(stab_res$frequencies), Frequency = stab_res$frequencies),
  file = file.path(output_dir, "FNConc_Train_stability_frequencies.csv"),
  row.names = FALSE
)

save(rf_model1, rf_model2, train_data, test_data, stab_res,
     file = file.path(output_dir, "FNConc_TrainTest_Models.RData"))
