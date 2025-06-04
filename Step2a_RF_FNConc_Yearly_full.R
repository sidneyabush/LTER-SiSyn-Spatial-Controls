# Load needed packages
librarian::shelf(remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot, mlbench, pROC, tree, dplyr,
                 plot.matrix, reshape2, rcartocolor, arsenal, googledrive, data.table, ggplot2, corrplot, pdp, 
                 iml, tidyr, viridis, parallel, doParallel, foreach)

# Clear environment
rm(list = ls())

# Global seed setting to ensure consistency across the whole workflow
set.seed(666)

# Function to save correlation matrix as PDF
save_correlation_plot <- function(driver_cor, output_dir) {
  png(filename = sprintf("%s/FNConc_Yearly_5yrs_corrplot.png", output_dir), width = 2500, height = 2500, res = 300)
  corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
  title("All Data Yearly FNConc")
  dev.off()
}


# Save RF Variable Importance Plot
save_rf_importance_plot <- function(rf_model, output_dir) {
  png(filename = sprintf("%s/RF_variable_importance_FNConc_Yearly_5_years.png", output_dir), width = 1600, height = 1200, res = 300)
  randomForest::varImpPlot(rf_model, main = "rf_model2 - Yearly FNConc", col = "darkblue")
  dev.off()
}


# Save Linear Model (LM) Plot, but with RMSE instead of MSE
save_lm_plot <- function(rf_model2, observed, output_dir) {
  # calculate RMSE from predicted vs observed
  preds <- rf_model2$predicted
  rmse  <- sqrt(mean((preds - observed)^2))
  rsq   <- mean(rf_model2$rsq)
  
  png(
    filename = sprintf("%s/RF2_lm_plot_FNConc_Yearly_5_years_drivers_df.png", output_dir),
    width    = 1500,
    height   = 1500,
    res      = 300
  )
  
  plot(
    preds, observed,
    pch   = 16,
    cex   = 1.5,
    xlab  = "Predicted",
    ylab  = "Observed",
    main  = "RF Model 2 Full Data Ave FNConc",
    cex.lab  = 1.5,
    cex.axis = 1.5,
    cex.main = 1.5
  )
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2)
  
  legend(
    "topleft",
    bty = "n",
    cex = 1.5,
    legend = paste("R² =", format(rsq, digits = 3))
  )
  legend(
    "bottomright",
    bty = "n",
    cex = 1.5,
    legend = paste("RMSE =", format(rmse, digits = 3))
  )
  
  dev.off()
}



# Parallelized function to test ntree
test_numtree_parallel <- function(ntree_list, formula, data) {
  num_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  MSE <- foreach(ntree = ntree_list, .combine = 'c', .packages = 'randomForest') %dopar% {
    set.seed(666)
    rf_model <- randomForest(formula, data = data, importance = TRUE, proximity = TRUE, ntree = ntree)
    mean(rf_model$mse)  # Return the mean of the MSE vector
  }
  
  stopCluster(cl)
  return(MSE)
}

# Adjusted function for testing different numbers of trees (ntree) using parallel processing
test_numtree_parallel_optimized <- function(ntree_list, formula, data) {
  # Detect the number of available cores
  num_cores <- parallel::detectCores() - 1
  # Initialize parallel cluster
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  # Run models in parallel using foreach
  MSE <- foreach(ntree = ntree_list, .combine = 'c', .packages = 'randomForest') %dopar% {
    set.seed(666)
    rf_model <- randomForest(formula, data = data, importance = TRUE, proximity = TRUE, ntree = ntree)
    mean(rf_model$mse)  # Return the mean MSE for each ntree
  }
  
  # Stop parallel cluster
  stopCluster(cl)
  return(MSE)
}

# RF Stability Selection Function (with OOB MSE tracking)
rf_stability_selection_parallel <- function(x, y, n_bootstrap = 100, threshold = 0.7, 
                                            ntree = 500, mtry = NULL, importance_threshold = 0) {
  feature_scores <- rep(0, ncol(x))
  names(feature_scores) <- colnames(x)
  
  num_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(num_cores)
  registerDoParallel(cl)
  
  # List to store OOB MSE for each bootstrap
  selection_and_mse <- foreach(i = 1:n_bootstrap, .combine = rbind, .packages = 'randomForest') %dopar% {
    set.seed(123 + i)
    boot_indices <- sample(nrow(x), replace = TRUE)
    x_boot <- x[boot_indices, ]
    y_boot <- y[boot_indices]
    rf_model <- randomForest(x_boot, y_boot, ntree = ntree, mtry = mtry, importance = TRUE)
    importance_scores <- importance(rf_model)[, "%IncMSE"]
    selected_features <- as.numeric(importance_scores > importance_threshold)
    c(selected_features, mean(rf_model$mse))
  }
  
  stopCluster(cl)
  
  # Last column is MSE
  selection_matrix <- selection_and_mse[, 1:ncol(x)]
  mse_vec <- selection_and_mse[, ncol(selection_and_mse)]
  
  # Calculate selection frequency
  selection_frequencies <- colMeans(selection_matrix)
  names(selection_frequencies) <- colnames(x)
  sorted_frequencies <- sort(selection_frequencies, decreasing = TRUE)
  stable_features <- names(selection_frequencies[selection_frequencies >= threshold])
  
  cat("\nStability Selection Results:\n")
  cat("Features selected above", threshold, "threshold:", length(stable_features), "\n")
  cat("\nAll features by selection frequency:\n")
  print(sorted_frequencies)
  
  return(list(
    features = stable_features,
    frequencies = selection_frequencies,
    sorted_frequencies = sorted_frequencies,
    mse_vec = mse_vec
  ))
}

# Set output directory
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"

# Define record length (1, 5, 10, 20... years)
record_length <- 5

# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files") 
drivers_df <- read.csv(sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length)) %>%
  dplyr::select(-contains("Yield"), -contains("Gen"), -contains("major"), 
                -Max_Daylength, -Q, -drainage_area) %>%
  mutate(greenup_day = as.numeric(greenup_day)) 

drivers_numeric <- drivers_df %>%
  dplyr::select(-Stream_ID, -Year)

# Plot and save correlation matrix ----
driver_cor <- cor(drivers_numeric[2:29])
save_correlation_plot(driver_cor, output_dir)

# ---- Train Initial RF Model ----
# Test different ntree values for rf_model1
ntree_values <- seq(100, 2000, by = 100)  # Define ntree values
set.seed(666)
MSE_list_rf1 <- test_numtree_parallel(ntree_values, FNConc ~ ., drivers_numeric)

# Visualize MSE results for rf_model1 ----
MSE_df_rf1 <- data.frame(
  ntree = ntree_values,
  mean_MSE = sapply(MSE_list_rf1, mean)
)

p <- ggplot(MSE_df_rf1, aes(ntree, mean_MSE)) + 
  geom_point() + 
  geom_line() + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(100, 2000, 100)) + 
  theme(text = element_text(size = 20))

print(p)

set.seed(666)
# Manually select ntree for rf_model1 ----
manual_ntree_rf1 <- 1200  # Replace with chosen value

set.seed(666)
# Tune mtry for rf_model1 ----
tuneRF(drivers_numeric[, 2:ncol(drivers_numeric)], 
       drivers_numeric[, 1], ntreeTry = manual_ntree_rf1, 
       stepFactor = 1, improve = 0.5, plot = TRUE)

# Manually select mtry for rf_model1 ----
manual_mtry_rf1 <- 9  # Replace with chosen value

# Run initial RF using tuned parameters ----
set.seed(666)
rf_model1 <- randomForest(FNConc ~ ., data = drivers_numeric, 
                          importance = TRUE, proximity = TRUE,
                          ntree = manual_ntree_rf1, mtry = manual_mtry_rf1)

# Visualize output for rf_model1
print(rf_model1)
randomForest::varImpPlot(rf_model1)

# Generate plots comparing predicted vs observed ----
lm_plot <- plot(rf_model1$predicted, drivers_numeric$FNConc, pch = 16, cex = 1.5,
                xlab = "Predicted", ylab = "Observed", main = "RF Model 1 Full Data - Ave FNConc",
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model1$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model1$mse), digits = 3)))

# Extract tuned parameters from rf_model1 for automatic use ----
rf1_ntree <- rf_model1$ntree
rf1_mtry <- rf_model1$mtry

# Start Tuning with Stability Selection and 2nd RFModel ----
# Divide data into predictor variables (x) and response variable (y)
x <- drivers_numeric[, !(colnames(drivers_numeric) == "FNConc")]
y <- drivers_numeric$FNConc

# Calculate 75th percentile importance for threshold
rf_test <- randomForest(x, y, ntree = rf1_ntree, mtry = rf1_mtry, importance = TRUE)
imp_vals <- importance(rf_test)[, "%IncMSE"]
importance_threshold_75th <- as.numeric(quantile(imp_vals[imp_vals > 0], 0.50))

# Run Stability Selection using 50th percentile importance and 80% selection freq ----
set.seed(666)
result_stability <- rf_stability_selection_parallel(
  x = x, 
  y = y, 
  n_bootstrap = 500,           
  threshold = 0.8,            
  ntree = rf1_ntree,           
  mtry = rf1_mtry,            
  importance_threshold = importance_threshold_75th    
)

# Print stability selection results
print(result_stability)

# After you run rf_stability_selection_parallel(), you get:
mse_vec <- result_stability$mse_vec

# Compute RMSE:
rmse_vec <- sqrt(mse_vec)

# Then plot RMSE instead of MSE:
mse_df <- data.frame(
  Bootstrap = seq_along(rmse_vec),
  OOB_RMSE  = rmse_vec
)

oob_rmse_plot <- ggplot(mse_df, aes(x = Bootstrap, y = OOB_RMSE)) +
  geom_line(color = "#3399CC") +
  geom_point(size = 1.1, color = "#3399CC", alpha = 0.7) +
  theme_classic(base_size = 16) +
  labs(
    title = "OOB RMSE Across Bootstraps (RF Stability Selection)",
    x = "Bootstrap Iteration",
    y = "Out-of-Bag RMSE"
  )

ggsave(
  filename = sprintf("%s/FNConc_OOB_RMSE_Stability_Selection.png", output_dir),
  plot     = oob_rmse_plot,
  width    = 8,
  height   = 6,
  dpi      = 300
)


# Put selected features into variable
new_rf_input <- paste(result_stability$features, collapse = "+")

# Format those features into a formula for the optimized random forest model
rf_formula <- formula(paste("FNConc ~", new_rf_input))

# Test different ntree values 
ntree_values <- seq(100, 2000, by = 100)  
set.seed(666)
MSE_list_parallel <- test_numtree_parallel_optimized(ntree_values, rf_formula, drivers_numeric)

# Create a data frame for visualization
MSE_df_parallel <- data.frame(
  ntree = ntree_values,
  mean_MSE = MSE_list_parallel
)

# Visualize the MSE results
ggplot(MSE_df_parallel, aes(x = ntree, y = mean_MSE)) + 
  geom_point() + 
  geom_line() + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(100, 2000, 100)) + 
  theme(text = element_text(size = 20))

# Global seed before re-tuning mtry
set.seed(666)
kept_drivers <- drivers_numeric[, colnames(drivers_numeric) %in% result_stability$features]
tuneRF(kept_drivers, drivers_numeric[, 1], ntreeTry = 1000, 
       stepFactor = 1, improve = 0.5, plot = FALSE)

# Run optimized random forest model, with re-tuned ntree and mtry parameters ----
set.seed(666)
rf_model2 <- randomForest(rf_formula, data = drivers_numeric, 
                          importance = TRUE, proximity = TRUE, ntree = 1000, mtry = 4)

# Visualize output for rf_model2
print(rf_model2)
randomForest::varImpPlot(rf_model2)

# Save RF variable importance plot and LM plot for rf_model2
save_rf_importance_plot(rf_model2, output_dir)
save_lm_plot(rf_model2, drivers_numeric$FNConc, output_dir)

write.csv(
  data.frame(Feature=names(result_stability$frequencies), Frequency=result_stability$frequencies),
  file = "FNConc_Yearly_stability_frequencies.csv", row.names = FALSE
)

# Save model and required objects for SHAP analysis
save(rf_model2, file = "FNConc_Yearly_rf_model2.RData")
kept_drivers <- drivers_numeric[, colnames(drivers_numeric) %in% result_stability$features]
save(kept_drivers, file = "FNConc_Yearly_kept_drivers.RData")
save(drivers_df, file = "FNConc_Yearly_stream_ids.RData")
save(drivers_numeric, file = "FNConc_Yearly_numeric.RData")
