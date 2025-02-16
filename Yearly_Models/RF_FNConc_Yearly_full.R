 # Load needed packages
librarian::shelf(remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot, mlbench, pROC, tree, dplyr,
                 plot.matrix, reshape2, rcartocolor, arsenal, googledrive, data.table, ggplot2, corrplot, pdp, 
                 iml, tidyr, viridis, parallel, doParallel, foreach)

# Clear environment
rm(list = ls())

# Global seed setting to ensure consistency across the whole workflow
set.seed(123)

# Load Functions ----
# Function to save correlation matrix as PDF
save_correlation_plot <- function(driver_cor, output_dir) {
  pdf(sprintf("%s/correlation_plot_FNConc_Yearly_5_years.pdf", output_dir), width = 10, height = 10)
  corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
  title("All Data Yearly FNConc")
  dev.off()
}

# Save RF Variable Importance Plot
save_rf_importance_plot <- function(rf_model, output_dir) {
  pdf(sprintf("%s/RF_variable_importance_FNConc_Yearly_5_years.pdf", output_dir), width = 8, height = 6)
  randomForest::varImpPlot(rf_model, main = "rf_model2 - Ave FNConc", col = "darkblue")
  dev.off()
}

# Save Linear Model (LM) Plot
save_lm_plot <- function(rf_model2, observed, output_dir) {
  pdf(sprintf("%s/RF2_lm_plot_FNConc_Yearly_5_years_drivers_df.pdf", output_dir), width = 8, height = 8)
  plot(rf_model2$predicted, observed, pch = 16, cex = 1.5,
       xlab = "Predicted", ylab = "Observed", main = "RF Model 2 Full Data Ave FNConc",
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2)
  legend("topleft", bty = "n", cex = 1.5, legend = paste("R² =", format(mean(rf_model2$rsq), digits = 3)))
  legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))
  dev.off()
}

# Parallelized function to test ntree
test_numtree_parallel <- function(ntree_list, formula, data) {
  num_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  # Collect results
  MSE <- foreach(ntree = ntree_list, .combine = 'c', .packages = 'randomForest') %dopar% {
    set.seed(123)
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
    set.seed(123)
    rf_model <- randomForest(formula, data = data, importance = TRUE, proximity = TRUE, ntree = ntree)
    mean(rf_model$mse)  # Return the mean MSE for each ntree
  }
  
  # Stop parallel cluster
  stopCluster(cl)
  return(MSE)
}

# Set the output directory path for saving PDFs
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

# Define record length (1, 5, 10, 20... years)
record_length <- 5

# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

drivers_df <- read.csv(sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length)) %>%
  dplyr::select(-contains("Yield"), -contains("Gen"), -contains("major"), 
                -Max_Daylength, -silicate_weathering, -Q, -drainage_area) %>%
  mutate(greenup_day = as.numeric(greenup_day)) %>%
  filter(!Stream_ID %in% c("NWT__SADDLE STREAM 007"))  # Remove specific outlier site


drivers_numeric <- drivers_df %>%
  dplyr::select(-Stream_ID, -Year)

# Plot and save correlation matrix ----
# numeric_drivers <- 2:24 # Change this range to reflect data frame length
driver_cor <- cor(drivers_numeric)
save_correlation_plot(driver_cor, output_dir)


# ---- Train Initial RF Model ----
# Test different ntree values for rf_model1
ntree_values <- seq(100, 2000, by = 100)  # Define ntree values
set.seed(123)
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

# Manually select ntree for rf_model1 ----
manual_ntree_rf1 <- 2000  # Replace with chosen value

# Tune mtry for rf_model1 ----
tuneRF(drivers_numeric[, 2:ncol(drivers_numeric)], drivers_numeric[, 1], ntreeTry = manual_ntree_rf1, stepFactor = 1, improve = 0.5, plot = TRUE)

# Manually select mtry for rf_model1 ----
manual_mtry_rf1 <- 7  # Replace with chosen value

# Run initial RF using tuned parameters ----
set.seed(123)
rf_model1 <- randomForest(FNConc ~ ., data = drivers_numeric, importance = TRUE, proximity = TRUE, ntree = manual_ntree_rf1, mtry = manual_mtry_rf1)

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

# Start Tuning with RFE and 2nd RFModel ----
# Global seed for RFE ----
size <- ncol(drivers_numeric) - 1  # This is the number of predictor variables
cv_repeats <- 5
cv_number <- 5
total_repeats <- (cv_repeats * cv_number) + 1

seeds <- vector(mode = "list", length = total_repeats)
for (i in 1:(cv_repeats * cv_number)) {
  seeds[[i]] <- rep(123, size)
}
seeds[[total_repeats]] <- 123

control <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = cv_repeats, 
                      number = cv_number, verbose = TRUE, allowParallel = FALSE)

# Divide data into predictor variables (x) and response variable (y)
x <- drivers_numeric[, !(colnames(drivers_numeric) == "FNConc")]
y <- drivers_numeric$FNConc

sink(NULL)  # Reset output sink
closeAllConnections()  # Close all connections


# Run RFE to select the best features ----
set.seed(123)
result_rfe <- rfe(x = x, y = y, sizes = c(1:size), rfeControl = control)

# Print RFE results
print(result_rfe)

# Put selected features into variable
new_rf_input <- paste(predictors(result_rfe), collapse = "+")

# Format those features into a formula for the optimized random forest model
rf_formula <- formula(paste("FNConc ~", new_rf_input))

# Test different ntree values 
ntree_values <- seq(100, 2000, by = 100)  
set.seed(123)
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
set.seed(123)
kept_drivers <- drivers_numeric[, colnames(drivers_numeric) %in% predictors(result_rfe)]
tuneRF(kept_drivers, drivers_numeric[, 1], ntreeTry = 2000, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run optimized random forest model, with re-tuned ntree and mtry parameters ----
set.seed(123)
rf_model2 <- randomForest(rf_formula, data = drivers_numeric, 
                          importance = TRUE, proximity = TRUE, ntree = 2000, mtry = 2)

# Visualize output for rf_model2
print(rf_model2)
randomForest::varImpPlot(rf_model2)

# Save RF variable importance plot and LM plot for rf_model2
save_rf_importance_plot(rf_model2, output_dir)
save_lm_plot(rf_model2, drivers_numeric$FNConc, output_dir)

# Save model and required objects for SHAP analysis
save(rf_model2, file = "FNConc_Yearly_rf_model2_full.RData")
kept_drivers <- drivers_numeric[, colnames(drivers_numeric) %in% predictors(result_rfe)]
save(kept_drivers, file = "FNConc_Yearly_kept_drivers_full.RData")
save(drivers_df, file = "FNConc_Yearly_full_stream_ids.RData")
save(drivers_numeric, file = "FNConc_Yearly_full.RData")


