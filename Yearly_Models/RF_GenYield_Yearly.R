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
  pdf(sprintf("%s/correlation_plot.pdf", output_dir), width = 10, height = 10)
  corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
  title("Yearly Gen Si Concentration")
  dev.off()
}

# Save RF Variable Importance Plot
save_rf_importance_plot <- function(rf_model, output_dir) {
  pdf(sprintf("%s/RF_variable_importance.pdf", output_dir), width = 8, height = 6)
  randomForest::varImpPlot(rf_model, main = "RF Variable Importance - Yearly Gen Concentration", col = "darkblue")
  dev.off()
}

# Save Linear Model (LM) Plot
save_lm_plot <- function(rf_model, observed, output_dir) {
  pdf(sprintf("%s/RF_lm_plot.pdf", output_dir), width = 8, height = 8)
  plot(rf_model$predicted, observed, pch = 16, cex = 1.5,
       xlab = "Predicted", ylab = "Observed", main = "Observed vs Predicted - Yearly Gen Concentration",
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2)
  legend("topleft", bty = "n", cex = 1.5, legend = paste("R² =", format(mean(rf_model$rsq), digits = 3)))
  legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model$mse), digits = 3)))
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
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/GenYield"

# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

drivers_df <- read.csv("AllDrivers_Harmonized_Yearly.csv") %>%
  filter(GenConc <= 60) %>%  # Remove rows where GenConc > 60
  select(-contains("Conc"), -contains("FN"), -contains("major"), -X, -Name, -ClimateZ, -drainSqKm) %>%
  dplyr::mutate_at(vars(19:34), ~replace(., is.na(.), 0)) %>%
  mutate(
    permafrost_mean_m = ifelse(is.na(permafrost_mean_m), 0, permafrost_mean_m),  # Set NA values in permafrost_mean_m to 0
    # num_days = ifelse(is.na(num_days), 0, num_days),        # Set NA values in num_days to 0
    # max_prop_area = ifelse(is.na(max_prop_area), 0, max_prop_area),  # Set NA values in max_prop_area to 0
    across(where(is.integer), as.numeric)) %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  select(GenYield, everything()) %>%
  select(-Stream_ID, -Year) %>%
  drop_na()

## NEED TO ALSO DECIDE WHICH YEARS TO FILTER (e.g., MODIS data only starts in 2001 so 2001 - 2022? 2023? see where wrtds_df ends)

# Plot and save correlation matrix ----
numeric_drivers <- 2:32
driver_cor <- cor(drivers_df[, numeric_drivers])
save_correlation_plot(driver_cor, output_dir)

# Test different ntree values for rf_model1 ----
ntree_values <- seq(100, 2000, by = 100)  # Define ntree values
set.seed(123)
MSE_list_rf1 <- test_numtree_parallel(ntree_values, GenYield ~ ., drivers_df)

# Visualize MSE results for rf_model1 ----
MSE_df_rf1 <- data.frame(
  ntree = ntree_values,
  mean_MSE = sapply(MSE_list_rf1, mean)
)

ggplot(MSE_df_rf1, aes(ntree, mean_MSE)) + 
  geom_point() + 
  geom_line() + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(100, 2000, 100)) + 
  theme(text = element_text(size = 20))


# Manually select ntree for rf_model1 ----
manual_ntree_rf1 <- 1000  # Replace with your chosen value

# Tune mtry for rf_model1 ----
tuneRF(drivers_df[, 2:ncol(drivers_df)], drivers_df[, 1], ntreeTry = manual_ntree_rf1, stepFactor = 1, improve = 0.5, plot = TRUE)

# Manually select mtry for rf_model1 ----
manual_mtry_rf1 <- 10  # Replace with your chosen value

# Run initial RF using tuned parameters ----
set.seed(123)
rf_model1 <- randomForest(GenYield ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = manual_ntree_rf1, mtry = manual_mtry_rf1)

# Visualize output for rf_model1
print(rf_model1)
randomForest::varImpPlot(rf_model1)

# Global seed for RFE ----
size <- ncol(drivers_df) - 1  # This is the number of predictor variables
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
x <- drivers_df[, !(colnames(drivers_df) == "GenYield")]
y <- drivers_df$GenYield

sink(NULL)  # Reset output sink
closeAllConnections()  # Close all connections
dev.off()  # Close any open graphic devices

# Run RFE to select the best features ----
set.seed(123)
result_rfe <- rfe(x = x, y = y, sizes = c(1:size), rfeControl = control)

# Print RFE results
print(result_rfe)

# Put selected features into variable
new_rf_input <- paste(predictors(result_rfe), collapse = "+")

# Format those features into a formula for the optimized random forest model
rf_formula <- formula(paste("GenYield ~", new_rf_input))

# Test different ntree values using parallel processing
ntree_values <- seq(100, 2000, by = 100)  # Define ntree values to test
set.seed(123)
MSE_list_parallel <- test_numtree_parallel_optimized(ntree_values, rf_formula, drivers_df)

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
kept_drivers <- drivers_df[, colnames(drivers_df) %in% predictors(result_rfe)]
tuneRF(kept_drivers, drivers_df[, 1], ntreeTry = 500, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run optimized random forest model, with re-tuned ntree and mtry parameters ----
set.seed(123)
rf_model2 <- randomForest(rf_formula, data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 500, mtry = 5)

# Visualize output for rf_model2
print(rf_model2)
randomForest::varImpPlot(rf_model2)

# Generate plots comparing predicted vs observed ----
lm_plot <- plot(rf_model2$predicted, drivers_df$GenYield, pch = 16, cex = 1.5,
                xlab = "Predicted", ylab = "Observed", main = "All Spatial Drivers - Yearly Gen Yield",
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model2$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))

# Save RF variable importance plot and LM plot for rf_model2
save_rf_importance_plot(rf_model2, output_dir)
save_lm_plot(rf_model2, drivers_df$GenYield, output_dir)

# Save model and required objects for SHAP analysis
save(rf_model2, file = "GenYield_Yearly_rf_model2.RData")
kept_drivers <- drivers_df[, colnames(drivers_df) %in% predictors(result_rfe)]
save(kept_drivers, file = "GenYield_Yearly_kept_drivers.RData")
save(drivers_df, file = "GenYield_Yearly_drivers_df.RData")
