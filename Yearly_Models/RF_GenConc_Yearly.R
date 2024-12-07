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
  
  # Export objects to workers
  clusterExport(cl, varlist = c("formula", "data"), envir = environment())
  
  MSE <- foreach(ntree = ntree_list, .combine = 'c', .packages = 'randomForest') %dopar% {
    set.seed(123)
    rf_model <- randomForest(formula, data = data, importance = TRUE, proximity = TRUE, ntree = ntree)
    rf_model$mse
  }
  
  stopCluster(cl)
  return(MSE)
}

# Set the output directory path for saving PDFs
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/GenConc"

# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

drivers_df <- read.csv("AllDrivers_Harmonized_Yearly.csv") %>%
  select(-contains("Yield"), -contains("FN"), -contains("major"), -X, -Year, -Name, -ClimateZ) %>%
  dplyr::mutate_at(vars(19:34), ~replace(., is.na(.), 0)) %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  select(GenConc, everything()) %>%
  select(-Stream_ID) %>%
  drop_na()

# Plot and save correlation matrix ----
numeric_drivers <- 2:33
driver_cor <- cor(drivers_df[, numeric_drivers])
save_correlation_plot(driver_cor, output_dir)

# Test different ntree values for rf_model1 ----
ntree_values <- seq(100, 2000, by = 100)  # Define ntree values
set.seed(123)
MSE_list_rf1 <- test_numtree_parallel(ntree_values, GenConc ~ ., drivers_df)

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
manual_ntree_rf1 <- 2000  # Replace with your chosen value

# Tune mtry for rf_model1 ----
tuneRF(drivers_df[, 2:ncol(drivers_df)], drivers_df[, 1], ntreeTry = manual_ntree_rf1, stepFactor = 1, improve = 0.5, plot = TRUE)

# Manually select mtry for rf_model1 ----
manual_mtry_rf1 <- 10  # Replace with your chosen value

# Fit rf_model1 ----
set.seed(123)# Load needed packages
librarian::shelf(remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot, mlbench, pROC, tree, dplyr,
                 plot.matrix, reshape2, rcartocolor, arsenal, googledrive, data.table, ggplot2, corrplot, pdp, 
                 iml, tidyr, viridis, parallel, doParallel, foreach)

# Clear environment
rm(list = ls())

# Global seed setting to ensure consistency across the whole workflow
set.seed(123)

# Load Functions ----
# Function to see variable importance by regime
import_plot <- function(rf_model) {
  importance_df <- as.data.frame(rf_model$importance)
  importance_df$driver <- rownames(importance_df)
  importance_melt <- melt(importance_df, id.vars = c("MeanDecreaseAccuracy", "MeanDecreaseGini", "driver"))
  ggplot(importance_melt, aes(driver, value)) +
    geom_point() +
    facet_wrap(~variable) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Optimized function to test ntree
test_numtree_average <- function(ntree_list) {
  library(parallel)
  library(doParallel)
  library(foreach)
  
  # Use available cores for parallel computation
  num_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  # Export drivers_df to workers
  clusterExport(cl, varlist = c("drivers_df"), envir = environment())
  
  # Parallel computation for each ntree
  MSE <- foreach(ntree = ntree_list, .combine = 'c', .packages = "randomForest") %dopar% {
    set.seed(123)  # Ensure reproducibility
    rf_model <- randomForest(GenConc ~ ., data = drivers_df, importance = TRUE, proximity = FALSE, ntree = ntree)
    rf_model$mse
  }
  
  # Stop the parallel cluster
  stopCluster(cl)
  
  return(MSE)
}


# Function to test different numbers of trees (ntree) for optimized RF
test_numtree_optimized <- function(ntree_list) {
  MSE <- list()
  for (i in 1:length(ntree_list)) {
    # Set seed for each model training step
    set.seed(123)
    rf_model <- randomForest(rf_formula, data = drivers_df, importance = TRUE, proximity = TRUE, ntree = ntree_list[[i]])
    MSE[[i]] <- rf_model$mse
  }
  return(MSE)
}

# Function to remove outliers based on Z-scores
cols_to_consider <- c("GenConc")
sd_limit <- 3
remove_outlier_rows <- function(data_to_filter, cols = cols_to_consider, limit = sd_limit) {
  z_scores <- sapply(data_to_filter[cols], function(data) abs((data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE)))
  return(data_to_filter[rowSums(z_scores > limit, na.rm = TRUE) == 0, ])
}

# Define a function to save RF variable importance plot as a PDF
save_rf_importance_plot <- function(rf_model, output_dir) {
  pdf(sprintf("%s/RF_variable_importance.pdf", output_dir), width = 8, height = 6)
  randomForest::varImpPlot(rf_model, main = "RF Variable Importance - Yearly Gen Concentration", col = "darkblue")
  dev.off()
}

# Define a function to save the linear model (LM) plot as a PDF
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

# Set the output directory path for saving PDFs
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/GenConc"

# Read in and tidy data ----
# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

drivers_df <- read.csv("AllDrivers_Harmonized_Yearly.csv") %>%
  select(-contains("Yield"), -contains("FN"), -contains("major"), -X, -Year, -Name, -ClimateZ) %>%
  dplyr::mutate_at(vars(19:34), ~replace(., is.na(.), 0)) %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  select(-Stream_ID) %>%
  select(GenConc, everything()) %>%
  drop_na()

# Remove outliers using custom function
# drivers_df <- remove_outlier_rows(drivers_df)

# Plot correlation between driver variables ----
numeric_drivers <- 2:33  # Indices for numeric drivers
driver_cor <- cor(drivers_df[, numeric_drivers])
corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = F)

pdf(sprintf("%s/correlation_plot.pdf", output_dir), width = 10, height = 10)
corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
title("Yearly Gen Si Concentration")  # Add title in the PDF
dev.off()

# Global seed before testing different numbers of trees (ntree) ----
set.seed(123)
MSE_list <- test_numtree_average(c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000))
tre_list <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000)
MSE_df <- as.data.frame(unlist(MSE_list))
MSE_num <- list()

for (i in 1:length(tre_list)) {
  MSE_num[[i]] <- rep(tre_list[i], tre_list[i])
}

MSE_df$tree_num <- unlist(MSE_num)
MSE_mean <- MSE_df %>%
  group_by(tree_num) %>%
  summarise(mean_MSE = mean(`unlist(MSE_list)`))

# Visualize and select the number of trees that gives the minimum MSE error
ggplot(MSE_mean, aes(tree_num, mean_MSE)) + geom_point() + geom_line() + theme_classic() +
  scale_x_continuous(breaks = seq(100, 2000, 100)) + theme(text = element_text(size = 20))

# Global seed before tuning mtry based on optimized ntree ----
set.seed(123)
tuneRF(drivers_df[, numeric_drivers], drivers_df[, 1], ntreeTry = 2000, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run initial RF using tuned parameters ----
set.seed(123)
rf_model1 <- randomForest(GenConc ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 2000, mtry = 10)

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


# control <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = cv_repeats, number = cv_number, seeds = seeds, verbose = TRUE)
control <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, number = 5, verbose = FALSE)

# Try a smaller subset of features (e.g., only the first 10 features)----
x_small <- drivers_df[, 2:11]
y_small <- drivers_df$GenConc

# Run RFE with a smaller dataset
set.seed(123)
result_rfe_small <- rfe(x = x_small, y = y_small, sizes = c(1:10), rfeControl = control)

# View the result
print(result_rfe_small)


# Divide data into predictor variables (x) and response variable (y)
x <- drivers_df[, !(colnames(drivers_df) == "GenConc")]
y <- drivers_df$GenConc

sink(NULL)  # Reset output sink
closeAllConnections()  # Close any open connections

# Run RFE to select the best features ----
set.seed(123)
result_rfe <- rfe(x = x, y = y, sizes = c(1:size), rfeControl = control)

# Print RFE results
print(result_rfe)

# Put selected features into variable
new_rf_input <- paste(predictors(result_rfe), collapse = "+")

# Format those features into a formula for the optimized random forest model
rf_formula <- formula(paste("GenConc ~", new_rf_input))

# Global seed before re-tuning RF after RFE optimization ----
set.seed(123)
MSE_list <- test_numtree_optimized(c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000))

# Visualize and select number of trees that gives the minimum MSE error
MSE_df <- as.data.frame(unlist(MSE_list))
MSE_num <- list()

for (i in 1:length(tre_list)) {
  MSE_num[[i]] <- rep(tre_list[i], tre_list[i])
}

MSE_df$tree_num <- unlist(MSE_num)
MSE_mean <- MSE_df %>%
  group_by(tree_num) %>%
  summarise(mean_MSE = mean(`unlist(MSE_list)`))

ggplot(MSE_mean, aes(tree_num, mean_MSE)) + geom_point() + geom_line() +
  theme_classic() + scale_x_continuous(breaks = seq(100, 2000, 100)) + theme(text = element_text(size = 20))

# Global seed before re-tuning mtry
set.seed(123)
kept_drivers <- drivers_df[, colnames(drivers_df) %in% predictors(result_rfe)]
tuneRF(kept_drivers, drivers_df[, 1], ntreeTry = 2000, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run optimized random forest model, with re-tuned ntree and mtry parameters ----
set.seed(123)
rf_model2 <- randomForest(rf_formula, data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 2000, mtry = 6)

# Visualize output for rf_model2
print(rf_model2)
randomForest::varImpPlot(rf_model2)

# Generate plots comparing predicted vs observed ----
lm_plot <- plot(rf_model2$predicted, drivers_df$GenConc, pch = 16, cex = 1.5,
                xlab = "Predicted", ylab = "Observed", main = "All Spatial Drivers - Gen Concentration",
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model2$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))

# Save RF variable importance plot and LM plot for rf_model2
save_rf_importance_plot(rf_model2, output_dir)
save_lm_plot(rf_model2, drivers_df$GenConc, output_dir)

# Save model and required objects for SHAP analysis
save(rf_model2, file = "GenConc_Yearly_rf_model2.RData")
kept_drivers <- drivers_df[, colnames(drivers_df) %in% predictors(result_rfe)]
save(kept_drivers, file = "GenConc_Yearly_kept_drivers.RData")
save(drivers_df, file = "GenConc_Yearly_drivers_df.RData")


rf_model1 <- randomForest(GenConc ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = manual_ntree_rf1, mtry = manual_mtry_rf1)

# Perform RFE to select features for rf_model2 ----
x <- drivers_df[, !(colnames(drivers_df) == "GenConc")]
y <- drivers_df$GenConc
control <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, number = 5, verbose = FALSE)

set.seed(123)
result_rfe <- rfe(x = x, y = y, sizes = c(1:ncol(x)), rfeControl = control)
selected_features <- predictors(result_rfe)
rf_formula <- as.formula(paste("GenConc ~", paste(selected_features, collapse = " + ")))

# Test different ntree values for rf_model2 ----
MSE_list_rf2 <- test_numtree_parallel(ntree_values, rf_formula, drivers_df)

# Visualize MSE results for rf_model2 ----
MSE_df_rf2 <- data.frame(
  ntree = ntree_values,
  mean_MSE = sapply(MSE_list_rf2, mean)
)

ggplot(MSE_df_rf2, aes(ntree, mean_MSE)) + 
  geom_point() + 
  geom_line() + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(100, 2000, 100)) + 
  theme(text = element_text(size = 20))

# Manually select ntree for rf_model2 ----
manual_ntree_rf2 <- 1200  # Replace with your chosen value

# Tune mtry for rf_model2 ----
tuneRF(drivers_df[, selected_features], drivers_df[, 1], ntreeTry = manual_ntree_rf2, stepFactor = 1, improve = 0.5, plot = TRUE)

# Manually select mtry for rf_model2 ----
manual_mtry_rf2 <- 8  # Replace with your chosen value

# Fit rf_model2 ----
set.seed(123)
rf_model2 <- randomForest(rf_formula, data = drivers_df, importance = TRUE, proximity = TRUE, ntree = manual_ntree_rf2, mtry = manual_mtry_rf2)

# Visualize and save RF variable importance plot for rf_model2 ----
save_rf_importance_plot(rf_model2, output_dir)

# Visualize and save LM plot for rf_model2 ----
save_lm_plot(rf_model2, drivers_df$GenConc, output_dir)

# Save final model and required objects for SHAP analysis ----
save(rf_model2, file = "GenConc_Yr_rf_model2.RData")
kept_drivers <- drivers_df[, colnames(drivers_df) %in% selected_features]
save(kept_drivers, file = "GenConc_Yr_kept_drivers.RData")
save(drivers_df, file = "GenConc_Yr_drivers_df.RData")
