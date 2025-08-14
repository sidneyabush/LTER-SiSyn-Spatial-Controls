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
  title("Yearly FNYield")
  dev.off()
}

# Save RF Variable Importance Plot
save_rf_importance_plot <- function(rf_model, output_dir) {
  pdf(sprintf("%s/RF_variable_importance.pdf", output_dir), width = 8, height = 6)
  randomForest::varImpPlot(rf_model, main = "RF Model 2 Trained Data - Yearly FNYield", col = "darkblue")
  dev.off()
}

# Save Linear Model (LM) Plot
save_lm_plot <- function(rf_model, observed, output_dir) {
  pdf(sprintf("%s/RF_lm_plot.pdf", output_dir), width = 8, height = 8)
  plot(rf_model$predicted, observed, pch = 16, cex = 1.5,
       xlab = "Predicted", ylab = "Observed", main = "RF Model 2 Trained Data - Yearly FNYield",
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2)
  legend("topleft", bty = "n", cex = 1.5, legend = paste("R² =", format(mean(rf_model$rsq), digits = 3)))
  legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model$mse), digits = 3)))
  dev.off()
}

# Parallelized function to test ntree
test_numtree_parallel <- function(ntree_list, formula, data) {
  num_cores <- min(4, parallel::detectCores() - 2)  # Use a maximum of 4 cores
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  
  # Collect results
  MSE <- foreach(ntree = ntree_list, .combine = 'c', .packages = 'randomForest') %dopar% {
    set.seed(123)
    rf_model <- randomForest(formula, data = data, importance = TRUE, ntree = ntree)
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
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"

# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

# Define record length (1, 5, 10, 20... years)
record_length <- 5

# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

# Load and preprocess the data with dynamic file selection
drivers_df <- read.csv(sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years.csv", record_length)) %>%
  filter(GenYield <= 80) %>%  # Remove rows where FNYield > 60 %>% 
  select(-contains("Conc"), -contains("Gen"), -contains("major"), -Max_Daylength, -Q, -drainage_area) %>%
  dplyr::mutate_at(vars(16:31), ~replace(., is.na(.), 0)) %>%  # Replace NAs with 0 for land and rock columns
  select(FNYield, everything()) %>%
  filter(!Stream_ID %in% c("USGS__Dismal River", "KRR__S65E"))  # Remove specific outlier sites

# Identify Stream_IDs, Years, and Variables with NA values
na_summary <- drivers_df %>%
  pivot_longer(cols = -c(Stream_ID, Year), names_to = "Variable", values_to = "Value") %>%
  filter(is.na(Value)) %>%
  filter(Year >= 2001 & Year <= 2024) %>%
  distinct(Stream_ID, Year, Variable)

# Count the number of unique Stream_IDs before removing it
unique_stream_id_na_count <- na_summary %>%
  summarise(na_summary = n_distinct(Stream_ID)) %>%
  pull(na_summary)

# Export unique NA Stream_IDs with dynamic filename
write.csv(na_summary, 
          sprintf("yearly_NA_stream_ids_%d_years.csv", record_length), 
          row.names = FALSE)

gc()

# Keep only complete cases
drivers_df <- drivers_df %>%
  select(FNYield, everything()) %>%
  filter(complete.cases(.))

# Count the number of unique Stream_IDs before removing it
unique_stream_id_count <- drivers_df %>%
  summarise(unique_count = n_distinct(Stream_ID)) %>%
  pull(unique_count)

# Export with dynamic filename
write.csv(drivers_df, 
          sprintf("unique_stream_ids_yearly_%d_years.csv", record_length), 
          row.names = FALSE)

gc()

# Final step: Remove Stream_ID and Year
drivers_df <- drivers_df %>%
  select(-Stream_ID, -Year)

# Plot and save correlation matrix ----
numeric_drivers <- 2:29 # Change this range to reflect data frame length
driver_cor <- cor(drivers_df[, numeric_drivers])
save_correlation_plot(driver_cor, output_dir)

# ---- Split Data into Train/Test ----
# Add the new training and testing workflow here
set.seed(123)
split_index <- sample(2, nrow(drivers_df), replace = TRUE, prob = c(0.7, 0.3))
train <- drivers_df[split_index == 1, ]
test <- drivers_df[split_index == 2, ]

# ---- Train Initial RF Model ----
# Test different ntree values for rf_model1
ntree_values <- seq(100, 2000, by = 100)  # Reduce to avoid memory overload

set.seed(123)
MSE_list_rf1 <- test_numtree_parallel(ntree_values, FNYield ~ ., drivers_df)

# Visualize MSE results for rf_model1 ----
MSE_df_rf1 <- data.frame(
  ntree = ntree_values,
  mean_MSE = sapply(MSE_list_rf1, mean)
)

gc()

ggplot(MSE_df_rf1, aes(ntree, mean_MSE)) + 
  geom_point() + 
  geom_line() + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(100, 2000, 100)) + 
  theme(text = element_text(size = 20))

gc()

# Manually select ntree for rf_model1 ----
manual_ntree_rf1 <- 700  # Replace with chosen value

gc()

# Tune mtry for rf_model1 ----
tuneRF(train[, 2:ncol(train)], train[, 1], ntreeTry = manual_ntree_rf1, stepFactor = 1, improve = 0.5, plot = TRUE)

gc()

# Manually select mtry for rf_model1 ----
manual_mtry_rf1 <- 10  # Replace with chosen value

gc()

# Run initial RF using tuned parameters ----
set.seed(123)
rf_model1 <- randomForest(FNYield ~ ., data = train, importance = TRUE, proximity = TRUE, ntree = manual_ntree_rf1, mtry = manual_mtry_rf1)

# Visualize output for rf_model1
print(rf_model1)
randomForest::varImpPlot(rf_model1)

# # Generate plots comparing predicted vs observed ----
# lm_plot <- plot(rf_model1$predicted, train$FNYield, pch = 16, cex = 1.5,
#                 xlab = "Predicted", ylab = "Observed", main = "RF Model 1 Trained Data - Yearly FNYield",
#                 cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
#   abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
#   theme(text = element_text(size = 40), face = "bold")
# legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model1$rsq), digits = 3)))
# legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model1$mse), digits = 3)))

# Evaluate RF Model on Train/Test Datasets
train_pred <- predict(rf_model1, train)
test_pred <- predict(rf_model1, test)

cat("Train R²:", cor(train_pred, train$FNYield)^2, "\n")
cat("Test R²:", cor(test_pred, test$FNYield)^2, "\n")

# Start Tuning with RFE and 2nd RFModel ----
# Global seed for RFE ----
size <- ncol(train) - 1  # This is the number of predictor variables
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
x <- train[, !(colnames(train) == "FNYield")]
y <- train$FNYield

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
rf_formula <- formula(paste("FNYield ~", new_rf_input))

# Test different ntree values 
ntree_values <- seq(100, 2000, by = 100)  
set.seed(123)
MSE_list_parallel <- test_numtree_parallel_optimized(ntree_values, rf_formula, train)

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
kept_drivers <- train[, colnames(train) %in% predictors(result_rfe)]
tuneRF(kept_drivers, train[, 1], ntreeTry = 1000, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run optimized random forest model, with re-tuned ntree and mtry parameters ----
set.seed(123)
rf_model2 <- randomForest(rf_formula, data = train, importance = TRUE, proximity = TRUE, ntree = 1000, mtry = 5)

# Visualize output for rf_model2
print(rf_model2)
randomForest::varImpPlot(rf_model2)

# Generate plots comparing predicted vs observed ----
lm_plot <- plot(rf_model2$predicted, train$FNYield, pch = 16, cex = 1.5,
                xlab = "Predicted", ylab = "Observed", main = "RF Model 2 Trained Data - Yearly FNYield",
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model2$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))

# Save RF variable importance plot and LM plot for rf_model2
save_rf_importance_plot(rf_model2, output_dir)
save_lm_plot(rf_model2, train$FNYield, output_dir)

# Save model and required objects for SHAP analysis
save(rf_model2, file = "FNYield_Yearly_rf_model2.RData")
kept_drivers <- train[, colnames(train) %in% predictors(result_rfe)]
save(kept_drivers, file = "FNYield_Yearly_kept_drivers.RData")
save(train, file = "FNYield_Yearly_train.RData")

# ---- Use Predict Function on Test Data ----
# Predict on test data using rf_model2
test_predictions <- predict(rf_model2, test)

# Evaluate predictions: Calculate R² and Mean Squared Error
test_r2 <- cor(test_predictions, test$FNYield)^2
test_mse <- mean((test_predictions - test$FNYield)^2)

cat("Test R² for rf_model2:", test_r2, "\n")
cat("Test MSE for rf_model2:", test_mse, "\n")

# ---- Visualize Observed vs Predicted ----
# Save observed vs predicted plot for test data
pdf(sprintf("%s/RF_Observed_vs_Predicted_Test_rf_model2.pdf", output_dir), width = 8, height = 8)
plot(
  test_predictions, test$FNYield, 
  pch = 16, cex = 1.5,
  xlab = "Predicted", ylab = "Observed", 
  main = "RF Model 2 Test Data Yearly FNYield",
  cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5
)
abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2)
legend(
  "topleft", bty = "n", cex = 1.5,
  legend = paste("R² =", format(test_r2, digits = 3))
)
legend(
  "bottomright", bty = "n", cex = 1.5,
  legend = paste("MSE =", format(test_mse, digits = 3))
)
dev.off()

# ---- Save Test Predictions ----
test_results <- test %>%
  mutate(Predicted_FNYield = test_predictions)  # Add predictions to test data

write.csv(test_results, "Test_Predictions_rf_model2_yearly_FNYield.csv", row.names = FALSE)

cat("Test predictions saved to Test_Predictions_rf_model2_yearly_FNYield.csv\n")


