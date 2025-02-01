dev.off()  # Close any open graphics device

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
  pdf(sprintf("%s/correlation_plot_GenYield_Average_5_years.pdf", output_dir), width = 10, height = 10)
  corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
  title("Average GenYield")
  dev.off()
}

# Save RF Variable Importance Plot
save_rf_importance_plot <- function(rf_model, output_dir) {
  pdf(sprintf("%s/RF_variable_importance_GenYield_Average_5_years_Train.pdf", output_dir), width = 8, height = 6)
  randomForest::varImpPlot(rf_model, main = "RF Variable Importance - Average GenYield", col = "darkblue")
  dev.off()
}

# Save Linear Model (LM) Plot
save_lm_plot <- function(rf_model, observed, output_dir) {
  pdf(sprintf("%s/RF_lm_plot_GenYield_Average_5_years.pdf", output_dir), width = 8, height = 8)
  plot(rf_model$predicted, observed, pch = 16, cex = 1.5,
       xlab = "Predicted", ylab = "Observed", main = "Observed vs Predicted - Average GenYield",
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
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Average_Model/GenYield"

# Define record length (1, 5, 10, 20... years)
record_length <- 5

# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

# Load and preprocess the data with dynamic file selection
drivers_df <- read.csv(sprintf("AllDrivers_Harmonized_Average_filtered_%d_years.csv", record_length)) %>%
  filter(!grepl("^MCM", Stream_ID)) %>% # Remove all Stream_IDs that start with "MCM"
  filter(GenYield <= 80) %>%  # Remove rows where FNYield > 60 %>% 
  select(-contains("Conc"), -contains("FN"), -contains("major"), -Max_Daylength) %>%
  dplyr::mutate_at(vars(19:34), ~replace(., is.na(.), 0)) %>%  # Replace NAs with 0 for land and rock columns
  select(GenYield, everything()) %>%
  filter(!Stream_ID %in% c("USGS__Dismal River", "KRR__S65E"))  # Remove specific outlier sites

# Identify Stream_IDs, Years, and Variables with NA values
na_summary <- drivers_df %>%
  pivot_longer(cols = -c(Stream_ID), names_to = "Variable", values_to = "Value") %>%
  filter(is.na(Value)) %>%
  distinct(Stream_ID, Variable)

# Count the number of unique Stream_IDs before removing it
unique_stream_id_na_count <- na_summary %>%
  summarise(na_summary = n_distinct(Stream_ID)) %>%
  pull(na_summary)

# Export unique NA Stream_IDs with dynamic filename
write.csv(na_summary, 
          sprintf("average_NA_stream_ids_%d_years.csv", record_length), 
          row.names = FALSE)

gc()

# Keep only complete cases
drivers_df <- drivers_df %>%
  select(GenYield, everything()) %>%
  filter(complete.cases(.))

# Count the number of unique Stream_IDs before removing it
unique_stream_id_count <- drivers_df %>%
  summarise(unique_count = n_distinct(Stream_ID)) %>%
  pull(unique_count)

# Export with dynamic filename
write.csv(drivers_df, 
          sprintf("unique_stream_ids_average_%d_years.csv", record_length), 
          row.names = FALSE)

gc()

# Final step: Remove Stream_ID and Year
drivers_df <- drivers_df %>%
  select(-Stream_ID)

# Plot and save correlation matrix ----
numeric_drivers <- 2:32 # Change this range to reflect data frame length
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
ntree_values <- seq(100, 2000, by = 100)  # Define ntree values
set.seed(123)
MSE_list_rf1 <- test_numtree_parallel(ntree_values, GenYield ~ ., drivers_df)

# Visualize MSE results for rf_model1 ----
MSE_df_rf1 <- data.frame(
  ntree = ntree_values,
  mean_MSE = sapply(MSE_list_rf1, mean)
)

dev.new()  # Open new plotting window

p <- ggplot(MSE_df_rf1, aes(ntree, mean_MSE)) + 
  geom_point() + 
  geom_line() + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(100, 2000, 100)) + 
  theme(text = element_text(size = 20))

print(p)


# Manually select ntree for rf_model1 ----
manual_ntree_rf1 <- 700  # Replace with chosen value

# Tune mtry for rf_model1 ----
tuneRF(train[, 2:ncol(train)], train[, 1], ntreeTry = manual_ntree_rf1, stepFactor = 1, improve = 0.5, plot = TRUE)

# Manually select mtry for rf_model1 ----
manual_mtry_rf1 <- 10  # Replace with chosen value

# Run initial RF using tuned parameters ----
set.seed(123)
rf_model1 <- randomForest(GenYield ~ ., data = train, importance = TRUE, proximity = TRUE, ntree = manual_ntree_rf1, mtry = manual_mtry_rf1)

# Visualize output for rf_model1
print(rf_model1)
randomForest::varImpPlot(rf_model1)

# Generate plots comparing predicted vs observed ----
lm_plot <- plot(rf_model1$predicted, train$GenYield, pch = 16, cex = 1.5,
                xlab = "Predicted", ylab = "Observed", main = "Trained RF Model 1 Average GenYield",
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model1$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model1$mse), digits = 3)))

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
x <- train[, !(colnames(train) == "GenYield")]
y <- train$GenYield

sink(NULL)  # Reset output sink
closeAllConnections()  # Close all connections
dev.off()  # Close any open graphic devices
dev.new()  # Open new plotting window


# Run RFE to select the best features ----
set.seed(123)
result_rfe <- rfe(x = x, y = y, sizes = c(1:size), rfeControl = control)

# Print RFE results
print(result_rfe)

# Check how many variables are selected by RFE
selected_variables <- predictors(result_rfe)

if (length(selected_variables) == 1) {
  # If only one variable is left, use a simple linear regression
  lm_formula <- as.formula(paste("GenYield ~", selected_variables))
  
  cat("Only one variable selected (", selected_variables, "). Running linear regression instead of RF.\n")
  
  # Train linear model
  lm_model <- lm(lm_formula, data = train)
  
  # Save model for later use
  save(lm_model, file = "GenYield_Average_lm_model.RData")
  
  # Predict on test data
  test_predictions <- predict(lm_model, test)
  train_predictions <- predict(lm_model, train)  # Predict on training data
  
  # Evaluate test predictions
  test_r2 <- cor(test_predictions, test$GenYield)^2
  test_mse <- mean((test_predictions - test$GenYield)^2)
  
  # Evaluate train predictions
  train_r2 <- cor(train_predictions, train$GenYield)^2
  train_mse <- mean((train_predictions - train$GenYield)^2)
  
  cat("Test R² for lm_model:", test_r2, "\n")
  cat("Test MSE for lm_model:", test_mse, "\n")
  cat("Train R² for lm_model:", train_r2, "\n")
  cat("Train MSE for lm_model:", train_mse, "\n")
  
  # Save observed vs predicted plot for test data
  pdf(sprintf("%s/LM_GenYield_Average_Observed_vs_Predicted_Test.pdf", output_dir), width = 8, height = 8)
  plot(
    test_predictions, test$GenYield, 
    pch = 16, cex = 1.5,
    xlab = "Predicted", ylab = "Observed", 
    main = "Observed vs Predicted - GenYield Average Test Data (lm_model)",
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
  dev.off()  # Close the PDF device
  
  # Save observed vs predicted plot for train data
  pdf(sprintf("%s/LM_GenYield_Average_Observed_vs_Predicted_Train.pdf", output_dir), width = 8, height = 8)
  plot(
    train_predictions, train$GenYield, 
    pch = 16, cex = 1.5,
    xlab = "Predicted", ylab = "Observed", 
    main = "Observed vs Predicted - GenYield Average Train Data (lm_model)",
    cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5
  )
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2)
  legend(
    "topleft", bty = "n", cex = 1.5,
    legend = paste("R² =", format(train_r2, digits = 3))
  )
  legend(
    "bottomright", bty = "n", cex = 1.5,
    legend = paste("MSE =", format(train_mse, digits = 3))
  )
  dev.off()  # Close the PDF device
  
  # Save test predictions
  test_results <- test %>%
    mutate(Predicted_GenYield = test_predictions)
  
  write.csv(test_results, "Test_Predictions_lm_Average.csv", row.names = FALSE)
  
  # Save train predictions
  train_results <- train %>%
    mutate(Predicted_GenYield = train_predictions)
  
  write.csv(train_results, "Train_Predictions_lm_Average.csv", row.names = FALSE)
  
  cat("Test predictions saved to Test_Predictions_lm_Average.csv\n")
  cat("Train predictions saved to Train_Predictions_lm_Average.csv\n")

} else {
  # If multiple variables remain, proceed with random forest model
  rf_formula <- as.formula(paste("GenYield ~", paste(selected_variables, collapse = "+")))
  
  set.seed(123)
  rf_model2 <- randomForest(rf_formula, data = train, 
                            importance = TRUE, proximity = TRUE, ntree = 1100, mtry = 10)
  
  # Save model
  save(rf_model2, file = "GenYield_Average_rf_model2.RData")
  
  # Predict on test data
  test_predictions <- predict(rf_model2, test)
  
  # Evaluate predictions
  test_r2 <- cor(test_predictions, test$GenYield)^2
  test_mse <- mean((test_predictions - test$GenYield)^2)
  
  cat("Test R² for rf_model2:", test_r2, "\n")
  cat("Test MSE for rf_model2:", test_mse, "\n")
  
  # Save test predictions
  test_results <- test %>%
    mutate(Predicted_GenYield = test_predictions)
  
  write.csv(test_results, "Test_Predictions_rf_model2_Average.csv", row.names = FALSE)
  
  cat("Test predictions saved to Test_Predictions_rf_model2_Average.csv\n")
}
