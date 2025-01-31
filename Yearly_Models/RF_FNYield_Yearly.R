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
  title("Yearly FN Si Yield")
  dev.off()
}

# Save RF Variable Importance Plot
save_rf_importance_plot <- function(rf_model, output_dir) {
  pdf(sprintf("%s/RF_variable_importance.pdf", output_dir), width = 8, height = 6)
  randomForest::varImpPlot(rf_model, main = "RF Variable Importance - Yearly FN Yield", col = "darkblue")
  dev.off()
}

# Save Linear Model (LM) Plot
save_lm_plot <- function(rf_model, observed, output_dir) {
  pdf(sprintf("%s/RF_lm_plot.pdf", output_dir), width = 8, height = 8)
  plot(rf_model$predicted, observed, pch = 16, cex = 1.5,
       xlab = "Predicted", ylab = "Observed", main = "Observed vs Predicted - Yearly FN Yield",
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
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"

# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

drivers_df <- read.csv("AllDrivers_Harmonized_Yearly.csv") %>%
  filter(FNYield <= 60) %>%  # Remove rows where FNYield > 60 %>% 
  filter_all(all_vars(!is.infinite(.))) %>%
  filter(FNYield <= 1.5 * FNYield & FNYield >= 0.5 * FNYield) %>%  # Filter rows where FNYield is within 50% of FNYield
  select(-contains("Conc"), -contains("Gen"), -contains("major"), -X, -drainage_area) %>%
  dplyr::mutate_at(vars(17:32), ~replace(., is.na(.), 0)) %>%
  # mutate(
  #   permafrost_mean_m = ifelse(is.na(permafrost_mean_m), 0, permafrost_mean_m),  # Set NA values in permafrost_mean_m to 0
  #   # num_days = ifelse(is.na(num_days), 0, num_days),        # Set NA values in num_days to 0
  #   # max_prop_area = ifelse(is.na(max_prop_area), 0, max_prop_area),  # Set NA values in max_prop_area to 0
  #   across(where(is.integer), as.numeric)) %>%
  select(FNYield, everything()) %>%# Load needed packages
  librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, randomForest, tibble, viridis)

# Clear environment
rm(list = ls())

# Set working directory                 
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Load required data and model from the RF script
load("FNYield_Yearly_rf_model2.RData")
load("FNYield_Yearly_kept_drivers.RData")
load("FNYield_Yearly_train.RData")

drivers_df <- train

# Set global seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"

# Function to create SHAP values
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  # Define a custom prediction function
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)
    predict(object, newdata = newdata)
  }
  
  # Compute SHAP values using fastshap
  shap_values <- fastshap::explain(
    object = model,
    X = kept_drivers,
    pred_wrapper = custom_predict,
    nsim = sample_size
  )
  
  return(shap_values)
}

# Generate SHAP values
shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)

# Function to create overall SHAP plots for the full model
create_all_shapley_plots <- function(shap_values, output_file) {
  # Calculate overall feature importance
  overall_feature_importance <- shap_values %>%
    as.data.frame() %>%
    summarise(across(everything(), ~ mean(abs(.)))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  # Create the feature importance plot
  pdf(output_file, width = 8, height = 8)
  overall_importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value", title = "Overall Feature Importance") +
    theme_minimal()
  print(overall_importance_plot)
  dev.off()
}

# Output full SHAP plots
output_file <- sprintf("%s/FNYield_Yearly_Overall_SHAP_Variable_Importance.pdf", output_dir)
create_all_shapley_plots(shap_values, output_file)

# Function to create SHAP-based partial dependence plots
create_shap_partial_dependence_plots <- function(shap_values, kept_drivers, drivers_df, output_dir, color_var = "FNYield") {
  # Check if the specified coloring variable exists in drivers_df
  if (!(color_var %in% colnames(drivers_df))) {
    stop(paste("The specified color_var:", color_var, "is not in the drivers_df dataframe."))
  }
  
  # Open a PDF to save all SHAP partial dependence plots
  pdf(file = file.path(output_dir, "FNYield_Yearly_SHAP_Partial_Dependence_Plots.pdf"), width = 8, height = 8)
  
  # Loop through each feature in shap_values
  for (feature in colnames(shap_values)) {
    # Extract SHAP values for the current feature
    shap_long <- tibble::tibble(
      feature_value = kept_drivers[[feature]],  # Feature values from kept_drivers
      shap_value = shap_values[, feature],     # SHAP values from shap_values (matrix indexing)
      color_value = drivers_df[[color_var]]    # Coloring variable
    )
    
    # Add the log scale for select drivers
    log_scaled_drivers <- c("drainage_area", "q_5", "evapotrans", "silicate_weathering")
    
    # Create the SHAP-based partial dependence plot
    shap_pdp_plot <- ggplot(shap_long, aes(x = feature_value, y = shap_value, color = color_value)) +
      geom_point(alpha = 0.6) +
      scale_color_viridis_c(name = color_var) +
      geom_hline(yintercept = 0, color = "darkred", linetype = "dashed", size = 1) +
      labs(
        title = paste("SHAP Partial Dependence Plot for", feature),
        x = ifelse(feature %in% log_scaled_drivers, paste("log(", feature, ")", sep = ""), paste("Value of", feature)),
        y = "SHAP Value"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold")) +
      if (feature %in% log_scaled_drivers) scale_x_log10()
    
    # Print the plot to the PDF
    print(shap_pdp_plot)
  }
  
  # Close the PDF file
  dev.off()
  message("SHAP partial dependence plots saved to PDF.")
}

# Generate SHAP-based partial dependence plots
create_shap_partial_dependence_plots(
  shap_values = shap_values,
  kept_drivers = kept_drivers,
  drivers_df = drivers_df,
  output_dir = output_dir,
  color_var = "FNYield"
)

create_subset_importance_plots <- function(shap_values, conditions, kept_drivers, output_dir) {
  for (condition in conditions) {
    condition_column <- condition$column
    condition_value <- condition$value
    operator <- condition$operator
    
    # Skip conditions for features not in kept_drivers
    if (!condition_column %in% colnames(kept_drivers)) {
      message(paste("Skipping condition:", condition_column, "not found in kept_drivers."))
      next
    }
    
    # Filter kept_drivers based on the condition
    subset_kept_drivers <- kept_drivers %>%
      filter(case_when(
        operator == ">" ~ .data[[condition_column]] > condition_value,
        operator == "<" ~ .data[[condition_column]] < condition_value,
        operator == "=" ~ .data[[condition_column]] == condition_value,
        TRUE ~ FALSE
      ))
    
    # Exclude the condition_column from the drivers
    subset_kept_drivers <- subset_kept_drivers %>%
      select(-all_of(condition_column))
    
    # Subset SHAP values to match filtered drivers
    subset_shap_values <- shap_values[rownames(subset_kept_drivers), ]
    
    # Convert subset_shap_values to a data frame and exclude the condition_column
    subset_shap_values <- as.data.frame(subset_shap_values) %>%
      select(-all_of(condition_column))
    
    # Summarize feature importance
    subset_importance <- subset_shap_values %>%
      summarise(across(everything(), ~ mean(abs(.)))) %>%
      pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
      arrange(desc(importance))
    
    # Create the importance plot
    subset_importance_plot <- ggplot(subset_importance, aes(x = reorder(feature, importance), y = importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste("Variable Importance for", condition_column, operator, condition_value, "(Excluding Subset Driver)"),
        x = "Feature",
        y = "Mean Absolute SHAP Value"
      ) +
      theme_minimal()
    
    # Save the plot
    output_file <- file.path(output_dir, paste0("SHAP_Variable_Importance_", condition_column, "_", operator, "_", condition_value, "_Excluding_Subset_Driver.pdf"))
    ggsave(output_file, plot = subset_importance_plot, width = 8, height = 6)
    message(paste("Subset variable importance plot saved:", output_file))
  }
}


# Define multiple flexible conditions for subsetting
conditions <- list(
  list(column = "rocks_volcanic", value = 50, operator = ">"),
  list(column = "land_shrubland_grassland", value = 50, operator = ">"),
  list(column = "land_shrubland_grassland", value = 50, operator = "<"),
  list(column = "Max_Daylength", value = 17, operator = "<"),
  list(column = "Max_Daylength", value = 17, operator = ">")
)

# Retain only conditions relevant to kept_drivers
valid_conditions <- lapply(conditions, function(cond) {
  if (cond$column %in% colnames(kept_drivers)) {
    return(cond)
  } else {
    message(paste("Skipping condition for non-kept feature:", cond$column))
    return(NULL)
  }
})
valid_conditions <- Filter(Negate(is.null), valid_conditions)  # Remove NULLs

# Generate importance plots for valid subsets
create_subset_importance_plots(
  shap_values = shap_values,
  conditions = valid_conditions,
  kept_drivers = kept_drivers,
  output_dir = output_dir
)

  select(-Stream_ID, -Year) %>%
  drop_na()

# Plot and save correlation matrix ----
numeric_drivers <- 2:30
driver_cor <- cor(drivers_df[, numeric_drivers])
save_correlation_plot(driver_cor, output_dir)

# Test different ntree values for rf_model1 ----
ntree_values <- seq(100, 2000, by = 100)  # Define ntree values
set.seed(123)
MSE_list_rf1 <- test_numtree_parallel(ntree_values, FNYield ~ ., drivers_df)

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
rf_model1 <- randomForest(FNYield ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = manual_ntree_rf1, mtry = manual_mtry_rf1)

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
x <- drivers_df[, !(colnames(drivers_df) == "FNYield")]
y <- drivers_df$FNYield

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
tuneRF(kept_drivers, drivers_df[, 1], ntreeTry = 1000, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run optimized random forest model, with re-tuned ntree and mtry parameters ----
set.seed(123)
rf_model2 <- randomForest(rf_formula, data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 1000, mtry = 5)

# Visualize output for rf_model2
print(rf_model2)
randomForest::varImpPlot(rf_model2)

# Generate plots comparing predicted vs observed ----
lm_plot <- plot(rf_model2$predicted, drivers_df$FNYield, pch = 16, cex = 1.5,
                xlab = "Predicted", ylab = "Observed", main = "All Spatial Drivers - Yearly FN Yield",
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model2$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))

# Save RF variable importance plot and LM plot for rf_model2
save_rf_importance_plot(rf_model2, output_dir)
save_lm_plot(rf_model2, drivers_df$FNYield, output_dir)

# Save model and required objects for SHAP analysis
save(rf_model2, file = "FNYield_Yearly_rf_model2.RData")
kept_drivers <- drivers_df[, colnames(drivers_df) %in% predictors(result_rfe)]
save(kept_drivers, file = "FNYield_Yearly_kept_drivers.RData")
save(drivers_df, file = "FNYield_Yearly_drivers_df.RData")
