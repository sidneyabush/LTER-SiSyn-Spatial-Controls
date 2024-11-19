# Load needed packages
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

# Function to test ntree - change the internal function to reflect the RF model that you are using
test_numtree_average <- function(ntree_list) {
  MSE <- list()
  for (i in 1:length(ntree_list)) {
    # Set seed for each model training step within the loop
    set.seed(123)
    rf_model <- randomForest(median_GenConc ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = ntree_list[[i]])
    MSE[[i]] <- rf_model$mse
  }
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
cols_to_consider <- c("median_GenConc")
sd_limit <- 3
remove_outlier_rows <- function(data_to_filter, cols = cols_to_consider, limit = sd_limit) {
  z_scores <- sapply(data_to_filter[cols], function(data) abs((data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE)))
  return(data_to_filter[rowSums(z_scores > limit, na.rm = TRUE) == 0, ])
}

# Define a function to save RF variable importance plot as a PDF
save_rf_importance_plot <- function(rf_model, output_dir) {
  pdf(sprintf("%s/RF_variable_importance.pdf", output_dir), width = 8, height = 6)
  randomForest::varImpPlot(rf_model, main = "RF Variable Importance - Average Gen Concentration", col = "darkblue")
  dev.off()
}

# Define a function to save the linear model (LM) plot as a PDF
save_lm_plot <- function(rf_model, observed, output_dir) {
  pdf(sprintf("%s/RF_lm_plot.pdf", output_dir), width = 8, height = 8)
  plot(rf_model$predicted, observed, pch = 16, cex = 1.5,
       xlab = "Predicted", ylab = "Observed", main = "Observed vs Predicted - Average Gen Concentration",
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2)
  legend("topleft", bty = "n", cex = 1.5, legend = paste("R² =", format(mean(rf_model$rsq), digits = 3)))
  legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model$mse), digits = 3)))
  dev.off()
}

# Set the output directory path for saving PDFs
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Average_Model/GenConc"

# Read in and tidy data ----
# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

drivers_df <- read.csv("AllDrivers_Harmonized_Average.csv") %>%
  select(-contains("Yield"), -contains("FN")) %>%
  dplyr::mutate_at(vars(14:29), ~replace(., is.na(.), 0)) %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  distinct(Stream_ID, .keep_all = TRUE) %>%
  select(-Stream_ID) %>%
  drop_na()

# Remove outliers using custom function
# drivers_df <- remove_outlier_rows(drivers_df)

# Plot correlation between driver variables ----
numeric_drivers <- 2:34  # Indices for numeric drivers
driver_cor <- cor(drivers_df[, numeric_drivers])
corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = F)

pdf(sprintf("%s/correlation_plot.pdf", output_dir), width = 10, height = 10)
corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
title("Median Gen Si Concentration")  # Add title in the PDF
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
rf_model1 <- randomForest(median_GenConc ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 2000, mtry = 11)

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

control <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = cv_repeats, number = cv_number, seeds = seeds, verbose = TRUE)

# Divide data into predictor variables (x) and response variable (y)
x <- drivers_df[, !(colnames(drivers_df) == "median_GenConc")]
y <- drivers_df$median_GenConc

# Run RFE to select the best features ----
set.seed(123)
result_rfe <- rfe(x = x, y = y, sizes = c(1:size), rfeControl = control)

# Print RFE results
print(result_rfe)

# Put selected features into variable
new_rf_input <- paste(predictors(result_rfe), collapse = "+")

# Format those features into a formula for the optimized random forest model
rf_formula <- formula(paste("median_GenConc ~", new_rf_input))

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
tuneRF(kept_drivers, drivers_df[, 1], ntreeTry = 1000, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run optimized random forest model, with re-tuned ntree and mtry parameters ----
set.seed(123)
rf_model2 <- randomForest(rf_formula, data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 1000, mtry = 6)

# Visualize output for rf_model2
print(rf_model2)
randomForest::varImpPlot(rf_model2)

# Generate plots comparing predicted vs observed ----
lm_plot <- plot(rf_model2$predicted, drivers_df$median_GenConc, pch = 16, cex = 1.5,
                xlab = "Predicted", ylab = "Observed", main = "All Spatial Drivers - Gen Concentration",
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model2$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))

# Save RF variable importance plot and LM plot for rf_model2
save_rf_importance_plot(rf_model2, output_dir)
save_lm_plot(rf_model2, drivers_df$median_GenConc, output_dir)

## Calculate SHAP values and make exploratory plots 
# Set the output directory for plots
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Average_Model/GenConc"

# Set up a parallel backend for speed (adjust cores as needed)
num_cores <- detectCores() - 1  # Leave one core free for other tasks
registerDoParallel(cores = num_cores)

# Function to create shapley_plot_data without any subsetting
create_shapley_plot_data <- function(model, kept_drivers, drivers_df, sample_size = 30) {
  # Create the predictor object for the full dataset
  predictor <- Predictor$new(model = model, data = kept_drivers, y = drivers_df$median_GenConc)
  
  # Parallel Shapley calculations for each observation
  set.seed(123)  # For reproducibility
  shapley_list <- foreach(i = 1:nrow(kept_drivers), .packages = 'iml') %dopar% {
    shap <- Shapley$new(predictor, x.interest = kept_drivers[i, ], sample.size = sample_size)
    shap$results
  }
  
  # Combine Shapley results into a single dataframe
  shapley_df <- do.call(rbind, shapley_list) %>%
    mutate(row_id = rep(1:nrow(kept_drivers), each = ncol(kept_drivers)))
  
  # Reshape kept_drivers for merging with Shapley values
  kept_drivers_melted <- melt(kept_drivers)
  kept_drivers_melted$row_id <- rep(1:nrow(kept_drivers), ncol(kept_drivers))
  
  # Merge Shapley values with feature values and drivers_df metadata
  shapley_plot_data <- shapley_df %>%
    left_join(kept_drivers_melted, by = c("feature" = "variable", "row_id")) %>%
    left_join(drivers_df %>% mutate(row_id = 1:nrow(.)), by = "row_id")
  
  return(shapley_plot_data)
}

# Generate shapley_plot_data for the entire dataset
shapley_plot_data <- create_shapley_plot_data(rf_model2, kept_drivers, drivers_df)

create_all_shapley_plots <- function(shap_data, output_file, color_vars = NULL, log_scaled_drivers = c("drainage_area", "npp", "q_95", "q_5", "silicate_weathering")) {
  # Filter out silicate_weathering values > 20
  shap_data <- shap_data %>%
    filter(silicate_weathering <= 20)
  dev.off()
  
  # Open a single PDF file for all plots
  pdf(output_file, width = 8, height = 8)
  
  # 1. Overall Feature Importance Plot (based on mean absolute SHAP values)
  overall_feature_importance <- shap_data %>%
    group_by(feature) %>%
    summarise(importance = mean(abs(phi))) %>%
    arrange(desc(importance))
  
  overall_importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value", title = "Overall Feature Importance - Average Gen Concentration") +
    theme_minimal()
  
  # Print plot to the PDF
  print(overall_importance_plot)
  
  # Reorder feature levels for the SHAP Summary Plot based on importance
  shap_data <- shap_data %>%
    mutate(feature = factor(feature, levels = rev(overall_feature_importance$feature)))
  
  # 2. SHAP Summary Plot
  shap_data_normalized <- shap_data %>%
    group_by(feature) %>%
    mutate(normalized_value = (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE)))
  
  shap_summary_plot <- ggplot(shap_data_normalized, aes(x = phi, y = feature)) + 
    geom_point(aes(color = normalized_value), alpha = 0.6) + 
    scale_color_gradient(low = "blue", high = "red", name = "Feature Value", breaks = c(0, 1), labels = c("Low", "High")) +
    labs(x = "SHAP Value", y = NULL, title = "SHAP Summary Plot - Average Gen Concentration") + 
    theme_bw() + 
    theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), 
          plot.title = element_text(size = 16, face = "bold"))
  
  # Print plot to the PDF
  print(shap_summary_plot)
  
  # 3. SHAP Dependence Plots for each feature
  for (feature_name in unique(shap_data$feature)) {
    for (color_var in color_vars) {
      aes_mapping <- if (color_var %in% names(shap_data)) {
        aes(x = value, y = phi, color = .data[[color_var]])
      } else {
        aes(x = value, y = phi)
      }
      
      dependence_plot <- ggplot(shap_data[shap_data$feature == feature_name, ], aes_mapping) +
        geom_point(alpha = 0.6) + 
        labs(x = paste("Value of", feature_name), y = "SHAP Value", title = paste("Average Gen Concentration SHAP Dependence Plot for", feature_name)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add y=0 red dashed line
        theme_minimal()
      
      # Apply log scale to x-axis if the feature is in the log_scaled_drivers list
      if (feature_name %in% log_scaled_drivers) {
        dependence_plot <- dependence_plot + scale_x_log10() +
          labs(x = paste("log(", feature_name, ")", sep = ""))
      }
      
      # Apply log scale to the color scale if the color_var is in the log_scaled_drivers list
      if (color_var %in% log_scaled_drivers) {
        dependence_plot <- dependence_plot + scale_color_viridis_c(name = paste("log(", color_var, ")", sep = ""), trans = "log10")
      } else if (color_var %in% names(shap_data)) {
        dependence_plot <- dependence_plot + scale_color_viridis_c(name = color_var)
      }
      
      # Print plot to the PDF
      print(dependence_plot)
    }
  }
  
  # Close the PDF file
  dev.off()
}

# Run the function to create all plots in a single PDF file, coloring dependence plots by specified variables (e.g., "precip", "temp")
color_vars <- c("drainage_area", "snow_cover", "precip", 
                "evapotrans", "temp", "npp", "permafrost", "greenup_day",
                "rocks_volcanic", "NOx", "P", "max_daylength", "silicate_weathering", "q_95", "q_5")  # List of features to color by
output_file <- sprintf("%s/all_shapley_plots_Average_GenConc.pdf", output_dir)  # Specify the output file path
create_all_shapley_plots(shapley_plot_data, output_file, color_vars)


### Create new pdf of just single drivers colored by Si: 
single_dependence_output_file <- sprintf("%s/single_shap_dependence_plots_Average_GenConc.pdf", output_dir)

# Define the drivers to apply log scaling
log_scaled_drivers <- c("drainage_area", "npp", "q_95", "q_5", "silicate_weathering")  # Customize the list as needed

create_single_shap_dependence_plots <- function(shap_data, output_file, log_scaled_drivers = c("drainage_area", "npp", "q_95", "q_5")) {
  # Open a new PDF file for the SHAP dependence plots
  pdf(output_file, width = 8, height = 8)
  
  # Loop through each unique feature in the SHAP data
  for (feature_name in unique(shap_data$feature)) {
    # Filter data for the current feature
    feature_data <- shap_data[shap_data$feature == feature_name, ]
    
    # Determine the x-axis label
    x_label <- if (feature_name %in% log_scaled_drivers) {
      paste("log(", feature_name, ")", sep = "")
    } else {
      paste("Value of", feature_name)
    }
    
    # Create the dependence plot
    dependence_plot <- ggplot(feature_data, aes(x = value, y = phi, color = median_GenConc)) +
      geom_point(alpha = 0.6) +
      scale_color_viridis_c(name = "Median GenConc") +
      labs(
        x = x_label,  # Use the custom x_label here
        y = "SHAP Value",
        title = paste("SHAP Dependence Plot for", feature_name)
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add y=0 red dashed line
      theme_minimal()
    
    # Apply log scale if the feature is in the log_scaled_drivers list
    if (feature_name %in% log_scaled_drivers) {
      dependence_plot <- dependence_plot + scale_x_log10()
    }
    
    # Print the plot to the PDF
    print(dependence_plot)
  }
  
  # Close the PDF
  dev.off()
}




# Call the function to generate the single SHAP dependence plots
create_single_shap_dependence_plots(
  shap_data = shapley_plot_data,       # Your SHAP data
  output_file = single_dependence_output_file, # Output file path
  log_scaled_drivers = log_scaled_drivers      # Drivers to log scale
)



# BASEMENT -------------------

# # Define plot parameters for specific features
# plot_parameters <- list(
#   list(var = "P", threshold = 0.1, above_threshold = FALSE, color_var = "precip"),
#   list(var = "P", threshold = 0.1, above_threshold = TRUE, color_var = "precip"),
#   list(var = "max_daylength", threshold = 17, above_threshold = FALSE, color_var = "precip"),
#   list(var = "max_daylength", threshold = 17, above_threshold = TRUE, color_var = "precip"),
#   list(var = "land_shrubland_grassland", threshold = 50, above_threshold = FALSE, color_var = "precip"),
#   list(var = "land_shrubland_grassland", threshold = 50, above_threshold = TRUE, color_var = "precip"),
#   list(var = "silicate_weathering", threshold = 0.25, above_threshold = FALSE, color_var = "precip"),
#   list(var = "silicate_weathering", threshold = 0.25, above_threshold = TRUE, color_var = "precip")
# )
# 
# 
# 
# # Generate plots based on specified plot parameters
# for (params in plot_parameters) {
#   subset_shapley_plot(
#     shap_data = shapley_plot_data,
#     feature_name = params$var,
#     threshold = params$threshold,
#     above_threshold = params$above_threshold,
#     color_var = params$color_var,
#     output_dir = output_dir
#   )
# }
# 


