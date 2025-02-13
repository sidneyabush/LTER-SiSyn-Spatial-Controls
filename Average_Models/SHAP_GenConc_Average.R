# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, randomForest, tibble, viridis)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Load required data and model from the RF script
load("GenConc_Average_rf_model2_full.RData")
load("GenConc_Average_kept_drivers_full.RData")
load("GenConc_Average_full.RData")

# Set global seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Average_Model/GenConc"

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
    labs(x = "Feature", y = "Mean Absolute SHAP Value", title = "GenConc Average - Overall Feature Importance") +
    theme_minimal()
  print(overall_importance_plot)
  dev.off()
}

# Output full SHAP plots
output_file <- sprintf("%s/SHAP_GenConc_Ave_Overall_Variable_Importance.pdf", output_dir)
create_all_shapley_plots(shap_values, output_file)


# Basement -----
# # Function to create SHAP-based partial dependence plots
# create_shap_partial_dependence_plots <- function(shap_values, kept_drivers, train, output_dir, color_var = "GenConc") {
#   # Check if the specified coloring variable exists in train
#   if (!(color_var %in% colnames(train))) {
#     stop(paste("The specified color_var:", color_var, "is not in the train dataframe."))
#   }
#   
#   # Open a PDF to save all SHAP partial dependence plots
#   pdf(file = file.path(output_dir, "SHAP_GenConc_Ave_Partial_Dependence_Plots_noWeathering.pdf"), width = 8, height = 8)
#   
#   # Loop through each feature in shap_values
#   for (feature in colnames(shap_values)) {
#     # Extract SHAP values for the current feature
#     shap_long <- tibble::tibble(
#       feature_value = kept_drivers[[feature]],  # Feature values from kept_drivers
#       shap_value = shap_values[, feature],     # SHAP values from shap_values (matrix indexing)
#       color_value = train[[color_var]]    # Coloring variable
#     )
#     
#     # Add the log scale for select drivers
#     log_scaled_drivers <- c("drainage_area", "q_5", "evapotrans", "silicate_weathering")
#     
#     # Create the SHAP-based partial dependence plot
#     shap_pdp_plot <- ggplot(shap_long, aes(x = feature_value, y = shap_value, color = color_value)) +
#       geom_point(alpha = 0.6) +
#       scale_color_viridis_c(name = color_var) +
#       geom_hline(yintercept = 0, color = "darkred", linetype = "dashed", size = 1) +
#       labs(
#         title = paste("Ave GenConc SHAP Partial Dependence Plot for", feature),
#         x = ifelse(feature %in% log_scaled_drivers, paste("log(", feature, ")", sep = ""), paste("Value of", feature)),
#         y = "SHAP Value"
#       ) +
#       theme_minimal() +
#       theme(plot.title = element_text(size = 16, face = "bold")) +
#       if (feature %in% log_scaled_drivers) scale_x_log10()
#     
#     # Print the plot to the PDF
#     print(shap_pdp_plot)
#   }
#   
#   # Close the PDF file
#   dev.off()
#   message("SHAP partial dependence plots saved to PDF.")
# }
# 
# # Generate SHAP-based partial dependence plots
# create_shap_partial_dependence_plots(
#   shap_values = shap_values,
#   kept_drivers = kept_drivers,
#   train = train,
#   output_dir = output_dir,
#   color_var = "GenConc"
# )
# 
# # This section is used to make subset importance plots (subsetting different drivers based on PDPs)
# create_subset_importance_plots <- function(shap_values, conditions, kept_drivers, output_dir) {
#   for (condition in conditions) {
#     condition_column <- condition$column
#     condition_value <- condition$value
#     operator <- condition$operator
#     
#     # Skip conditions for features not in kept_drivers
#     if (!condition_column %in% colnames(kept_drivers)) {
#       message(paste("Skipping condition:", condition_column, "not found in kept_drivers."))
#       next
#     }
#     
#     # Filter kept_drivers based on the condition (keeping all columns for now)
#     filtered_drivers <- kept_drivers %>%
#       filter(case_when(
#         operator == ">" ~ .data[[condition_column]] > condition_value,
#         operator == "<" ~ .data[[condition_column]] < condition_value,
#         operator == "=" ~ .data[[condition_column]] == condition_value,
#         TRUE ~ FALSE
#       ))
#     
#     row_indices <- which(rownames(kept_drivers) %in% rownames(filtered_drivers))
#     subset_shap_values <- shap_values[row_indices, , drop = FALSE]
#     
#     # Now safely remove the filtering column from both filtered drivers & SHAP values
#     filtered_drivers <- filtered_drivers %>% select(-all_of(condition_column))
#     subset_shap_values <- as.data.frame(subset_shap_values) %>% select(-all_of(condition_column), everything())
#     
#     # Summarize feature importance
#     subset_importance <- subset_shap_values %>%
#       summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
#       pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
#       arrange(desc(importance))
#     
#     # Create the importance plot
#     subset_importance_plot <- ggplot(subset_importance, aes(x = reorder(feature, importance), y = importance)) +
#       geom_bar(stat = "identity", fill = "steelblue") +
#       coord_flip() +
#       labs(
#         title = paste("GenConc Average", condition_column, operator, condition_value),
#         x = "Feature",
#         y = "Mean Absolute SHAP Value"
#       ) +
#       theme_minimal()
#     
#     # Save the plot
#     output_file <- file.path(output_dir, paste0("SHAP_GenConc_Average_noWeathering", condition_column, "_", operator, "_", condition_value, ".pdf"))
#     ggsave(output_file, plot = subset_importance_plot, width = 8, height = 6)
#     
#     message(paste("Subset variable importance plot saved:", output_file))
#   }
# }
# 
# # Define multiple flexible conditions for subsetting
# conditions <- list(
#   list(column = "rocks_volcanic", value = 50, operator = ">"),
#   list(column = "land_shrubland_grassland", value = 50, operator = ">"),
#   list(column = "land_shrubland_grassland", value = 50, operator = "<"),
#   list(column = "snow_cover", value = 0.4, operator = "<"),
#   list(column = "snow_cover", value = 0.4, operator = ">")
# )
# 
# # Retain only conditions relevant to kept_drivers
# valid_conditions <- lapply(conditions, function(cond) {
#   if (cond$column %in% colnames(kept_drivers)) {
#     return(cond)
#   } else {
#     message(paste("Skipping condition for non-kept feature:", cond$column))
#     return(NULL)
#   }
# })
# valid_conditions <- Filter(Negate(is.null), valid_conditions)  # Remove NULLs
# 
# # Generate importance plots for valid subsets
# create_subset_importance_plots(
#   shap_values = shap_values,
#   conditions = valid_conditions,
#   kept_drivers = kept_drivers,
#   output_dir = output_dir
# )
# 
# # Define multiple flexible conditions for subsetting
# conditions <- list(
#   list(column = "rocks_volcanic", value = 50, operator = ">"),
#   list(column = "land_shrubland_grassland", value = 50, operator = ">"),
#   list(column = "land_shrubland_grassland", value = 50, operator = "<"),
#   list(column = "snow_cover", value = 0.4, operator = "<"),
#   list(column = "snow_cover", value = 0.4, operator = ">")
# )
# 
# # Retain only conditions relevant to kept_drivers
# valid_conditions <- lapply(conditions, function(cond) {
#   if (cond$column %in% colnames(kept_drivers)) {
#     return(cond)
#   } else {
#     message(paste("Skipping condition for non-kept feature:", cond$column))
#     return(NULL)
#   }
# })
# valid_conditions <- Filter(Negate(is.null), valid_conditions)  # Remove NULLs
# 
# # Generate importance plots for valid subsets
# create_subset_importance_plots(
#   shap_values = shap_values,
#   conditions = valid_conditions,
#   kept_drivers = kept_drivers,
#   output_dir = output_dir
# )
# 
# # Define a threshold
# threshold <- 0.33
# 
# # Calculate how many sites have a majority land use for each type
# result <- train %>%
#   dplyr::select(starts_with("land_")) %>% # Select columns starting with "land_"
#   mutate(majority_land_use = apply(., 1, function(row) {
#     colnames(.)[which.max(row)] # Get the column name with the maximum value
#   })) %>%
#   filter(apply(., 1, max) > threshold) %>% # Keep rows where the maximum value is above the threshold
#   count(majority_land_use) # Count the number of occurrences of each land use type
# 
# # View the result
# print(result)
# 
