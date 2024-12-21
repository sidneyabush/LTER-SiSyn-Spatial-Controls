# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, randomForest, tibble, viridis)

# Clear environment
rm(list = ls())

# Set working directory                 
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Load required data and model from the RF script
load("FNConc_Yearly_rf_model2_noWeathering.RData")
load("FNConc_Yearly_kept_drivers_noWeathering.RData")
load("FNConc_Yearly_drivers_df_noWeathering.RData")

# Set global seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

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
output_file <- sprintf("%s/FNConc_Yearly_Overall_SHAP_Variable_Importance_noWeathering.pdf", output_dir)
create_all_shapley_plots(shap_values, output_file)

# Function to create SHAP-based partial dependence plots
create_shap_partial_dependence_plots <- function(shap_values, kept_drivers, drivers_df, output_dir, color_var = "FNConc") {
  # Check if the specified coloring variable exists in drivers_df
  if (!(color_var %in% colnames(drivers_df))) {
    stop(paste("The specified color_var:", color_var, "is not in the drivers_df dataframe."))
  }
  
  # Open a PDF to save all SHAP partial dependence plots
  pdf(file = file.path(output_dir, "FNConc_Yearly_SHAP_Partial_Dependence_Plots_noWeathering.pdf"), width = 8, height = 8)
  
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
  color_var = "FNConc"
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
    output_file <- file.path(output_dir, paste0("SHAP_Variable_Importance_", condition_column, "_", operator, "_", condition_value, "_Excluding_Subset_Driver_noWeathering.pdf"))
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
