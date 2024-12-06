# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach)

# Clear environment
rm(list = ls())

# Load required data and model from the RF script
load("GenConc_Ave_rf_model2.RData")
load("GenConc_Ave_kept_drivers.RData")
load("GenConc_Ave_drivers_df.RData")

# Set global seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Average_Model/GenConc"

# SHAP Analysis
create_shapley_plot_data <- function(model, kept_drivers, drivers_df, sample_size = 30) {
  predictor <- Predictor$new(model = model, data = kept_drivers, y = drivers_df$median_GenConc)
  shapley_list <- foreach(i = 1:nrow(kept_drivers), .packages = 'iml') %dopar% {
    shap <- Shapley$new(predictor, x.interest = kept_drivers[i, ], sample.size = sample_size)
    shap$results
  }
  shapley_df <- do.call(rbind, shapley_list) %>%
    mutate(row_id = rep(1:nrow(kept_drivers), each = ncol(kept_drivers)))
  kept_drivers_melted <- melt(kept_drivers)
  kept_drivers_melted$row_id <- rep(1:nrow(kept_drivers), ncol(kept_drivers))
  shapley_plot_data <- shapley_df %>%
    left_join(kept_drivers_melted, by = c("feature" = "variable", "row_id")) %>%
    left_join(drivers_df %>% mutate(row_id = 1:nrow(.)), by = "row_id")
  return(shapley_plot_data)
}

shapley_plot_data <- create_shapley_plot_data(rf_model2, kept_drivers, drivers_df)

# Function to create overall SHAP plots for the full model
create_all_shapley_plots <- function(shap_data, output_file) {
  pdf(output_file, width = 8, height = 8)
  
  # Overall Feature Importance Plot
  overall_feature_importance <- shap_data %>%
    group_by(feature) %>%
    summarise(importance = mean(abs(phi))) %>%
    arrange(desc(importance))
  
  overall_importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value", title = "Overall Feature Importance") +
    theme_minimal()
  
  print(overall_importance_plot)
  dev.off()
}

# Output full SHAP plots
output_file <- sprintf("%s/GenConc_Ave_Overall_SHAP_Variable_Importance.pdf", output_dir)
create_all_shapley_plots(shapley_plot_data, output_file)

create_shap_partial_dependence_plots <- function(shap_data, drivers_df, output_dir, color_var = "median_GenConc") {
  # Check if the specified coloring variable exists in drivers_df
  if (!(color_var %in% colnames(drivers_df))) {
    stop(paste("The specified color_var:", color_var, "is not in the drivers_df dataframe."))
  }
  
  # Open a PDF to save all SHAP partial dependence plots
  pdf(file = file.path(output_dir, "GenConc_Ave_SHAP_Partial_Dependence_Plots.pdf"), width = 8, height = 8)
  
  # Loop through each unique feature in the SHAP data
  for (driver in unique(shap_data$feature)) {
    # Filter SHAP data for the current driver
    driver_data <- shap_data %>% filter(feature == driver)
    
    # Add the color_var to driver_data
    driver_data <- driver_data %>%
      mutate(color_value = drivers_df[[color_var]][row_id])
    
    # Add the log scale for select drivers
    log_scaled_drivers <- c("drainage_area", "q_5", "evapotrans", "silicate_weathering")
    
    # Create the SHAP-based partial dependence plot
    shap_pdp_plot <- ggplot(driver_data, aes(x = value, y = phi, color = color_value)) +
      geom_point(alpha = 0.6) +
      scale_color_viridis_c(name = color_var) +
      geom_hline(yintercept = 0, color = "darkred", linetype = "dashed", size = 1) +  # Add a dark red dashed line at y = 0
      labs(
        title = paste("SHAP Partial Dependence Plot for", driver),
        x = ifelse(driver %in% log_scaled_drivers, paste("log(", driver, ")", sep = ""), paste("Value of", driver)),
        y = "SHAP Value"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold")) +
      # Apply log scale if the driver is in log_scaled_drivers
      if (driver %in% log_scaled_drivers) {
        scale_x_log10()
      }
    
    # Print the plot to the PDF
    print(shap_pdp_plot)
  }
  
  # Close the PDF file
  dev.off()
  message("SHAP partial dependence plots saved to PDF.")
}


# Generate SHAP-based partial dependence plots with custom coloring
create_shap_partial_dependence_plots(
  shap_data = shapley_plot_data,
  drivers_df = drivers_df,
  output_dir = output_dir,
  color_var = "drainage_area"  # Example: Color points by "drainage_area"
)

# Generate SHAP-based partial dependence plots with default coloring
create_shap_partial_dependence_plots(
  shap_data = shapley_plot_data,
  drivers_df = drivers_df,
  output_dir = output_dir
)


# Function to create variable importance plots for multiple flexible subsets
create_subset_importance_plots <- function(shap_data, conditions, output_dir) {
  for (condition in conditions) {
    condition_column <- condition$column
    condition_value <- condition$value
    operator <- condition$operator
    
    # Filter the shap_data based on the condition
    subset_data <- shap_data %>%
      filter(case_when(
        operator == ">" ~ .data[[condition_column]] > condition_value,
        operator == "<" ~ .data[[condition_column]] < condition_value,
        operator == "=" ~ .data[[condition_column]] == condition_value,
        TRUE ~ FALSE
      ))
    
    # Exclude the condition column from importance calculation
    subset_importance <- subset_data %>%
      filter(feature != condition_column) %>%  # Remove rows for the condition column
      group_by(feature) %>%
      summarise(importance = mean(abs(phi), na.rm = TRUE)) %>%
      arrange(desc(importance))
    
    # Create the importance plot
    subset_importance_plot <- ggplot(subset_importance, aes(x = reorder(feature, importance), y = importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste("Variable Importance for", condition_column, operator, condition_value),
        x = "Feature",
        y = "Mean Absolute SHAP Value"
      ) +
      theme_minimal()
    
    # Save the plot
    output_file <- file.path(output_dir, paste0("SHAP_Variable_Importance_", condition_column, "_", operator, "_", condition_value, ".pdf"))
    ggsave(output_file, plot = subset_importance_plot, width = 8, height = 6)
    message(paste("Subset variable importance plot saved:", output_file))
  }
}

# Define multiple flexible conditions for subsetting
conditions <- list(
  list(column = "rocks_volcanic", value = 50, operator = ">"),
  list(column = "land_shrubland_grassland", value = 50, operator = ">"),
  list(column = "drainage_area", value = 1000, operator = "<"),
  list(column = "NOx", value = 0.1, operator = ">")  
)

# Generate importance plots for all subsets
create_subset_importance_plots(
  shap_data = shapley_plot_data,
  conditions = conditions,
  output_dir = output_dir
)
