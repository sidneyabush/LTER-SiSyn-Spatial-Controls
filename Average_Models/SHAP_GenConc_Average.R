# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, randomForest, tibble, viridis)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Load required data and model from the RF script
load("GenConc_Average_rf_model2_noWeathering.RData")
load("GenConc_Average_kept_drivers_noWeathering.RData")
load("GenConc_Average_train_noWeathering.RData")

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

# Function to create SHAP-based partial dependence plots
create_shap_partial_dependence_plots <- function(shap_values, kept_drivers, train, output_dir, color_var = "GenConc") {
  # Check if the specified coloring variable exists in train
  if (!(color_var %in% colnames(train))) {
    stop(paste("The specified color_var:", color_var, "is not in the train dataframe."))
  }
  
  # Open a PDF to save all SHAP partial dependence plots
  pdf(file = file.path(output_dir, "SHAP_GenConc_Ave_Partial_Dependence_Plots_noWeathering.pdf"), width = 8, height = 8)
  
  # Loop through each feature in shap_values
  for (feature in colnames(shap_values)) {
    # Extract SHAP values for the current feature
    shap_long <- tibble::tibble(
      feature_value = kept_drivers[[feature]],  # Feature values from kept_drivers
      shap_value = shap_values[, feature],     # SHAP values from shap_values (matrix indexing)
      color_value = train[[color_var]]    # Coloring variable
    )
    
    # Add the log scale for select drivers
    log_scaled_drivers <- c("drainage_area", "q_5", "evapotrans", "silicate_weathering")
    
    # Create the SHAP-based partial dependence plot
    shap_pdp_plot <- ggplot(shap_long, aes(x = feature_value, y = shap_value, color = color_value)) +
      geom_point(alpha = 0.6) +
      scale_color_viridis_c(name = color_var) +
      geom_hline(yintercept = 0, color = "darkred", linetype = "dashed", size = 1) +
      labs(
        title = paste("Ave GenConc SHAP Partial Dependence Plot for", feature),
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
  train = train,
  output_dir = output_dir,
  color_var = "GenConc"
)

# Now we're going to import the cluster data: 
# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

# Read in and preprocess the data
clusters <- read.csv("cluster_assignments_AverageModel.csv")%>%
  dplyr::select(cluster)

# Merge clusters with kept_drivers (ensuring row alignment)
genconc_clusters <- bind_cols(train, clusters)

# Define a colorblind-friendly palette
cb_palette <- c(
  "#E69F00",  # Orange
  "#56B4E9",  # Sky Blue
  "#009E73",  # Green
  "#D55E00",  # Red
  "#CC79A7"   # Pink
)

# Ensure 'cluster' is a factor
genconc_clusters$cluster <- as.factor(genconc_clusters$cluster)

# Create a boxplot with the custom color palette
ggplot(genconc_clusters, aes(x = cluster, y = GenConc, fill = cluster)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(alpha = 0.3, width = 0.2) +  # Add individual points
  scale_fill_manual(values = cb_palette) +  # Apply custom color palette
  labs(title = "Average Model",
       x = "Cluster",
       y = "GenConc") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10),  # Rotate x-axis labels
    strip.text = element_text(size = 12, face = "bold"),  # Enlarge facet labels
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")  # Center & bold title
  )

# Merge clusters with kept_drivers (ensuring row alignment)
combined_data <- bind_cols(kept_drivers, clusters)

# Ensure 'cluster' is a factor
combined_data$cluster <- as.factor(combined_data$cluster)

generate_shap_plots_for_cluster <- function(cluster_id, model, combined_data, output_dir, sample_size = 30) {
  # Filter data for the specific cluster and exclude non-predictor variables
  cluster_data <- combined_data %>% filter(cluster == cluster_id) %>% select(-cluster)  
  
  if (nrow(cluster_data) == 0) {
    message(paste("Skipping cluster", cluster_id, "as it has no data"))
    return(NULL)
  }
  
  # Generate SHAP values
  shap_values <- generate_shap_values(model, cluster_data, sample_size)
  
  # Calculate overall feature importance for the cluster
  overall_feature_importance <- shap_values %>%
    as.data.frame() %>%
    summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  # Define a colorblind-friendly palette in order
  cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")
  
  # Ensure clusters are sorted so they are assigned the correct colors
  sorted_clusters <- sort(unique(combined_data$cluster))
  
  # Map cluster ID to the corresponding color in order
  cluster_index <- match(cluster_id, sorted_clusters)
  cluster_color <- cb_palette[cluster_index]
  
  # Save the feature importance plot
  output_file <- sprintf("%s/SHAP_GenConc_Ave_Cluster_%s_Variable_Importance.pdf", output_dir, cluster_id)
  
  pdf(output_file, width = 8, height = 8)
  cluster_importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = cluster_color) +  # Use a single color for all bars per cluster
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value",
         title = paste("GenConc Average - Feature Importance for Cluster", cluster_id)) +
    theme_minimal()
  print(cluster_importance_plot)
  dev.off()
  
  return(overall_feature_importance)
}

# Ensure clusters are in correct order before running the function
unique_clusters <- sort(unique(combined_data$cluster))

# Generate SHAP values and plots for each cluster in correct order
shap_importance_by_cluster <- lapply(unique_clusters, generate_shap_plots_for_cluster, 
                                     model = rf_model2, combined_data = combined_data, output_dir = output_dir, sample_size = 30)

# Combine results into a single dataframe
shap_importance_summary <- bind_rows(shap_importance_by_cluster, .id = "cluster")

# Save the summary as a CSV
write.csv(shap_importance_summary, file = sprintf("%s/SHAP_GenConc_Ave_Cluster_Importance_Summary.csv", output_dir), row.names = FALSE)

message("SHAP importance analysis per cluster completed and saved.")

# Basement -----
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
