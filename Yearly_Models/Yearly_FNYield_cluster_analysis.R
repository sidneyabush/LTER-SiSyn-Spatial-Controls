# Load necessary libraries
librarian::shelf(ggplot2, dplyr, tidyr, factoextra, cluster)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

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

# Read in and preprocess the data
# train <- read.csv("train_data_stream_id.csv")
load("FNYield_Yearly_kept_drivers_full.RData")
load("FNYield_Yearly_full_stream_ids.RData")
load("FNYield_Yearly_rf_model2_full.RData")

data <- kept_drivers

data <- data %>%
  dplyr::select("rocks_volcanic", "basin_slope", "land_shrubland_grassland", "npp", "land_forest_all")

# Scale the selected numerical columns 
scaled_data <- data %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))


# Set seed for reproducibility
set.seed(123)

# Perform silhouette method to determine optimal clusters
p2 <- fviz_nbclust(scaled_data, kmeans, method= "silhouette", k.max = 20)
p2

kmeans_result <- kmeans(scaled_data, iter.max = 50, nstart = 50, centers = 4)

# Add cluster assignments to the reg data
final_data <- data %>%
  mutate(cluster = as.factor(kmeans_result$cluster)) %>%
  dplyr::select(cluster)

# Save to CSV file
# write.csv(final_data, "cluster_assignments_YearlyModel.csv", row.names = FALSE)

scaled_data <- scaled_data %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

# Define a colorblind-friendly palette
cb_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

# Reshape data to long format for ggplot
long_data <- scaled_data %>%
  pivot_longer(-cluster, names_to = "Driver", values_to = "Value") %>%
  mutate(
    Driver = factor(Driver, levels = c("rocks_volcanic", "basin_slope", "land_shrubland_grassland", "npp", "land_forest_all")),
    Driver = recode(Driver, 
                    "rocks_volcanic" = "Volcanic Rock",
                    "basin_slope" = "Basin Slope",
                    "land_shrubland_grassland" = "Land: Shrubland/Grassland",
                    "npp" = "NPP",
                    "land_forest_all" = "Land: Forest"
    )
  )


box_plot <- ggplot(long_data, aes(x = Driver, y = Value, fill = cluster)) +
  geom_boxplot() +
  facet_wrap(~cluster, ncol = 2, scales = "free") +  
  scale_fill_manual(values = cb_palette) +  # Apply colorblind-friendly colors
  labs(title = "FNYield Yearly", x = NULL, y = "Scaled Value") +
  coord_cartesian(ylim = c(-2, 10)) + # Set Y-axis limits without removing data
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate x-axis labels
    axis.text.y = element_text(size = 14),  # Rotate x-axis labels
    strip.text = element_text(size = 14, face = "bold"), 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center & bold title
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Add panel borders
    panel.spacing = unit(1, "lines"),  # Ensure spacing between facets
    axis.title = element_text(size = 14, face = "bold"))  

print(box_plot)

ggsave(
  filename = "FNYield_Yearly_Cluster_Drivers_Boxplot.png",
  plot = box_plot,
  width = 10,
  height = 10,
  dpi = 300,
  path = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"
)


# Compute silhouette scores
sil <- silhouette(kmeans_result$cluster, dist(scaled_data %>% select(-cluster), method = "euclidean")^2)

# Create silhouette plot
sil_plot <- fviz_silhouette(sil) +
  labs(title = "FNYield Yearly", y = "Silhouette Width", x = "Sites") +
  theme_classic() +
  scale_fill_manual(values = cb_palette) +  # Ensure consistent colors
  scale_color_manual(values = cb_palette) +  # Apply the same colors to silhouette plot
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold"),  # Enlarge facet labels
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
    axis.title = element_text(size = 14, face = "bold"))  

print(sil_plot)

ggsave(
  filename = "FNYield_Yearly_Cluster_SilPlot.png",
  plot = sil_plot,
  width = 6,
  height = 4,
  dpi = 300,
  path = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"
)


# Select only Stream_ID and FNYield from drivers_df
drivers_subset <- drivers_df %>% select(Stream_ID, FNYield)

# Merge clusters with kept_drivers (ensuring row alignment)
all_data <- bind_cols(kept_drivers, drivers_subset)

# Merge with final_data
all_data <- bind_cols(all_data, final_data)

# Ensure 'cluster' is a factor
all_data$cluster <- as.factor(all_data$cluster)

# Create a boxplot with the custom color palette
dist <- ggplot(all_data, aes(x = cluster, y = FNYield, fill = cluster)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(alpha = 0.3, width = 0.2) +  # Add individual points
  scale_fill_manual(values = cb_palette) +  # Apply custom color palette
  labs(title = "FNYield Yearly",
       x = "Cluster",
       y = "FNYield") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),  # Rotate x-axis labels
    strip.text = element_text(size = 14, face = "bold"),  # Enlarge facet labels
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"))  

print(dist)

ggsave(
  filename = "FNYield_Yearly_Cluster_Boxplot.png",
  plot = dist,
  width = 6,
  height = 4,
  dpi = 300,
  path = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"
)

# Save  so we can look at Stream_ID and distribution later
write.csv(all_data, file= "Yearly_FNYield_Cluster_Stream_ID.csv")

# Now remove FNYield and Stream_ID columns to put into SHAP analysis
combined_data <- all_data %>%
  dplyr::select(-FNYield, -Stream_ID)

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
  
  # Ensure clusters are sorted so they are assigned the correct colors
  sorted_clusters <- sort(unique(combined_data$cluster))
  
  # Map cluster ID to the corresponding color in order
  cluster_index <- match(cluster_id, sorted_clusters)
  cluster_color <- cb_palette[cluster_index]
  
  # Save the feature importance plot
  output_file <- sprintf("%s/SHAP_FNYield_Ave_Cluster_%s_Variable_Importance.pdf", output_dir, cluster_id)
  
  pdf(output_file, width = 10, height = 8)
  cluster_importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = cluster_color) +  # Use a single color for all bars per cluster
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value",
         title = paste("FNYield Yearly - Feature Importance for Cluster", cluster_id)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 14),  # Increase x-axis text size
      axis.text.y = element_text(size = 14),  # Increase y-axis (feature labels) text size
      axis.title.x = element_text(size = 16, face = "bold"),  # Increase and bold x-axis title
      axis.title.y = element_text(size = 16, face = "bold"),  # Increase and bold y-axis title
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)  # Make title larger & centered
    )
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
# write.csv(shap_importance_summary, file = sprintf("%s/SHAP_FNYield_Ave_Cluster_Importance_Summary.csv", output_dir), row.names = FALSE)

message("SHAP importance analysis per cluster completed and saved.")
