# Load necessary libraries
librarian::shelf(ggplot2, dplyr, tidyr, factoextra, cluster, colorspace)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

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

# Read in and preprocess the data
# train <- read.csv("train_data_stream_id.csv")
load("FNConc_Yearly_kept_drivers_full.RData")
load("FNConc_Yearly_full.RData")
load("FNConc_Yearly_full_stream_ids.RData")
load("FNConc_Yearly_rf_model2_full.RData")

data <- kept_drivers

data <- data %>%
  dplyr::select("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")

# Scale the selected numerical columns 
scaled_data <- data %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))


# Set seed for reproducibility
set.seed(123)

# Perform silhouette method to determine optimal clusters
p2 <- fviz_nbclust(scaled_data, kmeans, method= "silhouette", k.max = 20)
print(p2)

kmeans_result <- kmeans(scaled_data, iter.max = 50, nstart = 50, centers = 3)

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
    Driver = factor(Driver, levels = c("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")),
    Driver = recode(Driver, 
                    "elevation" = "Elevation",
                    "basin_slope" = "Basin Slope",
                    "P" = "P",
                    "rocks_volcanic" = "Volcanic Rock",
                    "evapotrans" = "Evapotrans"
    )
  )


box_plot <- ggplot(long_data, aes(x = Driver, y = Value, fill = cluster)) +
  geom_boxplot() +
  facet_wrap(~cluster, ncol = 2, scales = "free") +  
  scale_fill_manual(values = cb_palette) +  # Apply colorblind-friendly colors
  labs(title = "FNConc Yearly", x = NULL, y = "Scaled Value") +
  coord_cartesian(ylim = c(-1, 5)) + # Set Y-axis limits without removing data
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
  filename = "FNConc_Yearly_Cluster_Drivers_Boxplot.png",
  plot = box_plot,
  width = 10,
  height = 10,
  dpi = 300,
  path = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"
)


# Compute silhouette scores
sil <- silhouette(kmeans_result$cluster, dist(scaled_data %>% select(-cluster), method = "euclidean")^2)

# Create silhouette plot
sil_plot <- fviz_silhouette(sil) +
  labs(title = "FNConc Yearly", y = "Silhouette Width", x = "Sites") +
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
  filename = "FNConc_Yearly_Cluster_SilPlot.png",
  plot = sil_plot,
  width = 6,
  height = 4,
  dpi = 300,
  path = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"
)


# Select only Stream_ID and FNConc from drivers_df
drivers_subset <- drivers_df %>% select(Stream_ID, FNConc)

# Merge clusters with kept_drivers (ensuring row alignment)
all_data <- bind_cols(kept_drivers, drivers_subset)

# Merge with final_data
all_data <- bind_cols(all_data, final_data)

# Ensure 'cluster' is a factor
all_data$cluster <- as.factor(all_data$cluster)

# Create a boxplot with the custom color palette
dist <- ggplot(all_data, aes(x = cluster, y = FNConc, fill = cluster)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(alpha = 0.3, width = 0.2) +  # Add individual points
  scale_fill_manual(values = cb_palette) +  # Apply custom color palette
  labs(title = "FNConc Yearly",
       x = "Cluster",
       y = "FNConc") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),  # Rotate x-axis labels
    strip.text = element_text(size = 14, face = "bold"),  # Enlarge facet labels
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"))  

print(dist)

ggsave(
  filename = "FNConc_Yearly_Cluster_Boxplot.png",
  plot = dist,
  width = 6,
  height = 4,
  dpi = 300,
  path = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"
)

# Save  so we can look at Stream_ID and distribution later
write.csv(all_data, file= "Yearly_FNConc_Cluster_Stream_ID.csv")

# Now remove FNConc and Stream_ID columns to put into SHAP analysis
combined_data <- all_data %>%
  dplyr::select(-Stream_ID)

generate_shap_plots_for_cluster <- function(cluster_id, model, combined_data, sample_size = 30) {
  # Filter data for the specific cluster and exclude non-predictor variables
  cluster_data <- combined_data %>% filter(cluster == cluster_id) %>% select(-cluster)
  
  if (nrow(cluster_data) == 0) {
    message(paste("Skipping cluster", cluster_id, "as it has no data"))
    return(NULL)
  }
  
  # Generate SHAP values for the cluster
  shap_values <- generate_shap_values(model, cluster_data, sample_size)
  
  # Compute overall feature importance
  overall_feature_importance <- shap_values %>%
    as.data.frame() %>%
    summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))  # Order by absolute mean SHAP value
  
  # Ensure clusters are sorted for color consistency
  sorted_clusters <- sort(unique(combined_data$cluster))
  cluster_index <- match(cluster_id, sorted_clusters)
  cluster_base_color <- cb_palette[cluster_index]  # Assign base cluster color
  
  # Generate lighter and darker shades of the cluster color
  cluster_light <- lighten(cluster_base_color, amount = 0.5)  # Lighter version
  cluster_dark <- darken(cluster_base_color, amount = 0.5)  # Darker version
  
  ### **Prepare Data for Faceted Plot**
  
  # Prepare feature importance data for faceting
  importance_data <- overall_feature_importance %>%
    mutate(cluster = paste("Cluster", cluster_id),  # Label for faceting
           plot_type = "Feature Importance",  # Distinguish bar plot in faceting
           cluster_color = cluster_base_color)  # Store cluster color
  
  # Scale feature values within the cluster
  cluster_data_scaled <- as.data.frame(scale(as.matrix(cluster_data)))  # Per-cluster scaling
  
  # Convert to long format
  cluster_data_long <- cluster_data_scaled %>%
    mutate(id = seq_len(nrow(.))) %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  # Convert SHAP values to long format
  shap_values_df <- as.data.frame(shap_values) %>%
    mutate(id = seq_len(nrow(.)))
  
  shap_long <- shap_values_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_data_long, by = c("id", "feature")) %>%
    mutate(feature = factor(feature, levels = rev(overall_feature_importance$feature)),  # Match importance order
           cluster = paste("Cluster", cluster_id),  # Label for faceting
           plot_type = "SHAP Values",  # Distinguish dot plot in faceting
           cluster_light = cluster_light,  # Store light shade
           cluster_dark = cluster_dark)  # Store dark shade
  
  return(list(importance_data = importance_data, shap_data = shap_long))
}

unique_clusters <- sort(unique(combined_data$cluster))

# Collect importance and dot plot data for all clusters
shap_plot_data <- lapply(unique_clusters, generate_shap_plots_for_cluster, 
                         model = rf_model2, combined_data = combined_data, sample_size = 30)

# Bind all cluster data together for faceting
importance_df <- bind_rows(lapply(shap_plot_data, function(x) x$importance_data))
shap_df <- bind_rows(lapply(shap_plot_data, function(x) x$shap_data))

# Combine both datasets for faceting
final_plot_data <- bind_rows(importance_df, shap_df)

facet_plot <- ggplot(final_plot_data) +
  # Use different geoms for importance (bar) and SHAP values (dot)
  geom_bar(data = importance_df, aes(x = reorder(feature, importance), y = importance, fill = cluster_color), 
           stat = "identity", show.legend = FALSE) +
  geom_point(data = shap_df, aes(x = shap_value, y = feature, color = feature_value), alpha = 0.6) +
  facet_wrap(~ cluster + plot_type, ncol = 2, scales = "free") +  # Arrange in 2 columns (bar plot left, dot plot right)
  labs(x = "Feature", y = "Mean Absolute SHAP Value", title = "FNConc Yearly - Cluster SHAP Importance & Values") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  # Apply cluster-specific color gradient for SHAP dot plots
  scale_color_gradientn(colors = c(shap_df$cluster_light[1], shap_df$cluster_dark[1]), name = "Scaled Value")

### **Save the Faceted Plot**
ggsave(
  filename = "FNConc_Yearly_Cluster_Faceted_Plot.pdf",
  plot = facet_plot,
  width = 12,
  height = 12,
  dpi = 300,
  path = output_dir
)

print(facet_plot)

message("Faceted SHAP importance and dot plots completed and saved.")
