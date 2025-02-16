# -------------------------------
# 1. Load Packages & Set Up Environment
# -------------------------------
librarian::shelf(iml, ggplot2, dplyr, tidyr, factoextra, cluster, colorspace, scales, fastshap)

rm(list = ls())  # Clear environment

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

# -------------------------------
# 2. Load Data & Model
# -------------------------------
load("FNConc_Yearly_rf_model2_full.RData")
load("FNConc_Yearly_kept_drivers_full.RData")
load("FNConc_Yearly_full.RData")
load("FNConc_Yearly_full_stream_ids.RData")


# -------------------------------
# 2a. Create additional informative plots
# -------------------------------

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

# Ensure 'cluster' is a factor
all_data$cluster <- as.factor(all_data$cluster)

# Function to lighten colors
lighten_color <- function(color, factor = 0.3) {
  col <- col2rgb(color) / 255
  col <- col + factor * (1 - col)  # Lighten by a factor
  rgb(col[1], col[2], col[3])
}

# Create a lighter version of cb_palette for the jitter points
light_cb_palette <- sapply(cb_palette, lighten_color)

# Create a named vector mapping clusters to lighter colors
light_cluster_palette <- setNames(light_cb_palette, levels(all_data$cluster))

# Create a boxplot with the custom color palette
dist <- ggplot(all_data, aes(x = cluster, y = FNConc, fill = cluster)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(aes(color = cluster), alpha = 0.3, width = 0.2) +  # Add individual points with color
  scale_fill_manual(values = cb_palette) +  # Apply original color palette to boxplot
  scale_color_manual(values = light_cluster_palette) +  # Apply lighter color palette to jitter points
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

# -------------------------------
# 3. SHAP Value Generation (Before Clustering!)
# -------------------------------
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  # Extract only predictor variables (remove cluster)
  predictors <- all.vars(model$terms)[-1]  # Extract predictor names
  X_input <- kept_drivers %>% select(all_of(predictors))  # Keep only predictors
  
  # Custom predict function for formula-based models
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)  # Ensure data frame format
    colnames(newdata) <- predictors  # Match column names exactly
    predict(object, newdata = newdata, type = "response")  # Ensure numeric output
  }
  
  # Compute SHAP values
  shap_values <- fastshap::explain(
    object = model,
    X = X_input,  # Ensure only predictor variables are used
    pred_wrapper = custom_predict,
    nsim = sample_size
  )
  
  return(shap_values)
}

# Generate global SHAP values BEFORE adding cluster assignments
shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)

# -------------------------------
# 4. Clustering (AFTER SHAP)
# -------------------------------
# Define variables to use for clustering
cluster_vars <- c("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")

# Create a dataset for clustering from kept_drivers
cluster_data <- kept_drivers %>% select(all_of(cluster_vars))

# Scale clustering variables
scaled_cluster_data <- cluster_data %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))

set.seed(123)  # Ensure reproducibility

# Determine optimal clusters (optional)
p2 <- fviz_nbclust(scaled_cluster_data, kmeans, method = "silhouette", k.max = 20)
print(p2)

# Perform k-means clustering
kmeans_result <- kmeans(scaled_cluster_data, iter.max = 50, nstart = 50, centers = 3)

# NOW attach cluster assignments to kept_drivers (AFTER SHAP)
kept_drivers$cluster <- as.factor(kmeans_result$cluster)

# -------------------------------
# 5. Prepare Data for Visualization (Scaled for Display)
# -------------------------------
full_scaled <- kept_drivers %>%
  select(-cluster) %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>%
  as.data.frame()

# Reattach cluster for visualization (but NOT for SHAP!)
full_scaled$cluster <- kept_drivers$cluster

# Determine global min/max for SHAP dot plot scaling
global_min <- min(full_scaled %>% select(-cluster), na.rm = TRUE)
global_max <- max(full_scaled %>% select(-cluster), na.rm = TRUE)

# -------------------------------
# 6. Define Color Palette for Clusters
# -------------------------------
base_colors <- c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73")
cluster_colors <- lapply(base_colors, function(col) {
  c(lighten(col, 0.4), col, darken(col, 0.4))
})

# -------------------------------
# 7. SHAP Feature Importance Plot per Cluster
# -------------------------------
generate_feature_importance_plot <- function(cluster_id, shap_values, full_scaled, output_dir) {
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  shap_cluster <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE]
  
  overall_feature_importance <- shap_cluster %>%
    summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  cluster_base_color <- base_colors[[as.character(cluster_id)]]
  
  importance_plot_path <- sprintf("%s/SHAP_FNConc_Ave_Cluster_%s_Variable_Importance.pdf", output_dir, cluster_id)
  
  # Open PDF device
  pdf(importance_plot_path, width = 10, height = 8)
  
  # Explicitly create and print ggplot
  importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = cluster_base_color) +
    coord_flip() +
    labs(title = paste("Feature Importance for Cluster", cluster_id), 
         y = "Mean Absolute SHAP Value",
         x = NULL) +
    theme_classic()+
    theme(axis.title = element_text(size = 16, face = "bold"),
  axis.text = element_text(size = 14))
  
  print(importance_plot)  # Ensure ggplot is printed
  
  # Close PDF device
  dev.off()
}

# -------------------------------
# 8. SHAP Dot Plot per Cluster
# -------------------------------
generate_shap_dot_plot <- function(cluster_id, shap_values, full_scaled, output_dir, global_min, global_max) {
  cluster_data <- full_scaled %>% filter(cluster == cluster_id) %>% select(-cluster)
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  shap_values_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  shap_long <- shap_values_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  cluster_palette <- cluster_colors[[as.character(cluster_id)]]
  cluster_light <- cluster_palette[1]
  cluster_dark  <- cluster_palette[3]
  
  dot_plot_path <- sprintf("%s/SHAP_FNConc_Ave_Cluster_%s_Dot_Plot.pdf", output_dir, cluster_id)
  
  # Open PDF device
  pdf(dot_plot_path, width = 9, height = 8)
  
  # Explicitly create and print ggplot
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, color = feature_value)) +
    geom_point(alpha = 0.6) +
    scale_color_gradient(low = cluster_light, high = cluster_dark, name = "Scaled Value",
                         limits = c(global_min, global_max)) +
    labs(title = paste("SHAP Dot Plot for Cluster", cluster_id),
         x = "SHAP Value",
         y = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey1") +
    theme_minimal() +
    theme_classic()+
    theme(axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 14))
  
  print(dot_plot)  # Ensure ggplot is printed
  
  # Close PDF device
  dev.off()
}

# -------------------------------
# 9. Generate Plots for Each Cluster
# -------------------------------
unique_clusters <- unique(full_scaled$cluster)

lapply(unique_clusters, generate_feature_importance_plot, shap_values, full_scaled, output_dir)
lapply(unique_clusters, generate_shap_dot_plot, shap_values, full_scaled, output_dir, global_min, global_max)


