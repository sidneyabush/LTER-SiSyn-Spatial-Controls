# -------------------------------
# 1. Load Packages & Set Up Environment
# -------------------------------
librarian::shelf(
  iml, ggplot2, dplyr, tidyr, factoextra, cluster, colorspace, scales,
  fastshap, patchwork, RColorBrewer, grid
)

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
# 3. Prepare Data & Perform Clustering
# -------------------------------
# Use 5 driver variables for clustering
data <- kept_drivers %>%
  dplyr::select("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")

scaled_data <- data %>% 
  mutate(across(where(is.numeric), ~ rescale(.)))

set.seed(123)
p2 <- fviz_nbclust(scaled_data, kmeans, method = "silhouette", k.max = 20)
print(p2)

kmeans_result <- kmeans(scaled_data, iter.max = 50, nstart = 50, centers = 5)

scaled_data <- scaled_data %>%
  mutate(cluster = factor(kmeans_result$cluster, levels = c("1","2","3","4","5")))

# -------------------------------
# 4. Create Long-format Data for Box Plots
# -------------------------------
long_data <- scaled_data %>%
  pivot_longer(-cluster, names_to = "Driver", values_to = "Value") %>%
  mutate(
    Driver = factor(Driver, levels = c("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")),
    Driver = recode(Driver, 
                    "elevation" = "Elevation",
                    "basin_slope" = "Basin Slope",
                    "P" = "P",
                    "rocks_volcanic" = "Volcanic Rock",
                    "evapotrans" = "ET")
  )

# -------------------------------
# 5. Define a Named Color Vector Matching Your Palette
# -------------------------------
my_cluster_colors <- c(
  "1" = "#E69F00",  # Cluster 1 (orange)
  "2" = "#56B4E9",  # Cluster 2 (blue)
  "3" = "#009E73",  # Cluster 3 (green)
  "4" = "#F0E442",  # Cluster 4 (yellow)
  "5" = "#648FFF"   # Cluster 5 (purple)
)

# -------------------------------
# 6. Generate Box Plots (Left Panels; Hide Legend & X-axis labels except last)
# -------------------------------
cluster_boxplots <- lapply(sort(unique(long_data$cluster)), function(cl) {
  long_data %>%
    filter(cluster == cl) %>%
    ggplot(aes(x = Driver, y = Value, fill = cluster)) +
    geom_boxplot() +
    scale_fill_manual(values = my_cluster_colors, guide = "none") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
      axis.text.y = element_text(size = 20)
    )
})

# Remove x-axis labels from all but the last box plot
for(i in seq_along(cluster_boxplots)){
  if(i < length(cluster_boxplots)){
    cluster_boxplots[[i]] <- cluster_boxplots[[i]] +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
}

# -------------------------------
# 7. Prepare Data for SHAP Dot Plots (Right Panels)
# -------------------------------
# Include all kept drivers (or add additional if desired)
full_scaled <- kept_drivers %>%
  mutate(across(where(is.numeric), ~ rescale(.))) %>%
  as.data.frame()
full_scaled$cluster <- factor(kmeans_result$cluster, levels = c("1","2","3","4","5"))

global_min <- min(full_scaled %>% select(-cluster), na.rm = TRUE)
global_max <- max(full_scaled %>% select(-cluster), na.rm = TRUE)

# -------------------------------
# 8. Define SHAP Dot Plot Function (with Recoding of Feature Names)
# -------------------------------
generate_shap_dot_plot_obj <- function(cluster_id, shap_values, full_scaled, global_shap_min, global_shap_max) {
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  
  cluster_data <- full_scaled[cluster_indices, , drop = FALSE] %>% select(-cluster)
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
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
  
  # Recode feature names as desired
  shap_long$feature <- recode(shap_long$feature,
                              "elevation" = "Elevation",
                              "basin_slope" = "Basin Slope",
                              "P" = "Phosphorus",
                              "rocks_volcanic" = "Volcanic Rock",
                              "evapotrans" = "ET",
                              "land_urban_and_built_up_land" = "Land: Urban & Built-up")
  
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, color = feature_value)) +
    geom_point(alpha = 0.6, size = 3) +
    scale_color_gradientn(
      colors = c("#2b8cbe", "#f0f0f0", "#e6550d"),  # Blue - Gray - Orange gradient
      name = NULL,
      limits = c(global_min, global_max)
    ) +
    labs(title = NULL, x = "SHAP Value", y = NULL, color = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_x_continuous(limits = c(global_shap_min, global_shap_max)) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20),
      legend.key.size = unit(1.5, "lines")
    )
  
  return(dot_plot)
}

# -------------------------------
# 9. Generate (or Load) SHAP Values & Set Global SHAP Limits
# -------------------------------
shap_file <- "shap_values_FNConc.rds"
if (file.exists(shap_file)) {
  shap_values <- readRDS(shap_file)
  message("Loaded cached SHAP values from ", shap_file)
} else {
  generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
    predictors <- all.vars(model$terms)[-1]
    X_input <- kept_drivers %>% select(all_of(predictors))
    custom_predict <- function(object, newdata) {
      newdata <- as.data.frame(newdata)
      colnames(newdata) <- predictors
      predict(object, newdata = newdata, type = "response")
    }
    fastshap::explain(
      object = model,
      X = X_input,
      pred_wrapper = custom_predict,
      nsim = sample_size
    )
  }
  shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)
  saveRDS(shap_values, shap_file)
  message("Saved SHAP values to ", shap_file)
}

global_shap_min <- min(shap_values, na.rm = TRUE)
global_shap_max <- max(shap_values, na.rm = TRUE)

# -------------------------------
# 10. Generate Dot Plots for Each Cluster
# -------------------------------
unique_clusters <- sort(unique(full_scaled$cluster))
dot_plots <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(cl, shap_values, full_scaled, global_shap_min, global_shap_max)
})

# Remove x-axis labels (title and tick labels) from all but the last dot plot
for(i in seq_along(dot_plots)){
  if(i < length(dot_plots)){
    dot_plots[[i]] <- dot_plots[[i]] +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
}

# -------------------------------
# 11. Assign Letters to Box Plots and Remove X-axis Labels from all but the Last Box Plot
# -------------------------------
letters_vec <- LETTERS[1:length(cluster_boxplots)]
for (i in seq_along(cluster_boxplots)) {
  cluster_boxplots[[i]] <- cluster_boxplots[[i]] +
    labs(tag = letters_vec[i]) +
    theme(
      plot.tag.position = c(-0.25, 1),  # Move letter far left
      plot.tag = element_text(face = "bold", size = 24),
      plot.clip = "off",               # Turn off clipping
      plot.margin = margin(0, 0, 0, 80, "pt")  # Add left margin so letters aren't cut off
    )
}

# -------------------------------
# 12. Combine Box Plots into a Single Column with Shared Y-axis Label
# -------------------------------
left_col <- wrap_plots(cluster_boxplots, ncol = 1)

# Create a text grob for the shared y-axis label, rotated 90 degrees
y_label_grob <- grid::textGrob("Scaled Value", rot = 90, gp = grid::gpar(fontsize = 20))

# Wrap the text grob as a patchwork element
y_label_plot <- wrap_elements(full = y_label_grob)

# Combine the y-axis label with the box plots column (label on left)
left_col_labeled <- y_label_plot | left_col

# -------------------------------
# 13. Combine Dot Plots into a Single Column
# -------------------------------
right_col <- wrap_plots(dot_plots, ncol = 1)

# -------------------------------
# 14. Combine the Labeled Box Plot Column and Dot Plot Column Side-by-Side
# -------------------------------
final_combined_plot <- left_col_labeled | right_col +
  plot_layout(guides = "collect")  # Collect dot plot legends into one

# Display the final layout
print(final_combined_plot)

# -------------------------------
# 15. Save the Final Figure
# -------------------------------
ggsave(
  filename = "Combined_Cluster_Boxplot_and_SHAP_DotPlots_5x2.png",
  plot = final_combined_plot,
  width = 14,
  height = 18,
  dpi = 300,
  path = output_dir
)
