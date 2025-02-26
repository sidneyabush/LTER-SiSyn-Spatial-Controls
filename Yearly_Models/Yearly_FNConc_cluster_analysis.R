###############################################################################
# COMPLETE WORKFLOW: FNConc Cluster Plotting
###############################################################################


## 1. Load Packages & Clear Environment
rm(list = ls())  
library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(patchwork)   # For wrap_plots(), plot_annotation()
library(scales)
library(fastshap)
library(RColorBrewer)
library(grid)        # For textGrob() if needed
library(colorspace)

# Set working directory and output directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

## 2. Load Data & Model
load("FNConc_Yearly_rf_model2_full.RData")
load("FNConc_Yearly_kept_drivers_full.RData")
load("FNConc_Yearly_full.RData")
load("FNConc_Yearly_full_stream_ids.RData")

## 3. Prepare Data & Perform Clustering
# Select driver variables
data <- kept_drivers %>%
  dplyr::select("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")

# Scale them between 0 and 1
scaled_data <- data %>%
  mutate(across(where(is.numeric), ~ scales::rescale(.)))

# Optional: check silhouette to pick k
# p2 <- factoextra::fviz_nbclust(scaled_data, kmeans, method = "silhouette", k.max = 20)
# print(p2)

set.seed(123)
kmeans_result <- kmeans(scaled_data, iter.max = 50, nstart = 50, centers = 5)

scaled_data <- scaled_data %>%
  mutate(cluster = factor(kmeans_result$cluster, levels = c("1","2","3","4","5")))

## 4. Create Long-format Data for Box Plots
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

## 5. Define Cluster Colors
my_cluster_colors <- c(
  "1" = "#E69F00",  # orange
  "2" = "#56B4E9",  # blue
  "3" = "#009E73",  # green
  "4" = "#F0E442",  # yellow
  "5" = "#648FFF"   # purple
)

# Create a lighter version of your cluster colors (adjust the 'amount' as needed)
my_cluster_colors_lighter <- sapply(my_cluster_colors, function(x) lighten(x, amount = 0.3))

## 6. Generate Box Plots (No manual letters)
unique_clusters <- sort(unique(long_data$cluster))

cluster_boxplots <- lapply(unique_clusters, function(cl) {
  p <- long_data %>%
    filter(cluster == cl) %>%
    ggplot(aes(x = Driver, y = Value, fill = cluster)) +
    geom_boxplot() +
    scale_fill_manual(values = my_cluster_colors_lighter, guide = "none") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = NULL, y = NULL) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14)
    )
  
  # Remove x-axis labels for all but the last plot
  if (cl != tail(unique_clusters, 1)) {
    p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  return(p)
})

## 7. Prepare Data for SHAP Dot Plots
full_scaled <- kept_drivers %>%
  mutate(across(where(is.numeric), ~ scales::rescale(.))) %>%
  as.data.frame()
full_scaled$cluster <- factor(kmeans_result$cluster, levels = c("1","2","3","4","5"))

global_min <- min(full_scaled %>% dplyr::select(-cluster), na.rm = TRUE)
global_max <- max(full_scaled %>% dplyr::select(-cluster), na.rm = TRUE)

## 8. Define SHAP Dot Plot Function
generate_shap_dot_plot_obj <- function(cluster_id, shap_values, full_scaled, global_shap_min, global_shap_max) {
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  
  cluster_data <- full_scaled[cluster_indices, , drop = FALSE] %>% dplyr::select(-cluster)
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  shap_values_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  shap_long <- shap_values_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  # Order features by mean absolute SHAP
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  # Recode feature names
  shap_long$feature <- recode(shap_long$feature,
                              "elevation" = "Elevation",
                              "basin_slope" = "Basin Slope",
                              "P" = "P",
                              "rocks_volcanic" = "Volcanic Rock",
                              "evapotrans" = "ET",
                              "land_urban_and_built_up_land" = "Land: Urban & Built-up")
  
  # Build dot plot
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, fill = feature_value)) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "gray", "black"),  
      name = NULL,
      limits = c(global_min, global_max)
    ) +
    labs(x = "SHAP Value", y = NULL, title = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_x_continuous(limits = c(global_shap_min, global_shap_max)) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.key.size = unit(1.5, "lines")
    )
  
  return(dot_plot)
}

## 9. Generate or Load SHAP Values, Set Global SHAP Limits
shap_file <- "shap_values_FNConc.rds"
if (file.exists(shap_file)) {
  shap_values <- readRDS(shap_file)
  message("Loaded cached SHAP values from ", shap_file)
} else {
  generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
    predictors <- all.vars(model$terms)[-1]
    X_input <- kept_drivers %>% dplyr::select(all_of(predictors))
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

## 10. Generate Dot Plots for Each Cluster
dot_plots <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(cl, shap_values, full_scaled, global_shap_min, global_shap_max)
})

# Remove x-axis labels for all but the last dot plot
for(i in seq_along(dot_plots)) {
  if(i < length(dot_plots)) {
    dot_plots[[i]] <- dot_plots[[i]] + 
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
}

## 11. Combine Plots in patchwork
# Left column (box plots) with shared y-label
left_col <- wrap_plots(cluster_boxplots, ncol = 1) & 
  labs(y = "Scaled Value")

# Right column (SHAP dot plots)
right_col <- wrap_plots(dot_plots, ncol = 1)

# Combine side-by-side, single letter scheme
final_combined_plot <- left_col | right_col
final_combined_plot <- final_combined_plot +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",   # Automatic labeling A, B, C, ...
    title = NULL,
    caption = NULL,
    theme = theme(
      plot.tag = element_text(size = 30, face = "bold"),
      plot.tag.position = "topleft"
    )
  )

## 12. Save the Final Figure
ggsave(
  filename = "Combined_Cluster_Boxplot_and_SHAP_DotPlots_5x2.png",
  plot = final_combined_plot,
  width = 11,
  height = 13,
  dpi = 300,
  path = output_dir
)

# Print to see in the current session
print(final_combined_plot)

###############################################################################
# CREATE BOX PLOT OF FNConc BY CLUSTER
###############################################################################

# -- STEP 1: Build a data frame with FNConc and cluster for each site --
df <- data.frame(
  FNConc  = drivers_df$FNConc,     # Adjust if your column name is different
  cluster = scaled_data$cluster    # cluster assignment from k-means
)

# -- STEP 2: Make a box plot + jitter of FNConc across 5 clusters --
p <- ggplot(df, aes(x = factor(cluster), y = FNConc, fill = factor(cluster))) +
  # Boxplot with lighter fill colors and black outline
  geom_boxplot(outlier.shape = NA, color = "black") +
  # Overlaid points using the lighter palette
  geom_jitter(aes(color = factor(cluster)), width = 0.3, alpha = 0.4, size = 2) +
  # Use the lighter palette for both box fill and points
  scale_fill_manual(values = my_cluster_colors_lighter) +
  scale_color_manual(values = my_cluster_colors) +
  # Labels and theme adjustments
  labs(
    title = NULL,
    x = "Cluster",
    y = expression("DSi Concentration (mg L"^{-1}*")")
  ) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

print(p)

ggsave("FNConc_Yearly_Clusters.png", p, width = 8, height = 5, dpi = 300, path = output_dir)
