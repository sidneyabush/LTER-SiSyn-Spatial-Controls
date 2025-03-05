###############################################################################
# COMPLETE WORKFLOW: FNYield Cluster Plotting
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

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"

# -------------------------------
# 2. Load Data & Model
# -------------------------------
load("FNYield_Yearly_rf_model2_full_new.RData")
load("FNYield_Yearly_kept_drivers_full_new.RData")
load("FNYield_Yearly_full_new.RData")
load("FNYield_Yearly_full_stream_ids_new.RData")
# Load precomputed SHAP values
load("FNYield_Yearly_shap_values_new.RData")

# -------------------------------
# 3. Prepare Data & Perform Clustering
# -------------------------------
data <- kept_drivers %>%
  dplyr::select("rocks_volcanic", "basin_slope", "land_shrubland_grassland", "temp", "permafrost")

scaled_data <- data %>% 
  mutate(across(where(is.numeric), ~ rescale(.)))

set.seed(123)
p2 <- fviz_nbclust(scaled_data, kmeans, method = "silhouette", k.max = 20)
print(p2)

# Evaluate optimal number of clusters using the WSS (elbow) method
wss_plot <- fviz_nbclust(scaled_data, kmeans, method = "wss", k.max = 20) +
  geom_vline(xintercept = 4, linetype = 2)  # Optional: add a vertical line to highlight potential optimal cluster count
print(wss_plot)

kmeans_result <- kmeans(scaled_data, iter.max = 50, nstart = 50, centers = 4)

scaled_data <- scaled_data %>%
  mutate(cluster = factor(kmeans_result$cluster, levels = c("1","2","3","4")))

# -------------------------------
# 4. Create Long-format Data for Box Plots
# -------------------------------
long_data <- scaled_data %>%
  pivot_longer(-cluster, names_to = "Driver", values_to = "Value") %>%
  mutate(
    Driver = factor(Driver, levels = c("rocks_volcanic", "temp", 
                                       "land_shrubland_grassland", "basin_slope", 
                                       "permafrost")),
    Driver = recode(Driver, 
                    "rocks_volcanic" = "Volcanic Rock",
                    "temp" = "Temperature",
                    "land_shrubland_grassland" = "Land: Shrubland & Grassland",
                    "basin_slope" = "Basin Slope",
                    "permafrost" = "Permafrost")
  )

# -------------------------------
# 5. Define a Named Color Vector
# -------------------------------
# my_cluster_colors <- c(
#   "1" = "#AC7B32",  # Rich Ochre (Warm Earthy Brown-Gold)  
#   "2" = "#579C8E",  # Muted Teal (Cool & fresh)  
#   "3" = "#C26F86",  # Dusty Rose (Soft but warm)  
#   "4" = "#8F7E4F",  # Olive-Taupe (Neutral & grounding)  
#   "5" = "#5E88B0"   # Soft Steel Blue (Cool contrast)  
#   )

my_cluster_colors <- c(
  "1" = "#AC7B32",  # Rich Ochre (Warm Earthy Brown-Gold)
  "2" = "#579C8E",  # Muted Teal (Cool & fresh)
  "3" = "#C26F86",  # Dusty Rose (Soft but warm)
  "4" = "#5E88B0"   # Soft Steel Blue (Cool contrast)
)


# Create a lighter version of your cluster colors (adjust the 'amount' as needed)
my_cluster_colors_lighter <- sapply(my_cluster_colors, function(x) lighten(x, amount = 0.1))

## 6. Generate Box Plots (No manual letters)
unique_clusters <- sort(unique(long_data$cluster))

cluster_boxplots <- lapply(unique_clusters, function(cl) {
  p <- long_data %>%
    filter(cluster == cl) %>%
    ggplot(aes(x = Driver, y = Value, fill = cluster)) +
    geom_boxplot() +
    scale_fill_manual(values = my_cluster_colors_lighter, guide = "none") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = NULL, y = NULL,
         title = paste("Cluster", cl)) +
    theme_classic() +
    theme(
      plot.title   = element_text(size = 14, hjust = 0.5),
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
full_scaled$cluster <- factor(kmeans_result$cluster, levels = c("1","2","3","4"))

global_min <- min(full_scaled %>% dplyr::select(-cluster), na.rm = TRUE)
global_max <- max(full_scaled %>% dplyr::select(-cluster), na.rm = TRUE)

## 8. Define SHAP Dot Plot Function
generate_shap_dot_plot_obj <- function(cluster_id, shap_values_FNYield, full_scaled, global_shap_min, global_shap_max) {
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  
  cluster_data <- full_scaled[cluster_indices, , drop = FALSE] %>% dplyr::select(-cluster)
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  shap_values_FNYield_df <- as.data.frame(shap_values_FNYield)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  shap_long <- shap_values_FNYield_df %>%
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
                              "rocks_volcanic" = "Volcanic Rock",
                              "basin_slope" = "Basin Slope",
                              "land_urban_and_built_up_land" = "Land: Urban & Built Up",
                              "temp" = "Temperature",
                              "land_shrubland_grassland" = "Land: Shrubland & Grassland", 
                              "land_cropland" = "Land: Cropland",
                              "rocks_sedimentary" = "Sedimentary Rock",
                              "npp" = "NPP",
                              "precip" = "Precipitation",
                              "land_forest_all" = "Land: Forest",
                              "rocks_plutonic" = "Plutonic Rock",
                              "elevation" = "Elevation",
                              "permafrost" = "Permafrost",
                              "rocks_metamorphic" = "Metamorphic Rock",
                              "NOx" = "NOx",
                              "evapotrans" = "ET",
                              "rocks_carbonate_evaporite" = "Carbonite & Evaporite Rock",
                              "P"="P")
  
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
global_shap_min <- min(shap_values_FNYield, na.rm = TRUE)
global_shap_max <- max(shap_values_FNYield, na.rm = TRUE)

## 10. Generate Dot Plots for Each Cluster
dot_plots <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(cl, shap_values_FNYield, full_scaled, global_shap_min, global_shap_max)
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
  width = 12,
  height = 18,
  dpi = 300,
  path = output_dir
)

# Print to see in the current session
print(final_combined_plot)

###############################################################################
# CREATE BOX PLOT OF FNYield BY CLUSTER
###############################################################################

# -- STEP 1: Build a data frame with FNConc and cluster for each site --
df <- data.frame(
  Stream_ID  = drivers_df$Stream_ID,
  Year = drivers_df$Year, 
  FNYield  = drivers_df$FNYield,     # Adjust if your column name is different
  cluster = scaled_data$cluster
  
)

# Export df to upload to the map making script
write.csv(
  df, 
  file = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/FNYield_Stream_ID_Year_Cluster.csv",
  row.names = FALSE
)

# -- STEP 2: Make a box plot + jitter of FNYield across 5 clusters --
p <- ggplot(df, aes(x = factor(cluster), y = FNYield, fill = factor(cluster))) +
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
    y = expression(DSi~Yield~(kg~m^{-2}~y^{-1}))
  ) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

print(p)

ggsave("FNYield_Yearly_Clusters.png", p, width = 8, height = 5, dpi = 300, path = output_dir)

###############################################################################
# CREATE SILHOUETTE PLOT FOR FNYield CLUSTERS
###############################################################################

library(cluster)    # for silhouette()
library(factoextra) # for fviz_silhouette()

# 1. Compute silhouette widths from your k-means result
sil_obj <- silhouette(kmeans_result$cluster, dist(scaled_data))
mean_sil_value <- mean(sil_obj[, "sil_width"])

# 2. Create a silhouette plot with factoextra
#    - 'palette = unname(my_cluster_colors)' uses your custom cluster colors
#    - 'label = FALSE' hides individual silhouette labels
p_sil <- fviz_silhouette(
  sil_obj, 
  label   = FALSE,
  palette = unname(my_cluster_colors)  # Remove names; just pass the color values
) +
  # 3. Add a red dashed line at the average silhouette width
  geom_hline(
    yintercept = mean(sil_obj[, "sil_width"]), 
    linetype   = "dashed", 
    color      = "gray4"
  ) +
  annotate(
    "text",
    x     = nrow(sil_obj) * 0.85,  # Position text near the right side of the plot
    y     = mean_sil_value,
    label = paste("Mean =", round(mean_sil_value, 2)),
    color = "gray4",
    vjust = -0.5
  ) +
  # 4. Tweak labels & theme
  labs(
    title = NULL,
    x     = "Sites",
    y     = "Silhouette Width"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank()  # Remove x-axis ticks
  )
# 5. Print and/or save the plot
print(p_sil)
ggsave(
  filename = "FNYield_Yearly_Silhouette.png",
  plot     = p_sil,
  width    = 8,
  height   = 5,
  dpi      = 300,
  path     = output_dir
)

###############################################################################
# MEAN ABSOLUTE SHAP BAR PLOTS 
###############################################################################

# Define a function to compute & plot mean absolute SHAP for one cluster
plot_mean_abs_shap <- function(cluster_id, shap_values_FNYield, full_scaled) {
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  shap_cluster    <- shap_values_FNYield[cluster_indices, , drop = FALSE]
  
  mean_abs_shap <- colMeans(abs(shap_cluster), na.rm = TRUE)
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap)
  ) %>%
    arrange(desc(mean_abs_shapval))
  
  # Recode feature names for the y-axis
  df_shap$feature <- recode(
    df_shap$feature,
    "rocks_volcanic" = "Volcanic Rock",
    "basin_slope" = "Basin Slope",
    "land_urban_and_built_up_land" = "Land: Urban & Built Up",
    "temp" = "Temperature",
    "land_shrubland_grassland" = "Land: Shrubland & Grassland", 
    "land_cropland" = "Land: Cropland",
    "rocks_sedimentary" = "Sedimentary Rock",
    "npp" = "NPP",
    "precip" = "Precipitation",
    "land_forest_all" = "Land: Forest",
    "rocks_plutonic" = "Plutonic Rock",
    "elevation" = "Elevation",
    "permafrost" = "Permafrost",
    "rocks_metamorphic" = "Metamorphic Rock",
    "NOx" = "NOx",
    "evapotrans" = "ET",
    "rocks_carbonate_evaporite" = "Carbonite & Evaporite Rock",
    "P"="P")
  
  ggplot(df_shap, aes(x = reorder(feature, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(
      stat  = "identity",
      fill  = my_cluster_colors[[as.character(cluster_id)]],
      alpha = 0.8
    ) +
    coord_flip() +
    # scale_y_continuous(limits = c(0, 15)) +
    labs(
      x = NULL,
      y = "Mean Absolute SHAP Value",  # We will selectively remove this later
      title = paste("Cluster", cluster_id)
    ) +
    theme_classic(base_size = 14) +
    theme(
      plot.title   = element_text(size = 14, hjust = 0.5),
      axis.text.y  = element_text(size = 12),
      axis.text.x  = element_text(size = 12)
    )
}

# 2. Generate a plot for each of your 5 clusters
unique_clusters <- c("1","2","3","4")  # Adjust as needed
plot_list <- lapply(seq_along(unique_clusters), function(i) {
  cl <- unique_clusters[i]
  plot_mean_abs_shap(cl, shap_values_FNYield, full_scaled)
})

# 3. Arrange them in a 3×2 grid
ncol <- 2
nrow <- 2  # 5 plots => 3 rows × 2 columns (the last cell is empty)

# 4. Remove the x-axis label for all but:
for (i in seq_along(plot_list)) {
  row_number <- ceiling(i / ncol)
  # Keep the x-axis label only if this plot is in the bottom row (row == nrow)
  # or if it is the 4th plot (i == 4, corresponding to cluster 4 in row 2 col 2).
  if (!(row_number == nrow || i == 4)) {
    plot_list[[i]] <- plot_list[[i]] + theme(axis.title.x = element_blank())
  }
}

final_shap_grid <- wrap_plots(plot_list, ncol = ncol, nrow = nrow)
print(final_shap_grid)

# 5. Optionally save
ggsave(
  filename = "MeanAbsSHAP_Grid_3x2.png",
  plot     = final_shap_grid,
  width    = 10,
  height   = 11,
  dpi      = 300,
  path     = output_dir
)





