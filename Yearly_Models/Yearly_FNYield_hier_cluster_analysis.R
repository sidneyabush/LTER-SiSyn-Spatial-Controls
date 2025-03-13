###############################################################################
# COMPLETE WORKFLOW: FNYield Cluster Plotting with k = Number of Consolidated Rock Categories
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
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"

## 2. Load Data & Model
load("FNYield_Yearly_rf_model2_full_new.RData")
load("FNYield_Yearly_kept_drivers_full_new.RData")
load("FNYield_Yearly_full_new.RData")
load("FNYield_Yearly_full_stream_ids_new.RData")
# Load precomputed SHAP values
load("FNYield_Yearly_shap_values_new.RData")

drivers_full <- read.csv("All_Drivers_Harmonized_Yearly_FNConc_FNYield_5_years.csv")

drivers_combined <- drivers_df %>%
  left_join(drivers_full %>% select(Stream_ID, Year, major_rock, major_land), 
            by = c("Stream_ID", "Year"))

## Consolidate Lithology Categories
# Create a new factor variable 'consolidated_rock' using the major_rock column.
drivers_numeric_consolidated_lith <- drivers_combined %>%
  filter(trimws(major_rock) != "") %>%
  mutate(consolidated_rock = case_when(
    major_rock == "volcanic"                                   ~ "volcanic",
    major_rock == "sedimentary"                                ~ "sedimentary",
    major_rock == "metamorphic"                                ~ "metamorphic",
    major_rock == "plutonic"                                   ~ "plutonic",
    major_rock == "carbonate_evaporite"                       ~ "sedimentary",
    major_rock == "sedimentary; carbonate_evaporite"          ~ "sedimentary",
    major_rock == "volcanic; plutonic"                         ~ "igneous",
    major_rock %in% c("plutonic; metamorphic", "volcanic; plutonic; metamorphic") ~ "igneous/metamorphic",
    major_rock %in% c("sedimentary; plutonic; carbonate_evaporite; metamorphic",
                      "volcanic; sedimentary; carbonate_evaporite",
                      "volcanic; carbonate_evaporite",
                      "sedimentary; metamorphic",
                      "carbonate_evaporite; metamorphic")       ~ "mixed"
  )) %>%
  mutate(consolidated_rock = factor(consolidated_rock,
                                    levels = c("volcanic", "sedimentary", "metamorphic",
                                               "plutonic", "igneous", "igneous/metamorphic", "mixed")))

ggplot(drivers_numeric_consolidated_lith, aes(x = consolidated_rock, y = FNYield)) +
  geom_boxplot() +
  labs(x = "Lithology Category", y = "DSi Yield") +
  theme_classic()

## 3. Prepare Data & Perform Clustering
# Use the dataset with consolidated lithology
data <- drivers_numeric_consolidated_lith

# Scale numeric variables (non-numeric columns remain unchanged)
scaled_data <- data %>%
  mutate(across(where(is.numeric), ~ scales::rescale(.)))

## 3.1. Perform Hierarchical Clustering Based on Lithology Variables
# Subset lithology-related variables
lithology_data <- scaled_data %>%
  select(rocks_volcanic, rocks_sedimentary, rocks_carbonate_evaporite,
         rocks_metamorphic, rocks_plutonic)

# Optional: Evaluate the optimal number of clusters via elbow method
# fviz_nbclust(lithology_data, FUN = hcut, method = "wss", k.max = 20)

# Perform hierarchical clustering using Ward's method
hc_litho <- hclust(dist(lithology_data), method = "ward.D2")

# --- Set k equal to the number of consolidated rock categories ---
k <- length(levels(drivers_numeric_consolidated_lith$consolidated_rock))
# For example, if consolidated_rock has 7 levels, then k = 7

# Cut the dendrogram to form k clusters
scaled_data$cluster <- factor(cutree(hc_litho, k = k))

## (Optional Prior-Knowledge Relabeling Step)
# If you want to force the clusters to align with a predetermined order,
# you could define a mapping here. Otherwise, the clusters will be labeled 1...k.
# For example, if you want:
#   Cluster "1" = volcanic, "2" = plutonic, etc.
# desired_mapping <- c("volcanic" = "1",
#                      "plutonic" = "2",
#                      "sedimentary" = "3",
#                      "metamorphic" = "4",
#                      "igneous" = "5",
#                      "igneous/metamorphic" = "6",
#                      "mixed" = "7")
# Then compute the dominant consolidated rock per cluster and reassign labels.
# Here we omit that step since k is now set to the number of consolidated rock categories.

## 4. Create Long-format Data for Box Plots
# Remove non-numeric columns before pivoting.
long_data <- scaled_data %>%
  select(-major_rock, -consolidated_rock, -major_land, -Stream_ID, -Year) %>%
  pivot_longer(-cluster, names_to = "Driver", values_to = "Value") %>%
  mutate(
    Driver = factor(Driver, levels = c(
      "FNYield",
      "elevation", "basin_slope",
      "NOx", "P", "npp", "greenup_day", "evapotrans",
      "precip", "temp", "snow_cover", "permafrost",
      "rocks_volcanic", "rocks_sedimentary", "rocks_carbonate_evaporite",
      "rocks_metamorphic", "rocks_plutonic",
      "land_tundra", "land_barren_or_sparsely_vegetated", "land_cropland",
      "land_shrubland_grassland", "land_urban_and_built_up_land",
      "land_wetland", "land_forest_all")),
    Driver = recode(Driver,
                    "FNYield" = "DSi Yield",
                    "NOx" = "Nitrate",
                    "P" = "Phosphorous",
                    "precip" = "Precip",
                    "temp" = "Temperature",
                    "snow_cover" = "Snow Cover",
                    "npp" = "NPP",
                    "evapotrans" = "ET",
                    "greenup_day" = "Greenup Day",
                    "permafrost" = "Permafrost",
                    "elevation" = "Elevation",
                    "basin_slope" = "Basin Slope",
                    "rocks_volcanic" = "Rock: Volcanic",
                    "rocks_sedimentary" = "Rock: Sedimentary",
                    "rocks_carbonate_evaporite" = "Rock: Carbonate & Evaporite",
                    "rocks_metamorphic" = "Rock: Metamorphic",
                    "rocks_plutonic" = "Rock: Plutonic",
                    "land_tundra" = "Land: Tundra",
                    "land_barren_or_sparsely_vegetated" = "Land: Barren & Sparsely Vegetated",
                    "land_cropland" = "Land: Cropland",
                    "land_shrubland_grassland" = "Land: Shrubland & Grassland",
                    "land_urban_and_built_up_land" = "Land: Urban & Built-up",
                    "land_wetland" = "Land: Wetland",
                    "land_forest_all" = "Land: Forest")
  )

## 5. Define Cluster Colors
# You can define colors for each cluster; here we assume k clusters.
# For example, if k = 7, you need 7 colors.
my_cluster_colors <- c(
  "1" = "#AC7B32",  # Example: volcanic
  "2" = "#579C8E",  # Example: plutonic
  "3" = "#C26F86",  # Example: sedimentary
  "4" = "#5E88B0",  # Example: metamorphic
  "5" = "#D8A7B1",  # Example: igneous
  "6" = "#B4C7E7",  # Example: igneous/metamorphic
  "7" = "#F0E68C"   # Example: mixed
)[1:k]  # Subset to k clusters if k is less than 7

my_cluster_colors_lighter <- sapply(my_cluster_colors, function(x) lighten(x, amount = 0.3))

## 6. Generate Box Plots
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
      plot.title  = element_text(size = 14, hjust = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
      axis.text.y = element_text(size = 14)
    )
  if (cl != tail(unique_clusters, 1)) {
    p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  return(p)
})

## 7. Prepare Data for SHAP Dot Plots
full_scaled <- drivers_numeric_consolidated_lith %>%
  mutate(across(where(is.numeric), ~ scales::rescale(.))) %>%
  mutate(cluster = scaled_data$cluster) %>%
  select(-major_rock, -consolidated_rock, -major_land, -Stream_ID, -Year) %>%
  as.data.frame()

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
  
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  shap_long$feature <- recode(shap_long$feature,
                              "FNYield" = "DSi Yield",
                              "NOx" = "Nitrate",
                              "P" = "Phosphorous",
                              "precip" = "Precip",
                              "temp" = "Temperature",
                              "snow_cover" = "Snow Cover",
                              "npp" = "NPP",
                              "evapotrans" = "ET",
                              "greenup_day" = "Greenup Day",
                              "permafrost" = "Permafrost",
                              "elevation" = "Elevation",
                              "basin_slope" = "Basin Slope",
                              "rocks_volcanic" = "Rock: Volcanic",
                              "rocks_sedimentary" = "Rock: Sedimentary",
                              "rocks_carbonate_evaporite" = "Rock: Carbonate & Evaporite",
                              "rocks_metamorphic" = "Rock: Metamorphic",
                              "rocks_plutonic" = "Rock: Plutonic",
                              "land_tundra" = "Land: Tundra",
                              "land_barren_or_sparsely_vegetated" = "Land: Barren & Sparsely Vegetated",
                              "land_cropland" = "Land: Cropland",
                              "land_shrubland_grassland" = "Land: Shrubland & Grassland",
                              "land_urban_and_built_up_land" = "Land: Urban & Built-up",
                              "land_wetland" = "Land: Wetland",
                              "land_forest_all" = "Land: Forest")
  
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, fill = feature_value)) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black") +
    scale_fill_gradientn(colors = c("white", "gray", "black"),
                         name = NULL,
                         limits = c(global_min, global_max)) +
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

## 9. Set Global SHAP Limits
global_shap_min <- min(shap_values_FNYield, na.rm = TRUE)
global_shap_max <- max(shap_values_FNYield, na.rm = TRUE)

## 10. Generate Dot Plots for Each Cluster
dot_plots <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(cl, shap_values_FNYield, full_scaled, global_shap_min, global_shap_max)
})
for(i in seq_along(dot_plots)) {
  if(i < length(dot_plots)) {
    dot_plots[[i]] <- dot_plots[[i]] +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
}

## 11. Combine Plots in patchwork
left_col <- wrap_plots(cluster_boxplots, ncol = 1) & labs(y = "Scaled Value")
right_col <- wrap_plots(dot_plots, ncol = 1)
final_combined_plot <- left_col | right_col
final_combined_plot <- final_combined_plot +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    title = NULL,
    caption = NULL,
    theme = theme(
      plot.tag = element_text(size = 30, face = "bold"),
      plot.tag.position = "topleft"
    )
  )

## 12. Save the Final Figure
ggsave(filename = "Combined_Cluster_Boxplot_and_SHAP_DotPlots_6x2.png",
       plot = final_combined_plot,
       width = 16, height = 18, dpi = 300, path = output_dir)

print(final_combined_plot)

###############################################################################
# CREATE BOX PLOT OF FNYield BY CLUSTER
###############################################################################

df <- data.frame(
  Stream_ID  = drivers_df$Stream_ID,
  Year       = drivers_df$Year,
  FNYield    = drivers_df$FNYield,
  cluster    = scaled_data$cluster
)

write.csv(df,
          file = file.path(output_dir, "FNYield_Stream_ID_Year_Cluster.csv"),
          row.names = FALSE)

p <- ggplot(df, aes(x = factor(cluster), y = FNYield, fill = factor(cluster))) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = factor(cluster)), width = 0.3, alpha = 0.4, size = 2) +
  scale_fill_manual(values = my_cluster_colors_lighter) +
  scale_color_manual(values = my_cluster_colors) +
  labs(title = NULL, x = "Cluster",
       y = expression(DSi~Yield~(kg~m^{-2}~y^{-1}))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

print(p)
ggsave("FNYield_Yearly_Clusters.png", p, width = 8, height = 5, dpi = 300, path = output_dir)

###############################################################################
# CREATE SILHOUETTE PLOT FOR FNYield CLUSTERS
###############################################################################

sil_obj <- silhouette(as.numeric(as.character(scaled_data$cluster)), dist(lithology_data))
mean_sil_value <- mean(sil_obj[, "sil_width"])

p_sil <- fviz_silhouette(sil_obj, label = FALSE, palette = unname(my_cluster_colors)) +
  geom_hline(yintercept = mean(sil_obj[, "sil_width"]), linetype = "dashed", color = "gray4") +
  annotate("text", x = nrow(sil_obj) * 0.8, y = mean_sil_value,
           label = paste("Mean =", round(mean_sil_value, 2)),
           color = "gray4", vjust = -0.5) +
  labs(title = NULL, x = "Sites", y = "Silhouette Width") +
  theme_classic(base_size = 16) +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

print(p_sil)
ggsave(filename = "FNYield_Yearly_Silhouette.png", plot = p_sil,
       width = 8, height = 5, dpi = 300, path = output_dir)

###############################################################################
# MEAN ABSOLUTE SHAP BAR PLOTS 
###############################################################################

plot_mean_abs_shap <- function(cluster_id, shap_values_FNYield, full_scaled) {
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  shap_cluster    <- shap_values_FNYield[cluster_indices, , drop = FALSE]
  mean_abs_shap <- colMeans(abs(shap_cluster), na.rm = TRUE)
  df_shap <- data.frame(feature = names(mean_abs_shap),
                        mean_abs_shapval = as.numeric(mean_abs_shap)) %>%
    arrange(desc(mean_abs_shapval))
  df_shap$feature <- recode(df_shap$feature,
                            "NOx" = "Nitrate",
                            "P" = "Phosphorous",
                            "precip" = "Precip",
                            "temp" = "Temperature",
                            "snow_cover" = "Snow Cover",
                            "npp" = "NPP",
                            "evapotrans" = "ET",
                            "greenup_day" = "Greenup Day",
                            "permafrost" = "Permafrost",
                            "elevation" = "Elevation",
                            "basin_slope" = "Basin Slope",
                            "FNConc" = "DSi Concentration",
                            "rocks_volcanic" = "Rock: Volcanic",
                            "rocks_sedimentary" = "Rock: Sedimentary",
                            "rocks_carbonate_evaporite" = "Rock: Carbonate & Evaporite",
                            "rocks_metamorphic" = "Rock: Metamorphic",
                            "rocks_plutonic" = "Rock: Plutonic",
                            "land_tundra" = "Land: Tundra",
                            "land_barren_or_sparsely_vegetated" = "Land: Barren & Sparsely Vegetated",
                            "land_cropland" = "Land: Cropland",
                            "land_shrubland_grassland" = "Land: Shrubland & Grassland",
                            "land_urban_and_built_up_land" = "Land: Urban & Built-up",
                            "land_wetland" = "Land: Wetland",
                            "land_forest_all" = "Land: Forest")
  
  ggplot(df_shap, aes(x = reorder(feature, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(stat = "identity",
             fill = my_cluster_colors[[as.character(cluster_id)]],
             alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 6500)) +
    labs(x = NULL,
         y = "Mean Absolute SHAP Value",
         title = paste("Cluster", cluster_id)) +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12))
}

unique_clusters <- sort(unique(as.character(scaled_data$cluster)))
plot_list <- lapply(unique_clusters, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNYield, full_scaled)
})

ncol <- 2
nrow <- ceiling(length(unique_clusters)/ncol)
final_shap_grid <- wrap_plots(plot_list, ncol = ncol, nrow = nrow)
print(final_shap_grid)
ggsave(filename = "MeanAbsSHAP_Grid.png", plot = final_shap_grid,
       width = 12, height = 9, dpi = 300, path = output_dir)
