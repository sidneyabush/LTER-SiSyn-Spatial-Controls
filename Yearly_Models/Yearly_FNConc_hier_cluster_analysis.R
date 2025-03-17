###############################################################################
# COMPLETE WORKFLOW: FNConc Cluster Plotting with Dot Plots, Silhouette, & Grid
###############################################################################

## 1. Load Packages & Clear Environment
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)   # For wrap_plots(), plot_annotation()
library(fastshap)
library(RColorBrewer)
library(grid)        # For textGrob() if needed
library(colorspace)
library(cluster)     # silhouette()
library(factoextra)  # fviz_silhouette() - used earlier, but we'll do custom now

# Set working directory and output directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

###############################################################################
# 2. Load Data & Model
###############################################################################
## 2. Load Data & Model
load("FNConc_Yearly_rf_model2_full_new.RData")
load("FNConc_Yearly_kept_drivers__full_new.RData")
load("FNConc_Yearly_full_new.RData")
load("FNConc_Yearly_full_stream_ids_full_new.RData")

# Load precomputed SHAP values
load("FNConc_Yearly_shap_values_new.RData")

drivers_full <- read.csv("All_Drivers_Harmonized_Yearly_FNConc_FNYield_5_years.csv")

# Join 'major_rock' and 'major_land' onto 'drivers_df'
drivers_combined <- drivers_df %>%
  left_join(
    drivers_full %>% dplyr::select(Stream_ID, Year, major_rock, major_land),
    by = c("Stream_ID", "Year")
  )

###############################################################################
# 3. Consolidate Lithology Categories & Manually Assign Clusters
###############################################################################
drivers_numeric_consolidated_lith <- drivers_combined %>%
  # Remove rows with missing, blank, or "0" in major_rock
  filter(!is.na(major_rock) & trimws(major_rock) != "" & major_rock != "0") %>%
  mutate(
    # Group various string combos to one of four categories
    consolidated_rock = case_when(
      major_rock %in% c(
        "volcanic", 
        "volcanic; sedimentary; carbonate_evaporite",
        "volcanic; carbonate_evaporite", 
        "volcanic; plutonic", 
        "volcanic; plutonic; metamorphic"
      ) ~ "Volcanic",
      major_rock %in% c(
        "sedimentary", 
        "sedimentary; carbonate_evaporite", 
        "sedimentary; plutonic; carbonate_evaporite; metamorphic",
        "sedimentary; metamorphic"
      ) ~ "Sedimentary",
      major_rock %in% c(
        "metamorphic", 
        "plutonic", 
        "plutonic; metamorphic", 
        "carbonate_evaporite; metamorphic"
      ) ~ "Metamorphic",
      major_rock == "carbonate_evaporite" ~ "Carbonate_Evaporite"
    )
  ) %>%
  mutate(
    # If Sedimentary and >=70% sed rocks -> Sedimentary; else Mixed Sedimentary
    final_cluster = case_when(
      consolidated_rock == "Sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "Sedimentary" & rocks_sedimentary < 70  ~ "Mixed Sedimentary",
      TRUE ~ consolidated_rock
    )
  ) %>%
  # Manually order clusters
  mutate(final_cluster = factor(
    final_cluster, 
    levels = c("Volcanic", "Sedimentary", "Mixed Sedimentary", 
               "Metamorphic", "Carbonate_Evaporite")
  ))

###############################################################################
# 4. Prepare Data for Further Analysis (Single Global Scaling)
###############################################################################
# Identify numeric columns to scale (exclude "cluster" if it exists)
numeric_cols <- setdiff(
  names(dplyr::select(drivers_numeric_consolidated_lith, where(is.numeric))),
  "cluster"
)

# Scale all numeric columns across the entire dataset
scaled_data <- drivers_numeric_consolidated_lith %>%
  mutate(
    across(
      all_of(numeric_cols),
      ~ scales::rescale(.x, na.rm = TRUE)
    )
  )

###############################################################################
# 5. Define Cluster Colors (Using New Naming & Order)
###############################################################################
my_cluster_colors <- c(
  "Volcanic"            = "#AC7B32",  
  "Sedimentary"         = "#579C8E",  
  "Mixed Sedimentary"   = "#89C8A0",
  "Metamorphic"         = "#C26F86",  
  "Carbonate_Evaporite" = "#5E88B0"   
)
my_cluster_colors_lighter <- sapply(my_cluster_colors, function(x) lighten(x, amount = 0.3))

###############################################################################
# 6. Create Long-format Data for Box Plots (Drivers) using final_cluster
###############################################################################
# Replace references to FNYield with FNConc.
long_data <- scaled_data %>%
  dplyr::select(-major_rock, -consolidated_rock, -major_land, -Stream_ID, -Year) %>%
  pivot_longer(-final_cluster, names_to = "Driver", values_to = "Value") %>%
  mutate(
    Driver = factor(
      Driver,
      levels = c(
        "FNConc",  # changed from FNYield to FNConc
        "elevation", "basin_slope",
        "NOx", "P", "npp", "greenup_day", "evapotrans",
        "precip", "temp", "snow_cover", "permafrost",
        "rocks_volcanic", "rocks_sedimentary", "rocks_carbonate_evaporite",
        "rocks_metamorphic", "rocks_plutonic",
        "land_tundra", "land_barren_or_sparsely_vegetated", "land_cropland",
        "land_shrubland_grassland", "land_urban_and_built_up_land",
        "land_wetland", "land_forest_all"
      )
    ),
    Driver = recode(
      Driver,
      "FNConc" = "DSi Concentration",  # updated recode
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
      "land_forest_all" = "Land: Forest"
    )
  )

###############################################################################
# 7. Generate Box Plots for Each Cluster (Drivers) using final_cluster
###############################################################################
unique_clusters <- levels(long_data$final_cluster)

cluster_boxplots <- lapply(unique_clusters, function(cl) {
  p <- long_data %>%
    filter(final_cluster == cl) %>%
    ggplot(aes(x = Driver, y = Value, fill = final_cluster)) +
    geom_boxplot() +
    scale_fill_manual(values = my_cluster_colors_lighter, guide = "none") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = NULL, y = NULL, title = NULL) +  # Remove cluster title labels
    theme_classic() +
    theme(
      plot.title = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
      axis.text.y = element_text(size = 14)
    )
  if (cl != tail(unique_clusters, 1)) {
    p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  return(p)
})

###############################################################################
# 8. Box Plot of FNConc by Manually Assigned Cluster (Unscaled FNConc)
###############################################################################
df <- drivers_numeric_consolidated_lith %>%
  dplyr::select(Stream_ID, Year, FNConc, final_cluster)  # replaced FNYield with FNConc

write.csv(
  df,
  file = file.path(output_dir, "FNConc_Stream_ID_Year_Cluster.csv"),
  row.names = FALSE
)

p_FNConc <- ggplot(df, aes(x = final_cluster, y = FNConc, fill = final_cluster)) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = final_cluster), width = 0.3, alpha = 0.4, size = 2) +
  scale_fill_manual(values = my_cluster_colors_lighter) +
  scale_color_manual(values = my_cluster_colors) +
  labs(x = NULL, y = expression(DSi~Concentration~(mg~L^{-1}))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

###############################################################################
# 9. Silhouette Plot with Factoextra (Remove x-axis elements)
###############################################################################
sil_obj <- silhouette(
  as.numeric(scaled_data$final_cluster),
  dist(scaled_data %>% dplyr::select(rocks_volcanic, rocks_sedimentary, 
                                     rocks_carbonate_evaporite, rocks_metamorphic, 
                                     rocks_plutonic))
)
mean_sil_value <- mean(sil_obj[, "sil_width"], na.rm = TRUE)

p_sil <- fviz_silhouette(
  sil_obj,
  label   = FALSE,
  palette = c("#AC7B32","#579C8E","#89C8A0","#C26F86","#5E88B0")
)
p_sil <- p_sil + guides(color = "none") +
  scale_fill_manual(
    name   = "Cluster",
    values = c("1"="#AC7B32", "2"="#579C8E", "3"="#89C8A0", "4"="#C26F86", "5"="#5E88B0"),
    labels = c("1"="Volcanic", "2"="Sedimentary", "3"="Mixed Sedimentary",
               "4"="Metamorphic", "5"="Carbonate_Evaporite")
  ) +
  geom_hline(yintercept = mean_sil_value, linetype = "dashed", color = "gray4") +
  annotate("text", x = nrow(sil_obj)*0.8, y = mean_sil_value,
           label = paste("Mean =", round(mean_sil_value,2)),
           color = "gray4", vjust = -0.5) +
  labs(x = NULL, y = "Silhouette Width", title = NULL, subtitle = NULL) +
  theme_classic(base_size = 16) +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title   = element_blank(),
        plot.subtitle = element_blank())

print(p_sil)

###############################################################################
# ***** Define the function plot_mean_abs_shap for SHAP bar plots *****
###############################################################################
plot_mean_abs_shap <- function(cluster_id, shap_values_FNConc, full_scaled) {
  cluster_indices <- which(full_scaled$final_cluster == cluster_id)
  shap_cluster    <- shap_values_FNConc[cluster_indices, , drop = FALSE]
  mean_abs_shap <- colMeans(abs(shap_cluster), na.rm = TRUE)
  
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap)
  ) %>%
    arrange(desc(mean_abs_shapval)) %>%
    filter(!grepl("rock", feature, ignore.case = TRUE))
  
  df_shap$feature <- recode(df_shap$feature,
                            "NOx" = "Nitrate",
                            "P"   = "Phosphorous",
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
                            "land_forest_all" = "Land: Forest"
  )
  
  ggplot(df_shap, aes(x = reorder(feature, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(stat = "identity", fill = my_cluster_colors[[as.character(cluster_id)]], alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1.3)) +
    labs(x = NULL, y = "Mean Absolute SHAP Value", title = NULL) +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
}

###############################################################################
# 10. SHAP Dot Plots (Remove Titles from Each Dot Plot)
###############################################################################
full_scaled <- scaled_data

global_min <- min(full_scaled %>% dplyr::select(where(is.numeric)), na.rm = TRUE)
global_max <- max(full_scaled %>% dplyr::select(where(is.numeric)), na.rm = TRUE)

generate_shap_dot_plot_obj <- function(cluster_name, shap_values_FNConc, full_scaled, global_shap_min, global_shap_max) {
  cluster_indices <- which(full_scaled$final_cluster == cluster_name)
  cluster_data <- full_scaled[cluster_indices, , drop = FALSE] %>% dplyr::select(where(is.numeric))
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  shap_values_FNConc_df <- as.data.frame(shap_values_FNConc)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  shap_long <- shap_values_FNConc_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  # Remove rock-related features
  shap_long <- shap_long %>% filter(!grepl("rock", feature, ignore.case = TRUE))
  
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  shap_long$feature <- recode(shap_long$feature,
                              "FNConc" = "DSi Concentration",  # updated recode
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
                              "basin_slope" = "Basin Slope")
  
  ggplot(shap_long, aes(x = shap_value, y = feature, fill = feature_value)) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "gray", "black"),
      name   = NULL,
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
      legend.key.size = unit(1.5, "lines"),
      plot.title = element_blank()
    )
}

global_shap_min <- min(shap_values_FNConc, na.rm = TRUE)
global_shap_max <- max(shap_values_FNConc, na.rm = TRUE)

dot_plots <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(cl, shap_values_FNConc, full_scaled, global_shap_min, global_shap_max)
})

for(i in seq_along(dot_plots)) {
  if(i < length(dot_plots)) {
    dot_plots[[i]] <- dot_plots[[i]] + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
}

###############################################################################
# 11. Combine Box Plots & SHAP Dot Plots with patchwork (No extra titles)
###############################################################################
left_col <- wrap_plots(cluster_boxplots, ncol = 1) & labs(y = "Scaled Value")
right_col <- wrap_plots(dot_plots, ncol = 1)

final_combined_plot <- left_col | right_col +
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

###############################################################################
# 12. Save Final Figures
###############################################################################
ggsave(
  filename = "Combined_Cluster_Boxplot_and_SHAP_DotPlots.png",
  plot = final_combined_plot,
  width = 16,
  height = 18,
  dpi = 300,
  path = output_dir
)
print(final_combined_plot)

ggsave(
  filename = "FNConc_Yearly_Clusters.png",
  plot = p_FNConc,
  width = 8,
  height = 5,
  dpi = 300,
  path = output_dir
)
print(p_FNConc)

ggsave(
  filename = "Custom_Silhouette_Plot.png",
  plot = p_sil,
  width = 10,
  height = 6,
  dpi = 300,
  path = output_dir
)
print(p_sil)

# ***** Create the list of SHAP bar plots (plot_list_bars) *****
unique_clusters_for_shap <- levels(full_scaled$final_cluster)
plot_list_bars <- lapply(unique_clusters_for_shap, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNConc, full_scaled)
})

ggsave(
  filename = "MeanAbsSHAP_Grid.png",
  plot = wrap_plots(plot_list_bars, ncol = 2),
  width = 12,
  height = 9,
  dpi = 300,
  path = output_dir
)
print(wrap_plots(plot_list_bars, ncol = 2))

save(full_scaled, cluster_boxplots, shap_values_FNConc,
     global_shap_min, global_shap_max, plot_mean_abs_shap, generate_shap_dot_plot_obj,
     file = "FNConc_HierClust_Workflow_Objects.RData")
