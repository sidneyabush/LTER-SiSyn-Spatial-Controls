###############################################################################
# 1. Load Packages & Clear Environment
###############################################################################
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
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"

###############################################################################
# 2. Load Data & Model
###############################################################################
load("FNYield_Yearly_rf_model2_full_new.RData")
load("FNYield_Yearly_kept_drivers_full_new.RData")
load("FNYield_Yearly_full_new.RData")
load("FNYield_Yearly_full_stream_ids_new.RData")

# Load precomputed SHAP values
load("FNYield_Yearly_shap_values_new.RData")

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
long_data <- scaled_data %>%
  dplyr::select(
    -major_rock, -consolidated_rock, -major_land,
    -Stream_ID, -Year
  ) %>%
  pivot_longer(-final_cluster, names_to = "Driver", values_to = "Value") %>%
  mutate(
    Driver = factor(
      Driver,
      levels = c(
        "FNYield",
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
    scale_y_continuous(limits = c(0, 1)) +  # Force axis 0..1
    labs(
      x = NULL,
      y = NULL,
      title = paste("Cluster", cl)
    ) +
    theme_classic() +
    theme(
      plot.title  = element_text(size = 14, hjust = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
      axis.text.y = element_text(size = 14)
    )
  # Remove x-axis labels for all but the last plot
  if (cl != tail(unique_clusters, 1)) {
    p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  return(p)
})

###############################################################################
# 8. Box Plot of FNYield by Manually Assigned Cluster
###############################################################################
df <- scaled_data %>%
  dplyr::select(Stream_ID, Year, FNYield, final_cluster)

write.csv(
  df,
  file = file.path(output_dir, "FNYield_Stream_ID_Year_Cluster.csv"),
  row.names = FALSE
)

p_FNYield <- ggplot(df, aes(x = final_cluster, y = FNYield, fill = final_cluster)) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = final_cluster), width = 0.3, alpha = 0.4, size = 2) +
  scale_fill_manual(values = my_cluster_colors_lighter) +
  scale_color_manual(values = my_cluster_colors) +
  labs(
    x = "Cluster",
    y = expression(DSi~Yield~(kg~m^{-2}~y^{-1}))
  ) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

###############################################################################
# 9. Silhouette-Like Plot with Custom ggplot (Using final_cluster)
###############################################################################
library(cluster)
library(factoextra)
library(ggplot2)

# 1) Compute silhouette with numeric clusters
sil_obj <- silhouette(
  as.numeric(scaled_data$final_cluster),  # silhouette requires numeric
  dist(
    scaled_data %>% 
      dplyr::select(rocks_volcanic, rocks_sedimentary, rocks_carbonate_evaporite,
                    rocks_metamorphic, rocks_plutonic)
  )
)

# 2) Create the fviz_silhouette plot, which initially uses numeric labels (1..5)
p_sil <- fviz_silhouette(
  sil_obj,
  label   = FALSE,        # Hide per-site text labels
  color   = "cluster",    # Color by cluster
  palette = c("#AC7B32", "#579C8E", "#89C8A0", "#C26F86", "#5E88B0")
)

# 3) Remove the default legend(s) that show numeric 1..5
#    Sometimes fviz_silhouette uses color or fill. Remove both just to be safe.
p_sil <- p_sil + guides(color = "none", fill = "none")

# 4) Add a new manual scale that uses your numeric codes (1..5) as keys
#    but displays the cluster names as labels. We apply it to both color & fill
#    to ensure we only get one legend.
p_sil <- p_sil + scale_color_manual(
  name       = "Cluster",
  aesthetics = c("color","fill"),
  values = c(
    "1" = "#AC7B32",  # Volcanic
    "2" = "#579C8E",  # Sedimentary
    "3" = "#89C8A0",  # Mixed Sedimentary
    "4" = "#C26F86",  # Metamorphic
    "5" = "#5E88B0"   # Carbonate_Evaporite
  ),
  labels = c(
    "1" = "Volcanic",
    "2" = "Sedimentary",
    "3" = "Mixed Sedimentary",
    "4" = "Metamorphic",
    "5" = "Carbonate_Evaporite"
  )
)

# 5) (Optional) Add mean silhouette line, etc.
mean_sil_value <- mean(sil_obj[, "sil_width"], na.rm = TRUE)
p_sil <- p_sil +
  geom_hline(
    yintercept = mean_sil_value,
    linetype   = "dashed",
    color      = "gray4"
  ) +
  annotate(
    "text",
    x     = nrow(sil_obj) * 0.8,
    y     = mean_sil_value,
    label = paste("Mean =", round(mean_sil_value, 2)),
    color = "gray4",
    vjust = -0.5
  ) +
  labs(x = "Sites", y = "Silhouette Width") +
  theme_classic(base_size = 16) +
  theme(legend.position = "right")  # place legend on the right

print(p_sil)



###############################################################################
# 10. Mean Absolute SHAP Bar Plots (Using final_cluster)
###############################################################################
# If shap_values_FNYield is an NxM matrix with M features
plot_mean_abs_shap <- function(cluster_id, shap_values_FNYield, full_scaled) {
  cluster_indices <- which(full_scaled$final_cluster == cluster_id)
  shap_cluster    <- shap_values_FNYield[cluster_indices, , drop = FALSE]
  
  mean_abs_shap <- colMeans(abs(shap_cluster), na.rm = TRUE)
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap)
  ) %>%
    arrange(desc(mean_abs_shapval)) %>%
    # Optionally remove 'rock' features if you don't want them in the bar plot
    filter(!grepl("rock", feature, ignore.case = TRUE))
  
  df_shap$feature <- recode(
    df_shap$feature,
    "NOx" = "Nitrate",
    "P"   = "Phosphorous",
    "precip" = "Precip",
    "temp"   = "Temperature",
    "snow_cover" = "Snow Cover",
    "npp"    = "NPP",
    "evapotrans" = "ET",
    "greenup_day" = "Greenup Day",
    "permafrost"  = "Permafrost",
    "elevation"   = "Elevation",
    "basin_slope" = "Basin Slope",
    "FNConc"      = "DSi Concentration",
    "rocks_volcanic" = "Rock: Volcanic",
    "rocks_sedimentary" = "Rock: Sedimentary",
    "rocks_carbonate_evaporite" = "Rock: Carbonate & Evaporite",
    "rocks_metamorphic" = "Rock: Metamorphic",
    "rocks_plutonic"    = "Rock: Plutonic",
    "land_tundra" = "Land: Tundra",
    "land_barren_or_sparsely_vegetated" = "Land: Barren & Sparsely Vegetated",
    "land_cropland" = "Land: Cropland",
    "land_shrubland_grassland" = "Land: Shrubland & Grassland",
    "land_urban_and_built_up_land" = "Land: Urban & Built-up",
    "land_wetland" = "Land: Wetland",
    "land_forest_all" = "Land: Forest"
  )
  
  ggplot(df_shap, aes(x = reorder(feature, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(
      stat  = "identity",
      fill  = my_cluster_colors[[as.character(cluster_id)]],
      alpha = 0.8
    ) +
    coord_flip() +
    # Adjust limits as needed for your data
    scale_y_continuous(limits = c(0, 6500)) +
    labs(x = NULL, y = "Mean Absolute SHAP Value", title = paste("Cluster", cluster_id)) +
    theme_classic(base_size = 14) +
    theme(
      plot.title  = element_text(size = 14, hjust = 0.5),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
}

# We'll rescale the data again for shap if needed, or just re-use scaled_data
full_scaled <- scaled_data

unique_clusters_shap <- sort(unique(full_scaled$final_cluster))
plot_list <- lapply(unique_clusters_shap, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNYield, full_scaled)
})

ncol <- 2
nrow <- ceiling(length(unique_clusters_shap)/ncol)
final_shap_grid <- wrap_plots(plot_list, ncol = ncol, nrow = nrow)

###############################################################################
# 11. Combine & Save Plots
###############################################################################
# Combine cluster boxplots for drivers
left_col <- wrap_plots(cluster_boxplots, ncol = 1) & labs(y = "Scaled Value")

# Optionally combine with other plots, e.g., a placeholder for a SHAP dot plot
# right_col <- some_shap_dot_plot_code

# final_combined_plot <- left_col | right_col +
#   plot_layout(guides = "collect") +
#   plot_annotation(
#     tag_levels = "A",
#     theme = theme(
#       plot.tag = element_text(size = 30, face = "bold"),
#       plot.tag.position = "topleft"
#     )
#   )

# For demonstration, we’ll just print and save the separate objects:
ggsave(
  filename = "Cluster_Boxplots_for_Drivers.png",
  plot = left_col,
  width = 12,
  height = 18,
  dpi = 300,
  path = output_dir
)

ggsave(
  filename = "FNYield_Yearly_Clusters.png",
  plot = p_FNYield,
  width = 8,
  height = 5,
  dpi = 300,
  path = output_dir
)

ggsave(
  filename = "Custom_Silhouette_Plot.png",
  plot = p_custom_sil,
  width = 10,
  height = 6,
  dpi = 300,
  path = output_dir
)

ggsave(
  filename = "MeanAbsSHAP_Grid.png",
  plot = final_shap_grid,
  width = 12,
  height = 9,
  dpi = 300,
  path = output_dir
)

# Print final objects to screen
print(left_col)
print(p_FNYield)
print(p_custom_sil)
print(final_shap_grid)
