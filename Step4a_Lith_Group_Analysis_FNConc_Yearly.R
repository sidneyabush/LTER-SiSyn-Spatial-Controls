###############################################################################
# COMPLETE WORKFLOW: FNConc Cluster Plotting with Dot Plots, Silhouette, & GRID
###############################################################################

## 1. Load Packages & Clear Environment
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)    # For wrap_plots(), plot_annotation()
library(fastshap)
library(RColorBrewer)
library(grid)         # For textGrob() if needed
library(colorspace)
library(cluster)      # silhouette()
library(factoextra)   # fviz_silhouette()
library(forcats)      # fct_recode()

## 2. (Again) Clear environment just to be sure
rm(list = ls())

## 3. Set working and output directories
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Figures"

###############################################################################
# 2. Load Data & Model
###############################################################################
load("FNConc_Yearly_rf_model2.RData")
load("FNConc_Yearly_kept_drivers.RData")
#load("FNConc_Yearly_numeric.RData")
load("FNConc_Yearly_stream_ids.RData")

# Load precomputed SHAP values
load("FNConc_Yearly_shap_values_new.RData")

drivers_full <- read.csv("harmonization_files/All_Drivers_Harmonized_Yearly_FNConc_FNYield_5_years.csv")

drivers_combined <- drivers_df %>%
  inner_join(
    drivers_full %>% 
      dplyr::select(Stream_ID, Year, major_rock, major_land),
    by = c("Stream_ID", "Year")
  ) %>%
  filter(!is.na(major_rock))


###############################################################################
# 3. Consolidate Lithology Categories & Manually Assign Clusters
###############################################################################
drivers_numeric_consolidated_lith <- drivers_combined %>%
  # Remove rows with missing, blank, or "0" in major_rock
  filter(!is.na(major_rock) & trimws(major_rock) != "" & major_rock != "0") %>%
  mutate(
    # Group various string combos into one of four categories
    consolidated_rock = case_when(
      major_rock %in% c(
        "volcanic", 
        "volcanic; plutonic"
      ) ~ "Volcanic",
      major_rock %in% c(
        "sedimentary", 
        "volcanic; sedimentary; carbonate_evaporite",
        "sedimentary; carbonate_evaporite", 
        "sedimentary; plutonic; carbonate_evaporite; metamorphic",
        "sedimentary; metamorphic"
      ) ~ "Sedimentary",
      major_rock %in% c(
        "plutonic", 
        "plutonic; metamorphic",
        "volcanic; plutonic; metamorphic"
      ) ~ "Plutonic",
      major_rock %in% c(
        "metamorphic", 
        "carbonate_evaporite; metamorphic"
      ) ~ "Metamorphic",
      major_rock %in% c(
        "carbonate_evaporite",
        "volcanic; carbonate_evaporite"
      ) ~ "Carbonate Evaporite"
    )
  ) %>%
  mutate(
    # If Sedimentary and ≥70% sed rocks → "Sedimentary"; else "Mixed Sedimentary"
    final_cluster = case_when(
      consolidated_rock == "Sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "Sedimentary" & rocks_sedimentary < 70  ~ "Mixed Sedimentary",
      TRUE ~ consolidated_rock
    )
  ) %>%
  # Manually order clusters
  mutate(
    final_cluster = factor(
      final_cluster, 
      levels = c(
        "Volcanic", "Sedimentary", "Mixed Sedimentary",
        "Plutonic", "Metamorphic", "Carbonate Evaporite"
      )
    )
  )

# Ensure it's a tibble
drivers_numeric_consolidated_lith <- as_tibble(drivers_numeric_consolidated_lith)

# Count rows per cluster
row_counts <- dplyr::count(drivers_numeric_consolidated_lith, final_cluster, name = "total_rows")

# Count unique Stream_IDs per cluster
stream_counts <- drivers_numeric_consolidated_lith %>%
  dplyr::group_by(final_cluster) %>%
  dplyr::summarise(
    unique_stream_ids = dplyr::n_distinct(Stream_ID),
    .groups = "drop"
  )

# Join counts for summary
summary_table <- dplyr::left_join(row_counts, stream_counts, by = "final_cluster")
print(summary_table)

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
  "Plutonic"            = "#8D9A40",
  "Metamorphic"         = "#C26F86",  
  "Carbonate Evaporite" = "#5E88B0"   
)

my_cluster_colors_lighter <- sapply(my_cluster_colors, function(x) lighten(x, amount = 0.3))

###############################################################################
# 6. Create Long-format Data for Box Plots (exclude FNConc & rocks*)
###############################################################################
long_data <- scaled_data %>%
  dplyr::select(-major_rock, -consolidated_rock, -major_land, -Stream_ID, -Year) %>%
  pivot_longer(
    cols       = -final_cluster,
    names_to   = "Driver",
    values_to  = "Value"
  ) %>%
  # FILTER OUT "FNConc" and anything starting with "rocks"
  filter(
    Driver != "FNConc",
    !grepl("^rocks", Driver, ignore.case = TRUE)
  ) %>%
  mutate(
    # First convert "Driver" to a factor with a specified order (including "recession_slope")
    Driver = factor(
      Driver,
      levels = c(
        "NOx", "P", "precip", "temp", "snow_cover", "npp", "evapotrans",
        "greenup_day", "permafrost", "elevation", "RBFI", "basin_slope",
        "recession_slope",
        "land_Bare", "land_Cropland", "land_Forest",
        "land_Grassland_Shrubland", "land_Ice_Snow", "land_Impervious",
        "land_Salt_Water", "land_Tidal_Wetland", "land_Water", "land_Wetland_Marsh"
      )
    ),
    # Then recode to human-readable labels, leaving anything not specified unchanged
    Driver = fct_recode(
      Driver,
      "Nitrate"                     = "NOx",
      "P"                           = "P",
      "Precip"                      = "precip",
      "Temperature"                 = "temp",
      "Snow Cover"                  = "snow_cover",
      "NPP"                         = "npp",
      "ET"                          = "evapotrans",
      "Greenup Day"                 = "greenup_day",
      "Permafrost"                  = "permafrost",
      "Elevation"                   = "elevation",
      "Flashiness Index"            = "RBFI",
      "Basin Slope"                 = "basin_slope",
      "Recession Curve Slope"       = "recession_slope",
      "Land: Bare"                  = "land_Bare", 
      "Land: Cropland"              = "land_Cropland", 
      "Land: Forest"                = "land_Forest",
      "Land: Grassland & Shrubland" = "land_Grassland_Shrubland", 
      "Land: Ice & Snow"            = "land_Ice_Snow", 
      "Land: Impervious"            = "land_Impervious", 
      "Land: Salt Water"            = "land_Salt_Water",
      "Land: Tidal Wetland"         = "land_Tidal_Wetland", 
      "Land: Water Body"            = "land_Water", 
      "Land: Wetland Marsh"         = "land_Wetland_Marsh"
      # any level not mentioned here remains unchanged
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
    labs(x = NULL, y = NULL, title = NULL) +
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
  dplyr::select(Stream_ID, Year, FNConc, final_cluster)

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
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

###############################################################################
# 9. Silhouette Plot with Factoextra (Remove x-axis elements)
###############################################################################
sil_obj <- silhouette(
  as.numeric(scaled_data$final_cluster),
  dist(scaled_data %>% dplyr::select(
    rocks_volcanic, rocks_sedimentary, 
    rocks_carbonate_evaporite, rocks_metamorphic, 
    rocks_plutonic
  ))
)
mean_sil_value <- mean(sil_obj[, "sil_width"], na.rm = TRUE)

p_sil <- fviz_silhouette(
  sil_obj,
  label   = FALSE,
  palette = c(
    "#AC7B32",  
    "#579C8E",  
    "#89C8A0",
    "#8D9A40",
    "#C26F86",  
    "#5E88B0"   
  )
)
p_sil <- p_sil + guides(color = "none") +
  scale_fill_manual(
    name   = "Cluster",
    values = c(
      "1" = "#AC7B32", "2" = "#579C8E", "3" = "#89C8A0",
      "4" = "#8D9A40", "5" = "#C26F86", "6" = "#5E88B0"
    ),
    labels = c(
      "1" = "Volcanic", "2" = "Sedimentary", "3" = "Mixed Sedimentary",
      "4" = "Plutonic", "5" = "Metamorphic", "6" = "Carbonate Evaporite"
    )
  ) +
  geom_hline(yintercept = mean_sil_value, linetype = "dashed", color = "gray4") +
  annotate(
    "text",
    x = nrow(sil_obj) * 0.8,
    y = mean_sil_value,
    label = paste("Mean =", round(mean_sil_value, 2)),
    color = "gray4",
    vjust = -0.5
  ) +
  labs(
    x = NULL,
    y = "Silhouette Width",
    title = NULL,
    subtitle = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    plot.title   = element_blank(),
    plot.subtitle = element_blank()
  )

print(p_sil)

###############################################################################
# ***** Define the function plot_mean_abs_shap for SHAP bar plots *****
###############################################################################
plot_mean_abs_shap <- function(cluster_id, shap_values_FNConc, full_scaled) {
  # Subset SHAP rows for this cluster
  cluster_indices <- which(full_scaled$final_cluster == cluster_id)
  shap_cluster    <- shap_values_FNConc[cluster_indices, , drop = FALSE]
  
  # Compute mean(|SHAP|) per feature
  mean_abs_shap <- colMeans(abs(shap_cluster), na.rm = TRUE)
  
  # Build a data frame of feature names + their mean(|SHAP|)
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(mean_abs_shapval)) %>%
    # REMOVE "FNConc" and anything starting with "rocks"
    filter(
      feature != "FNConc",
      !grepl("^rocks", feature, ignore.case = TRUE)
    )
  
  # FIXED: Use the same recoding approach
  feature_recode_map <- c(
    "NOx" = "Nitrate", "P" = "P", "precip" = "Precip", "temp" = "Temperature",
    "snow_cover" = "Snow Cover", "npp" = "NPP", "evapotrans" = "ET",
    "greenup_day" = "Greenup Day", "permafrost" = "Permafrost",
    "elevation" = "Elevation", "RBFI" = "Flashiness Index", "RBI" = "Flashiness Index",
    "basin_slope" = "Basin Slope", "recession_slope" = "Recession Curve Slope",
    "land_Bare" = "Land: Bare", "land_Cropland" = "Land: Cropland", 
    "land_Forest" = "Land: Forest", "land_Grassland_Shrubland" = "Land: Grassland & Shrubland",
    "land_Ice_Snow" = "Land: Ice & Snow", "land_Impervious" = "Land: Impervious",
    "land_Salt_Water" = "Land: Salt Water", "land_Tidal_Wetland" = "Land: Tidal Wetland",
    "land_Water" = "Land: Water Body", "land_Wetland_Marsh" = "Land: Wetland Marsh"
  )
  
  df_shap <- df_shap %>%
    mutate(
      feature_recoded = ifelse(
        feature %in% names(feature_recode_map),
        feature_recode_map[feature],
        feature
      ),
      feature_recoded = factor(feature_recoded, levels = feature_recoded)
    )
  
  # Draw the horizontal bar-plot
  ggplot(df_shap, aes(x = reorder(feature_recoded, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(stat = "identity", fill = my_cluster_colors[[as.character(cluster_id)]], alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1.3)) +
    labs(x = NULL, y = "Mean Absolute SHAP Value", title = NULL) +
    theme_classic(base_size = 14) +
    theme(
      plot.title  = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
}


###############################################################################
# 10. SHAP Dot Plots (Remove Titles from Each Dot Plot; include recession_slope)
###############################################################################
full_scaled <- scaled_data

global_min <- min(full_scaled %>% dplyr::select(where(is.numeric)), na.rm = TRUE)
global_max <- max(full_scaled %>% dplyr::select(where(is.numeric)), na.rm = TRUE)

generate_shap_dot_plot_obj <- function(cluster_name, shap_values_FNConc, full_scaled,
                                       global_shap_min, global_shap_max) {
  # (1) Subset to this cluster
  cluster_indices <- which(full_scaled$final_cluster == cluster_name)
  cluster_data    <- full_scaled[cluster_indices, , drop = FALSE] %>%
    dplyr::select(where(is.numeric))
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  # (2) Pivot feature values long
  cluster_long <- cluster_data %>%
    pivot_longer(
      cols      = -id,
      names_to  = "feature",
      values_to = "feature_value"
    )
  
  # (3) Pivot SHAP values long - FIXED: Make sure column names match
  shap_values_FNConc_df <- as.data.frame(shap_values_FNConc)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  # DEBUG: Print column names to verify
  cat("SHAP columns:", colnames(shap_values_FNConc_df), "\n")
  cat("Cluster data columns:", colnames(cluster_data), "\n")
  
  shap_long <- shap_values_FNConc_df %>%
    pivot_longer(
      cols      = -id,
      names_to  = "feature",
      values_to = "shap_value"
    ) %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  # (4) Remove any "rocks…" features and FNConc
  shap_long <- shap_long %>%
    filter(
      !grepl("^rocks", feature, ignore.case = TRUE),
      feature != "FNConc"
    )
  
  # DEBUG: Check for NAs before recoding
  cat("Features before recoding:", unique(shap_long$feature), "\n")
  cat("Any NAs in shap_value?", any(is.na(shap_long$shap_value)), "\n")
  cat("Any NAs in feature_value?", any(is.na(shap_long$feature_value)), "\n")
  
  # (5) Compute overall importance ordering
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_abs_shap))
  
  # (6) Turn feature into a factor in that order
  shap_long <- shap_long %>%
    mutate(
      feature = factor(feature, levels = rev(overall_feature_importance$feature))
    )
  
  # ===== FIXED RECODING BLOCK =====
  # Create a named vector for recoding
  feature_recode_map <- c(
    # Climate/Environmental
    "NOx" = "Nitrate",
    "P" = "P",
    "precip" = "Precip", 
    "temp" = "Temperature",
    "snow_cover" = "Snow Cover",
    "npp" = "NPP",
    "evapotrans" = "ET",
    "greenup_day" = "Greenup Day",
    "permafrost" = "Permafrost",
    
    # Topographic/Hydrologic
    "elevation" = "Elevation",
    "RBFI" = "Flashiness Index",  # Note: Check if this is RBFI or RBI in your data!
    "RBI" = "Flashiness Index",   # Alternative name
    "basin_slope" = "Basin Slope",
    "recession_slope" = "Recession Curve Slope",
    
    # Land Cover
    "land_Bare" = "Land: Bare",
    "land_Cropland" = "Land: Cropland", 
    "land_Forest" = "Land: Forest",
    "land_Grassland_Shrubland" = "Land: Grassland & Shrubland",
    "land_Ice_Snow" = "Land: Ice & Snow",
    "land_Impervious" = "Land: Impervious",
    "land_Salt_Water" = "Land: Salt Water",
    "land_Tidal_Wetland" = "Land: Tidal Wetland",
    "land_Water" = "Land: Water Body",
    "land_Wetland_Marsh" = "Land: Wetland Marsh"
  )
  
  # Apply recoding using ifelse to avoid NAs
  shap_long <- shap_long %>%
    mutate(
      feature_recoded = ifelse(
        as.character(feature) %in% names(feature_recode_map),
        feature_recode_map[as.character(feature)],
        as.character(feature)  # Keep original name if not in map
      )
    ) %>%
    # Reorder based on importance but with new names
    mutate(
      feature_recoded = factor(feature_recoded, 
                               levels = rev(unique(feature_recoded)))
    )
  
  # DEBUG: Check recoding results
  cat("Original vs Recoded features:\n")
  print(data.frame(
    original = levels(shap_long$feature),
    recoded = levels(shap_long$feature_recoded)
  ))
  
  # (7) Draw the dot plot using the recoded feature names
  ggplot(shap_long, aes(x = shap_value, y = feature_recoded, fill = feature_value)) +
    geom_point(
      alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black"
    ) +
    scale_fill_gradientn(
      colors = c("white", "gray", "black"),
      name   = NULL,
      limits = c(global_shap_min, global_shap_max)
    ) +
    labs(x = "SHAP Value", y = NULL, title = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_x_continuous(limits = c(global_shap_min, global_shap_max)) +
    theme_classic() +
    theme(
      axis.title      = element_text(size = 14, face = "bold"),
      axis.text       = element_text(size = 12),
      legend.text     = element_text(size = 12),
      legend.title    = element_text(size = 14),
      legend.key.size = unit(1.5, "lines"),
      plot.title      = element_blank()
    )
}


global_shap_min <- min(shap_values_FNConc, na.rm = TRUE)
global_shap_max <- max(shap_values_FNConc, na.rm = TRUE)

dot_plots <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(
    cl,
    shap_values_FNConc,
    full_scaled,
    global_shap_min,
    global_shap_max
  )
})

# Remove x-axis elements from all but the bottom-most dot plot
for (i in seq_along(dot_plots)) {
  if (i < length(dot_plots)) {
    dot_plots[[i]] <- dot_plots[[i]] +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
}

###############################################################################
# 11. Combine Box Plots & SHAP Dot Plots with patchwork (No extra titles)
###############################################################################
left_col  <- wrap_plots(cluster_boxplots, ncol = 1) & labs(y = "Scaled Value")
right_col <- wrap_plots(dot_plots, ncol = 1)

final_combined_plot <- left_col | right_col +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    title = NULL,
    caption = NULL,
    theme = theme(
      plot.tag          = element_text(size = 30, face = "bold"),
      plot.tag.position = "topleft"
    )
  )

###############################################################################
# 12. Save Final Figures
###############################################################################
ggsave(
  filename = "Fig4_FNConc_Cluster_Boxplot_SHAP_DotPlots.png",
  plot     = final_combined_plot,
  width    = 16,
  height   = 18,
  dpi      = 300,
  path     = output_dir
)
print(final_combined_plot)

ggsave(
  filename = "Fig5_FNConc_Yearly_Clusters.png",
  plot     = p_FNConc,
  width    = 8,
  height   = 5,
  dpi      = 300,
  path     = output_dir
)
print(p_FNConc)

ggsave(
  filename = "FigSX_FNConc_Sil.png",
  plot     = p_sil,
  width    = 10,
  height   = 6,
  dpi      = 300,
  path     = output_dir
)
print(p_sil)

# 13. Create and save SHAP bar‐plot grid
unique_clusters_for_shap <- levels(full_scaled$final_cluster)
plot_list_bars <- lapply(unique_clusters_for_shap, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNConc, full_scaled)
})

ggsave(
  filename = "FigSX_FNConc_MeanAbsSHAP_Grid.png",
  plot     = wrap_plots(plot_list_bars, ncol = 2),
  width    = 12,
  height   = 9,
  dpi      = 300,
  path     = output_dir
)
print(wrap_plots(plot_list_bars, ncol = 2))

# 14. Save workspace objects if needed
save(
  full_scaled,
  cluster_boxplots,
  shap_values_FNConc,
  global_shap_min,
  global_shap_max,
  plot_mean_abs_shap,
  generate_shap_dot_plot_obj,
  file = "FNConc_HierClust_Workflow_Objects.RData"
)
