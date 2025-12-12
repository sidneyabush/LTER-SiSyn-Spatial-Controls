# #############################################################################
# Figure: Land cover map with CQ slope boxplot
# #############################################################################
# Modified from Fig1_lithology_FNConc_FNYield.R
# Changes:
#   - Uses sizer_outs_with_drivers_6Aug25.csv
#   - Colors map by major_land instead of lithology
#   - Replaces panels b & c with single panel showing slope_estimate by major_land
# #############################################################################

rm(list = ls())

librarian::shelf(dplyr, stringr, ggplot2, maps, patchwork, scales, colorspace, ggrepel,
                 ggspatial, sf, ggpubr, cowplot)

# Allow an external caller to set `run_mode` before sourcing this file.
# Valid values: "both" (default), "canon" (only major_land), "ext" (only major_land_ext)
if (!exists("run_mode")) {
  run_mode <- Sys.getenv("FIG_MODE", "both")
}

# #############################################################################
# 1. Data Preparation
# #############################################################################

# Load sizer data
data_file <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map/sizer_outs_with_drivers_6Aug25.csv"
sizer_data <- read.csv(data_file, stringsAsFactors = FALSE)
unique_landcover <- unique(sizer_data$major_land)
print(unique_landcover)

land_cover_check <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map/all-data_si-extract_2_20250325.csv"
all_spatial_data <- read.csv(land_cover_check, stringsAsFactors = FALSE)

# Standardize 'East Fork' and 'West Fork' capitalization in Stream_Name for both datasets
sizer_data$Stream_Name <- gsub("(?i)east fork", "East Fork", sizer_data$Stream_Name, perl = TRUE)
sizer_data$Stream_Name <- gsub("(?i)west fork", "West Fork", sizer_data$Stream_Name, perl = TRUE)
all_spatial_data$Stream_Name <- gsub("(?i)east fork", "East Fork", all_spatial_data$Stream_Name, perl = TRUE)
all_spatial_data$Stream_Name <- gsub("(?i)west fork", "West Fork", all_spatial_data$Stream_Name, perl = TRUE)

sizer_df <- sizer_data %>%
  dplyr::group_by(LTER, Stream_Name) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(LTER, Stream_Name, latitude, longitude, precipitation, slope_estimate, drainage_area, major_land) 

# Manually set major_land to 'Forest' for East Fork and West Fork
sizer_df <- sizer_df %>%
  dplyr::mutate(major_land = ifelse(Stream_Name %in% c("East Fork", "West Fork"), "Forest", major_land))

# Extract LULC_spatial_data for comparison of major land classes
LULC_spatial_data <- all_spatial_data %>%
  dplyr::select(LTER, Stream_Name, major_land) %>%
  dplyr::rename(major_land_ext = major_land) %>%
  dplyr::group_by(LTER, Stream_Name) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

# Standardize land cover class names in major_land_ext
LULC_spatial_data <- LULC_spatial_data %>%
  dplyr::mutate(
    major_land_ext = dplyr::case_when(
      grepl("_forest", tolower(major_land_ext)) ~ "Forest",
      tolower(major_land_ext) == "tundra" ~ "Tundra",
      tolower(major_land_ext) == "shrubland_grassland" ~ "Grassland_Shrubland",
      tolower(major_land_ext) == "cropland" ~ "Cropland",
      tolower(major_land_ext) == "wetland_marsh" ~ "Wetland_Marsh",
      tolower(major_land_ext) == "urban_and_built_up_land" ~ "Impervious",
      TRUE ~ major_land_ext
    )
  )

# Create a combined data frame for comparison (no duplicated columns)
cols_to_add <- setdiff(names(LULC_spatial_data), c("LTER", "Stream_Name", names(sizer_df)))

combined_df <- dplyr::left_join(
  sizer_df,
  LULC_spatial_data[, c("LTER", "Stream_Name", cols_to_add)],
  by = c("LTER", "Stream_Name")
)

# Ensure combined_df is limited to what is in sizer_df (should already be the case with left_join, but enforce explicitly)
combined_df <- combined_df %>% 
  dplyr::semi_join(sizer_df, by = c("LTER", "Stream_Name")) %>%
  dplyr::mutate(
    major_land = ifelse(LTER == "MCM", "Ice", major_land),
    major_land_ext = ifelse(LTER == "MCM", "Ice", major_land_ext),
    land_match = major_land == major_land_ext
  )

# Ensure precipitation is set for MCM (manual override requested)
combined_df$precipitation[combined_df$LTER == "MCM"] <- 0.4

# Export combined_df as CSV for checking in the specified directory
combined_df <- combined_df %>% dplyr::mutate(across(everything(), ~na_if(trimws(.), "")))
# write.csv(combined_df, file = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map/LULC_site_check.csv", row.names = FALSE)

#--- End land-use checks---
# Ensure longitude and latitude are numeric for plotting
combined_df$longitude <- as.numeric(combined_df$longitude)
combined_df$latitude <- as.numeric(combined_df$latitude)
# Check unique land cover values
unique_landcover <- unique(combined_df$major_land)
print(unique_landcover)

unique_landcover <- unique(combined_df$major_land_ext)
print(unique_landcover)

# Ensure slope_estimate is numeric for boxplots
combined_df$slope_estimate <- as.numeric(combined_df$slope_estimate)


landcover_colors <- c(
  "Forest"              = "#355841",
  "Grassland_Shrubland" = "#A1A86B",
  "Cropland"            = "#CEB24C",
  "Impervious"          = "#4F4F4F",
  "Bare"                = "#C48A72",
  "Wetland_Marsh"       = "#4A6F84",
  "Ice"                 = "#8BAFCF"
)


# Add Tundra color so external runs can show it directly
landcover_colors["Tundra"] <- "#4A6F84"


# Desired legend order and display labels (original order)
desired_land_order <- c("Forest", "Grassland_Shrubland", "Cropland", "Wetland_Marsh", "Impervious", "Bare", "Ice")
display_labels <- c(
  "Forest" = "Forest",
  "Grassland_Shrubland" = "Grassland",
  "Cropland" = "Cropland",
  "Wetland_Marsh" = "Wetland",
  "Impervious" = "Impervious",
  "Bare" = "Bare",
  "Ice" = "Ice",
  "Tundra" = "Tundra"
)

# Define shape mapping (named) for the desired order.
shape_values_all <- c(
  "Forest" = 21,
  "Grassland_Shrubland" = 22,
  "Cropland" = 18,
  "Wetland_Marsh" = 15,
  "Impervious" = 24,
  "Bare" = 25,
  "Ice" = 21,
  "Tundra" = 23
)

# Base and jitter size mappings
point_size_base <- c(
  "Forest" = 2,
  "Grassland_Shrubland" = 2,
  "Cropland" = 2,
  "Wetland_Marsh" = 2,
  "Impervious" = 2,
  "Bare" = 2,
  "Ice" = 2,
  "Tundra" = 2
)
point_size_jitter <- point_size_base
# Make jitter points slightly larger for better visibility
point_size_jitter[] <- 2.5
# Ensure Impervious jitter uses the same enlarged size
point_size_jitter["Impervious"] <- 2.5
# Make circle-shaped jitter points a bit smaller
point_size_jitter["Forest"] <- 2.0
point_size_jitter["Ice"] <- 2.0

# Stroke map for legend and overlays
stroke_map <- c(
  "Forest" = 0.12,
  "Grassland_Shrubland" = 0.12,
  "Cropland" = 0.12,
  "Wetland_Marsh" = 0.12,
  "Impervious" = 0.0,
  "Bare" = 0.12,
  "Ice" = 0.12,
  "Tundra" = 0.12
)

# Map-specific shape mapping: make Wetland the same square shape as Grassland on the map only
shape_map_values <- shape_values_all
shape_map_values["Wetland_Marsh"] <- shape_values_all["Grassland_Shrubland"]

# Legend size mapping: default 4, but make triangles (Impervious, Bare) a bit smaller
legend_size_map <- rep(4, length(shape_map_values))
names(legend_size_map) <- names(shape_map_values)
legend_size_map["Impervious"] <- 3.0
legend_size_map["Bare"] <- 3.0

# Filter colors to only those present in the data, preserving desired order
present_landcovers <- desired_land_order[desired_land_order %in% unique(combined_df$major_land)]
my_landcover_colors <- landcover_colors[present_landcovers]
display_labels_present <- unname(display_labels[present_landcovers])

# Create factor for legend ordering using the desired order
combined_df <- combined_df %>%
  dplyr::mutate(
    landcover_factor = factor(major_land, levels = present_landcovers)
  ) %>%
  dplyr::filter(!is.na(landcover_factor))
# Plotting ----------------------------- 
# Options: choose either "precipitation" or "drainage_area" for the x-variable

panel_c_var <- "precipitation"  # set to either "precipitation" or "drainage_area"

# Prepare variable-specific vector and label
if (panel_c_var == "precipitation") {
  combined_df$precipitation <- as.numeric(combined_df$precipitation)
  var_vec <- combined_df$precipitation
  var_label <- "Precipitation (mm day\u207B\u00B9)" 
} else if (panel_c_var == "drainage_area") {
  combined_df$drainage_area <- as.numeric(combined_df$drainage_area)
  var_vec <- combined_df$drainage_area
  var_label <- expression(paste("Drainage area (km"^2, ")"))
}

# Compute quartile breaks and labels
var_breaks <- unname(quantile(var_vec, probs = seq(0, 1, 0.25), na.rm = TRUE))

# If plotting drainage area, round breaks to whole numbers (km^2)
if (panel_c_var == "drainage_area") {
  var_breaks <- round(var_breaks, 0)
}

var_labels <- vapply(seq_len(length(var_breaks)-1), function(i) {
  lo <- var_breaks[i]
  hi <- var_breaks[i+1]

  if (panel_c_var == "drainage_area") {
    lo_f <- formatC(lo, format = "f", big.mark = ",", digits = 0)
    hi_f <- formatC(hi, format = "f", big.mark = ",", digits = 0)
  } else {
    lo_f <- formatC(round(lo, 1), format = "f", digits = 1)
    hi_f <- formatC(round(hi, 1), format = "f", digits = 1)
  }

  if (i == 1) {
    paste0("\u2264 ", hi_f)
  } else {
    paste0(lo_f, " – ", hi_f)
  }
}, FUN.VALUE = character(1))

# #############################################################################
# 2. Global Map (Panel A)
# #############################################################################
world <- map_data("world")

global_base <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  coord_sf(crs = st_crs(3857), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background     = element_rect(fill = "white", color = NA),
    plot.background      = element_rect(fill = "white", color = NA),
    panel.grid           = element_blank(),
    axis.title           = element_blank(),
    axis.text            = element_blank(),
    axis.ticks           = element_blank(),
    legend.position      = c(0.05, 0.65),
    legend.justification = c("left","top"),
    legend.text          = element_text(size = 12),
    legend.title         = element_blank()
  )

global_map <- global_base +
  # Draw non-forest points with the standard size/alpha
  geom_point(
    data = combined_df %>% dplyr::filter(landcover_factor != "Forest"),
    aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor),
    size = 2.3, alpha = 0.60, stroke = 0.12
  ) +
  # Draw Forest (circle) points slightly smaller and a bit more transparent
  geom_point(
    data = combined_df %>% dplyr::filter(landcover_factor == "Forest"),
    aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor),
    size = 2.0, alpha = 0.45, stroke = 0.12
  ) +
  scale_fill_manual(name = "Land Cover", values = my_landcover_colors, labels = display_labels_present, drop = FALSE) +
  scale_color_manual(name = "Land Cover", values = my_landcover_colors, labels = display_labels_present, drop = FALSE) +
  scale_shape_manual(name = "Land Cover", values = shape_map_values[present_landcovers], labels = display_labels_present, drop = FALSE) +
  guides(
    fill = guide_legend(override.aes = list(shape = unname(shape_map_values[present_landcovers]), fill = unname(my_landcover_colors), colour = unname(my_landcover_colors), size = unname(legend_size_map[present_landcovers]), alpha = 1.0, stroke = unname(stroke_map[present_landcovers]))),
    shape = "none",
    color = "none"
  )

## Add emphasis overlays: thick Wetland points and filled Impervious triangles (no outline)
global_map <- global_map +
  geom_point(
    data = combined_df %>% dplyr::filter(landcover_factor == "Wetland_Marsh"),
    aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
    size = 2.3, stroke = 1, alpha = 0.90, inherit.aes = FALSE
  ) +
  
  geom_point(
    data = combined_df %>% dplyr::filter(landcover_factor == "Impervious"),
    aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
    size = 2.3, stroke = 0, alpha = 0.50, inherit.aes = FALSE
  )
  # Emphasize Grassland points slightly by drawing them on top with higher alpha
  global_map <- global_map +
    geom_point(
      data = combined_df %>% dplyr::filter(landcover_factor == "Grassland_Shrubland"),
      aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
      size = 2.3, stroke = 0.12, alpha = 0.70, inherit.aes = FALSE
    )

# #############################################################################
# 3) Regional Insets
# #############################################################################
create_regional_map <- function(xlim, ylim, data_df) {
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "lightgray", color = "white") +
    geom_point(
      data = data_df %>% dplyr::filter(landcover_factor != "Forest"),
      aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor),
      size = 2.3, alpha = 0.60, stroke = 0.12
    ) +
    geom_point(
      data = data_df %>% dplyr::filter(landcover_factor == "Forest"),
      aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor),
      size = 2.0, alpha = 0.45, stroke = 0.12
    ) +
    scale_fill_manual(values = my_landcover_colors, drop = FALSE, guide = "none") +
    scale_color_manual(values = my_landcover_colors[names(my_landcover_colors)], drop = FALSE, guide = "none") +
    scale_shape_manual(values = shape_map_values[names(my_landcover_colors)], drop = FALSE, guide = "none") +
    # Overlay Impervious in the inset as a filled triangle without outline
        geom_point(data = data_df %>% dplyr::filter(landcover_factor == "Impervious"),
          aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
         size = 2.3, stroke = 0, alpha = 0.90, inherit.aes = FALSE) +
        # Overlay Grassland in the inset as a slightly more opaque square
            geom_point(data = data_df %>% dplyr::filter(landcover_factor == "Grassland_Shrubland"),
              aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
              size = 2.3, stroke = 0.12, alpha = 0.70, inherit.aes = FALSE) +
    coord_sf(xlim = xlim, ylim = ylim, crs = st_crs(3857), expand = FALSE) +
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, size = 0.5))
}

# Define inset extents
uk_xlim          <- c(-10, 2);   uk_ylim <- c(49, 61)
scandinavia_xlim <- c(4, 30);    scandinavia_ylim <- c(50, 72)
australia_xlim   <- c(135,155);  australia_ylim   <- c(-40,-28)

# Subset for insets
df_uk <- filter(combined_df, longitude >= uk_xlim[1], longitude <= uk_xlim[2], latitude >= uk_ylim[1], latitude <= uk_ylim[2])
df_scandinavia <- filter(combined_df, longitude >= scandinavia_xlim[1], longitude <= scandinavia_xlim[2], latitude >= scandinavia_ylim[1], latitude <= scandinavia_ylim[2])
df_australia <- filter(combined_df, longitude >= australia_xlim[1], longitude <= australia_xlim[2], latitude >= australia_ylim[1], latitude <= australia_ylim[2])

uk_inset <- create_regional_map(uk_xlim, uk_ylim, df_uk)
scandinavia_inset <- create_regional_map(scandinavia_xlim, scandinavia_ylim, df_scandinavia)
australia_inset <- create_regional_map(australia_xlim, australia_ylim, df_australia)

final_map <- ggdraw(global_map) +
  draw_plot(uk_inset, x = 0.345, y = 0.447, width = 0.25, height = 0.25) +
  draw_plot(scandinavia_inset, x = 0.524, y = 0.47, width = 0.25, height = 0.25) +
  draw_plot(australia_inset, x = 0.56, y = 0.12, width = 0.25, height = 0.25) +
  draw_line(x = c(0.419,0.48), y = c(0.68,0.71), color = "black", size = 0.7) +
  draw_line(x = c(0.54,0.58), y = c(0.72,0.66), color = "black", size = 0.7) +
  draw_line(x = c(0.788,0.85), y = c(0.26,0.32), color = "black", size = 0.7)

p_map_labeled <- final_map +
  labs(tag = "a)") +
  theme(plot.tag = element_text(size = 16, hjust = 0, vjust = 1, face = "plain"),
        plot.tag.position = c(0.02, 0.98))

# #############################################################################
# 4) CQ Slope Boxplot (Panel B)
# #############################################################################

# Set seed for reproducible jitter
set.seed(42)

p_slope <- ggplot(combined_df, aes(x = slope_estimate, y = landcover_factor)) +
  geom_boxplot(aes(fill = landcover_factor), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(fill = landcover_factor, color = landcover_factor, shape = landcover_factor, size = landcover_factor),
              width = 0, height = 0.2, stroke = 0.12, alpha = 0.75) +
  scale_fill_manual(values = my_landcover_colors) +
  scale_color_manual(values = my_landcover_colors) +
  scale_shape_manual(values = shape_values_all[present_landcovers]) +
  scale_size_manual(values = point_size_jitter[present_landcovers], guide = "none") +
  labs(x = "CQ Slope Estimate", y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(present_landcovers), labels = rev(display_labels_present)) +
    theme(legend.position = "none",
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8)))

# Overlay jitter points for plus/asterisk categories with thicker lines only
p_slope <- p_slope +
  # Emphasize wetland points with thicker outlines
  geom_jitter(data = combined_df %>% dplyr::filter(landcover_factor == "Wetland_Marsh"),
              aes(x = slope_estimate, y = landcover_factor, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
              width = 0, height = 0.2, size = point_size_jitter["Wetland_Marsh"], stroke = 1, alpha = 0.98, inherit.aes = FALSE) +
  # Draw Impervious as a filled triangle in the jitter with no outline
  geom_jitter(data = combined_df %>% dplyr::filter(landcover_factor == "Impervious"),
              aes(x = slope_estimate, y = landcover_factor, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
              width = 0, height = 0.2, size = point_size_jitter["Impervious"], stroke = 0, alpha = 0.75, inherit.aes = FALSE)

p_slope_labeled <- p_slope +
  labs(tag = "b)") +
  theme(plot.tag = element_text(size = 16, hjust = 0))

# -----------------------------
# 4b) Panel C: Stacked proportion bars by quantiles of a selectable variable
# Options: choose either "precipitation" or "drainage_area" for the x-variable
panel_c_var <- "precipitation"  # set to either "precipitation" or "drainage_area"

# Prepare variable-specific vector and label
if (panel_c_var == "precipitation") {
  combined_df$precipitation <- as.numeric(combined_df$precipitation)
  var_vec <- combined_df$precipitation
  var_vec <- combined_df$precipitation
  # Use plotmath expression for mm day^-1 (superscript -1)
  var_label <- expression(paste("Precipitation (", mm~day^{-1}, ")"))
} else if (panel_c_var == "drainage_area") {
  combined_df$drainage_area <- as.numeric(combined_df$drainage_area)
  var_vec <- combined_df$drainage_area
  var_label <- "Drainage area (units)"
} else {
  stop("panel_c_var must be either 'precipitation' or 'drainage_area'")
}

# Compute quartile breaks and labels using the chosen variable
var_breaks <- unname(quantile(var_vec, probs = seq(0, 1, 0.25), na.rm = TRUE))
var_labels <- vapply(seq_len(length(var_breaks)-1), function(i) {
  lo <- round(var_breaks[i], 1)
  hi <- round(var_breaks[i+1], 1)
  if (i == 1) {
    paste0("\u2264 ", hi)
  } else {
    paste0(lo, " - ", hi)
  }
}, FUN.VALUE = character(1))

# Create a temporary data frame for panel C only. If using drainage_area, drop NAs for that variable here only.
panel_df <- combined_df
if (panel_c_var == "drainage_area") {
  panel_df <- panel_df %>% dplyr::filter(!is.na(drainage_area))
}

# Add a unified 'panel_bin' factor column with the computed labels
panel_df <- panel_df %>%
  dplyr::mutate(
    panel_var = if (panel_c_var == "precipitation") precipitation else drainage_area,
    panel_bin = cut(panel_var, breaks = var_breaks, include.lowest = TRUE, labels = var_labels)
  )

# Summarize counts and proportions by panel_bin
quartile_summary <- panel_df %>%
  dplyr::group_by(panel_bin, major_land) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(panel_bin) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()

# Ensure quartile colors respect desired order and force factor levels so stacking matches legend
present_quartile_land <- desired_land_order[desired_land_order %in% unique(quartile_summary$major_land)]
quartile_colors <- landcover_colors[present_quartile_land]
# Force factor levels for major_land to preserve stacking order
quartile_summary$major_land <- factor(quartile_summary$major_land, levels = present_quartile_land)

# Stacked proportion bar plot (panel C) using unified panel_bin
 p_third <- ggplot(quartile_summary, aes(x = panel_bin, y = prop, fill = major_land)) +
  geom_bar(stat = "identity", position = "fill", color = "gray20") +
  scale_fill_manual(values = quartile_colors, breaks = present_quartile_land, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = var_label, y = "Proportion of sites") +
    theme_classic(base_size = 14) +
    theme(legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1))

p_third_labeled <- p_third +
  labs(tag = "c)") +
  theme(plot.tag = element_text(size = 16, hjust = 0))

# #############################################################################
# 5) Combine panels
# #############################################################################

final_boxplots <- p_slope_labeled + p_third_labeled

combined_figure <- ggarrange(
  p_map_labeled,
  final_boxplots,
  ncol    = 1,
  nrow    = 2,
  heights = c(3, 2.5),
  align   = "v",
  labels  = NULL
)

# #############################################################################
# 6) Export Figures
# #############################################################################

# Set output directory to CQ_Site_Map for site map export
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map"

# Save as PNG for viewing with suffix based on selected panel variable
# e.g. Fig_landcover_cqslope_precipitation.png or Fig_landcover_cqslope_drainage_area.png
file_suffix <- ifelse(panel_c_var == "precipitation", "precipitation",
       ifelse(panel_c_var == "drainage_area", "drainage_area", panel_c_var))
output_file <- file.path(output_dir, paste0("Fig_landcover_cqslope_", file_suffix, ".png"))
ggsave(output_file, combined_figure,
  width = 8, height = 8.5, dpi = 300, bg = "white")
cat("Saved figure to:", output_file, "\n")

# ---------------------------------------------------------------------------
# Also produce figures using the external landcover column (`major_land_ext`).
# We'll create a small helper that builds the three panels for any input df
# (which must contain a `major_land` column to use for grouping) and saves
# both precipitation and drainage_area variants with an optional suffix.
# ---------------------------------------------------------------------------
build_and_save_for_df <- function(df_local, file_suffix = "", use_ext = FALSE) {
  # If requested, use the external landcover grouping (major_land_ext)
  if (use_ext) {
    df_local$major_land <- df_local$major_land_ext
  }
  # determine which landcover categories are present (respect desired order)
  present_landcovers_local <- desired_land_order[desired_land_order %in% unique(df_local$major_land)]
  if (length(present_landcovers_local) == 0) return()
  my_landcover_colors_local <- landcover_colors[present_landcovers_local]
  # keep a named vector of labels; we'll build/override labels later after
  # we compute the desired_local_order and present categories.
  display_labels_local_named <- display_labels[present_landcovers_local]
  display_labels_local <- unname(display_labels_local_named)
  
    # determine desired order for this run. For ext runs where the external data
    # may contain 'Tundra', we want a one-for-one substitution of the Wetland
    # slot to Tundra while preserving the exact same aesthetics.
    desired_local_order <- desired_land_order
    # For ext runs, insert Tundra immediately before Ice (if present).
    if (!is.null(file_suffix) && file_suffix == "_ext") {
      if ("Tundra" %in% unique(df_local$major_land)) {
        desired_local_order <- desired_local_order[desired_local_order != "Tundra"]
        ice_idx <- which(desired_local_order == "Ice")
        if (length(ice_idx) == 0) {
          desired_local_order <- c(desired_local_order, "Tundra")
        } else {
          desired_local_order <- append(desired_local_order, "Tundra", after = ice_idx - 1)
        }
      }
    } else {
      # Non-ext runs: ensure Wetland_Marsh is immediately before Ice so it plots above Ice
      if ("Wetland_Marsh" %in% unique(df_local$major_land)) {
        desired_local_order <- desired_local_order[desired_local_order != "Wetland_Marsh"]
        ice_idx <- which(desired_local_order == "Ice")
        if (length(ice_idx) == 0) {
          desired_local_order <- c(desired_local_order, "Wetland_Marsh")
        } else {
          desired_local_order <- append(desired_local_order, "Wetland_Marsh", after = ice_idx - 1)
        }
      }
    }
  
    # determine which landcover categories are present (respect desired_local_order)
    present_landcovers_local <- desired_local_order[desired_local_order %in% unique(df_local$major_land)]
    if (length(present_landcovers_local) == 0) return()
    my_landcover_colors_local <- landcover_colors[present_landcovers_local]
    # Create local copies of aesthetic mappings and, for ext runs, copy Wetland
    # aesthetics to Tundra so symbols/colors/sizes remain identical.
    landcover_colors_local <- landcover_colors
    shape_values_all_local <- shape_values_all
    shape_map_values_local <- shape_map_values
    point_size_jitter_local <- point_size_jitter
    stroke_map_local <- stroke_map
    legend_size_map_local <- legend_size_map
    # If ext run and Tundra present, make tundra diamond smaller for map and jitter
    if (!is.null(file_suffix) && file_suffix == "_ext" && ("Tundra" %in% present_landcovers_local)) {
      point_size_jitter_local["Tundra"] <- 1.5
      stroke_map_local["Tundra"] <- 0.08
      legend_size_map_local["Tundra"] <- 3.0
    }
  
    # No relabeling or aesthetic copying for Tundra — Tundra should have its
    # own defined aesthetics (if present) and will be handled by the mapping
    # vectors defined earlier in the script.
  
    # subset mappings to only present landcovers in the desired local order
    my_landcover_colors_local <- landcover_colors_local[present_landcovers_local]
    
    # Build display labels for only the present categories. For ext runs where
    # Tundra was introduced, ensure we provide a label (one-for-one swap)
    display_labels_local_named <- display_labels[present_landcovers_local]
    if (!is.null(file_suffix) && file_suffix == "_ext" && ("Tundra" %in% present_landcovers_local)) {
      # Ensure Tundra has an explicit display label (use 'Tundra' if not set)
      if (!("Tundra" %in% names(display_labels_local_named)) || is.na(display_labels_local_named["Tundra"])) {
        display_labels_local_named["Tundra"] <- "Tundra"
      }
    }
    # Replace any remaining NA labels with the category name so legend shows a string
    na_idx <- which(is.na(display_labels_local_named) | display_labels_local_named == "")
    if (length(na_idx) > 0) {
      display_labels_local_named[na_idx] <- names(display_labels_local_named)[na_idx]
    }
    display_labels_local <- unname(display_labels_local_named)

  # factor for ordering
  df_local <- df_local %>% dplyr::mutate(landcover_factor = factor(major_land, levels = present_landcovers_local)) %>% dplyr::filter(!is.na(landcover_factor))

  # Build map (Panel A)
  global_base_local <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    coord_sf(crs = st_crs(3857), expand = FALSE) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(0.05, 0.65), legend.justification = c("left","top"), legend.text = element_text(size = 12), legend.title = element_blank())

  # choose the wetland key depending on whether Tundra replaced Wetland in this run
  wetland_key_local <- if ("Tundra" %in% present_landcovers_local) "Tundra" else "Wetland_Marsh"

  global_map_local <- global_base_local +
    geom_point(data = df_local %>% dplyr::filter(landcover_factor != "Forest" & landcover_factor != wetland_key_local), aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor), size = 2.3, alpha = 0.60, stroke = 0.12) +
    geom_point(data = df_local %>% dplyr::filter(landcover_factor == "Forest"), aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor), size = 2.0, alpha = 0.45, stroke = 0.12) +
    scale_fill_manual(name = "Land Cover", values = my_landcover_colors_local, labels = display_labels_local, drop = FALSE) +
    scale_color_manual(name = "Land Cover", values = my_landcover_colors_local, labels = display_labels_local, drop = FALSE) +
    scale_shape_manual(name = "Land Cover", values = shape_map_values_local[present_landcovers_local], labels = display_labels_local, drop = FALSE) +
    guides(fill = guide_legend(override.aes = list(shape = unname(shape_map_values_local[present_landcovers_local]), fill = unname(my_landcover_colors_local), colour = unname(my_landcover_colors_local), size = unname(legend_size_map_local[present_landcovers_local]), alpha = 1.0, stroke = unname(stroke_map_local[present_landcovers_local]))), shape = "none", color = "none")

  global_map_local <- global_map_local +
    geom_point(data = df_local %>% dplyr::filter(landcover_factor == wetland_key_local), aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor), size = as.numeric(point_size_jitter_local[wetland_key_local]), stroke = 1, alpha = 0.90, inherit.aes = FALSE) +
    geom_point(data = df_local %>% dplyr::filter(landcover_factor == "Impervious"), aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor), size = 2.3, stroke = 0, alpha = 0.50, inherit.aes = FALSE) +
    geom_point(data = df_local %>% dplyr::filter(landcover_factor == "Grassland_Shrubland"), aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor), size = 2.3, stroke = 0.12, alpha = 0.70, inherit.aes = FALSE)

  # Insets
  create_regional_map_local <- function(xlim, ylim, data_df) {
    ggplot() +
      geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
      geom_point(data = data_df %>% dplyr::filter(landcover_factor != "Forest" & landcover_factor != wetland_key_local), aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor), size = 2.3, alpha = 0.60, stroke = 0.12) +
      geom_point(data = data_df %>% dplyr::filter(landcover_factor == "Forest"), aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor), size = 2.0, alpha = 0.45, stroke = 0.12) +
      scale_fill_manual(values = my_landcover_colors_local, drop = FALSE, guide = "none") +
      scale_color_manual(values = my_landcover_colors_local, drop = FALSE, guide = "none") +
      scale_shape_manual(values = shape_map_values_local[names(my_landcover_colors_local)], drop = FALSE, guide = "none") +
      geom_point(data = data_df %>% dplyr::filter(landcover_factor == "Impervious"), aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor), size = 2.3, stroke = 0, alpha = 0.90, inherit.aes = FALSE) +
      geom_point(data = data_df %>% dplyr::filter(landcover_factor == "Grassland_Shrubland"), aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor), size = 2.3, stroke = 0.12, alpha = 0.70, inherit.aes = FALSE) +
      coord_sf(xlim = xlim, ylim = ylim, crs = st_crs(3857), expand = FALSE) + theme_void() + theme(panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), panel.border = element_rect(color = "black", fill = NA, size = 0.5))
  }

  df_uk_local <- df_local %>% dplyr::filter(longitude >= uk_xlim[1], longitude <= uk_xlim[2], latitude >= uk_ylim[1], latitude <= uk_ylim[2])
  df_scandinavia_local <- df_local %>% dplyr::filter(longitude >= scandinavia_xlim[1], longitude <= scandinavia_xlim[2], latitude >= scandinavia_ylim[1], latitude <= scandinavia_ylim[2])
  df_australia_local <- df_local %>% dplyr::filter(longitude >= australia_xlim[1], longitude <= australia_xlim[2], latitude >= australia_ylim[1], latitude <= australia_ylim[2])

  uk_inset_local <- create_regional_map_local(uk_xlim, uk_ylim, df_uk_local)
  scandinavia_inset_local <- create_regional_map_local(scandinavia_xlim, scandinavia_ylim, df_scandinavia_local)
  australia_inset_local <- create_regional_map_local(australia_xlim, australia_ylim, df_australia_local)

  # Choose inset sizes based on point counts so sparse ext data doesn't produce
  # large empty inset boxes. If no points in a region, skip the inset.
  inset_dims <- function(n) {
    if (is.na(n) || n <= 0) return(NULL)
    if (n < 5) return(list(w = 0.12, h = 0.12))
    if (n < 15) return(list(w = 0.18, h = 0.18))
    return(list(w = 0.25, h = 0.25))
  }

  uk_dims <- inset_dims(nrow(df_uk_local))
  scandinavia_dims <- inset_dims(nrow(df_scandinavia_local))
  australia_dims <- inset_dims(nrow(df_australia_local))

  final_map_local <- ggdraw(global_map_local)
  if (!is.null(uk_dims)) final_map_local <- final_map_local + draw_plot(uk_inset_local, x = 0.345, y = 0.447, width = uk_dims$w, height = uk_dims$h)
  if (!is.null(scandinavia_dims)) final_map_local <- final_map_local + draw_plot(scandinavia_inset_local, x = 0.524, y = 0.47, width = scandinavia_dims$w, height = scandinavia_dims$h)
  if (!is.null(australia_dims)) final_map_local <- final_map_local + draw_plot(australia_inset_local, x = 0.56, y = 0.12, width = australia_dims$w, height = australia_dims$h)
  final_map_local <- final_map_local + draw_line(x = c(0.419,0.48), y = c(0.68,0.71), color = "black", size = 0.7) + draw_line(x = c(0.54,0.58), y = c(0.72,0.66), color = "black", size = 0.7) + draw_line(x = c(0.788,0.85), y = c(0.26,0.32), color = "black", size = 0.7)

  p_map_labeled_local <- final_map_local + labs(tag = "a)") + theme(plot.tag = element_text(size = 16, hjust = 0, vjust = 1, face = "plain"), plot.tag.position = c(0.02, 0.98))

  # Panel B (slope)
  p_slope_local <- ggplot(df_local, aes(x = slope_estimate, y = landcover_factor)) + geom_boxplot(aes(fill = landcover_factor), outlier.shape = NA, alpha = 0.7) + geom_jitter(aes(fill = landcover_factor, color = landcover_factor, shape = landcover_factor, size = landcover_factor), width = 0, height = 0.2, stroke = 0.12, alpha = 0.75) + scale_fill_manual(values = my_landcover_colors_local) + scale_color_manual(values = my_landcover_colors_local) + scale_shape_manual(values = shape_values_all_local[present_landcovers_local]) + scale_size_manual(values = point_size_jitter_local[present_landcovers_local], guide = "none") + labs(x = "CQ Slope Estimate", y = NULL) + theme_classic(base_size = 14) + scale_y_discrete(limits = rev(present_landcovers_local), labels = rev(display_labels_local_named)) + theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust = 0.5), axis.title.x = element_text(margin = margin(t = 8)))

  # overlay jitter emphases: choose wetland key depending on ext substitution
  wetland_key_local <- if ("Tundra" %in% present_landcovers_local) "Tundra" else "Wetland_Marsh"

  p_slope_local <- p_slope_local + geom_jitter(data = df_local %>% dplyr::filter(landcover_factor == wetland_key_local), aes(x = slope_estimate, y = landcover_factor, shape = landcover_factor, color = landcover_factor, fill = landcover_factor), width = 0, height = 0.2, size = point_size_jitter_local[wetland_key_local], stroke = 1, alpha = 0.98, inherit.aes = FALSE) + geom_jitter(data = df_local %>% dplyr::filter(landcover_factor == "Impervious"), aes(x = slope_estimate, y = landcover_factor, shape = landcover_factor, color = landcover_factor, fill = landcover_factor), width = 0, height = 0.2, size = point_size_jitter_local["Impervious"], stroke = 0, alpha = 0.75, inherit.aes = FALSE)

  p_slope_labeled_local <- p_slope_local + labs(tag = "b)") + theme(plot.tag = element_text(size = 16, hjust = 0))

  # Now build and save both panel C variants for this df
  for (panel_var_choice in c("precipitation", "drainage_area")) {
    if (panel_var_choice == "precipitation") {
      df_local$precipitation <- as.numeric(df_local$precipitation)
      var_vec <- df_local$precipitation
      var_label <- expression(paste("Precipitation (", mm~day^{-1}, ")"))
      panel_df_local <- df_local
    } else {
      # Convert drainage area to hectares for plotting (1 km^2 = 100 ha)
      df_local$drainage_area <- as.numeric(df_local$drainage_area)
      panel_df_local <- df_local %>% dplyr::filter(!is.na(drainage_area)) %>% dplyr::mutate(panel_var = drainage_area * 100)
      var_vec <- panel_df_local$panel_var
      var_label <- "Drainage area (ha)"
    }

    var_breaks <- unname(quantile(var_vec, probs = seq(0, 1, 0.25), na.rm = TRUE))
    # Round drainage-area (km^2) bin edges to whole numbers for readability
    if (panel_var_choice == "drainage_area") {
      # var_breaks currently in km^2; convert to hectares and round
      var_breaks <- round(var_breaks * 100, 0)
    }
    var_labels <- vapply(seq_len(length(var_breaks)-1), function(i) { lo <- var_breaks[i]; hi <- var_breaks[i+1]; if (i == 1) paste0("\u2264 ", hi) else paste0(lo, " - ", hi) }, FUN.VALUE = character(1))

    # ensure panel_var uses hectares for drainage_area
    panel_df_local <- panel_df_local %>% dplyr::mutate(panel_var = if (panel_var_choice == "precipitation") precipitation else (drainage_area * 100), panel_bin = cut(panel_var, breaks = var_breaks, include.lowest = TRUE, labels = var_labels))

    quartile_summary_local <- panel_df_local %>% dplyr::group_by(panel_bin, major_land) %>% dplyr::summarise(n = dplyr::n(), .groups = "drop") %>% dplyr::group_by(panel_bin) %>% dplyr::mutate(prop = n / sum(n)) %>% dplyr::ungroup()
    present_quartile_land_local <- desired_local_order[desired_local_order %in% unique(quartile_summary_local$major_land)]
    quartile_colors_local <- landcover_colors_local[present_quartile_land_local]
    quartile_summary_local$major_land <- factor(quartile_summary_local$major_land, levels = present_quartile_land_local)

    p_third_local <- ggplot(quartile_summary_local, aes(x = panel_bin, y = prop, fill = major_land)) + geom_bar(stat = "identity", position = "fill", color = "gray20") + scale_fill_manual(values = quartile_colors_local, breaks = present_quartile_land_local, guide = "none") + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + labs(x = var_label, y = "Proportion of sites") + theme_classic(base_size = 14) + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

    p_third_labeled_local <- p_third_local + labs(tag = "c)") + theme(plot.tag = element_text(size = 16, hjust = 0))

    final_boxplots_local <- p_slope_labeled_local + p_third_labeled_local
    combined_figure_local <- ggarrange(p_map_labeled_local, final_boxplots_local, ncol = 1, nrow = 2, heights = c(3, 2.5), align = "v", labels = NULL)

    out_suffix <- if (file_suffix == "") panel_var_choice else paste0(panel_var_choice, file_suffix)
    out_file <- file.path(output_dir, paste0("Fig_landcover_cqslope_", out_suffix, ".png"))
    ggsave(out_file, combined_figure_local, width = 8, height = 8.5, dpi = 300, bg = "white")
    cat("Saved figure to:", out_file, "\n")
  }
}

# Build and save figures for both canonical and external landcover workflows.
# Canonical: use `major_land` (from sizer). External: use `major_land_ext`.
# We run them in a simple loop to keep behavior explicit.
is_ext_list <- switch(as.character(run_mode),
                      "both" = c(FALSE, TRUE),
                      "canon" = c(FALSE),
                      "ext" = c(TRUE),
                      c(FALSE, TRUE))

for (is_ext in is_ext_list) {
  if (!is_ext) {
    df_local <- combined_df
    suffix_local <- ""
    use_ext_flag <- FALSE
  } else {
    df_local <- combined_df %>% dplyr::mutate(major_land = major_land_ext)
    suffix_local <- "_ext"
    use_ext_flag <- TRUE
  }

  build_and_save_for_df(df_local, file_suffix = suffix_local, use_ext = use_ext_flag)
}

# Also build and save the drainage_area version in the same run (use km^2)
panel_c_var2 <- "drainage_area"
combined_df$drainage_area <- as.numeric(combined_df$drainage_area)
var_vec2 <- combined_df$drainage_area
var_label2 <- expression(paste("Drainage area (km"^2, ")"))
var_breaks2 <- unname(quantile(var_vec2, probs = seq(0, 1, 0.25), na.rm = TRUE))
if (panel_c_var2 == "drainage_area") {
  var_breaks2 <- round(var_breaks2, 0)
}
var_labels2 <- vapply(seq_len(length(var_breaks2)-1), function(i) {
  lo <- var_breaks2[i]
  hi <- var_breaks2[i+1]
  if (i == 1) paste0("\u2264 ", hi) else paste0(lo, " - ", hi)
}, FUN.VALUE = character(1))

panel_df2 <- combined_df %>% dplyr::filter(!is.na(drainage_area)) %>%
  # convert to hectares for panel C (1 km^2 = 100 ha)
  dplyr::mutate(panel_var = drainage_area * 100, panel_bin = cut(panel_var, breaks = var_breaks2 * 100, include.lowest = TRUE, labels = var_labels2))

quartile_summary2 <- panel_df2 %>%
  dplyr::group_by(panel_bin, major_land) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(panel_bin) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()

    # Ensure Wetland_Marsh appears immediately before Ice in the non-ext ordering
    desired_order2 <- desired_land_order
    if ("Wetland_Marsh" %in% unique(quartile_summary2$major_land)) {
      desired_order2 <- desired_order2[desired_order2 != "Wetland_Marsh"]
      ice_idx2 <- which(desired_order2 == "Ice")
      if (length(ice_idx2) == 0) {
        desired_order2 <- c(desired_order2, "Wetland_Marsh")
      } else {
        desired_order2 <- append(desired_order2, "Wetland_Marsh", after = ice_idx2 - 1)
      }
    }
    present_quartile_land2 <- desired_order2[desired_order2 %in% unique(quartile_summary2$major_land)]
    quartile_colors2 <- landcover_colors[present_quartile_land2]
    quartile_summary2$major_land <- factor(quartile_summary2$major_land, levels = present_quartile_land2)

p_third2 <- ggplot(quartile_summary2, aes(x = panel_bin, y = prop, fill = major_land)) +
  geom_bar(stat = "identity", position = "fill", color = "gray20") +
  scale_fill_manual(values = quartile_colors2, breaks = present_quartile_land2, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = var_label2, y = "Proportion of sites") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

p_third2_labeled <- p_third2 + labs(tag = "c)") + theme(plot.tag = element_text(size = 16, hjust = 0))

final_boxplots2 <- p_slope_labeled + p_third2_labeled
combined_figure2 <- ggarrange(p_map_labeled, final_boxplots2, ncol = 1, nrow = 2, heights = c(3, 2.5), align = "v", labels = NULL)

output_file2 <- file.path(output_dir, "Fig_landcover_cqslope_drainage_area.png")
ggsave(output_file2, combined_figure2, width = 8, height = 8.5, dpi = 300, bg = "white")
cat("Saved figure to:", output_file2, "\n")


