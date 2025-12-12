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

# Define colors for land cover types
landcover_colors <- c(
  "Forest"              = "#2d5016",  # Dark green
  "Cropland"            = "#d4a017",  # Golden
  "Grassland_Shrubland" = "#b8a67d",  # Tan
  "Wetland_Marsh"       = "#4682b4",  # Teal green
  "Impervious"          = "#6b6b6b",  # Gray
  "Bare"                = "#c2a882",  # Beige
  "Ice"                 = "#e3f2fd"   # Pale blue
)

landcover_colors <- c(
  "Forest"              = "#3C5E3E",   # Muted green
  "Cropland"            = "#C4A000",   # Muted yellow-ochre
  "Grassland_Shrubland" = "#9EBD73",   # Muted greenish-khaki (different from Forest)
  "Wetland_Marsh"       = "#5B7F9E",   # Muted blue-grey
  "Impervious"          = "#555555",   # Neutral grey
  "Bare"                = "#C87E72",   # Muted salmon/terracotta (NOT brown)
  "Ice"                 = "#DDE9F0"    # Very pale icy blue
)

landcover_colors <- c(
  "Forest"              = "#2F5D36",   # Deep muted green
  "Grassland_Shrubland" = "#8DAA67",   # Muted sage green
  "Cropland"            = "#C9A227",   # Muted golden ochre
  "Wetland_Marsh"       = "#4F7A9A",   # Slate blue
  "Impervious"          = "#4E4E4E",   # Dark neutral gray
  "Bare"                = "#B86F5A",   # Muted terracotta (strong contrast)
  "Ice"                 = "#7FA7C4"    # Muted medium icy blue (VISIBLE!)
)

landcover_colors <- c(
  "Forest"              = "#1F5633",   # very dark green
  "Grassland_Shrubland" = "#89A55A",   # light olive
  "Cropland"            = "#D3A022",   # saturated ochre
  "Wetland_Marsh"       = "#3F6B8E",   # marine blue
  "Impervious"          = "#4A4949",   # dark grey
  "Bare"                = "#B45C4F",   # terracotta
  "Ice"                 = "#6D8FB9"    # mid blue
)

landcover_colors <- c(
  "Forest"              = "#305D3B",   
  "Grassland_Shrubland" = "#A1A86B",   
  "Cropland"            = "#CEB24C",   
  "Wetland_Marsh"       = "#5E7B71",   
  "Impervious"          = "#4F4F4F",   
  "Bare"                = "#C48A72",   
  "Ice"                 = "#8BAFCF"    
)

landcover_colors <- c(
  "Forest"              = "#2F5530",   # muted deep green
  "Grassland_Shrubland" = "#8E9F50",   # sage/olive green
  "Cropland"            = "#C89A1A",   # ochre gold
  "Wetland_Marsh"       = "#4A718C",   # steel blue
  "Impervious"          = "#5A5A5A",   # neutral grey
  "Bare"                = "#AA5745",   # muted clay red
  "Ice"                 = "#6D97C4"    # medium cool blue
)


# Desired legend order and display labels
desired_land_order <- c("Forest",  "Grassland_Shrubland", "Cropland", "Wetland_Marsh", "Impervious", "Bare", "Ice")
display_labels <- c(
  "Forest" = "Forest",
  "Grassland_Shrubland" = "Grassland",
  "Cropland" = "Cropland",
  "Wetland_Marsh" = "Wetland",
  "Impervious" = "Impervious",
  "Bare" = "Bare",
  "Ice" = "Ice"
)

# Define shape mapping (named) for the desired order. Use filled shapes for first five categories.
shape_values_all <- c(
  "Forest" = 21,
  "Cropland" = 22,
  "Grassland_Shrubland" = 23,
  "Wetland_Marsh" = 24,
  "Impervious" = 25,
  "Bare" = 3,
  "Ice" = 4
)


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
  var_label <- "Drainage area (sqkm)"
} else {
  stop("panel_c_var must be either 'precipitation' or 'drainage_area'")
}

# Compute quartile breaks and labels using the chosen variable
var_breaks <- unname(quantile(var_vec, probs = seq(0, 1, 0.25), na.rm = TRUE))
var_labels <- vapply(seq_len(length(var_breaks)-1), function(i) {
  lo <- round(var_breaks[i], 1)
  hi <- round(var_breaks[i+1], 1)
  if (i == 1) {
    paste0("\u2264 ", hi)        # ≤ hi
  } else {
    paste0(lo, " – ", hi)        # en-dash
  }
}, FUN.VALUE = character(1))

# Note: For drainage_area we will drop NA only in panel_df when constructing Panel C


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
    legend.text          = element_text(size = 10),
    legend.title         = element_blank()
  )

global_map <- global_base +
  geom_point(
    data = combined_df,
    aes(x = longitude, y = latitude, fill = landcover_factor),
    shape = 21, size = 2, alpha = 0.7, stroke = 0.1, color = "gray20"
  ) +
  scale_fill_manual(name = "Land Cover", values = my_landcover_colors, labels = display_labels_present, drop = FALSE) +
  guides(
    fill = guide_legend(override.aes = list(color = "gray20", size = 4, alpha = 1, stroke = 0.1))
  )

# #############################################################################
# 3) Regional Insets
# #############################################################################
create_regional_map <- function(xlim, ylim, data_df) {
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "lightgray", color = "white") +
    geom_point(
      data = data_df,
      aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor),
      size = 2, alpha = 0.7, stroke = 0.1, color = "gray20"
    ) +
    scale_fill_manual(values = my_landcover_colors, drop = FALSE, guide = "none") +
    scale_shape_manual(values = shape_values_all[names(my_landcover_colors)], drop = FALSE, guide = "none") +
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
  geom_jitter(aes(fill = landcover_factor, color = landcover_factor),
              width = 0, height = 0.2, size = 2.2, stroke = 0.1, alpha = 0.6, shape = 21) +
  scale_fill_manual(values = my_landcover_colors) +
  scale_color_manual(values = my_landcover_colors) +
  labs(x = "CQ Slope Estimate", y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(combined_df$landcover_factor))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

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
  var_label <- "Precipitation (mm day^-1)"
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
    paste0("<= ", hi)
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

# Ensure quartile colors respect desired order
present_quartile_land <- desired_land_order[desired_land_order %in% unique(quartile_summary$major_land)]
quartile_colors <- landcover_colors[present_quartile_land]

# Stacked proportion bar plot (panel C) using unified panel_bin
 p_third <- ggplot(quartile_summary, aes(x = panel_bin, y = prop, fill = major_land)) +
  geom_bar(stat = "identity", position = "fill", color = "gray20") +
  scale_fill_manual(values = quartile_colors, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = var_label, y = "Proportion of sites") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))

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

# Save as PNG for viewing
ggsave(file.path(output_dir, "Fig_landcover_cqslope.png"), combined_figure,
       width = 8, height = 8.5, dpi = 300, bg = "white")


