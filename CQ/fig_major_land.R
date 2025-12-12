# Fig: landcover CQ slope (major_land) — self-contained
rm(list = ls())

librarian::shelf(dplyr, stringr, ggplot2, maps, patchwork, scales, colorspace, ggrepel,
                 ggspatial, sf, ggpubr, cowplot)

# Data preparation

# Load sizer data
data_file <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map/sizer_outs_with_drivers_6Aug25.csv"
sizer_data <- read.csv(data_file, stringsAsFactors = FALSE)

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

# Ensure longitude and latitude are numeric for plotting
combined_df$longitude <- as.numeric(combined_df$longitude)
combined_df$latitude <- as.numeric(combined_df$latitude)
# Check unique land cover values (silent in minimal mode)

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
## (Tundra color intentionally omitted for canonical `major_land` runs)


# Desired legend order and display labels (original order)
desired_land_order <- c("Forest", "Grassland_Shrubland", "Cropland", "Wetland_Marsh", "Impervious", "Bare", "Ice")
display_labels <- c(
  "Forest" = "Forest",
  "Grassland_Shrubland" = "Grassland",
  "Cropland" = "Cropland",
  "Impervious" = "Impervious",
  "Bare" = "Bare",
  "Ice" = "Ice",
  # Tundra intentionally excluded for canonical run
)

# Define shape mapping (named) for the desired order.
shape_values_all <- c(
  "Forest" = 21,
  "Grassland_Shrubland" = 22,
  "Cropland" = 18,
  # Wetland not shown as separate class in canonical (kept as Wetland_Marsh in data but not styled here)
  "Impervious" = 24,
  "Bare" = 25,
  "Ice" = 21,
  # Tundra excluded
)

# Base and jitter size mappings
point_size_base <- c(
  "Forest" = 2,
  "Grassland_Shrubland" = 2,
  "Cropland" = 2,
  "Impervious" = 2,
  "Bare" = 2,
  "Ice" = 2,
  # Tundra excluded
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
  "Impervious" = 0.0,
  "Bare" = 0.12,
  "Ice" = 0.12,
  # Tundra excluded
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
