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
  dplyr::select(LTER, Stream_Name, major_land) %>%
  dplyr::group_by(LTER, Stream_Name) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

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

# Export combined_df as CSV for checking in the specified directory
write.csv(combined_df, file = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map/LULC_site_check.csv", row.names = FALSE)

#--- End land-use checks---

# Filter to one record per site
sites_df <- sizer_data %>%
  dplyr::group_by(LTER, Stream_Name) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(LTER, Stream_Name, latitude, longitude, major_land, slope_estimate) %>%
  dplyr::filter(!is.na(latitude), !is.na(longitude), !is.na(major_land))

# Check unique land cover values
unique_landcover <- unique(sites_df$major_land)
cat("Unique land cover types:\n")
print(unique_landcover)

# Define colors for land cover types
# Using a color palette appropriate for land cover categories
landcover_colors <- c(
  "Forest"              = "#2d5016",  # Dark green
  "Cropland"            = "#d4a017",  # Golden
  "Grassland_Shrubland" = "#b8a67d",  # Tan
  "Wetland_Marsh"       = "#4a7c59",  # Teal green
  "Impervious"          = "#6b6b6b",  # Gray
  "Water"               = "#4682b4",  # Steel blue
  "Bare"                = "#c2a882",  # Beige
  "Ice_Snow"            = "#e3f2fd"   # Pale blue
)

# Filter colors to only those present in the data
present_landcovers <- intersect(names(landcover_colors), unique(sites_df$major_land))
my_landcover_colors <- landcover_colors[present_landcovers]

# Create factor for legend ordering
sites_df <- sites_df %>%
  dplyr::mutate(
    landcover_factor = factor(major_land, levels = names(my_landcover_colors))
  ) %>%
  dplyr::filter(!is.na(landcover_factor))

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
    data = sites_df,
    aes(x = longitude, y = latitude, fill = landcover_factor),
    shape = 21, size = 2, alpha = 0.7, stroke = 0.1, color = "gray20"
  ) +
  scale_fill_manual(name = "Land Cover", values = my_landcover_colors, drop = FALSE) +
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
      aes(x = longitude, y = latitude, fill = landcover_factor),
      shape = 21, size = 2, alpha = 0.7, stroke = 0.1, color = "gray20"
    ) +
    scale_fill_manual(values = my_landcover_colors, drop = FALSE, guide = "none") +
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
df_uk <- filter(sites_df, longitude >= uk_xlim[1], longitude <= uk_xlim[2], latitude >= uk_ylim[1], latitude <= uk_ylim[2])
df_scandinavia <- filter(sites_df, longitude >= scandinavia_xlim[1], longitude <= scandinavia_xlim[2], latitude >= scandinavia_ylim[1], latitude <= scandinavia_ylim[2])
df_australia <- filter(sites_df, longitude >= australia_xlim[1], longitude <= australia_xlim[2], latitude >= australia_ylim[1], latitude <= australia_ylim[2])

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

p_slope <- ggplot(sites_df, aes(x = slope_estimate, y = landcover_factor)) +
  geom_boxplot(aes(fill = landcover_factor), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(fill = landcover_factor, color = landcover_factor),
              width = 0, height = 0.2, size = 2.2, stroke = 0.1, alpha = 0.6, shape = 21) +
  scale_fill_manual(values = my_landcover_colors) +
  scale_color_manual(values = my_landcover_colors) +
  labs(x = "CQ Slope Estimate", y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(sites_df$landcover_factor))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

p_slope_labeled <- p_slope +
  labs(tag = "b)") +
  theme(plot.tag = element_text(size = 16, hjust = 0))

# #############################################################################
# 5) Combine panels
# #############################################################################

combined_figure <- ggarrange(
  p_map_labeled,
  p_slope_labeled,
  ncol    = 1,
  nrow    = 2,
  heights = c(3, 1.5),
  align   = "v",
  labels  = NULL
)

# #############################################################################
# 6) Export Figures
# #############################################################################
output_dir <- "/Users/sidneybush/Documents/GitHub/LTER-SiSyn-Spatial-Controls/Exploratory_Analyses"

# Save as PNG for viewing
ggsave(file.path(output_dir, "Fig_landcover_cqslope.png"), combined_figure,
       width = 8, height = 8.5, dpi = 300, bg = "white")


