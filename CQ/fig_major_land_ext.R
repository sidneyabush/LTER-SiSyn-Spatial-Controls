# Fig: landcover CQ slope (major_land_ext) — self-contained
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

# For this script, use the external landcover classification: copy major_land_ext into major_land
combined_df$major_land <- combined_df$major_land_ext

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
  "Ice"                 = "#8BAFCF"
)


# Add Tundra color for external runs
landcover_colors["Tundra"] <- "#4A6F84"


# Desired legend order and display labels (original order)
desired_land_order <- c("Forest", "Grassland_Shrubland", "Cropland", "Impervious", "Ice", "Tundra")
display_labels <- c(
  "Forest" = "Forest",
  "Grassland_Shrubland" = "Grassland",
  "Cropland" = "Cropland",
  "Impervious" = "Impervious",
  "Ice" = "Ice",
  "Tundra" = "Tundra"
)

# Define shape mapping (named) for the desired order.
shape_values_all <- c(
  "Forest" = 21,
  "Grassland_Shrubland" = 22,
  "Cropland" = 18,
  "Impervious" = 24,
  "Ice" = 21,
  "Tundra" = 23
)

# Base and jitter size mappings
point_size_base <- c(
  "Forest" = 2,
  "Grassland_Shrubland" = 2,
  "Cropland" = 2,
  "Impervious" = 2,
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
  "Impervious" = 0.0,
  "Ice" = 0.12,
  "Tundra" = 0.12
)

# Map-specific shape mapping: make Wetland the same square shape as Grassland on the map only
shape_map_values <- shape_values_all

# Legend size mapping: default 4, but make triangles (Impervious, Bare) a bit smaller
legend_size_map <- rep(4, length(shape_map_values))
names(legend_size_map) <- names(shape_map_values)
legend_size_map["Impervious"] <- 3.0
## No Bare mapping in ext script — removed

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
  # use km^2 for drainage area (no conversion)
  var_vec <- combined_df$drainage_area
  var_label <- expression(paste("Drainage area (", km^2, ")"))
}

# Prepare map data, inset extents, and output directory for building/saving
world <- ggplot2::map_data("world")
uk_xlim          <- c(-10, 2);   uk_ylim <- c(49, 61)
scandinavia_xlim <- c(4, 30);    scandinavia_ylim <- c(50, 72)
australia_xlim   <- c(135,155);  australia_ylim   <- c(-40,-28)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map"

# Copy of build_and_save_for_df (self-contained) from main pipeline
build_and_save_for_df <- function(df_local, file_suffix = "", use_ext = FALSE) {
  if (use_ext) df_local$major_land <- df_local$major_land_ext
  present_landcovers_local <- desired_land_order[desired_land_order %in% unique(df_local$major_land)]
  if (length(present_landcovers_local) == 0) return()
  my_landcover_colors_local <- landcover_colors[present_landcovers_local]
  display_labels_local_named <- display_labels[present_landcovers_local]
  display_labels_local <- unname(display_labels_local_named)

  desired_local_order <- desired_land_order
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

  present_landcovers_local <- desired_local_order[desired_local_order %in% unique(df_local$major_land)]
  if (length(present_landcovers_local) == 0) return()
  my_landcover_colors_local <- landcover_colors[present_landcovers_local]

  landcover_colors_local <- landcover_colors
  shape_values_all_local <- shape_values_all
  shape_map_values_local <- shape_map_values
  point_size_jitter_local <- point_size_jitter
  stroke_map_local <- stroke_map
  legend_size_map_local <- legend_size_map
  if (!is.null(file_suffix) && file_suffix == "_ext" && ("Tundra" %in% present_landcovers_local)) {
    point_size_jitter_local["Tundra"] <- 1.5
    stroke_map_local["Tundra"] <- 0.08
    legend_size_map_local["Tundra"] <- 3.0
  }

  my_landcover_colors_local <- landcover_colors_local[present_landcovers_local]
  display_labels_local_named <- display_labels[present_landcovers_local]
  if (!is.null(file_suffix) && file_suffix == "_ext" && ("Tundra" %in% present_landcovers_local)) {
    if (!("Tundra" %in% names(display_labels_local_named)) || is.na(display_labels_local_named["Tundra"])) {
      display_labels_local_named["Tundra"] <- "Tundra"
    }
  }
  na_idx <- which(is.na(display_labels_local_named) | display_labels_local_named == "")
  if (length(na_idx) > 0) display_labels_local_named[na_idx] <- names(display_labels_local_named)[na_idx]
  display_labels_local <- unname(display_labels_local_named)

  df_local <- df_local %>% dplyr::mutate(landcover_factor = factor(major_land, levels = present_landcovers_local)) %>% dplyr::filter(!is.na(landcover_factor))

  global_base_local <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    coord_sf(crs = st_crs(3857), expand = FALSE) + theme_minimal() + theme(panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(0.05, 0.65), legend.justification = c("left","top"), legend.text = element_text(size = 12), legend.title = element_blank())

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

  p_map_labeled_local <- final_map_local + labs(tag = "a") + theme(plot.tag = element_text(size = 16, hjust = 0, vjust = 1, face = "plain"), plot.tag.position = c(0.02, 0.98))

  p_slope_local <- ggplot(df_local, aes(x = slope_estimate, y = landcover_factor)) + geom_boxplot(aes(fill = landcover_factor), outlier.shape = NA, alpha = 0.7) + geom_jitter(aes(fill = landcover_factor, color = landcover_factor, shape = landcover_factor, size = landcover_factor), width = 0, height = 0.2, stroke = 0.12, alpha = 0.75) + scale_fill_manual(values = my_landcover_colors_local) + scale_color_manual(values = my_landcover_colors_local) + scale_shape_manual(values = shape_values_all_local[present_landcovers_local]) + scale_size_manual(values = point_size_jitter_local[present_landcovers_local], guide = "none") + labs(x = "CQ Slope Estimate", y = NULL) + theme_classic(base_size = 14) + scale_y_discrete(limits = rev(present_landcovers_local), labels = rev(display_labels_local_named)) + theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust = 0.5), axis.title.x = element_text(margin = margin(t = 8)))

  p_slope_local <- p_slope_local + geom_jitter(data = df_local %>% dplyr::filter(landcover_factor == wetland_key_local), aes(x = slope_estimate, y = landcover_factor, shape = landcover_factor, color = landcover_factor, fill = landcover_factor), width = 0, height = 0.2, size = point_size_jitter_local[wetland_key_local], stroke = 1, alpha = 0.98, inherit.aes = FALSE) + geom_jitter(data = df_local %>% dplyr::filter(landcover_factor == "Impervious"), aes(x = slope_estimate, y = landcover_factor, shape = landcover_factor, color = landcover_factor, fill = landcover_factor), width = 0, height = 0.2, size = point_size_jitter_local["Impervious"], stroke = 0, alpha = 0.75, inherit.aes = FALSE)

  p_slope_labeled_local <- p_slope_local + labs(tag = "b)") + theme(plot.tag = element_text(size = 16, hjust = 0))

  for (panel_var_choice in c("precipitation", "drainage_area")) {
    if (panel_var_choice == "precipitation") {
      df_local$precipitation <- as.numeric(df_local$precipitation)
      var_vec <- df_local$precipitation
      var_label <- expression(paste("Precipitation (", mm~day^{-1}, ")"))
      panel_df_local <- df_local
    } else {
      df_local$drainage_area <- as.numeric(df_local$drainage_area)
      panel_df_local <- df_local %>% dplyr::filter(!is.na(drainage_area))
      var_vec <- panel_df_local$drainage_area
      var_label <- expression(paste("Drainage area (", km^2, ")"))
    }

    var_breaks <- unname(quantile(var_vec, probs = seq(0, 1, 0.25), na.rm = TRUE))
    if (panel_var_choice == "drainage_area") var_breaks <- round(var_breaks, 0)
    var_labels <- vapply(seq_len(length(var_breaks)-1), function(i) { lo <- var_breaks[i]; hi <- var_breaks[i+1]; if (i == 1) paste0("\u2264 ", hi) else paste0(lo, " - ", hi) }, FUN.VALUE = character(1))

    panel_df_local <- panel_df_local %>% dplyr::mutate(panel_var = if (panel_var_choice == "precipitation") precipitation else (drainage_area), panel_bin = cut(panel_var, breaks = var_breaks, include.lowest = TRUE, labels = var_labels))

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

# Run builder for ext file
build_and_save_for_df(combined_df, file_suffix = "_ext", use_ext = TRUE)
