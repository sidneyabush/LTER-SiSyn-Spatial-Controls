# Fig: landcover CQ slope — Drainage Area Plot
rm(list = ls())

librarian::shelf(dplyr, stringr, ggplot2, maps, patchwork, scales, colorspace, ggrepel,
                 ggspatial, sf, ggpubr, cowplot)

# Load harmonized sizer data with drivers
data_file <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map/sizer_outs_ave_drivers_harmonized.csv"
sizer_data <- read.csv(data_file, stringsAsFactors = FALSE)

# Prepare data
combined_df <- sizer_data %>%
  dplyr::group_by(LTER, Stream_Name) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(LTER, Stream_Name, latitude, longitude, precip, slope_estimate, drainSqKm, major_land) %>%
  dplyr::rename(precipitation = precip, drainage_area = drainSqKm) %>%
  dplyr::mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    slope_estimate = as.numeric(slope_estimate),
    drainage_area = as.numeric(drainage_area)
  )

# MCM override
combined_df$precipitation[combined_df$LTER == "MCM"] <- 0.4

# Color scheme
landcover_colors <- c(
  "Forest"              = "#355841",
  "Grassland_Shrubland" = "#A1A86B",
  "Cropland"            = "#CEB24C",
  "Impervious"          = "#4F4F4F",
  "Bare"                = "#C48A72",
  "Wetland_Marsh"       = "#4A6F84",
  "Ice"                 = "#8BAFCF",
  "Tundra"              = "#4A6F84"
)

# Legend order and labels
desired_land_order <- c("Forest", "Grassland_Shrubland", "Cropland", "Impervious", "Bare", "Wetland_Marsh", "Ice", "Tundra")
display_labels <- c(
  "Forest" = "Forest",
  "Grassland_Shrubland" = "Grassland",
  "Cropland" = "Cropland",
  "Impervious" = "Impervious",
  "Bare" = "Bare",
  "Wetland_Marsh" = "Wetland",
  "Ice" = "Ice",
  "Tundra" = "Tundra"
)

# Shapes
shape_values_all <- c(
  "Forest" = 21, "Grassland_Shrubland" = 22, "Cropland" = 18,
  "Impervious" = 24, "Bare" = 25, "Wetland_Marsh" = 22,
  "Ice" = 21, "Tundra" = 23
)

# Point sizes
point_size_jitter <- c(
  "Forest" = 2.0, "Grassland_Shrubland" = 2.5, "Cropland" = 2.5,
  "Impervious" = 2.5, "Bare" = 2.5, "Wetland_Marsh" = 2.5,
  "Ice" = 2.0, "Tundra" = 1.5
)

# Strokes
stroke_map <- c(
  "Forest" = 0.12, "Grassland_Shrubland" = 0.12, "Cropland" = 0.12,
  "Impervious" = 0.0, "Bare" = 0.12, "Wetland_Marsh" = 0.12,
  "Ice" = 0.12, "Tundra" = 0.08
)

# Legend sizes
legend_size_map <- c(
  "Forest" = 4, "Grassland_Shrubland" = 4, "Cropland" = 4,
  "Impervious" = 3.0, "Bare" = 3.0, "Wetland_Marsh" = 4,
  "Ice" = 4, "Tundra" = 3.0
)

# Map-specific shapes (Wetland same as Grassland)
shape_map_values <- shape_values_all
shape_map_values["Wetland_Marsh"] <- shape_values_all["Grassland_Shrubland"]

# Filter to present landcovers
present_landcovers <- desired_land_order[desired_land_order %in% unique(combined_df$major_land)]
my_landcover_colors <- landcover_colors[present_landcovers]

# Create factor
combined_df <- combined_df %>%
  dplyr::mutate(landcover_factor = factor(major_land, levels = present_landcovers)) %>%
  dplyr::filter(!is.na(landcover_factor))

# Map setup
world <- ggplot2::map_data("world")
uk_xlim <- c(-10, 2); uk_ylim <- c(49, 61)
scandinavia_xlim <- c(4, 30); scandinavia_ylim <- c(50, 72)
australia_xlim <- c(135,155); australia_ylim <- c(-40,-28)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map"

# Determine wetland key (Tundra if present, otherwise Wetland_Marsh)
wetland_key <- if ("Tundra" %in% present_landcovers) "Tundra" else "Wetland_Marsh"

# Global map
global_base <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  coord_sf(crs = st_crs(3857), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    legend.position = c(0.05, 0.65), legend.justification = c("left","top"),
    legend.text = element_text(size = 12), legend.title = element_blank()
  )

global_map <- global_base +
  geom_point(data = combined_df %>% dplyr::filter(landcover_factor != "Forest" & landcover_factor != wetland_key),
             aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor),
             size = 2.3, alpha = 0.60, stroke = 0.12) +
  geom_point(data = combined_df %>% dplyr::filter(landcover_factor == "Forest"),
             aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor),
             size = 2.0, alpha = 0.45, stroke = 0.12) +
  geom_point(data = combined_df %>% dplyr::filter(landcover_factor == wetland_key),
             aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
             size = point_size_jitter[wetland_key], stroke = 1, alpha = 0.90) +
  geom_point(data = combined_df %>% dplyr::filter(landcover_factor == "Impervious"),
             aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
             size = 2.3, stroke = 0, alpha = 0.50) +
  geom_point(data = combined_df %>% dplyr::filter(landcover_factor == "Grassland_Shrubland"),
             aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
             size = 2.3, stroke = 0.12, alpha = 0.70) +
  scale_fill_manual(name = "Land Cover", values = my_landcover_colors, labels = display_labels[present_landcovers], drop = FALSE) +
  scale_color_manual(name = "Land Cover", values = my_landcover_colors, labels = display_labels[present_landcovers], drop = FALSE) +
  scale_shape_manual(name = "Land Cover", values = shape_map_values[present_landcovers], labels = display_labels[present_landcovers], drop = FALSE) +
  guides(fill = guide_legend(override.aes = list(
    shape = unname(shape_map_values[present_landcovers]),
    fill = unname(my_landcover_colors),
    colour = unname(my_landcover_colors),
    size = unname(legend_size_map[present_landcovers]),
    alpha = 1.0, stroke = unname(stroke_map[present_landcovers])
  )), shape = "none", color = "none")

# Regional insets
create_regional_map <- function(xlim, ylim, data_df) {
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    geom_point(data = data_df %>% dplyr::filter(landcover_factor != "Forest" & landcover_factor != wetland_key),
               aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor),
               size = 2.3, alpha = 0.60, stroke = 0.12) +
    geom_point(data = data_df %>% dplyr::filter(landcover_factor == "Forest"),
               aes(x = longitude, y = latitude, fill = landcover_factor, shape = landcover_factor, color = landcover_factor),
               size = 2.0, alpha = 0.45, stroke = 0.12) +
    geom_point(data = data_df %>% dplyr::filter(landcover_factor == "Impervious"),
               aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
               size = 2.3, stroke = 0, alpha = 0.90) +
    geom_point(data = data_df %>% dplyr::filter(landcover_factor == "Grassland_Shrubland"),
               aes(x = longitude, y = latitude, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
               size = 2.3, stroke = 0.12, alpha = 0.70) +
    scale_fill_manual(values = my_landcover_colors, drop = FALSE, guide = "none") +
    scale_color_manual(values = my_landcover_colors, drop = FALSE, guide = "none") +
    scale_shape_manual(values = shape_map_values[present_landcovers], drop = FALSE, guide = "none") +
    coord_sf(xlim = xlim, ylim = ylim, crs = st_crs(3857), expand = FALSE) +
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5))
}

df_uk <- combined_df %>% dplyr::filter(longitude >= uk_xlim[1], longitude <= uk_xlim[2], latitude >= uk_ylim[1], latitude <= uk_ylim[2])
df_scandinavia <- combined_df %>% dplyr::filter(longitude >= scandinavia_xlim[1], longitude <= scandinavia_xlim[2], latitude >= scandinavia_ylim[1], latitude <= scandinavia_ylim[2])
df_australia <- combined_df %>% dplyr::filter(longitude >= australia_xlim[1], longitude <= australia_xlim[2], latitude >= australia_ylim[1], latitude <= australia_ylim[2])

uk_inset <- create_regional_map(uk_xlim, uk_ylim, df_uk)
scandinavia_inset <- create_regional_map(scandinavia_xlim, scandinavia_ylim, df_scandinavia)
australia_inset <- create_regional_map(australia_xlim, australia_ylim, df_australia)

inset_dims <- function(n) {
  if (is.na(n) || n <= 0) return(NULL)
  if (n < 5) return(list(w = 0.12, h = 0.12))
  if (n < 15) return(list(w = 0.18, h = 0.18))
  return(list(w = 0.25, h = 0.25))
}

uk_dims <- inset_dims(nrow(df_uk))
scandinavia_dims <- inset_dims(nrow(df_scandinavia))
australia_dims <- inset_dims(nrow(df_australia))

final_map <- ggdraw(global_map)
if (!is.null(uk_dims)) final_map <- final_map + draw_plot(uk_inset, x = 0.345, y = 0.447, width = uk_dims$w, height = uk_dims$h)
if (!is.null(scandinavia_dims)) final_map <- final_map + draw_plot(scandinavia_inset, x = 0.524, y = 0.47, width = scandinavia_dims$w, height = scandinavia_dims$h)
if (!is.null(australia_dims)) final_map <- final_map + draw_plot(australia_inset, x = 0.56, y = 0.12, width = australia_dims$w, height = australia_dims$h)
final_map <- final_map +
  draw_line(x = c(0.419,0.48), y = c(0.68,0.71), color = "black", size = 0.7) +
  draw_line(x = c(0.54,0.58), y = c(0.72,0.66), color = "black", size = 0.7) +
  draw_line(x = c(0.788,0.85), y = c(0.26,0.32), color = "black", size = 0.7)

p_map_labeled <- final_map + labs(tag = "a") + theme(plot.tag = element_text(size = 16, hjust = 0, vjust = 1, face = "plain"), plot.tag.position = c(0.02, 0.98))

# Slope boxplot
p_slope <- ggplot(combined_df, aes(x = slope_estimate, y = landcover_factor)) +
  geom_boxplot(aes(fill = landcover_factor), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(fill = landcover_factor, color = landcover_factor, shape = landcover_factor, size = landcover_factor),
              width = 0, height = 0.2, stroke = 0.12, alpha = 0.75) +
  geom_jitter(data = combined_df %>% dplyr::filter(landcover_factor == wetland_key),
              aes(x = slope_estimate, y = landcover_factor, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
              width = 0, height = 0.2, size = point_size_jitter[wetland_key], stroke = 1, alpha = 0.98, inherit.aes = FALSE) +
  geom_jitter(data = combined_df %>% dplyr::filter(landcover_factor == "Impervious"),
              aes(x = slope_estimate, y = landcover_factor, shape = landcover_factor, color = landcover_factor, fill = landcover_factor),
              width = 0, height = 0.2, size = point_size_jitter["Impervious"], stroke = 0, alpha = 0.75, inherit.aes = FALSE) +
  scale_fill_manual(values = my_landcover_colors) +
  scale_color_manual(values = my_landcover_colors) +
  scale_shape_manual(values = shape_values_all[present_landcovers]) +
  scale_size_manual(values = point_size_jitter[present_landcovers], guide = "none") +
  labs(x = "CQ Slope Estimate", y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(present_landcovers), labels = rev(display_labels[present_landcovers])) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust = 0.5), axis.title.x = element_text(margin = margin(t = 8))) +
  labs(tag = "b)") +
  theme(plot.tag = element_text(size = 16, hjust = 0))

# Drainage area panel
combined_df_drainage <- combined_df %>% dplyr::filter(!is.na(drainage_area))
var_vec <- combined_df_drainage$drainage_area
var_breaks <- unname(quantile(var_vec, probs = seq(0, 1, 0.25), na.rm = TRUE))
var_breaks <- round(var_breaks, 0)
var_labels <- vapply(seq_len(length(var_breaks)-1), function(i) {
  lo <- var_breaks[i]
  hi <- var_breaks[i+1]
  if (i == 1) paste0("\u2264 ", hi) else paste0(lo, " - ", hi)
}, FUN.VALUE = character(1))

panel_df <- combined_df_drainage %>%
  dplyr::mutate(panel_bin = cut(drainage_area, breaks = var_breaks, include.lowest = TRUE, labels = var_labels))

quartile_summary <- panel_df %>%
  dplyr::group_by(panel_bin, major_land) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(panel_bin) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()

quartile_summary$major_land <- factor(quartile_summary$major_land, levels = present_landcovers)

p_drainage <- ggplot(quartile_summary, aes(x = panel_bin, y = prop, fill = major_land)) +
  geom_bar(stat = "identity", position = "fill", color = "gray20") +
  scale_fill_manual(values = my_landcover_colors, breaks = present_landcovers, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = expression(paste("Drainage area (", km^2, ")")), y = "Proportion of sites") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(tag = "c)") +
  theme(plot.tag = element_text(size = 16, hjust = 0))

# Combine and save
final_boxplots <- p_slope + p_drainage
combined_figure <- ggarrange(p_map_labeled, final_boxplots, ncol = 1, nrow = 2, heights = c(3, 2.5), align = "v", labels = NULL)

out_file <- file.path(output_dir, "Fig_landcover_cqslope_drainage_area.png")
ggsave(out_file, combined_figure, width = 8, height = 8.5, dpi = 300, bg = "white")
cat("Saved figure to:", out_file, "\n")
