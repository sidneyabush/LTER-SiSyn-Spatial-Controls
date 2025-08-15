# #############################################################################
# Figure1: Create final map plot 
# #############################################################################
# Required inputs:
#   1) <drv_dir>/AllDrivers_Harmonized_Yearly_filtered_5_years.csv
#   2) <drv_dir>/Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv
#
# Outputs created:
#   A) <output_dir>/Fig1_map_and_boxplots.png
# #############################################################################

rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

librarian::shelf(dplyr, stringr, ggplot2, maps, patchwork, scales, colorspace, ggrepel, 
                 ggspatial, sf, ggpubr, cowplot)

# #############################################################################
# 1. Data Preparation  
# #############################################################################

# 1a) Final site-years 
drivers_df_final_sites <- read.csv("AllDrivers_Harmonized_Yearly_filtered_5_years.csv", stringsAsFactors = FALSE) %>%
  dplyr::distinct(Stream_ID, .keep_all = TRUE)

# 1b) Site reference table -> build Stream_ID, then keep lat/long
site_ref <- read.csv("Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv",
                     check.names = FALSE, stringsAsFactors = FALSE) %>%
  dplyr::distinct(Stream_Name, .keep_all = TRUE) %>%
  dplyr::mutate(
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Latitude  = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  dplyr::select(Stream_ID, Latitude, Longitude)

# 1c) Attach coordinates to the recent30 set
drivers_df_filtered <- drivers_df_final_sites %>%
  dplyr::left_join(site_ref, by = "Stream_ID") %>%
  dplyr::filter(!is.na(Latitude), !is.na(Longitude))

# #############################################################################
# 2. Add lithology clusters 
# #############################################################################
site_clusters <- drivers_df_filtered %>%
  dplyr::distinct(Stream_ID,
                  major_rock,
                  rocks_volcanic, rocks_sedimentary,
                  rocks_carbonate_evaporite, rocks_metamorphic, rocks_plutonic) %>%
  dplyr::mutate(
    consolidated_rock = dplyr::case_when(
      major_rock %in% c("volcanic","volcanic; plutonic") ~ "Volcanic",
      major_rock %in% c("sedimentary", "sedimentary; metamorphic",
                        "sedimentary; carbonate_evaporite",
                        "volcanic; sedimentary; carbonate_evaporite",
                        "sedimentary; plutonic; carbonate_evaporite; metamorphic") ~ "Sedimentary",
      major_rock %in% c("plutonic","plutonic; metamorphic","volcanic; plutonic; metamorphic") ~ "Plutonic",
      major_rock %in% c("metamorphic","carbonate_evaporite; metamorphic") ~ "Metamorphic",
      major_rock %in% c("carbonate_evaporite","volcanic; carbonate_evaporite") ~ "Carbonate Evaporite",
      TRUE ~ NA_character_
    ),
    final_cluster = dplyr::case_when(
      consolidated_rock == "Sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "Sedimentary" & rocks_sedimentary <  70 ~ "Mixed Sedimentary",
      TRUE ~ consolidated_rock
    )
  ) %>%
  dplyr::select(Stream_ID, consolidated_rock, final_cluster)  


# Join clusters back onto the recent30 + coordinates, then convert to sf for mapping
drivers_sf <- sf::st_as_sf(
  drivers_df_filtered,
  coords = c("Longitude", "Latitude"),
  crs    = 4326
) %>%
  dplyr::mutate(
    Longitude = sf::st_coordinates(.)[,1],
    Latitude  = sf::st_coordinates(.)[,2]
  )

sites_with_clusters <- drivers_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(site_clusters, by = "Stream_ID") %>%
  dplyr::filter(!is.na(final_cluster), !is.na(consolidated_rock))

# Colors (same as before)
my_cluster_colors <- c(
  "Volcanic"            = "#AC7B32",
  "Sedimentary"         = "#579C8E",
  "Mixed Sedimentary"   = "#89C8A0",
  "Plutonic"            = "#8D9A40",
  "Metamorphic"         = "#C26F86",
  "Carbonate Evaporite" = "#5E88B0"
)

shape_map <- c(
  "Volcanic"            = 22,  
  "Sedimentary"         = 21,  
  "Mixed Sedimentary"   = 21,   
  "Plutonic"            = 23,  
  "Metamorphic"         = 24,  
  "Carbonate Evaporite" = 25   
)

# One factor used everywhere for aesthetics
sites_with_clusters <- sites_with_clusters %>%
  dplyr::mutate(
    legend_lith = factor(final_cluster, levels = names(my_cluster_colors))
  )

# #############################################################################
# 3. Global Map (Panel A)
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

# global_map <- global_base +
#   geom_point(data = sites_with_clusters,
#              aes(x = Longitude, y = Latitude, fill = final_cluster),
#              shape = 21, size = 2, alpha = 0.7, stroke = 0.1, color = "gray20") +
#   scale_fill_manual(values = my_cluster_colors,
#                     guide = guide_legend(override.aes = list(size = 4, alpha = 1)))

global_map <- global_base +
  geom_point(
    data = sites_with_clusters,
    aes(x = Longitude, y = Latitude,
        fill = legend_lith,         # same variable
        shape = legend_lith),       # same variable
    size = 2, alpha = 0.7, stroke = 0.1, color = "gray20"
  ) +
  scale_fill_manual(name = "Lithology", values = my_cluster_colors, drop = FALSE) +
  scale_shape_manual(name = "Lithology", values = shape_map, drop = FALSE) +
  guides(
    # because both scales share the same name and variable, ggplot merges them
    fill = guide_legend(override.aes = list(color = "gray20", size = 4, alpha = 1, stroke = 0.1))
  )

# #############################################################################
# 4) Regional Insets
# #############################################################################
# create_regional_map <- function(xlim, ylim, data_df) {
#   ggplot() +
#     geom_polygon(data = world, aes(x = long, y = lat, group = group),
#                  fill = "lightgray", color = "white") +
#     geom_point(data = data_df,
#                aes(x = Longitude, y = Latitude, fill = final_cluster),
#                shape = 21, size = 2, alpha = 0.7, stroke = 0.1, color = "gray20") +
#     scale_fill_manual(values = my_cluster_colors, guide = "none") +
#     coord_sf(xlim = xlim, ylim = ylim, crs = st_crs(3857), expand = FALSE) +
#     theme_void() +
#     theme(panel.background = element_rect(fill = "white", color = NA),
#           plot.background  = element_rect(fill = "white", color = NA),
#           panel.border     = element_rect(color = "black", fill = NA, size = 0.5))
# }

create_regional_map <- function(xlim, ylim, data_df) {
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "lightgray", color = "white") +
    geom_point(
      data = data_df,
      aes(x = Longitude, y = Latitude,
          fill = legend_lith,
          shape = legend_lith),
      size = 2, alpha = 0.7, stroke = 0.1, color = "gray20"
    ) +
    scale_fill_manual(values = my_cluster_colors, drop = FALSE, guide = "none") +
    scale_shape_manual(values = shape_map, drop = FALSE, guide = "none") +
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
df_uk <- filter(sites_with_clusters, Longitude >= uk_xlim[1], Longitude <= uk_xlim[2], Latitude >= uk_ylim[1], Latitude <= uk_ylim[2])
df_scandinavia <- filter(sites_with_clusters, Longitude >= scandinavia_xlim[1], Longitude <= scandinavia_xlim[2], Latitude >= scandinavia_ylim[1], Latitude <= scandinavia_ylim[2])
df_australia <- filter(sites_with_clusters, Longitude >= australia_xlim[1], Longitude <= australia_xlim[2], Latitude >= australia_ylim[1], Latitude <= australia_ylim[2])

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
# 5) Boxplots (Panels B & C) plotting average by site (FNConc / FNYield)
# #############################################################################
site_summary <- sites_with_clusters %>%
  dplyr::group_by(Stream_ID, final_cluster, consolidated_rock) %>%  # keep lithology for shapes
  dplyr::summarise(
    FNConc  = mean(FNConc,  na.rm = TRUE),
    FNYield = mean(FNYield, na.rm = TRUE),
    .groups = "drop"
  )

site_summary$final_cluster     <- factor(site_summary$final_cluster,     levels = names(my_cluster_colors))
site_summary$consolidated_rock <- factor(site_summary$consolidated_rock, levels = names(shape_map))

p1 <- ggplot(site_summary, aes(x = FNConc, y = final_cluster)) +
  geom_boxplot(aes(fill = final_cluster), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(fill = final_cluster, color = final_cluster, shape = consolidated_rock),  # <-- map shape
              width = 0.1, size = 2.2, stroke = 0.1, alpha = 0.6) +
  scale_fill_manual(values = my_cluster_colors) +
  scale_color_manual(values = my_cluster_colors) +
  scale_shape_manual(values = shape_map, drop = FALSE) +  # <-- add shape scale
  labs(x = expression("Concentration (mg L"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(site_summary$final_cluster))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(site_summary, aes(x = FNYield, y = final_cluster)) +
  geom_boxplot(aes(fill = final_cluster), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(fill = final_cluster, color = final_cluster, shape = consolidated_rock),  
              width = 0.1, size = 2.2, stroke = 0.1, alpha = 0.6) +
  scale_fill_manual(values = my_cluster_colors) +
  scale_color_manual(values = my_cluster_colors) +
  scale_shape_manual(values = shape_map, drop = FALSE) +  
  labs(x = expression("Yield (kg km"^-2*" year"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(site_summary$final_cluster))) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

p1_labeled <- p1 + labs(tag = "b)") + theme(plot.tag = element_text(size = 16, hjust = 0))
p2_labeled <- p2 + labs(tag = "c)") + theme(plot.tag = element_text(size = 16, hjust = 0))
final_boxplots <- p1_labeled + p2_labeled

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
output_dir <- file.path("..", "Final_Figures")

ggsave(file.path(output_dir, "Fig1_map_and_boxplots.png"), combined_figure,
       width = 8, height = 8.5, dpi = 300, bg = "white")
