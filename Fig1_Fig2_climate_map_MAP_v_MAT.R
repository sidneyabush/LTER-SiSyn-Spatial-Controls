library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(patchwork)
library(scales)
library(colorspace)
library(ggrepel)
library(ggspatial)
library(sf)
library(ggpubr)
library(cowplot)

# --------------------------------------------------
# 1) Data Preparation
# --------------------------------------------------
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")
rm(list = ls())

record_length <- 5

drivers_df_uncleaned <- read.csv(sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length)) %>%
  distinct(Stream_ID, .keep_all = TRUE)

drivers_df_final_sites <- read.csv(sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length)) %>%
  distinct(Stream_ID, .keep_all = TRUE)

# Creating map
drivers_df_filtered <- drivers_df_final_sites %>%
  left_join(drivers_df_uncleaned, by = "Stream_ID") %>%
  select(-ends_with(".y"), -contains("Gen"), -contains("Flux")) %>%
  select(-Year.x, -X, -DecYear, -num_days, -drainSqKm, -chemical, 
         -Stream_Name, -LTER, -contains("Coord"), -cycle0, -prop_area, -silicate_weathering,
         -contains("elevation_"), -contains("basin_slope_"), -contains("rocks"),
         -contains("land_"),
         -contains("permafrost_"), -Use_WRTDS) %>%
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))


drivers_df_filtered$Name <- factor(
  drivers_df_filtered$Name,
  levels = c("Tropical", "Humid Subtropical", "Humid Temperate", 
             "Humid Continental", "Mediterranean", "Semi-Arid", 
             "Arid", "Subarctic")
)

cbPalette_named <- c(
  "Tropical"         = "#145A32",
  "Humid Subtropical" = "#3F7E5B",
  "Humid Temperate"  = "#A2BFA1",
  "Humid Continental" = "#1F78B4",
  "Mediterranean"    = "#35878F",
  "Semi-Arid"        = "#D4A017",
  "Arid"             = "#C55A11",
  "Subarctic"        = "#A75078"
)


cbPalette_lighter  <- lighten(cbPalette_named, amount = 0.3)
cbPalette_lighter2 <- lighten(cbPalette_named, amount = 0.4)

drivers_df_filtered <- drivers_df_filtered %>%
  filter(trimws(major_rock) != "") %>%
  mutate(consolidated_rock = case_when(
    # Pure categories
    major_rock == "volcanic"                                   ~ "volcanic",
    major_rock == "sedimentary"                                ~ "sedimentary",
    major_rock == "metamorphic"                                ~ "metamorphic",
    major_rock == "plutonic"                                   ~ "plutonic",
    
    # Treat carbonate_evaporite as sedimentary
    major_rock == "carbonate_evaporite"                       ~ "sedimentary",
    major_rock == "sedimentary; carbonate_evaporite"          ~ "sedimentary",
    
    # Pure igneous mixtures
    major_rock == "volcanic; plutonic"                         ~ "igneous",
    
    # Combinations that mix igneous with metamorphic
    major_rock %in% c("plutonic; metamorphic", 
                      "volcanic; plutonic; metamorphic")       ~ "igneous/metamorphic",
    
    # Mixed categories that include multiple types
    major_rock %in% c("sedimentary; plutonic; carbonate_evaporite; metamorphic", 
                      "volcanic; sedimentary; carbonate_evaporite",
                      "volcanic; carbonate_evaporite",
                      "sedimentary; metamorphic",
                      "carbonate_evaporite; metamorphic")       ~ "mixed"
  )) %>%
  # Convert consolidated_rock to a factor with the desired levels
  mutate(consolidated_rock = factor(consolidated_rock, 
                                    levels = c("volcanic", 
                                               "sedimentary", 
                                               "metamorphic", 
                                               "plutonic", 
                                               "igneous", 
                                               "igneous/metamorphic", 
                                               "mixed")))

cbPalette_named <- c(
  "volcanic"            = "#A23E34",  # muted, earthy red
  "sedimentary"         = "#BFA66B",  # muted sandy brown
  "metamorphic"         = "#6F6A8D",  # muted purple/blue
  "plutonic"            = "#476B66",  # muted teal/green
  "igneous"             = "#A98D6C",  # muted mustard/ochre
  "igneous/metamorphic" = "#7D8C4B",  # muted olive green
  "mixed"               = "#A89BAE"   # muted lavender/grey
)

drivers_df_filtered <- drivers_df_filtered %>%
  filter(trimws(major_land) != "") %>%
  # Consolidate forest types into "forest_all"
  mutate(consolidated_landcover = case_when(
    major_land %in% c("evergreen_needleleaf_forest", 
                      "evergreen_broadleaf_forest", 
                      "mixed_forest", 
                      "deciduous_needleleaf_forest", 
                      "deciduous_broadleaf_forest") ~ "forest_all",
    TRUE ~ major_land
  )) %>%
  # Convert to factor with desired ordering
  mutate(consolidated_landcover = factor(consolidated_landcover, 
                                         levels = c("forest_all", 
                                                    "shrubland_grassland",
                                                    "tundra",
                                                    "cropland",
                                                    "urban_and_built_up_land"))) %>%
  mutate(consolidated_landcover = recode(consolidated_landcover,
                                         "forest_all" = "Forest",
                                         "shrubland_grassland" = "Shrubland / Grassland",
                                         "tundra" = "Tundra",
                                         "cropland" = "Cropland",
                                         "urban_and_built_up_land" = "Urban & Built-up"))


cbPalette_named <- c(
  "Forest"                = "#4C9F70",  # Muted forest green
  "Shrubland / Grassland" = "#A3A847",  # Muted olive-yellow
  "Tundra"                = "#7AA1C2",  # Muted blue – distinct from grayish tones
  "Cropland"              = "#C68642",  # Brighter brown
  "Urban & Built-up"      = "#6E6E6E"   # Cooler dark gray
)


# Convert to sf in WGS84 (lat/lon)
drivers_df_filtered <- st_as_sf(drivers_df_filtered, 
                                coords = c("Longitude", "Latitude"), crs = 4326)
drivers_df_filtered <- drivers_df_filtered %>%
  mutate(Longitude = st_coordinates(.)[,1],
         Latitude = st_coordinates(.)[,2])

cbPalette_lighter  <- lighten(cbPalette_named, amount = 0.3)
cbPalette_lighter2 <- lighten(cbPalette_named, amount = 0.4)

# Choose the column you want to display on the maps. Name = ClimateZ, consolidated_rock, consolidated_landcover
map_variable <- "consolidated_landcover"  

# Assign the chosen variable to a unified column used for plotting.
drivers_df_filtered$map_category <- drivers_df_filtered[[map_variable]]

# Create a label for file names based on the selected variable.
map_var_label <- map_variable

# --------------------------------------------------
# 2) Create the Global Map (Panel A)
# --------------------------------------------------
world <- ggplot2::map_data("world")

global_map <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "lightgray",
    color = "white"
  ) +
  geom_point(
    data = drivers_df_filtered,
    aes(x = Longitude, y = Latitude, fill = map_category),  # now using map_category
    shape = 21, size = 2, alpha = 0.8, stroke = 0.1, color = "gray2"
  ) +
  scale_fill_manual(
    values = cbPalette_lighter,
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  coord_sf(crs = st_crs(3857), expand = FALSE) +
  theme_minimal() +
  labs(fill = NULL) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        legend.text      = element_text(size = 10),
        legend.position  = c(0.05, 0.65),
        legend.justification = c("left", "top"))

# --------------------------------------------------
# 3) Create Regional Maps with Hard-Coded Bounding Boxes
# --------------------------------------------------
# UK bounding box
uk_xlim <- c(-10, 2)
uk_ylim <- c(49, 61)

# Scandinavia bounding box (zoomed tighter, excluding the UK)
scandinavia_xlim <- c(4, 30)
scandinavia_ylim <- c(50, 72)

# Australia bounding box (zoom in on the bottom-right/southeast)
australia_xlim <- c(135, 155)
australia_ylim <- c(-40, -28)

create_regional_map <- function(xlim, ylim) {
  ggplot() +
    geom_polygon(
      data = world, 
      aes(x = long, y = lat, group = group),
      fill = "lightgray", color = "white"
    ) +
    geom_point(
      data = drivers_df_filtered,
      aes(x = Longitude, y = Latitude, fill = map_category),
      shape = 21, size = 2, alpha = 0.8, stroke = 0.1, color = "gray2"
    ) +
    scale_fill_manual(values = cbPalette_lighter) +
    coord_sf(
      xlim = xlim,
      ylim = ylim,
      crs = st_crs(3857),
      expand = FALSE
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      # White inside the black box
      panel.background = element_rect(fill = "white", color = NA),
      # A black outline around that white box
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      # Transparent outside that box
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  
  
}

uk_map <- create_regional_map(uk_xlim, uk_ylim)
scandinavia_map <- create_regional_map(scandinavia_xlim, scandinavia_ylim)
australia_map <- create_regional_map(australia_xlim, australia_ylim)

# --------------------------------------------------
# 4) Overlay the Regional Maps onto the Global Map using cowplot
# --------------------------------------------------
# Adjust positions
final_map <- ggdraw(global_map) +
  draw_plot(uk_map, x = 0.345, y = 0.447, width = 0.25, height = 0.25) +
  draw_plot(scandinavia_map, x = 0.524, y = 0.47, width = 0.25, height = 0.25) +
  draw_plot(australia_map, x = 0.56, y = 0.12, width = 0.25, height = 0.25) +
  draw_line(
    x = c(0.419, 0.48),
    y = c(0.68, 0.71),
    color = "black", linewidth = 0.7
  ) +
  # Scandinavia line: from bottom-center 
  draw_line(
    x = c(0.54, 0.58),
    y = c(0.72, 0.66),
    color = "black", linewidth = 0.7
  ) +
  # Australia line: from top-center 
  draw_line(
    x = c(0.788, 0.85),
    y = c(0.26, 0.32),
    color = "black", linewidth = 0.7
  )

# --------------------------------------------------
# 3) Boxplots (Panel B)
# --------------------------------------------------
p1 <- ggplot(drivers_df_filtered, aes(x = FNConc, y = map_category)) +
  geom_boxplot(aes(fill = map_category), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = map_category), width = 0.1, size = 2) +
  scale_fill_manual(values = cbPalette_lighter2) +
  scale_color_manual(values = cbPalette_lighter) +
  labs(x = expression("Concentration (mg L"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(drivers_df_filtered$map_category))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(drivers_df_filtered, aes(x = FNYield, y = map_category)) +
  geom_boxplot(aes(fill = map_category), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = map_category), width = 0.1, size = 2) +
  scale_fill_manual(values = cbPalette_lighter2) +
  scale_color_manual(values = cbPalette_lighter) +
  labs(x = expression("Yield (kg km"^-2*" year"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(drivers_df_filtered$map_category))) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Label only the concentration plot (p1) as "B"
p1_labeled <- p1 + 
  labs(tag = "B") +
  theme(
    plot.tag = element_text(size = 16, hjust = 0, vjust = 1)
  )

p2_labeled <- p2 + 
  labs(tag = "C") +
  theme(
    plot.tag = element_text(size = 16, hjust = 0, vjust = 1)
  )

# No label for p2
final_boxplots <- p1_labeled + p2_labeled

# Add a left margin to align with map
final_boxplots_labeled <- final_boxplots 
# --------------------------------------------------
# 4) Combine vertically with map bigger
# --------------------------------------------------
# Adjust margins for the map (Panel A) and the boxplots (Panel B) so they line up.
p_labeled_aligned <-final_map 

final_boxplots_labeled_aligned <- final_boxplots_labeled 

# Now, use ggarrange to stack them vertically
combined_figure <- ggarrange(
  p_labeled_aligned,
  final_boxplots_labeled_aligned,
  ncol = 1, nrow = 2,
  heights = c(3, 2.5),  # Keeps map larger, but reduces space
  align = "v",        # Ensure vertical alignment
  common.legend = FALSE, # Keeps legend only where needed
  labels = NULL,      # Avoids extra label space
  vjust = -2          # Move them closer together
)

print(p2)

print(combined_figure)

# --------------------------------------------------
# 5) Plot Precip vs Temp -----
# --------------------------------------------------
df_lter <- drivers_df_filtered %>%
  mutate(LTER_extracted = str_extract(Stream_ID, "^[^_]+")) %>%
  mutate(LTER_extracted = case_when(
    LTER_extracted == "Swedish Goverment" ~ "Swedish Government",
    TRUE ~ LTER_extracted
  )) %>%
  group_by(LTER_extracted, map_category) %>%  # Now this column exists
  summarise(
    Temperature   = mean(temp, na.rm = TRUE),     
    Precipitation = mean(precip * 365, na.rm = TRUE),   
    .groups = "drop"
  )



# Create Plot C: Precipitation vs. Temperature, one point per LTER
p_temp_precip <- ggplot(df_lter, aes(x = Temperature, y = Precipitation,
                                     fill = map_category, label = LTER_extracted)) +
  # Add dashed reference lines at Temperature = 0 and Precipitation = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray2", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray2", linewidth = 0.6) +
  geom_point(
    shape = 21,
    size = 5,
    stroke = 0.1,
    color = "gray2"  # outlines for the points
  ) +
  geom_text_repel(size = 5, max.overlaps = Inf, point.padding = unit(1, "lines")) +
  scale_fill_manual(values = cbPalette_lighter) +  # using your original palette
  labs(
    x = expression("Temperature ("*degree*C*")"),
    y = expression("Precipitation (mm)"),
    color = NULL
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = c(0.9, 0.55),
    legend.title =element_blank()
  )

# Print the figure
print(p_temp_precip)

# Save the combined map and boxplots with the map category in the filename
ggsave(sprintf("combined_map_boxplots_vertical_aligned_ggarrange_%s.png", map_var_label), 
       combined_figure, width = 8, height = 8.5, dpi = 300)

# Save the precipitation vs. temperature plot with the map category in the filename
ggsave(sprintf("precip_temp_plot_%s.png", map_var_label), 
       p_temp_precip, width = 10, height = 9.5, dpi = 300)


