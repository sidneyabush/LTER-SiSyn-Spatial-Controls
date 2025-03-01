librarian::shelf(dplyr, stringr, ggplot2, maps, patchwork, scales, 
                 colorspace, ggrepel, ggspatial, sf, ggpubr)

# --------------------------------------------------
# 1) Data preparation (same as your script)
# --------------------------------------------------
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
rm(list = ls())

record_length <- 5

drivers_df_uncleaned <- read.csv(sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length)) %>%
  distinct(Stream_ID, .keep_all = TRUE)

drivers_df_final_sites <- read.csv(sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length)) %>%
  distinct(Stream_ID, .keep_all = TRUE)
  
drivers_df_filtered <- drivers_df_final_sites %>%
  left_join(drivers_df_uncleaned, by = "Stream_ID") %>%
  select(-ends_with(".y"), -contains("Gen"), -contains("Flux")) %>%
  select(-Year.x, -X, -DecYear, -num_days, -drainSqKm, -chemical, 
         -Stream_Name, -LTER, -contains("Coord"), -cycle0, -prop_area, 
         -contains("elevation_"), -contains("basin_slope_"), -contains("major"),
         -contains("permafrost_"), -Use_WRTDS) %>%
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))

drivers_df_filtered$Name <- factor(
  drivers_df_filtered$Name,
  levels = c(
    "Tropical", "Humid Subtropical", "Humid Temperate", 
    "Humid Continental", "Mediterranean", "Semi-Arid", 
    "Arid", "Subarctic"
  )
)

# Ensure dataset remains in WGS84 (for lat/lon compatibility)
drivers_df_filtered <- st_as_sf(drivers_df_filtered, 
                                coords = c("Longitude", "Latitude"), crs = 4326)

drivers_df_filtered <- drivers_df_filtered %>%
  mutate(Longitude = st_coordinates(.)[,1],
         Latitude = st_coordinates(.)[,2])

cbPalette_named <- c(
  "Tropical"         = "#145A32",  # Deep Forest Green (Strongest, Darkest Green)
  "Humid Subtropical" = "#3F7E5B",  # Rich Leaf Green (More Vibrant but Earthy)
  "Humid Temperate"  = "#A2BFA1",  # Muted Sage Green (Softer, More Neutral)
  "Humid Continental" = "#1F78B4",  # Medium Blue (Dfb)
  "Mediterranean"    = "#35878F",  # Rich Teal/Blue-Green (Still Cool & Wet)
  "Semi-Arid"        = "#D4A017",  # Deeper Golden Yellow (Earthy, Less Pastel)
  "Arid"            = "#C55A11",  # Burnt Orange (More Natural)
  "Subarctic"       = "#A75078"   # Deepened Pinkish Purple (Colder but Stronger)
)


cbPalette_lighter <- lighten(cbPalette_named, amount = 0.3)
cbPalette_lighter2 <- lighten(cbPalette_named, amount = 0.4)

# --------------------------------------------------
# 2) MAIN WORLD PANEL (Panel A)
# --------------------------------------------------
world <- map_data("world")

p <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "lightgray",
    color = "white"
  ) +
  geom_point(
    data = drivers_df_filtered,
    aes(x = Longitude, y = Latitude, fill = Name),
    shape = 21,
    size = 2,
    alpha = 0.8,
    stroke = 0.1,
    color = "gray2"
  ) +
  scale_fill_manual(
    values = cbPalette_lighter,
    guide = guide_legend(override.aes = list(size = 5))  # Increase legend point size
  ) +
  # 2a) Use coord_sf with bounding box to reduce empty space
  coord_sf(
    crs = st_crs(3857), 
    expand = FALSE
  ) +
  # 2b) Add scale and north arrow
  annotation_scale(
    location = "bl",  
    width_hint = 0.5,  
    unit_category = "metric",
    transform = TRUE,     
    plot_unit = "km",
    pad_x = unit(11, "cm"),  
    pad_y = unit(0.1, "cm") 
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(0.7, "cm"),
    width = unit(0.5, "cm"),
    pad_x = unit(0.1, "cm"),
    pad_y = unit(0.1, "cm"),
    style = north_arrow_orienteering()
  ) +
  theme_minimal() +
  labs(fill = NULL) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.05, 0.65),
    legend.justification = c("left", "top")
  )

# Label as Panel A
p_labeled <- p + 
  labs(tag = "A") +
  theme(
    plot.tag = element_text(size = 16, hjust = 0, vjust = 1)
  )

print(p_labeled)

# # --------------------------------------------------
# # 4) Inset Map for UK/Europe (Black & White, Positioned Over Northern Africa)
# # --------------------------------------------------
# # Define bounding box for inset map (UK/Europe)
# inset_xlim <- c(-10, 30)  
# inset_ylim <- c(49.5, 72)
# 
# # Extract coordinates BEFORE filtering
# drivers_df_inset <- drivers_df_filtered %>%
#   mutate(Longitude = st_coordinates(.)[,1], Latitude = st_coordinates(.)[,2]) %>%
#   filter(Longitude > inset_xlim[1] & Longitude < inset_xlim[2],
#          Latitude > inset_ylim[1] & Latitude < inset_ylim[2]) 
# 
# # Create inset map with proper projection
# inset_map <- ggplot() +
#   geom_polygon(
#     data = world, aes(x = long, y = lat, group = group),
#     fill = "lightgray", color = "white", linewidth = 0.4
#   ) +
#   geom_point(
#     data = drivers_df_inset,
#     aes(x = Longitude, y = Latitude, fill = Name),
#     shape = 21, size = 3, stroke = 0.3, color = "black"
#   ) +
#   scale_fill_manual(values = cbPalette_lighter) +
#   
#   # Keep the inset map in EPSG:4326 (same as world map)
#   coord_sf(
#     xlim = inset_xlim,  # Explicit zoom for longitude
#     ylim = inset_ylim,  # Explicit zoom for latitude
#     expand = FALSE
#   ) +
# 
#   # Scale bar in km
#   annotation_scale(
#     location = "br",
#     width_hint = 0.3,
#     height = unit(0.3, "cm"),
#      transform = TRUE,        # <== Add this line
#     text_cex = 0.7,
#     pad_x = unit(0.4, "cm"),
#     pad_y = unit(0.2, "cm"),
#     style = "bar",
#     unit_category = "metric"  # Force km instead of meters
#   ) +
#   
#   # Solid white background
#   theme_void() +
#   theme(
#     legend.position = "none",
#     panel.background = element_rect(fill = "white", color = "white"),
#     plot.background = element_rect(fill = "white", color = "white"),
#     panel.border = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Convert inset to a grob
# inset_grob <- ggplotGrob(inset_map)
# 
# plot(inset_grob)
# 
# # --------------------------------------------------
# # 5) Correctly Position Inset Over Egypt & Saudi Arabia
# # --------------------------------------------------
# # Define a larger bounding box for the inset
# inset_xmin <- 20    # Move box more to the east
# inset_xmax <- 60   # Make it wider
# inset_ymin <- 15    # Move lower
# inset_ymax <- 40    # Increase height
# 
# 
# # Ensure the inset map is exactly this size
# inset_grob <- ggplotGrob(inset_map)  # Convert inset to grob
# 
# p_labeled_inset <- p_labeled +
#   # Place inset map with exact dimensions
#   annotation_custom(
#     grob = inset_grob,
#     xmin = inset_xmin, xmax = inset_xmax,
#     ymin = inset_ymin, ymax = inset_ymax
#   ) +
#   
#   # Callout lines from inset to UK/Europe region
#   geom_segment(aes(x = 10, y = 55, xend = inset_xmin + 5, yend = inset_ymax), linewidth = 0.5) +  
#   geom_segment(aes(x = 35, y = 65, xend = inset_xmax - 5, yend = inset_ymax), linewidth = 0.5) +  
#   
#   # Make the black outline EXACTLY match the inset
#   annotate(
#     "rect", xmin = inset_xmin, xmax = inset_xmax, ymin = inset_ymin, ymax = inset_ymax,
#     color = "black", fill = NA, linewidth = 1  # Keep a strong border
#   )
# 
# # Print the corrected map
# print(p_labeled_inset)
# 
# # Save the final map
# ggsave("world_map_with_corrected_inset_egypt_saudi.png", p_labeled_inset, width = 10, height = 7, dpi = 300)

# --------------------------------------------------
# 3) Boxplots (Panel B)
# --------------------------------------------------
p1 <- ggplot(drivers_df_filtered, aes(x = FNConc, y = Name)) +
  geom_boxplot(aes(fill = Name), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = Name), width = 0.1, size = 2) +
  scale_fill_manual(values = cbPalette_lighter2) +
  scale_color_manual(values = cbPalette_lighter) +
  labs(x = expression("Concentration (mg L"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(drivers_df_filtered$Name))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(drivers_df_filtered, aes(x = FNYield, y = Name)) +
  geom_boxplot(aes(fill = Name), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = Name), width = 0.1, size = 2) +
  scale_fill_manual(values = cbPalette_lighter2) +
  scale_color_manual(values = cbPalette_lighter) +
  labs(x = expression("Yield (kg km"^-2*" year"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(drivers_df_filtered$Name))) +
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
p_labeled_aligned <- p_labeled 

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

ggsave("combined_map_boxplots_vertical_aligned_ggarrange.png", 
       combined_figure, width = 8, height = 8.5, dpi = 300)


# --------------------------------------------------
# 5) Plot Precip vs Temp -----
# --------------------------------------------------
df_lter <- drivers_df_filtered %>%
  mutate(LTER_extracted = str_extract(Stream_ID, "^[^_]+")) %>%
  mutate(LTER_extracted = case_when(
    LTER_extracted == "Swedish Goverment" ~ "Swedish Government",
    TRUE ~ LTER_extracted
  )) %>%
  group_by(LTER_extracted, Name) %>%  # Grouping by both LTER and Climate Class
  summarise(
    Temperature   = mean(temp, na.rm = TRUE),     
    Precipitation = mean(precip*365, na.rm = TRUE),   
    .groups = "drop"  # Prevents ungrouping warnings
  ) %>%
  mutate(Name = factor(Name, levels = c(
    "Tropical", "Humid Subtropical", "Humid Temperate", 
    "Humid Continental", "Mediterranean", "Semi-Arid", 
    "Arid", "Subarctic"
  )))  # Ensuring all categories exist in the factor levels


df_lter$Name <- factor(
  df_lter$Name,
  levels = c(
    "Tropical", "Humid Subtropical", "Humid Temperate", 
    "Humid Continental", "Mediterranean", "Semi-Arid", 
    "Arid", "Subarctic"
  )
)


# Create Plot C: Precipitation vs. Temperature, one point per LTER
p_temp_precip <- ggplot(df_lter, aes(x = Temperature, y = Precipitation,
                                     fill = Name, label = LTER_extracted)) +
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

# Save the figure as a high-resolution PNG file
ggsave("precip_temp_plot.png", p_temp_precip, width = 10, height = 9.5, dpi = 300)

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(maps)

# Define discrete color palettes for clusters
my_conc_cluster_colors <- c(
  "1" = "#88A2DC",  # Muted Blue (instead of bold primary blue)
  "2" = "#E69F00",  # Warm Muted Orange
  "3" = "#E2C744",  # Softer Yellow-Gold
  "4" = "#6BAE75",  # Desaturated Green (instead of bright primary green)
  "5" = "#A3A3A3",  # Muted Neutral Gray (lighter than before)
  "6" = "#B07AA1"   # Muted Purple-Pink
)

my_yield_cluster_colors <- c(
  "1" = "#AC7B32",  # Rich Ochre (Warm Earthy Brown-Gold)
  "2" = "#579C8E",  # Muted Teal (Cool & fresh)
  "3" = "#C26F86",  # Dusty Rose (Soft but warm)
  "4" = "#5E88B0"   # Soft Steel Blue (Cool contrast)
)

# 1. Read in the cluster data
FNConc_clusters <- read.csv("FNConc_Stream_ID_Year_Cluster.csv")
FNYield_clusters <- read.csv("FNYield_Stream_ID_Year_Cluster.csv")

# Helper function to calculate the mode (first in case of ties)
calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 2. Merge clusters with drivers_df to get lat/long values and compute modal cluster per site

# For FNConc: merge and compute modal cluster per stream_id
df_FNConc <- drivers_df_filtered %>%
  inner_join(FNConc_clusters, by = c("Stream_ID")) %>% 
  group_by(Stream_ID) %>%
  summarize(
    modal_cluster = calc_mode(cluster),
    lat = first(Latitude),
    long = first(Longitude),
    .groups = "drop"
  ) %>%
  mutate(modal_cluster = factor(modal_cluster))

# For FNYield: merge and compute modal cluster per stream_id
df_FNYield <- drivers_df_filtered %>%
  inner_join(FNYield_clusters, by = c("Stream_ID")) %>% 
  group_by(Stream_ID) %>%
  summarize(
    modal_cluster = calc_mode(cluster),
    lat = first(Latitude),
    long = first(Longitude),
    .groups = "drop"
  ) %>%
  mutate(modal_cluster = factor(modal_cluster))

# --------------------------------------------------
# World Map for Clusters
# --------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(sf)
library(ggspatial)

# 1. Get the world data
world_data <- map_data("world")

# 2. Create a base world map with your desired styling
world_map_base <- ggplot() +
  geom_polygon(
    data = world_data,
    aes(x = long, y = lat, group = group),
    fill = "lightgray",
    color = "white"
  ) +
  coord_sf(
    crs = st_crs(3857), 
    expand = FALSE
  ) +
  annotation_scale(
    location = "bl",  
    width_hint = 0.5,  
    unit_category = "metric",
    transform = TRUE,       
    plot_unit = "km",
    pad_x = unit(11, "cm"),  
    pad_y = unit(0.1, "cm")
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(0.7, "cm"),
    width = unit(0.5, "cm"),
    pad_x = unit(0.1, "cm"),
    pad_y = unit(0.1, "cm"),
    style = north_arrow_orienteering()
  ) +
  theme_minimal() +
  labs(fill = NULL) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.05, 0.65),
    legend.justification = c("left", "top")
  )

# 3. Prepare your cluster data and drivers data (assuming these objects already exist)
#    For FNConc:
df_FNConc <- drivers_df_filtered %>%
  inner_join(FNConc_clusters, by = c("Stream_ID")) %>% 
  group_by(Stream_ID) %>%
  summarize(
    modal_cluster = calc_mode(cluster),
    lat = first(Latitude),
    long = first(Longitude),
    .groups = "drop"
  )
# Ensure modal_cluster is a factor:
df_FNConc <- df_FNConc %>% mutate(modal_cluster = factor(modal_cluster))

# For FNYield:
df_FNYield <- drivers_df_filtered %>%
  inner_join(FNYield_clusters, by = c("Stream_ID")) %>% 
  group_by(Stream_ID) %>%
  summarize(
    modal_cluster = calc_mode(cluster),
    lat = first(Latitude),
    long = first(Longitude),
    .groups = "drop"
  )
# Ensure modal_cluster is a factor:
df_FNYield <- df_FNYield %>% mutate(modal_cluster = factor(modal_cluster))

# Define your cluster color palettes
my_conc_cluster_colors <- c(
  "1" = "#88A2DC",  # Muted Blue
  "2" = "#E69F00",  # Warm Muted Orange
  "3" = "#E2C744",  # Softer Yellow-Gold
  "4" = "#6BAE75",  # Desaturated Green
  "5" = "gray40",  # Muted Neutral Gray
  "6" = "#B07AA1"   # Muted Purple-Pink
)

my_yield_cluster_colors <- c(
  "1" = "#AC7B32",  # Rich Ochre
  "2" = "#579C8E",  # Muted Teal
  "3" = "#C26F86",  # Dusty Rose
  "4" = "#5E88B0"   # Soft Steel Blue
)

# 4. Create Panel A: Global map for FNConc modal clusters
p_FNConc_map <- world_map_base +
  geom_point(
    data = df_FNConc,
    aes(x = long, y = lat, fill = modal_cluster),
    shape = 21,
    size = 2,
    alpha = 0.8,
    stroke = 0.1,
    color = "gray2"
  ) +
  scale_fill_manual(
    values = my_conc_cluster_colors,
    guide = guide_legend(override.aes = list(size = 5))  # Increase legend point size
  ) +
  labs(title = NULL,
       x = "Longitude", y = "Latitude",
       tag = "A") +
  theme(
    plot.tag = element_text(size = 16, hjust = 0, vjust = 1),
    legend.position = c(0.05, 0.65),
    legend.justification = c("left", "top"), 
    legend.title = element_blank())

p_FNConc_map

# 5. Create Panel B: Global map for FNYield modal clusters
p_FNYield_map <- world_map_base +
  geom_point(
    data = df_FNYield,
    aes(x = long, y = lat, fill = modal_cluster),
    shape = 21,
    size = 2,
    alpha = 0.8,
    stroke = 0.1,
    color = "gray2"
  ) +
  scale_fill_manual(
    values = my_yield_cluster_colors,
    guide = guide_legend(override.aes = list(size = 5))  # Increase legend point size
  ) +
  labs(title = NULL,
       x = "Longitude", y = "Latitude",
       tag = "B") +
  theme(
    plot.tag = element_text(size = 16, hjust = 0, vjust = 1),
    legend.position = c(0.05, 0.65),
    legend.justification = c("left", "top"), 
    legend.title = element_blank())

# 6. Combine the two panels vertically
combined_map <- ggarrange(p_FNConc_map, p_FNYield_map, 
                          ncol = 1, nrow = 2,
                          heights = c(1, 1))

# Display the combined map
print(combined_map)

ggsave("global_map_clusters.png", 
       combined_map, width = 8, height = 8.5, dpi = 300)

