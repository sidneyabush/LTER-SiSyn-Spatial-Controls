
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

FNConc_clusters <- read.csv("FNConc_Stream_ID_Year_Cluster.csv")
# FNYield_clusters <- read.csv("FNYield_Stream_ID_Year_Cluster.csv")
  
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

# --------------------------------------------------
# 4) Inset Map for UK/Europe (Black & White, Positioned Over Northern Africa)
# --------------------------------------------------
# Define bounding box for inset map (UK/Europe)
inset_xlim <- c(-10, 30)  
inset_ylim <- c(49, 72)

# Extract coordinates BEFORE filtering
drivers_df_inset <- drivers_df_filtered %>%
  mutate(Longitude = st_coordinates(.)[,1], Latitude = st_coordinates(.)[,2]) %>%
  filter(Longitude > inset_xlim[1] & Longitude < inset_xlim[2],
         Latitude > inset_ylim[1] & Latitude < inset_ylim[2]) 

# Create inset map with proper projection
inset_map <- ggplot() +
  geom_polygon(
    data = world, aes(x = long, y = lat, group = group),
    fill = "lightgray", color = "white", linewidth = 0.4
  ) +
  geom_point(
    data = drivers_df_inset,
    aes(x = Longitude, y = Latitude, fill = Name),
    shape = 21, size = 3, stroke = 0.3, color = "black"
  ) +
  scale_fill_manual(values = cbPalette_lighter) +
  
  # ✅ Keep the inset map in EPSG:4326 (same as world map)
  coord_sf(expand = FALSE) +
  
  # ✅ Scale bar in km
  annotation_scale(
    location = "br",
    width_hint = 0.3,
    height = unit(0.3, "cm"),
    text_cex = 0.7,
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.2, "cm"),
    style = "bar",
    unit_category = "metric"  # ✅ Force km instead of meters
  ) +
  
  # Solid white background
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Convert inset to a grob
inset_grob <- ggplotGrob(inset_map)

plot(inset_grob)

# --------------------------------------------------
# 5) Correctly Position Inset Over Egypt & Saudi Arabia
# --------------------------------------------------
# Define exact bounding box for inset & black outline
inset_xmin <- -20    # Move to center over Egypt/Saudi Arabia
inset_xmax <- 100    # Make it bigger
inset_ymin <- -50     # Lower bound
inset_ymax <- 20    # Upper bound

# Ensure the inset map is exactly this size
inset_grob <- ggplotGrob(inset_map)  # Convert inset to grob

p_labeled_inset <- p_labeled +
  # Place inset map with exact dimensions
  annotation_custom(
    grob = inset_grob,
    xmin = inset_xmin, xmax = inset_xmax,
    ymin = inset_ymin, ymax = inset_ymax
  ) +
  
  # Callout lines from inset to UK/Europe region
  geom_segment(aes(x = 10, y = 55, xend = inset_xmin + 5, yend = inset_ymax), linewidth = 0.5) +  
  geom_segment(aes(x = 35, y = 65, xend = inset_xmax - 5, yend = inset_ymax), linewidth = 0.5) +  
  
  # Make the black outline EXACTLY match the inset
  annotate(
    "rect", xmin = inset_xmin, xmax = inset_xmax, ymin = inset_ymin, ymax = inset_ymax,
    color = "black", fill = NA, linewidth = 1  # Keep a strong border
  )

# Print the corrected map
print(p_labeled_inset)

# Save the final map
ggsave("world_map_with_corrected_inset_egypt_saudi.png", p_labeled_inset, width = 10, height = 7, dpi = 300)

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
  theme(legend.position = "none")

p2 <- ggplot(drivers_df_filtered, aes(x = FNYield, y = Name)) +
  geom_boxplot(aes(fill = Name), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = Name), width = 0.1, size = 2) +
  scale_fill_manual(values = cbPalette_lighter2) +
  scale_color_manual(values = cbPalette_lighter) +
  labs(x = expression("Yield (kg km"^-2*" year"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(drivers_df_filtered$Name))) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank()
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
final_boxplots_labeled <- final_boxplots +
  theme(
    # Increase left margin to ensure the y-axis text aligns with the map
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )

# --------------------------------------------------
# 4) Combine vertically with map bigger
# --------------------------------------------------
# Adjust margins for the map (Panel A) and the boxplots (Panel B) so they line up.
p_labeled_aligned <- p_labeled +
  theme(plot.margin = margin(t = 10, r = 5, b = 0, l = 5))  # adjust as needed

final_boxplots_labeled_aligned <- final_boxplots_labeled +
  theme(plot.margin = margin(t = 0, r = 5, b = 5, l = 5))  # same left margin

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
    Precipitation = mean(precip, na.rm = TRUE),   
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
p_temp_precip <- ggplot(df_lter, aes(x = Temperature, y = Precipitation*365,
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
 