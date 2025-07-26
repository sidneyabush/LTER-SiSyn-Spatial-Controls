# =============================================================================
# Prepare histogram input from split subsets
# =============================================================================
rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

# Load libraries
librarian::shelf(dplyr, readr, tidyr, stringr, ggplot2)

theme_set(
  theme_bw(base_size = 20) +
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank()
    )
)

# Read split datasets
older70     <- read_csv("AllDrivers_cc_older70.csv", show_col_types = FALSE)     %>% mutate(subset = "Training")
recent30    <- read_csv("AllDrivers_cc_recent30.csv", show_col_types = FALSE)    %>% mutate(subset = "Testing")
unseen10_df <- read_csv("AllDrivers_cc_unseen10.csv", show_col_types = FALSE)    %>% mutate(subset = "Cross-Validation")

# Combine and set order
hist_input_df <- bind_rows(older70, recent30, unseen10_df) %>%
  mutate(subset = factor(subset, levels = c("Training", "Testing", "Cross-Validation")))

# Pivot longer for plotting
hist_long <- hist_input_df %>%
  pivot_longer(cols = c(NOx, P, npp, evapotrans, greenup_day, precip, temp,
                        snow_cover, permafrost, elevation, basin_slope,
                        RBI, recession_slope,
                        starts_with("land_"), starts_with("rocks_")),
               names_to = "driver", values_to = "value") %>%
  mutate(value = if_else(driver %in% c("NOx", "P"), log10(value), value))

# Recode variable names to pretty labels
recode_map <- setNames(
  c("log(N)","log(P)","NPP","ET","Greenup Day","Precip","Temp","Snow Cover","Permafrost",
    "Elevation","Basin Slope","Flashiness (RBI)","Recession Curve Slope",
    "Land: Bare","Land: Cropland","Land: Forest","Land: Grass & Shrub",
    "Land: Ice & Snow","Land: Impervious","Land: Salt Water","Land: Tidal Wetland",
    "Land: Water Body","Land: Wetland Marsh","Rock: Volcanic","Rock: Sedimentary",
    "Rock: Carbonate Evaporite","Rock: Metamorphic","Rock: Plutonic"),
  
  c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
    "snow_cover","permafrost","elevation","basin_slope","RBI",
    "recession_slope","land_Bare","land_Cropland","land_Forest",
    "land_Grassland_Shrubland","land_Ice_Snow","land_Impervious",
    "land_Salt_Water","land_Tidal_Wetland","land_Water","land_Wetland_Marsh",
    "rocks_volcanic","rocks_sedimentary","rocks_carbonate_evaporite",
    "rocks_metamorphic","rocks_plutonic")
)

driver_order <- c(
  "log(N)", "log(P)", "NPP", "ET", "Greenup Day", "Precip",
  "Temp", "Snow Cover", "Permafrost", "Elevation", "Basin Slope", "Flashiness (RBI)",
  "Recession Curve Slope", "Land: Bare", "Land: Cropland", "Land: Forest",
  "Land: Grass & Shrub", "Land: Ice & Snow", "Land: Impervious",
  "Land: Salt Water", "Land: Tidal Wetland", "Land: Water Body",
  "Land: Wetland Marsh", "Rock: Volcanic", "Rock: Sedimentary",
  "Rock: Carbonate Evaporite", "Rock: Metamorphic", "Rock: Plutonic"
)

hist_long <- hist_long %>%
  mutate(
    driver = recode(driver, !!!recode_map),
    driver = factor(driver, levels = driver_order)
  )

# Create the plot object
p <- ggplot(hist_long, aes(x = value, fill = subset)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  facet_wrap(~ driver, scales = "free", ncol = 4) +
  scale_fill_manual(
    values = c(
      "Training"            = "gray70",  
      "Testing"             = "#B45A3E", 
      "Cross-Validation"    = "#2E7F6B"
    ),
    guide = guide_legend(override.aes = list(alpha = 1))
  ) +
  labs(x = "Value", y = "Count", fill = NULL) +
  theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(1.5, "lines"),   
      legend.text = element_text(size = 18),  
      legend.title = element_blank(),
      strip.background = element_blank())

# Save to file
ggsave(
  filename = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Figures/FigSX_histograms.png",
  plot     = p,
  width    = 16,
  height   = 18,
  dpi      = 300
)

# =============================================================================
# Save correlation plots for each subset (using driver order & recode map)
# =============================================================================

library(corrplot)

# Define variable subset to match the histogram drivers
drivers_to_use <- names(recode_map)

save_subset_corrplot <- function(df, label) {
  # Filter to the defined driver variables only
  numeric_df <- df %>%
    dplyr::select(all_of(drivers_to_use)) %>%
    dplyr::mutate(across(c(NOx, P), log10)) %>%  # Apply log transform for NOx and P
    dplyr::rename_with(~ recode_map[.x]) %>%
    dplyr::select(all_of(driver_order))  # Reorder columns for consistent plot layout
  
  # Compute correlation matrix
  cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")
  
  # Save plot
  png(sprintf("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Figures/FigSX_corrplot_%s.png", label),
      width = 2500, height = 2500, res = 300)
  corrplot(
    cor_matrix,
    type         = "lower",
    pch.col      = "black",
    tl.col       = "black",
    diag         = FALSE,
    na.label     = "X",              # or " " for space
    na.label.col = "grey70",         # this controls the symbol's color
    addgrid.col  = "grey90"          # optional: lighten the grid
  )
  
  title(paste("Correlation Plot –", label), line = 2.5)
  dev.off()
}

# Call for each subset
save_subset_corrplot(older70,     "Training")
save_subset_corrplot(recent30,    "Testing")
save_subset_corrplot(unseen10_df, "Cross_Validation")
