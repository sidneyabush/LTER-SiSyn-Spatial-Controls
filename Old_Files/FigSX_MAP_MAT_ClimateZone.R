library(dplyr)
library(stringr)
library(ggplot2)
library(colorspace)
library(ggrepel)

# --------------------------------------------------
# 1) Data Preparation
# --------------------------------------------------
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")
rm(list = ls())

record_length <- 5
drivers_df_uncleaned <- read.csv(
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length)
) %>% 
  distinct(Stream_ID, .keep_all = TRUE)

drivers_df_final_sites <- read.csv(
  sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length)
) %>% 
  distinct(Stream_ID, .keep_all = TRUE)

drivers_df_filtered <- drivers_df_final_sites %>%
  left_join(drivers_df_uncleaned, by = "Stream_ID") %>%
  select(-ends_with(".y"), -contains("Gen"), -contains("Flux")) %>%
  select(
    -Year.x, -X, -DecYear, -num_days, -drainSqKm, -chemical,
    -Stream_Name, -LTER, -contains("Coord"), -cycle0, -prop_area,
    -silicate_weathering, -contains("elevation_"), -contains("basin_slope_"),
    -contains("rocks"), -contains("land_"), -contains("permafrost_"),
    -Use_WRTDS
  ) %>%
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))

drivers_df_filtered$Name <- factor(
  drivers_df_filtered$Name,
  levels = c(
    "Tropical", "Humid Subtropical", "Humid Temperate",
    "Humid Continental", "Mediterranean", "Semi-Arid",
    "Arid", "Subarctic"
  )
)

# define color palette
cbPalette_named <- c(
  "Tropical"          = "#145A32",
  "Humid Subtropical" = "#3F7E5B",
  "Humid Temperate"   = "#A2BFA1",
  "Humid Continental" = "#1F78B4",
  "Mediterranean"     = "#35878F",
  "Semi-Arid"         = "#D4A017",
  "Arid"              = "#C55A11",
  "Subarctic"         = "#A75078"
)
cbPalette_lighter <- lighten(cbPalette_named, amount = 0.3)

# --------------------------------------------------
# 2) MAP vs MAT (Precip vs Temp) – FigSX
# --------------------------------------------------
df_lter <- drivers_df_filtered %>%
  mutate(LTER_extracted = str_extract(Stream_ID, "^[^_]+")) %>%
  group_by(LTER_extracted, Name) %>%
  summarise(
    Temperature   = mean(temp, na.rm = TRUE),
    Precipitation = mean(precip * 365, na.rm = TRUE),
    .groups       = "drop"
  )

p_temp_precip <- ggplot(df_lter, aes(x = Temperature, y = Precipitation, fill = Name, label = LTER_extracted)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray2", size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray2", size = 0.6) +
  geom_point(shape = 21, size = 5, stroke = 0.1, color = "gray2", alpha = 0.8) +
  geom_text_repel(size = 5, max.overlaps = Inf, point.padding = unit(1, "lines")) +
  scale_fill_manual(values = cbPalette_lighter) +
  labs(
    x = expression("Temperature ("*degree*C*")"),
    y = expression("Precipitation (mm)"),
    fill = NULL
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = c(0.9, 0.55)
  )

# --------------------------------------------------
# 3) Export FigSX
# --------------------------------------------------
output_dir <- file.path("..", "Final_Figures")
ggsave(
  filename = file.path(output_dir, "FigSX_temp_vs_precip.png"),
  plot     = p_temp_precip,
  width    = 10,
  height   = 9.5,
  dpi      = 300
)
