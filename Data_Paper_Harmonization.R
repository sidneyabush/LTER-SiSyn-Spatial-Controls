# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr)

# Clear environment
rm(list = ls())

# Set working directory (change this path as needed)
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# -----------------------------------------------------------
# 1. Load the Chemistry Sites Data (key file with Stream_Name) ----
# -----------------------------------------------------------
chemistry_sites <- read.csv("chemistry_sites_si.csv", stringsAsFactors = FALSE)

# -----------------------------------------------------------
# 2. Load the Finnish Sites Data (for use in spatial drivers) ----
# -----------------------------------------------------------
finn <- read.csv("FinnishSites.csv")
finn <- finn %>%
  mutate(
    Stream_ID = paste0("Finnish Environmental Institute__", Site.ID),
    Stream_ID2 = paste0("Finnish Environmental Institute__", Site)
  )

# -----------------------------------------------------------
# 3. Process Spatial Drivers Data ----
# -----------------------------------------------------------
# Read and preprocess spatial drivers data
si_drivers <- read.csv("all-data_si-extract_2_20250203.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-contains("soil"), -contains("cycle1")) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    )
  ) %>%
  # Create Stream_ID using LTER and Stream_Name
  mutate(Stream_ID = paste0(LTER, "__", Stream_Name)) %>%
  # Remove rows from MCM LTER (no spatial data)
  filter(!(LTER == "MCM")) %>%
  dplyr::select(-contains(".y"), -contains(".x"))

# A helper to standardize Stream_ID formatting
standardize_stream_id <- function(df) {
  df %>%
    mutate(Stream_ID = str_trim(Stream_ID),
           Stream_ID = str_replace_all(Stream_ID, "\\s+", " "))
}

si_drivers <- standardize_stream_id(si_drivers)

# Convert any greenup-related columns to day-of-year format
greenup_cols <- grep("greenup_", colnames(si_drivers), value = TRUE)
if(length(greenup_cols) > 0){
  si_drivers[, greenup_cols] <- lapply(si_drivers[, greenup_cols], function(x) {
    x <- as.Date(x, format = "%m/%d/%y")
    format(x, "%j")
  })
}

# Merge Finnish site information into spatial drivers
si_drivers <- si_drivers %>%
  left_join(finn %>% dplyr::select(Stream_ID, Stream_ID2), by = "Stream_ID") %>%
  mutate(Stream_ID = coalesce(Stream_ID2, Stream_ID)) %>%
  dplyr::select(-Stream_ID2) %>%
  dplyr::select(-contains(".y")) %>%
  rename_with(~ str_remove(., "\\.y$"))

# Process annual spatial driver data by separating out monthly columns
months <- c("_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_")
months_cols <- si_drivers[, (colnames(si_drivers) %like% months)]

# Identify columns (e.g., permafrost, prop_area) to replace NA with 0
cols_to_replace <- grep("permafrost|prop_area", colnames(si_drivers), value = TRUE)
si_drivers[, cols_to_replace] <- lapply(si_drivers[, cols_to_replace], function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- 0
  return(x)
})

# Extract annual data (exclude monthly columns)
year_cols <- si_drivers[, !(colnames(si_drivers) %in% colnames(months_cols))]
year_cols$Stream_Name <- si_drivers$Stream_Name
character_vars <- c("elevation|rock|land|soil|slope|permafrost")
year_cols <- year_cols[, !(colnames(year_cols) %like% character_vars)]

year_cols <- year_cols %>%
  dplyr::select(-LTER, -Stream_ID, -Shapefile_Name, -Discharge_File_Name)

# Melt the annual data into long format
year_cols_melt <- melt(year_cols, id.vars = "Stream_Name")
year_cols_melt$variable <- as.character(year_cols_melt$variable)
year_cols_melt$year <- str_extract(year_cols_melt$variable, "(?<=_)[^_]+(?=\\MMDD$)")
year_cols_melt$year <- ifelse(is.na(year_cols_melt$year),
                              sapply(strsplit(year_cols_melt$variable, "_"), function(x) x[2]),
                              year_cols_melt$year)

vars_annual <- c("num_days","prop_area","evapotrans","precip","temp","cycle0","cycle1","npp")
units_annual <- c("days", "prop_watershed","kg_m2","mm_day","deg_C","MMDD","MMDD","kgC_m2_year")
units_df_annual <- data.frame(vars_annual, units_annual, stringsAsFactors = FALSE)
colnames(units_df_annual)[1] <- "driver"

year_cols_melt$driver <- NA
for (i in 1:length(vars_annual)) {
  year_cols_melt$driver <- ifelse(year_cols_melt$variable %like% vars_annual[i],
                                  paste(vars_annual[i]),
                                  year_cols_melt$driver)
}
year_cols_melt <- year_cols_melt[, -2]
year_cols_melt <- merge(year_cols_melt, units_df_annual, by = "driver")

# Also extract non-numeric (character) data from spatial drivers
character_cols <- si_drivers[, (colnames(si_drivers) %like% character_vars)]
character_cols$Stream_Name <- si_drivers$Stream_Name

# Pivot annual spatial driver data to wide format
drivers <- year_cols_melt
# drivers_cropped <- subset(drivers, as.numeric(year) > 2000 & as.numeric(year) < 2024)

drivers_cast <- drivers %>%
  distinct(Stream_Name, year, driver, value, .keep_all = TRUE) %>%
  rename(Year = year) %>%
  mutate(Year = as.numeric(Year)) %>%
  dplyr::select(-c(units_annual)) %>%
  pivot_wider(names_from = driver, values_from = value)

all_spatial <- drivers_cast %>% 
  left_join(character_cols, by = "Stream_Name", relationship = "many-to-many") %>%
  distinct(Stream_Name, Year, .keep_all = TRUE) %>%
  dplyr::select(-contains(".x")) %>%
  rename_with(~ str_remove(., "\\.x$"))

# Here, since we don't have any wrtds_df data, let the final spatial drivers dataset be "tot"
tot <- all_spatial

# Merge Daylength data ----
daylen <- read.csv("Monthly_Daylength_2.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-1)

daylen_range <- daylen %>%
  group_by(Stream_Name) %>%
  summarise(
    Min_Daylength = min(mean_daylength),
    Max_Daylength = max(mean_daylength)
  ) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    )
  ) %>%
  dplyr::select(-Min_Daylength) %>%
  rename_with(~ str_remove(., "\\.y$"))

tot <- tot %>% 
  left_join(daylen_range, by = "Stream_Name", relationship = "many-to-many") %>%
  distinct(Stream_Name, Year, .keep_all = TRUE)

# -----------------------------------------------------------
# New: Incorporate and re-classify Land Cover Data ----
# -----------------------------------------------------------
# Read in the original land cover file (with percentage values)
lulc <- read.csv("DSi_LULC_filled_interpolated_Simple.csv", stringsAsFactors = FALSE) %>%
  dplyr::select("Stream_Name", "Year", "Simple_Class", "LandClass_sum") %>%
  mutate(LandClass_sum = if_else(is.na(LandClass_sum) | 
                                   LandClass_sum == 0, LandClass_sum, LandClass_sum * 100))  # convert to percentage

# Remove any existing columns that start with "land_" and the "major_land" column from tot
tot <- tot %>% dplyr::select(-starts_with("land_"), -major_land)

# Pivot the aggregated data so each unique Simple_Class becomes its own column
lulc_wide <- lulc %>%
  pivot_wider(
    names_from = Simple_Class,
    values_from = LandClass_sum,
    names_prefix = "land_"
  ) %>%
  dplyr::select(-"land_Filled_Value")

# Identify all the land cover columns (i.e., columns starting with "land_")
land_cols <- grep("^land_", names(lulc_wide), value = TRUE)

# Create a new column 'major_land' that finds the class with the highest percentage in each row
lulc_wide <- lulc_wide %>%
  mutate(major_land = apply(select(., all_of(land_cols)), 1, function(x) {
    if(all(is.na(x))) {
      NA_character_
    } else {
      # Identify the column name with the maximum percentage
      max_col <- names(x)[which.max(x)]
      # Remove the "land_" prefix to get the actual land class name
      sub("^land_", "", max_col)
    }
  }))

# Merge the reclassified land cover data (including major_land) into tot by Stream_Name and Year
tot <- tot %>% left_join(lulc_wide, by = c("Stream_Name", "Year"))

# -----------------------------------------------------------
# 4. Gap Filling for Elevation and Slope ----
# -----------------------------------------------------------
# Slope gap filling
Krycklan_slopes <- transform(read.csv("Krycklan_basin_slopes.csv"), 
                             basin_slope_mean_degree = atan(gradient_pct / 100) * (180 / pi))

US_slopes <- read.csv("DSi_Basin_Slope_missing_sites.csv", header = FALSE)
colnames(US_slopes) <- US_slopes[1, ]
US_slopes <- US_slopes[-1, ] %>%
  pivot_longer(cols = everything(),
               names_to = "Stream_Name",
               values_to = "basin_slope_mean_degree") %>%
  mutate(basin_slope_mean_degree = as.numeric(basin_slope_mean_degree))

stream_key <- read.csv("basin_stream_id_conversions.csv", header = TRUE)

Krycklan_slopes <- left_join(Krycklan_slopes, stream_key, by = "Stream_Name") %>%
  filter(!is.na(basin_slope_mean_degree))
US_slopes <- left_join(US_slopes, stream_key, by = "Stream_Name") %>%
  filter(!is.na(basin_slope_mean_degree))

tot_with_na_slope <- tot %>%
  filter(is.na(basin_slope_mean_degree)) %>%
  dplyr::select(Stream_Name)
tot_with_slope_filled <- tot_with_na_slope %>%
  left_join(US_slopes %>% dplyr::select(Stream_Name, basin_slope_mean_degree), by = "Stream_Name") %>%
  left_join(Krycklan_slopes %>% dplyr::select(Stream_Name, basin_slope_mean_degree), by = "Stream_Name", suffix = c("_US", "_Krycklan")) %>%
  mutate(basin_slope_mean_degree = coalesce(basin_slope_mean_degree_US, basin_slope_mean_degree_Krycklan)) %>%
  dplyr::select(Stream_Name, basin_slope_mean_degree) %>%
  rename_with(~ str_remove(., "\\.x$"))

tot_with_slope_filled <- tot_with_slope_filled %>%
  mutate(
    basin_slope_mean_degree = case_when(
      Stream_Name == "Walker Branch__east fork" ~ 2.2124321596241265,
      Stream_Name == "Walker Branch__west fork" ~ 1.8972192246291828,  
      TRUE ~ basin_slope_mean_degree
    )
  )

tot_with_slope_filled <- as.data.table(tot_with_slope_filled)
tot <- as.data.table(tot)
setkey(tot, Stream_Name)
setkey(tot_with_slope_filled, Stream_Name)
tot[tot_with_slope_filled, basin_slope_mean_degree := 
      ifelse(is.na(basin_slope_mean_degree), i.basin_slope_mean_degree, basin_slope_mean_degree),
    on = .(Stream_Name)]

# Elevation gap filling
US_elev <- read.csv("DSi_Basin_Elevation_missing_sites.csv", header = FALSE)
colnames(US_elev) <- US_elev[1, ]
US_elev <- US_elev[-1, ] %>%
  pivot_longer(cols = everything(),
               names_to = "Stream_Name",
               values_to = "elevation_mean_m")
US_elev$elevation_mean_m <- as.numeric(US_elev$elevation_mean_m)
US_elev <- left_join(US_elev, stream_key, by = "Stream_Name") %>%
  filter(!is.na(elevation_mean_m))
tot_with_na_elev <- tot %>%
  filter(is.na(elevation_mean_m)) %>%
  dplyr::select(Stream_Name)
tot_with_elev_filled <- tot_with_na_elev %>%
  left_join(US_elev %>% dplyr::select(Stream_Name, elevation_mean_m), by = "Stream_Name") %>%
  rename_with(~ str_remove(., "\\.x$"))

tot <- tot %>%
  left_join(tot_with_elev_filled, by = "Stream_Name", suffix = c("", "_filled"), relationship = "many-to-many") %>%
  mutate(elevation_mean_m = coalesce(elevation_mean_m_filled, elevation_mean_m)) %>%
  mutate(permafrost_mean_m = replace_na(as.numeric(permafrost_mean_m), 0),
         prop_area = replace_na(as.numeric(prop_area), 0)) %>%
  dplyr::select(-elevation_mean_m_filled)

tot <- as.data.table(tot)  # Ensure tot is still a data.table

# -----------------------------------------------------------
# 5. Merge with Chemistry Sites and Convert to Long Format ----
# -----------------------------------------------------------
# Merge tot (the processed spatial drivers) with chemistry_sites by Stream_Name
chemistry_combined <- chemistry_sites %>%
  left_join(as.data.frame(tot), by = c("Stream_Name"))

# Verify the number of unique streams
num_unique_streams <- chemistry_combined %>%
  pull(Stream_Name) %>%
  n_distinct()
print(num_unique_streams)

# Convert the merged dataset to long format
# (Assume identifier columns: Stream_Name and Year; adjust if needed)
cols_to_keep <- c("Stream_Name", "Year")
chemistry_long <- chemistry_combined %>%
  pivot_longer(
    cols = -all_of(cols_to_keep),
    names_to = "Variable",
    values_to = "Value",
    values_transform = list(Value = as.character)
  )

# -----------------------------------------------------------
# 6. Export Final Data ----
# -----------------------------------------------------------
# Convert list columns to character strings
chemistry_combined <- chemistry_combined %>%
  mutate_if(is.list, ~ sapply(., toString))

# Export Final Data
write.csv(chemistry_long, "Chemistry_Sites_Harmonized_Long.csv", row.names = FALSE)
write.csv(chemistry_combined, "Chemistry_Sites_Harmonized_Wide.csv", row.names = FALSE)

gc()
