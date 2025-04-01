# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr)

# Clear environment
rm(list = ls())

# Set working directory (change this path as needed)
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# -----------------------------------------------------------
# 1. Load the Chemistry Sites Data (key file with Stream_Name) ----
# -----------------------------------------------------------
chemistry_sites <- read.csv("chemistry_sites_si.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ))
  
ref_table <- read.csv("Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv", 
                      stringsAsFactors = FALSE) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    )
  ) %>%
  dplyr::select("Stream_Name", "LTER", "drainSqKm") %>%
  # Create Stream_ID using LTER and Stream_Name
  dplyr::mutate(Stream_ID = paste0(LTER, "__", Stream_Name))

chemistry_sites <- chemistry_sites %>%
  left_join(ref_table, by = "Stream_Name")

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
si_drivers <- read.csv("all-data_si-extract_2_20250325.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-contains("cycle1")) %>%
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
    x <- as.Date(x, format = "%Y-%m-%d")
    as.numeric(format(x, "%j"))
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

## ------------------------------------------------------- ##
# Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv") %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name) # Closing parenthesis added here
  )


tot <- tot %>%
  left_join(KG, by = "Stream_Name") %>%
  distinct(Stream_Name, Year, .keep_all = TRUE) %>%
  # Remove columns with .x
  dplyr::select(-contains(".x")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.y$"))

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

#write.csv(lulc_wide, "lulc_new_class.csv")

# Merge the reclassified land cover data (including major_land) into tot by Stream_Name and Year
tot <- tot %>% left_join(lulc_wide, by = c("Stream_Name", "Year"))

## ------------------------------------------------------- ##
#  Gap Filling Missing Data ----
## ------------------------------------------------------- ##
# Load and process Krycklan slopes
Krycklan_slopes <- transform(read.csv("Krycklan_basin_slopes.csv"), 
                             basin_slope_mean_degree = atan(gradient_pct / 100) * (180 / pi))

# Load and process US slopes
US_slopes <- read.csv("DSi_Basin_Slope_missing_sites.csv", header = FALSE)
colnames(US_slopes) <- US_slopes[1, ]
US_slopes <- US_slopes[-1, ]
US_slopes <- US_slopes %>%
  pivot_longer(
    cols = everything(),
    names_to = "Stream_Name",
    values_to = "basin_slope_mean_degree"
  ) %>%
  mutate(basin_slope_mean_degree = as.numeric(basin_slope_mean_degree))

# Upload the Stream_Name to Stream_ID key file
stream_key <- read.csv("basin_stream_id_conversions.csv", header = TRUE)

# Merge stream key with Krycklan_slopes and US_slopes to add Stream_ID
Krycklan_slopes <- left_join(Krycklan_slopes, stream_key, by = "Stream_Name") %>%
  filter(!is.na(basin_slope_mean_degree))  # Remove rows with NA values after merging with key

US_slopes <- left_join(US_slopes, stream_key, by = "Stream_Name") %>%
  filter(!is.na(basin_slope_mean_degree))  # Remove rows with NA values after merging with key

# Filter rows with NA slopes from 'tot'
tot_with_na_slope <- tot %>%
  filter(is.na(basin_slope_mean_degree)) %>%
  dplyr::select(Stream_ID)

# Merge 'tot_with_na_slope' with US and Krycklan slope data
tot_with_slope_filled <- tot_with_na_slope %>%
  left_join(US_slopes %>% dplyr::select(Stream_ID, basin_slope_mean_degree), by = "Stream_ID") %>%
  left_join(Krycklan_slopes %>% dplyr::select(Stream_ID, basin_slope_mean_degree), by = "Stream_ID", suffix = c("_US", "_Krycklan")) %>%
  mutate(
    basin_slope_mean_degree = coalesce(basin_slope_mean_degree_US, basin_slope_mean_degree_Krycklan)
  ) %>%
  dplyr::select(Stream_ID, basin_slope_mean_degree) %>%
  # Remove columns with .y
  dplyr::select(-contains(".y")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

# Manually update specific values
tot_with_slope_filled <- tot_with_slope_filled %>%
  mutate(
    basin_slope_mean_degree = case_when(
      Stream_ID == "Walker Branch__east fork" ~ 2.2124321596241265,
      Stream_ID == "Walker Branch__west fork" ~ 1.8972192246291828,  
      # Stream_ID == "ARC__Imnavait Weir" ~ 3.83,   ## Need to confirm this value with Arial S.    
      TRUE ~ basin_slope_mean_degree               # Retain existing values
    )
  ) 
# %>%
# # Remove specific Stream_IDs (no shapefiles or spatial data)
# dplyr::filter(!Stream_ID %in% c("MD__Barham", "MD__Jingellic", "USGS__Arkansas River at Murray Dam",
#                        "USGS__COLUMBIA RIVER AT PORT WESTWARD", "USGS__DMF Brazos River", 
#                        "USGS__YAMPA RIVER BELOW CRAIG"))

# Convert to data.table for efficient key-based operations
tot_with_slope_filled <- as.data.table(tot_with_slope_filled)
tot <- as.data.table(tot)

# Set keys for efficient join
setkey(tot, Stream_ID)
setkey(tot_with_slope_filled, Stream_ID)

tot[tot_with_slope_filled, basin_slope_mean_degree := 
      ifelse(is.na(basin_slope_mean_degree), i.basin_slope_mean_degree, basin_slope_mean_degree),
    on = .(Stream_ID)]

# Now do gap filling for elevation for the same sites:
# Load the US elevation data without headers
US_elev <- read.csv("DSi_Basin_Elevation_missing_sites.csv", header = FALSE)

# Set the first row as column names and remove it from the data
colnames(US_elev) <- US_elev[1, ]
US_elev <- US_elev[-1, ]

# Convert the dataframe from wide to long format
US_elev <- US_elev %>%
  pivot_longer(
    cols = everything(),
    names_to = "Stream_Name",
    values_to = "elevation_mean_m"
  )

# Convert elevation_mean_m to numeric
US_elev$elevation_mean_m <- as.numeric(US_elev$elevation_mean_m)

# Merge with the stream key and remove rows with NA in elevation_mean_m after the merge
US_elev <- left_join(US_elev, stream_key, by = "Stream_Name") %>%
  filter(!is.na(elevation_mean_m))

# Filter tot for rows with NA elevation values
tot_with_na_elev <- tot %>%
  filter(is.na(elevation_mean_m)) %>%
  dplyr::select(Stream_ID)

# Merge tot_with_na_elev with US_elev to fill missing elevation values
tot_with_elev_filled <- tot_with_na_elev %>%
  left_join(US_elev %>% dplyr::select(Stream_ID, elevation_mean_m), by = "Stream_ID") %>%
  # Remove columns with .y
  dplyr::select(-contains(".y")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

# Update tot with the filled elevation values
tot <- tot %>%
  left_join(tot_with_elev_filled, by = "Stream_ID", suffix = c("", "_filled"), relationship = "many-to-many") %>%
  mutate(
    elevation_mean_m = coalesce(elevation_mean_m_filled, elevation_mean_m)
  ) %>%
  mutate(
    permafrost_mean_m = replace_na(as.numeric(permafrost_mean_m), 0),
    prop_area = replace_na(as.numeric(prop_area), 0)
  ) %>%
  dplyr::select(-elevation_mean_m_filled) %>%
  # # Remove specific Stream_IDs (no shapefiles or spatial data)
  # filter(!Stream_ID %in% c("MD__Barham", "MD__Jingellic", "USGS__Arkansas River at Murray Dam",
  #                          "USGS__COLUMBIA RIVER AT PORT WESTWARD", "USGS__DMF Brazos River", 
  #                          "USGS__YAMPA RIVER BELOW CRAIG")) %>%
  # Remove columns with .y
  dplyr::select(-contains(".y")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)
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

# Convert relevant columns to numeric 
chemistry_combined <- chemistry_combined %>%
  mutate(across(c(drainSqKm, cycle0, evapotrans, npp, num_days, precip, 
                  prop_area, temp, elevation_median_m, elevation_mean_m, 
                  elevation_min_m, elevation_max_m, basin_slope_median_degree, 
                  basin_slope_mean_degree, basin_slope_min_degree, basin_slope_max_degree, 
                  permafrost_median_m, permafrost_mean_m, permafrost_min_m, 
                  permafrost_max_m, Max_Daylength, land_Bare, land_Cropland, 
                  land_Forest, land_Grassland_Shrubland, land_Ice_Snow, 
                  land_Impervious, land_Salt_Water, land_Tidal_Wetland, 
                  land_Water, land_Wetland_Marsh), as.numeric))

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

# Now do averaging: 
tot_average <- chemistry_combined %>%
  dplyr::group_by(Stream_Name) %>%
  summarise(
    # Numerical variables: calculate the mean across all years
    drainSqKm = mean(drainSqKm, na.rm = TRUE),
    cycle0 = mean(cycle0, na.rm = TRUE),
    evapotrans = mean(evapotrans, na.rm = TRUE),
    npp = mean(npp, na.rm = TRUE),
    num_days = mean(num_days, na.rm = TRUE),
    precip = mean(precip, na.rm = TRUE),
    prop_area = mean(prop_area[Year > 2001], na.rm = TRUE),    
    temp = mean(temp, na.rm = TRUE),
    elevation_median_m = mean(elevation_median_m, na.rm = TRUE),
    elevation_mean_m = mean(elevation_mean_m, na.rm = TRUE),
    elevation_min_m = mean(elevation_min_m, na.rm = TRUE),
    elevation_max_m = mean(elevation_max_m, na.rm = TRUE),
    # basin_slope_median_degree = mean(basin_slope_median_degree, na.rm = TRUE),
    basin_slope_mean_degree = mean(basin_slope_mean_degree, na.rm = TRUE),
    # basin_slope_min_degree = mean(basin_slope_min_degree, na.rm = TRUE),
    # basin_slope_max_degree = mean(basin_slope_max_degree, na.rm = TRUE),
    permafrost_median_m = mean(permafrost_median_m, na.rm = TRUE),
    permafrost_mean_m = mean(permafrost_mean_m, na.rm = TRUE),
    permafrost_min_m = mean(permafrost_min_m, na.rm = TRUE),
    permafrost_max_m = mean(permafrost_max_m, na.rm = TRUE),
    Max_Daylength = mean(Max_Daylength, na.rm = TRUE),
    land_Bare = mean(land_Bare, na.rm = TRUE),
    land_Cropland = mean(land_Cropland, na.rm = TRUE),
    land_Forest = mean(land_Forest, na.rm = TRUE),
    land_Grassland_Shrubland = mean(land_Grassland_Shrubland, na.rm = TRUE),
    land_Ice_Snow = mean(land_Ice_Snow, na.rm = TRUE),
    land_Impervious = mean(land_Impervious, na.rm = TRUE),
    land_Salt_Water = mean(land_Salt_Water, na.rm = TRUE),
    land_Tidal_Wetland = mean(land_Tidal_Wetland, na.rm = TRUE),
    land_Water = mean(land_Water, na.rm = TRUE),
    land_Wetland_Marsh = mean(land_Wetland_Marsh, na.rm = TRUE),
  
    # Categorical variables: grab the first value
    across(contains("rocks"), ~ first(.)),
    across(contains("soil_"), ~ first(.))
  ) %>%
  ungroup() %>%
  # Replace NaN with NA in all columns
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

# Print a preview
print(head(tot_average))


# Count the number of unique Stream_IDs
num_unique_stream_ids <- tot_average %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids)

# Export average data with dynamic filename
write.csv(tot_average, "Chemistry_Sites_Harmonized_Wide_Average.csv", row.names = FALSE)

gc()


# Summary stats by LTER
npp_et_summary_LTER <- chemistry_combined %>%
  group_by(LTER.x) %>%
  summarise(
    npp_median = median(npp, na.rm = TRUE),
    npp_sd   = sd(npp, na.rm = TRUE),
    et_median  = median(evapotrans, na.rm = TRUE),
    et_sd    = sd(evapotrans, na.rm = TRUE)
  )
print(npp_et_summary_LTER)

# Summary stats by Stream_Name
npp_et_summary_Name <- chemistry_combined %>%
  group_by(Name) %>%
  summarise(
    npp_median = median(npp, na.rm = TRUE),
    npp_sd   = sd(npp, na.rm = TRUE),
    et_median  = median(evapotrans, na.rm = TRUE),
    et_sd    = sd(evapotrans, na.rm = TRUE)
  )
print(npp_et_summary_Name)

# Export summaries as CSV files
write.csv(npp_et_summary_LTER, "NPP_ET_Summary_by_LTER.csv", row.names = FALSE)
write.csv(npp_et_summary_Name, "NPP_ET_Summary_by_Climate.csv", row.names = FALSE)

