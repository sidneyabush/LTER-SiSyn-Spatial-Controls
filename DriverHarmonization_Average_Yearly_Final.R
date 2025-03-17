# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr)

# Clear environment
rm(list = ls())

# Define the record length in years (change this to 1, 5, 10, 20... as needed)
record_length <- 5  

# ## ------------------------------------------------------- ##
#              # Read in and Tidy Data ----
# ## ------------------------------------------------------- ##

## Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Read and clean WRTDS data this is filtered data
wrtds_df <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  rename(LTER = LTER.x) %>%
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  # filter(FNConc >= 0.5 * GenConc & FNConc <= 1.5 * GenConc) %>%
  dplyr::select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"),
                 -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Year = floor(as.numeric(DecYear))
  ) %>%
  filter(chemical == "DSi")

# Standardize Stream_ID formatting in all datasets to remove extra spaces
standardize_stream_id <- function(df) {
  df %>%
    mutate(Stream_ID = str_trim(Stream_ID),  # Remove leading/trailing spaces
           Stream_ID = str_replace_all(Stream_ID, "\\s+", " "))  # Convert multiple spaces to a single space
}

# Count number of years per site and filter for sites with at least 1 years of data
site_year_counts <- wrtds_df %>%
  dplyr::group_by(Stream_ID) %>%
  summarise(year_count = n_distinct(Year)) %>%
  filter(year_count >= record_length)

# Filter the main dataset to only include sites with sufficient data
wrtds_df <- wrtds_df %>%
  filter(Stream_ID %in% site_year_counts$Stream_ID)

## Need to tidy the Finnish site names:
finn <- read.csv("FinnishSites.csv")

finn$Stream_ID <- paste0("Finnish Environmental Institute__", finn$Site.ID)
finn$Stream_ID2 <- paste0("Finnish Environmental Institute__", finn$Site)

# Use left_join instead of for-loop to ensure all Finnish sites are replaced
wrtds_df <- wrtds_df %>%
  left_join(finn %>% dplyr::select(Stream_ID, Stream_ID2), by = "Stream_ID") %>%
  mutate(Stream_ID = coalesce(Stream_ID2, Stream_ID)) %>%
  dplyr::select(-Stream_ID2)

## ------------------------------------------------------- ##
# Calculate Yields ----
## ------------------------------------------------------- ##
yields <- wrtds_df %>%
  mutate(FNYield = (FNFlux * 365) / drainSqKm,
         GenYield = (GenFlux * 365) / drainSqKm) %>%
  dplyr::select(-FNFlux, -GenFlux)


tot <- wrtds_df %>%
  left_join(yields, by = c("Stream_ID", "Year")) %>%
  distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  # Remove columns with .y
  dplyr::select(-contains(".y")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

# -----------------------------------------------------------
# Import Flashiness Data and Merge with tot ----
# -----------------------------------------------------------
# Import the flashiness CSV that contains Stream_ID and RBFI
flashiness <- read_csv("flashiness_by_stream_id.csv")

# Merge the flashiness data into the tot dataset by Stream_ID
tot <- tot %>% 
  left_join(flashiness, by = "Stream_ID")

num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

# Identify unique Stream_IDs with NA values
unique_stream_ids_with_na <- tot %>%
  filter(if_any(everything(), is.na)) %>%
  distinct(Stream_ID) %>%
  pull(Stream_ID)

# Count the number of unique Stream_IDs with NA
num_unique_stream_ids_with_na <- length(unique_stream_ids_with_na)

# Print results
print(num_unique_stream_ids_with_na)
print(unique_stream_ids_with_na)

gc()

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
  left_join(KG, by = "Stream_ID") %>%
  distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  # Remove columns with .x
  dplyr::select(-contains(".x")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.y$"))

## ------------------------------------------------------- ##
              # Import Daylength ----
## ------------------------------------------------------- ##
# Load and clean daylength data
daylen <- read.csv("Monthly_Daylength_2.csv") %>%
  dplyr::select(-1)

# Calculate min and max daylength, update Stream_Name, and ensure all sites in "tot" are left-joined
daylen_range <- daylen %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(
    Min_Daylength = min(mean_daylength),
    Max_Daylength = max(mean_daylength)
  ) %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    )) %>%
  dplyr::select(-Min_Daylength) %>%
  # Remove columns with .x
  # select(-contains(".x")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.y$"))

# Ensure the result is left-joined to "tot"
tot <- tot %>% 
  left_join(daylen_range, by = "Stream_Name", relationship = "many-to-many") %>%
  distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  # Remove columns with .x
  dplyr::select(-contains(".x")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.y$"))

## ------------------------------------------------------- ##
              # Spatial Drivers----
## ------------------------------------------------------- ##
# Read and preprocess spatial drivers
si_drivers <- read.csv("all-data_si-extract_2_20250203.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-contains("soil"), -contains("cycle1")) %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    )) %>%
  # Create Stream_ID first using LTER and Stream_Name
  mutate(Stream_ID = paste0(LTER, "__", Stream_Name)) %>%
  # Remove MCM LTER (no spatial data)
  filter(!(LTER == "MCM")) %>%
  # Remove columns with .x
  dplyr::select(-contains(".y"), -contains(".x")) 

si_drivers <- standardize_stream_id(si_drivers)

# Identify and convert all greenup-related columns to Date format
greenup_cols <- grep("greenup_", colnames(si_drivers), value = TRUE)

si_drivers[, greenup_cols] <- lapply(si_drivers[, greenup_cols], function(x) {
  as.Date(x, format = "%m/%d/%y")
})

si_drivers[, greenup_cols] <- lapply(si_drivers[, greenup_cols], function(x) format(x, "%j"))

si_drivers <- si_drivers %>%
  left_join(finn %>% dplyr::select(Stream_ID, Stream_ID2), by = "Stream_ID") %>%
  mutate(Stream_ID = coalesce(Stream_ID2, Stream_ID)) %>%
  dplyr::select(-Stream_ID2)  %>%
  # Remove columns with .x
  dplyr::select(-contains(".y")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.y$"))

## Before, using full abbrevs removed some of the spatial driver columns (e.g., "dec" in "deciduous" was causing
#  deciduous land cover to be filtered out)
months <- c("_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_")
months_cols <- si_drivers[,(colnames(si_drivers) %like% months)]

# Identify columns related to permafrost and prop_area
cols_to_replace <- grep("permafrost|prop_area", colnames(si_drivers), value = TRUE)

# Replace NA values with 0 in the identified columns
si_drivers[, cols_to_replace] <- lapply(si_drivers[, cols_to_replace], function(x) {
  x <- as.numeric(x)  # Convert to numeric to avoid issues
  x[is.na(x)] <- 0
  return(x)
})

# Parse out and clean annual data
year_cols <- si_drivers[,!(colnames(si_drivers) %in% colnames(months_cols))]
year_cols$Stream_Name <- si_drivers$Stream_Name
character_vars <- c("elevation|rock|land|soil|slope|permafrost")
year_cols <- year_cols[,!(colnames(year_cols) %like% character_vars)]

year_cols <- year_cols %>%
  dplyr::select(-LTER, -Stream_ID, -Shapefile_Name, -Discharge_File_Name)

year_cols_melt <- melt(year_cols, id.vars = "Stream_Name")

year_cols_melt$variable <- as.character(year_cols_melt$variable)
year_cols_melt$year <- str_extract(year_cols_melt$variable, "(?<=_)[^_]+(?=\\MMDD$)")
year_cols_melt$year <- ifelse(is.na(year_cols_melt$year), sapply(strsplit(year_cols_melt$variable, "_"), function(x) x[2]),
                            year_cols_melt$year)

vars_annual <- c("num_days","prop_area","evapotrans","precip","temp","cycle0","cycle1","npp")
units_annual <- c("days", "prop_watershed","kg_m2","mm_day","deg_C","MMDD","MMDD","kgC_m2_year")
units_df_annual <- data.frame(vars_annual,units_annual)

colnames(units_df_annual)[1] <- "driver"

year_cols_melt$driver <- NA

for (i in 1:length(vars_annual)) {

  year_cols_melt$driver <- ifelse(year_cols_melt$variable %like% vars_annual[i],paste(vars_annual[i]),
                                year_cols_melt$driver)

}

year_cols_melt <- year_cols_melt[,-2]

year_cols_melt <- merge(year_cols_melt, units_df_annual, by = "driver")

# Parse out character data
character_cols <- si_drivers[,(colnames(si_drivers) %like% character_vars)]
character_cols$Stream_Name <- si_drivers$Stream_Name

## ------------------------------------------------------- ##
# Calculate Greenup Day ----
## ------------------------------------------------------- ##
# Melt data for processing
drivers <- year_cols_melt
# drivers <- subset(drivers, !drivers$driver %in% c("cycle1", "num_days"))
drivers_cropped <- subset(drivers, drivers$year > 2000 & drivers$year < 2024)

drivers_cast <- drivers_cropped %>%
  # Remove duplicates based on relevant columns
  distinct(Stream_Name, year, driver, value, .keep_all = TRUE) %>%
  # Rename 'year' to 'Year'
  rename(Year = year) %>%
  # Convert 'Year' to numeric
  mutate(Year = as.numeric(Year)) %>%
  # Remove unwanted columns
  dplyr::select(!c(units_annual)) %>%
  # Pivot the data
  tidyr::pivot_wider(names_from = driver, values_from = value)

# Combine with character columns: 
all_spatial <- drivers_cast %>% 
  left_join(character_cols, by = "Stream_Name", relationship = "many-to-many") %>%
  distinct(Stream_Name, Year, .keep_all = TRUE) %>%
  # Remove columns with .x
  dplyr::select(-contains(".x")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

# Merge with final dataset and clean up
tot <- tot %>%
  left_join(all_spatial, by = c("Stream_Name", "Year")) %>%
  filter(Year > 2000 & Year <= 2024) %>%  # Filter for years within range
  distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  mutate(
    permafrost_mean_m = as.numeric(permafrost_mean_m),
    cycle0 = as.numeric(cycle0),
    evapotrans = as.numeric(evapotrans),
    npp = as.numeric(npp),
    precip = as.numeric(precip),
    prop_area = as.numeric(prop_area),
    temp = as.numeric(temp)
  ) %>%
  mutate(
    permafrost_mean_m = replace_na(permafrost_mean_m, 0),
    prop_area = replace_na(prop_area, 0)
  ) %>%
  # Remove columns with .y
  dplyr::select(-contains(".y")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

# Identify missing values in numeric columns only
missing_spatial_data_summary <- tot %>%
  filter(Year > 2000 & Year <= 2024) %>%  # Filter for years within range
  dplyr::select(Stream_ID, Year, permafrost_mean_m, cycle0, evapotrans, npp, precip, prop_area, temp) %>%  # Keep only numeric columns + Stream_ID & Year
  pivot_longer(cols = -c(Stream_ID, Year), names_to = "Variable", values_to = "Value") %>%  # Reshape
  filter(is.na(Value)) %>%  # Keep only missing values
  dplyr::select(-Value)  # Remove the actual value column

# Count unique sites missing data in 2001
num_sites_missing_2001 <- missing_spatial_data_summary %>%
  filter(Year == 2001) %>%
  distinct(Stream_ID) %>%
  nrow()

# Print summary
print(num_sites_missing_2001)  # Number of unique sites missing data in 2001
print(nrow(missing_spatial_data_summary))  # Number of missing entries
print(head(missing_spatial_data_summary))  # Preview first few rows

# Export missing data summary with dynamic filename
write.csv(missing_spatial_data_summary, 
          sprintf("missing_spatial_data_summary_2001_2024_filtered_%d_years.csv", record_length), 
          row.names = FALSE)

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

# Create list of NA values for data up to this point
# Exclude the land use and rock types, and remove "median" columns

# Identify missing values in numeric columns only
missing_elev_slope_data_summary <- tot %>%
  filter(Year > 2000 & Year <= 2024) %>%  # Filter for years within range
  dplyr::select(Stream_ID, Year, basin_slope_mean_degree, elevation_mean_m) %>%  # Keep only numeric columns + Stream_ID & Year
  pivot_longer(cols = -c(Stream_ID, Year), names_to = "Variable", values_to = "Value") %>%  # Reshape
  filter(is.na(Value)) %>%  # Keep only missing values
  dplyr::select(-Value)  # Remove the actual value column

# Count unique sites missing data in 2001
num_sites_missing_2001 <- missing_elev_slope_data_summary %>%
  filter(Year == 2001) %>%
  distinct(Stream_ID) %>%
  nrow()

# Print summary
print(num_sites_missing_2001)  # Number of unique sites missing data in 2001
print(nrow(missing_elev_slope_data_summary))  # Number of missing entries
print(head(missing_elev_slope_data_summary))  # Preview first few rows


# Export missing data summary with dynamic filename
write.csv(missing_elev_slope_data_summary, 
          sprintf("missing_elev_slope_data_summary_2001_2024_filtered_%d_years.csv", record_length), 
          row.names = FALSE)


# ## ------------------------------------------------------- ##
#           # Import WRTDS N_P Data ---- 
# ## ------------------------------------------------------- ##
# Filter for relevant chemicals (N and P) and simplify NO3/NOx to NOx
wrtds_NP <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  rename(LTER = LTER.x) %>%
  dplyr::select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"),
                -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Year = floor(as.numeric(DecYear)) # Convert DecYear to Year
  ) %>%
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  # filter(GenConc <= 60) %>%  # Remove rows where GenConc > 60
  # filter(FNConc >= 0.5 * GenConc & FNConc <= 1.5 * GenConc)  %>%  # Remove rows where FNConc is ±50% of GenConc
  dplyr::select(-DecYear, -.groups, -LTER, -contains("FN"), -GenFlux)

wrtds_NP <- wrtds_NP %>%
  filter(chemical %in% c("P", "NO3", "NOx") & GenConc > 0) %>%  # Keep only positive GenConc
  mutate(
    chemical = ifelse(chemical %in% c("NOx", "NO3"), "NOx", chemical)  # Simplify to NOx
  )

# Handle duplicates by taking the median
wrtds_NP <- wrtds_NP %>%
  dplyr::group_by(Stream_ID, Year, chemical) %>%  # Group by unique combinations
  summarise(
    GenConc = median(GenConc, na.rm = TRUE),  # Take the median if duplicates exist
    .groups = "drop"  # Ungroup after summarizing
  )

# Reshape data to wide format
wrtds_NP_wide <- wrtds_NP %>%
  pivot_wider(
    id_cols = c(Stream_ID, Year),  # Group by Stream_ID and Year
    names_from = chemical,         # Create separate columns for NOx and P
    values_from = GenConc,         # Populate these columns with GenConc values
    values_fill = list(GenConc = NA)  # Fill missing combinations with NA
  )

# ## ------------------------------------------------------- ##
#           # Import RAW N_P Data ---- 
# ## ------------------------------------------------------- ##
raw_NP <- read.csv("converted_raw_NP.csv")

# Step 1: Filter, create Stream_ID, simplify solutes, and calculate median
raw_NP_median <- raw_NP %>%
  mutate(
    Year = as.integer(year(as.Date(date, format = "%Y-%m-%d"))),  # Extract Year from date
    Stream_ID = paste(LTER, Stream_Name, sep = "__"),              # Create Stream_ID by combining LTER and Stream_Name
    solute_simplified = case_when(  # Simplify solutes into NOx and P categories
      variable %in% c("NOx", "NO3") ~ "NOx",
      variable %in% c("SRP", "PO4") ~ "P",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(solute_simplified)) %>%  # Keep only relevant solutes (NOx, P)
  dplyr::group_by(Stream_ID, Year, solute_simplified) %>%  # Group by Stream_ID, year, and solute
  summarise(
    median_value = median(value, na.rm = TRUE),  # Calculate median value
    .groups = "drop"  # Ungroup after summarizing
  )

# Step 2: Reshape the data to wide format
raw_NP_wide <- raw_NP_median %>%
  pivot_wider(
    names_from = solute_simplified,  # Create columns for NOx and P
    values_from = median_value,      # Populate these columns with the median values
    values_fill = list(median_value = NA)  # Fill missing combinations with NA
  )

# ## ------------------------------------------------------- ##
#           # Combine WRTDS and RAW Data ---- 
# ## ------------------------------------------------------- ##
# Perform a full outer join to combine `wrtds_NP_wide` and `raw_NP_wide`
combined_NP <- full_join(wrtds_NP_wide, raw_NP_wide, by = c("Stream_ID", "Year"))

# Fill gaps in WRTDS data with raw data where necessary
combined_NP <- combined_NP %>%
  # Rename columns so that we have source identifiers
  rename(
    P_wrtds = P.x,      # P from WRTDS
    NOx_wrtds = NOx.x,  # NOx from WRTDS
    P_raw = P.y,        # P from raw data
    NOx_raw = NOx.y     # NOx from raw data
  ) %>%
  # Create source indicator columns
  mutate(
    P_source = if_else(is.na(P_wrtds), "raw", "WRTDS"),
    NOx_source = if_else(is.na(NOx_wrtds), "raw", "WRTDS")
  ) %>%
  # Fill gaps: if WRTDS is missing, use raw data
  mutate(
    P = if_else(is.na(P_wrtds), P_raw, P_wrtds),
    NOx = if_else(is.na(NOx_wrtds), NOx_raw, NOx_wrtds)
  ) %>%
  # Drop the intermediate columns
  dplyr::select(-P_wrtds, -NOx_wrtds, -P_raw, -NOx_raw)

streams_gapfill <- combined_NP %>%
  filter(P_source == "raw" | NOx_source == "raw") %>%
  distinct(Stream_ID) %>%
  pull(Stream_ID)

print(streams_gapfill)

# ## ------------------------------------------------------- ##
#           # Merge Combined Data with `tot` ----
# ## ------------------------------------------------------- ##
# Merge the combined dataset with `tot`
tot <- tot %>%
  left_join(combined_NP, by = c("Stream_ID", "Year")) %>%
  mutate(
    permafrost_mean_m = replace_na(as.numeric(permafrost_mean_m), 0),
    prop_area = replace_na(as.numeric(prop_area), 0)) 
  # %>%
  # # Remove specific Stream_IDs (no shapefiles or spatial data)
  # filter(!Stream_ID %in% c("MD__Barham", "MD__Jingellic", "USGS__Arkansas River at Murray Dam",
  #                          "USGS__COLUMBIA RIVER AT PORT WESTWARD", "USGS__DMF Brazos River", 
  #                          "USGS__YAMPA RIVER BELOW CRAIG"))

# Verify the number of unique Stream_IDs
num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

# Standardize NOx and P column names by removing suffixes
tot <- tot %>%
  rename_with(~ str_replace_all(., "\\.x$|\\.y$|\\.x.x$|\\.y.y$", ""))

# Now generate a list of missing NOx and P sites-year combinations:
missing_N_P_data_summary <- tot %>%
  filter(Year > 2000 & Year <= 2024) %>%  # Filter for years within range
  dplyr::select(Stream_ID, Year, NOx, P) %>%
  distinct(Stream_ID, Year, across(starts_with("NOx")), across(starts_with("P"))) %>%  # Ensure no duplicates
  pivot_longer(cols = starts_with("NOx") | starts_with("P"), names_to = "Variable", values_to = "Value") %>%  # Reshape
  filter(is.na(Value)) %>%  # Keep only missing values
  dplyr::select(-Value)  # Remove the actual value column

# Export missing data summary with dynamic filename
write.csv(missing_N_P_data_summary, 
          sprintf("missing_N_P_data_summary_2001_2024_filtered_%d_years.csv", record_length), 
          row.names = FALSE)

# Get the number of unique Stream_IDs with missing NOx or P
num_unique_missing_streams <- missing_N_P_data_summary %>%
  distinct(Stream_ID) %>%
  nrow()

# Print summary
print((num_unique_missing_streams))  # Number of missing entries
print(head(missing_N_P_data_summary))  # Preview first few rows

# Combine the missing data summaries using full_join so that every site/year combination is retained
missing_all_summary <- missing_N_P_data_summary %>%
  full_join(missing_elev_slope_data_summary, by = c("Stream_ID", "Year"), relationship = "many-to-many") %>%
  full_join(missing_spatial_data_summary, by = c("Stream_ID", "Year"), relationship = "many-to-many")

# Check the combined summary
print(head(missing_all_summary))

# Write out to a CSV file
write.csv(missing_all_summary, "missing_all_data_summary.csv", row.names = FALSE)

# Verify the number of unique Stream_IDs
num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

## ------------------------------------------------------- ##
              #  Silicate Weathering ----
## ------------------------------------------------------- ##
# Read and prepare data
mapped_lithologies <- fread("mapped_lithologies.csv")

# Convert `tot` and `mapped_lithologies` to data.tables
setDT(tot)
setDT(mapped_lithologies)

# Ensure compatibility in the major_rock column for merging
tot[, major_rock := as.character(major_rock)]
mapped_lithologies[, major_rock := as.character(major_rock)]

# Perform the merge and filter out NA values in mapped_lithology
weathering <- merge(tot[, .(Stream_ID, Year, major_rock, Q, temp, drainSqKm)],
                    mapped_lithologies[, .(major_rock, mapped_lithology)],
                    by = "major_rock", all.x = TRUE)
weathering <- weathering[!is.na(mapped_lithology)]  # Remove rows with NA in mapped_lithology

# Constants for weathering calculations
seconds_per_year <- 31536000
kg_per_m3 <- 1000
km2_to_m2 <- 10^6
R <- 8.314

# Define lithology parameters as a data.table
lithology_params <- data.table(
  mapped_lithology = c("su", "vb", "pb", "py", "va", "vi", "ss", "pi", "sm", "mt", "pa"),
  b = c(0.003364, 0.007015, 0.007015, 0.0061, 0.002455, 0.007015, 0.005341, 0.007015, 0.012481, 0.007626, 0.005095),
  sp = c(1, 1, 1, 1, 1, 1, 0.64, 0.58, 0.24, 0.25, 0.58),
  sa = c(60, 50, 50, 46, 60, 50, 60, 60, 60, 60, 60)
)

# Convert temperature to Kelvin for calculations
weathering[, temp := as.numeric(temp)]
weathering[, temp_K := temp + 273.15]

# Calculate runoff based on the given formula
weathering[, runoff := (Q * seconds_per_year * kg_per_m3) / (drainSqKm * km2_to_m2)]

# Define a function for vectorized calculation of weathering
calculate_weathering_vectorized <- function(lithologies, runoff, temp_k) {
  lithologies_split <- strsplit(lithologies, ",\\s*")
  weathering_results <- sapply(seq_along(lithologies_split), function(i) {
    liths <- lithologies_split[[i]]
    weathering_values <- sapply(liths, function(lith) {
      params <- lithology_params[mapped_lithology == lith]
      if (nrow(params) == 0) stop(paste("Lithology not found in the table for", lith))
      params$b * (params$sp * exp(((1000 * params$sa) / R) * ((1 / 284.2) - (1 / temp_k[i])))) * runoff[i]
    })
    mean(weathering_values, na.rm = TRUE)
  })
  return(weathering_results)
}

# Calculate silicate weathering for each row in the weathering data
weathering[, silicate_weathering := calculate_weathering_vectorized(mapped_lithology, runoff, temp_K), by = .(Stream_ID, Year)]

setDT(weathering)

# Ensure 'Stream_ID' and 'Year' are compatible types
tot[, Stream_ID := as.character(Stream_ID)]
tot[, Year := as.integer(Year)]
weathering[, Stream_ID := as.character(Stream_ID)]
weathering[, Year := as.integer(Year)]

# Calculate silicate weathering for each row in the weathering data
weathering[, silicate_weathering := calculate_weathering_vectorized(mapped_lithology, runoff, temp_K)]

# Ensure uniqueness in the datasets
weathering <- unique(weathering, by = c("Stream_ID", "Year"))
tot <- unique(tot, by = c("Stream_ID", "Year"))

# Perform the merge correctly
tryCatch({
  tot <- merge(
    tot,
    weathering[, .(Stream_ID, Year, silicate_weathering)],
    by = c("Stream_ID", "Year"),
    all.x = TRUE,
    allow.cartesian = FALSE
  )
}, error = function(e) {
  message("Error encountered during merge: ", e$message)
})

# Clean up memory
gc()

# Verify the number of unique Stream_IDs
num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

# -----------------------------------------------------------
# New: Incorporate Land Cover Data ----
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

tot <- tot %>%
  mutate(across(where(is.list), ~ sapply(., function(x) paste(x, collapse = ","))))

# Verify the number of unique Stream_IDs
num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

# Export at this step to keep lat/long: 
write.csv(as.data.frame(tot), 
          sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length),
          row.names = FALSE)

# Tidy data for export: 
tot_si <- tot %>%
  dplyr::select(Stream_ID, Year, drainSqKm, NOx, P, precip, Q,
                temp, Max_Daylength, prop_area, npp, evapotrans,
                cycle0, permafrost_mean_m, elevation_mean_m, 
                basin_slope_mean_degree, FNConc, FNYield, GenConc, GenYield, major_rock, major_land,
                contains("rocks"), contains("land_")) %>%
  dplyr::rename(snow_cover = prop_area, 
                greenup_day = cycle0,
                drainage_area = drainSqKm,
                elevation = elevation_mean_m,
                permafrost = permafrost_mean_m,
                basin_slope = basin_slope_mean_degree) %>%
  dplyr::mutate(permafrost = ifelse(is.na(permafrost), 0, permafrost)) %>%
  dplyr::mutate(snow_cover = ifelse(is.na(snow_cover), 0, snow_cover))

# Verify the number of unique Stream_IDs
num_unique_stream_ids <- tot_si %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

# Convert numeric columns to numeric
tot_annual <- tot_si %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)  %>% 
  mutate(across(c(drainage_area, NOx, P, precip, Q, temp, Max_Daylength, 
                  snow_cover, npp, evapotrans, 
                  greenup_day, permafrost, elevation, basin_slope, 
                  FNConc, FNYield, GenConc, GenYield), 
                as.numeric))

# Count the number of unique Stream_IDs
num_unique_stream_ids <- tot_annual %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

drivers_df <- tot_annual %>%
  # Convert blank strings to NA in all character columns
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  dplyr::select(FNConc, everything()) %>%
  # Replace NAs in selected numeric columns with 0 (if desired)
  mutate_at(vars(21:36), ~ replace(., is.na(.), 0)) %>%
  filter(FNConc >= 0.5 * GenConc & FNConc <= 1.5 * GenConc) %>%
  filter(complete.cases(.))

# Count the number of unique Stream_IDs
num_unique_stream_ids <- drivers_df %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

# ---- Remove Outliers for FNConc (5 SD Rule) ----
FNConc_mean <- mean(drivers_df$FNConc, na.rm = TRUE)
SD_val <- 5
FNConc_sd <- sd(drivers_df$FNConc, na.rm = TRUE)
FNConc_upper <- FNConc_mean + SD_val * FNConc_sd
FNConc_lower <- FNConc_mean - SD_val * FNConc_sd

drivers_df <- drivers_df %>%
  filter(FNConc >= FNConc_lower & FNConc <= FNConc_upper)

# Count occurrences of each stream_id and filter those with less than 5 entries
filtered_streams_FNConc <- drivers_df %>%
  group_by(Stream_ID) %>%
  filter(n() < 5) %>%
  ungroup()

# Display unique stream_IDs with less than 5 entries
unique(filtered_streams_FNConc$Stream_ID)

# ---- Remove Outliers for FNYield (5 SD Rule) ----
FNYield_mean <- mean(drivers_df$FNYield, na.rm = TRUE)
FNYield_sd <- sd(drivers_df$FNYield, na.rm = TRUE)
FNYield_upper <- FNYield_mean + SD_val * FNYield_sd
FNYield_lower <- FNYield_mean - SD_val * FNYield_sd

# Count occurrences of each stream_id and filter those with less than 5 entries
filtered_streams_FNYield <- drivers_df %>%
  group_by(Stream_ID) %>%
  filter(n() < 5) %>%
  ungroup()

# Display unique stream_IDs with less than 5 entries
unique(filtered_streams_FNYield$Stream_ID)

drivers_df <- drivers_df %>%
  filter(FNYield >= FNYield_lower & FNYield <= FNYield_upper)

# Count the number of unique Stream_IDs before removing it
unique_stream_id_count <- drivers_df %>%
  summarise(unique_count = n_distinct(Stream_ID)) %>%
  pull(unique_count)

print(unique_stream_id_count)

# Remove sites (Stream_IDs) that have fewer than 5 unique years of data
drivers_df <- drivers_df %>%
  group_by(Stream_ID) %>%
  filter(n_distinct(Year) >= 5) %>%
  ungroup() 

# Count the number of unique Stream_IDs after filtering
unique_stream_id_count <- drivers_df %>%
  summarise(unique_count = n_distinct(Stream_ID)) %>%
  pull(unique_count)

print(unique_stream_id_count)

# Export with dynamic filename based on record length
write.csv(drivers_df, 
          sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length), 
          row.names = FALSE)

# Create the tot_average dataframe with mean, q_5, and q_95
tot_average <- drivers_df %>%
  dplyr::group_by(Stream_ID) %>%
  summarise(
    # Numerical variables: calculate the mean across all years
    drainage_area = mean(drainage_area, na.rm = TRUE),
    NOx = mean(NOx, na.rm = TRUE),
    P = mean(P, na.rm = TRUE),
    precip = mean(precip, na.rm = TRUE),
    Q = mean(Q, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    Max_Daylength = mean(Max_Daylength, na.rm = TRUE),
    snow_cover = mean(snow_cover, na.rm = TRUE),
    npp = mean(npp, na.rm = TRUE),
    evapotrans = mean(evapotrans, na.rm = TRUE),
    # silicate_weathering = mean(silicate_weathering, na.rm = TRUE),
    greenup_day = mean(greenup_day, na.rm = TRUE),
    permafrost = mean(permafrost, na.rm = TRUE),
    elevation = mean(elevation, na.rm = TRUE),
    basin_slope = mean(basin_slope, na.rm = TRUE),
    FNConc = mean(FNConc, na.rm = TRUE),
    FNYield = mean(FNYield, na.rm = TRUE),
    GenConc = mean(GenConc, na.rm = TRUE),
    GenYield = mean(GenYield, na.rm = TRUE),

    # Calculate q_5 (5th percentile) and q_95 (95th percentile) for numerical variables
    q_5 = quantile(Q, 0.05, na.rm = TRUE),
    q_95 = quantile(Q, 0.95, na.rm = TRUE),

    # Categorical variables: grab the first value
    across(contains("rocks"), ~ first(.)),
    across(contains("land_"), ~ first(.))
  ) %>%
  ungroup() %>%
  # Replace NaN with NA in all columns
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

# Print a preview
print(head(tot_average))


# Count the number of unique Stream_IDs
num_unique_stream_ids <- tot_average %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

# Export average data with dynamic filename
write.csv(as.data.frame(tot_average),
          sprintf("AllDrivers_Harmonized_Average_filtered_%d_years.csv", record_length),
          row.names = FALSE)

gc()
