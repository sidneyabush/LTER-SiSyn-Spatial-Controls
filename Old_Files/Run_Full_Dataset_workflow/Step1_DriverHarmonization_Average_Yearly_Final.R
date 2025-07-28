# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr, corrplot)

# Clear environment
rm(list = ls())

# Define the record length in years (change this to 1, 5, 10, 20... as needed)
record_length <- 5  

# ## ------------------------------------------------------- ##
#              # Read in and Tidy Data ----
# ## ------------------------------------------------------- ##

## Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

# Read and clean WRTDS data this is filtered data
wrtds_df <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  dplyr::rename(LTER = LTER.x) %>%
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
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

wrtds_CJ <- read.csv("wrtds_kalman_annual_CatalinaJemez.csv") %>%
  dplyr::filter(chemical == "DSi")

## NEED TO COMBINE THESE
wrtds_df <- bind_rows(wrtds_df, wrtds_CJ) 

# Standardize Stream_ID formatting in all datasets to remove extra spaces
standardize_stream_id <- function(df) {
  df %>%
    mutate(Stream_ID = str_trim(Stream_ID),  # Remove leading/trailing spaces
           Stream_ID = str_replace_all(Stream_ID, "\\s+", " "))  # Convert multiple spaces to a single space
}

# Count number of years per site and filter for sites with at least 1 years of data
site_year_counts <- wrtds_df %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(year_count = n_distinct(Year)) %>%
  dplyr::filter(year_count >= record_length)

# Filter the main dataset to only include sites with sufficient data
wrtds_df <- wrtds_df %>%
  dplyr::filter(Stream_ID %in% site_year_counts$Stream_ID)

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
  dplyr::mutate(FNYield = (FNFlux * 365) / drainSqKm,
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
# Import the flashiness CSV that contains Stream_ID and RBI
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

# -----------------------------------------------------------
# Import Recession Curve Slope Data and Merge with tot ----
# -----------------------------------------------------------
# Import the recession_slope CSV that contains Stream_ID and RBI
recession_slope <- read_csv("Recession_Slopes_by_StreamID_Aggregate.csv") %>%
  dplyr::rename(recession_slope = slope) %>%
  dplyr::select(-'...1', -n_days)

# Merge the recession_slope data into the tot dataset by Stream_ID
tot <- tot %>% 
  left_join(recession_slope, by = "Stream_ID") 

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
si_drivers <- read.csv("all-data_si-extract_2_20250325.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-contains("soil"), 
                -contains("cycle1")) %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    )) %>%
  mutate(Stream_ID = paste0(LTER, "__", Stream_Name)) %>%
  filter(!(LTER == "MCM")) %>%
  dplyr::select(-contains(".y"), -contains(".x"))


si_drivers <- standardize_stream_id(si_drivers)

# Convert any greenup-related columns to day-of-year format
greenup_cols <- grep("greenup_", colnames(si_drivers), value = TRUE)
if(length(greenup_cols) > 0){
  si_drivers[, greenup_cols] <- lapply(si_drivers[, greenup_cols], function(x) {
    x <- as.Date(x, format = "%Y-%m-%d")
    as.numeric(format(x, "%j"))
  })
}

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
  dplyr::rename(Year = year) %>%
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
  dplyr::rename(LTER = LTER.x) %>%
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

## Import for Catalina Jemez: 
wrtds_NP_CJ <- read.csv("wrtds_kalman_annual_CatalinaJemez.csv") %>%
  dplyr::select(-DecYear, -LTER, -contains("FN"), -GenFlux)

## NOW COMBINE!!! 
wrtds_NP <- bind_rows(wrtds_NP, wrtds_NP_CJ) 

wrtds_NP <- wrtds_NP %>%
  filter(chemical %in% c("P", "NO3", "NOx") & GenConc > 0) %>%  # Keep only positive GenConc
  mutate(
    chemical = ifelse(chemical %in% c("NOx", "NO3"), "NOx", chemical)  # Simplify to NOx
  )

# Handle duplicates by taking the median
wrtds_NP <- wrtds_NP %>%
  dplyr::group_by(Stream_ID, Year, chemical) %>%  # Group by unique combinations
  dplyr::summarise(
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
  dplyr::mutate(
    Year = as.integer(year(as.Date(date, format = "%Y-%m-%d"))),  # Extract Year from date
    Stream_ID = paste(LTER, Stream_Name, sep = "__"),              # Create Stream_ID by combining LTER and Stream_Name
    solute_simplified = dplyr::case_when(  # Simplify solutes into NOx and P categories
      variable %in% c("NOx", "NO3") ~ "NOx",
      variable %in% c("SRP", "PO4") ~ "P",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(solute_simplified)) %>%  # Keep only relevant solutes (NOx, P)
  dplyr::group_by(Stream_ID, Year, solute_simplified) %>%  # Group by Stream_ID, year, and solute
  dplyr::summarise(
    median_value = median(value, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Reshape the data to wide format
raw_NP_wide <- raw_NP_median %>%
  tidyr::pivot_wider(
    names_from = solute_simplified,  # Create columns for NOx and P
    values_from = median_value,      # Populate these columns with the median values
    values_fn = mean,                # Handle any duplicates by taking mean
    values_fill = list(median_value = NA)  # Fill missing combinations with NA
  )

# ## ------------------------------------------------------- ##
#           # Combine WRTDS and RAW Data ---- 
# ## ------------------------------------------------------- ##

# First fix the WRTDS wide data to prevent list columns
wrtds_NP_wide <- wrtds_NP %>%
  tidyr::pivot_wider(
    id_cols = c(Stream_ID, Year),
    names_from = chemical,
    values_from = GenConc,
    values_fn = mean,  # Handle duplicates by taking mean
    values_fill = list(GenConc = NA)
  )

# Perform a full outer join to combine `wrtds_NP_wide` and `raw_NP_wide`
combined_NP <- dplyr::full_join(wrtds_NP_wide, raw_NP_wide, by = c("Stream_ID", "Year"))

# Fill gaps in WRTDS data with raw data where necessary
combined_NP <- combined_NP %>%
  # Rename columns so that we have source identifiers
  dplyr::rename(
    P_wrtds = P.x,      # P from WRTDS
    NOx_wrtds = NOx.x,  # NOx from WRTDS
    P_raw = P.y,        # P from raw data
    NOx_raw = NOx.y     # NOx from raw data
  ) %>%
  # Create source indicator columns
  dplyr::mutate(
    P_source = dplyr::if_else(is.na(P_wrtds), "raw", "WRTDS"),
    NOx_source = dplyr::if_else(is.na(NOx_wrtds), "raw", "WRTDS")
  ) %>%
  # Fill gaps: if WRTDS is missing, use raw data
  dplyr::mutate(
    P = dplyr::coalesce(P_wrtds, P_raw),
    NOx = dplyr::coalesce(NOx_wrtds, NOx_raw)
  ) %>%
  # Drop the intermediate columns
  dplyr::select(-P_wrtds, -NOx_wrtds, -P_raw, -NOx_raw)

streams_gapfill <- combined_NP %>%
  dplyr::filter(P_source == "raw" | NOx_source == "raw") %>%
  dplyr::distinct(Stream_ID) %>%
  dplyr::pull(Stream_ID)
print(streams_gapfill)

# ## ------------------------------------------------------- ##
#           # Merge Combined Data with `tot` ----
# ## ------------------------------------------------------- ##
# Merge the combined dataset with `tot`
tot <- tot %>%
  dplyr::left_join(combined_NP, by = c("Stream_ID", "Year")) %>%
  dplyr::mutate(
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
# drop all the “.y” columns, then strip “.x” from what remains
tot <- tot %>%
  dplyr::select(-ends_with(".y")) %>%
  dplyr::rename_with(~ str_remove(., "\\.x$"))

# Now generate a list of missing NOx and P sites-year combinations:
missing_N_P_data_summary <- tot %>%
  dplyr::filter(Year > 2000 & Year <= 2024) %>%  # Filter for years within range
  dplyr::select(Stream_ID, Year, NOx, P) %>%
  dplyr::distinct(Stream_ID, Year, across(starts_with("NOx")), across(starts_with("P"))) %>%  # Ensure no duplicates
  pivot_longer(cols = starts_with("NOx") | starts_with("P"), names_to = "Variable", values_to = "Value") %>%  # Reshape
  dplyr::filter(is.na(Value)) %>%  # Keep only missing values
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

# ## ------------------------------------------------------- ##
# #  Silicate Weathering ----
# ## ------------------------------------------------------- ##
# # Read and prepare data
# mapped_lithologies <- fread("mapped_lithologies.csv")
# 
# # Convert `tot` and `mapped_lithologies` to data.tables
# setDT(tot)
# setDT(mapped_lithologies)
# 
# # Ensure compatibility in the major_rock column for merging
# tot[, major_rock := as.character(major_rock)]
# mapped_lithologies[, major_rock := as.character(major_rock)]
# 
# ###############################################################################
# # EARLY FILTERING: Remove sites without valid major rock type
# ###############################################################################
# print("\n=== APPLYING EARLY FILTERING FOR MAJOR_ROCK ===")
# 
# # Check what will be filtered out
# rows_before <- nrow(tot)
# missing_major_rock <- sum(is.na(tot$major_rock))
# blank_major_rock <- sum(trimws(tot$major_rock) == "", na.rm = TRUE)
# zero_major_rock <- sum(tot$major_rock == "0", na.rm = TRUE)
# total_to_filter <- missing_major_rock + blank_major_rock + zero_major_rock
# 
# print(paste("Rows with missing major_rock:", missing_major_rock))
# print(paste("Rows with blank major_rock:", blank_major_rock))
# print(paste("Rows with '0' major_rock:", zero_major_rock))
# print(paste("Total rows to be filtered out:", total_to_filter))
# print(paste("Rows that will remain:", rows_before - total_to_filter))
# 
# # Apply the early filtering - REMOVE SITES WITH major_rock = "0", NA, or blank
# tot <- tot[!is.na(major_rock) & 
#              trimws(major_rock) != "" & 
#              major_rock != "0"]
# 
# print(paste("Actual rows after filtering:", nrow(tot)))
# 
# # Verify filtering worked as expected
# if(nrow(tot) == (rows_before - total_to_filter)) {
#   print("✓ Early filtering successful!")
# } else {
#   print("✗ Filtering didn't work as expected")
# }
# 
# # Report which Stream_IDs were removed (if any)
# if(total_to_filter > 0) {
#   print("Sites removed due to invalid major_rock:")
#   # This would require the original data to show what was removed
#   # Just report that filtering occurred
#   print(paste("Successfully removed", total_to_filter, "observations"))
# }
# 
# ###############################################################################
# # Continue with weathering analysis on filtered data
# ###############################################################################
# 
# # Perform the merge and filter out NA values in mapped_lithology
# weathering <- merge(tot[, .(Stream_ID, Year, major_rock, Q, temp, drainSqKm)],
#                     mapped_lithologies[, .(major_rock, mapped_lithology)],
#                     by = "major_rock", all.x = TRUE)
# weathering <- weathering[!is.na(mapped_lithology)]  # Remove rows with NA in mapped_lithology
# 
# # Constants for weathering calculations
# seconds_per_year <- 31536000
# kg_per_m3 <- 1000
# km2_to_m2 <- 10^6
# R <- 8.314
# 
# # Define lithology parameters as a data.table
# lithology_params <- data.table(
#   mapped_lithology = c("su", "vb", "pb", "py", "va", "vi", "ss", "pi", "sm", "mt", "pa"),
#   b = c(0.003364, 0.007015, 0.007015, 0.0061, 0.002455, 0.007015, 0.005341, 0.007015, 0.012481, 0.007626, 0.005095),
#   sp = c(1, 1, 1, 1, 1, 1, 0.64, 0.58, 0.24, 0.25, 0.58),
#   sa = c(60, 50, 50, 46, 60, 50, 60, 60, 60, 60, 60)
# )
# 
# # Convert temperature to Kelvin for calculations
# weathering[, temp := as.numeric(temp)]
# weathering[, temp_K := temp + 273.15]
# 
# # Calculate runoff based on the given formula
# weathering[, runoff := (Q * seconds_per_year * kg_per_m3) / (drainSqKm * km2_to_m2)]
# 
# # Define a function for vectorized calculation of weathering
# calculate_weathering_vectorized <- function(lithologies, runoff, temp_k) {
#   lithologies_split <- strsplit(lithologies, ",\\s*")
#   weathering_results <- sapply(seq_along(lithologies_split), function(i) {
#     liths <- lithologies_split[[i]]
#     weathering_values <- sapply(liths, function(lith) {
#       params <- lithology_params[mapped_lithology == lith]
#       if (nrow(params) == 0) stop(paste("Lithology not found in the table for", lith))
#       params$b * (params$sp * exp(((1000 * params$sa) / R) * ((1 / 284.2) - (1 / temp_k[i])))) * runoff[i]
#     })
#     mean(weathering_values, na.rm = TRUE)
#   })
#   return(weathering_results)
# }
# 
# # Calculate silicate weathering for each row in the weathering data
# weathering[, silicate_weathering := calculate_weathering_vectorized(mapped_lithology, runoff, temp_K), by = .(Stream_ID, Year)]
# 
# setDT(weathering)
# 
# # Ensure 'Stream_ID' and 'Year' are compatible types
# tot[, Stream_ID := as.character(Stream_ID)]
# tot[, Year := as.integer(Year)]
# weathering[, Stream_ID := as.character(Stream_ID)]
# weathering[, Year := as.integer(Year)]
# 
# # Calculate silicate weathering for each row in the weathering data
# weathering[, silicate_weathering := calculate_weathering_vectorized(mapped_lithology, runoff, temp_K)]
# 
# # Ensure uniqueness in the datasets
# weathering <- unique(weathering, by = c("Stream_ID", "Year"))
# tot <- unique(tot, by = c("Stream_ID", "Year"))
# 
# # Perform the merge correctly
# tryCatch({
#   tot <- merge(
#     tot,
#     weathering[, .(Stream_ID, Year, silicate_weathering)],
#     by = c("Stream_ID", "Year"),
#     all.x = TRUE,
#     allow.cartesian = FALSE
#   )
# }, error = function(e) {
#   message("Error encountered during merge: ", e$message)
# })
# 
# # Clean up memory
# gc()
# 
# # Verify the number of unique Stream_IDs after filtering
# num_unique_stream_ids <- tot %>%
#   pull(Stream_ID) %>%
#   n_distinct()
# print(paste("Unique Stream_IDs after early filtering:", num_unique_stream_ids))


# -----------------------------------------------------------
# New: Incorporate Land Cover Data ----
# -----------------------------------------------------------
# Read in the original land cover file (with percentage values)
lulc <- read.csv("DSi_LULC_filled_interpolated_Simple.csv", stringsAsFactors = FALSE) %>%
  dplyr::select("Stream_Name", "Year", "Simple_Class", "LandClass_sum") %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    LandClass_sum = if_else(is.na(LandClass_sum) | 
                                   LandClass_sum == 0, LandClass_sum, LandClass_sum * 100))  # convert to percentage

# Remove any existing columns that start with "land_" and the "major_land" column from tot
tot <- tot %>% dplyr::select(-dplyr::starts_with("land_"), -dplyr::any_of("major_land"))

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
  dplyr::mutate(major_land = apply(dplyr::select(., all_of(land_cols)), 1, function(x) {
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
                cycle0, permafrost_mean_m, elevation_mean_m, RBI, recession_slope,
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
  mutate_at(vars(25:39), ~ replace(., is.na(.), 0)) %>%
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

library(dplyr)
library(stringr)
library(tibble)

# 1) Total obs (site‐year rows) and total unique sites
total_obs   <- nrow(drivers_df)
total_sites <- drivers_df %>% pull(Stream_ID) %>% unique() %>% length()

# 2) Raw N/P gap‐filled obs
raw_obs_count <- drivers_df %>%
  left_join(
    combined_NP %>% select(Stream_ID, Year, P_source, NOx_source),
    by = c("Stream_ID","Year")
  ) %>%
  filter(P_source == "raw" | NOx_source == "raw") %>%
  nrow()
raw_obs_pct <- 100 * raw_obs_count / total_obs

# 3) Slope gap‐filled sites
slope_sites_in_final <- intersect(
  tot_with_slope_filled$Stream_ID %>% unique(),
  drivers_df$Stream_ID %>% unique()
)
slope_site_count <- length(slope_sites_in_final)
slope_site_pct   <- 100 * slope_site_count / total_sites

# 4) Australian sites in final
aus_sites      <- drivers_df %>%
  filter(str_starts(Stream_ID, "Australia")) %>%
  distinct(Stream_ID) %>%
  pull(Stream_ID)
aus_site_count <- length(aus_sites)
aus_site_pct   <- 100 * aus_site_count / total_sites

# 5) MD sites in final
md_sites      <- drivers_df %>%
  filter(str_starts(Stream_ID, "MD")) %>%
  distinct(Stream_ID) %>%
  pull(Stream_ID)
md_site_count <- length(md_sites)
md_site_pct   <- 100 * md_site_count / total_sites

# 6) Build a combined summary
summary_df <- tibble(
  Category         = c(
    "Raw_NP (obs)",
    "Slope (sites)",
    "Australian sites",
    "MD sites"
  ),
  Count            = c(
    raw_obs_count,
    slope_site_count,
    aus_site_count,
    md_site_count
  ),
  Percent_of_Total = c(
    raw_obs_pct,
    slope_site_pct,
    aus_site_pct,
    md_site_pct
  )
)

# 7) Print
print(summary_df)

## Create Correlation Plot for Supplement (including rock vars)
library(dplyr)
library(corrplot)
library(Cairo)    # make sure you’ve installed the Cairo package

# 0) record_length and output directory
record_length <- 5
output_dir    <- "Final_Figures"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 1) Manual ordering and labels
base_vars  <- c(
  "NOx", "P", "npp", "evapotrans", "greenup_day", "precip", "temp",
  "snow_cover", "permafrost", "elevation", "basin_slope", "RBI",
  "recession_slope", "land_Bare", "land_Cropland", "land_Forest",
  "land_Grassland_Shrubland", "land_Ice_Snow", "land_Impervious",
  "land_Salt_Water", "land_Tidal_Wetland", "land_Water",
  "land_Wetland_Marsh"
)
rock_vars  <- c(
  "rocks_volcanic", "rocks_sedimentary",
  "rocks_carbonate_evaporite", "rocks_metamorphic", "rocks_plutonic"
)
var_order <- c(base_vars, rock_vars)

pretty_labels <- c(
  # base
  "N", "P", "NPP", "ET", "Greenup Day", "Precip", "Temp",
  "Snow Cover", "Permafrost", "Elevation", "Basin Slope",
  "Flashiness (RBI)", "Recession Curve Slope", "Land: Bare",
  "Land: Cropland", "Land: Forest", "Land: Grass & Shrub",
  "Land: Ice & Snow", "Land: Impervious", "Land: Salt Water",
  "Land: Tidal Wetland", "Land: Water Body", "Land: Wetland Marsh",
  # rock
  "Rock: Volcanic", "Rock: Sedimentary", "Rock: Carbonate Evaporite",
  "Rock: Metamorphic", "Rock: Plutonic"
)

# 2) Build numeric matrix and compute correlation
drivers_numeric <- drivers_df %>%
  select(all_of(var_order)) %>%
  mutate(across(everything(), as.numeric))

driver_cor     <- cor(drivers_numeric, use = "pairwise.complete.obs")
driver_cor_ord <- driver_cor[var_order, var_order]
dimnames(driver_cor_ord) <- list(pretty_labels, pretty_labels)

# 3) Use CairoPNG to write
CairoPNG(
  filename = file.path(output_dir,
                       sprintf("FigSX_corr_plot_%d_years.png", record_length)),
  width    = 12, height = 12, units = "in", res = 300
)
par(mar = c(6, 5, 1, 1))  # more bottom margin
corrplot(
  driver_cor_ord,
  type    = "lower",
  order   = "original",
  tl.col  = "black",
  tl.cex  = 1.2,
  cl.cex  = 1.2,
  tl.pos  = "ld",
  tl.srt  = 90,
  diag    = FALSE
)
dev.off()

# ──────────────────────────────────────────────────────────────────────────────
# 11. Faceted histograms for all drivers (using drivers_df), with log10(N) & log10(P)
# ──────────────────────────────────────────────────────────────────────────────
# 0) load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# 1) set working + output dir
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "Final_Figures"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 2) define order + pretty labels (update N & P)
var_order <- c(
  "NOx", "P", "npp", "evapotrans", "greenup_day", "precip", "temp",
  "snow_cover", "permafrost", "elevation", "basin_slope", "RBI",
  "recession_slope", "land_Bare", "land_Cropland", "land_Forest",
  "land_Grassland_Shrubland", "land_Ice_Snow", "land_Impervious",
  "land_Salt_Water", "land_Tidal_Wetland", "land_Water",
  "land_Wetland_Marsh",
  "rocks_volcanic", "rocks_sedimentary",
  "rocks_carbonate_evaporite", "rocks_metamorphic", "rocks_plutonic"
)

pretty_labels <- c(
  "log(N)",         # for NOx
  "log(P)",         # for P
  "NPP", "ET", "Greenup Day", "Precip", "Temp",
  "Snow Cover", "Permafrost", "Elevation", "Basin Slope",
  "Flashiness (RBI)", "Recession Curve Slope", "Land: Bare",
  "Land: Cropland", "Land: Forest", "Land: Grass & Shrub",
  "Land: Ice & Snow", "Land: Impervious", "Land: Salt Water",
  "Land: Tidal Wetland", "Land: Water Body", "Land: Wetland Marsh",
  "Rock: Volcanic", "Rock: Sedimentary", "Rock: Carbonate Evaporite",
  "Rock: Metamorphic", "Rock: Plutonic"
)
names(pretty_labels) <- var_order

# 3) pivot drivers_df into long, recoding to pretty factor, and log‐transform N,P
all_drivers_long <- drivers_df %>%
  select(all_of(var_order)) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "driver",
    values_to = "value"
  ) %>%
  mutate(
    value = case_when(
      driver == "NOx" ~ log10(value),
      driver == "P"   ~ log10(value),
      TRUE            ~ value
    ),
    driver = factor(driver, levels = var_order, labels = pretty_labels)
  )

# 4) compute per-driver means
# compute per‐driver means
means_df <- all_drivers_long %>%
  group_by(driver) %>%
  summarize(mean_val = mean(value, na.rm=TRUE), .groups="drop")

# build the histogram with mean‐lines
hist_all <- ggplot(all_drivers_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "grey85", color = "black") +
  # dashed mean line
  geom_vline(data = means_df,
             aes(xintercept = mean_val),
             linetype = "dashed", color = "steelblue", size = 0.9) +
  facet_wrap(~ driver, scales = "free", ncol = 6) +
  labs(x = "Value", y = "Count") +
  theme_classic(base_size = 20) +
  theme(
    strip.text       = element_text(size = 18, face = "plain"),
    axis.text        = element_text(size = 16),
    axis.title       = element_text(size = 18),
    panel.spacing.x   = unit(0.5, "lines"),
    panel.spacing.y   = unit(0.5, "lines")
  )

print(hist_all)
ggsave(
  file.path(output_dir, "FigSX_Hist_All.png"),
  hist_all,
  width  = 24, height = 16, dpi = 300, bg = "white"
)

print(unique(drivers_df$Stream_ID))
