## ------------------------------------------------------- ##
# Silica WG - Harmonize Drivers: Yearly Models
## ------------------------------------------------------- ##
# Written by:
## Sidney A Bush, Keira Johnson

# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr)

# Clear environment
rm(list = ls())

## ------------------------------------------------------- ##
# Read in and Tidy Data ----
## ------------------------------------------------------- ##
## Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

wrtds_df <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  rename(LTER = LTER.x) %>%
  dplyr::select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"), 
                -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Year = floor(as.numeric(DecYear)) # Convert DecYear to Year
  ) %>%
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  filter(GenConc <= 60) %>%  # Remove rows where GenConc > 60
  filter(FNConc >= 0.5 * GenConc & FNConc <= 1.5 * GenConc)  %>%  # Remove rows where FNConc is ±50% of GenConc
  dplyr::select(-contains("Gen"), -DecYear, -.groups, -LTER) # Trying to tidy up this workflow by removing GenConc and GenFlux, this can be added back in in the future if GenConc/ GenFlux are desired for Yearly models.

gc()

num_unique_stream_ids <- wrtds_df %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

## Need to tidy the Finnish site names:
finn <- read.csv("FinnishSites.csv")

finn$Stream_ID <- paste0("Finnish Environmental Institute__", finn$Site.ID)
finn$Stream_ID2 <- paste0("Finnish Environmental Institute__", finn$Site)

for (i in 1:nrow(finn)) {
  site_id<-finn[i,3]
  row_num<-which(wrtds_df$Stream_ID==site_id)
  wrtds_df[row_num, "Stream_ID"]<-finn[i,4]
}

wrtds_df$Stream_ID <- ifelse(wrtds_df$Stream_ID=="Finnish Environmental Institute__TORNIONJ KUKKOLA 14310  ",
                           "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310", wrtds_df$Stream_ID)

wrtds_df$Stream_ID <- ifelse(wrtds_df$Stream_ID=="Finnish Environmental Institute__SIMOJOKI AS. 13500      ",
                           "Finnish Environmental Institute__SIMOJOKI AS. 13500", wrtds_df$Stream_ID)

## ------------------------------------------------------- ##
            # Calculate Yields ----
## ------------------------------------------------------- ##
wrtds_df <- wrtds_df %>%
  mutate(FNYield = FNFlux / drainSqKm) %>%
        # GenYield = GenFlux / drainSqKm) %>% # removed this since we removed GenFlux on import
  dplyr::select(-contains("Flux"))

## ------------------------------------------------------- ##
        # Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv") %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  )

tot <- inner_join(wrtds_df, KG %>% 
                    select(-Stream_Name, -Use_WRTDS, -contains("Coord"), -LTER, -X, -Latitude, -Longitude), 
                  by = "Stream_ID")

## Subset to just DSi to make merging easier from here out
## Now only wrtds_df has all chemicals, while tot has only DSi
tot <- subset(tot, chemical == "DSi")

## ------------------------------------------------------- ##
          # Import Daylength ----
## ------------------------------------------------------- ##
# Load and clean daylength data
daylen <- read.csv("Monthly_Daylength_2.csv") %>%
  dplyr::select(-1)

# Define renamed and old names directly in a streamlined manner
name_conversion <- data.frame(
  Stream_Name = c("East Fork", "West Fork"),
  Updated_StreamName = c("east fork", "west fork")
)

# Calculate min and max daylength per site
daylen_range <- daylen %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(
    Min_Daylength = min(mean_daylength, na.rm = TRUE),
    Max_Daylength = max(mean_daylength, na.rm = TRUE),
    .groups = "drop"  # Ungroup after summarization
  ) %>%
  left_join(name_conversion, by = "Stream_Name") %>%
  mutate(Stream_Name = coalesce(Updated_StreamName, Stream_Name)) %>%
  dplyr::select(-Updated_StreamName)

# Ensure the result is joined to "tot"
tot <- tot %>%
  inner_join(daylen_range, by = "Stream_Name")

# ## ------------------------------------------------------- ##
#           # On to the Dynamic Drivers ---- 
# ## ------------------------------------------------------- ##
## ------------------------------------------------------- ##
# Step 1: Load and preprocess spatial drivers
## ------------------------------------------------------- ##
# Define renamed and old names directly
name_conversion <- data.frame(
  Stream_ID = c("Walker Branch__East Fork", "Walker Branch__West Fork"),
  Updated_StreamName = c("Walker Branch__east fork", "Walker Branch__west fork")
)

# Read and preprocess spatial drivers
spatial_drivers <- read.csv("all-data_si-extract_2_202412.csv", stringsAsFactors = FALSE) %>%
  select(-contains("soil")) %>%
  # Create Stream_ID first using LTER and Stream_Name
  mutate(Stream_ID = paste0(LTER, "__", Stream_Name)) %>%
  # Incorporate site renaming
  left_join(name_conversion, by = "Stream_ID") %>%
  mutate(
    Stream_ID = coalesce(Updated_StreamName, Stream_ID)  # Replace Stream_ID with Updated_StreamName if available
  ) %>%
  select(-Updated_StreamName)  # Remove temporary renaming column


## Before, using full abbrevs removed some of the spatial driver columns (e.g., "dec" in "deciduous" was causing
#  deciduous land cover to be filtered out)
months_abb <- c("_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_")

# Remove monthly drivers from spatial drivers
spatial_drivers <- spatial_drivers[,-c(which(colnames(spatial_drivers) %like% months_abb))]

# Confirm NA replacement for permafrost columns
permafrost_cols <- grep("permafrost", colnames(spatial_drivers), value = TRUE)

# Replace NA values with 0 for all permafrost columns
spatial_drivers[, permafrost_cols] <- lapply(spatial_drivers[, permafrost_cols], function(x) {
  x <- as.numeric(x)  # Convert to numeric to avoid issues
  x[is.na(x)] <- 0
  return(x)
})

## GREEEN-UP DAY
# Define greenup cycles (excluding cycle1 as it's not needed)
greenup_cycles <- c("cycle0")

# Standardize column names by removing "MMDD" if present
colnames(spatial_drivers) <- sub("MMDD$", "", colnames(spatial_drivers))

# Extract years from tot to identify where DSi data exists
dsi_years <- unique(floor(tot$Year))  # Extract integer years

# Create a pattern for greenup_cycle0 columns corresponding to DSi years
greenup_pattern <- paste0("greenup_", greenup_cycles, "_(", paste(dsi_years, collapse = "|"), ")")
cycle_cols <- grep(greenup_pattern, colnames(spatial_drivers), value = TRUE)

# Filter the spatial drivers for relevant sites and years with DSi data
filtered_drivers <- spatial_drivers[spatial_drivers$Stream_ID %in% unique(tot$Stream_ID), ]

# Convert greenup date columns to day-of-year values for cycle0
for (col in cycle_cols) {
  # Ensure the column is a character vector
  filtered_drivers[[col]] <- as.character(filtered_drivers[[col]])
  
  # Convert to DOY
  filtered_drivers[[paste0(col, "_doy")]] <- ifelse(
    !is.na(as.Date(filtered_drivers[[col]], format = "%Y-%m-%d")),
    yday(as.Date(filtered_drivers[[col]], format = "%Y-%m-%d")),
    NA_real_
  )
}

# Remove greenup day columns that do not have "_doy" in their names
columns_to_keep <- grep("_doy$", colnames(filtered_drivers), value = TRUE)
columns_to_keep <- c("Stream_ID", columns_to_keep)  # Ensure essential columns are retained
filtered_drivers <- filtered_drivers[, columns_to_keep]

# Replace the greenup day columns in spatial_drivers with the DOY columns from filtered_drivers
# Remove greenup day columns from spatial_drivers
columns_to_remove <- grep("^greenup_", colnames(spatial_drivers), value = TRUE)

# Retain only the columns not in columns_to_remove
spatial_drivers <- spatial_drivers[, !colnames(spatial_drivers) %in% columns_to_remove]

# Add the updated DOY columns from filtered_drivers to spatial_drivers
spatial_drivers <- merge(
  spatial_drivers,
  filtered_drivers,
  by = c("Stream_ID"),
  all.x = TRUE
)

## ------------------------------------------------------- ##
# Step 2: Process annual data while retaining NA values
## ------------------------------------------------------- ##

# Identify columns with numbers in the header (annual data)
year_columns <- grep("[0-9]", colnames(spatial_drivers), value = TRUE)
non_year_columns <- setdiff(colnames(spatial_drivers), year_columns)

# Convert to data.table
setDT(spatial_drivers)

# Subset columns corresponding to years
spatial_drivers_with_years <- spatial_drivers[, ..year_columns]

# Add the Stream_ID column
spatial_drivers_with_years[, Stream_ID := spatial_drivers$Stream_ID]

spatial_drivers_with_years[, (setdiff(names(spatial_drivers_with_years), "Stream_ID")) := 
                             lapply(.SD, as.numeric), .SDcols = setdiff(names(spatial_drivers_with_years), "Stream_ID")]

spatial_drivers_long <- melt(
  spatial_drivers_with_years,
  id.vars = "Stream_ID",
  variable.name = "variable",
  value.name = "value"
)

setDT(spatial_drivers_long)

spatial_drivers_long[, `:=`(
  driver_type = fifelse(
    grepl("^snow", variable), 
    sub("^[^_]+_[0-9]{4}_", "", variable),
    sub("_[0-9]{4}.*$", "", variable)
  ),
  year = as.integer(sub(".*_([0-9]{4}).*", "\\1", variable))
)]

spatial_drivers_long <- spatial_drivers_long %>%
  filter(!is.na(value)) %>%
  distinct(Stream_ID, driver_type, year, .keep_all = TRUE)

gc()

## ------------------------------------------------------- ##
# Step 3: Pivot all driver types to wide format
## ------------------------------------------------------- ##

# Step 1: Summarize to ensure one row per Stream_ID, year, and driver_type
spatial_drivers_long <- spatial_drivers_long %>%
  group_by(Stream_ID, year, driver_type) %>%
  summarise(value = first(value), .groups = "drop")  # Keep only the first value if duplicates exist

# Step 2: Check for duplicates in long format
duplicates_long <- spatial_drivers_long %>%
  group_by(Stream_ID, year, driver_type) %>%
  filter(n() > 1)

if (nrow(duplicates_long) > 0) stop("Duplicate rows detected in long format after summarization.")

# Step 3: Pivot to wide format
spatial_drivers_wide <- spatial_drivers_long %>%
  pivot_wider(
    names_from = driver_type,  # Create columns for each feature
    values_from = value
  )

# Step 4: Ensure no duplicates in wide format
duplicates_wide <- spatial_drivers_wide %>%
  group_by(Stream_ID, year) %>%
  filter(n() > 1)

if (nrow(duplicates_wide) > 0) stop("Duplicate rows detected in wide format. Check grouping logic.")

# Step 5: Rename `year` to `Year`
spatial_drivers_wide <- spatial_drivers_wide %>%
  rename(Year = year)

# Step 6: Validate that all Stream_ID-Year pairs are accounted for
expected_keys <- spatial_drivers_long %>%
  select(Stream_ID, year) %>%
  distinct() %>%
  rename(Year = year)

actual_keys <- spatial_drivers_wide %>%
  select(Stream_ID, Year)

missing_keys <- anti_join(expected_keys, actual_keys, by = c("Stream_ID", "Year"))

if (nrow(missing_keys) > 0) stop("Some Stream_ID-Year pairs are missing in the wide format.")

## ------------------------------------------------------- ##
# Step 4: Combine static (non-annual) data
## ------------------------------------------------------- ##

# Ensure static data is merged properly
spatial_drivers_no_years <- spatial_drivers[, ..non_year_columns] %>%
  distinct(Stream_ID, .keep_all = TRUE)  # Remove duplicates by Stream_ID

combined_spatial_drivers <- merge(
  spatial_drivers_wide,
  spatial_drivers_no_years,
  by = "Stream_ID",
  all.x = TRUE
)

## ------------------------------------------------------- ##
# Step 5: Final merge to consolidate all rows by site and year
## ------------------------------------------------------- ##

# Merge with `tot` and consolidate rows by site and year
final_combined_data <- merge(
  tot,
  combined_spatial_drivers,
  by = c("Stream_ID", "Year"),
  all.x = TRUE
) %>%
  group_by(Stream_ID, Year) %>%  # Group by site and year
  summarise(across(everything(), ~ ifelse(all(is.na(.)), NA, first(na.omit(.))))) %>%
  ungroup()

gc()

# ## ------------------------------------------------------- ##
#           # Import WRTDS N_P Conc ---- 
# ## ------------------------------------------------------- ##
# Filter for relevant chemicals and positive GenConc values, and simplify NO3/NOx to NOx
wrtds_NP <- wrtds_df %>%
  filter(chemical %in% c("P", "NO3", "NOx"), FNConc > 0) %>%  # Removes NAs and zero values
  mutate(chemical = ifelse(chemical %in% c("NOx", "NO3"), "NOx", chemical))

# Reshape data to ensure NOx and P values are in the same row
wrtds_NP_annual_wide <- wrtds_NP %>%
  pivot_wider(
    id_cols = c(Stream_ID, Year),  # Keep Stream_ID and Year as unique identifiers
    names_from = chemical,         # Create separate columns for NOx and P
    values_from = FNConc          # Values for NOx and P come from GenConc
  )

# Find duplicates in the wide dataframe
duplicates <- wrtds_NP_annual_wide %>%
  group_by(Stream_ID, Year) %>%
  filter(n() > 1)  # Keep rows where there are duplicate Stream_ID-Year combinations

# Print duplicates for review
print(duplicates)

# Optional: Count the number of duplicates
duplicate_count <- duplicates %>%
  summarize(duplicate_count = n())

# Print the count of duplicates
print(duplicate_count)

gc()

setDT(wrtds_NP_annual_wide)

# Merge with the "tot" dataframe to add annual NOx and P data in a single row per Stream_ID and Year
tot <- final_combined_data %>%
  inner_join(wrtds_NP_annual_wide, by = c("Stream_ID", "Year"))

# Count the number of years with NA values for NOx and P for each Stream_ID, showing only non-zero results
na_counts <- tot %>%
  group_by(Stream_ID) %>%
  summarise(
    na_years_NOx = sum(is.na(NOx)),
    na_years_P = sum(is.na(P)),
    .groups = "drop"
  ) %>%
  filter(na_years_NOx > 0 | na_years_P > 0)  # Keep only rows with non-zero counts

tot <- tot %>%
  rename(Stream_Name = Stream_Name.x)

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
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Year = floor(as.numeric(DecYear)) # Convert DecYear to Year
  ) %>%
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  filter(GenConc <= 60) %>%  # Remove rows where GenConc > 60
  filter(FNConc >= 0.5 * GenConc & FNConc <= 1.5 * GenConc)  %>%  # Remove rows where FNConc is ±50% of GenConc
  dplyr::select(-DecYear, -.groups, -LTER, -contains("FN"), -GenFlux) # Trying to tidy up this workflow by removing GenConc and GenFlux, this can be added back in in the future if GenConc/ GenFlux are desired for Yearly models.


wrtds_NP <- wrtds_NP %>%
  filter(chemical %in% c("P", "NO3", "NOx") & GenConc > 0) %>%  # Keep only positive GenConc
  mutate(
    chemical = ifelse(chemical %in% c("NOx", "NO3"), "NOx", chemical)  # Simplify to NOx
  )

# Handle duplicates by taking the median
wrtds_NP <- wrtds_NP %>%
  group_by(Stream_ID, Year, chemical) %>%  # Group by unique combinations
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

# Verify structure after reshaping
str(wrtds_NP_wide)

gc()

setDT(wrtds_NP_wide)

# Merge with the "tot" dataframe to add annual NOx and P data
tot <- final_combined_data %>%
  inner_join(wrtds_NP_wide, by = c("Stream_ID", "Year"))

str(tot)


# ## ------------------------------------------------------- ##
#           # Import RAW N_P Data ---- 
# ## ------------------------------------------------------- ##
# Read in the dataset
raw_NP <- read.csv("20241003_masterdata_chem.csv")

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
  group_by(Stream_ID, Year, solute_simplified) %>%  # Group by Stream_ID, year, and solute
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

str(raw_NP_wide)

# ## ------------------------------------------------------- ##
#           # Match Data by Year ---- 
# ## ------------------------------------------------------- ##
# Merge raw N, P data into `tot`, matching years and replacing missing values
solutes <- c("P", "NOx")

for (solute in solutes) {
  raw_solute <- raw_NP_wide %>%
    select(Stream_ID, Year, !!sym(solute)) %>%
    rename(raw_solute = !!sym(solute))
  
  tot <- tot %>%
    mutate(Year = as.numeric(Year)) %>%  # Ensure Year is numeric for consistent merging
    left_join(raw_solute, by = c("Stream_ID", "Year")) %>%
    mutate(!!sym(solute) := ifelse(is.na(!!sym(solute)), raw_solute, !!sym(solute))) %>%
    select(-raw_solute)  # Remove the temporary column
}

str(tot)

# Check for duplicates in the `tot` dataframe
duplicates_tot <- tot %>%
  group_by(Stream_ID, Year) %>% 
  filter(n() > 1)  # Keep only duplicated Stream_ID-Year combinations

# Print the duplicates for review
print(duplicates_tot)

# Count the number of duplicates
num_duplicates <- duplicates_tot %>% 
  summarise(count = n()) %>% 
  ungroup()

print(num_duplicates)

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

## ------------------------------------------------------- ##
            #  Gap Filling Missing Data ----
## ------------------------------------------------------- ##
# Import streams with na slopes
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

# Filter tot for rows with NA basin slope values
tot_with_na_slope <- tot %>%
  filter(is.na(basin_slope_mean_degree)) %>%
  select(Stream_ID)

# Merge tot_with_na_slope with US_slopes and Krycklan_slopes to fill missing values
tot_with_slope_filled <- tot_with_na_slope %>%
  left_join(US_slopes %>% select(Stream_ID, basin_slope_mean_degree), by = "Stream_ID") %>%
  left_join(Krycklan_slopes %>% select(Stream_ID, basin_slope_mean_degree), by = "Stream_ID", suffix = c("_US", "_Krycklan")) %>%
  mutate(
    basin_slope_mean_degree = coalesce(basin_slope_mean_degree_US, basin_slope_mean_degree_Krycklan)
  ) %>%
  select(Stream_ID, basin_slope_mean_degree)

tot_with_slope_filled <- as.data.table(tot_with_slope_filled)

# Convert tot to data.table
tot <- as.data.table(tot)

# Set keys on Stream_ID for both tables
setkey(tot, Stream_ID)
setkey(tot_with_slope_filled, Stream_ID)

# Update in-place: replace NA values in basin_slope_mean_degree only
tot[tot_with_slope_filled, basin_slope_mean_degree := i.basin_slope_mean_degree, on = .(Stream_ID)]

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
  select(Stream_ID)

# Merge tot_with_na_elev with US_elev to fill missing elevation values
tot_with_elev_filled <- tot_with_na_elev %>%
  left_join(US_elev %>% select(Stream_ID, elevation_mean_m), by = "Stream_ID")

# Update tot with the filled elevation values
tot <- tot %>%
  left_join(tot_with_elev_filled, by = "Stream_ID", suffix = c("", "_filled"), relationship = "many-to-many") %>%
  mutate(
    elevation_mean_m = coalesce(elevation_mean_m_filled, elevation_mean_m)
  ) %>%
  select(-elevation_mean_m_filled)  # Remove Stream_Name_filled from final output

# Tidy data for export: 
tot_si <- tot %>%
  dplyr::select(Stream_ID, Year, drainSqKm, NOx, P, precip,
         temp, Max_Daylength, max_prop_area, npp, evapotrans,
         silicate_weathering, greenup_cycle0, permafrost_mean_m, elevation_mean_m, 
         basin_slope_mean_degree,
         contains("Conc"), contains("Yield"),
         contains("rocks"), contains("land_"))%>%
  dplyr::rename(snow_cover = max_prop_area, 
                greenup_day = greenup_cycle0,
                drainage_area = drainSqKm,
                elevation = elevation_mean_m,
                permafrost = permafrost_mean_m,
                basin_slope = basin_slope_mean_degree) %>%
  dplyr::mutate(permafrost = ifelse(is.na(permafrost), 0, permafrost)) 


duplicates <- tot_si %>%
  group_by(Stream_ID, Year) %>%
  filter(n() > 1)


if (nrow(duplicates) > 0) {
  print("Duplicates detected:")
  print(duplicates)
} else {
  print("No duplicates detected.")
}

tot_si <- tot_si %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

## ------------------------------------------------------- ##
#  Export Annual Driver data ----
## ------------------------------------------------------- ##
# Check number of unique sites: should agree with avg
num_unique_sites <- tot_si %>% 
  summarise(num_sites = n_distinct(Stream_ID))

print(num_unique_sites)

write.csv(as.data.frame(tot_si), "AllDrivers_Harmonized_Yearly.csv")

gc()
