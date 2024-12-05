## ------------------------------------------------------- ##
# Silica WG - Harmonize Drivers: AnnualModels
## ------------------------------------------------------- ##
# Written by:
## Sidney A Bush, Keira Johnson

# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr)

# Clear environment
rm(list = ls())

## ------------------------------------------------------- ##
# Read in and Tidy Data ----
## ------------------------------------------------------- ##
## Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

## Download most up to date WRTDS outputs (annual kalman)
# Read in WRTDS input file and process date and Stream_ID
wrtds_df <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  rename(LTER = LTER.x) %>%
  dplyr::select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"), -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    DecYear = as.Date(format(date_decimal(DecYear), "%Y-%m-%d")),
    Year = as.integer(format(DecYear, "%Y")),
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)  # Create Stream_ID after Stream_Name adjustment
  )

gc()

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
      # Download Reference Table from GD for DA ----
## ------------------------------------------------------- ##
ref_table_link <- "https://docs.google.com/spreadsheets/d/11t9YYTzN_T12VAQhHuY5TpVjGS50ymNmKznJK4rKTIU/edit#gid=357814834"
ref_table_folder = drive_get(as_id(ref_table_link))
ref_table <- drive_download(ref_table_folder$drive_resource, overwrite = T)

ref_table <- readxl::read_xlsx("Site_Reference_Table.xlsx")
ref_table$Stream_ID <- paste0(ref_table$LTER, "__", ref_table$Stream_Name)
area <- ref_table[,c("drainSqKm", "Stream_ID")]

# Define renamed and old names directly
name_conversion <- data.frame(
  Stream_ID = c("Walker Branch__East Fork", "Walker Branch__West Fork"),
  Updated_StreamName = c("Walker Branch__east fork", "Walker Branch__west fork")
)

# Filter and join in one step
missing_sites <- left_join(area[area$Stream_ID %in% name_conversion$Stream_ID, ], 
                           name_conversion, by = "Stream_ID")

# Remove the unnecessary column and rename the remaining one
missing_sites <- missing_sites %>%
  dplyr::select(-Stream_ID) %>%
  dplyr::rename(Stream_ID = Updated_StreamName)

# Append the updated sites to the original dataframe
area <- bind_rows(area, missing_sites)

# Perform left join and convert DecYear to Year
tot <- wrtds_df %>%
  left_join(area %>% select(Stream_ID), by = "Stream_ID", relationship = "many-to-many") %>%
  mutate(
    DecYear = as.numeric(format(DecYear, "%Y")) + as.numeric(format(DecYear, "%j")) / 365,
    Year = floor(DecYear)
  )

## ------------------------------------------------------- ##
            # Calculate Yields ----
## ------------------------------------------------------- ##
tot <- wrtds_df %>%
  mutate(FNYield = FNFlux / drainSqKm,
         GenYield = GenFlux / drainSqKm) %>%
  dplyr::select(-FNFlux, -GenFlux)

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

tot <- left_join(tot, KG %>% select(-Stream_Name), by = "Stream_ID", relationship = "many-to-many")

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

# Calculate min and max daylength, update Stream_Name, and ensure all sites in "tot" are left-joined
daylen_range <- daylen %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(
    Min_Daylength = min(mean_daylength),
    Max_Daylength = max(mean_daylength)
  ) %>%
  left_join(name_conversion, by = "Stream_Name") %>%
  mutate(Stream_Name = coalesce(Updated_StreamName, Stream_Name)) %>%
  dplyr::select(-Updated_StreamName)

# Ensure the result is left-joined to "tot"
tot <- left_join(tot, daylen_range, by = "Stream_Name", relationship = "many-to-many")

## Subset to just DSi to make merging easier from hereout
tot <- subset(tot, chemical == "DSi")

# ## ------------------------------------------------------- ##
#           # On to the Dynamic Drivers ---- 
# ## ------------------------------------------------------- ##
## ------------------------------------------------------- ##
# Step 1: Load and preprocess spatial drivers
## ------------------------------------------------------- ##
spatial_drivers <- fread("all-data_si-extract_2_202412_test.csv")  # Use fread for faster loading
spatial_drivers[, Stream_ID := paste0(LTER, "__", Stream_Name)]
spatial_drivers <- spatial_drivers[, !c("Shapefile_Name", "Discharge_File_Name"), with = FALSE]
spatial_drivers <- spatial_drivers[, !grepl("soil", names(spatial_drivers)), with = FALSE]
spatial_drivers <- spatial_drivers[, !grepl("_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_", names(spatial_drivers)), with = FALSE]

# Define greenup cycles (excluding cycle1 as it's not needed)
greenup_cycles <- c("cycle0")

# Standardize column names by removing "MMDD" if present
colnames(spatial_drivers) <- sub("MMDD$", "", colnames(spatial_drivers))

# Convert greenup date columns to day-of-year values for cycle0
for (cycle in greenup_cycles) {
  cycle_cols <- grep(paste0("greenup_", cycle), colnames(spatial_drivers), value = TRUE)
  for (col in cycle_cols) {
    spatial_drivers[[paste0(col, "_doy")]] <- ifelse(
      !is.na(as.Date(spatial_drivers[[col]], format = "%m/%d/%Y")),
      yday(as.Date(spatial_drivers[[col]], format = "%m/%d/%Y")),
      NA_real_
    )
  }
}

gc()

# Identify columns with numbers in the header (annual data)
year_columns <- grep("[0-9]", colnames(spatial_drivers), value = TRUE)
non_year_columns <- setdiff(colnames(spatial_drivers), year_columns)

## ------------------------------------------------------- ##
# Step 2: Process annual data while retaining NA values
## ------------------------------------------------------- ##
spatial_drivers_with_years <- spatial_drivers[, ..year_columns]
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

# Pivot long data into a wide format, ensuring each driver type becomes a column
spatial_drivers_wide <- spatial_drivers_long %>%
  pivot_wider(
    names_from = driver_type,  # Create columns for each driver type
    values_from = value
  )

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

# Rename "year" to "Year" in combined_spatial_drivers
combined_spatial_drivers <- combined_spatial_drivers %>%
  rename(Year = year)

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
  filter(chemical %in% c("P", "NO3", "NOx"), GenConc > 0) %>%  # Removes NAs and zero values
  mutate(chemical = ifelse(chemical %in% c("NOx", "NO3"), "NOx", chemical))

# Summarize to get the median GenConc by Stream_ID, Year, and simplified chemical with NA removal
wrtds_NP_annual <- wrtds_NP %>%
  group_by(Stream_ID, Year, chemical) %>%
  summarise(
    median_Conc = median(GenConc, na.rm = TRUE),  # Ensures NAs are ignored in median calculation
    .groups = 'drop'
  )

# Reshape data to have separate columns for NOx and P
wrtds_NP_annual_wide <- wrtds_NP_annual %>%
  pivot_wider(names_from = chemical, values_from = median_Conc)

gc()

setDT(wrtds_NP_annual_wide)

# Merge with the "tot" dataframe to add annual NOx and P data in a single row per Stream_ID and Year
tot <- final_combined_data %>%
  left_join(wrtds_NP_annual_wide, by = c("Stream_ID", "Year"))

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
#           # Import RAW N_P Conc ---- 
# ## ------------------------------------------------------- ##
# Read in the dataset
raw_NP <- read.csv("20241003_masterdata_chem.csv")

# Step 1: Filter for Nitrogen and Phosphorus variables with valid values, extract Year, and keep only years with >5 values
raw_NP <- raw_NP %>%
  filter(variable %in% c("SRP", "PO4", "NO3", "NOx") & value > 0) %>%
  mutate(Year = as.integer(year(as.Date(date, format = "%Y-%m-%d")))) %>%
  group_by(Stream_Name, Year) %>%
  filter(n() > 5) %>%  # Keep only Stream_ID and Year combinations with more than 5 values
  ungroup()

# Step 2: Calculate annual median N and P values, removing NAs before calculating
raw_NP_avg <- raw_NP %>%
  group_by(Stream_Name, Year, variable) %>%
  summarise(annual_median_Conc = median(value, na.rm = TRUE), .groups = "drop")

# Step 3: Get unique stream-variable combinations for units
raw_NP_units <- raw_NP %>%
  select(Stream_Name, variable, units) %>%
  distinct()

# Step 4: Merge annual average values with units
raw_NP_avg <- raw_NP_avg %>%
  left_join(raw_NP_units, by = c("Stream_Name", "variable"))

# Step 5: Rename sites using a lookup table and update stream names
name_conversion <- data.frame(
  Stream_Name = c("east fork", "west fork"),
  Updated_StreamName = c("East Fork", "West Fork")
)

raw_NP_avg <- raw_NP_avg %>%
  left_join(name_conversion, by = "Stream_Name") %>%
  mutate(Stream_Name = coalesce(Updated_StreamName, Stream_Name)) %>%
  select(-Updated_StreamName)

# Step 6: Simplify variable names for NOx and SRP to NOx and P
raw_NP_avg <- raw_NP_avg %>%
  mutate(solute_simplified = ifelse(variable %in% c("NOx", "NO3"), "NOx", "P"))

# Step 7: Average values for streams that switched between NO3 and NOx reporting
raw_NP_avg_final <- raw_NP_avg %>%
  group_by(Stream_Name, Year, solute_simplified) %>%
  summarise(annual_median_Conc = median(annual_median_Conc, na.rm = TRUE), .groups = "drop")

# Step 8: Ensure unique stream-solute-year combinations
raw_NP_avg_final <- raw_NP_avg_final %>%
  distinct(Stream_Name, Year, solute_simplified, .keep_all = TRUE)

# Step 9: Filter `raw_NP_avg_final` for the "P" solute
raw_P <- raw_NP_avg_final %>%
  filter(solute_simplified == "P") %>%
  select(Stream_Name, Year, annual_median_Conc) %>%
  rename(raw_P = annual_median_Conc)

# Convert Year to numeric in tot to match the type in raw_P
tot <- tot %>%
  mutate(Year = as.numeric(Year)) %>%  # Convert Year to numeric
  # Join and replace NA values in P with raw_P values where applicable
  left_join(raw_P, by = c("Stream_Name", "Year")) %>%
  mutate(P = ifelse(is.na(P), raw_P, P)) %>%  # Replace NA values in P with raw_P values
  select(-raw_P)  # Remove the temporary raw_P column

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
  dplyr::select(Stream_ID, Year, drainSqKm, ClimateZ, Name, NOx, P, precip,
         temp, Max_Daylength, num_days, max_prop_area, npp, evapotrans,
         silicate_weathering, greenup_cycle0, permafrost_mean_m, elevation_mean_m, 
         basin_slope_mean_degree, contains("Q"),
         contains("Conc"), contains("Yield"),
         contains("rock"), contains("land"))

## ------------------------------------------------------- ##
#  Export Annual Driver data ----
## ------------------------------------------------------- ##
# Check number of unique sites: should agree with avg
num_unique_sites <- tot_si %>% 
  summarise(num_sites = n_distinct(Stream_ID))

print(num_unique_sites)

write.csv(as.data.frame(tot_si), "AllDrivers_Harmonized_Yearly.csv")

gc()
