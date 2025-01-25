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
  filter(FNConc >= 0.5 * GenConc & FNConc <= 1.5 * GenConc) %>%
  dplyr::select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"), 
                -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Year = floor(as.numeric(DecYear))) %>%
  dplyr:: filter(Year >= 2001 & Year <= 2024) %>%  # Filter rows with dates between 2001 and 2024
  filter(chemical == "DSi") 

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
yields <- wrtds_df %>%
  mutate(FNYield = FNFlux / drainSqKm,
         GenYield = GenFlux / drainSqKm) %>%
  dplyr::select(-FNFlux, -GenFlux)

tot <- wrtds_df %>%
  left_join(yields, by = c("Stream_ID", "Year")) %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

gc()

## ------------------------------------------------------- ##
            # Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv")
KG$Stream_ID<-paste0(KG$LTER, "__", KG$Stream_Name)

tot <- tot %>%
  left_join(KG, by = "Stream_ID") %>%
  distinct(Stream_ID, .keep_all = TRUE)

num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

## ------------------------------------------------------- ##
              # Import Daylength ----
## ------------------------------------------------------- ##
# Load and clean daylength data
daylen <- read.csv("Monthly_Daylength_2.csv") %>%
  dplyr::select(-1)

num_unique_stream_ids <- daylen %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids)

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
tot <- tot %>% 
  left_join(daylen_range, by = "Stream_Name") %>%
  distinct(Stream_ID, .keep_all = TRUE)

num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

## ------------------------------------------------------- ##
# Spatial Drivers Processing----
## ------------------------------------------------------- ##

# Rename specific Stream_IDs
name_conversion <- data.frame(
  Stream_ID = c("Walker Branch__East Fork", "Walker Branch__West Fork"),
  Updated_StreamName = c("Walker Branch__east fork", "Walker Branch__west fork")
)

# Load and preprocess spatial drivers
si_drivers <- read.csv("all-data_si-extract_2_202412.csv", stringsAsFactors = FALSE) %>%
  select(-contains("soil")) %>%
  mutate(Stream_ID = paste0(LTER, "__", Stream_Name)) %>%
  left_join(name_conversion, by = "Stream_ID") %>%
  mutate(Stream_ID = coalesce(Updated_StreamName, Stream_ID)) %>%
  select(-Updated_StreamName)

# Standardize Finnish site names
for (i in seq_len(nrow(finn))) {
  site_id <- finn[i, 3]
  row_num <- which(si_drivers$Stream_ID == site_id)
  si_drivers[row_num, "Stream_ID"] <- finn[i, 4]
}

# Cleanup specific Finnish Stream_IDs
si_drivers <- si_drivers %>%
  mutate(
    Stream_ID = case_when(
      Stream_ID == "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310  " ~ "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310",
      Stream_ID == "Finnish Environmental Institute__SIMOJOKI AS. 13500      " ~ "Finnish Environmental Institute__SIMOJOKI AS. 13500",
      TRUE ~ Stream_ID
    )
  ) %>%
  filter(!grepl("^Site", Stream_Name)) %>%
  distinct(Shapefile_Name, .keep_all = TRUE)

# Replace NA values in permafrost columns with 0
permafrost_cols <- grep("permafrost", colnames(si_drivers), value = TRUE)
si_drivers[, permafrost_cols] <- lapply(si_drivers[, permafrost_cols], function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- 0
  return(x)
})

# Remove monthly columns
months <- c("_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_")
months_cols <- si_drivers %>%
  select(matches(months))
si_drivers <- si_drivers %>%
  select(-one_of(colnames(months_cols)))

## ------------------------------------------------------- ##
# Parse and Process Annual Data----
## ------------------------------------------------------- ##

# Define annual variables and their units
annual_vars <- c("num_days", "prop_area", "evapotrans", "precip", "temp", "cycle0", "cycle1", "npp")
units_annual <- c("days", "prop_watershed", "kg_m2", "mm_day", "deg_C", "MMDD", "MMDD", "kgC_m2_year")

# Check matched columns
matched_columns <- colnames(si_drivers)[grepl(paste(annual_vars, collapse = "|"), colnames(si_drivers))]
if (!"Stream_Name" %in% colnames(si_drivers)) {
  stop("Stream_Name column is missing from si_drivers!")
}

# Separate numeric columns, explicitly keeping Stream_Name
numeric_data <- si_drivers %>%
  dplyr::select(Stream_Name, all_of(matched_columns)) %>%
  dplyr::select(where(is.numeric))

numeric_data <- numeric_data %>%
  mutate(Stream_Name = si_drivers$Stream_Name)

# Pivot numeric columns
numeric_long <- numeric_data %>%
  pivot_longer(
    cols = -Stream_Name,  # Exclude Stream_Name from pivoting
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    # Extract driver and year from variable names
    driver = str_extract(variable, paste(annual_vars, collapse = "|")),
    year = str_extract(variable, "\\d{4}") %>% as.numeric()
  ) %>%
  drop_na(driver, year)  # Remove rows with missing driver or year

# Convert value column in numeric_long to character
numeric_long <- numeric_long %>%
  mutate(value = as.character(value))

character_data <- si_drivers %>%
  dplyr::select(Stream_Name, all_of(matched_columns)) %>%
  dplyr::select(where(is.character))

character_data <- character_data %>%
  mutate(Stream_Name = si_drivers$Stream_Name)

# Pivot character columns
character_long <- character_data %>%
  pivot_longer(
    cols = -Stream_Name,  # Exclude Stream_Name from pivoting
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    # Extract driver and year from variable names
    driver = str_extract(variable, paste(annual_vars, collapse = "|")),
    year = str_extract(variable, "\\d{4}") %>% as.numeric()
  ) %>%
  drop_na(driver, year)  # Remove rows with missing driver or year

# Combine numeric and character data
year_cols <- bind_rows(numeric_long, character_long)

# Verify the combined data
print("Combined data:")
print(head(year_cols))

# Add units to the drivers
units_df <- data.frame(driver = annual_vars, unit = units_annual)
year_cols <- year_cols %>%
  left_join(units_df, by = "driver")

## ------------------------------------------------------- ##
        # Merge Processed Data ----
## ------------------------------------------------------- ##

# Combine annual data with other variables
character_vars <- c("elevation", "rock", "land", "slope", "permafrost")
character_cols_full <- si_drivers %>%
  select(matches(paste(character_vars, collapse = "|")), Stream_Name)

# Pivot wider for numeric annual data
drivers <- year_cols %>%
  pivot_wider(names_from = driver, values_from = value) %>%
  left_join(character_cols_full, by = "Stream_Name")

# Convert greenup day to Julian day
drivers <- drivers %>%
  mutate(cycle0 = as.Date(cycle0, origin = "1970-01-01") %>% format("%j")) %>%
  rename(Year = year)

# Merge with `tot` and `wrtds_df`
tot <- tot %>%
  left_join(drivers, by = c("Stream_Name", "Year")) %>%
  left_join(wrtds_df, by = c("Stream_Name", "Year")) %>%
  rename(Stream_ID = Stream_ID.y)

num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)



## ------------------------------------------------------- ##
            # Import WRTDS N_P Conc ---- 
## ------------------------------------------------------- ##
wrtds_NP <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  rename(LTER = LTER.x) %>%
  filter(FNConc >= 0.5 * GenConc & FNConc <= 1.5 * GenConc) %>%
  filter(chemical %in% c("P", "NO3", "NOx"), GenConc > 0) %>%  # Removes NAs and zero values
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Year = floor(as.numeric(DecYear))) %>%
  dplyr:: filter(Year >= 2001 & Year <= 2024)  # Filter rows with dates between 2001 and 2024

## Need to tidy the Finnish site names:
finn <- read.csv("FinnishSites.csv")

finn$Stream_ID <- paste0("Finnish Environmental Institute__", finn$Site.ID)
finn$Stream_ID2 <- paste0("Finnish Environmental Institute__", finn$Site)

for (i in 1:nrow(finn)) {
  site_id<-finn[i,3]
  row_num<-which(wrtds_NP$Stream_ID==site_id)
  wrtds_NP[row_num, "Stream_ID"]<-finn[i,4]
}

wrtds_NP$Stream_ID <- ifelse(wrtds_NP$Stream_ID=="Finnish Environmental Institute__TORNIONJ KUKKOLA 14310  ",
                           "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310", wrtds_NP$Stream_ID)

wrtds_NP$Stream_ID <- ifelse(wrtds_NP$Stream_ID=="Finnish Environmental Institute__SIMOJOKI AS. 13500      ",
                           "Finnish Environmental Institute__SIMOJOKI AS. 13500", wrtds_NP$Stream_ID)
  
gc()

# Summarize to get the average GenConc by Stream_ID, and simplified chemical
wrtds_NP_avg <- wrtds_NP %>%
  group_by(Stream_ID, chemical) %>%
  summarise(
    avg_Conc = mean(GenConc, na.rm = TRUE),  # Calculate average, ignoring NA values
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = chemical, values_from = avg_Conc)

gc()

# Merge with the "tot" dataframe to add average NOx and P data in a single row per Stream_ID
tot <- tot %>%
  left_join(wrtds_NP_avg, by = "Stream_ID") %>%
  distinct(Stream_ID, .keep_all = TRUE)

## Validate results: Check rows still containing NA values for P or NOx
remaining_na_wrtdsNP <- tot %>%
  filter(is.na(P) | is.na(NOx)) %>%
  select(Stream_ID, P, NOx)  # Keep only necessary columns


# ## ------------------------------------------------------- ##
              # Import RAW N_P Conc ---- 
# ## ------------------------------------------------------- ##
raw_NP <- read.csv("20241003_masterdata_chem.csv") %>%
  filter(variable %in% c("SRP", "PO4", "NO3", "NOx") & value > 0) %>% # 
  mutate(solute_simplified = ifelse(variable %in% c("NOx", "NO3"), "NOx", "P")) 

## Need to tidy the Finnish site names:
finn <- read.csv("FinnishSites.csv")

finn$Stream_ID <- paste0("Finnish Environmental Institute__", finn$Site.ID)
finn$Stream_ID2 <- paste0("Finnish Environmental Institute__", finn$Site)

for (i in 1:nrow(finn)) {
  site_id<-finn[i,3]
  row_num<-which(raw_NP$Stream_ID==site_id)
  raw_NP[row_num, "Stream_ID"]<-finn[i,4]
}

raw_NP$Stream_ID <- ifelse(raw_NP$Stream_ID=="Finnish Environmental Institute__TORNIONJ KUKKOLA 14310  ",
                             "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310", raw_NP$Stream_ID)

raw_NP$Stream_ID <- ifelse(raw_NP$Stream_ID=="Finnish Environmental Institute__SIMOJOKI AS. 13500      ",
                             "Finnish Environmental Institute__SIMOJOKI AS. 13500", raw_NP$Stream_ID)


# Calculate stream-level averages for NOx and P concentrations
raw_NP_avg <- raw_NP %>%
  group_by(Stream_Name, solute_simplified) %>%
  summarise(
    avg_Conc = mean(value, na.rm = TRUE),  # Calculate average, ignoring NA values
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = solute_simplified, values_from = avg_Conc, names_prefix = "avg_Conc_")

# Merge raw_NP_avg with tot
tot <- tot %>%
  left_join(raw_NP_avg, by = "Stream_Name", relationship = "many-to-many") %>%
  mutate(
    # Replace NA values in P and NOx with corresponding averages from raw_NP_avg
    P = coalesce(P, avg_Conc_P), 
    NOx = coalesce(NOx, avg_Conc_NOx)
  ) %>%
  # Remove temporary columns created during the join
  select(-avg_Conc_P, -avg_Conc_NOx) %>%
  distinct(Stream_ID, .keep_all = TRUE)

# Validate results: Check rows still containing NA values for P or NOx
remaining_na_rawNP <- tot %>%
  filter(is.na(P) | is.na(NOx)) %>%
  select(Stream_ID, P, NOx)  # Keep only necessary columns

num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

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

# Filter tot for rows with NA basin slope values
na_slopes_post <- tot %>%
  filter(is.na(basin_slope_mean_degree)) %>%
  select(Stream_ID)

# Now Gap Fill Elevation ----
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
  select(-elevation_mean_m_filled) %>%
  distinct(Stream_ID, .keep_all = TRUE)

# Filter tot for rows with NA basin slope values
tot_with_na_slope_post <- tot %>%
  filter(is.na(basin_slope_mean_degree)) %>%
  select(Stream_ID)


# ------------------------------------------------------- #
# Tidy-up Full Dataset ----
# ------------------------------------------------------- #
tot <- tot %>%
  rename_with(~ str_replace(., "\\.x$", ""), ends_with(".x"))%>%  # Remove `.x` from column names
  filter(FNYield <= 100) %>%  
  select(-ends_with(".y"), -Use_WRTDS, -X, Stream_Name, -Latitude, -num_days,
         -Longitude, -LTER, -contains("coord"), -major_land, 
         -contains("median_degree"), -contains("min_degree"),
         -contains("max_degree"), -contains("median_m"), -contains("min_m"), -contains("max_m"))  # Remove columns with `.y` in their names
  
# ------------------------------------------------------- #
      # Silicate Weathering: Stream-Level Averages
# ------------------------------------------------------- #
# Read and prepare data
mapped_lithologies <- fread("mapped_lithologies.csv")

# Ensure `tot` is a data.table
if (!is.data.table(tot)) {
  setDT(tot)  # Convert to data.table
}

# Ensure compatibility in the `major_rock` column for merging
tot[, major_rock := as.character(major_rock)]


# Ensure compatibility in the `major_rock` column for merging
tot[, major_rock := as.character(major_rock)]
mapped_lithologies[, major_rock := as.character(major_rock)]

# Merge and filter out rows with NA in `mapped_lithology`
weathering <- merge(
  tot[, .(Stream_ID, major_rock, mean_q, temp, drainSqKm)], 
  mapped_lithologies[, .(major_rock, mapped_lithology)], 
  by = "major_rock", all.x = TRUE
)
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

# Calculate runoff based on the formula
weathering[, runoff := (mean_q * seconds_per_year * kg_per_m3) / (drainSqKm * km2_to_m2)]

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
weathering[, silicate_weathering := calculate_weathering_vectorized(mapped_lithology, runoff, temp_K)]

# Calculate average silicate weathering for each Stream_ID
weathering_avg <- weathering %>%
  group_by(Stream_ID) %>%
  summarise(
    silicate_weathering = mean(silicate_weathering, na.rm = TRUE),
    .groups = "drop"
  )

# Merge averaged weathering data back into the `tot` dataframe
tot <- tot %>%
  left_join(weathering_avg, by = "Stream_ID") %>%
  select(-major_rock, -Stream_Name, -min_daylength, -ClimateZ, -Name, -mean_q) %>%
  rename(snow_cover = prop_area, 
         greenup_day = greenup_mean,
       drainage_area = drainSqKm,
       elevation = elevation_mean_m,
       basin_slope = basin_slope_mean_degree) %>%
  distinct(Stream_ID, .keep_all = TRUE) 

# Export Stream_IDs with NA values in "permafrost" or "snow_cover"
na_stream_ids <- tot %>%
  filter(is.na(permafrost_mean_m) | is.na(snow_cover)) %>%
  select(Stream_ID, permafrost_mean_m, snow_cover)  # Include only relevant columns for clarity

write_csv(na_stream_ids, "na_permafrost_snow_cover_stream_ids.csv")

# Replace NA values in the specified column range with 0
drivers_df <- tot %>%
  # Replace NA values in the "permafrost" column with 0
  mutate(permafrost_mean_m = replace(permafrost_mean_m, is.na(permafrost_mean_m), 0), 
         snow_cover = replace(snow_cover, is.na(snow_cover), 0))

## ------------------------------------------------------- ##
# Calculate Stats ----
## ------------------------------------------------------- ##
# Calculate si_stats without CV columns
# This is different from the Yearly workflow since we're taking an average per site
si_stats <- tot %>%
  group_by(Stream_ID) %>%
  summarise(
    across(
      c(FNConc, GenConc, FNFlux, GenFlux),
      list(median = median),
      .names = "{.fn}_{.col}"
    ),
    .groups = "drop"  # Ungroup after summarise for a clean output
  )


# Calculate q_stats with CV for Q
q_stats <- tot %>%
  group_by(Stream_ID) %>%
  summarise(
    mean_q = mean(Q),
    sd_q = sd(Q),  # Calculate standard deviation for Q
    CV_Q = sd(Q) / mean(Q),  # Calculate coefficient of variation for Q
    q_95 = quantile(Q, 0.95),
    q_5 = quantile(Q, 0.05),
    .groups = "drop"
  )

# Combine si_stats and q_stats
tot <- si_stats %>%
  left_join(q_stats, by = c("Stream_ID")) %>%
  select(-sd_q) %>%
  distinct(Stream_ID, .keep_all = TRUE)

# Export the resulting dataframe to a .csv file
write.csv(drivers_df, "AllDrivers_Harmonized_Average.csv", row.names = FALSE)

