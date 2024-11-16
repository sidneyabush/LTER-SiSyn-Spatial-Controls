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
  filter(chemical == "DSi") %>% # Filter for "DSi"
  dplyr::select(-DecYear, -Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"), -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)  # Create Stream_ID after Stream_Name adjustment
  ) %>%
  group_by(Stream_ID) %>%
  summarise(
    FNConc = mean(FNConc, na.rm = TRUE),
    GenConc = mean(GenConc, na.rm = TRUE),
    FNFlux = mean(FNFlux, na.rm = TRUE),
    GenFlux = mean(GenFlux, na.rm = TRUE),
    Q = mean(Q, na.rm = TRUE),
    .groups = "drop"
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

# Perform a left join while selecting only specific columns from area
wrtds_df <- wrtds_df %>%
  left_join(area, by = "Stream_ID")

## ------------------------------------------------------- ##
# Calculate Yields ----
## ------------------------------------------------------- ##
wrtds_df <- wrtds_df %>%
  mutate(FNYield = FNFlux / drainSqKm,
         GenYield = GenFlux / drainSqKm) %>%
  dplyr::select(-FNFlux, -GenFlux)

## ------------------------------------------------------- ##
# Calculate DSi Stats ----
## ------------------------------------------------------- ##
### New below: 
# Calculate si_stats with CV columns
si_stats <- wrtds_df %>%
  group_by(Stream_ID) %>%
  summarise(
    across(
      c(FNConc, GenConc, GenYield, FNYield),
      list(mean = mean, median = median, min = min, max = max),
      .names = "{.fn}_si_{.col}"
    ),
    across(
      c(FNConc, GenConc, GenYield, FNYield),
      ~ sd(.) / mean(.),  # Calculate CV
      .names = "CV_si_{.col}"
    ),
    .groups = "drop"  # Ungroup after summarise for a clean output
  )

# Calculate q_stats with CV for Q
q_stats <- wrtds_df %>%
  group_by(Stream_ID) %>%
  summarise(
    mean_q = mean(Q),
    #med_q = median(Q),
    #min_q = min(Q),
    #max_q = max(Q),
    q_95 = quantile(Q, 0.95),
    q_5 = quantile(Q, 0.05),
    sd_q = sd(Q),  # Calculate standard deviation for Q
    CV_Q = sd(Q) / mean(Q),  # Calculate coefficient of variation for Q
    .groups = "drop"
  )

# Combine si_stats and q_stats
tot <- si_stats %>%
  left_join(q_stats, by = c("Stream_ID"))

# Combine si_stats and q_stats, then remove duplicate columns
tot <- tot %>%
  left_join(wrtds_df, by = "Stream_ID") %>%
  select(!ends_with(".y")) # Keep only non-duplicated columns

## ------------------------------------------------------- ##
# Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv")
KG$Stream_ID<-paste0(KG$LTER, "__", KG$Stream_Name)

# Combine si_stats and q_stats, handling duplicate columns with coalesce
tot <- tot %>%
  left_join(KG, by = "Stream_ID") 

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
tot <- left_join(tot, daylen_range, by = "Stream_Name")

## ------------------------------------------------------- ##
# Import Spatial Drivers ----
## ------------------------------------------------------- ##
spatial_drivers <- read.csv("all-data_si-extract_2_20240802.csv", stringsAsFactors = FALSE)
spatial_drivers$Stream_ID <- paste0(spatial_drivers$LTER, "__", spatial_drivers$Stream_Name)

## Before, using full abbrevs removed some of the spatial driver columns (e.g., "dec" in "deciduous" was causing
#  deciduous land cover to be filtered out)
months_abb <- c("_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_")

# ## Pull out the monthly drivers, plus the column that contains "Stream_ID"
# monthly_drivers <- spatial_drivers[,c(361,which(colnames(spatial_drivers) %like% months_abb))]

# Remove monthly drivers from spatial drivers
spatial_drivers <- spatial_drivers[,-c(which(colnames(spatial_drivers) %like% months_abb))]

# These are the categorical variables like "major_land", "major_soil", and "major_rock" and the numerical % of each
major_cat_vars <- which(colnames(spatial_drivers) %like% c("soil|land|rock"))
cat_vars <- spatial_drivers[,c(major_cat_vars)]

# Combine spatial drivers and categorical variables
spatial_vars <- cbind(spatial_drivers, cat_vars)

# Define the pattern to search for elevation and basin slope columns
relevant_columns <- c("elevation", "basin")

# Use grep to locate elevation and basin slope columns
elevation_basin_cols <- grep(paste(relevant_columns, collapse = "|"), colnames(spatial_drivers))

# Combine major categorical columns with elevation and basin slope
cat_vars <- spatial_drivers[, c(major_cat_vars, elevation_basin_cols)]

# Add Stream_ID back to the resulting data
cat_vars$Stream_ID <- spatial_drivers$Stream_ID

# These are the quantitative drivers
drivers_list_quant <- c("num_days", "prop_area", "precip", "evapotrans", "temp", "npp", "permafrost")

# Add in Greenup day
greenup <- c("cycle0", "cycle1")
site_mean <- list()

for (i in 1:length(drivers_list_quant)) {
  drive_cols <- grep(drivers_list_quant[i], colnames(spatial_drivers))
  one_driver <- spatial_drivers[,c(301, drive_cols)]
  site_mean[[i]] <- rowMeans(one_driver[,c(2:length(one_driver))], na.rm = TRUE)
}

mean_df <- as.data.frame(do.call(cbind, site_mean))
colnames(mean_df) <- drivers_list_quant
mean_df$Stream_ID <- spatial_drivers$Stream_ID

## Get mean greenup day
greenup_mean <- list()
stream_id <- c("Stream_ID")
for (i in 1:length(greenup)) {
  drive_cols <- grep(greenup[i], colnames(spatial_drivers))
  one_driver <- spatial_drivers[,c(301, drive_cols)]
  one_driver <- one_driver[!duplicated(one_driver$Stream_ID),]
  
  driver_melt <- melt(one_driver, id.vars=stream_id)
  
  driver_melt$doy <- yday(as.Date(driver_melt$value, "%Y-%m-%d"))
  one_driver <- dcast(driver_melt, Stream_ID~variable, value.var = "doy")
  greenup_mean[[i]] <- rowMeans(one_driver[,c(2:length(one_driver))], na.rm = TRUE)
}

green_df <- as.data.frame(do.call(cbind, greenup_mean))

colnames(green_df) <- greenup
green_df$Stream_ID <- one_driver$Stream_ID

##### THIS IS WHERE THE ISSUE IS: 

green_df <- green_df %>% drop_na(cycle0)  # Removes rows with NA values in the "cycle0" column
mean_drivers_df <- mean_df %>% drop_na()   # Removes NA rows

mean_df <- merge(mean_drivers_df, green_df, by="Stream_ID", all = TRUE)
mean_df <- merge(mean_df, cat_vars, by="Stream_ID", all=TRUE)

tot <- tot %>%
  left_join(mean_df, by = "Stream_ID") 

unique(tot$Stream_ID)

## ------------------------------------------------------- ##
# Import WRTDS N_P Conc ---- 
## ------------------------------------------------------- ##
wrtds_NP <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv")%>%
  rename(LTER = LTER.x) %>%
  filter(chemical %in% c("P", "NO3", "NOx"), GenConc > 0) %>%  # Removes NAs and zero values
  mutate(chemical = ifelse(chemical %in% c("NOx", "NO3"), "NOx", chemical)) %>%
  dplyr::mutate(
    Stream_Name = case_when(
    Stream_Name == "East Fork" ~ "east fork",
    Stream_Name == "West Fork" ~ "west fork",
    TRUE ~ Stream_Name
  ),
  Stream_ID = paste0(LTER, "__", Stream_Name)  # Create Stream_ID after Stream_Name adjustment
)

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
  left_join(wrtds_NP_avg, by = "Stream_ID")

# ## ------------------------------------------------------- ##
# Import RAW N_P Conc ---- 
# ## ------------------------------------------------------- ##
raw_NP <- read.csv("20241003_masterdata_chem.csv") %>%
  filter(variable %in% c("SRP", "PO4", "NO3", "NOx") & value > 0) %>% # 
  mutate(solute_simplified = ifelse(variable %in% c("NOx", "NO3"), "NOx", "P")) 

# Calculate stream-level averages for NOx and P concentrations
raw_NP_avg <- raw_NP %>%
  group_by(Stream_Name, solute_simplified) %>%
  summarise(
    avg_Conc = mean(value, na.rm = TRUE),  # Calculate average, ignoring NA values
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = solute_simplified, values_from = avg_Conc, names_prefix = "avg_Conc_")

# Merge `raw_NP_avg` with the `tot` dataframe
tot <- tot %>%
  left_join(raw_NP_avg, by = "Stream_Name", relationship = "many-to-many") %>%
  mutate(
    # Replace NA values in P and NOx with the corresponding averages from raw_NP_avg
    P = coalesce(P, avg_Conc_P)) %>%
  # Remove temporary columns created during the join
  select(-avg_Conc_P)

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
  tot[, .(Stream_ID, major_rock, Q, temp, drainSqKm)], 
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
weathering[, silicate_weathering := calculate_weathering_vectorized(mapped_lithology, runoff, temp_K)]

# Calculate average silicate weathering for each Stream_ID
weathering_avg <- weathering %>%
  group_by(Stream_ID) %>%
  summarise(
    avg_silicate_weathering = mean(silicate_weathering, na.rm = TRUE),
    avg_temp = mean(temp, na.rm = TRUE),
    avg_Q = mean(Q, na.rm = TRUE),
    avg_runoff = mean(runoff, na.rm = TRUE),
    .groups = "drop"
  )

# Merge averaged weathering data back into the `tot` dataframe
tot <- tot %>%
  left_join(weathering_avg, by = "Stream_ID")

# Clean up memory
gc()

# Export the resulting dataframe to a .csv file
write.csv(tot, "All_Drivers_Harmonized_Average", row.names = FALSE)



