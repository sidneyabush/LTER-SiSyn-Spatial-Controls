# #############################################################################
# Harmonize drivers for CQ Paper ----
# #############################################################################

rm(list = ls())
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr, corrplot)

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map")

# helper to clean up Stream_ID formatting
standardize_stream_id <- function(df) {
  df %>%
    mutate(
      Stream_ID = str_trim(Stream_ID),               
      Stream_ID = str_replace_all(Stream_ID, "\\s+", " ")  
    )
}

# #############################################################################
# 1. Read in & tidy sizer results (from Lienne) ----
# #############################################################################
sizer_outs <- read.csv("sizer_outs_DSi_15May24.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name),
    Date = as.Date(lubridate::parse_date_time(Date, orders = c("Ymd","ymd","dmy","mdy","Y-m-d","m/d/Y","d/m/Y"))),
    Year = lubridate::year(Date),
    Stream_ID = paste0(LTER, "__", Stream_Name)) 

# Merge drainage area from the site reference table so sizer_outs has `drainSqKm` column
site_ref <- read.csv('/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map/Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv', stringsAsFactors = FALSE) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                      ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  ) %>%
  dplyr::select(Stream_ID, drainSqKm, Latitude, Longitude)

sizer_outs <- sizer_outs %>%
  left_join(site_ref, by = "Stream_ID") %>%
  dplyr::select(LTER, Stream_Name, Stream_ID, Date, Year, drainSqKm, dplyr::everything())

# #############################################################################
# 2. Discharge Metrics: Calculate Flashiness (RBI) and Recession-curve Slope ----
# #############################################################################
# This is calculated using WRTDS data because we need a daily record of discharge
cols_needed <- c("LTER.x", "Stream_Name", "Date", "Q")
daily_kalman <- read_csv("Full_Results_WRTDS_kalman_daily_filtered.csv", 
                         col_select = all_of(cols_needed)) %>%
  rename(LTER = LTER.x) %>%
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    Date = as.Date(Date),
    Year = lubridate::year(Date),
    Stream_ID = paste0(LTER, "__", Stream_Name))

# We needed to grab Catalina Jemez sites separately because they were not 
# run in the original WRTDS, but we had not yet fully run the next round 
# of WRTDS where this issue was fixed

daily_Q_CJ <- read.csv("WRTDS-input_discharge.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(
    Date        = as.Date(Date, format = "%Y-%m-%d"),
    Year        = lubridate::year(Date),
    LTER        = "Catalina Jemez",
    Stream_Name = str_extract(Stream_ID, "OR_low|MG_WEIR")
  ) %>%
  filter(Stream_ID %in% c("Catalina Jemez__OR_low", "Catalina Jemez__MG_WEIR")) %>%
  select(-indicate)

# Combine
daily_kalman <- bind_rows(
  daily_kalman,
  daily_Q_CJ
) %>%
  arrange(Stream_ID, Date)  

# Calculate Daily Differences and Identify Recession Days
Q_diff <- daily_kalman %>%
  dplyr::arrange(Stream_ID, Date) %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::mutate(
    dQ = Q - lag(Q),
    change_dQ = Q / lag(Q),
    dQ_dt = dQ / as.numeric(Date - lag(Date))) %>%
  dplyr::filter(!is.na(dQ_dt)) %>% # Remove NA values (first row)
  dplyr::filter(!change_dQ < 0.7) 

# Calculate the recession slope (-dQ/dt)
recession_data <- Q_diff %>%
  dplyr::filter(dQ < 0) %>%  # Keep only recession periods
  dplyr::mutate(recession_slope = -dQ_dt)  # Make it positive for the slope

# Compute Aggregate Recession Slope per Stream
# For each stream, if there are at least 50 recession days, fit a linear model (recession_slope ~ Q)
# and extract the slope coefficient.
recession_slopes <- recession_data %>%
  dplyr::group_by(Stream_ID) %>%
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  dplyr::summarise(
    n_days = n(),
    recession_slope = if(n_days >= 50) {
      lm_model <- lm(log(recession_slope) ~ log(Q), data = cur_data())
      unname(coef(lm_model)[2])
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(recession_slope), recession_slope >= 0)

# Flashiness Index (RBI)
# For each stream, calculate daily discharge changes and compute RBI.
flashiness <- daily_kalman %>%
  group_by(Stream_ID) %>%
  arrange(Date) %>%                     # Ensure dates are in order for each stream
  mutate(dQ = Q - lag(Q),                # Daily change in discharge
         abs_dQ = abs(dQ)) %>%           # Absolute change in discharge
  filter(!is.na(abs_dQ)) %>%             # Remove NA from the first row (due to lag)
  summarise(
    total_discharge = sum(Q, na.rm = TRUE),         # Total discharge over the period
    total_change = sum(abs_dQ, na.rm = TRUE),         # Total absolute change
    RBI = total_change / total_discharge           # Richards-Baker Flashiness Index
  ) %>%
  dplyr::select(-total_discharge, -total_change) %>% 
  ungroup()

# Merge both metrics
Q_metrics <- left_join(recession_slopes, flashiness, by ="Stream_ID") %>%
  dplyr::select(-n_days)

# #############################################################################
# 3. Add Köppen–Geiger Classification ----
# #############################################################################
KG <- read.csv("Koeppen_Geiger_2.csv") %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)) %>%
  dplyr::rename(
    KG_Class = Name) %>%
  dplyr::select(-X, -Use_WRTDS, -rndCoord.lon, -rndCoord.lat, -Latitude, -Longitude)

Q_KG <- left_join(Q_metrics, KG, by="Stream_ID") %>%
  distinct(Stream_ID,.keep_all=TRUE) %>%
  dplyr::select(-contains(".x")) %>%
  rename_with(~str_remove(., "\\.y$"))

Q_KG <- Q_KG %>%
  dplyr::select(LTER, Stream_Name, Stream_ID, dplyr::everything())

# #############################################################################
# 4. Spatial Drivers + Basin Slope Gap-Fill
# #############################################################################
# a) read & tidy raw spatial‐drivers - from Lyon et al., spatial drivers output
si_drivers <- read.csv("all-data_si-extract_2_20250325.csv",
                       stringsAsFactors = FALSE) %>%
  dplyr::select(-contains("cycle1")) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  ) %>%
  dplyr::select(-contains(".y"), -contains(".x"))

# clean up Stream_ID text
si_drivers <- standardize_stream_id(si_drivers)

# b) convert any green-up dates to day-of-year
gcols <- grep("greenup_", names(si_drivers), value = TRUE)
si_drivers[gcols] <- lapply(si_drivers[gcols], function(x) {
  as.numeric(format(as.Date(x, "%Y-%m-%d"), "%j"))
})

# c) zero-fill all permafrost & prop_area
pcols <- grep("permafrost|prop_area", names(si_drivers), value = TRUE)
si_drivers[pcols] <- lapply(si_drivers[pcols], function(x) {
  x <- as.numeric(x); x[is.na(x)] <- 0; x
})

# d) split out the annual vars vs the purely character vars
months_regex <- "_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_"
annual_block <- si_drivers %>% 
  dplyr::select(-matches(months_regex))

char_block   <- annual_block %>% 
  dplyr::select(Stream_ID, matches("elevation|rock|land|soil|permafrost|slope"))

annual_vars  <- annual_block %>%
  dplyr::select(-matches("elevation|rock|land|soil|permafrost")) %>%
  dplyr::select(-LTER, -Shapefile_Name, -Stream_Name, -Discharge_File_Name)

# e) melt the annual numbers and tag them
melted <- reshape2::melt(annual_vars,
                         id.vars         = "Stream_ID",
                         variable.factor = FALSE)

# immediately coerce 'variable' to character
melted$variable <- as.character(melted$variable)

vars_ann <- c("num_days", "prop_area", "evapotrans",
              "precip", "temp", "cycle0", "cycle1", "npp")

units_df <- data.frame(
  driver = vars_ann,
  units  = c("days", "prop_watershed", "kg_m2",
             "mm_day", "deg_C", "MMDD",
             "MMDD", "kgC_m2_year"),
  stringsAsFactors = FALSE
)

# tag driver
melted$driver <- NA_character_

for (v in vars_ann) {
  melted$driver[grepl(v, melted$variable)] <- v
}

# extract year from variable name
melted$year <- ifelse(
  grepl("MMDD$", melted$variable),
  as.integer(str_extract(melted$variable, "(?<=_)[0-9]{4}(?=MMDD$)")),
  as.integer(sapply(strsplit(melted$variable, "_"), `[`, 2))
)

# merge in units, drop the old variable name
melted <- merge(melted, units_df, by = "driver")
melted <- melted %>% 
  dplyr::select(-variable, -units)

# f) pivot back to wide
drivers_cast <- melted %>%
  distinct(Stream_ID, year, driver, value) %>%
  pivot_wider(names_from = driver, values_from = value) %>%
  rename(Year = year)

# g) re‐attach the character columns
all_spatial <- drivers_cast %>%
  left_join(char_block, by = "Stream_ID", relationship = "many-to-many") %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

# h) Gap-fill basin_slope_mean_degree from US & Krycklan sources
# (a) Ensure all_spatial has the slope column
if (!"basin_slope_mean_degree" %in% names(all_spatial)) {
  all_spatial$basin_slope_mean_degree <- NA_real_
}

# (b) Read and process Krycklan slopes
Krycklan_slopes <- read.csv("Krycklan_basin_slopes.csv") %>%
  transform(basin_slope_mean_degree = atan(gradient_pct/100) * (180/pi)) %>%
  rename(Stream_Name = Stream_Name)  

# (c) Read and process US slopes (wide → long)
US_slopes <- read.csv("DSi_Basin_Slope_missing_sites.csv", header = FALSE)
colnames(US_slopes) <- US_slopes[1, ]
US_slopes <- US_slopes[-1, ] %>%
  pivot_longer(
    cols        = everything(),
    names_to    = "Stream_Name",
    values_to   = "basin_slope_mean_degree"
  ) %>%
  mutate(basin_slope_mean_degree = as.numeric(basin_slope_mean_degree))

# (d) Load key to map Stream_Name → Stream_ID
stream_key <- read.csv("basin_stream_id_conversions.csv", header = TRUE)

Krycklan_slopes <- Krycklan_slopes %>%
  left_join(stream_key, by = "Stream_Name") %>%
  filter(!is.na(basin_slope_mean_degree))

US_slopes <- US_slopes %>%
  left_join(stream_key, by = "Stream_Name") %>%
  filter(!is.na(basin_slope_mean_degree))

# (e) Build the small fill‐in table
setDT(all_spatial)

to_fill <- all_spatial[is.na(basin_slope_mean_degree), .(Stream_ID)]

fill_df <- to_fill %>%
  left_join(US_slopes    %>% dplyr::select(Stream_ID, basin_slope_mean_degree),
            by = "Stream_ID") %>%
  left_join(Krycklan_slopes %>% dplyr::select(Stream_ID, basin_slope_mean_degree),
            by = "Stream_ID",
            suffix = c("_US", "_KR")) %>%
  mutate(
    basin_slope_mean_degree = coalesce(basin_slope_mean_degree_US,
                                       basin_slope_mean_degree_KR)
  ) %>%
  dplyr::select(Stream_ID, basin_slope_mean_degree)

# (f) Manual corrections
fill_df[Stream_ID=="Walker Branch__east fork",  basin_slope_mean_degree:=2.2124321596241265]
fill_df[Stream_ID=="Walker Branch__west fork", basin_slope_mean_degree:=1.8972192246291828]

# (g) Join back into all_spatial
setkey(all_spatial, Stream_ID)
setkey(fill_df,Stream_ID)

all_spatial[fill_df, basin_slope_mean_degree := i.basin_slope_mean_degree]

# Tidy all_spatial 
all_spatial <- all_spatial %>%
  dplyr::select(-elevation_median_m, -elevation_min_m, -elevation_max_m, 
                -basin_slope_median_degree, -basin_slope_min_degree, -basin_slope_max_degree,
                -permafrost_median_m, - permafrost_min_m, -permafrost_max_m, -num_days) %>%
  dplyr::rename(
    snow_cover = prop_area, 
    greenup_day = cycle0,
    elevation = elevation_mean_m,
    permafrost = permafrost_mean_m,             
    basin_slope = basin_slope_mean_degree)

# #############################################################################
# 5) Merge Spatial Data with Q Metrics and KG Class (variable name "Q_KG")
# #############################################################################
all_drivers <- left_join(all_spatial, Q_KG, by = "Stream_ID") %>%
  # extract LTER and Stream_Name from Stream_ID as fallback
  mutate(
    .LTER_from_id = ifelse(grepl("__", Stream_ID), sub("__.*$", "", Stream_ID), NA_character_),
    .Stream_from_id = ifelse(grepl("__", Stream_ID), sub("^.*__", "", Stream_ID), NA_character_)
  ) %>%
  # set LTER and Stream_Name from parsed Stream_ID (keep Stream_ID as well)
  mutate(
    LTER = .LTER_from_id,
    Stream_Name = .Stream_from_id) %>%
  dplyr::select(-.LTER_from_id, -.Stream_from_id)

# Tidy column order
all_drivers <- all_drivers %>%
  dplyr::select(LTER, Stream_Name, Stream_ID, Year, ClimateZ, KG_Class, major_land, 
                major_rock, major_soil, recession_slope, RBI, dplyr::everything())

# Ensure columns that begin with land_/soil_/rocks_ use numeric 0 where values are NA
lsr_cols <- grep('^(land_|soil_|rocks_)', names(all_drivers), value = TRUE)
all_drivers <- all_drivers %>%
  dplyr::mutate(dplyr::across(dplyr::any_of(lsr_cols), ~ {
    tmp <- as.character(.);
    tmp[is.na(tmp) | tmp == "NA"] <- "0";
    as.numeric(tmp)
  }))

# #############################################################################
# 6) Calculate site averages for numerical driver data
# #############################################################################
# Calculate site summaries
# For land/soil/rocks: use the constant value (not averaged)
# For other numeric columns: calculate mean across years
num_cols <- names(all_drivers)[sapply(all_drivers, is.numeric) & names(all_drivers) != "Year"]
other_num_cols <- setdiff(num_cols, lsr_cols)

site_summary <- all_drivers %>%
  group_by(Stream_ID) %>%
  summarise(
    across(
      all_of(num_cols),
      ~ if (cur_column() %in% lsr_cols) {
        # Land/soil/rocks: take first non-NA (they should all be the same)
        v <- na.omit(.x)
        if(length(v) == 0) NA_real_ else v[1]
      } else {
        # Other numeric: calculate mean
        if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)
      }
    ),
    .groups = "drop"
  )

# Merge back with non-numeric columns (one row per Stream_ID)
non_numeric_cols <- all_drivers %>%
  dplyr::select(Stream_ID, LTER, Stream_Name, where(~ !is.numeric(.x))) %>%
  distinct(Stream_ID, .keep_all = TRUE)

site_summary <- site_summary %>%
  left_join(non_numeric_cols, by = "Stream_ID") %>%
  dplyr::select(LTER, Stream_Name, Stream_ID, ClimateZ, KG_Class, major_land,
                major_rock, major_soil, recession_slope, RBI, dplyr::everything()) %>%
  dplyr::select(-matches("\\.x$|\\.y$"))

# #############################################################################
# 7) Merge the site average driver data with sizer outs
# #############################################################################
# Filter site_summary to only Stream_IDs in sizer_outs and remove duplicate columns
sizer_stream_ids <- unique(sizer_outs$Stream_ID)

# Only remove specific overlapping columns that should come from sizer_outs
# Keep driver columns from site_summary (major_rock, major_land, major_soil, climate data, etc.)
cols_to_remove <- c("LTER", "Stream_Name", "Date", "Year")

# Prepare site_summary_filtered: remove only the specified overlapping columns
site_summary_filtered <- site_summary %>%
  filter(Stream_ID %in% sizer_stream_ids) %>%
  dplyr::select(-any_of(cols_to_remove))

sizer_outs_ave_drivers <- sizer_outs %>%
  left_join(site_summary_filtered, by = "Stream_ID") %>%
  dplyr::rename(
    drainage_area = drainSqKm) 

# Set major_land to "Ice" for MCM sites ******** This can be manually updated for more sites later
sizer_outs_ave_drivers <- sizer_outs_ave_drivers %>%
  mutate(major_land = ifelse(LTER == "MCM", "Ice", major_land)) %>%
  filter(!is.na(major_rock) & trimws(major_rock) != "" & major_rock != "0")

# Integrity check: Verify non-numeric columns match original all_spatial
# Get original non-numeric columns from all_spatial (one row per Stream_ID)
spatial_check <- all_spatial %>%
  dplyr::select(Stream_ID, major_land, major_rock, major_soil) %>%
  distinct(Stream_ID, .keep_all = TRUE)

# Compare with site_summary
site_summary_check <- site_summary %>%
  dplyr::select(Stream_ID, major_land, major_rock, major_soil)

# Find mismatches
comparison <- site_summary_check %>%
  left_join(spatial_check, by = "Stream_ID", suffix = c("_site", "_orig"))

non_numeric_issues <- comparison %>%
  filter(
    (major_land_site != major_land_orig & !(is.na(major_land_site) & is.na(major_land_orig))) |
    (major_rock_site != major_rock_orig & !(is.na(major_rock_site) & is.na(major_rock_orig))) |
    (major_soil_site != major_soil_orig & !(is.na(major_soil_site) & is.na(major_soil_orig)))
  )

if (nrow(non_numeric_issues) > 0) {
  warning(paste0("Found ", nrow(non_numeric_issues), " mismatches in major_land/rock/soil columns. Writing 'non_numeric_mismatches.csv'"))
  write.csv(non_numeric_issues, "non_numeric_mismatches.csv", row.names = FALSE)
} else {
  message("Non-numeric column integrity check passed: major_land/rock/soil match original all_spatial.")
}

# Last step, change the major_land classes to simple ones: 
sizer_outs_ave_drivers <- sizer_outs_ave_drivers %>%
  dplyr::mutate(
    major_land = dplyr::case_when(
      grepl("_forest", tolower(major_land)) ~ "Forest",
      tolower(major_land) == "tundra" ~ "Tundra",
      tolower(major_land) == "shrubland_grassland" ~ "Grassland_Shrubland",
      tolower(major_land) == "cropland" ~ "Cropland",
      tolower(major_land) == "wetland_marsh" ~ "Wetland_Marsh",
      tolower(major_land) == "urban_and_built_up_land" ~ "Impervious",
      TRUE ~ major_land
    )
  )

# Print unique major land: 
unique(sizer_outs_ave_drivers$major_land)

# And sites: 
unique(sizer_outs_ave_drivers$Stream_ID)

# n = 446

# #############################################################################
# 8) Export
# #############################################################################
# Tidy data for export: 
sizer_outs_ave_drivers <- sizer_outs_ave_drivers %>%
  dplyr::mutate(
    permafrost = 100 * pmin(pmax(replace_na(permafrost, 0), 0), 1),  # now in %
    snow_cover = replace_na(snow_cover, 0)) %>%
  mutate(across(where(is.character), ~ na_if(., "")))

# Write out. This is for the FULL dataset (not partitioned at all)
write.csv(
  sizer_outs_ave_drivers,
  "sizer_outs_ave_drivers_harmonized.csv",
  row.names = FALSE
)

#---- End of Script ----
