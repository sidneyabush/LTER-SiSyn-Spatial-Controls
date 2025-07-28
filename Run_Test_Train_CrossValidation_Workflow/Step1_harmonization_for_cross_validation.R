# =============================================================================
# Load libraries and clear environment
# =============================================================================
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr, corrplot)
rm(list = ls())

# Define record length
record_length <- 5

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

# helper to clean up Stream_ID formatting
standardize_stream_id <- function(df) {
  df %>%
    mutate(
      Stream_ID = str_trim(Stream_ID),               # remove leading/trailing spaces
      Stream_ID = str_replace_all(Stream_ID, "\\s+", " ")  # collapse multiple spaces
    )
}

# -----------------------------------------------------------------------------
# 1) Read in & tidy WRTDS DSi results
# -----------------------------------------------------------------------------
wrtds_df <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv", stringsAsFactors = FALSE) %>%
  rename(LTER = LTER.x) %>%
  filter(chemical == "DSi", !if_any(where(is.numeric), ~ . %in% c(Inf, -Inf))) %>%
  dplyr::select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y,
         -contains("date"), -contains("month"), -min_year, -max_year, -duration) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Year      = floor(as.numeric(DecYear))) %>%
  dplyr::filter(Year >= 2002, Year <= 2022)


wrtds_CJ <- read.csv("wrtds_kalman_annual_CatalinaJemez.csv", stringsAsFactors = FALSE) %>%
  filter(chemical == "DSi", !if_any(where(is.numeric), ~ . %in% c(Inf, -Inf))) %>%
  # harmonize naming
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Year      = floor(as.numeric(DecYear))
  ) %>%
  dplyr::select(-DecYear) %>%
  dplyr::filter(Year >= 2002, Year <= 2022)


# combine
wrtds_df <- bind_rows(wrtds_df, wrtds_CJ)

# only keep sites with at least `record_length` years of data
site_year_counts <- wrtds_df %>%
  group_by(Stream_ID) %>%
  summarise(year_count = n_distinct(Year), .groups="drop") %>%
  filter(year_count >= record_length)

wrtds_df <- wrtds_df %>%
  filter(Stream_ID %in% site_year_counts$Stream_ID)

# =============================================================================
# 2) Calculate Yields
# =============================================================================
yields <- wrtds_df %>%
  mutate(
    FNYield  = (FNFlux  * 365) / drainSqKm,
    GenYield = (GenFlux * 365) / drainSqKm
  ) %>%
  dplyr::select(-FNFlux, -GenFlux)

tot <- wrtds_df %>%
  left_join(yields, by=c("Stream_ID","Year")) %>%
  distinct(Stream_ID,Year,.keep_all=TRUE) %>%
  dplyr::select(-ends_with(".y")) %>%
  rename_with(~str_remove(., "\\.x$"))

# =============================================================================
# 3) Discharge Metrics: Calculate Flashiness (RBI) and Recession Curve Slope
# =============================================================================
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
    Stream_ID = paste0(LTER, "__", Stream_Name)
  ) %>%
  filter(LTER != "MCM") %>%
  filter(Year >= 2002, Year <= 2022)

daily_Q_CJ <- read.csv("WRTDS-input_discharge.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(
    Date        = as.Date(Date, format = "%Y-%m-%d"),
    Year        = lubridate::year(Date),
    LTER        = "Catalina Jemez",
    Stream_Name = str_extract(Stream_ID, "OR_low|MG_WEIR")
  ) %>%
  filter(Stream_ID %in% c("Catalina Jemez__OR_low", "Catalina Jemez__MG_WEIR")) %>%
  filter(Year >= 2002, Year <= 2022) %>%
  select(-indicate)


# — 3. Join them by Date & Stream_ID —
daily_kalman <- bind_rows(
  daily_kalman,
  daily_Q_CJ
) %>%
  arrange(Stream_ID, Date)  

# ----------------------------------------------------------------------------
# 2. Calculate Daily Differences and Identify Recession Days
# ----------------------------------------------------------------------------
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

# ----------------------------------------------------------------------------
# 3. Compute Aggregate Recession Slope per Stream
# ----------------------------------------------------------------------------
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
  ungroup()

# View the flashiness data frame with RBI values for each Stream_ID
print(flashiness)

# Merge both metrics into your annual ‘tot’ table
tot <- tot %>%
  left_join(recession_slopes, by = "Stream_ID") %>%
  left_join(flashiness, by = "Stream_ID"
  )

# =============================================================================
# 4) Merge Köppen–Geiger Classification
# =============================================================================
KG <- read.csv("Koeppen_Geiger_2.csv") %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  )

tot <- left_join(tot, KG, by="Stream_ID") %>%
  distinct(Stream_ID,Year,.keep_all=TRUE) %>%
  dplyr::select(-contains(".x")) %>%
  rename_with(~str_remove(., "\\.y$"))

# =============================================================================
# 5) Spatial Drivers + Basin Slope Gap‑Fill
# =============================================================================

# a) read & tidy raw spatial‐drivers sheet
si_drivers <- read.csv("all-data_si-extract_2_20250325.csv",
                       stringsAsFactors = FALSE) %>%
  dplyr::select(-contains("soil"), -contains("cycle1")) %>%
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

# b) convert any greenup_ dates → day‑of‑year
gcols <- grep("greenup_", names(si_drivers), value = TRUE)
si_drivers[gcols] <- lapply(si_drivers[gcols], function(x) {
  as.numeric(format(as.Date(x, "%Y-%m-%d"), "%j"))
})

# c) zero‑fill all permafrost & prop_area
pcols <- grep("permafrost|prop_area", names(si_drivers), value = TRUE)
si_drivers[pcols] <- lapply(si_drivers[pcols], function(x) {
  x <- as.numeric(x); x[is.na(x)] <- 0; x
})

# d) split out the annual vars vs the purely character vars
months_regex <- "_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_"
annual_block <- si_drivers %>% 
  dplyr::select(-matches(months_regex))

char_block   <- annual_block %>% 
  dplyr::select(Stream_Name, matches("elevation|rock|land|soil|permafrost|slope"))

annual_vars  <- annual_block %>%
  dplyr::select(-matches("elevation|rock|land|soil|permafrost")) %>%
  dplyr::select(-LTER, -Stream_ID, -Shapefile_Name, -Discharge_File_Name)

# e) melt the annual numbers and tag them
melted <- reshape2::melt(annual_vars,
                         id.vars         = "Stream_Name",
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
  distinct(Stream_Name, year, driver, value) %>%
  pivot_wider(names_from = driver, values_from = value) %>%
  filter(year >= 2002 & year <= 2022) %>%
  rename(Year = year)

# g) re‐attach the character columns
all_spatial <- drivers_cast %>%
  left_join(char_block, by = "Stream_Name", relationship = "many-to-many") %>%
  distinct(Stream_Name, Year, .keep_all = TRUE)

# h) merge into `tot` and coerce types + zero‐fill
tot <- tot %>%
  left_join(all_spatial, by = c("Stream_Name","Year")) %>%
  filter(Year >= 2002, Year <= 2022) %>%
  distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  mutate_at(vars(npp, evapotrans, precip, temp, cycle0, prop_area, permafrost_mean_m),
            as.numeric) %>%
  replace_na(list(permafrost_mean_m = 0, prop_area = 0))

# i) Gap‑fill basin_slope_mean_degree from US & Krycklan sources
# (a) Ensure tot has the slope column
if (!"basin_slope_mean_degree" %in% names(tot)) {
  tot$basin_slope_mean_degree <- NA_real_
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
library(data.table)
setDT(tot)

to_fill <- tot[is.na(basin_slope_mean_degree), .(Stream_ID)]

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

# (g) Join back into tot
setkey(tot,    Stream_ID)
setkey(fill_df,Stream_ID)

tot[fill_df, basin_slope_mean_degree := i.basin_slope_mean_degree]

tot <- tot[!is.na(major_rock) & 
             trimws(major_rock) != "" & 
             major_rock != "0"]

# =============================================================================
# 9) Land Cover + N/P gap‑fill
# =============================================================================
# — First, land cover as before:
# (remove any existing land_*, major_land columns)
tot <- tot %>% select(-starts_with("land_"), -any_of("major_land"))

lulc <- read.csv("DSi_LULC_filled_interpolated_Simple.csv", stringsAsFactors = FALSE) %>%
  select(Stream_Name, Year, Simple_Class, LandClass_sum) %>%
  
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    LandClass_sum = if_else(
      is.na(LandClass_sum) | LandClass_sum == 0,
      LandClass_sum,
      LandClass_sum * 100
    )
  ) %>%
  dplyr::filter(Year >= 2002, Year <= 2022)

lulc_wide <- lulc %>%
  pivot_wider(
    names_from   = Simple_Class,
    values_from  = LandClass_sum,
    names_prefix = "land_"
  )

lulc_wide <- lulc_wide %>% 
  dplyr::select(-land_Filled_Value)

land_cols <- grep("^land_", names(lulc_wide), value = TRUE)
lulc_wide <- lulc_wide %>%
  mutate(
    major_land = apply(select(., all_of(land_cols)), 1, function(x) {
      if(all(is.na(x))) NA_character_
      else sub("^land_", "", names(x)[which.max(x)])
    })
  )

tot <- tot %>% left_join(lulc_wide, by = c("Stream_Name","Year")) %>%
  mutate(across(where(is.list), ~ sapply(., paste, collapse = ",")))


# — Now the unified N/P gap‑fill:

# 1) Read & bind WRTDS (filtered) + CJ NP
wrtds_NP <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv",
                     stringsAsFactors=FALSE) %>%
  rename(LTER = LTER.x) %>%
  select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y,
         -contains("date"), -contains("month"), -min_year, -max_year, -duration) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name=="East Fork"              ~ "east fork",
      Stream_Name=="West Fork"              ~ "west fork",
      Stream_Name=="Amazon River at Obidos" ~ "Obidos",
      TRUE                                  ~ Stream_Name
    ),
    Stream_ID = paste0(LTER,"__",Stream_Name),
    Year      = floor(as.numeric(DecYear))
  ) %>%
  dplyr::filter(Year >= 2002, Year <= 2022) %>%
  filter(chemical %in% c("P","NO3","NOx"), GenConc > 0) %>%
  mutate(chemical = if_else(chemical %in% c("NO3","NOx"), "NOx", chemical)) %>%
  group_by(Stream_ID,Year,chemical) %>%
  summarise(GenConc = median(GenConc, na.rm=TRUE), .groups="drop")

wrtds_CJ_NP <- read.csv("wrtds_kalman_annual_CatalinaJemez.csv",
                        stringsAsFactors=FALSE) %>%
  filter(chemical %in% c("P","NO3","NOx") & GenConc>0) %>%
  mutate(
    Stream_ID = paste0(LTER,"__",Stream_Name),
    Year      = floor(as.numeric(DecYear)),
    chemical  = if_else(chemical %in% c("NO3","NOx"), "NOx", chemical)
  ) %>%
  dplyr::filter(Year >= 2002, Year <= 2022) %>%
  dplyr::select(Stream_ID,Year,chemical,GenConc)

wrtds_NP <- bind_rows(wrtds_NP, wrtds_CJ_NP)

# pivot to wide
wrtds_NP_wide <- wrtds_NP %>%
  pivot_wider(
    names_from  = chemical,
    values_from = GenConc
  )

# 2) Summarise raw NP
raw_NP_wide <- read.csv("converted_raw_NP.csv", stringsAsFactors=FALSE) %>%
  mutate(
    Year      = lubridate::year(as.Date(date)),
    Stream_ID = paste0(LTER,"__",Stream_Name),
    solute    = case_when(
      variable %in% c("NO3","NOx") ~ "NOx",
      variable %in% c("SRP","PO4") ~ "P",
      TRUE                         ~ NA_character_
    )
  ) %>%
  dplyr::filter(Year >= 2002, Year <= 2022) %>%
  filter(!is.na(solute)) %>%
  group_by(Stream_ID,Year,solute) %>%
  summarise(med = median(value,na.rm=TRUE), .groups="drop") %>%
  pivot_wider(
    names_from  = solute,
    values_from = med
  )


# 3) Full‐join, rename, flag, coalesce, drop intermediates
combined_NP <- full_join(wrtds_NP_wide, raw_NP_wide,
                         by = c("Stream_ID","Year")) %>%
  rename(
    P_wrtds   = P.x,    NOx_wrtds = NOx.x,
    P_raw     = P.y,    NOx_raw   = NOx.y
  ) %>%
  mutate(
    P_source   = if_else(is.na(P_wrtds),   "raw","WRTDS"),
    NOx_source = if_else(is.na(NOx_wrtds), "raw","WRTDS"),
    P   = coalesce(P_wrtds,   P_raw),
    NOx = coalesce(NOx_wrtds, NOx_raw)
  ) %>%
  select(Stream_ID,Year,P,NOx,P_source,NOx_source)


# 4) Merge once into tot
tot <- tot %>% left_join(combined_NP, by = c("Stream_ID","Year"))

# sanity check
stopifnot(all(c("P","NOx") %in% names(tot)))

# =============================================================================
# 10) Final Export tot_si 
# =============================================================================
# Tidy data for export: 
tot_si <- tot %>%
  dplyr::select(Stream_ID, Year, drainSqKm, NOx, P, precip, Q,
                temp, prop_area, npp, evapotrans,
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
  mutate(across(c(drainage_area, NOx, P, precip, Q, temp, 
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
  mutate_at(vars(24:38), ~ replace(., is.na(.), 0)) %>%
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

# write out tot_si
write.csv(
  drivers_df,
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years.csv", record_length),
  row.names = FALSE
)

# =============================================================================
# 11) Stratified Split by final_cluster
# =============================================================================
site_clusters <- drivers_df %>%
  distinct(Stream_ID,major_rock,rocks_volcanic,rocks_sedimentary,
           rocks_carbonate_evaporite,rocks_metamorphic,rocks_plutonic) %>%
  mutate(
    consolidated_rock = case_when(
      major_rock %in% c("volcanic","volcanic; plutonic") ~ "Volcanic",
      major_rock %in% c("sedimentary", "sedimentary; metamorphic", 
                        "sedimentary; carbonate_evaporite", 
                        "volcanic; sedimentary; carbonate_evaporite",
                        "sedimentary; plutonic; carbonate_evaporite; metamorphic") ~ "Sedimentary",
      major_rock %in% c("plutonic","plutonic; metamorphic","volcanic; plutonic; metamorphic") ~ "Plutonic",
      major_rock %in% c("metamorphic","carbonate_evaporite; metamorphic") ~ "Metamorphic",
      major_rock %in% c("carbonate_evaporite","volcanic; carbonate_evaporite") ~ "Carbonate Evaporite",
      TRUE ~ NA_character_
    ),
    final_cluster = case_when(
      consolidated_rock=="Sedimentary" & rocks_sedimentary>=70 ~ "Sedimentary",
      consolidated_rock=="Sedimentary" & rocks_sedimentary<70  ~ "Mixed Sedimentary",
      TRUE ~ consolidated_rock
    )
  ) %>%
  dplyr::select(Stream_ID,final_cluster)

set.seed(42)
unseen10 <- site_clusters %>% group_by(final_cluster) %>% slice_sample(prop=0.10) %>% pull(Stream_ID)
trainval  <- setdiff(site_clusters$Stream_ID, unseen10)

unseen10_df <- drivers_df %>% filter(Stream_ID %in% unseen10)
trainval_df <- drivers_df %>% filter(Stream_ID %in% trainval)

trainval_split <- trainval_df %>%
  group_by(Stream_ID) %>%
  arrange(Year) %>%
  mutate(
    tot_count = n(),
    n_recent  = ceiling(0.30*tot_count),
    idx       = row_number(),
    split     = if_else(idx>tot_count-n_recent,"recent","older")
  ) %>%
  ungroup()

older70 <- trainval_split %>% filter(split=="older")  %>% 
  dplyr::select(-tot_count,-n_recent,-idx,-split)

recent30 <- trainval_split %>% filter(split=="recent") %>% 
  dplyr::select(-tot_count,-n_recent,-idx,-split)

write.csv(unseen10_df,"AllDrivers_cc_unseen10.csv",row.names=FALSE)
write.csv(older70,     "AllDrivers_cc_older70.csv", row.names=FALSE)
write.csv(recent30,    "AllDrivers_cc_recent30.csv",row.names=FALSE)

# =============================================================================
# 11) Compute 70/30 split–specific RBI & recession slope (inline)
# =============================================================================
# Create a Year column: 
daily_kalman <- daily_kalman %>%
  mutate(Year = year(Date))

# --- a) older70 split ------------------
daily_older70 <- daily_kalman %>%
  inner_join(older70 %>% select(Stream_ID, Year),
             by = c("Stream_ID","Year"))

rbi_older70 <- daily_older70 %>%
  group_by(Stream_ID) %>%
  arrange(Date) %>%
  mutate(
    dQ     = Q - lag(Q),
    abs_dQ = abs(dQ)
  ) %>%
  filter(!is.na(abs_dQ)) %>%
  summarise(
    RBI_older70 = sum(abs_dQ, na.rm=TRUE) / sum(Q, na.rm=TRUE),
    .groups = "drop"
  )

rec_older70 <- daily_older70 %>%
  group_by(Stream_ID) %>%
  arrange(Date) %>%
  mutate(
    dQ              = Q - lag(Q),
    dQdt            = dQ / as.numeric(Date - lag(Date)),
    ratio           = Q / lag(Q),
    recession_slope = -dQdt
  ) %>%
  filter(!is.na(dQdt), ratio >= 0.7, dQ < 0, recession_slope > 0, Q > 0) %>%
  summarise(
    n_days        = n(),
    slope_older70 = ifelse(
      n_days >= 50,
      tryCatch(coef(lm(log(recession_slope) ~ log(Q), data = cur_data()))[2],
               error = function(e) NA_real_),
      NA_real_
    ),
    .groups = "drop"
  )

older70 <- older70 %>%
  left_join(rbi_older70, by = "Stream_ID") %>%
  left_join(rec_older70, by = "Stream_ID")


# --- b) recent30 split ------------------
daily_recent30 <- daily_kalman %>%
  inner_join(recent30 %>% select(Stream_ID, Year),
             by = c("Stream_ID","Year"))

rbi_recent30 <- daily_recent30 %>%
  group_by(Stream_ID) %>%
  arrange(Date) %>%
  mutate(
    dQ     = Q - lag(Q),
    abs_dQ = abs(dQ)
  ) %>%
  filter(!is.na(abs_dQ)) %>%
  summarise(
    RBI_recent30 = sum(abs_dQ, na.rm=TRUE) / sum(Q, na.rm=TRUE),
    .groups = "drop"
  )

rec_recent30 <- daily_recent30 %>%
  group_by(Stream_ID) %>%
  arrange(Date) %>%
  mutate(
    dQ              = Q - lag(Q),
    dQdt            = dQ / as.numeric(Date - lag(Date)),
    ratio           = Q / lag(Q),
    recession_slope = -dQdt
  ) %>%
  filter(!is.na(dQdt), ratio >= 0.7, dQ < 0, recession_slope > 0, Q > 0) %>%
  summarise(
    n_days         = n(),
    slope_recent30 = ifelse(
      n_days >= 50,
      tryCatch(coef(lm(log(recession_slope) ~ log(Q), data = cur_data()))[2],
               error = function(e) NA_real_),
      NA_real_
    ),
    .groups = "drop"
  )

recent30 <- recent30 %>%
  left_join(rbi_recent30, by = "Stream_ID") %>%
  left_join(rec_recent30, by = "Stream_ID")

# =============================================================================
# 12) Recalculate median NOx & P per 70/30 training/testing split
# =============================================================================
recalc_medians <- function(df) {
  df %>%
    group_by(Stream_ID) %>%
    summarise(
      NOx_med = median(NOx, na.rm=TRUE),
      P_med   = median(P,   na.rm=TRUE),
      .groups = "drop"
    )
}

med_full     <- recalc_medians(drivers_df)
med_unseen10 <- recalc_medians(unseen10_df)
med_older70  <- recalc_medians(older70)
med_recent30 <- recalc_medians(recent30)

# =============================================================================
# Quick sanity checks
# =============================================================================

# 1. Collapse discharge metrics to one row per site for each split
rbi_older_summary <- older70 %>%
  distinct(Stream_ID, RBI_older70)

rbi_recent_summary <- recent30 %>%
  distinct(Stream_ID, RBI_recent30)

rec_older_summary <- older70 %>%
  distinct(Stream_ID, slope_older70)

rec_recent_summary <- recent30 %>%
  distinct(Stream_ID, slope_recent30)

# 2. Combine everything into a single dataframe
time_split_checks <- med_older70 %>%
  rename(NOx_med_older70 = NOx_med, P_med_older70 = P_med) %>%
  full_join(
    med_recent30 %>% rename(NOx_med_recent30 = NOx_med, P_med_recent30 = P_med),
    by = "Stream_ID"
  ) %>%
  left_join(rbi_older_summary, by = "Stream_ID") %>%
  left_join(rbi_recent_summary, by = "Stream_ID") %>%
  left_join(rec_older_summary, by = "Stream_ID") %>%
  left_join(rec_recent_summary, by = "Stream_ID") %>%
  distinct(Stream_ID, .keep_all = TRUE)

# 3. Preview the clean one-row-per-site summary
print(time_split_checks)

# 4. Count number of sites per subset
counts_df <- tibble(
  Subset    = c("Full", "Unseen10", "Older70", "Recent30"),
  n_streams = c(
    n_distinct(drivers_df$Stream_ID),
    n_distinct(unseen10_df$Stream_ID),
    n_distinct(older70$Stream_ID),
    n_distinct(recent30$Stream_ID)
  )
)

print(counts_df)
print(unique(drivers_df$Stream_ID))
