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
    Year = floor(as.numeric(DecYear))
  ) %>%
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  filter(chemical == "DSi") %>%
  filter(GenConc <= 60) %>%
  filter(FNConc >= 0.5 * GenConc & FNConc <= 1.5 * GenConc)

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
# Calculate Stats ----
## ------------------------------------------------------- ##
# Calculate si_stats without CV columns
si_stats <- wrtds_df %>%
  group_by(Stream_ID, Year) %>%
  summarise(
    across(
      c(FNConc, GenConc, FNFlux, GenFlux),
      list(median = median),
      .names = "{.fn}_{.col}"
    ),
    .groups = "drop"  # Ungroup after summarise for a clean output
  )


# Calculate q_stats with CV for Q
q_stats <- wrtds_df %>%
  group_by(Stream_ID, Year) %>%
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
  left_join(q_stats, by = c("Stream_ID", "Year")) %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

## ------------------------------------------------------- ##
# Download Reference Table from GD for DA ----
## ------------------------------------------------------- ##
ref_table <- read.csv("Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv")
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
tot <- tot %>%
  left_join(area, by = "Stream_ID")%>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

gc()

## ------------------------------------------------------- ##
# Calculate Yields ----
## ------------------------------------------------------- ##
yields <- tot %>%
  mutate(FNYield = median_FNFlux / drainSqKm,
         GenYield = median_GenFlux / drainSqKm) %>%
  dplyr::select(-median_FNFlux, -median_GenFlux)

# Combine si_stats and q_stats, handling duplicate columns with coalesce
tot <- tot %>%
  left_join(yields, by = c("Stream_ID", "Year")) %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

gc()

## ------------------------------------------------------- ##
# Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv")
KG$Stream_ID<-paste0(KG$LTER, "__", KG$Stream_Name)

# Combine si_stats and q_stats, handling duplicate columns with coalesce
tot <- tot %>%
  left_join(KG, by = "Stream_ID") %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

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
    min_daylength = min(mean_daylength),
    max_daylength = max(mean_daylength)
  ) %>%
  left_join(name_conversion, by = "Stream_Name") %>%
  mutate(Stream_Name = coalesce(Updated_StreamName, Stream_Name)) %>%
  dplyr::select(-Updated_StreamName)

# Ensure the result is left-joined to "tot"
tot <- tot %>% 
  left_join(daylen_range, by = "Stream_Name") %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

## ------------------------------------------------------- ##
# Import Spatial Drivers ----
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

# Confirm updates
summary(spatial_drivers[, permafrost_cols])

# Extract years from wrtds_df
dsi_years <- unique(floor(wrtds_df$DecYear))  # Extract integer year

# Create a pattern for DSi years
year_pattern <- paste0("_", dsi_years, "_", collapse = "|")

# Filter columns in spatial_drivers that match DSi years
matching_columns <- grep(year_pattern, colnames(spatial_drivers), value = TRUE)

# These are the categorical variables like "major_land", "major_soil", and "major_rock" and the numerical % of each
major_cat_vars <- which(colnames(spatial_drivers) %like% c("soil|land|rock"))
cat_vars <- spatial_drivers[,c(major_cat_vars)]

# Combine spatial drivers and categorical variables
spatial_vars <- cbind(spatial_drivers, cat_vars)

# Define the pattern to search for elevation and basin slope columns
relevant_columns <- c("elevation_mean_m", "basin_slope_mean_degree", "permafrost_mean_m")

# Use grep to locate elevation and basin slope columns
cat_vars <- spatial_drivers[, c(major_cat_vars, relevant_columns)]

cat_vars$Stream_ID <- spatial_drivers$Stream_ID

matching_vars <- spatial_drivers[, c("Stream_ID", matching_columns)]

# Join with totals
tot <- tot %>%
  left_join(cat_vars, by = "Stream_ID") %>%
  left_join(matching_vars, by = "Stream_ID") %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

## Export Full Yearly Results
tot %>%
  write.csv("AllDrivers_Harmonized_Yearly_test.csv", row.names = FALSE)
