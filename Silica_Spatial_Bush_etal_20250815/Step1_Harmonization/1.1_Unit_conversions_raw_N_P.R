# #############################################################################
# Unit conversion from raw N and P data to mg/L to match non-flow-normalized data
# #############################################################################
# Required inputs:
#   1) <drv_dir>/20241003_masterdata_chem.csv
#
# Outputs created:
#   A) <drv_dir>/converted_raw_NP.csv
# #############################################################################

# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr, tibble)

# Clear environment
rm(list = ls())

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

# #############################################################################
# Read in and Tidy Data
# #############################################################################
raw_NP <- read.csv("20241003_masterdata_chem.csv") %>%
  filter(variable %in% c("NOx", "NO3", "SRP", "PO4")) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(date >= as.Date("2001-01-01") & date <= as.Date("2023-12-31"))

# Create Stream_ID if not already present (assumes raw_NP has LTER and Stream_Name)
raw_NP <- raw_NP %>%
  mutate(Stream_ID = paste0(LTER, "__", Stream_Name))

# Create a lookup table with conversion factors.
# For nitrate:
#   For groups reporting mg NO3-N (e.g., Andrews, Australia, MD, etc.): factor = 14
#   For groups reporting mg NO3 (e.g., HBR, KRR, Finnish, NIVA): factor = 62
#   For groups reporting ug NO3-N (e.g., LUQ, GRO): factor = 14 * 1000 = 14000
#
# For phosphorus:
#   For groups reporting mg PO4-P or mg P: factor = 31
#   For HBR (mg PO4): factor = 95
#   For groups reporting ug PO4-P (e.g., LUQ, GRO): factor = 31 * 1000 = 31000
#   For KRR, Finnish, NIVA (mg P): factor = 31
conversion_lookup <- tribble(
  ~LTER,                           ~nitrate_factor, ~phosphate_factor,
  "AND",                      14,              31,
  "Australia",                    14,              31,
  "MD",                           14,              31,
  "Krycklan",                     14,              31,
  "UMR",                          14,              31,
  "UK",                           14,              31,
  "USGS",                         14,              31,
  "NWT",                          14,              31,
  "LUQ",                          14000,           31000,  # values in ug/L
  "GRO",                          14000,           31000,  # values in ug/L
  "HBR",                          62,              95,
  "KRR",                          62,              31,
  "Finnish Environmental Institute", 62,          31,
  "NIVA",                         62,              31
)

# Filter raw_NP to only include the listed LTER's
raw_NP <- raw_NP %>%
  filter(LTER %in% c("AND", "Australia", "MD", "Krycklan", "UMR", "UK",
                     "USGS", "NWT", "LUQ", "GRO", "HBR", "KRR",
                     "Finnish Environmental Institute", "NIVA"))

# Join the raw data with the conversion lookup table based on LTER
raw_NP <- raw_NP %>%
  left_join(conversion_lookup, by = c("LTER" = "LTER"))

# Convert the raw values (in mmol/L) to the desired units:
# - For nitrate: if variable is "NOx" or "NO3", multiply by nitrate_factor.
# - For phosphorus: if variable is "SRP" or "PO4", multiply by phosphate_factor.
raw_NP <- raw_NP %>%
  mutate(value_converted = case_when(
    variable %in% c("NOx", "NO3") ~ value * nitrate_factor,
    variable %in% c("SRP", "PO4") ~ value * phosphate_factor,
    TRUE ~ NA_real_
  ))

# Export the converted data to CSV
write_csv(raw_NP, "converted_raw_NP.csv")
