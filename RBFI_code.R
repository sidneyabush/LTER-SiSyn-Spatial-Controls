# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr)

# Clear environment
rm(list = ls())

# Set working directory (change this path as needed)
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# -----------------------------------------------------------
# 1. Load the Chemistry Sites Data (key file with Stream_Name) ----
# -----------------------------------------------------------
# daily_kalman <- read_csv("Full_Results_WRTDS_kalman_daily_filtered.csv") 

cols_needed <- c("LTER.x", "Stream_Name", "Date", "Q")
daily_kalman <- read_csv("Full_Results_WRTDS_kalman_daily_filtered.csv", 
                         col_select = all_of(cols_needed)) %>%
  rename(LTER = LTER.x) %>%
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    # Convert Date to a Date object and filter for 2001-2023
    Date = as.Date(Date),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  ) %>%
  filter(Date >= as.Date("2001-01-01") & Date <= as.Date("2023-12-31"))

daily_Q_CJ <- read.csv("WRTDS-input_discharge.csv",
                       stringsAsFactors = FALSE) %>%
  # parse your Date column
  mutate(
    Date        = as.Date(Date, format = "%Y-%m-%d"),
    # add fixed LTER
    LTER        = "Catalina Jemez",
    # extract OR_low or MG_WEIR from the Stream_ID
    Stream_Name = str_extract(Stream_ID, "OR_low|MG_WEIR")
  ) %>%
  # keep only the Catalina Jemez OR_low / MG_WEIR rows
  filter(Stream_ID %in% c("Catalina Jemez__OR_low",
                          "Catalina Jemez__MG_WEIR")) %>%
  # drop the "indicate" column
  select(-indicate)

# inspect
head(daily_Q_CJ)
  
# — 3. Join them by Date & Stream_ID —
daily_kalman <- bind_rows(
  daily_kalman,
  daily_Q_CJ
) %>%
  arrange(Stream_ID, Date)  # optional: sort by site & date

# -----------------------------------------------------------
# 2. Calculate Flashiness (RBFI) for Each Stream_ID ----
# -----------------------------------------------------------
# For each stream, calculate daily discharge changes and compute RBFI.
flashiness <- daily_kalman %>%
  group_by(Stream_ID) %>%
  arrange(Date) %>%                     # Ensure dates are in order for each stream
  mutate(dQ = Q - lag(Q),                # Daily change in discharge
         abs_dQ = abs(dQ)) %>%           # Absolute change in discharge
  filter(!is.na(abs_dQ)) %>%             # Remove NA from the first row (due to lag)
  summarise(
    total_discharge = sum(Q, na.rm = TRUE),         # Total discharge over the period
    total_change = sum(abs_dQ, na.rm = TRUE),         # Total absolute change
    RBFI = total_change / total_discharge           # Richards-Baker Flashiness Index
  ) %>%
  ungroup()

# View the flashiness data frame with RBFI values for each Stream_ID
print(flashiness)

# Keep only the Stream_ID and RBFI columns
flashiness_export <- flashiness %>%
  select(Stream_ID, RBFI)

# Export the result as a CSV file
write_csv(flashiness_export, "flashiness_by_stream_id.csv")

