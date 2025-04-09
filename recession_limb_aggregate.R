# Load needed libraries
librarian::shelf(dplyr, ggplot2, data.table, ggpubr, reshape2, EflowStats, zoo, lubridate, readr)

# Clear environment
rm(list = ls())

# Set working directory (adjust the path as needed)
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# ----------------------------------------------------------------------------
# 1. Read and Prepare the Data; Remove Duplicate Date Entries per Stream_ID
# ----------------------------------------------------------------------------

# Specify the columns needed. Note: we use backticks around LTER.x in rename
cols_needed <- c("LTER.x", "Stream_Name", "Date", "Q")
daily_kalman <- read_csv("Full_Results_WRTDS_kalman_daily_filtered.csv", 
                         col_select = all_of(cols_needed)) %>%
  dplyr::rename(LTER = `LTER.x`) %>%  
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Date = as.Date(Date),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Year = year(Date)
  ) %>%
  # Remove duplicate entries if needed (if you have a unique_id column already)
  distinct(Stream_ID, Date, .keep_all = TRUE) %>%
  filter(Date >= as.Date("2001-01-01") & Date <= as.Date("2023-12-31"))

# Optionally, inspect the unique combinations:
unique_sites <- unique(daily_kalman %>% dplyr::select(Stream_ID, Date, Year, Q))
print(unique_sites)

# ----------------------------------------------------------------------------
# 2. Calculate Daily Differences and Identify Recession Days
# ----------------------------------------------------------------------------

daily_kalman <- daily_kalman %>%
  arrange(Stream_ID, Date) %>%
  dplyr::group_by(Stream_ID) %>%
  mutate(
    dQ = Q - lag(Q),
    change_dQ = Q / lag(Q),
    dQ_dt = dQ / as.numeric(Date - lag(Date)),
    # Flag recession days: discharge falling (dQ < 0) and a relative change threshold (>= 0.7)
    is_recession = if_else(!is.na(dQ) & dQ < 0 & (change_dQ >= 0.7), TRUE, FALSE)
  ) %>%
  ungroup()

# Keep only recession days and create a recession_slope as the positive value of -dQ_dt
recession_days <- daily_kalman %>%
  filter(is_recession) %>%
  mutate(recession_slope = -dQ_dt)

# ----------------------------------------------------------------------------
# 3. Compute Aggregate Recession Slope per Stream
# ----------------------------------------------------------------------------

# For each stream, if there are at least 5 recession days, fit a linear model (recession_slope ~ Q)
# and extract the slope coefficient.
recession_slopes <- recession_days %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(
    n_days = n(),
    slope = if(n_days >= 5) {
      lm_model <- lm(recession_slope ~ Q, data = cur_data())
      unname(coef(lm_model)[2])
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(slope))

# View the result (one row per Stream_ID)
print(recession_slopes)

# ----------------------------------------------------------------------------
# 4. Export the Results
# ----------------------------------------------------------------------------

write_csv(recession_slopes, "Recession_Slopes_by_StreamID_Aggregate.csv")
