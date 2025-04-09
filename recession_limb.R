# Load needed libraries
librarian::shelf(dplyr, ggplot2, data.table, ggpubr, reshape2, EflowStats, zoo, lubridate, readr)

# Clear environment
rm(list = ls())

# Set working directory (adjust the path as needed)
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# ----------------------------------------------------------------------------
# 1. Read and Prepare the Data; Remove Duplicate Date Entries per Stream_ID
# ----------------------------------------------------------------------------

cols_needed <- c("LTER.x", "Stream_Name", "Date", "Q")
daily_kalman <- read_csv("Full_Results_WRTDS_kalman_daily_filtered.csv", 
                         col_select = all_of(cols_needed)) %>%
  dplyr::rename(LTER = LTER.x) %>%
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
    Year = year(Date),
    unique_id = paste(Stream_ID, Date, sep = "_")
  ) %>%
  # Remove duplicate entries based on unique_id
  distinct(unique_id, .keep_all = TRUE) %>%
  filter(Date >= as.Date("2001-01-01") & Date <= as.Date("2023-12-31"))

# Optionally, inspect the unique combinations:
unique_sites <- unique(daily_kalman %>% 
                         dplyr::select(Stream_ID, Date, Year, Q, unique_id))
print(unique_sites)

# ----------------------------------------------------------------------------
# 2. Calculate Recession Events and Compute Recession Slope per Event
# ----------------------------------------------------------------------------

# Order data by Stream_ID and Date and compute daily differences
daily_kalman <- daily_kalman %>%
  arrange(Stream_ID, Date) %>%
  group_by(Stream_ID) %>%
  mutate(
    dQ = Q - lag(Q),
    change_dQ = Q / lag(Q),
    dQ_dt = dQ / as.numeric(Date - lag(Date)),
    # Flag recession days: where discharge is falling (dQ < 0) and relative change meets threshold
    is_recession = if_else(!is.na(dQ) & dQ < 0 & (change_dQ >= 0.7), TRUE, FALSE)
  ) %>%
  ungroup()

# Convert to data.table to assign event IDs to consecutive recession days
library(data.table)
setDT(daily_kalman)
daily_kalman[, event := rleid(is_recession), by = Stream_ID]
setDF(daily_kalman)  # Convert back to a data.frame/tibble if desired

# Keep only recession days
recession_days <- daily_kalman %>%
  filter(is_recession)

# For each recession event (unique combination of Stream_ID and event),
# calculate the recession slope via a linear model (recession_slope ~ Q)
# where recession_slope is defined as -dQ_dt (making it positive).
# Also, record the event start date.
# For each recession event (unique combination of Stream_ID and event),
# fit a linear model (recession_slope ~ Q) if there are at least 5 observations,
# and record the event start date along with the computed slope.
recession_events <- recession_days %>%
  group_by(Stream_ID, event) %>%
  arrange(Date) %>%
  mutate(recession_slope = -dQ_dt) %>%  # Make the slope positive
  group_modify(~ {
    n_days <- nrow(.x)
    event_start <- min(.x$Date)
    slope <- if (n_days >= 5) {
      lm_model <- lm(recession_slope ~ Q, data = .x)
      coef(lm_model)[2]
    } else {
      NA_real_
    }
    tibble(event_start = event_start, n_days = n_days, slope = slope)
  }) %>%
  ungroup() %>%
  filter(!is.na(slope))  # Optionally remove events without enough data

print(recession_events)


# If needed, further aggregate to have one unique value per (Stream_ID, event_start)
unique_stream_date_slopes <- recession_events %>%
  group_by(Stream_ID, event_start) %>%
  summarise(avg_slope = mean(slope, na.rm = TRUE), .groups = "drop")

# View the result: each row is uniquely defined by Stream_ID and the event's starting Date
print(unique_stream_date_slopes)

# ----------------------------------------------------------------------------
# 3. Export the Results
# ----------------------------------------------------------------------------

write_csv(unique_stream_date_slopes, "Recession_Slopes_by_StreamID_EventStart.csv")
