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
  dplyr::filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  dplyr::mutate(
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
  dplyr::distinct(Stream_ID, Date, .keep_all = TRUE) %>%
  dplyr::filter(Date >= as.Date("2001-01-01") & Date <= as.Date("2023-12-31"))

# Optionally, inspect the unique combinations:
unique_sites <- unique(daily_kalman %>% 
                         dplyr::select(Stream_ID, Date, Year, Q))
print(unique_sites)

# ----------------------------------------------------------------------------
# 2. Calculate Daily Differences and Identify Recession Days
# ----------------------------------------------------------------------------
daily_kalman <- daily_kalman %>%
  dplyr::arrange(Stream_ID, Date) %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::mutate(
    dQ = Q - lag(Q),
    change_dQ = Q / lag(Q),
    dQ_dt = dQ / as.numeric(Date - lag(Date))) %>%
  dplyr::filter(!is.na(dQ_dt)) %>% # Remove NA values (first row)
  dplyr::filter(!change_dQ < 0.7) 

# Calculate the recession slope (-dQ/dt)
recession_data <- daily_kalman %>%
  dplyr::filter(dQ < 0) %>%  # Keep only recession periods
  dplyr::mutate(recession_slope = -dQ_dt)  # Make it positive for the slope

# ----------------------------------------------------------------------------
# 3. Compute Aggregate Recession Slope per Stream
# ----------------------------------------------------------------------------
# For each stream, if there are at least 50 recession days, fit a linear model (recession_slope ~ Q)
# and extract the slope coefficient.
recession_slopes <- recession_data %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(
    n_days = n(),
    slope = if(n_days >= 50) {
      lm_model <- lm(recession_slope ~ Q, data = cur_data())
      unname(coef(lm_model)[2])
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  dplyr::filter(!is.na(slope))

# View the result (one row per Stream_ID)
print(recession_slopes)

# -------------------------------
# Plot: dQ/dT vs Q for Multiple Sites
# -------------------------------
# Define the sites of interest by providing their Stream_ID values.
# Replace with the actual identifiers from your dataset if needed.
sites_of_interest <- c("USGS__ST. LAWRENCE", "HYBAM__Manacapuru", "HYBAM__Obidos")

multi_site_data <- recession_data %>% 
  dplyr::filter(Stream_ID %in% sites_of_interest)

# Step 2: Calculate slope for each site, and determine label positions using the 90th percentile
slopes <- multi_site_data %>% 
  dplyr::group_by(Stream_ID) %>% 
  dplyr::summarise(
    slope = round(coef(lm(recession_slope ~ Q))[["Q"]], 2),
    Q_pos = quantile(Q, 0.9, na.rm = TRUE),
    dQ_dt_pos = quantile(recession_slope, 0.9, na.rm = TRUE)
  )

# Step 3: Create the plot with slope annotation
p_dQ_dt <- ggplot(multi_site_data, aes(x = Q, y = recession_slope)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "skyblue", se = FALSE) + 
  facet_wrap(~ Stream_ID, scales = "free") +  # One panel per site
  # Add slope annotations from the slopes dataframe
  geom_text(
    data = slopes, 
    aes(x = Q_pos, y = dQ_dt_pos, label = paste("slope =", slope)),
    hjust = 1, vjust = 1, size = 3, color = "skyblue"
  ) +
  labs(
    title = NULL,
    x = "Discharge (Q)",
    y = "dQ/dT"
  ) +
  theme_bw()

# Display the plot
print(p_dQ_dt)

# ----------------------------------------------------------------------------
# 5. Export the Results
# ----------------------------------------------------------------------------
write.csv(recession_slopes, "Recession_Slopes_by_StreamID_Aggregate.csv")
