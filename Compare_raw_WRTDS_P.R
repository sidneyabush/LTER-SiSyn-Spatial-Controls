# Load needed libraries
librarian::shelf(remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot, mlbench, pROC, tree, dplyr,
                 plot.matrix, reshape2, rcartocolor, arsenal, googledrive, data.table, ggplot2, corrplot, pdp, iml, tidyr)

# Clear environment
rm(list = ls())

# Read in and tidy raw data, retaining only complete cases for num_days
drivers_df_raw <- read.csv("AllDrivers_Harmonized_20241018_WRTDS_MD_KG_rawNP.csv") %>%
  distinct(Stream_ID, .keep_all = TRUE) %>%
  filter(!is.na(num_days)) %>%
  select(Stream_ID, Stream_Name, P, num_days) %>%
  dplyr::rename(raw_P = P)

# Read in and tidy WRTDS data, retaining only complete cases for num_days
drivers_df_WRTDS <- read.csv("AllDrivers_Harmonized_20241017_WRTDS_MD_KG_NP.csv") %>%
  distinct(Stream_ID, .keep_all = TRUE) %>%
  filter(!is.na(num_days)) %>%
  select(Stream_ID, Stream_Name, P, num_days) %>%
  dplyr::rename(WRTDS_P = P)

# Merge raw and WRTDS datasets by Stream_ID and Stream_Name
merged_df <- merge(drivers_df_raw, drivers_df_WRTDS, by = c("Stream_ID", "Stream_Name"))

# Fit a linear model to calculate R² and p-value
lm_model <- lm(WRTDS_P ~ raw_P, data = merged_df)
summary_lm <- summary(lm_model)

# Extract R² and p-value from the model summary
r_squared <- round(summary_lm$r.squared, 2)

# Plot WRTDS P versus raw P with R² and p-value
library(ggplot2)

ggplot(merged_df, aes(x = raw_P, y = WRTDS_P)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(title = "WRTDS P vs Raw P",
       x = "Raw P",
       y = "WRTDS P") +
  theme_minimal() +
  # Annotate R² and p-value on the plot
  annotate("text", x = 20, y = 2.5, hjust = 1.1, vjust = -1.5, 
           label = paste("R² =", r_squared),
           size = 4, color = "black")
