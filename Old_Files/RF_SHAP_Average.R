# Load needed packages
librarian::shelf(remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot, mlbench, pROC, tree, dplyr,
                 plot.matrix, reshape2, rcartocolor, arsenal, googledrive, data.table, ggplot2, corrplot, pdp, 
                 iml, tidyr, viridis)

# Clear environment
rm(list = ls())

# Global seed setting to ensure consistency across the whole workflow
set.seed(123)

# Load Functions ----
# Function to see variable importance by regime
import_plot <- function(rf_model) {
  importance_df <- as.data.frame(rf_model$importance)
  importance_df$driver <- rownames(importance_df)
  importance_melt <- melt(importance_df, id.vars = c("MeanDecreaseAccuracy", "MeanDecreaseGini", "driver"))
  ggplot(importance_melt, aes(driver, value)) +
    geom_point() +
    facet_wrap(~variable) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Function to test ntree - change the internal function to reflect the RF model that you are using
test_numtree_average <- function(ntree_list) {
  MSE <- list()
  for (i in 1:length(ntree_list)) {
    # Set seed for each model training step within the loop
    set.seed(123)
    rf_model <- randomForest(med_si ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = ntree_list[[i]])
    MSE[[i]] <- rf_model$mse
  }
  return(MSE)
}

# Function to test different numbers of trees (ntree) for optimized RF
test_numtree_optimized <- function(ntree_list) {
  MSE <- list()
  for (i in 1:length(ntree_list)) {
    # Set seed for each model training step
    set.seed(123)
    rf_model <- randomForest(rf_formula, data = drivers_df, importance = TRUE, proximity = TRUE, ntree = ntree_list[[i]])
    MSE[[i]] <- rf_model$mse
  }
  return(MSE)
}

# Function to remove outliers based on Z-scores
cols_to_consider <- c("med_si")
sd_limit <- 1.5
remove_outlier_rows <- function(data_to_filter, cols = cols_to_consider, limit = sd_limit) {
  z_scores <- sapply(data_to_filter[cols], function(data) abs((data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE)))
  return(data_to_filter[rowSums(z_scores > limit, na.rm = TRUE) == 0, ])
}

# Read in and tidy data ----
# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

drivers_df <- read.csv("AllDrivers_Harmonized_20241105_WRTDS_MD_KG_NP_silicate_weathering.csv") %>%
  distinct(Stream_ID, .keep_all = TRUE) %>%
  select(-Use_WRTDS, -cycle1, -X, -X.1, -Name, -ClimateZ, -Latitude, -Longitude, -LTER, -major_soil, -contains("soil"),
         -rndCoord.lat, -rndCoord.lon, -Min_Daylength, -elevation_min_m, 
         -elevation_max_m, -elevation_median_m, -basin_slope_median_degree, -basin_slope_min_degree, -basin_slope_max_degree,
         -num_days, -mean_si, -sd_si, -min_Si, -max_Si, -CV_C, -mean_q, -med_q, -sd_q, -CV_Q, -min_Q, -max_Q,
         -cvc_cvq, -C_Q_slope, -major_land, -major_soil, -major_rock, -temp_K, -mapped_lithology,
         -lithology_description, -runoff, -contains("flux")) %>%
  # Filter to retain complete cases for snow_cover
  filter(!is.na(snow_cover))


# # Export a list of stream names with NA values for basin slope
# streams_with_na_slope <- drivers_df %>%
#   filter(is.na(basin_slope_mean_degree)) %>%
#   select(Stream_ID, Stream_Name)
# 
# # Save the list to a CSV file
# write.csv(streams_with_na_slope, "streams_with_na_slope.csv", row.names = FALSE)
# 
# # Export a list of stream names with NA values for permafrost
# streams_with_na_permafrost <- drivers_df %>%
#   filter(is.na(permafrost)) %>%
#   select(Stream_ID, Stream_Name)
# 
# # Save the list to a CSV file
# write.csv(streams_with_na_permafrost, "streams_with_na_permafrost.csv", row.names = FALSE)

# Now import raw P data and merge it for sites in drivers_df where there are NA P values
raw_P <- read.csv("AllDrivers_Harmonized_20241018_WRTDS_MD_KG_rawNP.csv") %>%
  distinct(Stream_ID, .keep_all = TRUE) %>%
  filter(!is.na(num_days)) %>%
  select(Stream_Name, P) %>%
  dplyr::rename(raw_P = P)  # Rename the P column to distinguish raw P from WRTDS P

# Identify rows in drivers_df where P is NA
drivers_df_with_na_P <- drivers_df %>%
  filter(is.na(P)) %>%
  select(Stream_Name, Stream_ID, P)

# Merge raw P data for sites where P is NA in drivers_df
drivers_df_with_raw_P <- drivers_df_with_na_P %>%
  left_join(raw_P, by = "Stream_Name") %>%
  mutate(P = ifelse(is.na(P), raw_P, P))  # Replace NA values in P with raw P values

# Update the original drivers_df with the new P values
drivers_df <- drivers_df %>%
  left_join(drivers_df_with_raw_P %>% select(Stream_ID, P), by = "Stream_ID", suffix = c("", "_new")) %>%
  mutate(P = ifelse(is.na(P_new), P, P_new)) %>%  # Replace P with P_new where P_new is not NA
  select(-P_new)  # Remove the temporary P_new column

## Now import streams with na slopes
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

# Filter drivers_df for rows with NA basin slope values
drivers_df_with_na_slope <- drivers_df %>%
  filter(is.na(basin_slope_mean_degree)) %>%
  select(Stream_Name, Stream_ID)

# Merge drivers_df_with_na_slope with US_slopes and Krycklan_slopes to fill missing values
drivers_df_with_slope_filled <- drivers_df_with_na_slope %>%
  left_join(US_slopes %>% select(Stream_ID, basin_slope_mean_degree), by = "Stream_ID") %>%
  left_join(Krycklan_slopes %>% select(Stream_ID, basin_slope_mean_degree), by = "Stream_ID", suffix = c("_US", "_Krycklan")) %>%
  mutate(
    basin_slope_mean_degree = coalesce(basin_slope_mean_degree_US, basin_slope_mean_degree_Krycklan)
  ) %>%
  select(Stream_ID, basin_slope_mean_degree)

# Update drivers_df with the filled values
drivers_df <- drivers_df %>%
  left_join(drivers_df_with_slope_filled, by = "Stream_ID", suffix = c("", "_filled")) %>%
  mutate(
    basin_slope_mean_degree = coalesce(basin_slope_mean_degree_filled, basin_slope_mean_degree)
  ) %>%
  select(-basin_slope_mean_degree_filled)

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

# Filter drivers_df for rows with NA elevation values
drivers_df_with_na_elev <- drivers_df %>%
  filter(is.na(elevation_mean_m)) %>%
  select(Stream_Name, Stream_ID)

# Merge drivers_df_with_na_elev with US_elev to fill missing elevation values
drivers_df_with_elev_filled <- drivers_df_with_na_elev %>%
  left_join(US_elev %>% select(Stream_ID, elevation_mean_m), by = "Stream_ID")

# Update drivers_df with the filled elevation values
drivers_df <- drivers_df %>%
  left_join(drivers_df_with_elev_filled, by = "Stream_ID", suffix = c("", "_filled")) %>%
  mutate(
    elevation_mean_m = coalesce(elevation_mean_m_filled, elevation_mean_m)
  ) %>%
  select(-elevation_mean_m_filled, -Stream_Name_filled)  # Remove Stream_Name_filled from final output

# Replace NA values in the specified column range with 0
drivers_df <- drivers_df %>%
  dplyr::mutate_at(vars(14:29), ~replace(., is.na(.), 0)) %>%
  # Replace NA values in the "permafrost" column with 0
  mutate(permafrost = replace(permafrost, is.na(permafrost), 0)) 
  

# Testing which drivers we want to keep based on the # of sites it leaves us with
# Assuming drivers_df is your data frame, and you want to check for complete cases in specific columns
cols_to_check <- c("P", "basin_slope_mean_degree", "green_up_day", "drainage_area", "NOx")

# Use complete.cases() on those columns
drivers_df <- drivers_df[complete.cases(drivers_df[, cols_to_check]), ]

# Export the dataframe to a CSV file
write.csv(drivers_df, "Final_Sites.csv", row.names = FALSE)

# Convert all integer columns to numeric in one step
drivers_df <- drivers_df %>% mutate(across(where(is.integer), as.numeric))%>%
  # Remove Stream_Name and Stream_ID columns
  select(-Stream_Name, -Stream_ID)

# Remove outliers using custom function
# drivers_df <- remove_outlier_rows(drivers_df)

# Optionally, check distribution of NAs across all columns
sapply(drivers_df, function(x) sum(is.na(x)))

# Plot correlation between driver variables ----
numeric_drivers <- 2:33  # Indices for numeric drivers
driver_cor <- cor(drivers_df[, numeric_drivers])
corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = F)

# Global seed before testing different numbers of trees (ntree) ----
set.seed(123)
MSE_list <- test_numtree_average(c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000))
tre_list <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000)
MSE_df <- as.data.frame(unlist(MSE_list))
MSE_num <- list()

for (i in 1:length(tre_list)) {
  MSE_num[[i]] <- rep(tre_list[i], tre_list[i])
}

MSE_df$tree_num <- unlist(MSE_num)
MSE_mean <- MSE_df %>%
  group_by(tree_num) %>%
  summarise(mean_MSE = mean(`unlist(MSE_list)`))

# Visualize and select the number of trees that gives the minimum MSE error
ggplot(MSE_mean, aes(tree_num, mean_MSE)) + geom_point() + geom_line() + theme_classic() +
  scale_x_continuous(breaks = seq(100, 2000, 100)) + theme(text = element_text(size = 20))


# Global seed before tuning mtry based on optimized ntree ----
set.seed(123)
tuneRF(drivers_df[, numeric_drivers], drivers_df[, 1], ntreeTry = 200, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run initial RF using tuned parameters ----
set.seed(123)
rf_model1 <- randomForest(med_si ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 200, mtry = 10)

# Visualize output for rf_model1
print(rf_model1)
randomForest::varImpPlot(rf_model1)

# Global seed for RFE ----
size <- ncol(drivers_df) - 1  # This is the number of predictor variables
cv_repeats <- 5
cv_number <- 5
total_repeats <- (cv_repeats * cv_number) + 1

seeds <- vector(mode = "list", length = total_repeats)
for (i in 1:(cv_repeats * cv_number)) {
  seeds[[i]] <- rep(123, size)
}
seeds[[total_repeats]] <- 123

control <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = cv_repeats, number = cv_number, seeds = seeds, verbose = TRUE)

# Divide data into predictor variables (x) and response variable (y)
x <- drivers_df[, !(colnames(drivers_df) == "med_si")]
y <- drivers_df$med_si

# Run RFE to select the best features ----
set.seed(123)
result_rfe <- rfe(x = x, y = y, sizes = c(1:size), rfeControl = control)

# Print RFE results
print(result_rfe)

# Put selected features into variable
new_rf_input <- paste(predictors(result_rfe), collapse = "+")

# Format those features into a formula for the optimized random forest model
rf_formula <- formula(paste("med_si ~", new_rf_input))

# Global seed before re-tuning RF after RFE optimization ----
set.seed(123)
MSE_list <- test_numtree_optimized(c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000))

# Visualize and select number of trees that gives the minimum MSE error
MSE_df <- as.data.frame(unlist(MSE_list))
MSE_num <- list()

for (i in 1:length(tre_list)) {
  MSE_num[[i]] <- rep(tre_list[i], tre_list[i])
}

MSE_df$tree_num <- unlist(MSE_num)
MSE_mean <- MSE_df %>%
  group_by(tree_num) %>%
  summarise(mean_MSE = mean(`unlist(MSE_list)`))

ggplot(MSE_mean, aes(tree_num, mean_MSE)) + geom_point() + geom_line() +
  theme_classic() + scale_x_continuous(breaks = seq(100, 2000, 100)) + theme(text = element_text(size = 20))

# Global seed before re-tuning mtry
set.seed(123)
kept_drivers <- drivers_df[, colnames(drivers_df) %in% predictors(result_rfe)]
tuneRF(kept_drivers, drivers_df[, 1], ntreeTry = 2000, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run optimized random forest model, with re-tuned ntree and mtry parameters ----
set.seed(123)
rf_model2 <- randomForest(rf_formula, data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 2000, mtry = )

# Visualize output for rf_model2
print(rf_model2)
randomForest::varImpPlot(rf_model2)

# Generate plots comparing predicted vs observed ----
lm_plot <- plot(rf_model2$predicted, drivers_df$med_si, pch = 16, cex = 1.5,
                xlab = "Predicted", ylab = "Observed", main = "All Spatial Drivers",
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model2$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))

# # Generate partial dependence plots ----
# plot_partial_dependence <- function(model, variable) {
#   par.Long <- partial(model, pred.var = variable)
#   partial_plot <- autoplot(par.Long, contour = TRUE) + theme_bw() + theme(text = element_text(size = 20))
#   print(partial_plot)
# }
# 
# # List of variables to plot
# variables <- c("P", "temp", "green_up_day", "precip")
# 
# # Apply the function for each variable
# lapply(variables, function(var) plot_partial_dependence(rf_model2, var))


# # Shapley values ----
# 
# # Step 1: Create the predictor object for the iml package using the original model and all data
# predictor <- Predictor$new(model = rf_model2, data = kept_drivers, y = drivers_df$med_si)
# 
# # Step 2: Calculate Shapley values for all observations
# set.seed(123)  # For reproducibility
# shapley_list <- lapply(1:nrow(kept_drivers), function(i) {
#   shap <- Shapley$new(predictor, x.interest = kept_drivers[i, ], sample.size = 100)  # Adjust sample size as needed
#   shap$results  # Extract the Shapley results
# })
# 
# # Combine all Shapley results into a single dataframe
# shapley_df <- do.call(rbind, shapley_list)
# 
# # Step 3: Combine Shapley values with feature values from kept_drivers directly
# # Add row_id to shapley_df for joining with the original data
# shapley_df <- shapley_df %>%
#   mutate(row_id = rep(1:nrow(kept_drivers), each = ncol(kept_drivers)))
# 
# # Reshape kept_drivers using melt from reshape2 to match the structure of shapley_df
# kept_drivers_melted <- melt(kept_drivers)
# kept_drivers_melted$row_id <- rep(1:nrow(kept_drivers), ncol(kept_drivers))
# 
# # Step 4: Merge Shapley values with feature values from kept_drivers
# shapley_plot_data <- shapley_df %>%
#   left_join(kept_drivers_melted, by = c("feature" = "variable", "row_id"))
# 
# # Step 5: Add metadata from drivers_df ----
# # Add row_id to drivers_df to enable merging with shapley_plot_data
# drivers_df$row_id <- 1:nrow(drivers_df)
# 
# # Merge metadata into the shapley_plot_data based on the row_id
# shapley_plot_data <- shapley_plot_data %>%
#   left_join(drivers_df, by = "row_id")
# 
# # Step 6: Calculate feature importance based on the mean absolute SHAP values
# feature_importance <- shapley_plot_data %>%
#   group_by(feature) %>%
#   summarise(importance = mean(abs(phi))) %>%
#   arrange(desc(importance))  # Order by importance (descending)
# 
# # Step 7: Plot SHAP summary with magnitude, direction, and colors
# # Reorder features by importance in descending order (most important at the top)
# shapley_plot_data <- shapley_plot_data %>%
#   mutate(feature = factor(feature, levels = rev(feature_importance$feature)))  # Reorder factor levels in descending order
# 
# ggplot(shapley_plot_data, aes(x = phi, y = feature, color = value)) + 
#   geom_point(alpha = 0.6) +  # Transparency for better visualization of overlapping points
#   scale_color_viridis_c(option = "C") +  # Color scale to represent feature values
#   labs(x = "SHAP value (impact on model output)", y = "Features", title = "SHAP Summary Plot") + 
#   theme_bw() + 
#   theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), 
#         plot.title = element_text(size = 16, face = "bold"))
# 
# # Step 8: Plot the feature importance
# ggplot(feature_importance, aes(x = reorder(feature, importance), y = importance)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   coord_flip() +  # Flip coordinates for horizontal bars
#   labs(x = "Feature", y = "Mean Absolute SHAP Value", title = "Feature Importance") +
#   theme_minimal()
# 
# # Step 9: Reshape the SHAP values into a wide format for heatmap
# shap_wide <- dcast(shapley_plot_data, row_id ~ feature, value.var = "phi")
# 
# # Step 10: Create a force plot for a single observation
# single_shap <- shapley_plot_data[shapley_plot_data$row_id == 1, ]  # Example for the first observation
# 
# ggplot(single_shap, aes(x = feature, y = phi, fill = phi > 0)) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values = c("red", "blue"), labels = c("Negative Impact", "Positive Impact")) +
#   labs(x = "Feature", y = "SHAP Value", title = "SHAP Force Plot for Observation 1") +
#   coord_flip() +  # Flip for horizontal bars
#   theme_minimal()
# 
# # Step 11: Calculate SHAP feature importance (mean absolute SHAP values)
# shapley_feature_importance <- shapley_plot_data %>%
#   group_by(feature) %>%
#   summarise(importance = mean(abs(phi))) %>%
#   arrange(desc(importance))  # Order by importance (descending)
# 
# # Step 12: Select the top 5 most important features
# top_5_features <- shapley_feature_importance$feature[1:5]
# 
# # Step 13: Update function to create a SHAP dependence plot, with an option to color by another variable
# create_shap_dependence_plot <- function(data, feature_name, color_var = NULL) {
#   # Determine if the color variable is continuous or discrete
#   if (!is.null(color_var) && is.numeric(data[[color_var]])) {
#     color_scale <- scale_color_viridis_c(option = "C")  # Continuous scale
#   } else {
#     color_scale <- scale_color_viridis_d(option = "C")  # Discrete scale
#   }
#   
#   # Create the dependence plot
#   plot <- ggplot(data[data$feature == feature_name, ], 
#                  aes_string(x = "value", y = "phi", color = if (!is.null(color_var)) color_var else "feature")) +
#     geom_point(alpha = 0.6) + 
#     color_scale + 
#     labs(x = paste("Value of", feature_name), 
#          y = "SHAP Value", 
#          title = paste("SHAP Dependence Plot for", feature_name)) + 
#     theme_minimal() +
#     theme(plot.title = element_text(size = 16, face = "bold"))
#   
#   return(plot)
# }
# 
# # Step 14: Iterate through the top 5 most important features and create a plot for each one, with optional coloring
# color_variable <- "silicate_weathering" 
# 
# # Plot histogram of silicate_weathering values
# hist(shapley_plot_data$silicate_weathering, 
#      main = "Histogram of silicate weathering",
#      xlab = "silicate weathering",
#      ylab = "Frequency",
#      col = "lightblue",
#      border = "black")
# 
# # Filter out rows where silicate_weathering is over 20
# # shapley_plot_data <- shapley_plot_data[shapley_plot_data$silicate_weathering <= 20, ]
# 
# for (feature in top_5_features) {
#   print(create_shap_dependence_plot(shapley_plot_data, feature, color_var = color_variable))  
# }


### MAKE INTO A FUNCTION ---- 

# Function to calculate and plot overall summary and top 5 important features
create_overall_summary_and_top_plots <- function(shap_data, output_dir) {
  
  # Calculate overall feature importance for all data (no threshold filtering)
  overall_feature_importance <- shap_data %>%
    group_by(feature) %>%
    summarise(importance = mean(abs(phi))) %>%
    arrange(desc(importance))
  
  # Create the overall summary plot
  overall_summary_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(x = NULL, y = "Mean Absolute SHAP Value", title = "Overall Feature Importance") +
    theme_minimal()
  
  # Save the overall summary plot as a PDF
  pdf(sprintf("%s/overall_summary_plot.pdf", output_dir), width = 8, height = 6)
  print(overall_summary_plot)
  dev.off()
  
  # Identify top 5 most important features
  top_features <- overall_feature_importance %>%
    slice_head(n = 5) %>%
    pull(feature)
  
  # Generate and save plots for each of the top 5 features
  for (feature in top_features) {
    subset_shapley_plot(
      shap_data = shap_data,
      feature_name = feature,
      threshold = 0,  # Neutral threshold, include all data for each top feature
      above_threshold = TRUE,
      color_var = NULL,  # No color variable for top feature plots
      output_dir = output_dir
    )
  }
}

# Main subset_shapley_plot function
subset_shapley_plot <- function(shap_data, feature_name, threshold = 50, above_threshold = TRUE, color_var = NULL, output_dir = "path/to/output") {
  
  # Step 1: Subset the data based on the threshold
  filtered_data <- shap_data %>%
    filter(if (above_threshold) .data[[feature_name]] >= threshold else .data[[feature_name]] < threshold)
  
  # Remove the thresholding feature from filtered_data
  filtered_data <- filtered_data %>%
    filter(feature != feature_name)
  
  # Generate the threshold condition text for the title
  threshold_condition <- if (above_threshold) "above" else "below"
  title_threshold <- paste(feature_name, threshold_condition, threshold)
  
  # Step 2: Calculate feature importance based on the mean absolute SHAP values
  feature_importance <- filtered_data %>%
    group_by(feature) %>%
    summarise(importance = mean(abs(phi))) %>%
    arrange(desc(importance))  # Order by importance (descending)
  
  # Step 3: Reorder features by importance for the summary plot
  filtered_data <- filtered_data %>%
    mutate(feature = factor(feature, levels = rev(feature_importance$feature)))  # Reorder factor levels
  
  # Step 4: Scale the feature values only if color_var is provided
  if (!is.null(color_var)) {
    filtered_data <- filtered_data %>%
      mutate(scaled_feature_value = (get(color_var) - min(get(color_var), na.rm = TRUE)) / 
               (max(get(color_var), na.rm = TRUE) - min(get(color_var), na.rm = TRUE)))
  }
  
  # Step 5: SHAP Summary Plot using scaled feature values for color (conditionally)
  shap_summary_plot <- ggplot(filtered_data, aes(x = phi, y = feature)) + 
    geom_point(aes(color = if (!is.null(color_var)) scaled_feature_value else NULL), alpha = 0.6) + 
    scale_color_gradient(low = "blue", high = "red", name = "Value", 
                         breaks = c(0, 1), labels = c("Low", "High")) +
    guides(color = guide_colorbar(title = "Value", title.position = "top",
                                  barwidth = 1, barheight = 10)) +
    labs(x = "SHAP Value", y = NULL, title = paste(title_threshold)) + 
    theme_bw() + 
    theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), 
          plot.title = element_text(size = 16, face = "bold"))
  
  # Step 6: Feature Importance Plot
  feature_importance_plot <- ggplot(feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +  
    labs(x = NULL, y = "Mean Absolute SHAP Value", title = paste(title_threshold)) +
    theme_minimal()
  
  # Step 7: Overall Positive/Negative SHAP Impact Plot with Ordering
  pos_neg_summary <- filtered_data %>%
    group_by(feature) %>%
    summarise(mean_phi = mean(phi)) %>%
    mutate(feature = factor(feature, levels = rev(feature_importance$feature)))  # Order by absolute importance
  
  pos_neg_plot <- ggplot(pos_neg_summary, aes(x = feature, y = mean_phi, fill = mean_phi > 0)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("red", "blue"), labels = c("Negative Impact", "Positive Impact")) +
    labs(x = "Feature", y = "Mean SHAP Value", title = "Overall SHAP Impact by Feature") +
    coord_flip() +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"))
  
  # Step 8: SHAP Dependence Plot for Specific Feature
  dependence_plots <- list()
  for (top_feature in feature_importance$feature[1:5]) {
    aes_mapping <- if (!is.null(color_var)) {
      aes(x = value, y = phi, color = get(color_var))
    } else {
      aes(x = value, y = phi)
    }
    
    dependence_plot <- ggplot(filtered_data[filtered_data$feature == top_feature, ], aes_mapping) +
      geom_point(alpha = 0.6) + 
      (if (!is.null(color_var)) scale_color_gradient(low = "blue", high = "red", name = color_var) else NULL) +
      labs(x = paste("Value of", top_feature), y = "SHAP Value", title = paste("Dependence Plot for", top_feature)) + 
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"))
    
    dependence_plots <- c(dependence_plots, list(dependence_plot))
  }
  
  # Save all plots to PDF
  pdf_filename <- sprintf("%s/%s_%s_threshold_%s_above_%s.pdf",
                          output_dir, feature_name, ifelse(is.null(color_var), "shap", color_var),
                          threshold, ifelse(above_threshold, "above", "below"))
  
  pdf(pdf_filename, width = 8, height = 6)
  
  print(shap_summary_plot)
  print(feature_importance_plot)
  print(pos_neg_plot)
  
  for (j in seq_along(dependence_plots)) {
    print(dependence_plots[[j]])
  }
  
  dev.off()
  
  return(list(shap_summary_plot = shap_summary_plot,
              feature_importance_plot = feature_importance_plot,
              pos_neg_plot = pos_neg_plot,
              dependence_plots = dependence_plots))
}

# Set up output directory
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/FN_Conc"

# Generate overall summary and top 5 feature plots
create_overall_summary_and_top_plots(shapley_plot_data, output_dir)

# Generate plots based on specified plot parameters
for (params in plot_parameters) {
  subset_shapley_plot(
    shap_data = shapley_plot_data,
    feature_name = params$var,
    threshold = params$threshold,
    above_threshold = params$above_threshold,
    color_var = params$color_var,
    output_dir = output_dir
  )
}
