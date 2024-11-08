# Load needed libraries
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

drivers_df <- read.csv("AllDrivers_Harmonized_20241018_WRTDS_MD_KG_rawNP.csv") %>%
  distinct(Stream_ID, .keep_all = TRUE) %>%
  select(-Use_WRTDS, -cycle1, -X, -X.1, -Name, -ClimateZ, -Latitude, -Longitude, -LTER, -major_soil, -contains("soil"),
         -rndCoord.lat, -rndCoord.lon, -Min_Daylength, -elevation_min_m, 
         -elevation_max_m, -elevation_median_m, -basin_slope_median_degree, -basin_slope_min_degree, -basin_slope_max_degree,
         -num_days, -mean_si, -sd_si, -min_Si, -max_Si, -CV_C, -mean_q, -med_q, -sd_q, -CV_Q, -min_Q, -max_Q,
         -cvc_cvq, -C_Q_slope, -major_land, -major_soil, -major_rock, -contains("flux")) %>%
  # Rename specific columns
  dplyr::rename(drainage_area = drainSqKm,
                snow_cover = prop_area,
                green_up_day = cycle0,
                max_daylength = Max_Daylength) %>%
  # Filter to retain complete cases for snow_cover
  filter(!is.na(snow_cover))

# Export a list of stream names with NA values for permafrost
streams_with_na_permafrost <- drivers_df %>%
  filter(is.na(permafrost)) %>%
  select(Stream_ID, Stream_Name)

# Save the list to a CSV file
write.csv(streams_with_na_permafrost, "streams_with_na_permafrost.csv", row.names = FALSE)

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

# Replace NA values in the specified column range with 0
drivers_df <- drivers_df %>%
  dplyr::mutate_at(vars(14:29), ~replace(., is.na(.), 0)) %>%
  # Replace NA values in the "permafrost" column with 0
  mutate(permafrost = replace(permafrost, is.na(permafrost), 0)) %>%
  # Remove Stream_Name and Stream_ID columns
  select(-Stream_Name, -Stream_ID)
  

# Testing which drivers we want to keep based on the # of sites it leaves us with
# Assuming drivers_df is your data frame, and you want to check for complete cases in specific columns
cols_to_check <- c("P", "basin_slope_mean_degree", "green_up_day", "drainage_area")

# Use complete.cases() on those columns
drivers_df <- drivers_df[complete.cases(drivers_df[, cols_to_check]), ]

# Convert all integer columns to numeric in one step
drivers_df <- drivers_df %>% mutate(across(where(is.integer), as.numeric))

# Remove outliers using custom function
# drivers_df <- remove_outlier_rows(drivers_df)

# Optionally, check distribution of NAs across all columns
sapply(drivers_df, function(x) sum(is.na(x)))

# Plot correlation between driver variables ----
numeric_drivers <- 2:32  # Indices for numeric drivers
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
tuneRF(drivers_df[, numeric_drivers], drivers_df[, 1], ntreeTry = 600, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run initial RF using tuned parameters ----
set.seed(123)
rf_model1 <- randomForest(med_si ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 600, mtry = 10)

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
tuneRF(kept_drivers, drivers_df[, 1], ntreeTry = 300, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run optimized random forest model, with re-tuned ntree and mtry parameters ----
set.seed(123)
rf_model2 <- randomForest(rf_formula, data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 300, mtry = 2)

# Visualize output for rf_model2
print(rf_model2)
randomForest::varImpPlot(rf_model2)

# # Generate plots comparing predicted vs observed ----
# lm_plot <- plot(rf_model2$predicted, drivers_df$med_si, pch = 16, cex = 1.5,
#                 xlab = "Predicted", ylab = "Observed", main = "All Spatial Drivers",
#                 cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) + 
#   abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) + 
#   theme(text = element_text(size = 40), face = "bold")
# legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model2$rsq), digits = 3))) 
# legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))
# 
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


# Shapley values ----
# Step 1: Create the predictor object for the iml package using the original model and all data
predictor <- Predictor$new(model = rf_model2, data = kept_drivers, y = drivers_df$med_si)

# Step 2: Calculate Shapley values for all variables
set.seed(123)  # For reproducibility
shapley_list <- lapply(1:nrow(kept_drivers), function(i) {
  shap <- Shapley$new(predictor, x.interest = kept_drivers[i, ], sample.size = 100)  # Reduce sample size if necessary
  shap$results  # Extract the Shapley results
})

# Combine all Shapley results into a single dataframe
shapley_df <- do.call(rbind, shapley_list)

# Step 3: Combine Shapley values with feature values from kept_drivers directly
# Add row_id to shapley_df for joining with the original data
shapley_df <- shapley_df %>%
  mutate(row_id = rep(1:nrow(kept_drivers), each = ncol(kept_drivers)))

# Reshape kept_drivers using melt from reshape2 to match the structure of shapley_df
kept_drivers_melted <- melt(kept_drivers)
kept_drivers_melted$row_id <- rep(1:nrow(kept_drivers), ncol(kept_drivers))

# Step 4: Combine Shapley values with feature values from kept_drivers
shapley_plot_data <- shapley_df %>%
  left_join(kept_drivers_melted, by = c("feature" = "variable", "row_id"))

# Step 5: Calculate feature importance based on the mean absolute SHAP values
feature_importance <- shapley_plot_data %>%
  group_by(feature) %>%
  summarise(importance = mean(abs(phi))) %>%
  arrange(desc(importance))  # Order by importance (descending)

# Step 6: Plot SHAP summary with magnitude, direction, and colors
# Reorder features by importance in descending order (most important at the top)
shapley_plot_data <- shapley_plot_data %>%
  mutate(feature = factor(feature, levels = rev(feature_importance$feature)))  # Reorder factor levels in descending order

ggplot(shapley_plot_data, aes(x = phi, y = feature, color = value)) + 
  geom_point(alpha = 0.6) +  # Transparency for better visualization of overlapping points
  scale_color_viridis_c(option = "C") +  # Color scale to represent feature values
  labs(x = "SHAP value (impact on model output)", y = "Features", title = "SHAP Summary Plot") + 
  theme_bw() + 
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), 
        plot.title = element_text(size = 16, face = "bold"))

# Calculate feature importance by averaging the absolute SHAP values
feature_importance <- shapley_plot_data %>%
  group_by(feature) %>%
  summarise(importance = mean(abs(phi))) %>%
  arrange(desc(importance))

# Plot the feature importance
ggplot(feature_importance, aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for horizontal bars
  labs(x = "Feature", y = "Mean Absolute SHAP Value", title = "Feature Importance") +
  theme_minimal()

# ReshapviridisLite# Reshape the SHAP values into a wide format for heatmap
shap_wide <- dcast(shapley_plot_data, row_id ~ feature, value.var = "phi")

# Create a force plot for a single observation
single_shap <- shapley_plot_data[shapley_plot_data$row_id == 1, ]  # Example for the first observation

ggplot(single_shap, aes(x = feature, y = phi, fill = phi > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Negative Impact", "Positive Impact")) +
  labs(x = "Feature", y = "SHAP Value", title = "SHAP Force Plot for Observation 1") +
  coord_flip() +  # Flip for horizontal bars
  theme_minimal()

# Define a function to create a dependence plot for a given feature, with an option to color by another variable
create_shap_dependence_plot <- function(data, feature_name, color_var = NULL) {
  # Determine if the color variable is continuous or discrete
  if (!is.null(color_var) && is.numeric(data[[color_var]])) {
    color_scale <- scale_color_viridis_c(option = "C")  # Continuous scale
  } else {
    color_scale <- scale_color_viridis_d(option = "C")  # Discrete scale
  }
  
  # Create a base plot
  plot <- ggplot(data[data$feature == feature_name, ], 
                 aes_string(x = "value", y = "phi", color = if (!is.null(color_var)) color_var else "feature")) +
    geom_point(alpha = 0.6) + 
    color_scale + 
    labs(x = paste("Value of", feature_name), 
         y = "SHAP Value", 
         title = paste("SHAP Dependence Plot for", feature_name)) + 
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"))
  
  return(plot)
}

# Example of feature list you want to iterate over
feature_list <- c("max_daylength", "P",  "precip", "land_shrubland_grassland", 
                  "elevation_mean_m", "snow_cover", "rocks_volcanic")

# Iterate through the features and create a plot for each one, without specifying another variable for color
for (feature in feature_list) {
  print(create_shap_dependence_plot(shapley_plot_data, feature))  
}
