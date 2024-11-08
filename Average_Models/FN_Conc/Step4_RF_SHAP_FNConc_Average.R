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

# Define a function to save RF variable importance plot as a PDF
save_rf_importance_plot <- function(rf_model, output_dir) {
  pdf(sprintf("%s/RF_variable_importance.pdf", output_dir), width = 8, height = 6)
  randomForest::varImpPlot(rf_model, main = "RF Variable Importance - Average FN Concentration", col = "darkblue")
  dev.off()
}

# Define a function to save the linear model (LM) plot as a PDF
save_lm_plot <- function(rf_model, observed, output_dir) {
  pdf(sprintf("%s/RF_lm_plot.pdf", output_dir), width = 8, height = 8)
  plot(rf_model$predicted, observed, pch = 16, cex = 1.5,
       xlab = "Predicted", ylab = "Observed", main = "Observed vs Predicted - Average FN Concentration",
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2)
  legend("topleft", bty = "n", cex = 1.5, legend = paste("R² =", format(mean(rf_model$rsq), digits = 3)))
  legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model$mse), digits = 3)))
  dev.off()
}

# Set the output directory path for saving PDFs
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Average_Model/FNConc"

# Read in and tidy data ----
# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

drivers_df <- read.csv("AllDrivers_Harmonized_20241108_WRTDS_MD_KG_NP_FNConc_silicate_weathering.csv") %>%
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
raw_P <- read.csv("AllDrivers_Harmonized_20241108_WRTDS_MD_KG_rawNP_FNConc.csv") %>%
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

# # Export the dataframe to a CSV file
# write.csv(drivers_df, "Final_Sites.csv", row.names = FALSE)

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

pdf(sprintf("%s/correlation_plot.pdf", output_dir), width = 10, height = 10)
corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
dev.off()

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
tuneRF(drivers_df[, numeric_drivers], drivers_df[, 1], ntreeTry = 2000, stepFactor = 1, improve = 0.5, plot = FALSE)

# Run initial RF using tuned parameters ----
set.seed(123)
rf_model1 <- randomForest(med_si ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 2000, mtry = 10)

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
rf_model2 <- randomForest(rf_formula, data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 2000, mtry = 6)

# Visualize output for rf_model2
print(rf_model2)
randomForest::varImpPlot(rf_model2)

# Generate plots comparing predicted vs observed ----
lm_plot <- plot(rf_model2$predicted, drivers_df$med_si, pch = 16, cex = 1.5,
                xlab = "Predicted", ylab = "Observed", main = "All Spatial Drivers - FN Concentration",
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model2$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))

# Save RF variable importance plot and LM plot for rf_model2
save_rf_importance_plot(rf_model2, output_dir)
save_lm_plot(rf_model2, drivers_df$med_si, output_dir)

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



### MAKE INTO A FUNCTION ---- 
# Set the output directory for plots
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Average_Model/FNConc"

# Required libraries
library(parallel)
library(doParallel)
library(foreach)
library(iml)
library(dplyr)
library(reshape2)
library(ggplot2)

# Set up a parallel backend for speed (adjust cores as needed)
num_cores <- detectCores() - 1  # Leave one core free for other tasks
registerDoParallel(cores = num_cores)

# Function to create shapley_plot_data without any subsetting
create_shapley_plot_data <- function(model, kept_drivers, drivers_df, sample_size = 30) {
  # Create the predictor object for the full dataset
  predictor <- Predictor$new(model = model, data = kept_drivers, y = drivers_df$med_si)
  
  # Parallel Shapley calculations for each observation
  set.seed(123)  # For reproducibility
  shapley_list <- foreach(i = 1:nrow(kept_drivers), .packages = 'iml') %dopar% {
    shap <- Shapley$new(predictor, x.interest = kept_drivers[i, ], sample.size = sample_size)
    shap$results
  }
  
  # Combine Shapley results into a single dataframe
  shapley_df <- do.call(rbind, shapley_list) %>%
    mutate(row_id = rep(1:nrow(kept_drivers), each = ncol(kept_drivers)))
  
  # Reshape kept_drivers for merging with Shapley values
  kept_drivers_melted <- melt(kept_drivers)
  kept_drivers_melted$row_id <- rep(1:nrow(kept_drivers), ncol(kept_drivers))
  
  # Merge Shapley values with feature values and drivers_df metadata
  shapley_plot_data <- shapley_df %>%
    left_join(kept_drivers_melted, by = c("feature" = "variable", "row_id")) %>%
    left_join(drivers_df %>% mutate(row_id = 1:nrow(.)), by = "row_id")
  
  return(shapley_plot_data)
}

# Generate shapley_plot_data for the entire dataset
shapley_plot_data <- create_shapley_plot_data(rf_model2, kept_drivers, drivers_df)

# Create and save all required plots for full Shapley data, with optional color combinations for dependence plots
create_all_shapley_plots <- function(shap_data, output_file, color_vars = NULL) {  # Accept color_vars as an argument
  # Filter out silicate_weathering values > 20
  shap_data <- shap_data %>%
    filter(silicate_weathering <= 20)
  dev.off()
  
  # Open a single PDF file for all plots
  pdf(output_file, width = 8, height = 8)
  
  # 1. Overall Feature Importance Plot (based on mean absolute SHAP values)
  overall_feature_importance <- shap_data %>%
    group_by(feature) %>%
    summarise(importance = mean(abs(phi))) %>%
    arrange(desc(importance))
  
  overall_importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value", title = "Overall Feature Importance - Average FN Concentration") +
    theme_minimal()
  
  # Print plot to the PDF
  print(overall_importance_plot)
  
  # Reorder feature levels for the SHAP Summary Plot based on importance
  shap_data <- shap_data %>%
    mutate(feature = factor(feature, levels = rev(overall_feature_importance$feature)))
  
  # 2. SHAP Summary Plot with low-to-high coloring (normalized for each feature)
  shap_data_normalized <- shap_data %>%
    group_by(feature) %>%
    mutate(normalized_value = (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE)))
  
  shap_summary_plot <- ggplot(shap_data_normalized, aes(x = phi, y = feature)) + 
    geom_point(aes(color = normalized_value), alpha = 0.6) + 
    scale_color_gradient(low = "blue", high = "red", name = "Feature Value", breaks = c(0, 1), labels = c("Low", "High")) +
    labs(x = "SHAP Value", y = NULL, title = "SHAP Summary Plot - Average FN Concentration") + 
    theme_bw() + 
    theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), 
          plot.title = element_text(size = 16, face = "bold"))
  
  # Print plot to the PDF
  print(shap_summary_plot)
  
  # 3. Positive/Negative SHAP Impact Plot
  pos_neg_summary <- shap_data %>%
    group_by(feature) %>%
    summarise(mean_phi = mean(phi)) %>%
    mutate(feature = factor(feature, levels = rev(overall_feature_importance$feature)))
  
  pos_neg_plot <- ggplot(pos_neg_summary, aes(x = feature, y = mean_phi, fill = mean_phi > 0)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("red", "blue"), labels = c("Negative Impact", "Positive Impact")) +
    labs(x = "Feature", y = "Mean SHAP Value", title = "Overall SHAP Impact by Feature - Average FN Concentration") +
    coord_flip() +
    theme_minimal()
  
  # Print plot to the PDF
  print(pos_neg_plot)
  
  # 4. SHAP Dependence Plots for each feature with all combinations of coloring variables
  for (feature_name in unique(shap_data$feature)) {
    for (color_var in color_vars) {
      aes_mapping <- if (color_var %in% names(shap_data)) {
        aes(x = value, y = phi, color = .data[[color_var]])
      } else {
        aes(x = value, y = phi)
      }
      
      dependence_plot <- ggplot(shap_data[shap_data$feature == feature_name, ], aes_mapping) +
        geom_point(alpha = 0.6) + 
        labs(x = paste("Value of", feature_name), y = "SHAP Value", title = paste("Average FN Concentration SHAP Dependence Plot for", feature_name)) + 
        theme_minimal() +
        (if (color_var %in% names(shap_data)) scale_color_viridis_c(name = color_var) else NULL)
      
      # Print plot to the PDF
      print(dependence_plot)
    }
  }
  
  # Close the PDF file
  dev.off()
}

# Run the function to create all plots in a single PDF file, coloring dependence plots by specified variables (e.g., "precip", "temp")
color_vars <- c("drainage_area", "snow_cover", "precip", 
                "evapotrans", "temp", "npp", "permafrost", "green_up_day",
                "rocks_volcanic", "NOx", "P", "max_daylength", "silicate_weathering", "q_95", "q_5")  # List of features to color by
output_file <- sprintf("%s/all_shapley_plots.pdf", output_dir)  # Specify the output file path
create_all_shapley_plots(shapley_plot_data, output_file, color_vars)


# BASEMENT -------------------

# # Define plot parameters for specific features
# plot_parameters <- list(
#   list(var = "P", threshold = 0.1, above_threshold = FALSE, color_var = "precip"),
#   list(var = "P", threshold = 0.1, above_threshold = TRUE, color_var = "precip"),
#   list(var = "max_daylength", threshold = 17, above_threshold = FALSE, color_var = "precip"),
#   list(var = "max_daylength", threshold = 17, above_threshold = TRUE, color_var = "precip"),
#   list(var = "land_shrubland_grassland", threshold = 50, above_threshold = FALSE, color_var = "precip"),
#   list(var = "land_shrubland_grassland", threshold = 50, above_threshold = TRUE, color_var = "precip"),
#   list(var = "silicate_weathering", threshold = 0.25, above_threshold = FALSE, color_var = "precip"),
#   list(var = "silicate_weathering", threshold = 0.25, above_threshold = TRUE, color_var = "precip")
# )
# 
# 
# 
# # Generate plots based on specified plot parameters
# for (params in plot_parameters) {
#   subset_shapley_plot(
#     shap_data = shapley_plot_data,
#     feature_name = params$var,
#     threshold = params$threshold,
#     above_threshold = params$above_threshold,
#     color_var = params$color_var,
#     output_dir = output_dir
#   )
# }
# 


