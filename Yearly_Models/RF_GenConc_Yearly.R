# Load needed packages
librarian::shelf(remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot, mlbench, pROC, tree, dplyr,
                 plot.matrix, reshape2, rcartocolor, arsenal, googledrive, data.table, ggplot2, corrplot, pdp, 
                 iml, tidyr, viridis, parallel, doParallel, foreach)

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
    rf_model <- randomForest(GenConc ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = ntree_list[[i]])
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
cols_to_consider <- c("GenConc")
sd_limit <- 3
remove_outlier_rows <- function(data_to_filter, cols = cols_to_consider, limit = sd_limit) {
  z_scores <- sapply(data_to_filter[cols], function(data) abs((data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE)))
  return(data_to_filter[rowSums(z_scores > limit, na.rm = TRUE) == 0, ])
}

# Define a function to save RF variable importance plot as a PDF
save_rf_importance_plot <- function(rf_model, output_dir) {
  pdf(sprintf("%s/RF_variable_importance.pdf", output_dir), width = 8, height = 6)
  randomForest::varImpPlot(rf_model, main = "RF Variable Importance - Average Gen Concentration", col = "darkblue")
  dev.off()
}

# Define a function to save the linear model (LM) plot as a PDF
save_lm_plot <- function(rf_model, observed, output_dir) {
  pdf(sprintf("%s/RF_lm_plot.pdf", output_dir), width = 8, height = 8)
  plot(rf_model$predicted, observed, pch = 16, cex = 1.5,
       xlab = "Predicted", ylab = "Observed", main = "Observed vs Predicted - Average Gen Concentration",
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2)
  legend("topleft", bty = "n", cex = 1.5, legend = paste("R² =", format(mean(rf_model$rsq), digits = 3)))
  legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model$mse), digits = 3)))
  dev.off()
}

# Set the output directory path for saving PDFs
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/GenConc"

# Read in and tidy data ----
# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

drivers_df <- read.csv("AllDrivers_Harmonized_Yearly.csv") %>%
  select(-contains("Yield"), -contains("FN"), -contains("major"), -X, -Year, -Name, -ClimateZ) %>%
  dplyr::mutate_at(vars(19:34), ~replace(., is.na(.), 0)) %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  select(-Stream_ID) %>%
  drop_na()

drivers_df <- drivers_df %>% select(GenConc, everything())

# Remove outliers using custom function
# drivers_df <- remove_outlier_rows(drivers_df)

# Plot correlation between driver variables ----
numeric_drivers <- 2:33  # Indices for numeric drivers
driver_cor <- cor(drivers_df[, numeric_drivers])
corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = F)

pdf(sprintf("%s/correlation_plot.pdf", output_dir), width = 10, height = 10)
corrplot(driver_cor, type = "lower", pch.col = "black", tl.col = "black", diag = FALSE)
title("Annual Gen Si Concentration")  # Add title in the PDF
dev.off()

library(doParallel)
library(foreach)

# Set up parallel backend
cl <- makeCluster(detectCores() - 1)  # Use all but one core
registerDoParallel(cl)

test_numtree_average_parallel <- function(ntree_list) {
  MSE <- foreach(ntree = ntree_list, .combine = c, .packages = "randomForest") %dopar% {
    set.seed(123)
    rf_model <- randomForest(GenConc ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = ntree)
    rf_model$mse
  }
  return(MSE)
}

# Run parallel version
MSE_list <- test_numtree_average_parallel(c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000))

# Stop the cluster
stopCluster(cl)


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
rf_model1 <- randomForest(GenConc ~ ., data = drivers_df, importance = TRUE, proximity = TRUE, ntree = 200, mtry = 11)

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
x <- drivers_df[, !(colnames(drivers_df) == "GenConc")]
y <- drivers_df$GenConc

# Run RFE to select the best features ----
set.seed(123)
result_rfe <- rfe(x = x, y = y, sizes = c(1:size), rfeControl = control)

# Print RFE results
print(result_rfe)

# Put selected features into variable
new_rf_input <- paste(predictors(result_rfe), collapse = "+")

# Format those features into a formula for the optimized random forest model
rf_formula <- formula(paste("GenConc ~", new_rf_input))

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
lm_plot <- plot(rf_model2$predicted, drivers_df$GenConc, pch = 16, cex = 1.5,
                xlab = "Predicted", ylab = "Observed", main = "All Spatial Drivers - Gen Concentration",
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model2$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))

# Save RF variable importance plot and LM plot for rf_model2
save_rf_importance_plot(rf_model2, output_dir)
save_lm_plot(rf_model2, drivers_df$GenConc, output_dir)

# Save model and required objects for SHAP analysis
save(rf_model2, file = "GenConc_Yr_rf_model2.RData")
kept_drivers <- drivers_df[, colnames(drivers_df) %in% predictors(result_rfe)]
save(kept_drivers, file = "GenConc_Yr_kept_drivers.RData")
save(drivers_df, file = "GenConc_Yr_drivers_df.RData")

