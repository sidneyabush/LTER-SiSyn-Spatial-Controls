# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(factoextra)
library(cluster)

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Clear environment
rm(list = ls())

# Read in and preprocess the data
train <- read.csv("train_data_stream_id.csv")

data <- train %>%
  dplyr::select("Stream_ID", "P", "snow_cover", "precip", "NOx", "rocks_volcanic", "basin_slope")

# Scale the selected numerical columns (excluding Stream_ID) and ensure correct column names
scaled_data <- data %>%
  mutate(across(-Stream_ID, ~ as.numeric(scale(.))))

# Set seed for reproducibility
set.seed(123)

# Perform silhouette method to determine optimal clusters
p2 <- fviz_nbclust(scaled_data %>% select(-Stream_ID), kmeans, method= "silhouette", k.max = 20)
p2

kmeans_result <- kmeans(scaled_data %>% select(-Stream_ID), iter.max = 50, nstart = 50, centers = 5)

# Add cluster assignments to the reg data
final_data <- data %>%
  mutate(cluster = as.factor(kmeans_result$cluster)) %>%
  dplyr::select(Stream_ID, cluster)

# Save to CSV file
write.csv(final_data, "cluster_assignments_AverageModel.csv", row.names = FALSE)

scaled_data <- scaled_data %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

# Define a colorblind-friendly palette
cb_palette <- c(
  "#E69F00",  
  "#56B4E9",  
  "#009E73",  
  "#D55E00",   
  "#CC79A7"  
  
)

#E69F00  # Orange
#56B4E9  # Sky Blue
#009E73  # Green
#F0E442  # Yellow
#0072B2  # Blue
#D55E00  # Vermilion
#CC79A7  # Reddish Purple


# Reshape data to long format for ggplot
long_data <- scaled_data %>%
  pivot_longer(-c(Stream_ID, cluster), names_to = "Driver", values_to = "Value") %>%
  mutate(
    Driver = factor(Driver, levels = c("P", "snow_cover", "precip", "NOx", "rocks_volcanic", "basin_slope")),
    Driver = recode(Driver, 
                    "P" = "P",
                    "snow_cover" = "Snow",
                    "precip" = "Precip",
                    "NOx" = "NOx",
                    "rocks_volcanic" = "Volcanic Rock",
                    "basin_slope" = "Basin Slope"
    )
  )

# Create box plots with facet_wrap
box_plot <- ggplot(long_data, aes(x = Driver, y = Value, fill = cluster)) +
  geom_boxplot() +
  facet_wrap(~cluster, scales = "free") +
  scale_fill_manual(values = cb_palette) +  # Apply colorblind-friendly colors
  labs(title = "Average Model", x = NULL, y = "Scaled Value") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels
    strip.text = element_text(size = 12, face = "bold"),  # Enlarge facet labels
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")  # Center & bold title
  )

# Compute silhouette scores
sil <- silhouette(kmeans_result$cluster, dist(scaled_data %>% select(-Stream_ID, -cluster), method = "euclidean")^2)

# Create silhouette plot
sil_plot <- fviz_silhouette(sil) +
  labs(title = "Average Model", y = "Silhouette Width", x = "Sites") +
  theme_classic() +
  scale_fill_manual(values = cb_palette) +  # Ensure consistent colors
  scale_color_manual(values = cb_palette) +  # Apply the same colors to silhouette plot
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold"),  # Enlarge facet labels
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")  # Center & bold title
  )

# Display both plots
print(box_plot)
print(sil_plot)
