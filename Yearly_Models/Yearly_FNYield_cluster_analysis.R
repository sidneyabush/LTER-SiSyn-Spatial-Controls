# -------------------------------
# 1. Load Packages & Set Up Environment
# -------------------------------
librarian::shelf(
  iml, ggplot2, dplyr, tidyr, factoextra, cluster, colorspace, scales,
  fastshap, patchwork, RColorBrewer
)

rm(list = ls())  # Clear environment

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"

# -------------------------------
# 2. Load Data & Model
# -------------------------------
load("FNYield_Yearly_rf_model2_full.RData")
load("FNYield_Yearly_kept_drivers_full.RData")
load("FNYield_Yearly_full.RData")
load("FNYield_Yearly_full_stream_ids.RData")

# -------------------------------
# 3. Prepare Data & Perform Clustering
# -------------------------------
data <- kept_drivers %>%
  dplyr::select("rocks_volcanic", "basin_slope", "land_urban_and_built_up_land", "temp", "land_shrubland_grassland")

scaled_data <- data %>% 
  mutate(across(where(is.numeric), ~ rescale(.)))

set.seed(123)
p2 <- fviz_nbclust(scaled_data, kmeans, method = "silhouette", k.max = 20)
print(p2)

kmeans_result <- kmeans(scaled_data, iter.max = 50, nstart = 50, centers = 3)

scaled_data <- scaled_data %>%
  mutate(cluster = factor(kmeans_result$cluster, levels = c("1","2","3")))

# -------------------------------
# 4. Create Long-format Data for Box Plots
# -------------------------------
long_data <- scaled_data %>%
  pivot_longer(-cluster, names_to = "Driver", values_to = "Value") %>%
  mutate(
    Driver = factor(Driver, levels = c("rocks_volcanic", "basin_slope", 
                                       "land_urban_and_built_up_land", "temp", "land_shrubland_grassland")),
    Driver = recode(Driver, 
                    "rocks_volcanic" = "Volcanic Rock",
                    "basin_slope" = "Basin Slope",
                    "land_urban_and_built_up_land" = "Land: Urban & Built Up",
                    "temp" = "Temperature",
                    "land_shrubland_grassland" = "Land: Shrubland & Grassland")
  )

# -------------------------------
# 5. Define a Named Color Vector
# -------------------------------
my_cluster_colors <- c(
  "1" = "#0072B2",
  "2" = "#CC79A7",
  "3" = "#D55E00"
)

# -------------------------------
# 6. Generate Box Plots (Hide Their Legend)
# -------------------------------
cluster_boxplots <- lapply(sort(unique(long_data$cluster)), function(cl) {
  long_data %>%
    filter(cluster == cl) %>%
    ggplot(aes(x = Driver, y = Value, fill = cluster)) +
    geom_boxplot() +
    # Hide legend for fill, so only the dot plots' color scale remains
    scale_fill_manual(values = my_cluster_colors, guide = "none") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = NULL, x = NULL, y = "Scaled Value") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
      axis.text.y = element_text(size = 18)
    )
})

# -------------------------------
# 7. Prepare Data for SHAP Dot Plots
# -------------------------------
full_scaled <- kept_drivers %>%
  mutate(across(where(is.numeric), ~ rescale(.))) %>%
  as.data.frame()
full_scaled$cluster <- factor(kmeans_result$cluster, levels = c("1","2","3","4","5"))

global_min <- min(full_scaled %>% select(-cluster), na.rm = TRUE)
global_max <- max(full_scaled %>% select(-cluster), na.rm = TRUE)

# -------------------------------
# 8. Dot Plot Function (With Single Color Scale)
# -------------------------------
generate_shap_dot_plot_obj <- function(cluster_id, shap_values, full_scaled, global_shap_min, global_shap_max) {
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  
  cluster_data <- full_scaled[cluster_indices, , drop = FALSE] %>% select(-cluster)
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  shap_values_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  shap_long <- shap_values_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  # Example recoding (adjust as needed)
  shap_long$feature <- recode(shap_long$feature,
                              "rocks_volcanic" = "Volcanic Rock",
                              "basin_slope" = "Basin Slope",
                              "land_urban_and_built_up_land" = "Land: Urban & Built Up",
                              "temp" = "Temperature",
                              "land_shrubland_grassland" = "Land: Shrubland & Grassland", 
                              "land_cropland" = "Land: Cropland",
                              "rocks_sedimentary" = "Sedimentary Rock",
                              "npp" = "NPP",
                              "precip" = "Precipitation",
                              "land_forest_all" = "Land: Forest",
                              "rocks_plutonic" = "Plutonic Rock",
                              "elevation" = "Elevation",
                              "permafrost" = "Permafrost",
                              "temp" = "Temperature",
                              "rocks_metamorphic" = "Metamorphic Rock",
                              "NOx" = "NOx",
                              "evapotrans" = "ET",
                              "rocks_carbonate_evaporite" = "Carbonite & Evaporite Rock",
                              "P"="P")

  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, color = feature_value)) +
    geom_point(alpha = 0.6, size = 3) +
    scale_color_gradientn(
      colors = c("#2b8cbe", "#f0f0f0", "#e6550d"),  # Blue - Gray - Orange
      limits = c(global_min, global_max)  # Adjust based on your SHAP value range
    ) +
    labs(title = NULL, x = "SHAP Value", y = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_x_continuous(limits = c(global_shap_min, global_shap_max)) +
    theme_classic() +
    theme(
      #axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 18)
    )
  
  return(dot_plot)
}

# -------------------------------
# 9. Generate (or Load) SHAP Values & Limits
# -------------------------------
shap_file <- "shap_values_FNYield.rds"
if (file.exists(shap_file)) {
  shap_values <- readRDS(shap_file)
  message("Loaded cached SHAP values from ", shap_file)
} else {
  generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
    predictors <- all.vars(model$terms)[-1]
    X_input <- kept_drivers %>% select(all_of(predictors))
    custom_predict <- function(object, newdata) {
      newdata <- as.data.frame(newdata)
      colnames(newdata) <- predictors
      predict(object, newdata = newdata, type = "response")
    }
    fastshap::explain(
      object = model,
      X = X_input,
      pred_wrapper = custom_predict,
      nsim = sample_size
    )
  }
  shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)
  saveRDS(shap_values, shap_file)
  message("Saved SHAP values to ", shap_file)
}

global_shap_min <- min(shap_values, na.rm = TRUE)
global_shap_max <- max(shap_values, na.rm = TRUE)

# -------------------------------
# 10. Generate Dot Plots for Each Cluster
# -------------------------------
unique_clusters <- sort(unique(full_scaled$cluster))
dot_plots <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(cl, shap_values, full_scaled, global_shap_min, global_shap_max)
})

# -------------------------------
# 11. Remove All X-axis Labels (Title & Ticks) from Rows 1-4
# -------------------------------
# For box plots
for(i in seq_along(cluster_boxplots)){
  if(i < length(cluster_boxplots)){
    cluster_boxplots[[i]] <- cluster_boxplots[[i]] + 
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
}

# For dot plots
for(i in seq_along(dot_plots)){
  if(i < length(dot_plots)){
    dot_plots[[i]] <- dot_plots[[i]] + 
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
}

# Assign letters (A, B, C, ...) to each box plot in the upper-left corner
letters_vec <- LETTERS[1:length(cluster_boxplots)]
for (i in seq_along(cluster_boxplots)) {
  cluster_boxplots[[i]] <- cluster_boxplots[[i]] +
    labs(tag = letters_vec[i]) +
    theme(
      plot.tag.position = c(0.01, 0.98),  # Adjust to move the label as needed
      plot.tag = element_text(face = "bold", size = 18)
    )
}

# -------------------------------
# 12. Combine Plots with patchwork & Collect the Legend
# -------------------------------
combined_plots <- list()
for (i in seq_along(unique_clusters)) {
  combined_plots[[2 * i - 1]] <- cluster_boxplots[[i]]  # left
  combined_plots[[2 * i]]     <- dot_plots[[i]]         # right
}

final_combined_plot <- wrap_plots(combined_plots, ncol = 2) +
  plot_layout(guides = "collect")  # Merge the dot plot legends into one

# Display the final layout
print(final_combined_plot)

# Save the final figure
ggsave(
  filename = "Combined_Cluster_Boxplot_and_SHAP_DotPlots_5x2.png",
  plot = final_combined_plot,
  width = 16,
  height = 20,
  dpi = 300,
  path = output_dir
)
