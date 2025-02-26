# -------------------------------
# 1. Load Packages & Set Up Environment
# -------------------------------
librarian::shelf(
  cowplot, ggplot2, dplyr, tidyr, factoextra, cluster, colorspace, scales,
  fastshap, RColorBrewer, grid, patchwork

)

rm(list = ls())  # Clear environment

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

# -------------------------------
# 2. Load Data & Model
# -------------------------------
load("FNConc_Yearly_rf_model2_full.RData")
load("FNConc_Yearly_kept_drivers_full.RData")
load("FNConc_Yearly_full.RData")
load("FNConc_Yearly_full_stream_ids.RData")

# -------------------------------
# 3. Prepare Data & Perform Clustering
# -------------------------------
# Use 5 driver variables for clustering
data <- kept_drivers %>%
  dplyr::select("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")

scaled_data <- data %>% 
  mutate(across(where(is.numeric), ~ scales::rescale(.)))

set.seed(123)
p2 <- factoextra::fviz_nbclust(scaled_data, kmeans, method = "silhouette", k.max = 20)
print(p2)

kmeans_result <- kmeans(scaled_data, iter.max = 50, nstart = 50, centers = 5)

scaled_data <- scaled_data %>%
  mutate(cluster = factor(kmeans_result$cluster, levels = c("1","2","3","4","5")))

# -------------------------------
# 4. Create Long-format Data for Box Plots
# -------------------------------
long_data <- scaled_data %>%
  pivot_longer(-cluster, names_to = "Driver", values_to = "Value") %>%
  mutate(
    Driver = factor(Driver, levels = c("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")),
    Driver = recode(Driver, 
                    "elevation" = "Elevation",
                    "basin_slope" = "Basin Slope",
                    "P" = "P",
                    "rocks_volcanic" = "Volcanic Rock",
                    "evapotrans" = "ET")
  )

# -------------------------------
# 5. Define a Named Color Vector Matching Your Palette
# -------------------------------
my_cluster_colors <- c(
  "1" = "#E69F00",  # Cluster 1 (orange)
  "2" = "#56B4E9",  # Cluster 2 (blue)
  "3" = "#009E73",  # Cluster 3 (green)
  "4" = "#F0E442",  # Cluster 4 (yellow)
  "5" = "#648FFF"   # Cluster 5 (purple)
)

# -------------------------------
# 6. Generate Box Plots (Left Panels; Hide Legend & Remove X-axis labels except last)
# -------------------------------
letters_vec <- LETTERS[1:length(unique(long_data$cluster))]  # Generate letters for each cluster

cluster_boxplots <- lapply(seq_along(sort(unique(long_data$cluster))), function(i) {
  cl <- sort(unique(long_data$cluster))[i]
  letter <- letters_vec[i]  # Assign corresponding letter
  
  p <- long_data %>%
    filter(cluster == cl) %>%
    ggplot(aes(x = Driver, y = Value, fill = cluster)) +
    geom_boxplot() +
    scale_fill_manual(values = my_cluster_colors, guide = "none") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = NULL, y = NULL) +
    annotate("text", x = 0.5, y = 1, label = letter, size = 8, fontface = "bold", hjust = 0, vjust = 1.5) + 
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
      axis.text.y = element_text(size = 20)
    )
  
  # Remove x-axis labels for all but the last plot
  if (cl != tail(sort(unique(long_data$cluster)), 1)) {
    p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  return(p)
})


# -------------------------------
# 7. Prepare Data for SHAP Dot Plots (Right Panels)
# -------------------------------
full_scaled <- kept_drivers %>%
  mutate(across(where(is.numeric), ~ scales::rescale(.))) %>%
  as.data.frame()
full_scaled$cluster <- factor(kmeans_result$cluster, levels = c("1","2","3","4","5"))

global_min <- min(full_scaled %>% select(-cluster), na.rm = TRUE)
global_max <- max(full_scaled %>% select(-cluster), na.rm = TRUE)

# -------------------------------
# 8. Define SHAP Dot Plot Function (with Recoding of Feature Names)
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
  
  # Recode feature names as desired
  shap_long$feature <- recode(shap_long$feature,
                              "elevation" = "Elevation",
                              "basin_slope" = "Basin Slope",
                              "P" = "Phosphorus",
                              "rocks_volcanic" = "Volcanic Rock",
                              "evapotrans" = "ET",
                              "land_urban_and_built_up_land" = "Land: Urban & Built-up")
  
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, fill = feature_value)) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "lightgray") +  # Use fill for color mapping
    scale_fill_gradientn(
      colors = c("lightgray", "gray", "gray4"),  
      name = NULL,
      limits = c(global_min, global_max)
    ) +
    labs(x = "SHAP Value", y = NULL, color = NULL, title = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_x_continuous(limits = c(global_shap_min, global_shap_max)) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20),
      legend.key.size = unit(1.5, "lines")
    )
  
  return(dot_plot)
}

# -------------------------------
# 9. Generate (or Load) SHAP Values & Set Global SHAP Limits
# -------------------------------
shap_file <- "shap_values_FNConc.rds"
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

# Remove x-axis labels (title and tick labels) from all but the last dot plot
for(i in seq_along(dot_plots)){
  if(i < length(dot_plots)){
    dot_plots[[i]] <- dot_plots[[i]] + 
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
}

# -------------------------------
# 11. Create a Column of Letter Grobs for the Box Plots
# -------------------------------
letters_vec <- LETTERS[1:length(cluster_boxplots)]
letter_grobs <- lapply(letters_vec, function(letter) {
  grid::textGrob(letter, gp = grid::gpar(fontsize = 24, fontface = "bold"))
})
letter_col <- wrap_plots(letter_grobs, ncol = 1)

# -------------------------------
# 12. Create the Shared Y-axis Label Grob
# -------------------------------
y_label_grob <- grid::textGrob("Scaled Value", rot = 90, gp = grid::gpar(fontsize = 20), x = unit(0.4, "npc"))
y_label_plot <- wrap_elements(full = y_label_grob, clip = FALSE)

# -------------------------------
# 13. Combine the Letter Column and Shared Y-axis Label into a Left Header Column
# -------------------------------
left_header <- letter_col | y_label_plot + 
  plot_layout(widths = c(0.01, 0.01))  # Adjust widths to reduce whitespace

# -------------------------------
# 14. Combine the Box Plots into a Single Column
# -------------------------------
left_col <- wrap_plots(cluster_boxplots, ncol = 1)

# -------------------------------
# 15. Combine the Left Header (Letters + Shared Y-axis Label) with the Box Plots
# -------------------------------
final_left <- left_header | left_col + 
  plot_layout(widths = c(0.05, 0.95))  # Adjust to control left column width

# -------------------------------
# 16. Combine Dot Plots into a Single Column
# -------------------------------
right_col <- wrap_plots(dot_plots, ncol = 1)

# -------------------------------
# 17. Combine the Left Column and Right Column Side-by-Side, Collecting the Dot Plot Legend
# -------------------------------
# Apply margin to each plot correctly
cluster_boxplots <- lapply(cluster_boxplots, function(p) {
  p + theme(plot.margin = margin(20, 20, 20, 20))  # Ensure ggplot2 is handling this
})

dot_plots <- lapply(dot_plots, function(p) {
  p + theme(plot.margin = margin(20, 20, 20, 20))  # Same here
})

# Combine the plots
final_combined_plot <- (wrap_plots(cluster_boxplots, ncol = 1) | wrap_plots(dot_plots, ncol = 1)) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = NULL,
    caption = NULL,
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 24, face = "bold"),
      plot.tag.position = "topleft"
    )
  )

# Add y-axis label separately
final_combined_plot <- final_combined_plot + labs(y = "Scaled Value")

# -------------------------------
# 18. Save the Final Figure
# -------------------------------
ggsave(
  filename = "Combined_Cluster_Boxplot_and_SHAP_DotPlots_5x2.png",
  plot = final_combined_plot,
  width = 14,
  height = 18,
  dpi = 300,
  path = output_dir
)
