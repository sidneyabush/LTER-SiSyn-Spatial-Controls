# -------------------------------
# 1. Load Packages & Set Up Environment
# -------------------------------
librarian::shelf(iml, ggplot2, dplyr, tidyr, factoextra, cluster, colorspace, scales, fastshap)
rm(list = ls())

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
# 3. Clustering
# -------------------------------
cluster_vars <- c("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")
cluster_data <- kept_drivers %>% select(all_of(cluster_vars))

scaled_cluster_data <- cluster_data %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))

set.seed(123)
p2 <- fviz_nbclust(scaled_cluster_data, kmeans, method = "silhouette", k.max = 20)
print(p2)

kmeans_result <- kmeans(scaled_cluster_data, iter.max = 50, nstart = 50, centers = 3)
kept_drivers$cluster <- as.factor(kmeans_result$cluster)

# -------------------------------
# 4. Silhouette Plot (with Original Settings)
# -------------------------------
sil <- silhouette(kmeans_result$cluster, dist(scaled_cluster_data, method = "euclidean")^2)

cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sil_data <- fviz_silhouette(sil)$data  # Extract silhouette plot data
sil_data <- sil_data[sil_data$cluster != "avg.width", ]  # Remove the average width row

sil_plot <- ggplot(sil_data, aes(x = factor(cluster), y = sil_width, fill = factor(cluster))) +
  geom_bar(stat = "identity") +
  labs(title = "FNConc Yearly", y = "Silhouette Width", x = "Sites") +
  theme_classic() +
  scale_fill_manual(values = cb_palette) +
  scale_color_manual(values = cb_palette) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right"
  )


print(sil_plot)

ggsave("FNConc_Yearly_Cluster_SilPlot.png", plot = sil_plot, width = 6, height = 4, dpi = 300, path = output_dir)

# -------------------------------
# 5. FNConc Distribution Boxplots per Cluster
# -------------------------------
lighten_color <- function(color, factor = 0.3) {
  col <- col2rgb(color) / 255
  col <- col + factor * (1 - col)
  rgb(col[1], col[2], col[3])
}

light_cb_palette <- sapply(cb_palette, lighten_color)
light_cluster_palette <- setNames(light_cb_palette, levels(kept_drivers$cluster))

drivers_subset <- drivers_df %>% select(Stream_ID, FNConc)
all_data <- bind_cols(kept_drivers, drivers_subset)
all_data$cluster <- as.factor(all_data$cluster)

dist <- ggplot(all_data, aes(x = cluster, y = FNConc, fill = cluster)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = cluster), alpha = 0.3, width = 0.2) +
  scale_fill_manual(values = cb_palette) +
  scale_color_manual(values = light_cluster_palette) +
  labs(title = "FNConc Yearly - Cluster Distribution", x = "Cluster", y = "FNConc") +
  theme_classic() +
  theme(legend.position = "none")

print(dist)

ggsave("FNConc_Yearly_Cluster_Boxplot.png", plot = dist, width = 6, height = 4, dpi = 300, path = output_dir)

# -------------------------------
# 6. SHAP Analysis
# -------------------------------
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)
    predict(object, newdata = newdata)
  }
  fastshap::explain(object = model, X = kept_drivers %>% select(-cluster), pred_wrapper = custom_predict, nsim = sample_size)
}

shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)

base_colors <- c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73")

# -------------------------------
# 7. Generate SHAP Feature Importance Plots per Cluster
# -------------------------------
generate_feature_importance_plot <- function(cluster_id, shap_values, kept_drivers, output_dir) {
  shap_cluster <- as.data.frame(shap_values)[which(kept_drivers$cluster == cluster_id), , drop = FALSE]
  
  overall_feature_importance <- shap_cluster %>%
    summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  pdf(sprintf("%s/SHAP_FNConc_Ave_Cluster_%s_Variable_Importance.pdf", output_dir, cluster_id), width = 10, height = 8)
  ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = base_colors[[as.character(cluster_id)]]) +
    coord_flip() +
    labs(title = paste("Feature Importance for Cluster", cluster_id)) +
    theme_classic()
  dev.off()
}

# -------------------------------
# 8. Generate SHAP Dot Plots per Cluster
# -------------------------------
generate_shap_dot_plot <- function(cluster_id, shap_values, kept_drivers, output_dir) {
  cluster_indices <- which(kept_drivers$cluster == cluster_id)
  shap_values_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE]
  
  shap_long <- shap_values_df %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "shap_value")
  
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  pdf(sprintf("%s/SHAP_FNConc_Ave_Cluster_%s_Dot_Plot.pdf", output_dir, cluster_id), width = 9, height = 8)
  ggplot(shap_long, aes(x = shap_value, y = feature)) +
    geom_point(alpha = 0.6, color = base_colors[[as.character(cluster_id)]]) +
    # geom_hline(xintercept = 0, linetype = "dashed", color = "grey30") +
    labs(title = paste("SHAP Dot Plot for Cluster", cluster_id)) +
    theme_minimal()
  dev.off()
}

# -------------------------------
# 9. Generate All SHAP Plots
# -------------------------------
unique_clusters <- unique(kept_drivers$cluster)

lapply(unique_clusters, generate_feature_importance_plot, shap_values, kept_drivers, output_dir)
lapply(unique_clusters, generate_shap_dot_plot, shap_values, kept_drivers, output_dir)
