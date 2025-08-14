###############################################################################
# Figure 2 with Test-Train-Cross-Validation Performance (Panels A & B)
###############################################################################

# 1. Packages & theme
library(iml)
library(ggplot2)
library(dplyr)
library(tidyr)
library(randomForest)
library(tibble)
library(scales)
library(cowplot)    
theme_set(
  theme_classic(base_size = 20) +
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA),
      plot.tag          = element_text(size = 22),
      plot.title        = element_text(size = 24, vjust = 4)  
    )
)

# 2. Clear & paths
rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
fm <- "Final_Models"; od <- "Final_Figures"
dir.create(od, recursive = TRUE, showWarnings = FALSE)

# 3. Load data
pred_GenConc  <- read.csv(file.path(fm, "Predictions_GenConc.csv"))
pred_GenYield <- read.csv(file.path(fm, "Predictions_GenYield.csv"))
load(file.path(fm, "GenConc_Yearly_shap_values_recent30.RData"));  SV_Gen  <- shap_values_GenConc
load(file.path(fm, "GenYield_Yearly_shap_values_recent30.RData")); SV_FY  <- shap_values_GenYield
load(file.path(fm, "GenConc_Yearly_kept_drivers.RData"));  KD_Gen  <- kept_drivers_GenConc
load(file.path(fm, "GenYield_Yearly_kept_drivers.RData")); KD_FY  <- kept_drivers_GenYield

# 4. Recode & scale setup
recode_map <- setNames(
  # Pretty Labels:
  c("N","P","NPP","ET","Greenup Day","Precip","Temp","Snow Cover","Permafrost",
    "Elevation","Basin Slope","Flashiness (RBI)","Recession Curve Slope",
    "Land: Bare","Land: Cropland","Land: Forest","Land: Grass & Shrub",
    "Land: Ice & Snow","Land: Impervious","Land: Salt Water","Land: Tidal Wetland",
    "Land: Water Body","Land: Wetland Marsh","Rock: Volcanic","Rock: Sedimentary",
    "Rock: Carbonate Evaporite","Rock: Metamorphic","Rock: Plutonic"),
  
  # Variable Names from the DataFrame:
  c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
    "snow_cover","permafrost","elevation","basin_slope","RBI",
    "recession_slope","land_Bare","land_Cropland","land_Forest",
    "land_Grassland_Shrubland","land_Ice_Snow","land_Impervious",
    "land_Salt_Water","land_Tidal_Wetland","land_Water","land_Wetland_Marsh",
    "rocks_volcanic","rocks_sedimentary","rocks_carbonate_evaporite",
    "rocks_metamorphic","rocks_plutonic")
)

kept_GenConc_scaled  <- KD_Gen  %>% 
  mutate(P = log10(P)) %>% 
  mutate(across(everything(), ~ rescale(., to=c(0,1))))

kept_GenYield_scaled <- KD_FY %>% 
  mutate(NOx = log10(NOx)) %>% 
  # NOx=log10(NOx), P = log10(P)) %>% 
  mutate(across(everything(), ~ rescale(., to=c(0,1))))

gmin <- 0; gmax <- 1

# 5. Dot‐plot
dot_plot <- function(SV, KD_s) {
  shap_df <- as.data.frame(SV) %>% 
    mutate(id = row_number()) %>% 
    pivot_longer(-id, names_to = "feature", values_to = "shap")
  
  val_df  <- KD_s %>% 
    mutate(id = row_number()) %>%
    pivot_longer(-id, names_to = "feature", values_to = "val")
  
  df <- left_join(shap_df, val_df, by = c("id","feature")) %>%
    mutate(pretty = recode(feature, !!!recode_map)) %>% 
    filter(!is.na(pretty))
  
  ord <- df %>% 
    group_by(pretty) %>% 
    summarize(m=mean(abs(shap))) %>% 
    arrange(desc(m)) %>% 
    pull(pretty)
  
  df$pretty <- factor(df$pretty, levels=rev(ord))
  
  ggplot(df, aes(shap, pretty)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_jitter(aes(fill= val), shape = 21, color = "darkgray",
                height = 0.2, size = 2.7, alpha = 0.9) +
    scale_fill_gradient(
      low = "white", high = "black", limits = c(gmin, gmax),
      name = "Scaled Value",
      guide = guide_colourbar(
        direction      = "horizontal",      
        title.position = "right",           
        title.hjust    = -0.5,     
        title.vjust    = 1,               
        barwidth       = unit(15, "lines"),
        barheight      = unit(1.5, "lines"),
        label.theme    = element_text(size = 16)
      )
    ) +
    labs(x = NULL,y = NULL) +
    theme(legend.position = "right",
          legend.direction = "horizontal",
          legend.margin   = ggplot2::margin(t = 2, r = 0, b = 2, l = 0, unit = "pt"))
}

# 6. Bar‐plot
bar_plot <- function(SV) {
  bs <- as.data.frame(SV) %>%
    pivot_longer(everything(), names_to = "feature", values_to = "shap") %>%
    group_by(feature) %>%
    summarize(m = mean(abs(shap), na.rm = TRUE)) %>%
    mutate(pretty = recode(feature, !!!recode_map)) %>%
    filter(!is.na(pretty)) %>%
    arrange(desc(m))
  
  bs$pretty <- factor(bs$pretty, levels = rev(bs$pretty))
  
  ggplot(bs, aes(x = pretty, y = m)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(
      expand = expansion(mult = c(0.03, 0.25))
    ) +
    labs(x = NULL, y = "Mean Absolute SHAP Value")
}


# 7. Panels A & B
metrics_GenConc <- pred_GenConc %>%
  group_by(subset) %>%
  summarize(R2 = cor(predicted, observed)^2,
            pRMSE = sqrt(mean((predicted - observed)^2)) / mean(observed) * 100,
            .groups = "drop")
Gen_x <- range(pred_GenConc$predicted)
Gen_y <- range(pred_GenConc$observed)
Gen_r <- diff(Gen_y)

# # Base colors
# subset_cols <- c(
#   "older70"  = "gray55",  
#   "recent30" = "#C0805B",  
#   "unseen10" = "#376A5A"   
# )
# 
# subset_ann_cols <- c(
#   "older70"  = "#5A5A5A",     
#   "recent30" = "#B45A3E",     
#   "unseen10" = "#2E7F6B"      
# )

# Base colors
subset_cols <- c(
  "older70"  = "gray69",  
  "recent30" = "#b9d7ef",  
  "unseen10" = "#525693"   
)

subset_ann_cols <- c(
  "older70"  = "gray50",  
  "recent30" = "#6ea8d3",  # or "#356b9c"
  "unseen10" = "#525693"   
)



# Transparent fills for interior
subset_fills <- scales::alpha(subset_cols, 0.35)

# Shapes: circle, triangle, square (21–25 = fillable shapes)
subset_shapes <- c(
  "older70"  = 21,  # Circle
  "recent30" = 24,  # Triangle
  "unseen10" = 22   # Square (instead of diamond)
)

# Sizes per group
subset_sizes <- c(
  "older70"  = 5,
  "recent30" = 3.5,
  "unseen10" = 4.1
)

subset_labs <- c(
  "older70"  = "Training",
  "recent30" = "Testing",
  "unseen10" = "Cross-Validation"
)

# Desired order
subset_levels <- c("older70", "recent30", "unseen10")

# Ensure correct type
pred_GenConc$subset <- factor(pred_GenConc$subset, levels = names(subset_cols))
# Panel A y positioning using expanded range
a_y_upper <- max(Gen_y) + 0.02 * diff(Gen_y)
a_y_base  <- a_y_upper - 0.05 * diff(Gen_y)  # 12% below the top

A <- ggplot(pred_GenConc, aes(predicted, observed)) +
  geom_point(
    aes(
      color = subset,
      fill  = subset,
      shape = subset,
      size  = subset
    ),
    stroke = 0.7
  ) +
  geom_abline(linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = subset_cols, labels = subset_labs, name = NULL) +
  scale_fill_manual(values = subset_fills, labels = subset_labs, name = NULL) +
  scale_shape_manual(values = subset_shapes, labels = subset_labs, name = NULL) +
  scale_size_manual(values = subset_sizes, labels = subset_labs, name = NULL) +
  guides(
    color = guide_legend(
      override.aes = list(
        shape = unname(subset_shapes),
        size  = unname(subset_sizes),
        stroke = 1.2,
        fill   = unname(subset_fills),
        color  = unname(subset_cols)
      ),
      order = 1
    ),
    shape = guide_legend(order = 1),
    fill  = "none",
    size  = "none"
  ) +
  annotate("text",
           x = Gen_x[1] + 0.02 * diff(Gen_x),
           y = a_y_base,
           label = sprintf("R² = %.3f, pRMSE = %.1f%%", metrics_GenConc$R2[1], metrics_GenConc$pRMSE[1]),
           hjust = 0, size = 6.5,
           color = subset_ann_cols[["older70"]]) +
  annotate("text",
           x = Gen_x[1] + 0.02 * diff(Gen_x),
           y = a_y_base - 0.08 * diff(Gen_y),
           label = sprintf("R² = %.3f, pRMSE = %.1f%%", metrics_GenConc$R2[2], metrics_GenConc$pRMSE[2]),
           hjust = 0, size = 6.5,
           color = subset_ann_cols[["recent30"]]) +
  annotate("text",
           x = Gen_x[1] + 0.02 * diff(Gen_x),
           y = a_y_base - 2 * 0.08 * diff(Gen_y),
           label = sprintf("R² = %.3f, pRMSE = %.1f%%", metrics_GenConc$R2[3], metrics_GenConc$pRMSE[3]),
           hjust = 0, size = 6.5,
           color = subset_ann_cols[["unseen10"]]) +
  labs(x = "Predicted", y = "Observed", title = "Concentration", tag = "A") +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.2))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  theme(plot.margin = unit(c(5, 5, 5, 0), "pt"))

metrics_GenYield <- pred_GenYield %>%
  group_by(subset) %>%
  summarize(R2 = cor(predicted, observed)^2, 
            pRMSE = sqrt(mean((predicted-observed)^2)) / mean(observed) * 100, 
            .groups="drop")
fy_x <- range(pred_GenYield$predicted)
fy_y <- range(pred_GenYield$observed)
fy_r <- diff(fy_y)


pred_GenYield$subset <- factor(pred_GenYield$subset, levels = names(subset_cols))
# Panel B y positioning using expanded range
b_y_upper <- max(fy_y) + 0.02 * diff(fy_y)
b_y_base  <- b_y_upper - 0.05 * diff(fy_y)

B <- ggplot(pred_GenYield, aes(predicted, observed)) +
  geom_point(
    aes(
      color = subset,
      fill  = subset,
      shape = subset,
      size  = subset
    ),
    stroke = 0.7
  ) +
  geom_abline(linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = subset_cols, labels = subset_labs, name = NULL) +
  scale_fill_manual(values = subset_fills, labels = subset_labs, name = NULL) +
  scale_shape_manual(values = subset_shapes, labels = subset_labs, name = NULL) +
  scale_size_manual(values = subset_sizes, labels = subset_labs, name = NULL) +
  guides(
    color = guide_legend(
      override.aes = list(
        shape  = unname(subset_shapes),
        size   = unname(subset_sizes),
        stroke = 1.2,
        fill   = unname(subset_fills),
        color  = unname(subset_cols)
      ),
      order = 1
    ),
    shape = guide_legend(order = 1),
    fill  = "none",
    size  = "none"
  ) +
  annotate("text",
           x = fy_x[1] + 0.02 * diff(fy_x),
           y = b_y_base,
           label = sprintf("R² = %.3f, pRMSE = %.1f%%", metrics_GenYield$R2[1], metrics_GenYield$pRMSE[1]),
           hjust = 0, size = 6.5,
           color = subset_ann_cols[["older70"]]) +
  annotate("text",
           x = fy_x[1] + 0.02 * diff(fy_x),
           y = b_y_base - 0.08 * diff(fy_y),
           label = sprintf("R² = %.3f, pRMSE = %.1f%%", metrics_GenYield$R2[2], metrics_GenYield$pRMSE[2]),
           hjust = 0, size = 6.5,
           color = subset_ann_cols[["recent30"]]) +
  annotate("text",
           x = fy_x[1] + 0.02 * diff(fy_x),
           y = b_y_base - 2 * 0.08 * diff(fy_y),
           label = sprintf("R² = %.3f, pRMSE = %.1f%%", metrics_GenYield$R2[3], metrics_GenYield$pRMSE[3]),
           hjust = 0, size = 6.5,
           color = subset_ann_cols[["unseen10"]]) +
  labs(x = "Predicted", y = NULL, title = "Yield", tag = "B") +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.2))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  theme(plot.margin = unit(c(5, 5, 5, 0),"pt"))

# 7a. row1 + subset legend
row1 <- plot_grid(
  A + theme(legend.position="none"),
  B + theme(legend.position="none"),
  ncol = 2, align = "h", axis = "tblr", rel_widths=c(1, 1)
)

leg1 <- get_legend(
  A + theme(
    legend.position    = "right",
    legend.direction   = "horizontal",
    legend.key.width   = unit(2,   "lines"),    
    legend.key.height  = unit(1.2, "lines"),    
    legend.text        = element_text(size = 17)
  )
)

# 8. row2
row2 <- plot_grid(
  bar_plot(SV_Gen) + labs(tag = "C"),
  bar_plot(SV_FY) + labs(tag = "D"),
  ncol = 2, align = "h", axis = "tblr", rel_widths = c(1, 1)
)

# 9. row3 + scale‐bar legend
E <- dot_plot(SV_Gen, kept_GenConc_scaled) +
  labs(tag = "E") +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.25))) +
  theme(plot.margin = unit(c(5, 5, 5, 5), "pt"),
        legend.position = "none")

F <- dot_plot(SV_FY, kept_GenYield_scaled) +
  labs(tag = "F") +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.25))) +
  theme(plot.margin = unit(c(5, 5, 5, 5), "pt"),
        legend.position = "none")

row3 <- plot_grid(E, F, ncol = 2, align = "h", axis = "tblr", rel_widths = c(1, 1))

leg2 <- get_legend(
  dot_plot(SV_Gen, kept_GenConc_scaled) +
    theme(legend.position ="right", legend.direction = "horizontal")
)

# 10. Final assemble
final_fig2 <- plot_grid(
  row1, leg1, row2, row3, leg2,
  ncol        = 1,
  rel_heights = c(1.2, 0.1, 1.15, 1.1, 0.15),
  align       = "v"
)

# 11. Save
ggsave(
  file.path(od, "Fig2_Global_GenConc_GenYield_multi.png"),
  final_fig2, width=17, height=19, dpi=300, bg="white"
)
