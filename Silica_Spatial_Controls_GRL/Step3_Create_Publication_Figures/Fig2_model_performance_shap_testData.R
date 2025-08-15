###############################################################################
# Figure 2: Test-Train-Cross-Validation Performance & SHAP
###############################################################################
# Required inputs:
#   1) <fm>/Predictions_FNConc_split.csv
#   2) <fm>/Predictions_FNYield_split.csv
#   3) <fm>/FNConc_Yearly_shap_values_recent30_split.RData
#   4) <fm>/FNYield_Yearly_shap_values_recent30_split.RData
#   5) <fm>/FNConc_Yearly_kept_drivers_split.RData
#   6) <fm>/FNYield_Yearly_kept_drivers_split.RData
#
# Outputs created:
#   A) <od>/Fig2_Global_FNConc_FNYield_multi_split.png


# 1. Packages & theme
librarian::shelf(iml, ggplot2, dplyr, tidyr, randomForest, tibble, scales, cowplot)

theme_set(
  theme_classic(base_size = 20) +
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA),
      plot.tag          = element_text(size = 26, face = "plain"),  
      plot.title        = element_text(size = 24, vjust = 4)
    )
)

# 2. Clear & paths
rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
fm <- "Final_Models"; od <- "Final_Figures"
dir.create(od, recursive = TRUE, showWarnings = FALSE)

# 3. Define subset styling vectors
subset_levels <- c("older70", "recent30", "unseen10")
subset_cols   <- c(
  older70  = "gray40",
  recent30 = "#b9d7ef",
  unseen10 = "#525693"
)
subset_fills  <- scales::alpha(subset_cols, 0.35)
subset_shapes <- c(
  older70  = 21,
  recent30 = 24,
  unseen10 = 22
)
subset_sizes  <- c(
  older70  = 5,
  recent30 = 3.5,
  unseen10 = 4.1
)
subset_labs   <- c(
  older70  = "Training",
  recent30 = "Testing",
  unseen10 = "Cross-Validation"
)
subset_ann_cols <- c(
  older70  = "gray40",
  recent30 = "#6ea8d3",
  unseen10 = "#525693"
)

# 4. Load data
pred_FNConc  <- read.csv(file.path(fm, "Predictions_FNConc_split.csv"))
pred_FNYield <- read.csv(file.path(fm, "Predictions_FNYield_split.csv"))
load(file.path(fm, "FNConc_Yearly_shap_values_recent30_split.RData"));  SV_FN  <- shap_values_FNConc
load(file.path(fm, "FNYield_Yearly_shap_values_recent30_split.RData")); SV_FY  <- shap_values_FNYield
load(file.path(fm, "FNConc_Yearly_kept_drivers_split.RData"));  KD_FN  <- kept_drivers_FNConc
load(file.path(fm, "FNYield_Yearly_kept_drivers_split.RData")); KD_FY  <- kept_drivers_FNYield

# 5. Recode & scale setup
recode_map <- setNames(
  c("Log(N)","Log(P)","NPP","ET","Green-up day","Precip","Temp","Snow cover","Permafrost",
    "Elevation","Basin slope","RBI","RCS",
    "Bare land cover","Cropland cover","Forest cover","Grass & shrub cover",
    "Ice & snow cover","Impervious cover","Saltwater cover","Tidal wetland cover",
    "Open water cover","Wetland cover","Volcanic rock","Sedimentary rock",
    "Carbonate-evaporite rock","Metamorphic rock","Plutonic rock"),
  c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
    "snow_cover","permafrost","elevation","basin_slope","RBI",
    "recession_slope","land_Bare","land_Cropland","land_Forest",
    "land_Grassland_Shrubland","land_Ice_Snow","land_Impervious",
    "land_Salt_Water","land_Tidal_Wetland","land_Water","land_Wetland_Marsh",
    "rocks_volcanic","rocks_sedimentary","rocks_carbonate_evaporite",
    "rocks_metamorphic","rocks_plutonic")
)

kept_FNConc_scaled  <- KD_FN  %>% 
  mutate(P = log10(P)) %>% 
  mutate(across(everything(), ~ rescale(., to = c(0,1))))

kept_FNYield_scaled <- KD_FY %>% 
  mutate(NOx = log10(NOx)) %>% 
  mutate(across(everything(), ~ rescale(., to = c(0,1))))

gmin <- 0; gmax <- 1

# 6. Dot‐plot function
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
    summarize(m = mean(abs(shap)), .groups="drop") %>% 
    arrange(desc(m)) %>% 
    pull(pretty)
  
  df$pretty <- factor(df$pretty, levels = rev(ord))
  
  ggplot(df, aes(shap, pretty)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_jitter(aes(fill = val), shape = 21, color = "darkgray",
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
    labs(x = NULL, y = NULL) +
    theme(
      legend.position = "right",
      legend.direction = "horizontal",
      legend.margin   = ggplot2::margin(t = 2, r = 0, b = 2, l = 0, unit = "pt")
    )
}

# 7. Bar‐plot function
bar_plot <- function(SV) {
  bs <- as.data.frame(SV) %>%
    pivot_longer(everything(), names_to = "feature", values_to = "shap") %>%
    group_by(feature) %>%
    summarize(m = mean(abs(shap), na.rm = TRUE), .groups="drop") %>%
    mutate(pretty = recode(feature, !!!recode_map)) %>%
    filter(!is.na(pretty)) %>%
    arrange(desc(m))
  
  bs$pretty <- factor(bs$pretty, levels = rev(bs$pretty))
  
  ggplot(bs, aes(x = pretty, y = m)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.25))) +
    labs(x = NULL, y = "Mean Absolute SHAP Value")
}

# 8. Panels A & B

## A: Concentration
metrics_FNConc <- pred_FNConc %>%
  group_by(subset) %>%
  summarize(
    R2    = cor(predicted, observed)^2,
    RRMSE = sqrt(mean((predicted - observed)^2)) / mean(observed),
    .groups = "drop"
  ) %>%
  mutate(subset = factor(subset, levels = subset_levels)) %>%
  arrange(subset)

pred_FNConc$subset <- factor(pred_FNConc$subset, levels = subset_levels)

fn_x <- range(pred_FNConc$predicted, na.rm = TRUE)

# Put annotations near the top of the capped axis (0–20)
yr        <- c(0, 20)
a_y_upper <- yr[2] + 0.02 * diff(yr)   # top + 2% headroom
a_y_base  <- a_y_upper - 0.05 * diff(yr)  # first line 5% below top
line_gap  <- 0.08 * diff(yr)              # 8% spacing between lines

A <- ggplot(pred_FNConc, aes(predicted, observed)) +
  geom_point(aes(color = subset, fill = subset, shape = subset, size = subset),
             stroke = 0.7) +
  geom_abline(linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = subset_cols, labels = subset_labs, name = NULL) +
  scale_fill_manual(values = subset_fills, labels = subset_labs, name = NULL) +
  scale_shape_manual(values = subset_shapes, labels = subset_labs, name = NULL) +
  scale_size_manual(values = subset_sizes, labels = subset_labs, name = NULL) +
  guides(
    color = guide_legend(
      override.aes = list(
        shape = unname(subset_shapes),
        size  = 6,
        stroke= 1.2,
        fill  = unname(subset_fills),
        color = unname(subset_cols)
      ), order = 1
    ),
    shape = guide_legend(order = 1), fill = "none", size = "none"
  ) +
  annotate(
    "text",
    x = fn_x[1] + 0.02 * diff(fn_x),
    y = a_y_base,
    label = sprintf("R² = %.3f, RRMSE = %.3f", metrics_FNConc$R2[1], metrics_FNConc$RRMSE[1]),
    hjust = 0, size = 6.5, color = subset_ann_cols["older70"]
  ) +
  annotate(
    "text",
    x = fn_x[1] + 0.02 * diff(fn_x),
    y = a_y_base - 1 * line_gap,
    label = sprintf("R² = %.3f, RRMSE = %.3f", metrics_FNConc$R2[2], metrics_FNConc$RRMSE[2]),
    hjust = 0, size = 6.5, color = subset_ann_cols["recent30"]
  ) +
  annotate(
    "text",
    x = fn_x[1] + 0.02 * diff(fn_x),
    y = a_y_base - 2 * line_gap,
    label = sprintf("R² = %.3f, RRMSE = %.3f", metrics_FNConc$R2[3], metrics_FNConc$RRMSE[3]),
    hjust = 0, size = 6.5, color = subset_ann_cols["unseen10"]
  ) +
  labs(
    x = expression(paste("Predicted (", mg~L^{-1}, ")")),
    y = expression(paste("Observed (", mg~L^{-1}, ")")),
    title = "Concentration", tag = "a)"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.2))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  coord_cartesian(ylim = c(0, 20), xlim = c(0,20)) +
  theme(plot.margin = unit(c(5, 5, 5, 0), "pt"))

## B: Yield
metrics_FNYield <- pred_FNYield %>%
  group_by(subset) %>%
  summarize(
    R2    = cor(predicted, observed)^2,
    RRMSE = sqrt(mean((predicted - observed)^2)) / mean(observed),
    .groups = "drop"
  )

fy_x <- range(pred_FNYield$predicted)
fy_y <- range(pred_FNYield$observed)

pred_FNYield$subset <- factor(pred_FNYield$subset, levels = subset_levels)

b_y_upper <- max(fy_y) + 0.02 * diff(fy_y)
b_y_base  <- b_y_upper - 0.05 * diff(fy_y)

B <- ggplot(pred_FNYield, aes(predicted, observed)) +
  geom_point(aes(color = subset, fill = subset, shape = subset, size = subset),
             stroke = 0.7) +
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
      ), order = 1
    ),
    shape = guide_legend(order = 1), fill = "none", size = "none"
  ) +
  annotate(
    "text",
    x = fy_x[1] + 0.02 * diff(fy_x),
    y = b_y_base,
    label = sprintf("R² = %.3f, RRMSE = %.3f", metrics_FNYield$R2[1], metrics_FNYield$RRMSE[1]),
    hjust = 0, size = 6.5, color = subset_ann_cols["older70"]
  ) +
  annotate(
    "text",
    x = fy_x[1] + 0.02 * diff(fy_x),
    y = b_y_base - 0.08 * diff(fy_y),
    label = sprintf("R² = %.3f, RRMSE = %.3f", metrics_FNYield$R2[2], metrics_FNYield$RRMSE[2]),
    hjust = 0, size = 6.5, color = subset_ann_cols["recent30"]
  ) +
  annotate(
    "text",
    x = fy_x[1] + 0.02 * diff(fy_x),
    y = b_y_base - 2 * 0.08 * diff(fy_y),
    label = sprintf("R² = %.3f, RRMSE = %.3f", metrics_FNYield$R2[3], metrics_FNYield$RRMSE[3]),
    hjust = 0, size = 6.5, color = subset_ann_cols["unseen10"]
  ) +
  labs(
    x = expression(paste("Predicted (", kg~km^{-2}~yr^{-1}, ")")),
    y = expression(paste("Observed (", kg~km^{-2}~yr^{-1}, ")")),
    title = "Yield", tag = "b)"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.2))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  theme(plot.margin = unit(c(5, 5, 5, 0), "pt"))

# 9. Assemble & Save
row1 <- plot_grid(
  A + theme(legend.position="none"),
  B + theme(legend.position="none"),
  ncol = 2, align = "h", axis = "tblr", rel_widths = c(1, 1)
)

leg1 <- get_legend(
  A + theme(
    legend.position   = "right",
    legend.direction  = "horizontal",
    legend.key.width  = unit(1.5, "lines"),
    legend.key.height = unit(1.3, "lines"),
    legend.text       = element_text(size = 18)
  )
)

row2 <- plot_grid(
  bar_plot(SV_FN) + labs(tag = "c)"),
  bar_plot(SV_FY) + labs(tag = "d)"),
  ncol = 2, align = "h", axis = "tblr", rel_widths = c(1, 1)
)

row3 <- plot_grid(
  dot_plot(SV_FN, kept_FNConc_scaled) + labs(tag = "e)") + theme(legend.position="none"),
  dot_plot(SV_FY, kept_FNYield_scaled) + labs(tag = "f)") + theme(legend.position="none"),
  ncol = 2, align = "h", axis = "tblr", rel_widths = c(1, 1)
)

leg2 <- get_legend(
  dot_plot(SV_FN, kept_FNConc_scaled) +
    theme(legend.position = "right", legend.direction = "horizontal")
)

final_fig2 <- plot_grid(
  row1, leg1, row2, row3, leg2,
  ncol        = 1,
  rel_heights = c(1.15, 0.12, 1.15, 1.1, 0.15),
  align       = "v"
)

ggsave(
  file.path(od, "Fig2_Global_FNConc_FNYield_multi_split.png"),
  final_fig2, width = 17, height = 20, dpi = 300, bg = "white"
)
