# ############################################################
# Figure S1 — Gen vs FN (A,B) + OLS Predicted vs Observed (C,D)
# ############################################################
# Required inputs:
#   1) <harmonization_dir>/AllDrivers_Harmonized_Yearly_filtered_<record_length>_years.csv
#   2) <fm>/Predictions_GenConc.csv
#   3) <fm>/Predictions_GenYield.csv
#
# Outputs created:
#   A) <od>/FigS1_GenFN_and_OLS.png

librarian::shelf(dplyr, ggplot2, readr, patchwork, cowplot)

###### Paths ######
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
fm <- "Final_Models"
od <- "Final_Figures"
dir.create(od, recursive = TRUE, showWarnings = FALSE)

###### Load data for S1A/S1B (Gen vs FN) ######
record_length <- 5
infile <- file.path(
  "harmonization_files",
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years.csv", record_length)
)

df <- readr::read_csv(infile, show_col_types = FALSE) |>
  dplyr::select(GenConc, FNConc, GenYield, FNYield) |>
  dplyr::mutate(
    GenConc  = as.numeric(GenConc),
    FNConc   = as.numeric(FNConc),
    GenYield = as.numeric(GenYield),
    FNYield  = as.numeric(FNYield)
  )

###### Helpers ######
p_fmt <- function(p) {
  if (is.na(p)) return("= NA")
  if (p < 1e-16) return("< 1e-16")
  paste0("= ", formatC(p, format = "e", digits = 2))
}
r2_p_lm <- function(y, x) {
  ok <- stats::complete.cases(x, y)
  if (!any(ok)) return(list(r2 = NA_real_, p = NA_real_))
  s <- summary(stats::lm(y[ok] ~ x[ok]))
  list(r2 = s$r.squared, p = coef(s)[2, 4])
}
panel_genfn <- function(data, x, y, xlab, ylab, r2, pval) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_point(alpha = 0.35, size = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "lm", se = FALSE, color = "#6699CC") +
    coord_equal() +
    labs(x = xlab, y = ylab) +
    annotate("text", x = -Inf, y = Inf,
             label = sprintf("R² = %.2f\np %s", r2, p_fmt(pval)),
             hjust = -0.1, vjust = 1.3, size = 5) +
    # IMPORTANT: no titles/ticks/labels on top/right
    scale_x_continuous(sec.axis = dup_axis(name = NULL, breaks = NULL, labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL, breaks = NULL, labels = NULL)) +
    theme_classic(base_size = 11) +
    theme(
      axis.line             = element_line(color = "black"),
      axis.line.x.top       = element_line(color = "black"),
      axis.line.y.right     = element_line(color = "black"),
      axis.ticks.length.x.top   = grid::unit(0, "pt"),
      axis.ticks.length.y.right = grid::unit(0, "pt")
    )
}

###### S1A/S1B — Gen vs FN panels ######
conc_stats  <- r2_p_lm(df$FNConc,  df$GenConc)
yield_stats <- r2_p_lm(df$FNYield, df$GenYield)

S1A <- panel_genfn(
  df, GenConc, FNConc,
  xlab = expression("GenConc (mg Si L"^-1*")"),
  ylab = expression("FNConc (mg Si L"^-1*")"),
  r2 = conc_stats$r2, pval = conc_stats$p
) + labs(tag = "a)")

S1B <- panel_genfn(
  df, GenYield, FNYield,
  xlab = expression("GenYield (kg Si km"^-2*" yr"^-1*")"),
  ylab = expression("FNYield (kg Si km"^-2*" yr"^-1*")"),
  r2 = yield_stats$r2, pval = yield_stats$p
) + labs(tag = "b)")

###### S1C/S1D — OLS Predicted vs Observed (import like Fig 2) ######
pred_GenConc  <- read.csv(file.path(fm, "Predictions_GenConc.csv"))
pred_GenYield <- read.csv(file.path(fm, "Predictions_GenYield.csv"))

panel_ols <- function(df, xlab, ylab) {
  ok <- stats::complete.cases(df$predicted, df$observed)
  m  <- stats::lm(observed ~ predicted, data = df[ok, ])
  s  <- summary(m)
  r2 <- s$r.squared
  p  <- coef(s)[2, 4]
  ptxt <- if (is.na(p)) "NA" else if (p < 1e-16) "< 1e-16" else formatC(p, format = "e", digits = 2)
  
  ggplot(df, aes(x = predicted, y = observed)) +
    geom_point(color = "grey40", alpha = 0.5, size = 1.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
    geom_smooth(method = "lm", se = FALSE, color = "#6699CC", linewidth = 1.1) +
    coord_equal() +
    labs(x = xlab, y = ylab) +
    annotate("text", x = -Inf, y = Inf,
             label = sprintf("R² = %.2f\np = %s", r2, ptxt),
             hjust = -0.1, vjust = 1.3, size = 5) +
    scale_x_continuous(sec.axis = dup_axis(name = NULL, breaks = NULL, labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL, breaks = NULL, labels = NULL)) +
    theme_classic(base_size = 11) +
    theme(
      axis.line.x.top   = element_line(color = "black"),
      axis.line.y.right = element_line(color = "black"),
      axis.ticks.length.x.top   = grid::unit(0, "pt"),
      axis.ticks.length.y.right = grid::unit(0, "pt")
    )
}

S1C <- panel_ols(
  pred_GenConc,
  xlab = expression("Predicted (mg Si L"^-1*")"),
  ylab = expression("Observed (mg Si L"^-1*")")
) + labs(tag = "c)")

S1D <- panel_ols(
  pred_GenYield,
  xlab = expression("Predicted (kg Si km"^-2*" yr"^-1*")"),
  ylab = expression("Observed (kg Si km"^-2*" yr"^-1*")")
) + labs(tag = "d)")

###### Assemble & Save Figure S1 (Option A: responsive + strict hv align) ######
library(cowplot)

# override coord_equal() so panels can resize; identical margins help alignment
m <- ggplot2::margin(6, 6, 6, 6)
S1A_r <- S1A + coord_cartesian(expand = FALSE) + theme(plot.margin = m)
S1B_r <- S1B + coord_cartesian(expand = FALSE) + theme(plot.margin = m)
S1C_r <- S1C + coord_cartesian(expand = FALSE) + theme(plot.margin = m)
S1D_r <- S1D + coord_cartesian(expand = FALSE) + theme(plot.margin = m)

# align all four in one shot (both horizontal & vertical; lock tick baselines)
aligned <- cowplot::align_plots(
  S1A_r, S1B_r, S1C_r, S1D_r,
  align = "hv", axis = "tblr"
)

FigS1 <- cowplot::plot_grid(
  aligned[[1]], aligned[[2]],
  aligned[[3]], aligned[[4]],
  ncol = 2, align = "hv", axis = "tblr"
)

ggsave(file.path(od, "FigS1_GenFN_and_OLS.png"),
       FigS1, width = 9, height = 8, dpi = 300, bg = "white")
