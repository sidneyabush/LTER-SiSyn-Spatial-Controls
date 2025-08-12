# ############################################################
# Gen vs FN comparison: concentrations & yields (two-panel)
# theme_classic; top/right lines only (no ticks/labels)
# Primary axis labels on bottom/left; figure TITLE added
# R² & p-value in upper-left; regression line = #6699CC
# ############################################################

librarian::shelf(dplyr, ggplot2, readr, patchwork)

record_length <- 5
infile   <- sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years.csv", record_length)
out_png  <- "Fig_GenFN_conc_yield.png"
out_pdf  <- "Fig_GenFN_conc_yield.pdf"
fig_title <- "Gen vs FN: Concentrations and Yields"

# ---- Load ----
df <- readr::read_csv(infile, show_col_types = FALSE) |>
  dplyr::select(GenConc, FNConc, GenYield, FNYield) |>
  dplyr::mutate(
    GenConc  = as.numeric(GenConc),
    FNConc   = as.numeric(FNConc),
    GenYield = as.numeric(GenYield),
    FNYield  = as.numeric(FNYield)
  )

# ---- Helpers ----
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

panel_scatter <- function(data, x, y, xlab, ylab, r2, pval) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_point(alpha = 0.35, size = 1.2) +
    # 1:1 reference
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    # OLS fit
    geom_smooth(method = "lm", se = FALSE, color = "#6699CC") +
    coord_equal() +
    labs(x = xlab, y = ylab) +
    # Upper-left annotation
    annotate("text",
             x = -Inf, y = Inf,
             label = sprintf("R² = %.2f\np %s", r2, p_fmt(pval)),
             hjust = -0.1, vjust = 1.3) +
    # Create empty secondary axes so top/right LINES draw (no ticks/labels/titles)
    scale_x_continuous(sec.axis = dup_axis(name = NULL, breaks = NULL, labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL, breaks = NULL, labels = NULL)) +
    theme_classic(base_size = 11) +
    theme(
      panel.grid.minor      = element_blank(),
      axis.line             = element_line(color = "black"),  # bottom/left
      axis.line.x.top       = element_line(color = "black"),  # top line
      axis.line.y.right     = element_line(color = "black"),  # right line
      axis.ticks.length.x.top = grid::unit(0, "pt"),
      axis.ticks.length.y.right = grid::unit(0, "pt")
    )
}

# ---- Stats + panels ----
conc_stats  <- r2_p_lm(df$FNConc,  df$GenConc)
yield_stats <- r2_p_lm(df$FNYield, df$GenYield)

p_conc <- panel_scatter(
  df, GenConc, FNConc,
  xlab = expression("GenConc (mg Si L"^-1*")"),
  ylab = expression("FNConc (mg Si L"^-1*")"),
  r2 = conc_stats$r2, pval = conc_stats$p
)

p_yield <- panel_scatter(
  df, GenYield, FNYield,
  xlab = expression("GenYield (kg Si km"^-2*" yr"^-1*")"),
  ylab = expression("FNYield (kg Si km"^-2*" yr"^-1*")"),
  r2 = yield_stats$r2, pval = yield_stats$p
)

fig <- (p_conc + p_yield) +
  plot_annotation(
    title = fig_title,
    tag_levels = "A",
    theme = theme(
      plot.title = element_blank()
    )
  )

# ---- Save ----
ggsave(out_png, fig, width = 8, height = 4, dpi = 300)
ggsave(out_pdf, fig, width = 8, height = 4)

fig
