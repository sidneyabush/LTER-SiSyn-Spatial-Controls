###### Figure S1 — A & B: Gen vs FN (build p_conc, p_yield; no save here) ######
# (Paste your Gen–FN code here but STOP before the ggsave lines.)
# Only need the pieces that create `p_conc` and `p_yield`.

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
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "lm", se = FALSE, color = "#6699CC") +
    coord_equal() +
    labs(x = xlab, y = ylab) +
    annotate("text", x = -Inf, y = Inf,
             label = sprintf("R² = %.2f\np %s", r2, p_fmt(pval)),
             hjust = -0.1, vjust = 1.3) +
    scale_x_continuous(sec.axis = dup_axis(breaks = NULL, labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(breaks = NULL, labels = NULL)) +
    theme_classic(base_size = 11) +
    theme(
      axis.line             = element_line(color = "black"),
      axis.line.x.top       = element_line(color = "black"),
      axis.line.y.right     = element_line(color = "black"),
      axis.ticks.length.x.top = grid::unit(0, "pt"),
      axis.ticks.length.y.right = grid::unit(0, "pt")
    )
}
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

S1A <- p_conc + labs(tag = "A")
S1B <- p_yield + labs(tag = "B")

###### Figure S1 — C & D: OLS from Fig2 A/B (Predicted vs Observed) ######
.pfmt <- function(p) ifelse(is.na(p), "NA", ifelse(p < 1e-16, "< 1e-16",
                                                   formatC(p, format = "e", digits = 2)))
.reg_stats <- function(df, x, y) {
  ok <- complete.cases(df[[x]], df[[y]])
  s  <- summary(lm(df[[y]][ok] ~ df[[x]][ok]))
  list(r2 = s$r.squared, p = coef(s)[2, 4])
}
ols_panel <- function(df, xvar, yvar, title_txt, tag_txt) {
  st <- .reg_stats(df, xvar, yvar)
  ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(color = "grey40", alpha = 0.5, size = 1.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
    geom_smooth(method = "lm", se = FALSE, color = "#6699CC", linewidth = 1.1) +
    coord_equal() +
    labs(x = "Predicted", y = "Observed", title = title_txt, tag = tag_txt) +
    annotate("text", x = -Inf, y = Inf,
             label = sprintf("R² = %.2f\np = %s", st$r2, .pfmt(st$p)),
             hjust = -0.1, vjust = 1.3) +
    scale_x_continuous(sec.axis = dup_axis(breaks = NULL, labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(breaks = NULL, labels = NULL)) +
    theme_classic(base_size = 20) +
    theme(
      axis.line.x.top   = element_line(color = "black"),
      axis.line.y.right = element_line(color = "black"),
      axis.ticks.length.x.top = grid::unit(0, "pt"),
      axis.ticks.length.y.right = grid::unit(0, "pt"),
      plot.title = element_text(vjust = 4)
    )
}

S1C <- ols_panel(pred_GenConc,  "predicted", "observed", "Linear regression: Concentration", "C")
S1D <- ols_panel(pred_GenYield, "predicted", "observed", "Linear regression: Yield",         "D")

###### Figure S1 — Assemble & Save ######
FigS1 <- cowplot::plot_grid(S1A, S1B, S1C, S1D, ncol = 2, align = "hv")
ggsave(file.path(od, "FigS1_GenFN_plus_OLS.png"),
       FigS1, width = 14, height = 10, dpi = 300, bg = "white")
