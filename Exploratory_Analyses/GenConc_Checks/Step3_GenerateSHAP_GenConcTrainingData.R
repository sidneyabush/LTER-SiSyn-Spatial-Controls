# ──────────────────────────────────────────────────────────────────────────────
# Compute SHAP values for recent30 data using RF2 models for GenConc and GenYield
# ──────────────────────────────────────────────────────────────────────────────

## 1. Load needed packages
librarian::shelf(
  iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach,
  randomForest, tibble, viridis, RColorBrewer, patchwork, fastshap
)

## 2. Clear env & seed
rm(list = ls())
set.seed(123)

## 3. Set WD
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
final_models_dir <- "Final_Models"

# 4. Load GenConc model & kept‑drivers into their own env to avoid name clashes
load(file.path(final_models_dir, "GenConc_Yearly_rf_model2.RData"))
load(file.path(final_models_dir, "GenConc_Yearly_kept_drivers.RData"))

# 5. Load GenYield model & kept‑drivers into a separate env
load(file.path(final_models_dir, "GenYield_Yearly_rf_model2.RData"))
load(file.path(final_models_dir, "GenYield_Yearly_kept_drivers.RData"))

###############################################################################
# 5. Helper to generate SHAP with a sanity‐check
###############################################################################
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  # 1) which predictors did RF actually use?
  model_vars <- names(model$forest$xlevels)
  
  # 2) check for any missing columns
  missing <- setdiff(model_vars, colnames(kept_drivers))
  if (length(missing) > 0) {
    stop(
      "Cannot compute SHAP: the following variable(s) are in the RF model but",
      " not in your kept_drivers:\n  ",
      paste(missing, collapse = ", ")
    )
  }
  
  # 3) now safely subset & reorder
  X_ordered <- kept_drivers[, model_vars, drop = FALSE]
  
  # 4) wrap predict
  custom_predict <- function(object, newdata) {
    predict(object, newdata = as.data.frame(newdata))
  }
  
  # 5) compute SHAP
  fastshap::explain(
    object       = model,
    X            = X_ordered,
    pred_wrapper = custom_predict,
    nsim         = sample_size
  )
}


###############################################################################
# 6. Generate & save SHAP for GenConc
###############################################################################
shap_values_GenConc <- generate_shap_values(
  model        = rf2_GenConc,
  kept_drivers = kept_drivers_GenConc,
  sample_size  = 30
)
save(
  shap_values_GenConc,
  file = file.path(final_models_dir, "GenConc_Yearly_shap_values_recent30.RData")
)


###############################################################################
# 7. Generate & save SHAP for GenYield
###############################################################################
shap_values_GenYield <- generate_shap_values(
  model        = rf2_GenYield,
  kept_drivers = kept_drivers_GenYield,
  sample_size  = 30
)
save(
  shap_values_GenYield,
  file = file.path(final_models_dir, "GenYield_Yearly_shap_values_recent30.RData")
)
