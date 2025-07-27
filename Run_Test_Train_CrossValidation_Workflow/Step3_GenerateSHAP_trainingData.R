# ──────────────────────────────────────────────────────────────────────────────
# Compute SHAP values for recent30 data using RF2 models for FNConc and FNYield
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

# 4. Load FNConc model & kept‑drivers into their own env to avoid name clashes
load(file.path(final_models_dir, "FNConc_Yearly_rf_model2.RData"))
rf_model2_FNConc <- rf2; rm(rf2)

fn_env <- new.env()
load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"), envir = fn_env)
kept_drivers_FNConc <- fn_env$kept_drivers
rm(fn_env)

# 5. Load FNYield model & kept‑drivers into a separate env
load(file.path(final_models_dir, "FNYield_Yearly_rf_model2.RData"))
rf_model2_FNYield <- rf2; rm(rf2)

fy_env <- new.env()
load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData"), envir = fy_env)
kept_drivers_FNYield <- fy_env$kept_drivers
rm(fy_env)

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
# 6. Generate & save SHAP for FNConc
###############################################################################
shap_values_FNConc <- generate_shap_values(
  model        = rf_model2_FNConc,
  kept_drivers = kept_drivers_FNConc,
  sample_size  = 30
)
save(
  shap_values_FNConc,
  file = file.path(final_models_dir, "FNConc_Yearly_shap_values_recent30.RData")
)


###############################################################################
# 7. Generate & save SHAP for FNYield
###############################################################################
shap_values_FNYield <- generate_shap_values(
  model        = rf_model2_FNYield,
  kept_drivers = kept_drivers_FNYield,
  sample_size  = 30
)
save(
  shap_values_FNYield,
  file = file.path(final_models_dir, "FNYield_Yearly_shap_values_recent30.RData")
)
