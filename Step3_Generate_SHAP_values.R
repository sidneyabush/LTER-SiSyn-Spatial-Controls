# 1. Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, 
                 randomForest, tibble, viridis, RColorBrewer, patchwork)

# 2. Clear environment
rm(list = ls())

# 3. Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Define the path to your “Final_Models” folder
final_models_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"

# ------------------------- FNConc Data (Concentration) -------------------------
# 4. Load FNConc model/data from “Final_Models”
load(file.path(final_models_dir, "FNConc_Yearly_rf_model2.RData"))   
rf_model2_FNConc     <- rf_model2

load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"))
kept_drivers_FNConc  <- kept_drivers

load(file.path(final_models_dir, "FNConc_Yearly_numeric.RData"))
drivers_numeric_FNConc <- drivers_numeric

# (If you need the stream‐ID file, uncomment:)
# load(file.path(final_models_dir, "FNConc_Yearly_stream_ids.RData"))
# drivers_df_FNConc    <- drivers_df


# ------------------------- FNYield Data (Yield) -------------------------
# 5. Load FNYield model/data from “Final_Models”
load(file.path(final_models_dir, "FNYield_Yearly_rf_model2.RData"))   
rf_model2_FNYield    <- rf_model2

load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData"))
kept_drivers_FNYield <- kept_drivers

load(file.path(final_models_dir, "FNYield_Yearly_numeric.RData"))
drivers_numeric_FNYield <- drivers_numeric

# (If you need the stream‐ID file, uncomment:)
# load(file.path(final_models_dir, "FNYield_Yearly_stream_ids.RData"))
# drivers_df_FNYield   <- drivers_df

# 6. Set seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/"

###############################################################################
# 7. Define Function to Create SHAP Values
###############################################################################
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  # Custom prediction function
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)
    predict(object, newdata = newdata)
  }
  
  # Compute SHAP values using fastshap
  shap_values <- fastshap::explain(
    object       = model,
    X            = kept_drivers,
    pred_wrapper = custom_predict,
    nsim         = sample_size
  )
  return(shap_values)
}

###############################################################################
# 8. Generate SHAP Values for Both FNConc and FNYield
###############################################################################
shap_values_FNConc <- generate_shap_values(rf_model2_FNConc, kept_drivers_FNConc, sample_size = 30)
shap_values_FNYield <- generate_shap_values(rf_model2_FNYield, kept_drivers_FNYield, sample_size = 30)

# 9. (Optional) Save SHAP Values to “Final_Models”
save(shap_values_FNConc,
     file = file.path(final_models_dir, "FNConc_Yearly_shap_values_new.RData"))

save(shap_values_FNYield,
     file = file.path(final_models_dir, "FNYield_Yearly_shap_values_new.RData"))


