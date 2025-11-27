# Configuration file for Silica Spatial Controls Analysis
#
# SETUP INSTRUCTIONS:
# 1. Download the analysis data from Zenodo: https://doi.org/10.5281/zenodo.14223733
# 2. Extract the data to a location on your computer
# 3. Update the DATA_DIR path below to point to your extracted data directory
#
# The data directory should contain the following subdirectories:
#   - harmonization_files/inputs/
#   - model_output_files/
#   - model_performance/
#   - GRL_Materials/Final_Figures/

# ==============================================================================
# USER CONFIGURATION - EDIT THIS SECTION
# ==============================================================================

# Path to the root directory where you extracted the Zenodo data
# Example: "/Users/yourname/Downloads/SiSyn_Data"
# Windows example: "C:/Users/yourname/Documents/SiSyn_Data"
DATA_DIR <- "/path/to/your/data/directory"

# ==============================================================================
# DERIVED PATHS - DO NOT EDIT BELOW THIS LINE
# ==============================================================================

# Input data directories
HARMONIZATION_DIR <- file.path(DATA_DIR, "harmonization_files", "inputs")
MODEL_OUTPUT_DIR <- file.path(DATA_DIR, "model_output_files")
MODEL_PERFORMANCE_DIR <- file.path(DATA_DIR, "model_performance")

# Output directories
OUTPUT_PNG_DIR <- file.path(DATA_DIR, "GRL_Materials", "Final_Figures", "PNG")
OUTPUT_PDF_DIR <- file.path(DATA_DIR, "GRL_Materials", "Final_Figures", "PDF")

# Create output directories if they don't exist
if (!dir.exists(OUTPUT_PNG_DIR)) dir.create(OUTPUT_PNG_DIR, recursive = TRUE)
if (!dir.exists(OUTPUT_PDF_DIR)) dir.create(OUTPUT_PDF_DIR, recursive = TRUE)

# Validate that required directories exist
required_dirs <- c(HARMONIZATION_DIR, MODEL_OUTPUT_DIR, MODEL_PERFORMANCE_DIR)
for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    stop(paste0(
      "\nERROR: Required directory not found: ", dir,
      "\n\nPlease check that:\n",
      "1. You have downloaded the data from Zenodo\n",
      "2. You have set DATA_DIR correctly in config.R\n",
      "3. The data directory contains the expected subdirectories\n"
    ))
  }
}

message("Configuration loaded successfully!")
message("Data directory: ", DATA_DIR)
