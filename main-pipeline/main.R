# ==========================================
# MAIN EXECUTION SCRIPT
# ==========================================
# 1. Load the 'here' library to manage paths
library(here)

# 2. Load functions using relative paths from the project root
# This ensures R finds them regardless of your current working directory
source(here("main-pipeline", "utils_data.R"))
source(here("main-pipeline", "utils_viz.R"))

# 3. Set Global Parameters
PATCH <- "16.3"
# Use 'here' for directories too
BASE_DIR <- here("data", "processed")
PLOT_DIR <- file.path(BASE_DIR, "plots")

# 4. Execute Pipeline
tft_data <- scrape_tft_data(PATCH, BASE_DIR)

generate_tft_plot(tft_data, PATCH, PLOT_DIR)

message("Pipeline completed successfully!")