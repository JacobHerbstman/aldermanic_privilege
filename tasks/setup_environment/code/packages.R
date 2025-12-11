# --- Package bootstrap script ---

rm(list = ls())

# 1. CRAN Packages
# (Standard packages installed via install.packages)
cran_packages <- c(
  "DBI", "readr","fixest","haven","stringr", "ipumsr", "tidycensus","dplyr","sf", 
  "furrr", "purrr", "nngeo", "data.table", "tigris", "zoo", "patchwork", "glue", 
  "writexl", "arrow", "duckdb", "ggplot2", "here", "tidyr", "sfarrow", "geoarrow", 
  "tibble", "rdrobust", "forcats", "binsreg", "did", "igraph", "sentimentr", 
  "tidytext", "textdata", "Matrix", "devtools", "scico", "RColorBrewer", "palaetteer"
)

# --- Step 1: ensure library paths are aligned ---
user_lib <- Sys.getenv("R_LIBS_USER")
dir.create(user_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

# --- Step 2: choose a reliable CRAN mirror ---
options(repos = c(CRAN = "https://cloud.r-project.org"))

# --- Step 3: install & load CRAN packages ---
output <- character()

for (pkg in cran_packages) {
  if (!require(pkg, character.only = TRUE)) {
    message(sprintf("Installing %s ...", pkg))
    
    # Try binary first
    success <- FALSE
    tryCatch({
      install.packages(pkg, dependencies = TRUE, type = "binary")
      success <- TRUE
    }, error = function(e) {
      message(sprintf("Binary install failed for %s: %s", pkg, e$message))
    })
    
    # If binary fails, try source
    if (!success) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE, type = "source")
      }, error = function(e) {
        message(sprintf("Source install failed for %s: %s", pkg, e$message))
      })
    }
    
    if (!require(pkg, character.only = TRUE)) {
      warning(sprintf("Package %s could not be installed or loaded.", pkg))
    }
  }
  
  version <- tryCatch({ packageDescription(pkg, fields = "Version") }, error = function(e) NA)
  output <- c(output, paste(pkg, version, sep = " : "))
}

# --- Step 4: Install GitHub Packages (Dewey) ---
# This must run AFTER the loop so 'devtools' is available
if (!require("deweydatar", character.only = TRUE)) {
  message("Installing deweydatar from GitHub...")
  tryCatch({
    devtools::install_github("Dewey-Data/deweydatar", upgrade = "never", dependencies = TRUE)
  }, error = function(e) {
    message(sprintf("GitHub install failed for deweydatar: %s", e$message))
  })
}

# Add deweydatar to the log manually
version_dewey <- tryCatch({ packageDescription("deweydatar", fields = "Version") }, error = function(e) NA)
output <- c(output, paste("deweydatar", version_dewey, sep = " : "))

# --- Step 5: log output ---
output_log <- paste("Packages installed:", paste(output, collapse = "\n"), sep = "\n")
cat(output_log)