# --- Package bootstrap script ---

rm(list = ls())

# Packages required for the project
packages <- c(
  "DBI", "readr","fixest","haven","stringr", "ipumsr", "tidycensus","dplyr","sf", "furrr", "purrr", "nngeo", "data.table", 
  "tigris", "zoo", "patchwork", "glue", "writexl", "arrow", "duckdb", "ggplot2", "here", "tidyr", "sfarrow", "geoarrow", 
  "tibble", "rdrobust", "forcats", "binsreg", "did", "igraph", "sentimentr", "tidytext", "textdata", "Matrix"
)

# --- Step 1: ensure library paths are aligned ---
# Create user lib dir if needed, and prepend it to .libPaths()
user_lib <- Sys.getenv("R_LIBS_USER")
dir.create(user_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))


# --- Step 2: choose a reliable CRAN mirror ---
options(repos = c(CRAN = "https://cloud.r-project.org"))


# --- Step 3: install & load packages robustly ---
output <- character()

for (pkg in packages) {
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
    
    # Load after install attempt
    if (!require(pkg, character.only = TRUE)) {
      warning(sprintf("Package %s could not be installed or loaded.", pkg))
    }
  }
  
  # Record version
  version <- tryCatch({
    packageDescription(pkg, fields = "Version")
  }, error = function(e) NA)
  
  output <- c(output, paste(pkg, version, sep = " : "))
}

# --- Step 4: log output ---
output <- paste("Packages installed:", paste(output, collapse = "\n"), sep = "\n")
cat(output)

# Optionally write to file in repo (uncomment if desired)
# writeLines(output, "../output/R_packages.txt")
