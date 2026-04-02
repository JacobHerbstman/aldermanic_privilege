# --- Package bootstrap script ---

rm(list = ls())

# 1. CRAN Packages
# (Standard packages installed via install.packages)
cran_packages <- c(
  "DBI", "readr","fixest","haven","stringr", "ipumsr", "tidycensus","dplyr","sf", 
  "furrr", "purrr", "nngeo", "data.table", "tigris", "zoo", "patchwork", "glue", 
  "writexl", "arrow", "duckdb", "ggplot2", "here", "tidyr", "sfarrow", "geoarrow", 
  "tibble", "rdrobust", "forcats", "binsreg", "did", "igraph", "sentimentr", 
  "tidytext", "textdata", "Matrix", "devtools", "scico", "RColorBrewer", 
  "optparse", "jsonlite"
)

# --- Step 1: ensure library paths are aligned ---
user_lib <- Sys.getenv("R_LIBS_USER")
dir.create(user_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

# --- Step 2: choose a reliable CRAN mirror ---
options(repos = c(CRAN = "https://cloud.r-project.org"))

# --- Step 2b: repair stale Fortran linker paths on Homebrew R setups ---
if (!dir.exists("/opt/gfortran/lib")) {
  gcc_main <- "/opt/homebrew/opt/gcc/lib/gcc/current"
  gcc_aux <- Sys.glob("/opt/homebrew/Cellar/gcc/*/lib/gcc/current/gcc/*/*")
  gcc_aux <- if (length(gcc_aux) > 0) gcc_aux[[length(gcc_aux)]] else NA_character_
  if (dir.exists(gcc_main) && is.character(gcc_aux) && dir.exists(gcc_aux)) {
    gcc_lib_paths <- c(gcc_main, gcc_aux)
    Sys.setenv(
      LIBRARY_PATH = paste(gcc_lib_paths, collapse = ":"),
      LDFLAGS = paste(sprintf("-L%s", gcc_lib_paths), collapse = " ")
    )
  }
}

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
if (!require("summclust", character.only = TRUE)) {
  message("Installing summclust from GitHub...")
  tryCatch({
    devtools::install_github("s3alfisc/summclust", upgrade = "never", dependencies = TRUE)
  }, error = function(e) {
    message(sprintf("GitHub install failed for summclust: %s", e$message))
  })
}

version_summclust <- tryCatch({ packageDescription("summclust", fields = "Version") }, error = function(e) NA)
output <- c(output, paste("summclust", version_summclust, sep = " : "))

if (!require("fwildclusterboot", character.only = TRUE)) {
  message("Installing fwildclusterboot from GitHub...")
  tryCatch({
    devtools::install_github("s3alfisc/fwildclusterboot", upgrade = "never", dependencies = TRUE)
  }, error = function(e) {
    message(sprintf("GitHub install failed for fwildclusterboot: %s", e$message))
  })
}

version_fwild <- tryCatch({ packageDescription("fwildclusterboot", fields = "Version") }, error = function(e) NA)
output <- c(output, paste("fwildclusterboot", version_fwild, sep = " : "))

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

panelview_installed <- require("panelView", character.only = TRUE)
panelview_remote_sha <- if (panelview_installed) {
  tryCatch(packageDescription("panelView", fields = "RemoteSha"), error = function(e) NA_character_)
} else {
  NA_character_
}

if (!panelview_installed || is.na(panelview_remote_sha) || !nzchar(panelview_remote_sha)) {
  message("Installing panelView development branch from GitHub...")
  tryCatch({
    devtools::install_github("xuyiqing/panelView@dev", upgrade = "never", dependencies = TRUE)
  }, error = function(e) {
    message(sprintf("GitHub install failed for panelView: %s", e$message))
  })
}

version_panelview <- tryCatch({ packageDescription("panelView", fields = "Version") }, error = function(e) NA)
output <- c(output, paste("panelView", version_panelview, sep = " : "))

# --- Step 5: log output ---
output_log <- paste("Packages installed:", paste(output, collapse = "\n"), sep = "\n")
cat(output_log)
