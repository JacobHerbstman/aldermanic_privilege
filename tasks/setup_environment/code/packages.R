cran_packages <- c(
  "DBI", "readr", "fixest", "haven", "stringr", "ipumsr", "tidycensus", "dplyr", "sf",
  "furrr", "purrr", "nngeo", "data.table", "tigris", "zoo", "patchwork", "glue",
  "writexl", "arrow", "duckdb", "ggplot2", "here", "tidyr", "sfarrow", "geoarrow",
  "tibble", "rdrobust", "forcats", "binsreg", "did", "igraph", "sentimentr",
  "tidytext", "textdata", "Matrix", "devtools", "scico", "RColorBrewer",
  "optparse", "jsonlite"
)

user_lib <- Sys.getenv("R_LIBS_USER")
dir.create(user_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

options(repos = c(CRAN = "https://cloud.r-project.org"))

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

load_package <- function(pkg) {
  suppressPackageStartupMessages(require(pkg, character.only = TRUE, quietly = TRUE))
}

package_version_line <- function(pkg) {
  version <- tryCatch(packageDescription(pkg, fields = "Version"), error = function(e) NA)
  paste(pkg, version, sep = " : ")
}

install_cran_package <- function(pkg) {
  message(sprintf("Installing %s ...", pkg))
  success <- FALSE
  tryCatch({
    install.packages(pkg, dependencies = TRUE, type = "binary")
    success <- TRUE
  }, error = function(e) {
    message(sprintf("Binary install failed for %s: %s", pkg, e$message))
  })

  if (!success) {
    tryCatch({
      install.packages(pkg, dependencies = TRUE, type = "source")
    }, error = function(e) {
      message(sprintf("Source install failed for %s: %s", pkg, e$message))
    })
  }
}

install_github_package <- function(pkg, repo) {
  message(sprintf("Installing %s from GitHub...", pkg))
  tryCatch({
    devtools::install_github(repo, upgrade = "never", dependencies = TRUE)
  }, error = function(e) {
    message(sprintf("GitHub install failed for %s: %s", pkg, e$message))
  })
}

output <- character()

for (pkg in cran_packages) {
  if (!load_package(pkg)) {
    install_cran_package(pkg)
  }
  if (!load_package(pkg)) {
    warning(sprintf("Package %s could not be installed or loaded.", pkg))
  }
  output <- c(output, package_version_line(pkg))
}

github_packages <- list(
  summclust = "s3alfisc/summclust",
  fwildclusterboot = "s3alfisc/fwildclusterboot",
  deweydatar = "Dewey-Data/deweydatar"
)

for (pkg in names(github_packages)) {
  if (!load_package(pkg)) {
    install_github_package(pkg, github_packages[[pkg]])
  }
  if (!load_package(pkg)) {
    warning(sprintf("Package %s could not be installed or loaded.", pkg))
  }
  output <- c(output, package_version_line(pkg))
}

panelview_installed <- load_package("panelView")
panelview_remote_sha <- if (panelview_installed) {
  tryCatch(packageDescription("panelView", fields = "RemoteSha"), error = function(e) NA_character_)
} else {
  NA_character_
}

if (!panelview_installed || is.na(panelview_remote_sha) || !nzchar(panelview_remote_sha)) {
  install_github_package("panelView", "xuyiqing/panelView@dev")
}
if (!load_package("panelView")) {
  warning("Package panelView could not be installed or loaded.")
}

output <- c(output, package_version_line("panelView"))

output_log <- paste("Packages installed:", paste(output, collapse = "\n"), sep = "\n")
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else ""
running_directly <- nzchar(script_file) &&
  basename(normalizePath(script_file, winslash = "/", mustWork = FALSE)) == "packages.R"

log_path <- Sys.getenv("R_PACKAGE_LOG", unset = "")
if (!nzchar(log_path) && running_directly) {
  log_path <- "../output/R_packages.txt"
}
if (nzchar(log_path)) {
  writeLines(output_log, log_path)
}
