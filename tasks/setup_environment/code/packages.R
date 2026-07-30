cran_packages <- c(
  "DBI", "readr", "fixest", "stringr", "tidycensus", "dplyr", "sf", "purrr",
  "data.table", "tigris", "zoo", "patchwork", "glue", "arrow", "duckdb",
  "ggplot2", "tidyr", "tibble", "curl", "janitor", "lubridate", "units",
  "remotes", "jsonlite"
)

user_lib <- Sys.getenv("R_LIBS_USER")
dir.create(user_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

options(repos = c(CRAN = "https://cloud.r-project.org"))

load_package <- function(pkg) {
  suppressPackageStartupMessages(require(pkg, character.only = TRUE, quietly = TRUE))
}

package_version_line <- function(pkg) {
  version <- tryCatch(packageDescription(pkg, fields = "Version"), error = function(e) NA)
  paste(pkg, version, sep = " : ")
}

install_cran_package <- function(pkg) {
  message(sprintf("Installing %s ...", pkg))
  install.packages(pkg, dependencies = NA)
}

install_github_package <- function(pkg, repo) {
  message(sprintf("Installing %s from GitHub...", pkg))
  remotes::install_github(repo, upgrade = "never", dependencies = NA)
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
  deweydatar = "Dewey-Data/deweydatar@964c887e19bb3817d4f6b8c668c5016ebe762aba"
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

output_log <- paste("Packages installed:", paste(output, collapse = "\n"), sep = "\n")
if (sys.nframe() == 0) {
  writeLines(output_log, "../output/R_packages.txt")
}
