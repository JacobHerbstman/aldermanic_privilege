cran_packages <- c(
  "RSQLite", "igraph", "DBI", "readr", "fixest", "stringr", "tidycensus", "dplyr", "sf", "purrr",
  "data.table", "tigris", "zoo", "patchwork", "glue", "arrow", "duckdb",
  "ggplot2", "tidyr", "tibble", "curl", "janitor", "lubridate", "units",
  "remotes", "jsonlite", "xml2", "nabor", "httr2"
)

# Running this file through setup_environment/Makefile installs missing packages.
# Analysis scripts source it only to load the recorded environment.
install_missing <- sys.nframe() == 0L
if (install_missing) {
  dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
}
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

for (pkg in c(cran_packages, "deweydatar")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!install_missing) {
      stop(paste("Missing R package:", pkg, "— run make in tasks/setup_environment/code."), call. = FALSE)
    }
    if (pkg == "deweydatar") {
      remotes::install_github(
        "Dewey-Data/deweydatar@964c887e19bb3817d4f6b8c668c5016ebe762aba",
        upgrade = "never", dependencies = NA
      )
    } else {
      install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = NA)
    }
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

if (install_missing) {
  versions <- vapply(c(cran_packages, "deweydatar"), function(pkg) {
    paste(pkg, packageVersion(pkg), sep = " : ")
  }, character(1))
  writeLines(c("Packages installed:", versions), "../output/R_packages.txt")
}
