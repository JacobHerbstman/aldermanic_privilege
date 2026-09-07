# setwd("tasks/new_construction_cleaning/code")
# minimum_construction_year <- 1999

# The historical-coordinate screen used this earlier Assessor selection.
# Final project construction uses the later cross-section rule separately.
source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(minimum_construction_year)
if (length(args) != 1L) stop("Expected the earliest construction year for discovery.")
minimum_construction_year <- suppressWarnings(as.integer(args[1]))
if (!is.finite(minimum_construction_year)) stop("The discovery construction year is invalid.")

con <- DBI::dbConnect(duckdb::duckdb())

data <- DBI::dbGetQuery(con, sprintf("
SELECT * EXCLUDE (apartments_text, source_row_order)
FROM read_parquet('../input/residential_assessor_history.parquet')
WHERE year_built >= %d
ORDER BY source_row_order
", minimum_construction_year))

# Preserve the discovery rule: earliest reported construction year, then assessment.
cross_section_buildings <- data %>%
  dplyr::group_by(pin) %>%
  dplyr::slice_min(order_by = dplyr::if_else(is.na(year_built), Inf, year_built), with_ties = TRUE) %>%  # earliest build year
  dplyr::slice_min(order_by = tax_year, with_ties = TRUE) %>%                                            # then earliest tax year
  dplyr::slice_max(order_by = building_sqft, with_ties = FALSE) %>%                                      # then largest sqft
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(year_built))

if (any(cross_section_buildings$year_built < minimum_construction_year, na.rm = TRUE)) {
  stop("Residential discovery records precede the configured construction-year bound.", call. = FALSE)
}

# (Optional) sanity check uniqueness
stopifnot(nrow(cross_section_buildings) == dplyr::n_distinct(cross_section_buildings$pin, cross_section_buildings$card_num))

write_csv(cross_section_buildings, "../output/residential_discovery_cross_section.csv")

DBI::dbDisconnect(con, shutdown = TRUE)
