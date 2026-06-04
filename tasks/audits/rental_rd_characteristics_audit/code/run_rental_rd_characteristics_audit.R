# Audit listed-rent RD characteristics panel amenity-distance construction.

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rental_rd_characteristics_audit/code")
# bandwidth_ft <- 500

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <bandwidth_ft>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))

rent <- read_parquet(sprintf("../input/rental_rd_characteristics_panel_bw%s.parquet", bandwidth_label)) %>%
  as_tibble()

required_cols <- c(
  "longitude",
  "latitude",
  "year_month",
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft",
  "nearest_cta_station_id",
  "nearest_cta_station_name",
  "nearest_cta_lines"
)
if (!all(required_cols %in% names(rent))) {
  stop("Characteristics panel is missing required amenity audit columns.", call. = FALSE)
}

amenity_cols <- c(
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft"
)

amenity_diagnostics <- bind_rows(lapply(amenity_cols, function(metric) {
  x <- rent[[metric]]
  tibble(
    metric = metric,
    n_rows = nrow(rent),
    n_unique_coords = nrow(distinct(rent, longitude, latitude)),
    n_nonmissing = sum(!is.na(x)),
    share_nonmissing = mean(!is.na(x)),
    min_distance_ft = min(x, na.rm = TRUE),
    p50_distance_ft = median(x, na.rm = TRUE),
    p90_distance_ft = quantile(x, 0.90, na.rm = TRUE),
    mean_distance_ft = mean(x, na.rm = TRUE),
    max_distance_ft = max(x, na.rm = TRUE)
  )
}))

cta_station_assignment <- rent %>%
  distinct(longitude, latitude, year_month, nearest_cta_station_id, nearest_cta_station_name, nearest_cta_lines) %>%
  count(year_month, nearest_cta_station_id, nearest_cta_station_name, nearest_cta_lines, name = "n_unique_coordinate_months") %>%
  arrange(year_month, desc(n_unique_coordinate_months))

write_csv(
  amenity_diagnostics,
  sprintf("../output/rental_rd_amenity_distance_diagnostics_bw%s.csv", bandwidth_label)
)
write_csv(
  cta_station_assignment,
  sprintf("../output/rental_rd_cta_station_assignment_bw%s.csv", bandwidth_label)
)

message("Saved listed-rent RD characteristics audit outputs.")
