source("../../setup_environment/code/packages.R")

sf_use_s2(FALSE)

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/summary_stats_dwellsy/code")
# analysis_file <- "../input/chicago_dwellsy_listing_analysis_sample.parquet"
# block_group_geometry_file <- "../input/block_group_geometry_2019.gpkg"
# boundary_file <- "../input/chicago_boundary.geojson"
# output_dir <- "../output"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- tryCatch(
    c(
      analysis_file,
      block_group_geometry_file,
      boundary_file,
      output_dir
    ),
    error = function(e) {
      stop(
        "FATAL: run_summary_stats_dwellsy.R requires 4 args, or uncomment the top interactive block before sourcing in RStudio.",
        call. = FALSE
      )
    }
  )
}

if (length(cli_args) != 4) {
  stop(
    "FATAL: run_summary_stats_dwellsy.R requires 4 args: <analysis_file> <block_group_geometry_file> <boundary_file> <output_dir>",
    call. = FALSE
  )
}

analysis_file <- cli_args[1]
block_group_geometry_file <- cli_args[2]
boundary_file <- cli_args[3]
output_dir <- cli_args[4]

crs_projected <- 3435
acs_year <- 2024L

winsorize_series <- function(x, lower_prob = 0.01, upper_prob = 0.99) {
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (sum(keep) == 0) {
    return(list(values = out, lower = NA_real_, upper = NA_real_))
  }

  bounds <- quantile(x[keep], probs = c(lower_prob, upper_prob), na.rm = TRUE, type = 7)
  out[keep] <- pmin(pmax(x[keep], bounds[[1]]), bounds[[2]])
  list(values = out, lower = as.numeric(bounds[[1]]), upper = as.numeric(bounds[[2]]))
}

summarize_numeric_series <- function(x, variable, series, winsor_lower = NA_real_, winsor_upper = NA_real_) {
  keep <- is.finite(x)
  out <- data.table(
    variable = variable,
    series = series,
    n_nonmissing = sum(keep),
    share_missing = mean(!keep),
    winsor_lower = winsor_lower,
    winsor_upper = winsor_upper,
    min = NA_real_,
    p01 = NA_real_,
    p05 = NA_real_,
    p25 = NA_real_,
    median = NA_real_,
    mean = NA_real_,
    p75 = NA_real_,
    p95 = NA_real_,
    p99 = NA_real_,
    max = NA_real_
  )

  if (sum(keep) == 0) {
    return(out)
  }

  quantiles <- quantile(x[keep], probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99), na.rm = TRUE, type = 7)
  out[, `:=`(
    min = min(x[keep], na.rm = TRUE),
    p01 = as.numeric(quantiles[[1]]),
    p05 = as.numeric(quantiles[[2]]),
    p25 = as.numeric(quantiles[[3]]),
    median = as.numeric(quantiles[[4]]),
    mean = mean(x[keep], na.rm = TRUE),
    p75 = as.numeric(quantiles[[5]]),
    p95 = as.numeric(quantiles[[6]]),
    p99 = as.numeric(quantiles[[7]]),
    max = max(x[keep], na.rm = TRUE)
  )]
  out
}

message("Loading Dwellsy analysis sample...")
analysis_dt <- as.data.table(read_parquet(
  analysis_file,
  col_select = c(
    "dwellsy_row_id", "listing_id", "address_1", "address_city", "rent_price", "rent_price_real_2024", "sqft",
    "beds", "baths_total", "year_built", "latitude", "longitude", "creation_date",
    "creation_ts", "deactivation_ts", "listing_status"
  )
))

if (anyDuplicated(analysis_dt$listing_id) > 0) {
  stop("Analysis sample should have exactly one row per listing_id.", call. = FALSE)
}

analysis_dt[, creation_date := as.Date(creation_date)]
analysis_dt[, creation_ts := as.POSIXct(creation_ts, tz = "UTC")]
analysis_dt[, deactivation_ts := as.POSIXct(deactivation_ts, tz = "UTC")]
analysis_dt[, has_valid_latlon := is.finite(latitude) & is.finite(longitude)]
analysis_dt[, has_positive_rent := is.finite(rent_price_real_2024) & rent_price_real_2024 > 0]
analysis_dt[, has_positive_sqft := is.finite(sqft) & sqft > 0]
analysis_dt[, rent_per_sqft_real_2024 := fifelse(
  has_positive_rent & has_positive_sqft,
  rent_price_real_2024 / sqft,
  NA_real_
)]

rent_winsor <- winsorize_series(analysis_dt[has_positive_rent == TRUE, rent_price_real_2024])
analysis_dt[has_positive_rent == TRUE, rent_price_real_2024_winsor := rent_winsor$values]
analysis_dt[has_positive_rent == FALSE, rent_price_real_2024_winsor := NA_real_]

sqft_winsor <- winsorize_series(analysis_dt[has_positive_sqft == TRUE, sqft])
analysis_dt[has_positive_sqft == TRUE, sqft_winsor := sqft_winsor$values]
analysis_dt[has_positive_sqft == FALSE, sqft_winsor := NA_real_]

message("Building summary tables and integrity extracts...")
summary_stats <- rbindlist(
  list(
    summarize_numeric_series(analysis_dt$rent_price_real_2024, "rent_price_real_2024", "raw"),
    summarize_numeric_series(analysis_dt$rent_price_real_2024_winsor, "rent_price_real_2024", "winsorized_1_99", rent_winsor$lower, rent_winsor$upper),
    summarize_numeric_series(analysis_dt$sqft, "sqft", "raw"),
    summarize_numeric_series(analysis_dt$sqft_winsor, "sqft", "winsorized_1_99", sqft_winsor$lower, sqft_winsor$upper),
    summarize_numeric_series(analysis_dt$rent_per_sqft_real_2024, "rent_per_sqft_real_2024", "raw")
  ),
  use.names = TRUE
)

integrity_extremes <- rbindlist(
  list(
    head(analysis_dt[order(-rent_price_real_2024), .(
      issue_type = "top_rent",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      creation_date,
      listing_status,
      rent_price,
      rent_price_real_2024,
      rent_price_real_2024_winsor,
      sqft,
      sqft_winsor,
      rent_per_sqft_real_2024,
      beds,
      baths_total,
      year_built
    )], 100L),
    head(analysis_dt[is.finite(sqft)][order(-sqft), .(
      issue_type = "top_sqft",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      creation_date,
      listing_status,
      rent_price,
      rent_price_real_2024,
      rent_price_real_2024_winsor,
      sqft,
      sqft_winsor,
      rent_per_sqft_real_2024,
      beds,
      baths_total,
      year_built
    )], 100L),
    head(analysis_dt[is.finite(sqft) & sqft > 0][order(sqft), .(
      issue_type = "bottom_positive_sqft",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      creation_date,
      listing_status,
      rent_price,
      rent_price_real_2024,
      rent_price_real_2024_winsor,
      sqft,
      sqft_winsor,
      rent_per_sqft_real_2024,
      beds,
      baths_total,
      year_built
    )], 100L),
    head(analysis_dt[is.finite(rent_per_sqft_real_2024)][order(-rent_per_sqft_real_2024), .(
      issue_type = "top_rent_per_sqft",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      creation_date,
      listing_status,
      rent_price,
      rent_price_real_2024,
      rent_price_real_2024_winsor,
      sqft,
      sqft_winsor,
      rent_per_sqft_real_2024,
      beds,
      baths_total,
      year_built
    )], 100L)
  ),
  use.names = TRUE
)
setorder(integrity_extremes, issue_type, -rent_per_sqft_real_2024, -rent_price_real_2024, listing_id)

message("Loading block-group geometry and Chicago boundary...")
block_groups <- st_read(block_group_geometry_file, quiet = TRUE) %>%
  st_make_valid()
chicago_boundary <- st_read(boundary_file, quiet = TRUE) %>%
  st_make_valid()

if (!("GEOID" %in% names(block_groups))) {
  stop("Block-group geometry is missing GEOID.", call. = FALSE)
}
if (anyDuplicated(block_groups$GEOID) > 0) {
  stop("Block-group geometry GEOID values are not unique.", call. = FALSE)
}

block_groups <- block_groups[, "GEOID", drop = FALSE]
if (st_crs(block_groups)$epsg != crs_projected) {
  block_groups <- st_transform(block_groups, crs_projected)
}
if (st_crs(chicago_boundary) != st_crs(block_groups)) {
  chicago_boundary <- st_transform(chicago_boundary, st_crs(block_groups))
}

chicago_block_groups <- block_groups[lengths(st_intersects(block_groups, chicago_boundary)) > 0, ]
if (nrow(chicago_block_groups) == 0) {
  stop("No block groups intersect the Chicago boundary.", call. = FALSE)
}

message("Assigning analysis-sample listings to Chicago block groups...")
analysis_points <- analysis_dt[has_valid_latlon == TRUE]
analysis_points_sf <- st_as_sf(
  analysis_points,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(st_crs(chicago_block_groups))

analysis_with_blocks <- st_join(
  analysis_points_sf,
  chicago_block_groups[, "GEOID"],
  join = st_within,
  left = TRUE
) %>%
  st_drop_geometry() %>%
  as.data.table()

block_group_metrics <- analysis_with_blocks[!is.na(GEOID), .(
  n_listings = .N,
  mean_rent_real_2024 = mean(rent_price_real_2024, na.rm = TRUE),
  median_rent_real_2024 = median(rent_price_real_2024, na.rm = TRUE),
  mean_rent_real_2024_winsor = mean(rent_price_real_2024_winsor, na.rm = TRUE),
  median_rent_real_2024_winsor = median(rent_price_real_2024_winsor, na.rm = TRUE),
  mean_sqft = mean(sqft, na.rm = TRUE),
  median_sqft = median(sqft, na.rm = TRUE)
), by = GEOID]

block_group_map_data <- merge(
  as.data.table(st_drop_geometry(chicago_block_groups)),
  block_group_metrics,
  by = "GEOID",
  all.x = TRUE,
  sort = FALSE
)
block_group_map_data[is.na(n_listings), n_listings := 0L]

message("Fetching ACS 2024 5-year block-group median gross rent...")
if (nzchar(Sys.getenv("CENSUS_API_KEY"))) {
  tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE, overwrite = FALSE)
}

acs_block_group_rent <- tidycensus::get_acs(
  geography = "block group",
  variables = "B25064_001",
  state = "IL",
  county = "Cook",
  year = acs_year,
  survey = "acs5",
  cache_table = TRUE
) %>%
  st_drop_geometry() %>%
  transmute(
    GEOID,
    NAME,
    acs_median_gross_rent = as.numeric(estimate)
  ) %>%
  filter(GEOID %in% chicago_block_groups$GEOID) %>%
  as.data.table()

block_group_comparison <- merge(
  block_group_metrics,
  acs_block_group_rent,
  by = "GEOID",
  all.x = TRUE,
  sort = FALSE
)
block_group_comparison[, mean_rent_real_2024_gap_vs_acs := mean_rent_real_2024 - acs_median_gross_rent]
block_group_comparison[, median_rent_real_2024_gap_vs_acs := median_rent_real_2024 - acs_median_gross_rent]
block_group_comparison[, mean_rent_real_2024_winsor_gap_vs_acs := mean_rent_real_2024_winsor - acs_median_gross_rent]
block_group_comparison[, median_rent_real_2024_winsor_gap_vs_acs := median_rent_real_2024_winsor - acs_median_gross_rent]

comparison_primary <- block_group_comparison[
  n_listings >= 5 & is.finite(acs_median_gross_rent)
]

comparison_summary <- data.table(
  acs_year = acs_year,
  n_analysis_rows = nrow(analysis_dt),
  n_analysis_rows_with_valid_latlon = analysis_dt[has_valid_latlon == TRUE, .N],
  n_analysis_rows_with_block_group = analysis_with_blocks[!is.na(GEOID), .N],
  share_analysis_rows_with_block_group = analysis_with_blocks[!is.na(GEOID), .N] / nrow(analysis_dt),
  n_block_groups_with_any_dwellsy = block_group_metrics[, .N],
  n_block_groups_with_acs_match = block_group_comparison[is.finite(acs_median_gross_rent), .N],
  n_block_groups_primary_sample = nrow(comparison_primary),
  corr_mean_rent_real_2024_vs_acs = if (nrow(comparison_primary) == 0) NA_real_ else cor(comparison_primary$mean_rent_real_2024, comparison_primary$acs_median_gross_rent),
  corr_median_rent_real_2024_vs_acs = if (nrow(comparison_primary) == 0) NA_real_ else cor(comparison_primary$median_rent_real_2024, comparison_primary$acs_median_gross_rent),
  mean_mean_rent_real_2024_gap_vs_acs = if (nrow(comparison_primary) == 0) NA_real_ else mean(comparison_primary$mean_rent_real_2024_gap_vs_acs, na.rm = TRUE),
  median_mean_rent_real_2024_gap_vs_acs = if (nrow(comparison_primary) == 0) NA_real_ else median(comparison_primary$mean_rent_real_2024_gap_vs_acs, na.rm = TRUE),
  mean_median_rent_real_2024_gap_vs_acs = if (nrow(comparison_primary) == 0) NA_real_ else mean(comparison_primary$median_rent_real_2024_gap_vs_acs, na.rm = TRUE),
  median_median_rent_real_2024_gap_vs_acs = if (nrow(comparison_primary) == 0) NA_real_ else median(comparison_primary$median_rent_real_2024_gap_vs_acs, na.rm = TRUE),
  mean_absolute_median_rent_real_2024_gap_vs_acs = if (nrow(comparison_primary) == 0) NA_real_ else mean(abs(comparison_primary$median_rent_real_2024_gap_vs_acs), na.rm = TRUE)
)

message("Building figures...")
map_plot_data <- merge(chicago_block_groups, block_group_map_data, by = "GEOID", all.x = TRUE, sort = FALSE)

p_map_counts <- ggplot(map_plot_data) +
  geom_sf(aes(fill = n_listings), color = NA) +
  geom_sf(data = chicago_boundary, fill = NA, color = "#3a3a3a", linewidth = 0.12) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", na.value = "#f0f0f0") +
  theme_void() +
  labs(title = "Dwellsy Listings by Block Group", fill = "Listings")

p_map_median <- ggplot(map_plot_data) +
  geom_sf(aes(fill = median_rent_real_2024), color = NA) +
  geom_sf(data = chicago_boundary, fill = NA, color = "#3a3a3a", linewidth = 0.12) +
  scale_fill_gradient(low = "#fff7ec", high = "#d94801", na.value = "#f0f0f0", labels = scales::dollar_format()) +
  theme_void() +
  labs(title = "Median Dwellsy Rent by Block Group (2024$)", fill = "Median Rent")

p_map_mean_winsor <- ggplot(map_plot_data) +
  geom_sf(aes(fill = mean_rent_real_2024_winsor), color = NA) +
  geom_sf(data = chicago_boundary, fill = NA, color = "#3a3a3a", linewidth = 0.12) +
  scale_fill_gradient(low = "#edf8e9", high = "#006d2c", na.value = "#f0f0f0", labels = scales::dollar_format()) +
  theme_void() +
  labs(title = "Mean Winsorized Dwellsy Rent by Block Group (2024$)", fill = "Mean Rent")

ggsave(
  file.path(output_dir, "fig_dwellsy_chicago_block_group_maps.pdf"),
  p_map_counts + p_map_median + p_map_mean_winsor + patchwork::plot_layout(ncol = 3),
  width = 15,
  height = 6,
  bg = "white"
)

rent_per_sqft_plot_cap <- quantile(analysis_dt[is.finite(rent_per_sqft_real_2024), rent_per_sqft_real_2024], 0.99, na.rm = TRUE, type = 7)

p_rent_dist <- ggplot(analysis_dt[is.finite(rent_price_real_2024_winsor)], aes(x = rent_price_real_2024_winsor)) +
  geom_histogram(bins = 50, fill = "#2b8cbe", color = "white") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_minimal(base_size = 11) +
  labs(title = "Winsorized Rent (2024$)", x = "Rent", y = "Listings")

p_sqft_dist <- ggplot(analysis_dt[is.finite(sqft_winsor)], aes(x = sqft_winsor)) +
  geom_histogram(bins = 50, fill = "#7bccc4", color = "white") +
  theme_minimal(base_size = 11) +
  labs(title = "Winsorized Sqft", x = "Sqft", y = "Listings")

p_rpsf_dist <- ggplot(analysis_dt[is.finite(rent_per_sqft_real_2024)], aes(x = rent_per_sqft_real_2024)) +
  geom_histogram(bins = 50, fill = "#fc8d59", color = "white") +
  coord_cartesian(xlim = c(0, rent_per_sqft_plot_cap)) +
  theme_minimal(base_size = 11) +
  labs(title = "Rent per Sqft (2024$)", x = "Rent per Sqft", y = "Listings")

ggsave(
  file.path(output_dir, "fig_dwellsy_distributions.pdf"),
  p_rent_dist + p_sqft_dist + p_rpsf_dist + patchwork::plot_layout(ncol = 3),
  width = 15,
  height = 5,
  bg = "white"
)

p_acs_scatter <- ggplot(comparison_primary, aes(x = acs_median_gross_rent, y = median_rent_real_2024)) +
  geom_abline(intercept = 0, slope = 1, color = "#636363", linewidth = 0.7, linetype = "dashed") +
  geom_point(color = "#2b8cbe", alpha = 0.7, size = 1.8) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Block-Group Dwellsy Rent vs ACS Median Gross Rent (2024$)",
    subtitle = "Block groups with at least 5 Dwellsy listings",
    x = "ACS Median Gross Rent (2024$)",
    y = "Dwellsy Median Rent (2024$)"
  )

ggsave(
  file.path(output_dir, "fig_dwellsy_block_group_rent_vs_acs.pdf"),
  p_acs_scatter,
  width = 7,
  height = 6,
  bg = "white"
)

message("Writing outputs...")
write_csv(summary_stats, file.path(output_dir, "dwellsy_summary_stats.csv"))
write_csv(block_group_map_data, file.path(output_dir, "dwellsy_block_group_map_data.csv"))
write_csv(block_group_comparison, file.path(output_dir, "dwellsy_block_group_rent_comparison.csv"))
write_csv(comparison_summary, file.path(output_dir, "dwellsy_block_group_rent_comparison_summary.csv"))
write_csv(integrity_extremes, file.path(output_dir, "dwellsy_analysis_integrity_extremes.csv"))
write_csv(acs_block_group_rent, file.path(output_dir, "acs_block_group_rent_2024.csv"))

message("summary_stats_dwellsy complete.")
