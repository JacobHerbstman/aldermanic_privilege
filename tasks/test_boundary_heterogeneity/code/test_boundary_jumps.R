# setwd("tasks/test_boundary_heterogeneity/code")
# Do outcomes jump at ward boundaries, in either direction, more than at placebo lines, and do the jumps change when
# the aldermen facing each other change? No stringency measure is used: each side is labeled by ward number.
# A cell is a boundary segment during one pair of facing aldermen (density: segment by joint service; rents and
# sales: segment by the two aldermen's names). Within each cell the jump is the average difference in the log
# outcome between the lower- and higher-numbered ward's side within 500 ft, with the main specifications' controls
# and fixed effects. Placebo lines lie 1,000 ft inside each ward, where both sides share one alderman, and get the
# same treatment. Jump dispersion beyond sampling noise is estimated as mean(estimate^2 - se^2) across cells; the
# change in a segment's jump between consecutive alderman pairs as mean(difference^2 - se1^2 - se2^2) / 2. Each cell's
# jump is identified within one ward pair, so standard errors are clustered by building (rents: repeated listings of
# a building; density: one row per building) or property (sales), not by ward pair.
bandwidth_ft <- 500
placebo_ft <- 1000
# Cells enter only with at least this many observations on each side of the line.
min_per_side <- c(density_all = 3L, density_multifamily = 3L, rents = 20L, sales = 10L)
start_year <- 2006L
end_year <- 2022L
rent_start_year <- 2014L
# Controls, property-type fixed effects and time fixed effects as in tasks/price_boundary_results.
rent_controls <- "log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor"
sales_controls <- "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/density_boundary_helpers.R")
source("../../shared/code/canonical_geometry_helpers.R")

# Each sample: outcome, controls, time fixed effects, distance from the boundary in feet (unsigned), which side of
# the boundary (the observation's own ward is the lower-numbered one), cell and the date that orders cells in time.
buildings <- read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample() |>
  transmute(sample = "density_all", outcome = log(density_dupac), distance_ft = abs(signed_distance_m) / 0.3048,
    low_ward_side = as.integer(as.integer(ward) < as.integer(neighbor_ward)), stricter_side = as.integer(signed_distance_m >= 0),
    segment_id, cell = paste(segment_id, joint_service), date = as.Date(sprintf("%d-06-15", construction_year)),
    cluster_id = building_id,
    ward_pair, zone_group, share_white_own, share_black_own, median_hh_income_own, share_bach_plus_own,
    homeownership_rate_own, multifamily, dwelling_units)
densities <- bind_rows(buildings,
  filter(buildings, multifamily, dwelling_units >= 2) |> mutate(sample = "density_multifamily"))

rents <- arrow::read_parquet("../input/rental_rd_characteristics_panel_bw1500.parquet") |>
  mutate(file_date = as.Date(file_date), assignment_date = as.Date(assignment_date), year = lubridate::year(file_date),
    era = canonical_era_from_date(assignment_date, allow_pre_2003 = FALSE),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    beds_factor = factor(beds), building_type_factor = factor(coalesce(building_type_clean, "other"))) |>
  filter(!is.na(file_date), year >= rent_start_year, year <= end_year, is.finite(rent_price), rent_price > 0,
    is.finite(signed_dist), abs(signed_dist) < placebo_ft + bandwidth_ft, is.finite(strictness_own),
    is.finite(strictness_neighbor), strictness_own != strictness_neighbor, !is.na(segment_id), segment_id != "",
    !is.na(ward_pair_id), ward_pair_id != "", !is.na(era), flag_clean_location_sample, is.finite(longitude),
    is.finite(latitude), is.finite(beds), beds >= 0, !is.na(log_sqft), !is.na(log_baths),
    if_all(c(nearest_school_dist_kft, nearest_park_dist_kft, nearest_major_road_dist_kft, nearest_cta_stop_dist_kft,
      lake_michigan_dist_kft), is.finite)) |>
  transmute(sample = "rents", outcome = log(rent_price), distance_ft = abs(signed_dist),
    low_ward_side = as.integer(ward < neighbor_ward), stricter_side = as.integer(signed_dist >= 0),
    segment_id = as.character(segment_id),
    cell = paste(segment_id, pmin(alderman_own, alderman_neighbor), pmax(alderman_own, alderman_neighbor)),
    date = file_date, time = format(file_date, "%Y-%m"), ward_pair = as.character(ward_pair_id),
    cluster_id = sub("^(([^|]*\\|){2}[^|]*)\\|.*$", "\\1", rent_panel_id),
    log_sqft, beds_factor, log_baths, nearest_school_dist_kft, nearest_park_dist_kft, nearest_major_road_dist_kft,
    nearest_cta_stop_dist_kft, lake_michigan_dist_kft, building_type_factor)

sales <- arrow::read_parquet("../input/sales_with_hedonics_amenities.parquet") |>
  mutate(sale_date = as.Date(sale_date), year = lubridate::year(sale_date),
    era = canonical_era_from_date(sale_date, allow_pre_2003 = TRUE), signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
    property_class_factor = factor(class)) |>
  filter(!is.na(sale_price), sale_price > 0, year >= start_year, year <= end_year, is.finite(signed_dist_ft),
    abs(signed_dist_ft) < placebo_ft + bandwidth_ft, is.finite(strictness_own), is.finite(strictness_neighbor),
    strictness_own != strictness_neighbor, !is.na(segment_id), segment_id != "", !is.na(ward_pair_id),
    ward_pair_id != "", !is.na(era), is.finite(longitude), is.finite(latitude),
    if_all(c(log_sqft, log_land_sqft, log_building_age, log_bedrooms, log_baths, has_garage, nearest_school_dist_ft,
      nearest_park_dist_ft, nearest_major_road_dist_ft, nearest_cta_stop_dist_ft, lake_michigan_dist_ft), is.finite)) |>
  transmute(sample = "sales", outcome = log(sale_price), distance_ft = abs(signed_dist_ft),
    low_ward_side = as.integer(ward < neighbor_ward), stricter_side = as.integer(signed_dist_ft >= 0),
    segment_id = as.character(segment_id),
    cell = paste(segment_id, pmin(alderman_own, alderman_neighbor), pmax(alderman_own, alderman_neighbor)),
    date = sale_date, time = paste0(year, "-Q", lubridate::quarter(sale_date)), ward_pair = as.character(ward_pair_id),
    cluster_id = as.character(pin),
    log_sqft, log_land_sqft, log_building_age, log_bedrooms, log_baths, has_garage, nearest_school_dist_ft,
    nearest_park_dist_ft, nearest_major_road_dist_ft, nearest_cta_stop_dist_ft, lake_michigan_dist_ft,
    property_class_factor)

regressors <- list(density_all = density_controls, density_multifamily = density_controls, rents = rent_controls,
  sales = sales_controls)
# Density: cell (segment by joint service) and zoning-group fixed effects, as in the main specification; prices:
# segment-by-month (rents) or segment-by-quarter (sales) fixed effects, nested within cells.
fixed_effects <- list(density_all = "cell + zone_group", density_multifamily = "cell + zone_group",
  rents = "cell + segment_id^time", sales = "cell + segment_id^time")
samples <- list(density_all = filter(densities, sample == "density_all"),
  density_multifamily = filter(densities, sample == "density_multifamily"), rents = rents, sales = sales)

# The main average differences (more-stringent side) must match the paper's before anything else is estimated.
published_prices <- read_csv("../input/price_boundary_estimates.csv", show_col_types = FALSE) |>
  filter(check == "main") |>
  distinct(market, average_estimate)
for (s in c("rents", "sales")) {
  main <- fixest::feols(stats::as.formula(sprintf("outcome ~ stricter_side + %s | segment_id^time", regressors[[s]])),
    data = filter(samples[[s]], distance_ft < bandwidth_ft), cluster = ~ward_pair, notes = FALSE, warn = FALSE)
  published <- published_prices$average_estimate[published_prices$market == if (s == "rents") "rent" else "sales"]
  stopifnot(abs(coef(main)[["stricter_side"]] - published) < 1e-6)
}

# Jumps across a line within each cell. line = boundary: side is the lower-numbered ward's side, within 500 ft of the
# boundary. line = placebo_low or placebo_high: a line 1,000 ft inside that ward; side is the half farther from the
# boundary, within 500 ft of the placebo line.
cell_jumps <- function(sample, line) {
  data <- samples[[sample]]
  data <- if (line == "boundary") {
    data |> filter(distance_ft < bandwidth_ft) |> mutate(side = low_ward_side)
  } else {
    in_ward <- if (line == "placebo_low") 1L else 0L
    data |>
      filter(low_ward_side == in_ward, abs(distance_ft - placebo_ft) < bandwidth_ft) |>
      mutate(side = as.integer(distance_ft >= placebo_ft))
  }
  eligible <- data |>
    summarise(n_side = sum(side == 1L), n_other = sum(side == 0L), .by = cell) |>
    filter(n_side >= min_per_side[[sample]], n_other >= min_per_side[[sample]])
  data <- semi_join(data, eligible, by = "cell")
  model <- fixest::feols(stats::as.formula(sprintf("outcome ~ i(cell, side) + %s | %s", regressors[[sample]],
    fixed_effects[[sample]])), data = data, cluster = ~cluster_id, notes = FALSE, warn = FALSE)
  estimates <- fixest::coeftable(model)
  estimates <- estimates[grepl("^cell::", rownames(estimates)), , drop = FALSE]
  tibble(sample, line, cell = sub("^cell::(.*):side$", "\\1", rownames(estimates)),
    estimate = estimates[, "Estimate"], std_error = estimates[, "Std. Error"]) |>
    left_join(eligible, by = "cell", relationship = "one-to-one") |>
    left_join(summarise(data, segment_id = first(segment_id), first_date = min(date), .by = cell), by = "cell",
      relationship = "one-to-one")
}
jumps <- bind_rows(lapply(names(samples), function(s) {
  bind_rows(lapply(c("boundary", "placebo_low", "placebo_high"), function(l) cell_jumps(s, l)))
}))
SaveData(jumps, c("sample", "line", "cell"), "../output/boundary_cell_jumps.csv")

# Dispersion beyond sampling noise, across cells and between consecutive cells of the same segment.
consecutive <- jumps |>
  arrange(sample, line, segment_id, first_date) |>
  mutate(next_estimate = lead(estimate), next_std_error = lead(std_error), .by = c(sample, line, segment_id)) |>
  filter(!is.na(next_estimate))
summary <- bind_rows(
  jumps |>
    summarise(comparison = "across_cells", units = n(), signal_variance = mean(estimate^2 - std_error^2),
      signal_variance_se = sd(estimate^2 - std_error^2) / sqrt(n()),
      share_significant = mean(abs(estimate / std_error) > 1.96), .by = c(sample, line)),
  consecutive |>
    summarise(comparison = "within_segment_change", units = n(),
      signal_variance = mean((next_estimate - estimate)^2 - std_error^2 - next_std_error^2) / 2,
      signal_variance_se = sd((next_estimate - estimate)^2 - std_error^2 - next_std_error^2) / 2 / sqrt(n()),
      share_significant = mean(abs(next_estimate - estimate) / sqrt(std_error^2 + next_std_error^2) > 1.96),
      .by = c(sample, line))
) |>
  mutate(signal_sd = sqrt(pmax(signal_variance, 0)))
SaveData(summary, c("sample", "line", "comparison"), "../output/boundary_jump_heterogeneity.csv")
