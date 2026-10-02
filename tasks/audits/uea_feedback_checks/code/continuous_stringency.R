# setwd("tasks/audits/uea_feedback_checks/code")
# Exploratory (UEA feedback): is there a power gain from the size of the stringency difference rather than only its
# sign? Each design is estimated with its binary treatment and with the continuous score difference in its place.
#   Event study (pooled post-2015 effects, PPML): sign of the destination-minus-origin change versus the change
#     itself, combined and split by direction (the split uses the size of each move).
#   Boundaries (average difference within 500 ft): the more-stringent-side indicator versus that indicator times the
#     gap between the two sides' scores, so the coefficient is the difference across a boundary per standard deviation
#     of score gap. Density uses the paper's density specification; rents and sales use the price specification.
# Event study: blocks within 500 ft (152.4 m), as in tasks/run_event_study_permit.
bandwidth_m <- 152.4
event_specs <- tibble::tribble(
  ~spec, ~fixed_effect, ~first_year, ~last_year,
  "both_sides_2010_2020", "ward_pair_id^year", 2010L, 2020L,
  "same_side_2012_2018", "ward_pair_side^year", 2012L, 2018L
)
# Price samples and specification, as in tasks/price_boundary_results.
price_bandwidth_ft <- 500L
rent_years <- c(2014L, 2022L)
sales_years <- c(2006L, 2022L)
rent_controls <- "log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor"
sales_controls <- "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor"

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/density_boundary_helpers.R")
source("../../../shared/code/canonical_geometry_helpers.R")

row_from <- function(model, term, design, sample, treatment, mean_size) {
  tibble(design, sample, treatment, term, estimate = coef(model)[[term]], std_error = fixest::se(model)[[term]],
    t_stat = estimate / std_error, p_value = fixest::pvalue(model)[[term]], observations = nobs(model), mean_size)
}
results <- list()

# 1. Event study.
panel <- arrow::read_parquet("../input/permit_block_year_panel_2015.parquet") |>
  filter(dist_m <= bandwidth_m, relative_year >= -5L, relative_year <= 5L, !is.na(strictness_change_frozen),
    !is.na(ward_pair_id), ward_pair_id != "", stable_both)
active <- panel |>
  filter(relative_year < 0L) |>
  summarise(pre_permits = sum(n_high_discretion_application), .by = block_id) |>
  filter(pre_permits > 0)
panel <- panel |>
  filter(block_id %in% active$block_id) |>
  mutate(
    post = as.integer(year >= 2015L),
    change = strictness_change_frozen,
    post_sign = post * sign(change), post_change = post * change,
    post_stricter = post * (change > 0), post_lenient = post * (change < 0),
    post_stricter_size = post * pmax(change, 0), post_lenient_size = post * pmax(-change, 0)
  )
stopifnot(all(panel$change[!panel$switched] == 0), all(panel$change[panel$switched] != 0))
sizes <- panel |> filter(switched, year == 2014L) |> summarise(all = mean(abs(change)), stricter = mean(change[change > 0]),
  lenient = mean(-change[change < 0]))
for (s in seq_len(nrow(event_specs))) {
  d <- filter(panel, year >= event_specs$first_year[s], year <= event_specs$last_year[s])
  fit <- function(rhs) fixest::fepois(as.formula(paste("n_high_discretion_application ~", rhs, "| block_id +",
    event_specs$fixed_effect[s])), data = d, cluster = ~ward_pair_id, notes = FALSE)
  binary <- fit("post_sign"); continuous <- fit("post_change")
  binary_split <- fit("post_stricter + post_lenient"); continuous_split <- fit("post_stricter_size + post_lenient_size")
  results[[length(results) + 1]] <- bind_rows(
    row_from(binary, "post_sign", "event_study", event_specs$spec[s], "binary", 1),
    row_from(continuous, "post_change", "event_study", event_specs$spec[s], "continuous", sizes$all),
    row_from(binary_split, "post_stricter", "event_study", event_specs$spec[s], "binary", 1),
    row_from(continuous_split, "post_stricter_size", "event_study", event_specs$spec[s], "continuous", sizes$stricter),
    row_from(binary_split, "post_lenient", "event_study", event_specs$spec[s], "binary", 1),
    row_from(continuous_split, "post_lenient_size", "event_study", event_specs$spec[s], "continuous", sizes$lenient)
  )
}

# 2. Density, in the paper's specification.
buildings <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample() |>
  mutate(running_distance_ft = signed_distance_m / 0.3048) |>
  bin_running_distance() |>
  mutate(score_gap = abs(strictness_own - strictness_neighbor), stricter_side_gap = stricter_side * score_gap)
for (i in seq_len(nrow(density_samples))) {
  d <- filter_density_sample(buildings, density_samples$sample[i])
  fit <- function(treatment) fixest::feols(as.formula(sprintf("log(density_dupac) ~ %s + %s | %s", treatment,
    density_controls, density_fixed_effects)), data = d, cluster = ~ward_pair, warn = FALSE, notes = FALSE)
  results[[length(results) + 1]] <- bind_rows(
    row_from(fit("stricter_side"), "stricter_side", "density", density_samples$sample[i], "binary", 1),
    row_from(fit("stricter_side_gap"), "stricter_side_gap", "density", density_samples$sample[i], "continuous",
      mean(d$score_gap))
  )
}

# 3. Rents and sales, in the price specification (tasks/price_boundary_results).
rent <- arrow::read_parquet("../input/rental_rd_characteristics_panel_bw1500.parquet") |>
  mutate(
    file_date = as.Date(file_date), year = lubridate::year(file_date), year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist), ward_pair = as.character(ward_pair_id), segment_id = as.character(segment_id),
    era = canonical_era_from_date(as.Date(assignment_date), allow_pre_2003 = FALSE),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_), beds_factor = factor(beds),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  ) |>
  filter(!is.na(file_date), year >= rent_years[1], year <= rent_years[2], is.finite(rent_price), rent_price > 0,
    is.finite(signed_dist_ft), abs(signed_dist_ft) < price_bandwidth_ft, is.finite(strictness_own),
    is.finite(strictness_neighbor), strictness_own != strictness_neighbor, !is.na(segment_id), segment_id != "",
    !is.na(ward_pair), ward_pair != "", !is.na(era), flag_clean_location_sample, is.finite(longitude), is.finite(latitude),
    is.finite(beds), beds >= 0, !is.na(log_sqft), !is.na(log_baths),
    if_all(c(nearest_school_dist_kft, nearest_park_dist_kft, nearest_major_road_dist_kft, nearest_cta_stop_dist_kft,
      lake_michigan_dist_kft), is.finite))
sales <- arrow::read_parquet("../input/sales_with_hedonics_amenities.parquet") |>
  mutate(
    sale_date = as.Date(sale_date), year = lubridate::year(sale_date),
    year_quarter = paste0(year, "-Q", lubridate::quarter(sale_date)), signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
    ward_pair = as.character(ward_pair_id), segment_id = as.character(segment_id), property_class_factor = factor(class),
    era = canonical_era_from_date(sale_date, allow_pre_2003 = TRUE)
  ) |>
  filter(!is.na(sale_price), sale_price > 0, year >= sales_years[1], year <= sales_years[2], is.finite(signed_dist_ft),
    abs(signed_dist_ft) < price_bandwidth_ft, is.finite(strictness_own), is.finite(strictness_neighbor),
    strictness_own != strictness_neighbor, !is.na(segment_id), segment_id != "", !is.na(ward_pair), ward_pair != "",
    !is.na(era), is.finite(longitude), is.finite(latitude),
    if_all(c(log_sqft, log_land_sqft, log_building_age, log_bedrooms, log_baths, has_garage, nearest_school_dist_ft,
      nearest_park_dist_ft, nearest_major_road_dist_ft, nearest_cta_stop_dist_ft, lake_michigan_dist_ft), is.finite))
markets <- list(
  rent = list(data = rent, outcome = "log(rent_price)", controls = rent_controls, fixed_effects = "segment_id^year_month"),
  sales = list(data = sales, outcome = "log(sale_price)", controls = sales_controls, fixed_effects = "segment_id^year_quarter")
)
for (m in names(markets)) {
  d <- markets[[m]]$data |>
    mutate(stricter_side = as.integer(signed_dist_ft >= 0), score_gap = abs(strictness_own - strictness_neighbor),
      stricter_side_gap = stricter_side * score_gap)
  fit <- function(treatment) fixest::feols(as.formula(sprintf("%s ~ %s + %s | %s", markets[[m]]$outcome, treatment,
    markets[[m]]$controls, markets[[m]]$fixed_effects)), data = d, cluster = ~ward_pair, warn = FALSE, notes = FALSE)
  results[[length(results) + 1]] <- bind_rows(
    row_from(fit("stricter_side"), "stricter_side", m, "within_500ft", "binary", 1),
    row_from(fit("stricter_side_gap"), "stricter_side_gap", m, "within_500ft", "continuous", mean(d$score_gap))
  )
}

# The continuous coefficient times the mean size (score change or gap) is comparable to the binary coefficient.
results <- bind_rows(results) |>
  mutate(estimate_at_mean_size = estimate * mean_size)
write_csv(results, "../output/continuous_stringency.csv")
