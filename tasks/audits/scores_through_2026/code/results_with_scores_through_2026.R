# setwd("tasks/audits/scores_through_2026/code")
# Exploratory: the paper's boundary results with the score re-estimated on permits through June 2026 in place of the
# 2006-2022 score. Each observation keeps its own and neighboring aldermen; the more-stringent side is decided again
# by the score version, and boundaries whose two aldermen have equal scores drop out. Reports the average difference
# within 500 ft and the difference between the two 100 ft bands at the boundary.
#   Density: log units per acre, as in tasks/shared/code/density_boundary_helpers.R.
#   Rents and sales: the specification of tasks/price_boundary_results.
# The permit event study is unaffected: it classifies moves with the 2006-2014 score.
# Only buildings and properties whose two aldermen have scores in every version are kept, so samples are comparable.
price_bandwidth_ft <- 500L
price_bin_width_ft <- 100L
rent_years <- c(2014L, 2022L)
sales_years <- c(2006L, 2022L)
rent_controls <- "log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor"
sales_controls <- "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor"

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/density_boundary_helpers.R")
source("../../../shared/code/canonical_geometry_helpers.R")

# The published 2006-2022 score, and the versions of tasks/audits/scores_through_2026/code/build_scores_through_2026.R
# (2006-2022 and through June 2026, with and without self-certification permits).
published <- readr::read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE)
versions <- readr::read_csv("../output/scores_through_2026.csv", show_col_types = FALSE)
score_versions <- c(list(published = setNames(published$uncertainty_index, published$alderman)),
  lapply(split(versions, versions$score_version), \(x) setNames(x$uncertainty_index, x$alderman)))
scored_in_all <- Reduce(intersect, lapply(score_versions, names))
add_scores <- function(data, version) {
  score <- score_versions[[version]]
  data |>
    mutate(score_own = unname(score[alderman_own]), score_neighbor = unname(score[alderman_neighbor]))
}
results <- list()

# 1. Density.
buildings <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample() |>
  filter(abs(distance_to_boundary_ft) < density_bandwidth_ft, alderman_own %in% scored_in_all,
    alderman_neighbor %in% scored_in_all)
for (version in names(score_versions)) {
  scored <- add_scores(buildings, version)
  stopifnot(!anyNA(scored$score_own), !anyNA(scored$score_neighbor))
  scored <- scored |>
    filter(score_own != score_neighbor) |>
    mutate(running_distance_ft = abs(distance_to_boundary_ft) * sign(score_own - score_neighbor),
      side_changed = sign(score_own - score_neighbor) != sign(strictness_own - strictness_neighbor)) |>
    bin_running_distance()
  for (i in seq_len(nrow(density_samples))) {
    d <- filter_density_sample(scored, density_samples$sample[i])
    fit <- fit_density_boundary(d)
    results[[length(results) + 1]] <- bind_rows(
      mutate(fit$average, statistic = "average_difference"),
      transmute(fit$first_bin, estimate, std_error, p_value, statistic = "first_band_difference")
    ) |>
      mutate(outcome = "density_dupac", sample = density_samples$sample[i], score_version = version,
        observations = fit$observations, share_side_changed = mean(d$side_changed), .before = 1)
  }
}

# 2. Rents and sales.
rent <- arrow::read_parquet("../input/rental_rd_characteristics_panel_bw1500.parquet") |>
  mutate(
    file_date = as.Date(file_date), year = lubridate::year(file_date), year_month = format(file_date, "%Y-%m"),
    distance_ft = abs(as.numeric(signed_dist)), ward_pair = as.character(ward_pair_id), segment_id = as.character(segment_id),
    era = canonical_era_from_date(as.Date(assignment_date), allow_pre_2003 = FALSE),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_), beds_factor = factor(beds),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  ) |>
  filter(!is.na(file_date), year >= rent_years[1], year <= rent_years[2], is.finite(rent_price), rent_price > 0,
    is.finite(distance_ft), distance_ft < price_bandwidth_ft, alderman_own %in% scored_in_all,
    alderman_neighbor %in% scored_in_all, is.finite(strictness_own), is.finite(strictness_neighbor), !is.na(segment_id), segment_id != "", !is.na(ward_pair),
    ward_pair != "", !is.na(era), flag_clean_location_sample, is.finite(longitude), is.finite(latitude), is.finite(beds),
    beds >= 0, !is.na(log_sqft), !is.na(log_baths),
    if_all(c(nearest_school_dist_kft, nearest_park_dist_kft, nearest_major_road_dist_kft, nearest_cta_stop_dist_kft,
      lake_michigan_dist_kft), is.finite))
sales <- arrow::read_parquet("../input/sales_with_hedonics_amenities.parquet") |>
  mutate(
    sale_date = as.Date(sale_date), year = lubridate::year(sale_date),
    year_quarter = paste0(year, "-Q", lubridate::quarter(sale_date)), distance_ft = abs(as.numeric(signed_dist_m)) / 0.3048,
    ward_pair = as.character(ward_pair_id), segment_id = as.character(segment_id), property_class_factor = factor(class),
    era = canonical_era_from_date(sale_date, allow_pre_2003 = TRUE)
  ) |>
  filter(!is.na(sale_price), sale_price > 0, year >= sales_years[1], year <= sales_years[2], is.finite(distance_ft),
    distance_ft < price_bandwidth_ft, alderman_own %in% scored_in_all, alderman_neighbor %in% scored_in_all,
    is.finite(strictness_own),
    is.finite(strictness_neighbor), !is.na(segment_id), segment_id != "", !is.na(ward_pair), ward_pair != "", !is.na(era),
    is.finite(longitude), is.finite(latitude),
    if_all(c(log_sqft, log_land_sqft, log_building_age, log_bedrooms, log_baths, has_garage, nearest_school_dist_ft,
      nearest_park_dist_ft, nearest_major_road_dist_ft, nearest_cta_stop_dist_ft, lake_michigan_dist_ft), is.finite))
markets <- list(
  rent = list(data = rent, outcome = "log(rent_price)", controls = rent_controls, fixed_effects = "segment_id^year_month"),
  sales = list(data = sales, outcome = "log(sale_price)", controls = sales_controls, fixed_effects = "segment_id^year_quarter")
)
bin_edges <- seq(-price_bandwidth_ft, price_bandwidth_ft, by = price_bin_width_ft)
bin_labels <- sprintf("bin_%02d", seq_len(length(bin_edges) - 1L))
for (m in names(markets)) for (version in names(score_versions)) {
  d <- add_scores(markets[[m]]$data, version) |>
    filter(is.finite(score_own), is.finite(score_neighbor), score_own != score_neighbor) |>
    mutate(signed_ft = distance_ft * sign(score_own - score_neighbor), stricter_side = as.integer(signed_ft >= 0),
      distance_bin = cut(signed_ft, breaks = bin_edges, labels = bin_labels, include.lowest = TRUE, right = FALSE),
      side_changed = sign(score_own - score_neighbor) != sign(strictness_own - strictness_neighbor))
  fit <- function(rhs) fixest::feols(as.formula(sprintf("%s ~ %s + %s | %s", markets[[m]]$outcome, rhs,
    markets[[m]]$controls, markets[[m]]$fixed_effects)), data = d, cluster = ~ward_pair, warn = FALSE, notes = FALSE)
  average <- fit("stricter_side")
  bins <- fit("i(distance_bin, ref = 'bin_05')")
  results[[length(results) + 1]] <- tibble(outcome = m, sample = "within_500ft", score_version = version,
      observations = nobs(average), share_side_changed = mean(d$side_changed),
      statistic = c("average_difference", "first_band_difference"),
      estimate = c(coef(average)[["stricter_side"]], coef(bins)[["distance_bin::bin_06"]]),
      std_error = c(fixest::se(average)[["stricter_side"]], fixest::se(bins)[["distance_bin::bin_06"]]),
      p_value = c(fixest::pvalue(average)[["stricter_side"]], fixest::pvalue(bins)[["distance_bin::bin_06"]]))
}
write_csv(bind_rows(results), "../output/results_with_scores_through_2026.csv")
