# setwd("tasks/audits/score_vintage_reflection/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")
library(arrow)
setFixest_notes(FALSE)

score_data <- readRDS("../output/score_leaveout_data.rds")
score_2022 <- score_data$baseline_score |>
  dplyr::select(alderman, score)
score_2014 <- score_data$score_2006_2014 |>
  dplyr::select(alderman, score = score_2006_2014)

current_vintage_correlations <- dplyr::bind_rows(
  score_data$baseline_score |>
    dplyr::rename(score_full = score) |>
    dplyr::inner_join(
      score_data$score_2006_2014 |>
        dplyr::select(alderman, comparison_score = score_2006_2014),
      by = "alderman",
      relationship = "one-to-one"
    ) |>
    dplyr::summarise(
      comparison = "2006-2014 versus 2006-2022",
      n_aldermen = dplyr::n(),
      pearson_correlation = stats::cor(comparison_score, score_full),
      spearman_correlation = stats::cor(comparison_score, score_full, method = "spearman")
    ),
  score_data$baseline_score |>
    dplyr::rename(score_full = score) |>
    dplyr::inner_join(
      score_data$score_2015_2022 |>
        dplyr::select(alderman, comparison_score = score_2015_2022),
      by = "alderman",
      relationship = "one-to-one"
    ) |>
    dplyr::summarise(
      comparison = "2015-2022 versus 2006-2022",
      n_aldermen = dplyr::n(),
      pearson_correlation = stats::cor(comparison_score, score_full),
      spearman_correlation = stats::cor(comparison_score, score_full, method = "spearman")
    )
)

add_context_id <- function(data) {
  data |>
    dplyr::mutate(
      context_id = paste(
        year,
        as.character(ward),
        as.character(neighbor_ward),
        alderman_own,
        alderman_neighbor,
        sep = "|"
      )
    )
}

add_scores <- function(data, score_table) {
  own_scores <- score_table |>
    dplyr::rename(alderman_own = alderman, score_own = score)
  neighbor_scores <- score_table |>
    dplyr::rename(alderman_neighbor = alderman, score_neighbor = score)
  data |>
    dplyr::select(-dplyr::any_of(c("score_own", "score_neighbor"))) |>
    dplyr::left_join(own_scores, by = "alderman_own", relationship = "many-to-one") |>
    dplyr::left_join(
      neighbor_scores,
      by = "alderman_neighbor",
      relationship = "many-to-one"
    )
}

orient_distance <- function(data, absolute_distance_ft) {
  data |>
    dplyr::mutate(
      score_gap = abs(score_own - score_neighbor),
      score_sign = dplyr::case_when(
        score_own > score_neighbor ~ 1,
        score_own < score_neighbor ~ -1,
        TRUE ~ NA_real_
      ),
      running_distance_ft = {{ absolute_distance_ft }} * score_sign,
      pair_average_score = (score_own + score_neighbor) / 2,
      side = as.integer(running_distance_ft >= 0),
      distance_bin = cut(
        running_distance_ft,
        breaks = seq(-500, 500, by = 100),
        labels = sprintf("bin_%02d", 1:10),
        include.lowest = TRUE,
        right = FALSE
      )
    )
}

extract_result <- function(
    model,
    term,
    market,
    sample,
    outcome,
    score_method,
    estimator,
    clusters) {
  table <- fixest::coeftable(model)
  tibble::tibble(
    market,
    sample,
    outcome,
    score_method,
    estimator,
    estimate = unname(table[term, "Estimate"]),
    std_error = unname(table[term, "Std. Error"]),
    p_value = unname(table[term, "Pr(>|t|)"]),
    n = stats::nobs(model),
    clusters
  )
}

estimate_density <- function(data, sample_name, outcome, score_method) {
  model_data <- data |>
    dplyr::filter(
      construction_year >= 2006L,
      construction_year <= 2022L,
      within_500ft,
      dwelling_units > 0,
      sample_name == "all" | external_multifamily,
      allow_far,
      allow_dupac,
      is.finite(density_far),
      density_far > 0,
      is.finite(density_dupac),
      density_dupac > 0,
      is.finite(score_own),
      is.finite(score_neighbor),
      score_own != score_neighbor,
      is.finite(pair_average_score),
      is.finite(share_white_own),
      is.finite(share_black_own),
      is.finite(median_hh_income_own),
      is.finite(share_bach_plus_own),
      is.finite(homeownership_rate_own),
      !is.na(zone_group),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      ward_pair != "",
      abs(running_distance_ft) < 500,
      !is.na(distance_bin)
    ) |>
    dplyr::mutate(
      log_outcome = log(.data[[outcome]]),
      cluster_id = ward_pair
    )

  bin_model <- fixest::feols(
    log_outcome ~
      i(distance_bin, ref = "bin_05") +
      pair_average_score +
      share_white_own +
      share_black_own +
      median_hh_income_own +
      share_bach_plus_own +
      homeownership_rate_own |
      zone_group + segment_id + construction_year,
    data = model_data,
    cluster = ~ward_pair,
    warn = FALSE,
    notes = FALSE
  )
  flat_model <- fixest::feols(
    log_outcome ~
      side +
      pair_average_score +
      share_white_own +
      share_black_own +
      median_hh_income_own +
      share_bach_plus_own +
      homeownership_rate_own |
      zone_group + segment_id + construction_year,
    data = model_data,
    cluster = ~ward_pair,
    warn = FALSE,
    notes = FALSE
  )
  dplyr::bind_rows(
    extract_result(
      bin_model,
      "distance_bin::bin_06",
      "density",
      sample_name,
      outcome,
      score_method,
      "nearest_100ft_bin",
      dplyr::n_distinct(model_data$ward_pair)
    ),
    extract_result(
      flat_model,
      "side",
      "density",
      sample_name,
      outcome,
      score_method,
      "full_500ft",
      dplyr::n_distinct(model_data$ward_pair)
    )
  )
}

prepare_density <- function() {
  projects <- readr::read_csv(
    "../input/new_construction_analysis_data.csv",
    show_col_types = FALSE,
    col_types = readr::cols(
      project_id = readr::col_character(),
      ward = readr::col_character(),
      neighbor_ward = readr::col_character(),
      ward_pair = readr::col_character(),
      segment_id = readr::col_character(),
      .default = readr::col_guess()
    )
  ) |>
    dplyr::mutate(year = as.integer(construction_year)) |>
    add_context_id()
  if (anyDuplicated(projects$project_id)) {
    stop("Construction input is not unique by project ID.", call. = FALSE)
  }
  projects
}

rent_controls <- c(
  "log_sqft", "beds_factor", "log_baths", "building_type_factor",
  "nearest_school_dist_kft", "nearest_park_dist_kft",
  "nearest_major_road_dist_kft", "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft"
)
sales_controls <- c(
  "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms",
  "log_baths", "has_garage", "nearest_school_dist_ft",
  "nearest_park_dist_ft", "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
)

prepare_prices <- function() {
  rent <- arrow::read_parquet(
    "../input/rental_rd_characteristics_panel_bw1500.parquet"
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      file_date = as.Date(file_date),
      year = lubridate::year(file_date),
      year_month = format(file_date, "%Y-%m"),
      ward = as.character(ward),
      neighbor_ward = as.character(neighbor_ward),
      ward_pair = as.character(ward_pair_id),
      segment_id = as.character(segment_id),
      absolute_distance_ft = abs(as.numeric(signed_dist)),
      log_sqft = dplyr::if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
      beds_factor = factor(beds),
      log_baths = dplyr::if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
      building_type_factor = factor(dplyr::coalesce(building_type_clean, "other"))
    ) |>
    add_context_id() |>
    dplyr::filter(
      !is.na(file_date),
      year >= 2014L,
      year <= 2022L,
      is.finite(rent_price),
      rent_price > 0,
      absolute_distance_ft < 500,
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      ward_pair != "",
      flag_clean_location_sample,
      is.finite(longitude),
      is.finite(latitude),
      is.finite(beds),
      beds >= 0,
      !is.na(log_sqft),
      !is.na(log_baths),
      dplyr::if_all(dplyr::all_of(rent_controls[5:9]), is.finite)
    )

  sales <- arrow::read_parquet(
    "../input/sales_with_hedonics_amenities.parquet"
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      sale_date = as.Date(sale_date),
      year = lubridate::year(sale_date),
      year_quarter = paste0(year, "-Q", lubridate::quarter(sale_date)),
      ward = as.character(ward),
      neighbor_ward = as.character(neighbor_ward),
      ward_pair = as.character(ward_pair_id),
      segment_id = as.character(segment_id),
      absolute_distance_ft = abs(as.numeric(signed_dist_m)) / 0.3048
    ) |>
    add_context_id() |>
    dplyr::filter(
      !is.na(sale_price),
      sale_price > 0,
      year >= 2006L,
      year <= 2022L,
      absolute_distance_ft < 500,
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      ward_pair != "",
      is.finite(longitude),
      is.finite(latitude),
      dplyr::if_all(dplyr::all_of(sales_controls), is.finite)
    )
  list(rent = rent, sales = sales)
}

estimate_price <- function(data, market, score_method) {
  controls <- if (market == "rent") rent_controls else sales_controls
  outcome <- if (market == "rent") "rent_price" else "sale_price"
  fixed_effects <- if (market == "rent") {
    "segment_id^year_month"
  } else {
    "segment_id^year_quarter"
  }
  model_data <- data |>
    dplyr::filter(
      is.finite(score_own),
      is.finite(score_neighbor),
      score_own != score_neighbor,
      !is.na(distance_bin)
    ) |>
    dplyr::mutate(cluster_id = segment_id)
  bin_model <- fixest::feols(
    stats::as.formula(sprintf(
      "log(%s) ~ i(distance_bin, ref = 'bin_05') + %s | %s",
      outcome,
      paste(controls, collapse = " + "),
      fixed_effects
    )),
    data = model_data,
    cluster = ~segment_id,
    warn = FALSE,
    notes = FALSE
  )
  flat_model <- fixest::feols(
    stats::as.formula(sprintf(
      "log(%s) ~ side + %s | %s",
      outcome,
      paste(controls, collapse = " + "),
      fixed_effects
    )),
    data = model_data,
    cluster = ~segment_id,
    warn = FALSE,
    notes = FALSE
  )
  dplyr::bind_rows(
    extract_result(
      bin_model,
      "distance_bin::bin_06",
      market,
      "all",
      outcome,
      score_method,
      "nearest_100ft_bin",
      dplyr::n_distinct(model_data$segment_id)
    ),
    extract_result(
      flat_model,
      "side",
      market,
      "all",
      outcome,
      score_method,
      "full_500ft",
      dplyr::n_distinct(model_data$segment_id)
    )
  )
}

density <- prepare_density()
prices <- prepare_prices()

validate_scores <- function(data, label) {
  validation <- data |>
    add_scores(score_2022) |>
    dplyr::filter(is.finite(strictness_own), is.finite(strictness_neighbor))
  if (
    max(abs(validation$strictness_own - validation$score_own)) > 1e-10 ||
      max(abs(validation$strictness_neighbor - validation$score_neighbor)) > 1e-10
  ) {
    stop(paste(label, "does not contain the current through-2022 score."), call. = FALSE)
  }
}
validate_scores(density, "Density input")
validate_scores(prices$rent, "Rental input")
validate_scores(prices$sales, "Sales input")

ward_year_scores <- score_data$ward_year_leaveout_scores |>
  dplyr::select(context_id, score_own, score_neighbor)
project_scores <- score_data$project_leaveout_scores |>
  dplyr::select(project_id, score_own, score_neighbor)

score_versions <- list(
  baseline_2022 = function(data, market) add_scores(data, score_2022),
  ward_year_leaveout = function(data, market) {
    data |>
      dplyr::select(-dplyr::any_of(c("score_own", "score_neighbor"))) |>
      dplyr::left_join(ward_year_scores, by = "context_id", relationship = "many-to-one")
  },
  pre2015_common_sample_2022_score = function(data, market) {
    data |>
      dplyr::filter(
        alderman_own %in% score_2014$alderman,
        alderman_neighbor %in% score_2014$alderman
      ) |>
      add_scores(score_2022)
  },
  pre2015_score = function(data, market) add_scores(data, score_2014)
)

results <- list()
result_i <- 0L
for (method in names(score_versions)) {
  density_method <- score_versions[[method]](density, "density") |>
    orient_distance(abs(distance_to_boundary_ft))
  rent_method <- score_versions[[method]](prices$rent, "rent") |>
    orient_distance(absolute_distance_ft)
  sales_method <- score_versions[[method]](prices$sales, "sales") |>
    orient_distance(absolute_distance_ft)

  for (sample_name in c("all", "multifamily")) {
    for (outcome in c("density_far", "density_dupac")) {
      result_i <- result_i + 1L
      results[[result_i]] <- estimate_density(
        density_method,
        sample_name,
        outcome,
        method
      )
    }
  }
  result_i <- result_i + 1L
  results[[result_i]] <- estimate_price(rent_method, "rent", method)
  result_i <- result_i + 1L
  results[[result_i]] <- estimate_price(sales_method, "sales", method)
}

density_project_leaveout <- density |>
  dplyr::select(-dplyr::any_of(c("score_own", "score_neighbor"))) |>
  dplyr::left_join(project_scores, by = "project_id", relationship = "one-to-one") |>
  orient_distance(abs(distance_to_boundary_ft))
for (sample_name in c("all", "multifamily")) {
  for (outcome in c("density_far", "density_dupac")) {
    result_i <- result_i + 1L
    results[[result_i]] <- estimate_density(
      density_project_leaveout,
      sample_name,
      outcome,
      "project_leaveout"
    )
  }
}

baseline_density <- density |>
  add_scores(score_2022) |>
  orient_distance(abs(distance_to_boundary_ft))
baseline_rent <- prices$rent |>
  add_scores(score_2022) |>
  orient_distance(absolute_distance_ft)
baseline_sales <- prices$sales |>
  add_scores(score_2022) |>
  orient_distance(absolute_distance_ft)

for (threshold in c(0.25, 0.50, 0.75, 1.00)) {
  method <- sprintf("baseline_gap_at_least_%.2f_sd", threshold)
  density_threshold <- baseline_density |> dplyr::filter(score_gap >= threshold)
  rent_threshold <- baseline_rent |> dplyr::filter(score_gap >= threshold)
  sales_threshold <- baseline_sales |> dplyr::filter(score_gap >= threshold)
  for (sample_name in c("all", "multifamily")) {
    for (outcome in c("density_far", "density_dupac")) {
      result_i <- result_i + 1L
      results[[result_i]] <- estimate_density(
        density_threshold,
        sample_name,
        outcome,
        method
      )
    }
  }
  result_i <- result_i + 1L
  results[[result_i]] <- estimate_price(rent_threshold, "rent", method)
  result_i <- result_i + 1L
  results[[result_i]] <- estimate_price(sales_threshold, "sales", method)
}

boundary_results <- dplyr::bind_rows(results) |>
  dplyr::mutate(
    stars = dplyr::case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    )
  ) |>
  dplyr::arrange(market, sample, outcome, estimator, score_method)

classification_data <- list(
  density = density |>
    dplyr::filter(
      construction_year >= 2006L,
      construction_year <= 2022L,
      within_500ft,
      dwelling_units > 0,
      allow_far,
      allow_dupac,
      is.finite(density_far),
      density_far > 0,
      is.finite(density_dupac),
      density_dupac > 0,
      is.finite(share_white_own),
      is.finite(share_black_own),
      is.finite(median_hh_income_own),
      is.finite(share_bach_plus_own),
      is.finite(homeownership_rate_own),
      !is.na(zone_group),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      ward_pair != ""
    ) |>
    dplyr::select(
      project_id, segment_id, year, ward, neighbor_ward,
      alderman_own, alderman_neighbor
    ),
  rent = prices$rent |>
    dplyr::select(
      rent_panel_id, segment_id, year, ward, neighbor_ward,
      alderman_own, alderman_neighbor
    ),
  sales = prices$sales |>
    dplyr::select(
      row_id, segment_id, year, ward, neighbor_ward,
      alderman_own, alderman_neighbor
    )
)
classification_stability <- lapply(names(classification_data), function(market) {
  comparison <- classification_data[[market]] |>
    add_scores(score_2022) |>
    dplyr::rename(score_own_2022 = score_own, score_neighbor_2022 = score_neighbor) |>
    add_scores(score_2014) |>
    dplyr::filter(
      is.finite(score_own_2022),
      is.finite(score_neighbor_2022),
      is.finite(score_own),
      is.finite(score_neighbor)
    ) |>
    dplyr::mutate(
      sign_2022 = sign(score_own_2022 - score_neighbor_2022),
      sign_2014 = sign(score_own - score_neighbor),
      score_gap_2022 = abs(score_own_2022 - score_neighbor_2022),
      classification_flip = sign_2022 != sign_2014,
      market
    )
  observation_summary <- comparison |>
    dplyr::summarise(
      n_observations = dplyr::n(),
      n_segments = dplyr::n_distinct(segment_id),
      flip_share_observations = mean(classification_flip),
      median_score_gap_2022 = median(score_gap_2022),
      flip_share_gap_at_least_025 = mean(classification_flip[score_gap_2022 >= 0.25]),
      flip_share_gap_at_least_050 = mean(classification_flip[score_gap_2022 >= 0.50]),
      flip_share_gap_at_least_075 = mean(classification_flip[score_gap_2022 >= 0.75]),
      flip_share_gap_at_least_100 = mean(classification_flip[score_gap_2022 >= 1.00]),
      .by = market
    )
  segment_period_summary <- comparison |>
    dplyr::distinct(
      segment_id,
      year,
      alderman_own,
      alderman_neighbor,
      .keep_all = TRUE
    ) |>
    dplyr::summarise(
      n_segment_periods = dplyr::n(),
      flip_share_segment_periods = mean(classification_flip),
      flip_share_segment_periods_gap_at_least_025 = mean(
        classification_flip[score_gap_2022 >= 0.25]
      ),
      flip_share_segment_periods_gap_at_least_050 = mean(
        classification_flip[score_gap_2022 >= 0.50]
      ),
      flip_share_segment_periods_gap_at_least_075 = mean(
        classification_flip[score_gap_2022 >= 0.75]
      ),
      flip_share_segment_periods_gap_at_least_100 = mean(
        classification_flip[score_gap_2022 >= 1.00]
      ),
      .by = market
    )
  observation_summary |>
    dplyr::left_join(segment_period_summary, by = "market", relationship = "one-to-one")
}) |>
  dplyr::bind_rows()

summarise_leaveout_classification <- function(data, market, leaveout_scores, join_key) {
  comparison <- data |>
    add_scores(score_2022) |>
    dplyr::rename(
      baseline_score_own = score_own,
      baseline_score_neighbor = score_neighbor
    ) |>
    dplyr::left_join(leaveout_scores, by = join_key, relationship = "many-to-one") |>
    dplyr::mutate(
      baseline_sign = sign(baseline_score_own - baseline_score_neighbor),
      leaveout_sign = sign(score_own - score_neighbor),
      complete_leaveout = is.finite(score_own) & is.finite(score_neighbor),
      classification_flip = dplyr::if_else(
        complete_leaveout,
        baseline_sign != leaveout_sign,
        NA
      ),
      market
    )
  observation_summary <- comparison |>
    dplyr::summarise(
      n_observations = dplyr::n(),
      n_missing_leaveout = sum(!complete_leaveout),
      flip_share_observations = mean(classification_flip, na.rm = TRUE),
      .by = market
    )
  context_summary <- comparison |>
    dplyr::distinct(dplyr::across(dplyr::all_of(join_key)), .keep_all = TRUE) |>
    dplyr::summarise(
      n_contexts = dplyr::n(),
      n_missing_contexts = sum(!complete_leaveout),
      flip_share_contexts = mean(classification_flip, na.rm = TRUE),
      .by = market
    )
  observation_summary |>
    dplyr::left_join(context_summary, by = "market", relationship = "one-to-one")
}

leaveout_classification_stability <- dplyr::bind_rows(
  summarise_leaveout_classification(
    classification_data$density,
    "density_project_leaveout",
    project_scores,
    "project_id"
  ),
  summarise_leaveout_classification(
    add_context_id(classification_data$density),
    "density_ward_year_leaveout",
    ward_year_scores,
    "context_id"
  ),
  summarise_leaveout_classification(
    add_context_id(classification_data$rent),
    "rent_ward_year_leaveout",
    ward_year_scores,
    "context_id"
  ),
  summarise_leaveout_classification(
    add_context_id(classification_data$sales),
    "sales_ward_year_leaveout",
    ward_year_scores,
    "context_id"
  )
)

readr::write_csv(boundary_results, "../output/boundary_score_robustness.csv")
readr::write_csv(
  classification_stability,
  "../output/boundary_classification_stability.csv"
)
readr::write_csv(
  leaveout_classification_stability,
  "../output/leaveout_classification_stability.csv"
)
readr::write_csv(
  current_vintage_correlations,
  "../output/current_vintage_correlations.csv"
)
