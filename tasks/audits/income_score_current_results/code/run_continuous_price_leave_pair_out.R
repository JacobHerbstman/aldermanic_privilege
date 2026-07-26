# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/income_score_current_results/code")
# workers <- 8

source("../../../setup_environment/code/packages.R")
library(arrow)
setFixest_notes(FALSE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- workers
}
if (length(cli_args) != 1) {
  stop("Script requires the number of workers.", call. = FALSE)
}

workers <- as.integer(cli_args[1])
if (!is.finite(workers) || workers < 1) {
  stop("workers must be a positive integer.", call. = FALSE)
}

scores <- read_csv("../output/current_income_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L) %>%
  select(alderman, variant, score)

score_maps <- split(scores, scores$variant) %>%
  lapply(function(data) setNames(data$score, data$alderman))

rent <- read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    beds_factor = factor(beds),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
  ) %>%
  filter(
    !is.na(file_date),
    year >= 2014,
    year <= 2022,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(as.numeric(signed_dist)),
    abs(as.numeric(signed_dist)) <= 500,
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    flag_clean_location_sample,
    is.finite(beds),
    beds >= 0,
    !is.na(log_sqft),
    !is.na(log_baths),
    if_all(
      all_of(c(
        "nearest_school_dist_kft",
        "nearest_park_dist_kft",
        "nearest_major_road_dist_kft",
        "nearest_cta_stop_dist_kft",
        "lake_michigan_dist_kft"
      )),
      is.finite
    )
  )

rent_controls <- "log_sqft + beds_factor + log_baths"
if (n_distinct(rent$building_type_factor) > 1) {
  rent_controls <- paste(rent_controls, "+ building_type_factor")
}
rent_formula <- as.formula(paste0(
  "log(rent_price) ~ relative_score + pair_average_score + ",
  rent_controls,
  " + nearest_school_dist_kft + nearest_park_dist_kft + ",
  "nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + ",
  "lake_michigan_dist_kft | segment_id^year_month"
))

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    signed_dist = as.numeric(signed_dist_m) / 0.3048
  ) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= 2006,
    year <= 2022,
    !is.na(ward_pair),
    !is.na(segment_id),
    segment_id != "",
    is.finite(signed_dist),
    abs(signed_dist) <= 500,
    if_all(
      all_of(c(
        "log_sqft",
        "log_land_sqft",
        "log_building_age",
        "log_bedrooms",
        "log_baths",
        "has_garage",
        "nearest_school_dist_ft",
        "nearest_park_dist_ft",
        "nearest_major_road_dist_ft",
        "nearest_cta_stop_dist_ft",
        "lake_michigan_dist_ft"
      )),
      ~ !is.na(.x)
    )
  )

sales_formula <- log(sale_price) ~ relative_score + pair_average_score +
  log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths +
  has_garage + nearest_school_dist_ft + nearest_park_dist_ft +
  nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft |
  segment_id^year_quarter

prepare_variant <- function(data, score_map) {
  data %>%
    mutate(
      score_own = unname(score_map[alderman_own]),
      score_neighbor = unname(score_map[alderman_neighbor]),
      relative_score = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2
    ) %>%
    filter(is.finite(score_own), is.finite(score_neighbor), alderman_own != alderman_neighbor)
}

analysis_rows <- list()
for (variant in names(score_maps)) {
  analysis_rows[[length(analysis_rows) + 1L]] <- list(
    analysis = "rental",
    variant = variant,
    data = prepare_variant(rent, score_maps[[variant]]),
    formula = rent_formula
  )
  analysis_rows[[length(analysis_rows) + 1L]] <- list(
    analysis = "sales",
    variant = variant,
    data = prepare_variant(sales, score_maps[[variant]]),
    formula = sales_formula
  )
}

expected_results <- read_csv(
  "../output/current_income_score_results.csv",
  show_col_types = FALSE
) %>%
  filter(analysis %in% c("rental", "sales"), treatment == "continuous")

setFixest_nthreads(1)
available_cores <- parallel::detectCores(logical = FALSE)
if (!is.finite(available_cores) || available_cores < 1) {
  available_cores <- workers
}
workers <- max(1L, min(workers, available_cores))

leave_out_results <- list()
for (analysis_row in analysis_rows) {
  full_model <- feols(
    analysis_row$formula,
    data = analysis_row$data,
    warn = FALSE,
    notes = FALSE
  )
  full_estimate <- unname(coef(full_model)[["relative_score"]])
  expected <- expected_results %>%
    filter(
      analysis == analysis_row$analysis,
      variant == analysis_row$variant
    )

  if (nrow(expected) != 1L || abs(full_estimate - expected$estimate) > 1e-8) {
    stop("A leave-pair-out baseline does not reproduce the audit estimate.", call. = FALSE)
  }

  pair_counts <- analysis_row$data %>%
    mutate(
      alderman_comparison = paste(
        pmin(alderman_own, alderman_neighbor),
        pmax(alderman_own, alderman_neighbor),
        sep = " | "
      )
    ) %>%
    group_by(ward_pair) %>%
    summarise(
      n_omitted = n(),
      alderman_comparisons = paste(
        sort(unique(alderman_comparison)),
        collapse = "; "
      ),
      .groups = "drop"
    ) %>%
    arrange(ward_pair)

  pair_results <- parallel::mclapply(
    pair_counts$ward_pair,
    function(pair_name) {
      model <- feols(
        analysis_row$formula,
        data = analysis_row$data %>% filter(ward_pair != pair_name),
        cluster = ~segment_id,
        warn = FALSE,
        notes = FALSE
      )
      tibble(
        ward_pair = pair_name,
        estimate_without_pair = unname(coef(model)[["relative_score"]]),
        std_error_without_pair = unname(se(model)[["relative_score"]]),
        p_value_without_pair = unname(pvalue(model)[["relative_score"]])
      )
    },
    mc.cores = workers,
    mc.preschedule = TRUE
  )

  if (any(vapply(pair_results, inherits, logical(1), what = "try-error"))) {
    stop("At least one leave-pair-out regression failed.", call. = FALSE)
  }

  leave_out_results[[length(leave_out_results) + 1L]] <- bind_rows(pair_results) %>%
    left_join(pair_counts, by = "ward_pair", relationship = "one-to-one") %>%
    mutate(
      analysis = analysis_row$analysis,
      variant = analysis_row$variant,
      full_estimate,
      change_from_full = estimate_without_pair - full_estimate,
      n_full = nrow(analysis_row$data),
      .before = 1
    )
}

leave_out_results <- bind_rows(leave_out_results) %>%
  arrange(analysis, variant, desc(abs(change_from_full)))

write_csv(
  leave_out_results,
  "../output/continuous_price_leave_pair_out.csv"
)

write_csv(
  leave_out_results %>%
    group_by(analysis, variant, full_estimate, n_full) %>%
    summarise(
      n_ward_pairs = n(),
      minimum_estimate = min(estimate_without_pair),
      maximum_estimate = max(estimate_without_pair),
      p025 = quantile(estimate_without_pair, 0.025),
      median = median(estimate_without_pair),
      p975 = quantile(estimate_without_pair, 0.975),
      maximum_absolute_change = max(abs(change_from_full)),
      most_influential_pair = ward_pair[which.max(abs(change_from_full))],
      most_influential_aldermen = alderman_comparisons[
        which.max(abs(change_from_full))
      ],
      most_influential_pair_change = change_from_full[which.max(abs(change_from_full))],
      most_influential_pair_p_value = p_value_without_pair[
        which.max(abs(change_from_full))
      ],
      maximum_p_value = max(p_value_without_pair),
      share_significant_5pct = mean(p_value_without_pair < 0.05),
      sign_reversals = sum(sign(estimate_without_pair) != sign(full_estimate)),
      .groups = "drop"
    ),
  "../output/continuous_price_leave_pair_out_summary.csv"
)
