source("../../setup_environment/code/packages.R")
source("../../_lib/land_transaction_model_helpers.R")

options(dplyr.summarise.inform = FALSE)

extract_did_row <- function(model, term, group_label, display_mode = c("multiply100", "exp_minus_one")) {
  display_mode <- match.arg(display_mode)

  estimate <- unname(stats::coef(model)[term])
  std_error <- sqrt(diag(stats::vcov(model)))[term]
  ci_bounds <- stats::confint(model, parm = term, level = 0.95)
  ci_low <- as.numeric(ci_bounds[1, 1])
  ci_high <- as.numeric(ci_bounds[1, 2])

  out <- tibble::tibble(
    group = group_label,
    term = term,
    estimate = estimate,
    std_error = unname(std_error),
    ci_low = ci_low,
    ci_high = ci_high
  )

  if (display_mode == "exp_minus_one") {
    out %>%
      dplyr::mutate(
        estimate_display = 100 * (exp(estimate) - 1),
        ci_low_display = 100 * (exp(ci_low) - 1),
        ci_high_display = 100 * (exp(ci_high) - 1),
        display_unit = "%"
      )
  } else {
    out %>%
      dplyr::mutate(
        estimate_display = 100 * estimate,
        ci_low_display = 100 * ci_low,
        ci_high_display = 100 * ci_high,
        display_unit = "pp"
      )
  }
}

raw_price_outcome_from_var <- function(outcome_var) {
  dplyr::case_when(
    outcome_var == "log_sale_price" ~ "sale_price",
    outcome_var == "log_sale_price_psf_current" ~ "sale_price_psf_current_raw",
    TRUE ~ NA_character_
  )
}

prepare_price_outcome <- function(df, outcome_var, winsorization) {
  raw_outcome_var <- raw_price_outcome_from_var(outcome_var)

  df <- df %>%
    dplyr::filter(!is.na(.data[[raw_outcome_var]]), .data[[raw_outcome_var]] > 0)

  if (winsorization == "none") {
    df <- df %>%
      dplyr::mutate(
        outcome_raw_for_estimation = .data[[raw_outcome_var]],
        outcome_estimation = log(outcome_raw_for_estimation),
        outcome_winsorized_flag = FALSE
      )

    return(list(
      data = df,
      raw_outcome_var = raw_outcome_var,
      lower = NA_real_,
      upper = NA_real_,
      n_modified = 0L,
      share_modified = 0
    ))
  }

  if (winsorization == "p01_p99") {
    winsor_result <- winsorize_numeric_series(df[[raw_outcome_var]], 0.01, 0.99)

    df <- df %>%
      dplyr::mutate(
        outcome_raw_for_estimation = winsor_result$values,
        outcome_estimation = log(outcome_raw_for_estimation),
        outcome_winsorized_flag = abs(outcome_raw_for_estimation - .data[[raw_outcome_var]]) > 1e-8
      )

    return(list(
      data = df,
      raw_outcome_var = raw_outcome_var,
      lower = winsor_result$lower,
      upper = winsor_result$upper,
      n_modified = winsor_result$n_modified,
      share_modified = winsor_result$share_modified
    ))
  }

  stop("--winsorization must be one of: none, p01_p99", call. = FALSE)
}

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_did_land_transaction_price/code")
# cohort <- "cohort_2015"
# outcome_var <- "log_sale_price"
# sample_flag <- "price_sample_arm_length_land_like"
# treatment_type <- "continuous"
# weighting <- "uniform"
# bandwidth <- 1000
# geo_fe_level <- "ward_pair"
# post_end <- 3
# sample_restriction <- "none"
# winsorization <- "p01_p99"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(cohort, outcome_var, sample_flag, treatment_type, weighting, bandwidth, geo_fe_level, post_end, sample_restriction, winsorization)
}

if (!length(cli_args) %in% c(9, 10)) {
  stop(
    paste(
      "FATAL: Script requires 9 or 10 args:",
      "<cohort> <outcome_var> <sample_flag> <treatment_type>",
      "<weighting> <bandwidth> <geo_fe_level> <post_end> <sample_restriction> [<winsorization>]"
    ),
    call. = FALSE
  )
}

cohort <- cli_args[1]
outcome_var <- cli_args[2]
sample_flag <- cli_args[3]
treatment_type <- tolower(cli_args[4])
weighting <- cli_args[5]
bandwidth <- as.numeric(cli_args[6])
geo_fe_level <- tolower(cli_args[7])
post_end <- as.integer(cli_args[8])
sample_restriction <- tolower(cli_args[9])
winsorization <- if (length(cli_args) >= 10) tolower(cli_args[10]) else "none"

valid_sample_flags <- c(
  "price_sample_arm_length_land_like",
  "price_sample_broad_land_like",
  "price_sample_arm_length_raw_land",
  "price_sample_raw_land_small_package_le2",
  "price_sample_warranty_small_package_le2"
)
if (!cohort %in% c("cohort_2012", "cohort_2015")) {
  stop("--cohort must be one of: cohort_2012, cohort_2015", call. = FALSE)
}
if (!outcome_var %in% c("log_sale_price", "log_sale_price_psf_current")) {
  stop("--outcome_var must be one of: log_sale_price, log_sale_price_psf_current", call. = FALSE)
}
if (!sample_flag %in% valid_sample_flags) {
  stop(sprintf("--sample_flag must be one of: %s", paste(valid_sample_flags, collapse = ", ")), call. = FALSE)
}
if (!treatment_type %in% c("continuous", "continuous_split")) {
  stop("--treatment_type must be one of: continuous, continuous_split", call. = FALSE)
}
if (!weighting %in% c("uniform", "triangular")) {
  stop("--weighting must be one of: uniform, triangular", call. = FALSE)
}
if (!bandwidth %in% c(500, 1000)) {
  stop("--bandwidth must be one of: 500, 1000", call. = FALSE)
}
if (!geo_fe_level %in% c("ward_pair", "segment")) {
  stop("--geo_fe_level must be one of: ward_pair, segment", call. = FALSE)
}
if (post_end < 0 || post_end > 5) {
  stop("--post_end must be between 0 and 5 for the active price DID runner.", call. = FALSE)
}
if (!winsorization %in% c("none", "p01_p99")) {
  stop("--winsorization must be one of: none, p01_p99", call. = FALSE)
}
sample_restriction_info <- get_land_transaction_sample_restriction_info(sample_restriction)

suffix <- sprintf(
  "land_transaction_price_%s_%s_%s_%s_%s_%dft_geo_%s_clust_block_post%d",
  cohort,
  outcome_var,
  sample_flag,
  treatment_type,
  weighting,
  as.integer(bandwidth),
  geo_fe_level,
  post_end
)
if (sample_restriction != "none") {
  suffix <- paste0(suffix, "_samp_", sample_restriction_info$suffix_tag)
}
if (winsorization != "none") {
  suffix <- paste0(suffix, "_winsor_", gsub("_", "", winsorization))
}

data <- arrow::read_parquet("../input/land_transaction_price_model_panel.parquet") %>%
  tibble::as_tibble() %>%
  dplyr::filter(cohort == !!cohort) %>%
  dplyr::filter(.data[[sample_flag]] %in% TRUE) %>%
  dplyr::filter(relative_year >= -5, relative_year <= post_end) %>%
  dplyr::filter(if (bandwidth == 500) in_500ft %in% TRUE else dist_to_boundary_ft <= bandwidth) %>%
  dplyr::mutate(
    sale_price_psf_current_raw = dplyr::if_else(
      is.finite(sale_price) & sale_price > 0 &
        is.finite(lot_sqft_current) & lot_sqft_current > 0,
      sale_price / lot_sqft_current,
      NA_real_
    ),
    weight = if (weighting == "triangular") pmax(0, 1 - dist_to_boundary_ft / bandwidth) else 1,
    post = as.integer(relative_year >= 0)
  )

if (geo_fe_level == "ward_pair") {
  data <- data %>%
    dplyr::filter(!is.na(ward_pair_side), ward_pair_side != "", !is.na(ward_pair_id), ward_pair_id != "")
  fe_formula <- "ward_pair_side + ward_pair_id^sale_year"
} else {
  data <- data %>%
    dplyr::filter(!is.na(segment_side), segment_side != "", !is.na(segment_id), segment_id != "")
  fe_formula <- "segment_side + segment_id^sale_year"
}

sample_restriction_result <- apply_land_transaction_sample_restriction(
  data,
  sample_restriction = sample_restriction,
  unit_id_var = "sale_event_id"
)
data <- sample_restriction_result$data

if (nrow(data) == 0) {
  stop("No observations remain after applying the requested price DID sample restrictions.", call. = FALSE)
}

price_outcome_result <- prepare_price_outcome(data, outcome_var, winsorization)
data <- price_outcome_result$data

if (nrow(data) == 0) {
  stop("No observations remain after preparing the requested price DID outcome.", call. = FALSE)
}

metadata <- tibble::tibble(
  cohort = cohort,
  outcome_var = outcome_var,
  sample_flag = sample_flag,
  treatment_type = treatment_type,
  weighting = weighting,
  bandwidth = bandwidth,
  geo_fe_level = geo_fe_level,
  cluster_level = "block",
  post_end = post_end,
  winsorization = winsorization,
  sample_restriction = sample_restriction_info$sample_restriction,
  sample_restriction_label = sample_restriction_info$label,
  sample_restriction_obs_before = sample_restriction_result$summary$n_obs_before,
  sample_restriction_obs_after = sample_restriction_result$summary$n_obs_after,
  sample_restriction_obs_dropped = sample_restriction_result$summary$n_obs_dropped,
  sample_restriction_units_before = sample_restriction_result$summary$n_units_before,
  sample_restriction_units_after = sample_restriction_result$summary$n_units_after,
  sample_restriction_units_dropped = sample_restriction_result$summary$n_units_dropped,
  analysis_n = nrow(data),
  analysis_sale_events = dplyr::n_distinct(data$sale_event_id),
  analysis_pin10 = dplyr::n_distinct(data$pin10),
  analysis_blocks = dplyr::n_distinct(data$block_id),
  treated_sale_events = sum(data$treat == 1, na.rm = TRUE),
  control_sale_events = sum(data$treat == 0, na.rm = TRUE),
  effective_weight_n = sum(data$weight, na.rm = TRUE),
  raw_outcome_var = price_outcome_result$raw_outcome_var,
  winsor_lower = price_outcome_result$lower,
  winsor_upper = price_outcome_result$upper,
  n_winsorized = price_outcome_result$n_modified,
  share_winsorized = price_outcome_result$share_modified,
  fe_formula = fe_formula,
  outcome_caveat = dplyr::if_else(
    outcome_var == "log_sale_price_psf_current",
    "Price per square foot uses current lot square feet because sale-time lot size history is unavailable in this branch.",
    ""
  )
)

if (treatment_type == "continuous") {
  data <- data %>%
    dplyr::mutate(post_treat = post * strictness_change)

  formula_str <- sprintf("outcome_estimation ~ post_treat | %s", fe_formula)
  model <- fixest::feols(
    stats::as.formula(formula_str),
    data = data,
    weights = ~weight,
    cluster = ~block_id
  )

  coefficients <- extract_did_row(model, "post_treat", "All sales", "exp_minus_one")
  readr::write_csv(coefficients, sprintf("../output/did_coefficients_%s.csv", suffix))
  readr::write_csv(
    metadata %>% dplyr::mutate(formula = formula_str),
    sprintf("../output/did_metadata_%s.csv", suffix)
  )
} else {
  data <- data %>%
    dplyr::mutate(
      post_treatment_stricter = post * treatment_stricter_continuous,
      post_treatment_lenient = post * treatment_lenient_continuous
    )

  formula_stricter <- sprintf("outcome_estimation ~ post_treatment_stricter | %s", fe_formula)
  formula_lenient <- sprintf("outcome_estimation ~ post_treatment_lenient | %s", fe_formula)

  model_stricter <- fixest::feols(
    stats::as.formula(formula_stricter),
    data = data,
    weights = ~weight,
    cluster = ~block_id
  )
  model_lenient <- fixest::feols(
    stats::as.formula(formula_lenient),
    data = data,
    weights = ~weight,
    cluster = ~block_id
  )

  coefficients <- dplyr::bind_rows(
    extract_did_row(model_stricter, "post_treatment_stricter", "Moved to Stricter", "exp_minus_one"),
    extract_did_row(model_lenient, "post_treatment_lenient", "Moved to More Lenient", "exp_minus_one")
  )
  readr::write_csv(coefficients, sprintf("../output/did_coefficients_%s.csv", suffix))
  readr::write_csv(
    metadata %>%
      dplyr::mutate(
        formula_stricter = formula_stricter,
        formula_lenient = formula_lenient
      ),
    sprintf("../output/did_metadata_%s.csv", suffix)
  )
}
