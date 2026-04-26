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

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_did_land_transaction_incidence/code")
# cohort <- "cohort_2015"
# outcome_var <- "sold_any"
# treatment_type <- "continuous"
# weighting <- "uniform"
# bandwidth <- 1000
# geo_fe_level <- "segment"
# post_end <- 5
# sample_restriction <- "none"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(cohort, outcome_var, treatment_type, weighting, bandwidth, geo_fe_level, post_end, sample_restriction)
}

if (length(cli_args) != 8) {
  stop(
    paste(
      "FATAL: Script requires 8 args:",
      "<cohort> <outcome_var> <treatment_type> <weighting>",
      "<bandwidth> <geo_fe_level> <post_end> <sample_restriction>"
    ),
    call. = FALSE
  )
}

cohort <- cli_args[1]
outcome_var <- cli_args[2]
treatment_type <- tolower(cli_args[3])
weighting <- cli_args[4]
bandwidth <- as.numeric(cli_args[5])
geo_fe_level <- tolower(cli_args[6])
post_end <- as.integer(cli_args[7])
sample_restriction <- tolower(cli_args[8])

valid_outcomes <- c(
  "sold_any",
  "sold_any_arm_length",
  "sold_land_like",
  "sold_land_like_arm_length",
  "sold_land_tag",
  "sold_land_tag_arm_length"
)
if (!cohort %in% c("cohort_2012", "cohort_2015")) {
  stop("--cohort must be one of: cohort_2012, cohort_2015", call. = FALSE)
}
if (!outcome_var %in% valid_outcomes) {
  stop(sprintf("--outcome_var must be one of: %s", paste(valid_outcomes, collapse = ", ")), call. = FALSE)
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
if (!geo_fe_level %in% c("segment", "ward_pair")) {
  stop("--geo_fe_level must be one of: segment, ward_pair", call. = FALSE)
}
if (post_end < 0 || post_end > 5) {
  stop("--post_end must be between 0 and 5 for the active incidence DID runner.", call. = FALSE)
}
sample_restriction_info <- get_land_transaction_sample_restriction_info(sample_restriction)

suffix <- sprintf(
  "land_transaction_incidence_%s_%s_%s_%s_%dft_geo_%s_clust_block_post%d",
  cohort,
  outcome_var,
  treatment_type,
  weighting,
  as.integer(bandwidth),
  geo_fe_level,
  post_end
)
if (sample_restriction != "none") {
  suffix <- paste0(suffix, "_samp_", sample_restriction_info$suffix_tag)
}

data <- arrow::read_parquet("../input/land_transaction_incidence_model_panel.parquet") %>%
  tibble::as_tibble() %>%
  dplyr::filter(cohort == !!cohort) %>%
  dplyr::filter(relative_year >= -5, relative_year <= post_end) %>%
  dplyr::filter(if (bandwidth == 500) in_500ft %in% TRUE else dist_to_boundary_ft <= bandwidth) %>%
  dplyr::mutate(
    weight = if (weighting == "triangular") pmax(0, 1 - dist_to_boundary_ft / bandwidth) else 1,
    post = as.integer(relative_year >= 0)
  )

if (geo_fe_level == "segment") {
  data <- data %>%
    dplyr::filter(!is.na(segment_id), segment_id != "")
  fe_formula <- "pin10 + segment_id^calendar_year"
} else {
  data <- data %>%
    dplyr::filter(!is.na(ward_pair_id), ward_pair_id != "")
  fe_formula <- "pin10 + ward_pair_id^calendar_year"
}

sample_restriction_result <- apply_land_transaction_sample_restriction(
  data,
  sample_restriction = sample_restriction,
  unit_id_var = "pin10"
)
data <- sample_restriction_result$data

if (nrow(data) == 0) {
  stop("No observations remain after applying the requested incidence DID sample restrictions.", call. = FALSE)
}

metadata <- tibble::tibble(
  cohort = cohort,
  outcome_var = outcome_var,
  treatment_type = treatment_type,
  weighting = weighting,
  bandwidth = bandwidth,
  geo_fe_level = geo_fe_level,
  cluster_level = "block",
  post_end = post_end,
  sample_restriction = sample_restriction_info$sample_restriction,
  sample_restriction_label = sample_restriction_info$label,
  sample_restriction_obs_before = sample_restriction_result$summary$n_obs_before,
  sample_restriction_obs_after = sample_restriction_result$summary$n_obs_after,
  sample_restriction_obs_dropped = sample_restriction_result$summary$n_obs_dropped,
  sample_restriction_units_before = sample_restriction_result$summary$n_units_before,
  sample_restriction_units_after = sample_restriction_result$summary$n_units_after,
  sample_restriction_units_dropped = sample_restriction_result$summary$n_units_dropped,
  analysis_n = nrow(data),
  analysis_pin10 = dplyr::n_distinct(data$pin10),
  analysis_blocks = dplyr::n_distinct(data$block_id),
  treated_rows = sum(data$treat == 1, na.rm = TRUE),
  control_rows = sum(data$treat == 0, na.rm = TRUE),
  effective_weight_n = sum(data$weight, na.rm = TRUE),
  fe_formula = fe_formula
)

if (treatment_type == "continuous") {
  data <- data %>%
    dplyr::mutate(post_treat = post * strictness_change)

  formula_str <- sprintf("%s ~ post_treat | %s", outcome_var, fe_formula)
  model <- fixest::feols(
    stats::as.formula(formula_str),
    data = data,
    weights = ~weight,
    cluster = ~block_id
  )

  coefficients <- extract_did_row(model, "post_treat", "All parcels", "multiply100")
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

  formula_stricter <- sprintf("%s ~ post_treatment_stricter | %s", outcome_var, fe_formula)
  formula_lenient <- sprintf("%s ~ post_treatment_lenient | %s", outcome_var, fe_formula)

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
    extract_did_row(model_stricter, "post_treatment_stricter", "Moved to Stricter", "multiply100"),
    extract_did_row(model_lenient, "post_treatment_lenient", "Moved to More Lenient", "multiply100")
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
