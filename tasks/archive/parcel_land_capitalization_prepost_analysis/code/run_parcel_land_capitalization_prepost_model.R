source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/parcel_land_capitalization_prepost_analysis/code")
# in_prepost <- "../input/parcel_land_capitalization_prepost_2014_2016.parquet"
# sample_scope <- "all_baseline_empty"
# year_scope <- "admin_only"
# bandwidth <- "1000ft"
# outcome <- "delta_log_land_psf_2016_minus_2014"
# treatment_spec <- "sign"
# out_coefficients <- "../output/model_coefficients_all_baseline_empty__admin_only__1000ft__sign__delta_log_land_psf_2016_minus_2014.csv"
# out_metadata <- "../output/model_metadata_all_baseline_empty__admin_only__1000ft__sign__delta_log_land_psf_2016_minus_2014.csv"
# fe_scope <- "segment_id"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    in_prepost,
    sample_scope,
    year_scope,
    bandwidth,
    outcome,
    treatment_spec,
    out_coefficients,
    out_metadata,
    fe_scope
  )
}

if (!length(cli_args) %in% c(8, 9)) {
  stop(
    paste(
      "FATAL: Script requires 8 or 9 args:",
      "<in_prepost_parquet> <sample_scope> <year_scope> <bandwidth> <outcome> <treatment_spec>",
      "<out_coefficients_csv> <out_metadata_csv> [fe_scope]"
    ),
    call. = FALSE
  )
}

in_prepost <- cli_args[1]
sample_scope <- cli_args[2]
year_scope <- cli_args[3]
bandwidth <- cli_args[4]
outcome <- cli_args[5]
treatment_spec <- cli_args[6]
out_coefficients <- cli_args[7]
out_metadata <- cli_args[8]
fe_scope <- if (length(cli_args) == 9) cli_args[9] else "segment_id"
fe_scope_name <- fe_scope

stopifnot(file.exists(in_prepost))

valid_year_scopes <- c("admin_only", "admin_plus_fallback")
valid_bandwidths <- c("1000ft", "500ft")
valid_fe_scopes <- c("segment_id", "ward_pair_id")
valid_sample_scopes <- c(
  "all_baseline_empty",
  "no_ex",
  "developable_loose",
  "developable_core",
  "history_vacant_loose",
  "history_vacant_core",
  "history_vacant_urban_core",
  "current_vacant_loose",
  "current_vacant_core",
  "current_vacant_private_core"
)
valid_treatment_specs <- c("continuous", "sign")

if (!sample_scope %in% valid_sample_scopes) {
  stop(sprintf("sample_scope must be one of: %s", paste(valid_sample_scopes, collapse = ", ")), call. = FALSE)
}
if (!year_scope %in% valid_year_scopes) {
  stop(sprintf("year_scope must be one of: %s", paste(valid_year_scopes, collapse = ", ")), call. = FALSE)
}
if (!bandwidth %in% valid_bandwidths) {
  stop(sprintf("bandwidth must be one of: %s", paste(valid_bandwidths, collapse = ", ")), call. = FALSE)
}
if (!treatment_spec %in% valid_treatment_specs) {
  stop(sprintf("treatment_spec must be one of: %s", paste(valid_treatment_specs, collapse = ", ")), call. = FALSE)
}
if (!fe_scope %in% valid_fe_scopes) {
  stop(sprintf("fe_scope must be one of: %s", paste(valid_fe_scopes, collapse = ", ")), call. = FALSE)
}

extract_term_results <- function(model, treatment_spec_i) {
  if (is.null(model)) {
    return(tibble(
      term = if (treatment_spec_i == "continuous") "strictness_change" else c("treatment_sign::to_lenient", "treatment_sign::to_stricter"),
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_
    ))
  }

  coef_table <- coeftable(model)
  conf_int <- confint(model)
  p_col <- grep("^Pr\\(", colnames(coef_table), value = TRUE)[1]
  stat_col <- grep("value$", colnames(coef_table), value = TRUE)[1]

  coef_df <- tibble(
    term = rownames(coef_table),
    estimate = as.numeric(coef_table[, "Estimate"]),
    std_error = as.numeric(coef_table[, "Std. Error"]),
    statistic = if (!is.na(stat_col)) as.numeric(coef_table[, stat_col]) else NA_real_,
    p_value = if (!is.na(p_col)) as.numeric(coef_table[, p_col]) else NA_real_
  )

  conf_df <- tibble(
    term = rownames(conf_int),
    conf_low = as.numeric(conf_int[, 1]),
    conf_high = as.numeric(conf_int[, 2])
  )

  term_keep <- if (treatment_spec_i == "continuous") {
    "strictness_change"
  } else {
    c("treatment_sign::to_lenient", "treatment_sign::to_stricter")
  }

  coef_df %>%
    left_join(conf_df, by = "term") %>%
    filter(term %in% term_keep) %>%
    right_join(tibble(term = term_keep), by = "term") %>%
    select(term, estimate, std_error, statistic, p_value, conf_low, conf_high)
}

prepost <- arrow::read_parquet(in_prepost) %>%
  mutate(
    pin10 = as.character(pin10),
    in_500ft = as.logical(in_500ft),
    in_1000ft = as.logical(in_1000ft),
    switched_2015 = as.logical(switched_2015),
    valid_control_2015 = as.logical(valid_control_2015),
    sample_no_ex = as.logical(sample_no_ex),
    sample_developable_loose = as.logical(sample_developable_loose),
    sample_developable_core = as.logical(sample_developable_core),
    sample_history_vacant_loose = as.logical(sample_history_vacant_loose),
    sample_history_vacant_core = as.logical(sample_history_vacant_core),
    sample_history_vacant_urban_core = as.logical(sample_history_vacant_urban_core),
    sample_current_vacant_land_loose = as.logical(sample_current_vacant_land_loose),
    sample_current_vacant_land_core = as.logical(sample_current_vacant_land_core),
    sample_current_vacant_private_core = as.logical(sample_current_vacant_private_core),
    treatment_sign = factor(treatment_sign, levels = c("no_change", "to_lenient", "to_stricter"))
  )

if (!outcome %in% names(prepost)) {
  stop(sprintf("Outcome column not found in prepost panel: %s", outcome), call. = FALSE)
}

if ("admin_only_prepost" %in% names(prepost)) {
  prepost <- prepost %>%
    mutate(admin_only_prepost = as.logical(admin_only_prepost))
} else if ("admin_only_2014_2016" %in% names(prepost)) {
  prepost <- prepost %>%
    mutate(admin_only_prepost = as.logical(admin_only_2014_2016))
} else {
  stop("Prepost panel is missing admin-only year-source flag.", call. = FALSE)
}

if (nrow(prepost) == 0) {
  stop("Input prepost parquet is empty.", call. = FALSE)
}

if (anyDuplicated(prepost["pin10"]) > 0) {
  stop("Input prepost parquet has duplicate pin10 rows.", call. = FALSE)
}

analysis_df <- prepost %>%
  filter(
    case_when(
      sample_scope == "all_baseline_empty" ~ TRUE,
      sample_scope == "no_ex" ~ sample_no_ex %in% TRUE,
      sample_scope == "developable_loose" ~ sample_developable_loose %in% TRUE,
      sample_scope == "developable_core" ~ sample_developable_core %in% TRUE,
      sample_scope == "history_vacant_loose" ~ sample_history_vacant_loose %in% TRUE,
      sample_scope == "history_vacant_core" ~ sample_history_vacant_core %in% TRUE,
      sample_scope == "history_vacant_urban_core" ~ sample_history_vacant_urban_core %in% TRUE,
      sample_scope == "current_vacant_loose" ~ sample_current_vacant_land_loose %in% TRUE,
      sample_scope == "current_vacant_core" ~ sample_current_vacant_land_core %in% TRUE,
      sample_scope == "current_vacant_private_core" ~ sample_current_vacant_private_core %in% TRUE,
      TRUE ~ FALSE
    ),
    if (year_scope == "admin_only") admin_only_prepost %in% TRUE else TRUE,
    if (bandwidth == "500ft") in_500ft %in% TRUE else in_1000ft %in% TRUE,
    !is.na(.data[[fe_scope_name]]),
    .data[[fe_scope_name]] != "",
    !is.na(strictness_change),
    !is.na(treatment_sign),
    !is.na(.data[[outcome]])
  )

segment_support <- analysis_df %>%
  group_by(fe_unit = .data[[fe_scope_name]]) %>%
  summarise(
    n_obs = n(),
    n_treat_values = if (treatment_spec == "continuous") n_distinct(strictness_change) else n_distinct(treatment_sign),
    has_no_change = any(treatment_sign == "no_change"),
    has_switcher = any(treatment_sign != "no_change"),
    .groups = "drop"
  )

n_identifying_segments <- if (treatment_spec == "continuous") {
  sum(segment_support$n_treat_values > 1)
} else {
  sum(segment_support$has_no_change & segment_support$has_switcher)
}

model <- NULL
model_status <- "ok"

if (nrow(analysis_df) == 0) {
  model_status <- "empty_sample"
} else if (n_identifying_segments == 0) {
  model_status <- "no_identifying_segments"
} else {
  formula_text <- if (treatment_spec == "continuous") {
    paste0(outcome, " ~ strictness_change | ", fe_scope_name)
  } else {
    paste0(outcome, ' ~ i(treatment_sign, ref = "no_change") | ', fe_scope_name)
  }

  model <- tryCatch(
    feols(
      as.formula(formula_text),
      data = analysis_df,
      cluster = as.formula(paste0("~", fe_scope_name)),
      warn = FALSE
    ),
    error = function(e) {
      model_status <<- paste0("fixest_error: ", conditionMessage(e))
      NULL
    }
  )
}

coefficients_df <- extract_term_results(model, treatment_spec) %>%
  mutate(
    sample_scope = sample_scope,
    year_scope = year_scope,
    bandwidth = bandwidth,
    outcome = outcome,
    treatment_spec = treatment_spec,
    fe_scope = fe_scope_name,
    pre_year = if ("pre_year" %in% names(prepost)) dplyr::first(prepost$pre_year) else NA_integer_,
    post_year = if ("post_year" %in% names(prepost)) dplyr::first(prepost$post_year) else NA_integer_,
    baseline_start_year = if ("baseline_start_year" %in% names(prepost)) dplyr::first(prepost$baseline_start_year) else NA_integer_,
    baseline_end_year = if ("baseline_end_year" %in% names(prepost)) dplyr::first(prepost$baseline_end_year) else NA_integer_,
    term_label = case_when(
      term == "strictness_change" ~ "Strictness change",
      term == "treatment_sign::to_lenient" ~ "To more lenient",
      term == "treatment_sign::to_stricter" ~ "To stricter",
      TRUE ~ term
    ),
    n_obs = nrow(analysis_df),
    n_segments = n_distinct(analysis_df$segment_id),
    n_identifying_segments = n_identifying_segments,
    n_ward_pairs = n_distinct(analysis_df$ward_pair_id),
    n_fe_units = n_distinct(analysis_df[[fe_scope_name]])
  ) %>%
  select(
    sample_scope, year_scope, bandwidth, outcome, treatment_spec,
    term, term_label, estimate, std_error, statistic, p_value, conf_low, conf_high,
    fe_scope, pre_year, post_year, baseline_start_year, baseline_end_year,
    n_obs, n_segments, n_identifying_segments, n_ward_pairs, n_fe_units
  )

metadata_df <- tibble(
  sample_scope = sample_scope,
  year_scope = year_scope,
  bandwidth = bandwidth,
  outcome = outcome,
  treatment_spec = treatment_spec,
  fe_scope = fe_scope_name,
  pre_year = if ("pre_year" %in% names(prepost)) dplyr::first(prepost$pre_year) else NA_integer_,
  post_year = if ("post_year" %in% names(prepost)) dplyr::first(prepost$post_year) else NA_integer_,
  baseline_start_year = if ("baseline_start_year" %in% names(prepost)) dplyr::first(prepost$baseline_start_year) else NA_integer_,
  baseline_end_year = if ("baseline_end_year" %in% names(prepost)) dplyr::first(prepost$baseline_end_year) else NA_integer_,
  model_status = model_status,
  n_obs = nrow(analysis_df),
  n_segments = n_distinct(analysis_df$segment_id),
  n_identifying_segments = n_identifying_segments,
  n_ward_pairs = n_distinct(analysis_df$ward_pair_id),
  n_fe_units = n_distinct(analysis_df[[fe_scope_name]]),
  n_to_lenient = sum(analysis_df$treatment_sign == "to_lenient"),
  n_no_change = sum(analysis_df$treatment_sign == "no_change"),
  n_to_stricter = sum(analysis_df$treatment_sign == "to_stricter"),
  n_switchers = sum(analysis_df$switched_2015 %in% TRUE),
  n_valid_controls = sum(analysis_df$valid_control_2015 %in% TRUE),
  mean_outcome = if (nrow(analysis_df) > 0) mean(analysis_df[[outcome]], na.rm = TRUE) else NA_real_,
  sd_outcome = if (nrow(analysis_df) > 0) stats::sd(analysis_df[[outcome]], na.rm = TRUE) else NA_real_,
  mean_abs_strictness_change = if (nrow(analysis_df) > 0) mean(abs(analysis_df$strictness_change), na.rm = TRUE) else NA_real_,
  r2 = if (!is.null(model)) tryCatch(as.numeric(r2(model, type = "r2")), error = function(e) NA_real_) else NA_real_,
  within_r2 = if (!is.null(model)) tryCatch(as.numeric(r2(model, type = "wr2")), error = function(e) NA_real_) else NA_real_,
  fe = fe_scope_name,
  cluster = fe_scope_name
)

readr::write_csv(coefficients_df, out_coefficients)
readr::write_csv(metadata_df, out_metadata)

message(sprintf("Saved %s", out_coefficients))
message(sprintf("Saved %s", out_metadata))
