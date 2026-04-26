source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_block_land_value_panel_models/code")
# in_panel <- "../input/block_land_value_panel.parquet"
# sample_scope <- "history_vacant_core"
# bandwidth <- "1000ft"
# year_scope <- "admin_95"
# event_year <- 2015L
# outcome <- "building_positive_share"
# treatment_spec <- "sign"
# year_window <- "core"
# fe_spec <- "segment_year"
# control_spec <- "none"
# out_coefficients <- "../output/event_coefficients_history_vacant_core__1000ft__admin95__event2015__sign__building_positive_share.csv"
# out_pretrend <- "../output/event_pretrend_history_vacant_core__1000ft__admin95__event2015__sign__building_positive_share.csv"
# out_metadata <- "../output/event_metadata_history_vacant_core__1000ft__admin95__event2015__sign__building_positive_share.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    in_panel,
    sample_scope,
    bandwidth,
    year_scope,
    event_year,
    outcome,
    treatment_spec,
    year_window,
    fe_spec,
    control_spec,
    out_coefficients,
    out_pretrend,
    out_metadata
  )
}

if (length(cli_args) != 13) {
  stop(
    paste(
      "FATAL: Script requires 13 args:",
      "<block_panel_parquet> <sample_scope> <bandwidth> <year_scope> <event_year>",
      "<outcome> <treatment_spec> <year_window> <fe_spec> <control_spec>",
      "<out_coefficients_csv> <out_pretrend_csv> <out_metadata_csv>"
    ),
    call. = FALSE
  )
}

in_panel <- cli_args[1]
sample_scope <- cli_args[2]
bandwidth <- cli_args[3]
year_scope <- cli_args[4]
event_year <- as.integer(cli_args[5])
outcome <- cli_args[6]
treatment_spec <- cli_args[7]
year_window <- cli_args[8]
fe_spec <- cli_args[9]
control_spec <- cli_args[10]
out_coefficients <- cli_args[11]
out_pretrend <- cli_args[12]
out_metadata <- cli_args[13]

stopifnot(file.exists(in_panel))

valid_sample_scopes <- c("developable_core", "history_vacant_core")
valid_bandwidths <- c("1000ft", "500ft")
valid_year_scopes <- c("admin_only", "admin_95", "admin_plus_fallback")
valid_year_windows <- c("core", "long_pre")
valid_event_years <- c(2012L, 2015L)
valid_fe_specs <- c("segment_year", "ward_pair_year")
valid_control_specs <- c("none", "baseline_x_year")
valid_outcomes <- c(
  "log_land_psf_block",
  "log_land_sum_block",
  "mean_log_land_psf",
  "building_positive_share",
  "land_share_block",
  "land_positive_share"
)
valid_treatment_specs <- c("continuous", "sign")

if (!sample_scope %in% valid_sample_scopes) {
  stop(sprintf("sample_scope must be one of: %s", paste(valid_sample_scopes, collapse = ", ")), call. = FALSE)
}
if (!bandwidth %in% valid_bandwidths) {
  stop(sprintf("bandwidth must be one of: %s", paste(valid_bandwidths, collapse = ", ")), call. = FALSE)
}
if (!year_scope %in% valid_year_scopes) {
  stop(sprintf("year_scope must be one of: %s", paste(valid_year_scopes, collapse = ", ")), call. = FALSE)
}
if (!year_window %in% valid_year_windows) {
  stop(sprintf("year_window must be one of: %s", paste(valid_year_windows, collapse = ", ")), call. = FALSE)
}
if (!event_year %in% valid_event_years) {
  stop(sprintf("event_year must be one of: %s", paste(valid_event_years, collapse = ", ")), call. = FALSE)
}
if (!outcome %in% valid_outcomes) {
  stop(sprintf("outcome must be one of: %s", paste(valid_outcomes, collapse = ", ")), call. = FALSE)
}
if (!treatment_spec %in% valid_treatment_specs) {
  stop(sprintf("treatment_spec must be one of: %s", paste(valid_treatment_specs, collapse = ", ")), call. = FALSE)
}
if (!fe_spec %in% valid_fe_specs) {
  stop(sprintf("fe_spec must be one of: %s", paste(valid_fe_specs, collapse = ", ")), call. = FALSE)
}
if (!control_spec %in% valid_control_specs) {
  stop(sprintf("control_spec must be one of: %s", paste(valid_control_specs, collapse = ", ")), call. = FALSE)
}

extract_event_terms <- function(model, treatment_spec_i, outcome_i) {
  if (is.null(model)) {
    return(tibble::tibble())
  }

  coef_table <- fixest::coeftable(model)
  conf_int <- stats::confint(model)
  p_col <- grep("^Pr\\(", colnames(coef_table), value = TRUE)[1]
  stat_col <- grep("value$", colnames(coef_table), value = TRUE)[1]

  coef_df <- tibble::tibble(
    term = rownames(coef_table),
    estimate = as.numeric(coef_table[, "Estimate"]),
    std_error = as.numeric(coef_table[, "Std. Error"]),
    statistic = if (!is.na(stat_col)) as.numeric(coef_table[, stat_col]) else NA_real_,
    p_value = if (!is.na(p_col)) as.numeric(coef_table[, p_col]) else NA_real_
  )

  conf_df <- tibble::tibble(
    term = rownames(conf_int),
    conf_low = as.numeric(conf_int[, 1]),
    conf_high = as.numeric(conf_int[, 2])
  )

  coef_df %>%
    dplyr::left_join(conf_df, by = "term") %>%
    dplyr::mutate(
      event_time = as.integer(stringr::str_match(term, "relative_year::(-?[0-9]+):")[, 2]),
      treatment_group = dplyr::case_when(
        treatment_spec_i == "continuous" ~ "Strictness change",
        stringr::str_detect(term, ":to_lenient$") ~ "To more lenient",
        stringr::str_detect(term, ":to_stricter$") ~ "To stricter",
        TRUE ~ term
      ),
      display_unit = dplyr::case_when(
        stringr::str_starts(outcome_i, "log_") | outcome_i == "mean_log_land_psf" ~ "100_log_points",
        TRUE ~ "percentage_points"
      ),
      estimate_display = 100 * estimate,
      ci_low_display = 100 * conf_low,
      ci_high_display = 100 * conf_high
    ) %>%
    dplyr::filter(!is.na(event_time)) %>%
    dplyr::arrange(treatment_group, event_time)
}

compute_pretrend <- function(model, coefficients_df) {
  if (is.null(model) || nrow(coefficients_df) == 0) {
    return(tibble::tibble())
  }

  coefficients_df %>%
    dplyr::filter(event_time < -1) %>%
    dplyr::group_by(treatment_group) %>%
    dplyr::summarise(
      n_leads = dplyr::n(),
      min_lead = min(event_time),
      max_lead = max(event_time),
      terms = list(term),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      wald_result = list(tryCatch(fixest::wald(model, unlist(terms)), error = function(e) NULL)),
      wald_stat = if (is.null(wald_result)) NA_real_ else wald_result$stat,
      p_value = if (is.null(wald_result)) NA_real_ else wald_result$p,
      df1 = if (is.null(wald_result)) NA_real_ else wald_result$df1,
      df2 = if (is.null(wald_result)) NA_real_ else wald_result$df2
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(treatment_group, n_leads, min_lead, max_lead, wald_stat, p_value, df1, df2)
}

message("\n=== Block Land-Value Panel Event Model ===")
message(sprintf("Sample: %s", sample_scope))
message(sprintf("Bandwidth: %s", bandwidth))
message(sprintf("Year scope: %s", year_scope))
message(sprintf("Event year: %d", event_year))
message(sprintf("Outcome: %s", outcome))
message(sprintf("Treatment spec: %s", treatment_spec))
message(sprintf("Year window: %s", year_window))
message(sprintf("FE spec: %s", fe_spec))
message(sprintf("Control spec: %s", control_spec))

analysis_years <- switch(
  year_window,
  core = c(2010L, 2011L, 2012L, 2014L, 2016L, 2017L, 2018L),
  long_pre = c(2002L, 2003L, 2004L, 2006L, 2007L, 2008L, 2009L, 2010L, 2011L, 2012L, 2014L, 2016L, 2017L, 2018L)
)

panel <- arrow::read_parquet(in_panel) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    block_origin_side_id = as.character(block_origin_side_id),
    block_border_side_id = as.character(block_border_side_id),
    ward_pair_id = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    tax_year = as.integer(tax_year),
    switched_2015 = as.logical(switched_2015),
    valid_control_2015 = as.logical(valid_control_2015),
    comparison_sample = as.logical(comparison_sample),
    treatment_sign = factor(treatment_sign, levels = c("no_change", "to_lenient", "to_stricter")),
    to_lenient = as.integer(treatment_sign == "to_lenient"),
    to_stricter = as.integer(treatment_sign == "to_stricter"),
    relative_year = tax_year - event_year
  )

analysis_df <- panel %>%
  dplyr::filter(
    sample_scope == !!sample_scope,
    bandwidth == !!bandwidth,
    tax_year %in% analysis_years,
    comparison_sample %in% TRUE,
    coverage_share >= 0.95,
    dplyr::case_when(
      year_scope == "admin_only" ~ admin_share == 1,
      year_scope == "admin_95" ~ admin_share >= 0.95,
      year_scope == "admin_plus_fallback" ~ TRUE,
      TRUE ~ FALSE
    ),
    !is.na(.data[[outcome]]),
    !is.na(strictness_change),
    !is.na(treatment_sign),
    !is.na(block_border_side_id),
    block_border_side_id != "",
    !is.na(ward_pair_id),
    ward_pair_id != "",
    !is.na(segment_id),
    segment_id != "",
    relative_year != 0L | event_year == 2012L
  )

if (control_spec == "baseline_x_year") {
  baseline_controls <- panel %>%
    dplyr::filter(
      sample_scope == !!sample_scope,
      bandwidth == !!bandwidth,
      tax_year == event_year - 1L,
      comparison_sample %in% TRUE,
      coverage_share >= 0.95,
      dplyr::case_when(
        year_scope == "admin_only" ~ admin_share == 1,
        year_scope == "admin_95" ~ admin_share >= 0.95,
        year_scope == "admin_plus_fallback" ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::transmute(
      block_border_side_id,
      baseline_log_lot_sqft_block = dplyr::if_else(lot_sqft_block > 0, log(lot_sqft_block), NA_real_),
      baseline_mean_dist_to_boundary_ft = mean_dist_to_boundary_ft,
      baseline_n_pin10_max = n_pin10_max
    )

  analysis_df <- analysis_df %>%
    dplyr::left_join(baseline_controls, by = "block_border_side_id") %>%
    dplyr::filter(
      !is.na(baseline_log_lot_sqft_block),
      !is.na(baseline_mean_dist_to_boundary_ft),
      !is.na(baseline_n_pin10_max)
    )
}

fe_group <- dplyr::case_when(
  fe_spec == "segment_year" ~ "segment_id",
  fe_spec == "ward_pair_year" ~ "ward_pair_id",
  TRUE ~ NA_character_
)

fe_group_support <- analysis_df %>%
  dplyr::distinct(.data[[fe_group]], block_border_side_id, treatment_sign, strictness_change) %>%
  dplyr::group_by(.data[[fe_group]]) %>%
  dplyr::summarise(
    n_block_sides = dplyr::n_distinct(block_border_side_id),
    n_treatment_signs = dplyr::n_distinct(treatment_sign),
    n_strictness_values = dplyr::n_distinct(strictness_change),
    has_no_change = any(treatment_sign == "no_change"),
    has_switcher = any(treatment_sign != "no_change"),
    .groups = "drop"
  )

n_identifying_fe_groups <- if (treatment_spec == "continuous") {
  sum(fe_group_support$n_strictness_values > 1)
} else {
  sum(fe_group_support$has_no_change & fe_group_support$has_switcher)
}

model <- NULL
model_status <- "ok"
formula_text <- NA_character_

if (nrow(analysis_df) == 0) {
  model_status <- "empty_sample"
} else if (n_identifying_fe_groups == 0) {
  model_status <- "no_identifying_fe_groups"
} else {
  rhs_text <- if (treatment_spec == "continuous") {
    "i(relative_year, strictness_change, ref = -1)"
  } else {
    paste0(
      "i(relative_year, to_lenient, ref = -1) + ",
      "i(relative_year, to_stricter, ref = -1)"
    )
  }

  if (control_spec == "baseline_x_year") {
    rhs_text <- paste0(
      rhs_text,
      " + i(tax_year, baseline_log_lot_sqft_block)",
      " + i(tax_year, baseline_mean_dist_to_boundary_ft)",
      " + i(tax_year, baseline_n_pin10_max)"
    )
  }

  formula_text <- paste0(outcome, " ~ ", rhs_text, " | block_border_side_id + ", fe_group, "^tax_year")

  model <- tryCatch(
    fixest::feols(
      stats::as.formula(formula_text),
      data = analysis_df,
      cluster = stats::as.formula(paste0("~", fe_group)),
      warn = FALSE
    ),
    error = function(e) {
      model_status <<- paste0("fixest_error: ", conditionMessage(e))
      NULL
    }
  )
}

coefficients <- extract_event_terms(model, treatment_spec, outcome) %>%
  dplyr::mutate(
    sample_scope = sample_scope,
    bandwidth = bandwidth,
    year_scope = year_scope,
    event_year = event_year,
    outcome = outcome,
    treatment_spec = treatment_spec,
    year_window = year_window,
    fe_spec = fe_spec,
    control_spec = control_spec,
    .before = 1
  )

pretrend <- compute_pretrend(model, coefficients) %>%
  dplyr::mutate(
    sample_scope = sample_scope,
    bandwidth = bandwidth,
    year_scope = year_scope,
    event_year = event_year,
    outcome = outcome,
    treatment_spec = treatment_spec,
    year_window = year_window,
    fe_spec = fe_spec,
    control_spec = control_spec,
    .before = 1
  )

support_by_event <- analysis_df %>%
  dplyr::group_by(relative_year, treatment_sign) %>%
  dplyr::summarise(
    n_block_years = dplyr::n(),
    n_block_sides = dplyr::n_distinct(block_border_side_id),
    n_block_origin_sides = dplyr::n_distinct(block_origin_side_id),
    n_census_blocks = dplyr::n_distinct(block_id),
    n_segments = dplyr::n_distinct(segment_id),
    n_pin10 = sum(n_pin10, na.rm = TRUE),
    mean_admin_share = mean(admin_share, na.rm = TRUE),
    mean_coverage_share = mean(coverage_share, na.rm = TRUE),
    .groups = "drop"
  )

metadata <- tibble::tibble(
  sample_scope = sample_scope,
  bandwidth = bandwidth,
  year_scope = year_scope,
  event_year = event_year,
  outcome = outcome,
  treatment_spec = treatment_spec,
  year_window = year_window,
  fe_spec = fe_spec,
  control_spec = control_spec,
  model_status = model_status,
  formula = formula_text,
  cluster = fe_group,
  analysis_years = paste(analysis_years, collapse = "|"),
  n_obs = nrow(analysis_df),
  n_block_sides = dplyr::n_distinct(analysis_df$block_border_side_id),
  n_block_origin_sides = dplyr::n_distinct(analysis_df$block_origin_side_id),
  n_census_blocks = dplyr::n_distinct(analysis_df$block_id),
  n_ward_pairs = dplyr::n_distinct(analysis_df$ward_pair_id),
  n_segments = dplyr::n_distinct(analysis_df$segment_id),
  n_switched_block_sides = dplyr::n_distinct(analysis_df$block_border_side_id[analysis_df$switched_2015 %in% TRUE]),
  n_valid_control_block_sides = dplyr::n_distinct(analysis_df$block_border_side_id[analysis_df$valid_control_2015 %in% TRUE]),
  n_identifying_fe_groups = n_identifying_fe_groups,
  mean_admin_share = mean(analysis_df$admin_share, na.rm = TRUE),
  mean_coverage_share = mean(analysis_df$coverage_share, na.rm = TRUE)
) %>%
  dplyr::mutate(
    support_by_event = paste(
      paste(
        support_by_event$relative_year,
        support_by_event$treatment_sign,
        support_by_event$n_block_sides,
        sep = ":"
      ),
      collapse = "|"
    )
  )

readr::write_csv(coefficients, out_coefficients)
readr::write_csv(pretrend, out_pretrend)
readr::write_csv(metadata, out_metadata)

message(sprintf("Saved %s", out_coefficients))
message(sprintf("Saved %s", out_pretrend))
message(sprintf("Saved %s", out_metadata))
