# run_event_study.R
# Transaction-level event study for home sales with hedonic controls
# Hedonic controls use actual values (no imputation) - observations with missing hedonics are dropped

source("../../setup_environment/code/packages.R")

# =============================================================================
# COMMAND LINE ARGUMENTS
# =============================================================================
# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_disaggregate/code")
# treatment_type <- "continuous"
# include_hedonics <- TRUE
# time_unit <- "yearly"
# fe_type <- "strict_pair_x_year"
# weighting <- "uniform"
# bandwidth <- 1000
# stacked <- TRUE
# stack_type <- "implementation"
# cohort <- "2015"
# post_window <- "full"
# geo_fe_level <- "segment"
# cluster_level <- "twoway_block_segment"
# Rscript run_event_study.R "continuous" TRUE "yearly" "strict_pair_x_year" "uniform" 1000 TRUE "implementation" "2015" "full" "segment" "twoway_block_segment"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 12) {
  treatment_type <- cli_args[1]
  include_hedonics <- tolower(cli_args[2]) %in% c("true", "t", "1", "yes")
  time_unit <- cli_args[3]
  fe_type <- cli_args[4]
  weighting <- cli_args[5]
  bandwidth <- as.numeric(cli_args[6])
  stacked <- tolower(cli_args[7]) %in% c("true", "t", "1", "yes")
  stack_type <- cli_args[8]
  cohort <- cli_args[9]
  post_window <- cli_args[10]
  geo_fe_level <- tolower(cli_args[11])
  cluster_level <- tolower(cli_args[12])
} else if (length(cli_args) >= 10) {
  treatment_type <- cli_args[1]
  include_hedonics <- tolower(cli_args[2]) %in% c("true", "t", "1", "yes")
  time_unit <- cli_args[3]
  fe_type <- cli_args[4]
  weighting <- cli_args[5]
  bandwidth <- as.numeric(cli_args[6])
  stacked <- tolower(cli_args[7]) %in% c("true", "t", "1", "yes")
  stack_type <- cli_args[8]
  cohort <- cli_args[9]
  post_window <- cli_args[10]
  geo_fe_level <- tolower(Sys.getenv("GEO_FE_LEVEL", "segment"))
  cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "twoway_block_segment"))
} else {
  if (!exists("treatment_type") || !exists("include_hedonics") || !exists("time_unit") || !exists("fe_type") || !exists("weighting") || !exists("bandwidth") || !exists("stacked") || !exists("stack_type") || !exists("cohort") || !exists("post_window") || !exists("geo_fe_level") || !exists("cluster_level")) {
    stop("FATAL: Script requires args: <treatment_type> <include_hedonics> <time_unit> <fe_type> <weighting> <bandwidth> <stacked> <stack_type> <cohort> <post_window> [<geo_fe_level> <cluster_level>]", call. = FALSE)
  }
}

TREATMENT_TYPE <- treatment_type
INCLUDE_HEDONICS <- include_hedonics
TIME_UNIT <- time_unit
FE_TYPE <- fe_type
WEIGHTING <- weighting
BANDWIDTH <- bandwidth
STACKED <- stacked
STACK_TYPE <- stack_type
COHORT <- cohort
POST_WINDOW <- post_window
GEO_FE_LEVEL <- geo_fe_level
CLUSTER_LEVEL <- cluster_level

message("\n=== Disaggregate Sales Event Study ===")
message(sprintf("Stacked: %s", STACKED))
if (STACKED) {
  message(sprintf("Stack Type: %s", STACK_TYPE))
} else {
  message(sprintf("Cohort: %s", COHORT))
}
message(sprintf("Treatment Type: %s", TREATMENT_TYPE))
message(sprintf("Include Hedonics: %s", INCLUDE_HEDONICS))
message(sprintf("Time Unit: %s", TIME_UNIT))
message(sprintf("FE Type: %s", FE_TYPE))
message(sprintf("Weighting: %s", WEIGHTING))
message(sprintf("Bandwidth: %d ft", BANDWIDTH))
message(sprintf("Post Window: %s", POST_WINDOW))
message(sprintf("Geo FE Level: %s", GEO_FE_LEVEL))
message(sprintf("Cluster Level: %s", CLUSTER_LEVEL))

valid_fe_types <- c(
  "ward_pair_side", "block", "block_group",
  "strict_pair_x_year", "pair_trend_plus_year", "side_plus_year"
)
if (!FE_TYPE %in% valid_fe_types) {
  stop(sprintf(
    "--fe_type must be one of: %s",
    paste(valid_fe_types, collapse = ", ")
  ))
}
if (!GEO_FE_LEVEL %in% c("segment", "ward_pair")) {
  stop("--geo_fe_level must be one of: segment, ward_pair", call. = FALSE)
}
if (!CLUSTER_LEVEL %in% c("twoway_block_segment", "block", "segment")) {
  stop("--cluster_level must be one of: twoway_block_segment, block, segment", call. = FALSE)
}
if (GEO_FE_LEVEL == "segment" && BANDWIDTH > 1000) {
  stop("Segment FE requested with bandwidth > 1000. Use bandwidth <= 1000.", call. = FALSE)
}

use_new_fe_spec <- FE_TYPE %in% c("strict_pair_x_year", "pair_trend_plus_year", "side_plus_year")

# Output suffix
fe_suffix <- switch(FE_TYPE,
  "ward_pair_side" = "",
  "block" = "_block_fe",
  "block_group" = "_bg_fe",
  "strict_pair_x_year" = "",
  "pair_trend_plus_year" = "_pairtrend",
  "side_plus_year" = "_yearfe"
)

# Stack suffix depends on mode
if (STACKED) {
  stack_suffix <- sprintf("_stacked_%s", STACK_TYPE)
} else {
  stack_suffix <- sprintf("_unstacked_%s", COHORT)
}

suffix <- sprintf(
  "disaggregate_%s%s_%s_%s_%dft%s%s%s",
  TIME_UNIT,
  stack_suffix,
  TREATMENT_TYPE,
  WEIGHTING,
  as.integer(BANDWIDTH),
  fe_suffix,
  ifelse(INCLUDE_HEDONICS, "", "_no_hedonics"),
  ifelse(POST_WINDOW == "short", "_short", "")
)
if (GEO_FE_LEVEL != "segment") {
  suffix <- paste0(suffix, "_geo_wardpair")
}
if (CLUSTER_LEVEL == "block") {
  suffix <- paste0(suffix, "_clust_block")
} else if (CLUSTER_LEVEL == "segment") {
  suffix <- paste0(suffix, "_clust_segment")
}

# =============================================================================
# LOAD DATA
# =============================================================================
message("\nLoading transaction panel...")

if (STACKED) {
  # Choose stacked panel based on stack_type
  input_file <- switch(STACK_TYPE,
    "announcement" = "../input/sales_transaction_panel_announcement.parquet",
    "implementation" = "../input/sales_transaction_panel.parquet",
    stop(sprintf("Unknown stack_type: %s. Must be 'announcement' or 'implementation'.", STACK_TYPE))
  )

  message(sprintf("Loading stacked panel (%s): %s", STACK_TYPE, input_file))
  data <- read_parquet(input_file)
  setDT(data)

  # Report which cohorts are included
  cohorts_in_data <- unique(data$cohort)
  message(sprintf("Cohorts in data: %s", paste(cohorts_in_data, collapse = " + ")))
} else {
  # Unstacked: load cohort-specific panel
  input_file <- switch(COHORT,
    "2012" = "../input/sales_transaction_panel_2012.parquet",
    "2015" = "../input/sales_transaction_panel_2015.parquet",
    "2022" = "../input/sales_transaction_panel_2022.parquet",
    "2023" = "../input/sales_transaction_panel_2023.parquet",
    stop(sprintf("Unknown cohort: %s. Must be 2012, 2015, 2022, or 2023.", COHORT))
  )

  message(sprintf("Loading %s cohort panel: %s", COHORT, input_file))
  data <- read_parquet(input_file)
  setDT(data)
}

message(sprintf("Loaded %s transactions", format(nrow(data), big.mark = ",")))

needs_segment <- GEO_FE_LEVEL == "segment" || CLUSTER_LEVEL %in% c("segment", "twoway_block_segment")
if (needs_segment) {
  required_segment_cols <- if (STACKED) {
    c("segment_id_cohort", "segment_side", "cohort_segment", "cohort_segment_side")
  } else {
    c("segment_id_cohort", "segment_side")
  }
  missing_cols <- setdiff(required_segment_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required segment columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  data <- data[!is.na(segment_id_cohort) & segment_id_cohort != ""]
}

# Create treatment indicators for binary analysis
data[, `:=`(
  treatment_continuous = strictness_change,
  treat_stricter = as.integer(strictness_change > 0),
  treat_lenient = as.integer(strictness_change < 0),
  treatment_stricter_continuous = pmax(strictness_change, 0),
  treatment_lenient_continuous = pmax(-strictness_change, 0)
)]

# =============================================================================
# APPLY BANDWIDTH AND CONSTRUCT WEIGHTS
# =============================================================================
message(sprintf("\nApplying bandwidth filter: %d ft", BANDWIDTH))
message(sprintf("Observations before filter: %s", format(nrow(data), big.mark = ",")))

data <- data[dist_ft <= BANDWIDTH]
data[, weight := if (WEIGHTING == "triangular") pmax(0, 1 - dist_ft / BANDWIDTH) else 1]

message(sprintf("Observations after filter: %s", format(nrow(data), big.mark = ",")))

# =============================================================================
# RESTRICT TO COMPLETE HEDONIC SAMPLE (for comparability across specs)
# =============================================================================
hedonic_vars_list <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")

n_before <- nrow(data)
data <- data[complete.cases(data[, ..hedonic_vars_list])]
n_after <- nrow(data)

message(sprintf(
  "Restricted to complete hedonic sample: %s -> %s transactions (dropped %s)",
  format(n_before, big.mark = ","),
  format(n_after, big.mark = ","),
  format(n_before - n_after, big.mark = ",")
))

# Weighting diagnostics
message("\n=== WEIGHTING DIAGNOSTICS ===")
message(sprintf("Sum of weights (effective N): %.0f", sum(data$weight)))
message(sprintf("Efficiency ratio: %.1f%%", 100 * sum(data$weight) / nrow(data)))

message("\nWeight distribution by treatment status:")
data[, .(
  n = .N,
  sum_weights = sum(weight),
  mean_weight = mean(weight),
  mean_dist_ft = mean(dist_ft)
), by = treat] %>% print()

message("\nWeight distribution by distance bins:")
bin_width <- min(250, BANDWIDTH / 4)
data[, dist_bin := cut(dist_ft, breaks = seq(0, BANDWIDTH, by = bin_width), include.lowest = TRUE)]
data[, .(n = .N, mean_weight = mean(weight)), by = dist_bin][order(dist_bin)] %>% print()
data[, dist_bin := NULL]

# =============================================================================
# REPORT HEDONIC COVERAGE
# =============================================================================
message("\n=== HEDONIC VARIABLE COVERAGE ===")
message(sprintf("log_sqft: %.1f%%", 100 * mean(!is.na(data$log_sqft))))
message(sprintf("log_land_sqft: %.1f%%", 100 * mean(!is.na(data$log_land_sqft))))
message(sprintf("log_building_age: %.1f%%", 100 * mean(!is.na(data$log_building_age))))
message(sprintf("log_bedrooms: %.1f%%", 100 * mean(!is.na(data$log_bedrooms))))
message(sprintf("log_baths: %.1f%%", 100 * mean(!is.na(data$log_baths))))
message(sprintf("has_garage: %.1f%%", 100 * mean(!is.na(data$has_garage))))

# Complete hedonic cases
n_total <- nrow(data)
n_complete <- data[!is.na(log_sqft) & !is.na(log_land_sqft) & !is.na(log_building_age) &
  !is.na(log_bedrooms) & !is.na(log_baths) & !is.na(has_garage), .N]
message(sprintf(
  "\nComplete hedonic cases: %s of %s (%.1f%%)",
  format(n_complete, big.mark = ","),
  format(n_total, big.mark = ","),
  100 * n_complete / n_total
))

if (INCLUDE_HEDONICS) {
  message("\nNOTE: Observations with missing hedonics will be dropped from regression")
}

# =============================================================================
# CREATE TIME VARIABLES BASED ON TIME_UNIT
# =============================================================================
if (TIME_UNIT == "quarterly") {
  message("\nCreating quarterly relative periods...")

  data[, `:=`(
    sale_quarter = quarter(sale_date),
    sale_yearqtr = year(sale_date) + (quarter(sale_date) - 1) / 4
  )]

  if (STACKED) {
    data[, cohort_yearqtr := as.numeric(cohort)]
    data[, relative_qtr := round((sale_yearqtr - cohort_yearqtr) * 4)]
  } else {
    # For 2015 cohort only, hardcode the reference year
    data[, relative_qtr := round((sale_yearqtr - 2015) * 4)]
  }
  data[, relative_qtr_capped := pmax(pmin(relative_qtr, 12), -12)]

  data[, relative_period := relative_qtr_capped]
  time_var <- "sale_yearqtr"
  
  # Set window based on post_window argument
  if (POST_WINDOW == "short") {
    min_period <- -20
    max_period <- 8
    x_breaks <- seq(-20, 8, by = 4)
  } else {
    min_period <- -12
    max_period <- 12
    x_breaks <- seq(-12, 12, by = 2)
  }

  message(sprintf(
    "Relative quarter range: %d to %d",
    min(data$relative_qtr, na.rm = TRUE),
    max(data$relative_qtr, na.rm = TRUE)
  ))
} else {
  data[, relative_period := relative_year_capped]
  time_var <- "sale_year"
  
  # Set window based on post_window argument
  if (POST_WINDOW == "short") {
    min_period <- -5
    max_period <- 2
    x_breaks <- -5:2
  } else {
    min_period <- -5
    max_period <- 5
    x_breaks <- -5:5
  }
}

message(sprintf("Post-period window: [%d, %d]", min_period, max_period))

# =============================================================================
# SET X-AXIS LABEL BASED ON TIMING
# =============================================================================
# Announcement timing: 2012, 2022, or stacked_announcement
# Implementation timing: 2015, 2023, or stacked_implementation

if (STACKED) {
  is_announcement_timing <- (STACK_TYPE == "announcement")
} else {
  is_announcement_timing <- (COHORT %in% c("2012", "2022"))
}

if (TIME_UNIT == "quarterly") {
  if (is_announcement_timing) {
    x_label <- "Quarters Relative to Redistricting Announcement"
  } else {
    x_label <- "Quarters Relative to Redistricting"
  }
} else {
  if (is_announcement_timing) {
    x_label <- "Years Relative to Redistricting Announcement"
  } else {
    x_label <- "Years Relative to Redistricting"
  }
}

message(sprintf("X-axis label: %s", x_label))

# =============================================================================
# BUILD FE / CLUSTER STRUCTURE
# =============================================================================
trend_var <- "sale_year"

if (STACKED) {
  data[, ward_pair_side_temp := sub("^[0-9]+_", "", cohort_ward_pair_side)]
  data[, ward_pair := sub("_[0-9]+$", "", ward_pair_side_temp)]
  data[, cohort_ward_pair := paste(cohort, ward_pair, sep = "_")]

  if (GEO_FE_LEVEL == "segment") {
    fe_side_var <- "cohort_segment_side"
    fe_group_var <- "cohort_segment"
  } else {
    fe_side_var <- "cohort_ward_pair_side"
    fe_group_var <- "cohort_ward_pair"
  }

  if (FE_TYPE == "block_group") {
    data[, block_group_id := substr(block_id, 1, 12)]
    data[, cohort_block_group_id := paste(cohort, block_group_id, sep = "_")]
    unit_fe <- "cohort_block_group_id"
  } else if (FE_TYPE == "block") {
    unit_fe <- "cohort_block_id"
  } else {
    unit_fe <- fe_side_var
  }

  block_cluster_var <- "cohort_block_id"
  segment_cluster_var <- "cohort_segment"
} else {
  data[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]

  if (GEO_FE_LEVEL == "segment") {
    fe_side_var <- "segment_side"
    fe_group_var <- "segment_id_cohort"
  } else {
    fe_side_var <- "ward_pair_side"
    fe_group_var <- "ward_pair"
  }

  if (FE_TYPE == "block_group") {
    data[, block_group_id := substr(block_id, 1, 12)]
    unit_fe <- "block_group_id"
  } else if (FE_TYPE == "block") {
    unit_fe <- "block_id"
  } else {
    unit_fe <- fe_side_var
  }

  block_cluster_var <- "block_id"
  segment_cluster_var <- "segment_id_cohort"
}

if (GEO_FE_LEVEL == "segment" || CLUSTER_LEVEL %in% c("segment", "twoway_block_segment")) {
  data <- data[!is.na(get(segment_cluster_var)) & get(segment_cluster_var) != ""]
}
if (GEO_FE_LEVEL == "segment") {
  data <- data[!is.na(get(fe_side_var)) & get(fe_side_var) != ""]
}

if (use_new_fe_spec) {
  if (STACKED) {
    fe_formula <- switch(FE_TYPE,
      "strict_pair_x_year" = sprintf("%s + %s^%s", fe_side_var, fe_group_var, time_var),
      "pair_trend_plus_year" = sprintf("%s + cohort^%s + %s[%s]", fe_side_var, time_var, fe_group_var, trend_var),
      "side_plus_year" = sprintf("%s + cohort^%s", fe_side_var, time_var)
    )
  } else {
    fe_formula <- switch(FE_TYPE,
      "strict_pair_x_year" = sprintf("%s + %s^%s", fe_side_var, fe_group_var, time_var),
      "pair_trend_plus_year" = sprintf("%s + %s + %s[%s]", fe_side_var, time_var, fe_group_var, trend_var),
      "side_plus_year" = sprintf("%s + %s", fe_side_var, time_var)
    )
  }
} else {
  fe_formula <- sprintf("%s + %s^%s", unit_fe, fe_group_var, time_var)
}

if (CLUSTER_LEVEL == "twoway_block_segment") {
  cluster_formula <- as.formula(sprintf("~%s + %s", block_cluster_var, segment_cluster_var))
} else if (CLUSTER_LEVEL == "segment") {
  cluster_formula <- as.formula(sprintf("~%s", segment_cluster_var))
} else {
  cluster_formula <- as.formula(sprintf("~%s", block_cluster_var))
}

message(sprintf("FE formula: %s", fe_formula))
message(sprintf("Cluster formula: %s", paste(deparse(cluster_formula), collapse = "")))

# =============================================================================
# EFFECTIVE OBSERVATIONS DIAGNOSTIC
# =============================================================================
message("\n=== EFFECTIVE OBSERVATIONS DIAGNOSTIC ===")
data[, is_switcher := abs(strictness_change) > 0]

pair_summary <- data[, .(
  n_sides = uniqueN(get(fe_side_var)),
  n_obs = .N,
  n_switcher = sum(is_switcher),
  n_stayer = sum(!is_switcher)
), by = c(fe_group_var)]

identifying_pairs <- pair_summary[n_sides == 2]
effective_obs <- data[get(fe_group_var) %in% identifying_pairs[[fe_group_var]], .N]

message(sprintf("Total %s groups: %d", fe_group_var, nrow(pair_summary)))
message(sprintf("Pairs with observations on BOTH sides: %d (%.1f%%)",
  nrow(identifying_pairs), 100 * nrow(identifying_pairs) / nrow(pair_summary)))
message(sprintf("Effective observations (in identifying groups): %s of %s (%.1f%%)",
  format(effective_obs, big.mark = ","),
  format(nrow(data), big.mark = ","),
  100 * effective_obs / nrow(data)))
message(sprintf("Transactions in switcher blocks: %s", format(sum(data$is_switcher), big.mark = ",")))
message(sprintf("Transactions in stayer blocks: %s", format(sum(!data$is_switcher), big.mark = ",")))


# =============================================================================
# SPECIFY HEDONIC CONTROLS (NO IMPUTATION - NAs will be dropped)
# =============================================================================
if (INCLUDE_HEDONICS) {
  hedonic_formula <- "+ log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage"
  message("\nHedonic controls: log_sqft, log_land_sqft, log_building_age, log_bedrooms, log_baths, has_garage")
} else {
  hedonic_formula <- ""
  message("\nHedonic controls: OFF")
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================
extract_iplot_data <- function(model, group_label) {
  iplot_data <- tryCatch(iplot(model, .plot = FALSE)[[1]], error = function(e) NULL)
  if (is.null(iplot_data) || nrow(iplot_data) == 0) {
    return(NULL)
  }
  iplot_data %>%
    as_tibble() %>%
    filter(x >= min_period & x <= max_period) %>%  # Apply window filtering
    mutate(
      group = group_label,
      estimate_pct = estimate * 100,
      ci_low_pct = ci_low * 100,
      ci_high_pct = ci_high * 100
    )
}

make_event_study_plot <- function(plot_data, title = NULL) {
  p <- ggplot(plot_data, aes(x = x, y = estimate_pct)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.2, fill = "#009E73", color = NA) +
    geom_line(color = "#009E73", linewidth = 1) +
    geom_point(size = 2.5, color = "#009E73") +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      x = x_label,
      y = "Effect on Home Prices",
      title = title
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      axis.line = element_line(color = "gray40", linewidth = 0.3),
      axis.ticks = element_line(color = "gray40", linewidth = 0.3),
      axis.title = element_text(size = 10, color = "gray20"),
      axis.text = element_text(size = 9, color = "gray30"),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )
  return(p)
}

# =============================================================================
# RUN REGRESSIONS
# =============================================================================

if (TREATMENT_TYPE == "continuous") {
  message("\n=== Continuous Treatment ===")

  formula_str <- sprintf(
    "log(sale_price) ~ i(relative_period, treatment_continuous, ref = -1) %s | %s",
    hedonic_formula, fe_formula
  )
  message(sprintf("Formula: %s", formula_str))

  message("\nEstimating model...")
  t0 <- Sys.time()

  m <- feols(
    as.formula(formula_str),
    data = data,
    weights = ~weight,
    cluster = cluster_formula
  )

  t1 <- Sys.time()
  message(sprintf("Estimation time: %.1f seconds", difftime(t1, t0, units = "secs")))
  message(sprintf("Observations used: %s", format(m$nobs, big.mark = ",")))

  message("\n--- Regression Summary ---")
  print(summary(m))

  # Extract and plot
  plot_data <- extract_iplot_data(m, "Continuous Treatment")

  if (!is.null(plot_data) && nrow(plot_data) > 0) {
    p <- make_event_study_plot(plot_data)

    outfile <- sprintf("../output/event_study_%s.pdf", suffix)
    ggsave(outfile, p, width = 7, height = 4.5, bg = "white")
    message(sprintf("\nSaved: %s", outfile))
  }

  # Save regression table
  # etable(list(m),
  #   fitstat = ~ n + r2,
  #   style.tex = style.tex("aer",
  #     model.format = "", fixef.title = "", fixef.suffix = "",
  #     yesNo = c("$\\checkmark$", "")
  #   ),
  #   depvar = FALSE,
  #   digits = 3,
  #   headers = c("Continuous"),
  #   signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  #   notes = sprintf(
  #     "Transaction-level regression. %s weighting with %dft bandwidth. Hedonics in logs, no imputation.",
  #     tools::toTitleCase(WEIGHTING), as.integer(BANDWIDTH)
  #   ),
  #   float = FALSE,
  #   file = sprintf("../output/did_table_%s.tex", suffix),
  #   replace = TRUE
  # )
  # message(sprintf("Saved: ../output/did_table_%s.tex", suffix))
} else if (TREATMENT_TYPE == "binary_direction") {
  message("\n=== Binary Direction Treatment ===")

  # --- Moved to Stricter ---
  message("\n--- Estimating: Moved to Stricter ---")

  data_stricter <- data[treat_lenient == 0]
  message(sprintf("Stricter sample: %s transactions", format(nrow(data_stricter), big.mark = ",")))

  formula_stricter <- sprintf(
    "log(sale_price) ~ i(relative_period, treat_stricter, ref = -1) %s | %s",
    hedonic_formula, fe_formula
  )

  t0 <- Sys.time()
  m_stricter <- feols(as.formula(formula_stricter),
    data = data_stricter,
    weights = ~weight,
    cluster = cluster_formula
  )
  t1 <- Sys.time()
  message(sprintf("Estimation time: %.1f seconds", difftime(t1, t0, units = "secs")))
  message(sprintf("Observations used: %s", format(m_stricter$nobs, big.mark = ",")))

  print(summary(m_stricter))

  # --- Moved to More Lenient ---
  message("\n--- Estimating: Moved to More Lenient ---")

  data_lenient <- data[treat_stricter == 0]
  message(sprintf("Lenient sample: %s transactions", format(nrow(data_lenient), big.mark = ",")))

  formula_lenient <- sprintf(
    "log(sale_price) ~ i(relative_period, treat_lenient, ref = -1) %s | %s",
    hedonic_formula, fe_formula
  )

  t0 <- Sys.time()
  m_lenient <- feols(as.formula(formula_lenient),
    data = data_lenient,
    weights = ~weight,
    cluster = cluster_formula
  )
  t1 <- Sys.time()
  message(sprintf("Estimation time: %.1f seconds", difftime(t1, t0, units = "secs")))
  message(sprintf("Observations used: %s", format(m_lenient$nobs, big.mark = ",")))

  print(summary(m_lenient))

  # --- Combined Plot ---
  plot_data <- bind_rows(
    extract_iplot_data(m_stricter, "Moved to Stricter"),
    extract_iplot_data(m_lenient, "Moved to More Lenient")
  ) %>% filter(!is.na(estimate))

  if (nrow(plot_data) > 0) {
    # Faceted plot
    p_facet <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
      geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
      geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5, shape = 21, stroke = 0.5) +
      scale_color_manual(
        values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
        name = NULL
      ) +
      scale_fill_manual(
        values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
        name = NULL
      ) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      facet_wrap(~group, ncol = 1) +
      labs(
        x = x_label,
        y = "Effect on Home Prices"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        axis.line = element_line(color = "gray40", linewidth = 0.3),
        axis.ticks = element_line(color = "gray40", linewidth = 0.3),
        axis.title = element_text(size = 10, color = "gray20"),
        axis.text = element_text(size = 9, color = "gray30"),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 10),
        plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
      )

    outfile <- sprintf("../output/event_study_%s.pdf", suffix)
    ggsave(outfile, p_facet, width = 7, height = 6, bg = "white")
    message(sprintf("\nSaved: %s", outfile))

    # Combined (overlaid) plot
    p_combined <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
      geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
      geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5, shape = 21, stroke = 0.5) +
      scale_color_manual(
        values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
        labels = c(
          "Moved to Stricter" = "Moved to Stricter Alderman",
          "Moved to More Lenient" = "Moved to More Lenient Alderman"
        ),
        name = NULL
      ) +
      scale_fill_manual(
        values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
        labels = c(
          "Moved to Stricter" = "Moved to Stricter Alderman",
          "Moved to More Lenient" = "Moved to More Lenient Alderman"
        ),
        name = NULL
      ) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        x = x_label,
        y = "Effect on Home Prices"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        axis.line = element_line(color = "gray40", linewidth = 0.3),
        axis.ticks = element_line(color = "gray40", linewidth = 0.3),
        axis.title = element_text(size = 10, color = "gray20"),
        axis.text = element_text(size = 9, color = "gray30"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
      )

    outfile_combined <- sprintf("../output/event_study_combined_%s.pdf", suffix)
    ggsave(outfile_combined, p_combined, width = 7, height = 4.5, bg = "white")
    message(sprintf("Saved: %s", outfile_combined))
  }

  # Save regression table
  # etable(list(m_stricter, m_lenient),
  #   fitstat = ~ n + r2,
  #   style.tex = style.tex("aer",
  #     model.format = "", fixef.title = "", fixef.suffix = "",
  #     yesNo = c("$\\checkmark$", "")
  #   ),
  #   depvar = FALSE,
  #   digits = 3,
  #   headers = c("To Stricter", "To More Lenient"),
  #   signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  #   notes = sprintf(
  #     "Transaction-level regression. %s weighting with %dft bandwidth. Hedonics in logs, no imputation.",
  #     tools::toTitleCase(WEIGHTING), as.integer(BANDWIDTH)
  #   ),
  #   float = FALSE,
  #   file = sprintf("../output/did_table_%s.tex", suffix),
  #   replace = TRUE
  # )
  # message(sprintf("Saved: ../output/did_table_%s.tex", suffix))
} else if (TREATMENT_TYPE == "continuous_split") {
  message("\n=== Continuous Split Treatment ===")
  message("Running two regressions on FULL sample with continuous treatment intensity")
  message(sprintf("Full sample size: %s observations", format(nrow(data), big.mark = ",")))
  message(sprintf("  - Stricter-movers (Δ > 0): %s", format(sum(data$strictness_change > 0), big.mark = ",")))
  message(sprintf("  - Lenient-movers (Δ < 0): %s", format(sum(data$strictness_change < 0), big.mark = ",")))
  message(sprintf("  - Controls (Δ = 0): %s", format(sum(data$strictness_change == 0), big.mark = ",")))

  # Stricter regression: full sample, continuous treatment
  message("\n--- Moved to Stricter (Continuous) ---")
  message(sprintf("Sample size: %s transactions", format(nrow(data), big.mark = ",")))

  formula_stricter <- sprintf(
    "log(sale_price) ~ i(relative_period, treatment_stricter_continuous, ref = -1) %s | %s",
    hedonic_formula, fe_formula
  )

  t0 <- Sys.time()
  m_stricter <- feols(as.formula(formula_stricter),
    data = data,  # full sample
    weights = ~weight,
    cluster = cluster_formula
  )
  t1 <- Sys.time()
  message(sprintf("Estimation time: %.1f seconds", difftime(t1, t0, units = "secs")))
  message(sprintf("Observations used: %s", format(m_stricter$nobs, big.mark = ",")))
  print(summary(m_stricter))

  # Lenient regression: full sample, continuous treatment
  message("\n--- Moved to More Lenient (Continuous) ---")

  formula_lenient <- sprintf(
    "log(sale_price) ~ i(relative_period, treatment_lenient_continuous, ref = -1) %s | %s",
    hedonic_formula, fe_formula
  )

  t0 <- Sys.time()
  m_lenient <- feols(as.formula(formula_lenient),
    data = data,  # full sample
    weights = ~weight,
    cluster = cluster_formula
  )
  t1 <- Sys.time()
  message(sprintf("Estimation time: %.1f seconds", difftime(t1, t0, units = "secs")))
  message(sprintf("Observations used: %s", format(m_lenient$nobs, big.mark = ",")))
  print(summary(m_lenient))

  # Extract plot data
  plot_data <- bind_rows(
    extract_iplot_data(m_stricter, "Moved to Stricter (Continuous)"),
    extract_iplot_data(m_lenient, "Moved to More Lenient (Continuous)")
  ) %>% filter(!is.na(estimate))

  if (nrow(plot_data) > 0) {
    # Faceted plot
    p_facet <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
      geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
      geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5, shape = 21, stroke = 0.5) +
      scale_color_manual(
        values = c("Moved to Stricter (Continuous)" = "#c23616", 
                  "Moved to More Lenient (Continuous)" = "#7f8fa6"),
        name = NULL
      ) +
      scale_fill_manual(
        values = c("Moved to Stricter (Continuous)" = "#c23616", 
                  "Moved to More Lenient (Continuous)" = "#7f8fa6"),
        name = NULL
      ) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      facet_wrap(~group, ncol = 1) +
      labs(
        x = x_label,
        y = "Effect on Home Prices"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        axis.line = element_line(color = "gray40", linewidth = 0.3),
        axis.ticks = element_line(color = "gray40", linewidth = 0.3),
        axis.title = element_text(size = 10, color = "gray20"),
        axis.text = element_text(size = 9, color = "gray30"),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 10),
        plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
      )

    outfile <- sprintf("../output/event_study_%s.pdf", suffix)
    ggsave(outfile, p_facet, width = 7, height = 6, bg = "white")
    message(sprintf("\nSaved: %s", outfile))

    # Combined (overlaid) plot
    p_combined <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
      geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
      geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5, shape = 21, stroke = 0.5) +
      scale_color_manual(
        values = c("Moved to Stricter (Continuous)" = "#c23616", 
                  "Moved to More Lenient (Continuous)" = "#7f8fa6"),
        labels = c("Moved to Stricter (Continuous)" = "Moved to Stricter Alderman", 
                  "Moved to More Lenient (Continuous)" = "Moved to More Lenient Alderman"),
        name = NULL
      ) +
      scale_fill_manual(
        values = c("Moved to Stricter (Continuous)" = "#c23616", 
                  "Moved to More Lenient (Continuous)" = "#7f8fa6"),
        labels = c("Moved to Stricter (Continuous)" = "Moved to Stricter Alderman", 
                  "Moved to More Lenient (Continuous)" = "Moved to More Lenient Alderman"),
        name = NULL
      ) +
      scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.02, 0.02))) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        x = x_label,
        y = "Effect on Home Prices"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        axis.line = element_line(color = "gray40", linewidth = 0.3),
        axis.ticks = element_line(color = "gray40", linewidth = 0.3),
        axis.ticks.length = unit(0.15, "cm"),
        axis.title = element_text(size = 10, color = "gray20"),
        axis.text = element_text(size = 9, color = "gray30"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.key.width = unit(1.5, "cm"),
        legend.margin = margin(t = 5, b = 0),
        plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
      )

    outfile_combined <- sprintf("../output/event_study_combined_%s.pdf", suffix)
    ggsave(outfile_combined, p_combined, width = 7, height = 4.5, bg = "white")
    message(sprintf("Saved: %s", outfile_combined))
  }
}

message("\n\nDone!")
