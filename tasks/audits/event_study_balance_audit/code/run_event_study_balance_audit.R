source("../../../setup_environment/code/packages.R")

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/event_study_balance_audit/code")
# source("run_event_study_balance_audit.R")

permit_balance_full_path <- "../output/permit_balance_full.csv"
sales_balance_full_path <- "../output/sales_balance_full.csv"
permit_balance_summary_path <- "../output/permit_balance_summary.csv"
sales_balance_summary_path <- "../output/sales_balance_summary.csv"
permit_balance_flags_path <- "../output/permit_balance_flags.csv"
sales_balance_flags_path <- "../output/sales_balance_flags.csv"
memo_path <- "../output/event_study_balance_memo.md"

demographic_vars <- c(
  "percent_white",
  "percent_black",
  "percent_hispanic",
  "homeownership_rate",
  "median_rent",
  "median_home_value",
  "median_income",
  "share_bach_plus",
  "avg_household_size",
  "median_age",
  "population_density"
)

permit_vars <- c(
  demographic_vars,
  "dist_ft",
  "pre_mean_n_high_discretion_issue",
  "pre_any_high_discretion_issue",
  "pre_mean_n_low_discretion_nosigns_issue",
  "pre_any_low_discretion_nosigns_issue"
)

sales_vars <- c(
  demographic_vars,
  "dist_ft",
  "pre_mean_log_sale_price",
  "pre_n_transactions",
  "pre_mean_building_sqft",
  "pre_mean_land_sqft",
  "pre_mean_building_age",
  "pre_mean_num_bedrooms",
  "pre_mean_baths_total",
  "pre_mean_has_garage"
)

comparisons <- list(
  list(
    comparison = "treated_vs_control",
    group_a = "treated",
    group_b = "control",
    filter_a = function(df) df$treat == 1,
    filter_b = function(df) df$treat == 0
  ),
  list(
    comparison = "stricter_vs_control",
    group_a = "moved_to_stricter",
    group_b = "control",
    filter_a = function(df) df$strictness_change > 0,
    filter_b = function(df) df$treat == 0
  ),
  list(
    comparison = "lenient_vs_control",
    group_a = "moved_to_more_lenient",
    group_b = "control",
    filter_a = function(df) df$strictness_change < 0,
    filter_b = function(df) df$treat == 0
  ),
  list(
    comparison = "stricter_vs_lenient",
    group_a = "moved_to_stricter",
    group_b = "moved_to_more_lenient",
    filter_a = function(df) df$strictness_change > 0,
    filter_b = function(df) df$strictness_change < 0
  )
)

safe_mean <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  x <- as.numeric(x)
  if (sum(!is.na(x)) <= 1) {
    return(NA_real_)
  }
  stats::sd(x, na.rm = TRUE)
}

safe_ttest_p <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  if (length(x) <= 1 || length(y) <= 1) {
    return(NA_real_)
  }
  out <- tryCatch(
    stats::t.test(x, y)$p.value,
    error = function(e) NA_real_
  )
  out
}

balance_row <- function(branch, comparison_name, group_a_name, group_b_name, variable_name, a_df, b_df) {
  x_a <- as.numeric(a_df[[variable_name]])
  x_b <- as.numeric(b_df[[variable_name]])
  mean_a <- safe_mean(x_a)
  mean_b <- safe_mean(x_b)
  sd_a <- safe_sd(x_a)
  sd_b <- safe_sd(x_b)
  pooled_sd <- sqrt((sd_a^2 + sd_b^2) / 2)
  diff_val <- mean_a - mean_b
  std_diff <- if (is.na(pooled_sd) || pooled_sd == 0) NA_real_ else diff_val / pooled_sd

  tibble(
    branch = branch,
    comparison = comparison_name,
    variable = variable_name,
    group_a = group_a_name,
    group_b = group_b_name,
    mean_a = mean_a,
    mean_b = mean_b,
    sd_a = sd_a,
    sd_b = sd_b,
    diff = diff_val,
    std_diff = std_diff,
    p_value = safe_ttest_p(x_a, x_b),
    n_a = sum(!is.na(x_a)),
    n_b = sum(!is.na(x_b)),
    n_missing_a = sum(is.na(x_a)),
    n_missing_b = sum(is.na(x_b))
  )
}

build_balance_full <- function(units_df, branch_name, variable_names, comparison_defs) {
  bind_rows(lapply(comparison_defs, function(comp) {
    group_a_df <- units_df[comp$filter_a(units_df), , drop = FALSE]
    group_b_df <- units_df[comp$filter_b(units_df), , drop = FALSE]
    bind_rows(lapply(variable_names, function(var_name) {
      balance_row(
        branch = branch_name,
        comparison_name = comp$comparison,
        group_a_name = comp$group_a,
        group_b_name = comp$group_b,
        variable_name = var_name,
        a_df = group_a_df,
        b_df = group_b_df
      )
    }))
  })) %>%
    mutate(
      abs_std_diff = abs(std_diff),
      flag = abs_std_diff >= 0.10 | p_value < 0.10,
      material_imbalance = abs_std_diff >= 0.25
    ) %>%
    arrange(comparison, desc(abs_std_diff), variable)
}

build_balance_summary <- function(units_df, sample_df, branch_name, balance_full_df, comparison_defs) {
  demographic_missing <- function(df) {
    if (nrow(df) == 0) {
      return(0L)
    }
    sum(rowSums(is.na(df[, demographic_vars, drop = FALSE])) > 0)
  }

  bind_rows(lapply(comparison_defs, function(comp) {
    group_a_units <- units_df[comp$filter_a(units_df), , drop = FALSE]
    group_b_units <- units_df[comp$filter_b(units_df), , drop = FALSE]
    group_a_blocks <- unique(group_a_units$block_id)
    group_b_blocks <- unique(group_b_units$block_id)
    group_a_rows <- sample_df %>% filter(block_id %in% group_a_blocks)
    group_b_rows <- sample_df %>% filter(block_id %in% group_b_blocks)
    comparison_rows <- balance_full_df %>% filter(comparison == comp$comparison)

    tibble(
      branch = branch_name,
      comparison = comp$comparison,
      group_a = comp$group_a,
      group_b = comp$group_b,
      n_units_a = nrow(group_a_units),
      n_units_b = nrow(group_b_units),
      n_ward_pairs_a = n_distinct(group_a_rows$ward_pair_id[!is.na(group_a_rows$ward_pair_id) & group_a_rows$ward_pair_id != ""]),
      n_ward_pairs_b = n_distinct(group_b_rows$ward_pair_id[!is.na(group_b_rows$ward_pair_id) & group_b_rows$ward_pair_id != ""]),
      n_missing_demographics_a = demographic_missing(group_a_units),
      n_missing_demographics_b = demographic_missing(group_b_units),
      n_flagged = sum(comparison_rows$flag, na.rm = TRUE),
      n_material = sum(comparison_rows$material_imbalance, na.rm = TRUE),
      n_multi_pair_blocks_a = if ("ward_pair_count" %in% names(group_a_units)) sum(group_a_units$ward_pair_count > 1, na.rm = TRUE) else 0L,
      n_multi_pair_blocks_b = if ("ward_pair_count" %in% names(group_b_units)) sum(group_b_units$ward_pair_count > 1, na.rm = TRUE) else 0L
    )
  }))
}

assert_invariant <- function(units_df, branch_name, variable_names, comparison_defs, balance_full_df) {
  shuffled_df <- units_df %>% slice_sample(prop = 1)
  shuffled_full <- build_balance_full(shuffled_df, branch_name, variable_names, comparison_defs) %>%
    arrange(comparison, variable)
  original_full <- balance_full_df %>%
    arrange(comparison, variable)

  compare_cols <- c(
    "branch", "comparison", "variable", "group_a", "group_b",
    "mean_a", "mean_b", "sd_a", "sd_b", "diff", "std_diff", "p_value",
    "n_a", "n_b", "n_missing_a", "n_missing_b", "abs_std_diff", "flag", "material_imbalance"
  )

  identical_ok <- isTRUE(all.equal(
    original_full[, compare_cols],
    shuffled_full[, compare_cols],
    tolerance = 1e-12,
    check.attributes = FALSE
  ))

  if (!identical_ok) {
    stop(sprintf("Row-order invariance check failed for %s balance table.", branch_name), call. = FALSE)
  }
}

branch_verdict <- function(summary_df) {
  treated_row <- summary_df %>% filter(comparison == "treated_vs_control")
  if (nrow(treated_row) == 0) {
    return("serious_balance_problem")
  }
  if (treated_row$n_material[[1]] > 0 || sum(summary_df$n_material, na.rm = TRUE) >= 2) {
    return("serious_balance_problem")
  }
  if (sum(summary_df$n_flagged, na.rm = TRUE) > 0) {
    return("some_imbalance_but_not_fatal")
  }
  "looks_comparable"
}

worst_five_lines <- function(balance_full_df) {
  worst_rows <- balance_full_df %>%
    filter(!is.na(abs_std_diff)) %>%
    arrange(desc(abs_std_diff), p_value) %>%
    slice_head(n = 5)

  if (nrow(worst_rows) == 0) {
    return("- No auditable balance rows were available.")
  }

  paste0(
    "- ",
    worst_rows$comparison,
    " | ",
    worst_rows$variable,
    " | std diff = ",
    sprintf("%.3f", worst_rows$std_diff),
    " | p = ",
    ifelse(is.na(worst_rows$p_value), "NA", sprintf("%.3f", worst_rows$p_value))
  )
}

controls_raw <- read_csv("../input/block_group_controls.csv", show_col_types = FALSE)
controls_2014 <- controls_raw %>%
  filter(year == 2014) %>%
  group_by(GEOID, year) %>%
  summarise(
    across(all_of(demographic_vars), ~ dplyr::first(.x)),
    control_rows_2014 = n(),
    .groups = "drop"
  ) %>%
  transmute(
    block_group_id = as.character(GEOID),
    control_rows_2014,
    percent_white,
    percent_black,
    percent_hispanic,
    homeownership_rate,
    median_rent,
    median_home_value,
    median_income,
    share_bach_plus,
    avg_household_size,
    median_age,
    population_density
  )

permit_panel <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  as_tibble()

permit_sample <- permit_panel %>%
  filter(!is.na(strictness_change), !is.na(n_high_discretion_issue)) %>%
  filter(dist_ft <= 1000) %>%
  filter(relative_year >= -5, relative_year <= 5) %>%
  filter(!is.na(ward_pair_id), ward_pair_id != "")

permit_constancy <- permit_sample %>%
  group_by(block_id) %>%
  summarise(
    n_treat = n_distinct(treat),
    n_strictness = n_distinct(strictness_change),
    n_dist = n_distinct(dist_ft),
    n_pair = n_distinct(ward_pair_id),
    .groups = "drop"
  )

if (any(permit_constancy$n_treat > 1 | permit_constancy$n_strictness > 1 | permit_constancy$n_dist > 1 | permit_constancy$n_pair > 1)) {
  stop("Permit sample has non-constant treatment or geometry fields within block_id.", call. = FALSE)
}

permit_preperiod <- permit_sample %>%
  filter(relative_year < 0) %>%
  group_by(block_id) %>%
  summarise(
    pre_mean_n_high_discretion_issue = safe_mean(n_high_discretion_issue),
    pre_any_high_discretion_issue = as.integer(any(n_high_discretion_issue > 0, na.rm = TRUE)),
    pre_mean_n_low_discretion_nosigns_issue = safe_mean(n_low_discretion_nosigns_issue),
    pre_any_low_discretion_nosigns_issue = as.integer(any(n_low_discretion_nosigns_issue > 0, na.rm = TRUE)),
    .groups = "drop"
  )

permit_units <- permit_sample %>%
  group_by(block_id) %>%
  summarise(
    treat = dplyr::first(treat),
    strictness_change = dplyr::first(strictness_change),
    dist_ft = dplyr::first(dist_ft),
    ward_pair_id = dplyr::first(ward_pair_id),
    block_group_id = substr(as.character(dplyr::first(block_id)), 1, 12),
    .groups = "drop"
  ) %>%
  left_join(permit_preperiod, by = "block_id") %>%
  left_join(controls_2014, by = "block_group_id")

sales_panel <- read_parquet("../input/sales_transaction_panel_2015.parquet") %>%
  as_tibble()

hedonic_vars <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")
sales_sample <- sales_panel %>%
  filter(!is.na(strictness_change), !is.na(sale_price), sale_price > 0) %>%
  filter(dist_ft <= 1000) %>%
  filter(if_all(all_of(hedonic_vars), ~ !is.na(.x))) %>%
  mutate(
    relative_period = relative_year_capped,
    ward_pair = sub("_[0-9]+$", "", ward_pair_side)
  ) %>%
  filter(relative_period >= -5, relative_period <= 5) %>%
  filter(!is.na(ward_pair_side), ward_pair_side != "") %>%
  filter(!is.na(ward_pair), ward_pair != "")

sales_constancy <- sales_sample %>%
  group_by(block_id) %>%
  summarise(
    n_treat = n_distinct(treat),
    n_strictness = n_distinct(strictness_change),
    n_pair = n_distinct(ward_pair_id),
    n_side = n_distinct(ward_pair_side),
    .groups = "drop"
  )

if (any(sales_constancy$n_treat > 1 | sales_constancy$n_strictness > 1)) {
  stop("Sales sample has non-constant treatment fields within block_id.", call. = FALSE)
}

sales_preperiod <- sales_sample %>%
  filter(relative_period < 0) %>%
  group_by(block_id) %>%
  summarise(
    pre_mean_log_sale_price = safe_mean(log(sale_price)),
    pre_n_transactions = n(),
    pre_mean_building_sqft = safe_mean(building_sqft),
    pre_mean_land_sqft = safe_mean(land_sqft),
    pre_mean_building_age = safe_mean(building_age),
    pre_mean_num_bedrooms = safe_mean(num_bedrooms),
    pre_mean_baths_total = safe_mean(baths_total),
    pre_mean_has_garage = safe_mean(as.numeric(has_garage)),
    .groups = "drop"
  )

sales_units <- sales_sample %>%
  group_by(block_id) %>%
  summarise(
    treat = dplyr::first(treat),
    strictness_change = dplyr::first(strictness_change),
    dist_ft = safe_mean(dist_ft),
    ward_pair_count = n_distinct(ward_pair_id),
    ward_pair_side_count = n_distinct(ward_pair_side),
    block_group_id = substr(as.character(dplyr::first(block_id)), 1, 12),
    .groups = "drop"
  ) %>%
  left_join(sales_preperiod, by = "block_id") %>%
  left_join(controls_2014, by = "block_group_id")

permit_balance_full <- build_balance_full(permit_units, "permits", permit_vars, comparisons)
sales_balance_full <- build_balance_full(sales_units, "home_sales", sales_vars, comparisons)

assert_invariant(permit_units, "permits", permit_vars, comparisons, permit_balance_full)
assert_invariant(sales_units, "home_sales", sales_vars, comparisons, sales_balance_full)

permit_balance_summary <- build_balance_summary(permit_units, permit_sample, "permits", permit_balance_full, comparisons)
sales_balance_summary <- build_balance_summary(sales_units, sales_sample, "home_sales", sales_balance_full, comparisons)

permit_balance_flags <- permit_balance_full %>%
  filter(flag | material_imbalance) %>%
  arrange(comparison, desc(abs_std_diff), p_value, variable)

sales_balance_flags <- sales_balance_full %>%
  filter(flag | material_imbalance) %>%
  arrange(comparison, desc(abs_std_diff), p_value, variable)

write_csv(permit_balance_full %>% select(-abs_std_diff, -flag, -material_imbalance), permit_balance_full_path)
write_csv(sales_balance_full %>% select(-abs_std_diff, -flag, -material_imbalance), sales_balance_full_path)
write_csv(permit_balance_summary, permit_balance_summary_path)
write_csv(sales_balance_summary, sales_balance_summary_path)
write_csv(permit_balance_flags, permit_balance_flags_path)
write_csv(sales_balance_flags, sales_balance_flags_path)

permit_verdict <- branch_verdict(permit_balance_summary)
sales_verdict <- branch_verdict(sales_balance_summary)

permit_flag_counts <- permit_balance_summary %>%
  transmute(line = sprintf("- %s: %d flagged, %d material.", comparison, n_flagged, n_material)) %>%
  pull(line)

sales_flag_counts <- sales_balance_summary %>%
  transmute(line = sprintf("- %s: %d flagged, %d material.", comparison, n_flagged, n_material)) %>%
  pull(line)

memo_lines <- c(
  "# Event-Study Balance Audit",
  "",
  sprintf("Generated on %s.", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Permits",
  sprintf("- Verdict: `%s`.", permit_verdict),
  sprintf("- Analysis sample: %s block-year rows, %s unique blocks.", format(nrow(permit_sample), big.mark = ","), format(n_distinct(permit_sample$block_id), big.mark = ",")),
  sprintf("- 2014 block-group controls matched for %s of %s permit blocks.", format(sum(complete.cases(permit_units[, demographic_vars])), big.mark = ","), format(nrow(permit_units), big.mark = ",")),
  sprintf("- Duplicate 2014 block-group control rows before deduplication: %s.", format(sum(controls_2014$control_rows_2014 > 1), big.mark = ",")),
  "### Flag Counts",
  permit_flag_counts,
  "### Worst Five Variables",
  worst_five_lines(permit_balance_full),
  "",
  "## Home Sales",
  sprintf("- Verdict: `%s`.", sales_verdict),
  sprintf("- Analysis sample: %s transactions, %s unique blocks.", format(nrow(sales_sample), big.mark = ","), format(n_distinct(sales_sample$block_id), big.mark = ",")),
  sprintf("- 2014 block-group controls matched for %s of %s sales blocks.", format(sum(complete.cases(sales_units[, demographic_vars])), big.mark = ","), format(nrow(sales_units), big.mark = ",")),
  sprintf("- Blocks with transactions on multiple ward pairs in the analysis sample: %s.", format(sum(sales_units$ward_pair_count > 1, na.rm = TRUE), big.mark = ",")),
  "### Flag Counts",
  sales_flag_counts,
  "### Worst Five Variables",
  worst_five_lines(sales_balance_full),
  "",
  "## Default Interpretation",
  "- Treat `treated_vs_control` as the primary comparability check for the main event-study design.",
  "- Treat `stricter_vs_control`, `lenient_vs_control`, and `stricter_vs_lenient` as diagnostic comparisons for heterogeneity and split-spec fragility.",
  "- This memo is diagnostic only and does not change the manuscript."
)

writeLines(memo_lines, memo_path)
