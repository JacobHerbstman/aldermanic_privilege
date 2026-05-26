# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/price_rd_stress_tests_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

bandwidth_ft <- 500
bandwidth_m <- bandwidth_ft * FT_TO_M

message("=== Price RD Supply-Mechanism Stress Tests ===")
message(sprintf("Bandwidth: %d ft", bandwidth_ft))

stars <- function(p_value) {
  case_when(
    !is.finite(p_value) ~ "",
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

format_pct_cell <- function(estimate, std_error, p_value) {
  if (!is.finite(estimate) || !is.finite(std_error)) {
    return("")
  }
  sprintf(
    "%.2f%s (%.2f)",
    100 * (exp(estimate) - 1),
    stars(p_value),
    100 * std_error
  )
}

coef_p_value <- function(coef_table, row_name) {
  p_col <- grep("^Pr\\(", colnames(coef_table), value = TRUE)
  if (length(p_col) != 1L) {
    return(NA_real_)
  }
  unname(coef_table[row_name, p_col])
}

segment_gaps <- function(data, sample_label) {
  side <- data %>%
    group_by(segment_id, right) %>%
    summarise(
      ward_pair = first(na.omit(as.character(ward_pair))),
      pair_dash = first(na.omit(pair_dash)),
      era = first(na.omit(era)),
      n_projects = n(),
      total_units = sum(unitscount, na.rm = TRUE),
      mean_log_dupac = mean(log(density_dupac), na.rm = TRUE),
      mean_log_far = mean(log(density_far), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(side = if_else(right == 1L, "strict", "lenient")) %>%
    select(-right) %>%
    pivot_wider(
      names_from = side,
      values_from = c(n_projects, total_units, mean_log_dupac, mean_log_far)
    )

  side %>%
    transmute(
      segment_id,
      ward_pair,
      pair_dash,
      era,
      !!paste0("n_projects_strict_", sample_label) := n_projects_strict,
      !!paste0("n_projects_lenient_", sample_label) := n_projects_lenient,
      !!paste0("units_strict_", sample_label) := total_units_strict,
      !!paste0("units_lenient_", sample_label) := total_units_lenient,
      !!paste0("strict_minus_lenient_log_dupac_", sample_label) := mean_log_dupac_strict - mean_log_dupac_lenient,
      !!paste0("strict_minus_lenient_log_far_", sample_label) := mean_log_far_strict - mean_log_far_lenient,
      !!paste0("strict_minus_lenient_project_count_", sample_label) := n_projects_strict - n_projects_lenient,
      !!paste0("strict_minus_lenient_units_", sample_label) := total_units_strict - total_units_lenient
    )
}

message("Loading density/new-construction panel...")
parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    construction_year = suppressWarnings(as.integer(construction_year)),
    segment_id = as.character(segment_id),
    ward_pair = as.character(ward_pair),
    pair_dash = normalize_pair_dash(ward_pair),
    era = era_from_year(construction_year),
    signed_distance_ft = signed_distance_m * M_TO_FT,
    right = as.integer(signed_distance_m > 0),
    unitscount = suppressWarnings(as.numeric(unitscount)),
    density_dupac = suppressWarnings(as.numeric(density_dupac)),
    density_far = suppressWarnings(as.numeric(density_far)),
    homeownership_rate_bg = suppressWarnings(as.numeric(homeownership_rate_bg))
  ) %>%
  filter(
    construction_year >= 2006,
    construction_year <= 2022,
    is.finite(signed_distance_ft),
    abs(signed_distance_ft) <= bandwidth_ft,
    !is.na(segment_id),
    segment_id != "",
    unitscount > 0,
    density_dupac > 0,
    density_far > 0,
    !is.na(right)
  )

first_stage_all <- segment_gaps(parcels, "all")
first_stage_mf <- segment_gaps(parcels %>% filter(unitscount > 1), "mf")

first_stage <- full_join(
  first_stage_all,
  first_stage_mf,
  by = "segment_id",
  suffix = c("_allmeta", "_mfmeta"),
  relationship = "one-to-one"
) %>%
  transmute(
    segment_id,
    ward_pair = coalesce(ward_pair_mfmeta, ward_pair_allmeta),
    pair_dash = coalesce(pair_dash_mfmeta, pair_dash_allmeta),
    era = coalesce(era_mfmeta, era_allmeta),
    n_projects_strict_all,
    n_projects_lenient_all,
    units_strict_all,
    units_lenient_all,
    strict_minus_lenient_log_dupac_all,
    strict_minus_lenient_log_far_all,
    strict_minus_lenient_project_count_all,
    strict_minus_lenient_units_all,
    n_projects_strict_mf,
    n_projects_lenient_mf,
    units_strict_mf,
    units_lenient_mf,
    strict_minus_lenient_log_dupac_mf,
    strict_minus_lenient_log_far_mf,
    strict_minus_lenient_project_count_mf,
    strict_minus_lenient_units_mf,
    supply_constraint_dupac_mf = -strict_minus_lenient_log_dupac_mf,
    supply_constraint_far_mf = -strict_minus_lenient_log_far_mf
  )

if (anyDuplicated(first_stage$segment_id) > 0) {
  stop("First-stage file is not unique by segment_id.", call. = FALSE)
}

finite_first_stage <- is.finite(first_stage$supply_constraint_dupac_mf)
first_stage_mean <- mean(first_stage$supply_constraint_dupac_mf[finite_first_stage])
first_stage_sd <- sd(first_stage$supply_constraint_dupac_mf[finite_first_stage])
if (!is.finite(first_stage_sd) || first_stage_sd <= 0) {
  stop("Primary supply first stage has no variation.", call. = FALSE)
}
first_stage_median <- median(first_stage$supply_constraint_dupac_mf[finite_first_stage])

first_stage <- first_stage %>%
  mutate(
    supply_constraint_std = (supply_constraint_dupac_mf - first_stage_mean) / first_stage_sd,
    supply_constraint_split = case_when(
      !is.finite(supply_constraint_dupac_mf) ~ NA_character_,
      supply_constraint_dupac_mf > first_stage_median ~ "High supply constraint",
      TRUE ~ "Low supply constraint"
    ),
    supply_constraint_split = factor(
      supply_constraint_split,
      levels = c("Low supply constraint", "High supply constraint")
    )
  )

write_csv(first_stage, "../output/price_rd_supply_first_stage_segments.csv")

first_stage_support <- first_stage %>%
  summarise(
    bandwidth_ft = bandwidth_ft,
    n_segments = n(),
    n_segments_with_primary_first_stage = sum(is.finite(supply_constraint_dupac_mf)),
    n_segments_high_supply_constraint = sum(supply_constraint_split == "High supply constraint", na.rm = TRUE),
    n_segments_low_supply_constraint = sum(supply_constraint_split == "Low supply constraint", na.rm = TRUE),
    median_supply_constraint_dupac_mf = first_stage_median,
    mean_supply_constraint_dupac_mf = first_stage_mean,
    sd_supply_constraint_dupac_mf = first_stage_sd,
    n_ward_pairs = n_distinct(ward_pair)
  )
write_csv(first_stage_support, "../output/price_rd_supply_first_stage_support.csv")

message("Loading pruned-boundary flags...")
segment_flags <- read_csv(
  "../input/confounded_segment_flags.csv",
  show_col_types = FALSE,
  col_types = cols(segment_id = col_character(), .default = col_guess())
) %>%
  mutate(
    segment_id = as.character(segment_id),
    drop_confound = as.logical(drop_confound),
    drop_reason = as.character(drop_reason)
  ) %>%
  filter(!is.na(segment_id), segment_id != "") %>%
  group_by(segment_id) %>%
  summarise(
    drop_confound = any(drop_confound, na.rm = TRUE),
    drop_reason = paste(sort(unique(na.omit(drop_reason))), collapse = "; "),
    .groups = "drop"
  )
if (anyDuplicated(segment_flags$segment_id) > 0) {
  stop("Pruned segment flags are not unique by segment_id after collapse.", call. = FALSE)
}

message("Loading rent and sales RD panels...")
rent <- read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble()
sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble()

if (!"rent_panel_id" %in% names(rent)) {
  stop("Rental input must include rent_panel_id.", call. = FALSE)
}
if (any(is.na(rent$rent_panel_id) | rent$rent_panel_id == "")) {
  stop("Rental input contains missing rent_panel_id.", call. = FALSE)
}
if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Rental input is not unique by rent_panel_id.", call. = FALSE)
}
if (!"signed_dist" %in% names(rent) && "signed_dist_m" %in% names(rent)) {
  rent <- rent %>% mutate(signed_dist = signed_dist_m * M_TO_FT)
}
if (!"signed_dist" %in% names(rent)) {
  stop("Rental input must include signed_dist or signed_dist_m.", call. = FALSE)
}

rent <- rent %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    right = as.integer(signed_dist_ft >= 0),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    rent_row_id = row_number()
  ) %>%
  filter(
    year >= 2014,
    year <= 2022,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= bandwidth_ft,
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    !is.na(strictness_own),
    !is.na(strictness_neighbor)
  )

if (!"signed_dist" %in% names(sales)) {
  sales <- sales %>% mutate(signed_dist = signed_dist_m * M_TO_FT)
}
sales <- sales %>%
  mutate(
    sale_date = as.Date(sale_date),
    year = lubridate::year(sale_date),
    year_quarter = paste0(year, "-Q", lubridate::quarter(sale_date)),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    right = as.integer(signed_dist_ft >= 0),
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000,
    sale_row_id = row_number()
  ) %>%
  filter(
    year >= 2006,
    year <= 2022,
    is.finite(sale_price),
    sale_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= bandwidth_ft,
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    !is.na(strictness_own),
    !is.na(strictness_neighbor)
  )

rent_context <- rent %>%
  filter(year >= 2014, year <= 2016, is.finite(rent_price), rent_price > 0) %>%
  group_by(segment_id) %>%
  summarise(
    baseline_log_rent = median(log(rent_price), na.rm = TRUE),
    baseline_rent_obs = n(),
    .groups = "drop"
  )
baseline_rent_median <- median(rent_context$baseline_log_rent, na.rm = TRUE)

cta_context <- rent %>%
  group_by(segment_id) %>%
  summarise(
    segment_cta_dist_ft = median(nearest_cta_stop_dist_ft, na.rm = TRUE),
    .groups = "drop"
  )
cta_median <- median(cta_context$segment_cta_dist_ft, na.rm = TRUE)

homeownership_context <- parcels %>%
  group_by(segment_id) %>%
  summarise(
    segment_homeownership_rate = mean(homeownership_rate_bg, na.rm = TRUE),
    .groups = "drop"
  )
homeownership_median <- median(homeownership_context$segment_homeownership_rate, na.rm = TRUE)

segment_context <- first_stage %>%
  select(segment_id, supply_constraint_dupac_mf, supply_constraint_std, supply_constraint_split) %>%
  left_join(rent_context, by = "segment_id", relationship = "one-to-one") %>%
  left_join(cta_context, by = "segment_id", relationship = "one-to-one") %>%
  left_join(homeownership_context, by = "segment_id", relationship = "one-to-one") %>%
  mutate(
    baseline_rent_split = case_when(
      !is.finite(baseline_log_rent) ~ NA_character_,
      baseline_log_rent > baseline_rent_median ~ "High baseline rent",
      TRUE ~ "Low baseline rent"
    ),
    cta_access_split = case_when(
      !is.finite(segment_cta_dist_ft) ~ NA_character_,
      segment_cta_dist_ft <= cta_median ~ "Near CTA",
      TRUE ~ "Farther from CTA"
    ),
    renter_share_split = case_when(
      !is.finite(segment_homeownership_rate) ~ NA_character_,
      segment_homeownership_rate <= homeownership_median ~ "High renter share",
      TRUE ~ "Low renter share"
    )
  )
if (anyDuplicated(segment_context$segment_id) > 0) {
  stop("Segment context split file is not unique by segment_id.", call. = FALSE)
}
write_csv(segment_context, "../output/price_rd_segment_context_splits.csv")

join_segment_diagnostics <- function(data, row_id_col) {
  before_n <- nrow(data)
  out <- data %>%
    left_join(
      first_stage %>%
        select(
          segment_id,
          supply_constraint_dupac_mf,
          supply_constraint_std,
          supply_constraint_split
        ),
      by = "segment_id",
      relationship = "many-to-one"
    ) %>%
    left_join(
      segment_context %>%
        select(segment_id, baseline_rent_split, cta_access_split, renter_share_split),
      by = "segment_id",
      relationship = "many-to-one"
    ) %>%
    left_join(segment_flags, by = "segment_id", relationship = "many-to-one") %>%
    mutate(
      drop_confound = coalesce(drop_confound, NA),
      keep_pruned_segment = !is.na(drop_confound) & !drop_confound
    )
  if (nrow(out) != before_n) {
    stop(sprintf("Segment joins expanded %s rows.", row_id_col), call. = FALSE)
  }
  if (anyDuplicated(out[[row_id_col]]) > 0) {
    stop(sprintf("Segment joins duplicated %s.", row_id_col), call. = FALSE)
  }
  out
}

rent <- join_segment_diagnostics(rent, "rent_panel_id")
sales <- join_segment_diagnostics(sales, "sale_row_id")

rent_numeric_controls <- c(
  "log_sqft",
  "log_beds",
  "log_baths",
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft"
)
rent_controls <- c(rent_numeric_controls, "building_type_factor")
sales_controls <- c(
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage",
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "lake_michigan_dist_kft"
)

missing_rent_controls <- setdiff(rent_numeric_controls, names(rent))
missing_sales_controls <- setdiff(sales_controls, names(sales))
if (length(missing_rent_controls) > 0) {
  stop(sprintf("Rental panel is missing controls: %s", paste(missing_rent_controls, collapse = ", ")), call. = FALSE)
}
if (length(missing_sales_controls) > 0) {
  stop(sprintf("Sales panel is missing controls: %s", paste(missing_sales_controls, collapse = ", ")), call. = FALSE)
}

rent <- rent %>%
  filter(if_all(all_of(rent_numeric_controls), ~ is.finite(.x)))
sales <- sales %>%
  filter(if_all(all_of(sales_controls), ~ is.finite(.x)))

keep_two_sided_segments <- function(data) {
  two_sided <- data %>%
    group_by(segment_id) %>%
    summarise(has_left = any(right == 0), has_right = any(right == 1), .groups = "drop") %>%
    filter(has_left, has_right) %>%
    select(segment_id)
  data %>% semi_join(two_sided, by = "segment_id")
}

sample_support <- function(data, dataset, sample_name) {
  segment_side <- data %>%
    group_by(segment_id) %>%
    summarise(has_left = any(right == 0), has_right = any(right == 1), .groups = "drop")
  tibble(
    dataset = dataset,
    sample = sample_name,
    n_obs = nrow(data),
    n_left = sum(data$right == 0, na.rm = TRUE),
    n_right = sum(data$right == 1, na.rm = TRUE),
    n_segments = n_distinct(data$segment_id),
    n_two_sided_segments = sum(segment_side$has_left & segment_side$has_right),
    n_ward_pairs = n_distinct(data$ward_pair),
    n_missing_supply_first_stage = sum(!is.finite(data$supply_constraint_dupac_mf)),
    n_pruned_flag_missing = sum(is.na(data$drop_confound))
  )
}

fit_rd <- function(data, dataset, sample_name, outcome, time_var, controls, cluster_var, rhs_prefix = "right", coefficient_names = "right") {
  if (nrow(data) == 0 || n_distinct(data$right) < 2 || n_distinct(data[[cluster_var]]) < 2) {
    return(tibble(
      dataset = dataset,
      sample = sample_name,
      coefficient = coefficient_names,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(data),
      n_segments = n_distinct(data$segment_id),
      n_ward_pairs = n_distinct(data$ward_pair),
      status = "insufficient_support"
    ))
  }

  rhs <- paste(c(rhs_prefix, controls), collapse = " + ")
  fit <- tryCatch(
    feols(
      as.formula(paste0("log(", outcome, ") ~ ", rhs, " | segment_id^", time_var)),
      data = data,
      cluster = as.formula(paste0("~", cluster_var))
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(tibble(
      dataset = dataset,
      sample = sample_name,
      coefficient = coefficient_names,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(data),
      n_segments = n_distinct(data$segment_id),
      n_ward_pairs = n_distinct(data$ward_pair),
      status = paste("model_error:", fit$message)
    ))
  }

  ct <- coeftable(fit)
  bind_rows(lapply(coefficient_names, function(coef_name) {
    match_name <- coef_name
    if (!match_name %in% rownames(ct) && coef_name == "right:supply_constraint_std") {
      alt <- "supply_constraint_std:right"
      if (alt %in% rownames(ct)) {
        match_name <- alt
      }
    }
    if (!match_name %in% rownames(ct)) {
      return(tibble(
        dataset = dataset,
        sample = sample_name,
        coefficient = coef_name,
        estimate = NA_real_,
        std_error = NA_real_,
        p_value = NA_real_,
        n_obs = fit$nobs,
        n_segments = n_distinct(data$segment_id),
        n_ward_pairs = n_distinct(data$ward_pair),
        status = "coefficient_not_estimated"
      ))
    }
    tibble(
      dataset = dataset,
      sample = sample_name,
      coefficient = coef_name,
      estimate = unname(ct[match_name, "Estimate"]),
      std_error = unname(ct[match_name, "Std. Error"]),
      p_value = coef_p_value(ct, match_name),
      n_obs = fit$nobs,
      n_segments = n_distinct(data$segment_id),
      n_ward_pairs = n_distinct(data$ward_pair),
      status = "ok"
    )
  }))
}

estimate_rows <- list()
support_rows <- list()

add_estimate <- function(data, dataset, sample_name, outcome, time_var, controls, cluster_var, rhs_prefix = "right", coefficient_names = "right") {
  data <- keep_two_sided_segments(data)
  support_rows[[length(support_rows) + 1]] <<- sample_support(data, dataset, sample_name)
  estimate_rows[[length(estimate_rows) + 1]] <<- fit_rd(
    data,
    dataset,
    sample_name,
    outcome,
    time_var,
    controls,
    cluster_var,
    rhs_prefix,
    coefficient_names
  )
}

run_split <- function(data, dataset, split_var, split_title, outcome, time_var, controls, cluster_var) {
  d <- data %>% filter(!is.na(.data[[split_var]]))
  for (split_value in sort(unique(as.character(d[[split_var]])))) {
    add_estimate(
      d %>% filter(.data[[split_var]] == split_value),
      dataset,
      paste(split_title, split_value, sep = ": "),
      outcome,
      time_var,
      controls,
      cluster_var
    )
  }
}

add_estimate(rent, "Listed rents", "Main sample", "rent_price", "year_month", rent_controls, "segment_id")
add_estimate(sales, "Home sales", "Main sample", "sale_price", "year_quarter", sales_controls, "ward_pair")

run_split(rent, "Listed rents", "supply_constraint_split", "Supply first stage", "rent_price", "year_month", rent_controls, "segment_id")
run_split(sales, "Home sales", "supply_constraint_split", "Supply first stage", "sale_price", "year_quarter", sales_controls, "ward_pair")

add_estimate(
  rent %>% filter(is.finite(supply_constraint_std)),
  "Listed rents",
  "Supply first stage interaction",
  "rent_price",
  "year_month",
  rent_controls,
  "segment_id",
  "right * supply_constraint_std",
  c("right", "right:supply_constraint_std")
)
add_estimate(
  sales %>% filter(is.finite(supply_constraint_std)),
  "Home sales",
  "Supply first stage interaction",
  "sale_price",
  "year_quarter",
  sales_controls,
  "ward_pair",
  "right * supply_constraint_std",
  c("right", "right:supply_constraint_std")
)

for (split_var in c("baseline_rent_split", "cta_access_split", "renter_share_split")) {
  split_title <- recode(
    split_var,
    baseline_rent_split = "Baseline rent pressure",
    cta_access_split = "CTA access",
    renter_share_split = "Renter-share context"
  )
  run_split(rent, "Listed rents", split_var, split_title, "rent_price", "year_month", rent_controls, "segment_id")
  run_split(sales, "Home sales", split_var, split_title, "sale_price", "year_quarter", sales_controls, "ward_pair")
}

add_estimate(rent %>% filter(year_built <= 2000), "Listed rents", "Old stock: built by 2000", "rent_price", "year_month", rent_controls, "segment_id")
add_estimate(rent %>% filter(year_built <= 1980), "Listed rents", "Old stock: built by 1980", "rent_price", "year_month", rent_controls, "segment_id")
add_estimate(sales %>% filter(year_built <= 2000), "Home sales", "Old stock: built by 2000", "sale_price", "year_quarter", sales_controls, "ward_pair")
add_estimate(sales %>% filter(year_built <= 1980), "Home sales", "Old stock: built by 1980", "sale_price", "year_quarter", sales_controls, "ward_pair")

add_estimate(rent %>% filter(keep_pruned_segment), "Listed rents", "Pruned segments", "rent_price", "year_month", rent_controls, "segment_id")
add_estimate(sales %>% filter(keep_pruned_segment), "Home sales", "Pruned segments", "sale_price", "year_quarter", sales_controls, "ward_pair")

estimates <- bind_rows(estimate_rows) %>%
  mutate(
    pct_change = 100 * (exp(estimate) - 1),
    pct_ci_low = 100 * (exp(estimate - 1.96 * std_error) - 1),
    pct_ci_high = 100 * (exp(estimate + 1.96 * std_error) - 1),
    bandwidth_ft = bandwidth_ft
  )
write_csv(estimates, "../output/price_rd_stress_test_estimates.csv")

support <- bind_rows(support_rows) %>%
  mutate(bandwidth_ft = bandwidth_ft)
write_csv(support, "../output/price_rd_stress_test_support.csv")

old_stock_attrition <- bind_rows(
  tibble(
    dataset = "Listed rents",
    total_rows = nrow(rent),
    missing_year_built = sum(!is.finite(rent$year_built)),
    built_by_2000 = sum(rent$year_built <= 2000, na.rm = TRUE),
    built_by_1980 = sum(rent$year_built <= 1980, na.rm = TRUE)
  ),
  tibble(
    dataset = "Home sales",
    total_rows = nrow(sales),
    missing_year_built = sum(!is.finite(sales$year_built)),
    built_by_2000 = sum(sales$year_built <= 2000, na.rm = TRUE),
    built_by_1980 = sum(sales$year_built <= 1980, na.rm = TRUE)
  )
) %>%
  mutate(
    share_missing_year_built = missing_year_built / total_rows,
    share_built_by_2000 = built_by_2000 / total_rows,
    share_built_by_1980 = built_by_1980 / total_rows
  )
write_csv(old_stock_attrition, "../output/price_rd_old_stock_attrition.csv")

pruned_support <- bind_rows(
  rent %>%
    summarise(
      dataset = "Listed rents",
      total_rows = n(),
      missing_pruned_flag = sum(is.na(drop_confound)),
      dropped_by_pruned_flag = sum(drop_confound %in% TRUE, na.rm = TRUE),
      kept_by_pruned_flag = sum(keep_pruned_segment, na.rm = TRUE),
      total_segments = n_distinct(segment_id),
      kept_segments = n_distinct(segment_id[keep_pruned_segment])
    ),
  sales %>%
    summarise(
      dataset = "Home sales",
      total_rows = n(),
      missing_pruned_flag = sum(is.na(drop_confound)),
      dropped_by_pruned_flag = sum(drop_confound %in% TRUE, na.rm = TRUE),
      kept_by_pruned_flag = sum(keep_pruned_segment, na.rm = TRUE),
      total_segments = n_distinct(segment_id),
      kept_segments = n_distinct(segment_id[keep_pruned_segment])
    )
)
write_csv(pruned_support, "../output/price_rd_pruned_sample_support.csv")

message("Checking listed-supply normalization feasibility...")
residential <- read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  distinct(pin, .keep_all = TRUE)
parcel_segments <- read_csv(
  "../input/parcel_segment_ids.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
)
if (anyDuplicated(parcel_segments$pin) > 0) {
  stop("parcel_segment_ids.csv is not unique by pin.", call. = FALSE)
}

residential_segment_join <- residential %>%
  left_join(parcel_segments, by = "pin", relationship = "many-to-one")

normalization_diagnostics <- bind_rows(
  tibble(
    candidate = "residential_cross_section + parcel_segment_ids",
    n_rows = nrow(residential),
    n_with_segment = sum(!is.na(residential_segment_join$segment_id) & residential_segment_join$segment_id != ""),
    has_segment_side = FALSE,
    usable_for_segment_side_ppml = FALSE,
    reason = "Existing stock join identifies segment_id but not strict/lenient side by segment and era."
  ),
  tibble(
    candidate = "parcels_with_ward_distances",
    n_rows = nrow(parcels),
    n_with_segment = sum(!is.na(parcels$segment_id) & parcels$segment_id != ""),
    has_segment_side = TRUE,
    usable_for_segment_side_ppml = FALSE,
    reason = "This is realized new construction flow, not an underlying rental/residential stock denominator."
  )
)
write_csv(normalization_diagnostics, "../output/listed_supply_normalization_diagnostics.csv")

supply_split_plot_data <- estimates %>%
  filter(
    coefficient == "right",
    str_starts(sample, "Supply first stage:"),
    status == "ok"
  ) %>%
  mutate(
    split = str_remove(sample, "^Supply first stage: "),
    split = factor(split, levels = c("Low supply constraint", "High supply constraint"))
  )

plot_supply <- ggplot(supply_split_plot_data, aes(x = split, y = pct_change, ymin = pct_ci_low, ymax = pct_ci_high, color = dataset)) +
  geom_hline(yintercept = 0, color = "gray55", linetype = "dotted") +
  geom_pointrange(position = position_dodge(width = 0.45), linewidth = 0.45) +
  facet_wrap(~dataset, scales = "free_y") +
  scale_color_manual(values = c("Listed rents" = "#1f77b4", "Home sales" = "#d62728"), guide = "none") +
  labs(
    title = "Price RD by Segment-Level Multifamily Supply First Stage",
    subtitle = "High supply constraint means lower realized multifamily DUPAC on the stricter side",
    x = NULL,
    y = "Estimated stricter-side price jump (%)"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())
ggsave("../output/price_rd_supply_split_estimates.pdf", plot_supply, width = 8.8, height = 5.4, dpi = 300, bg = "white")

plot_all_data <- estimates %>%
  filter(coefficient == "right", status == "ok") %>%
  mutate(
    sample = factor(
      sample,
      levels = c(
        "Main sample",
        "Supply first stage: Low supply constraint",
        "Supply first stage: High supply constraint",
        "Baseline rent pressure: Low baseline rent",
        "Baseline rent pressure: High baseline rent",
        "CTA access: Farther from CTA",
        "CTA access: Near CTA",
        "Renter-share context: Low renter share",
        "Renter-share context: High renter share",
        "Old stock: built by 2000",
        "Old stock: built by 1980",
        "Pruned segments"
      )
    )
  ) %>%
  filter(!is.na(sample))

plot_all <- ggplot(plot_all_data, aes(x = sample, y = pct_change, ymin = pct_ci_low, ymax = pct_ci_high, color = dataset)) +
  geom_hline(yintercept = 0, color = "gray55", linetype = "dotted") +
  geom_pointrange(linewidth = 0.35) +
  facet_wrap(~dataset, scales = "free_y") +
  coord_flip() +
  scale_color_manual(values = c("Listed rents" = "#1f77b4", "Home sales" = "#d62728"), guide = "none") +
  labs(
    title = "Price RD Stress Tests",
    x = NULL,
    y = "Estimated stricter-side price jump (%)"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())
ggsave("../output/price_rd_stress_test_estimates.pdf", plot_all, width = 9.5, height = 7.2, dpi = 300, bg = "white")

tex_data <- supply_split_plot_data %>%
  mutate(cell = pmap_chr(list(estimate, std_error, p_value), format_pct_cell)) %>%
  select(dataset, split, cell) %>%
  pivot_wider(names_from = split, values_from = cell)

tex_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "Outcome & Low supply constraint & High supply constraint \\\\",
  "\\midrule",
  sprintf(
    "%s & %s & %s \\\\",
    tex_data$dataset,
    tex_data$`Low supply constraint`,
    tex_data$`High supply constraint`
  ),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tex_lines, "../output/price_rd_supply_split_estimates.tex")

message("Saved price RD stress-test diagnostics.")
