# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/working_paper_release_audit/code")
suppressPackageStartupMessages({library(dplyr); library(readr); library(arrow); library(fixest); library(tidyr)})
setFixest_nthreads(2)
terms <- read_csv("../input/terms.csv", show_col_types = FALSE)
scores <- read_csv("../input/scores.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(scores$alderman), !anyDuplicated(terms[c("ward", "start_date")]))
score_vector <- setNames(scores$uncertainty_index, scores$alderman)

# This lookup is reused for construction and rental observations.
check_dates <- function(data) {
  data |>
    left_join(terms |> rename(exact_own = alderman),
              by = join_by(ward, date >= start_date, date <= end_date), relationship = "many-to-one") |>
    select(-start_date, -end_date) |>
    left_join(terms |> rename(neighbor_ward = ward, exact_neighbor = alderman),
              by = join_by(neighbor_ward, date >= start_date, date <= end_date), relationship = "many-to-one") |>
    select(-start_date, -end_date) |>
    mutate(own_mismatch = coalesce(alderman_own, "VACANT") != coalesce(exact_own, "VACANT"),
           neighbor_mismatch = coalesce(alderman_neighbor, "VACANT") != coalesce(exact_neighbor, "VACANT"),
           exact_sign = sign(unname(score_vector[exact_own] - score_vector[exact_neighbor])),
           old_sign = sign(distance),
           orientation_change = is.finite(exact_sign) & exact_sign != 0 & exact_sign != old_sign,
           loses_score = !is.finite(exact_sign) | exact_sign == 0)
}

projects <- read_csv("../input/projects.csv", show_col_types = FALSE) |>
  mutate(date = as.Date(construction_date),
         distance = abs(signed_distance_m) / 0.3048 * sign(unname(score_vector[alderman_own] - score_vector[alderman_neighbor]))) |>
  filter(construction_year >= 2006, construction_year <= 2022, dwelling_units > 0,
         allow_far, allow_dupac, is.finite(density_far), density_far > 0,
         is.finite(density_dupac), density_dupac > 0,
         if_all(c(share_white_own, share_black_own, median_hh_income_own, share_bach_plus_own, homeownership_rate_own), is.finite),
         !is.na(zone_group), !is.na(segment_id), segment_id != "", !is.na(ward_pair), ward_pair != "") |>
  check_dates()

rent <- read_parquet("../input/rents.parquet") |>
  mutate(date = as.Date(assignment_date), distance = signed_dist,
         year_month = format(as.Date(file_date), "%Y-%m"), ward_pair = as.character(ward_pair_id),
         log_sqft = log(sqft), log_baths = log(baths), beds_factor = factor(beds),
         building_type_factor = factor(coalesce(building_type_clean, "other"))) |>
  filter(!is.na(date), rent_price > 0, is.finite(distance), abs(distance) < 1500,
         is.finite(strictness_own), is.finite(strictness_neighbor), strictness_own != strictness_neighbor,
         !is.na(segment_id), segment_id != "", !is.na(ward_pair), ward_pair != "", flag_clean_location_sample,
         is.finite(beds), beds >= 0, is.finite(log_sqft), is.finite(log_baths),
         if_all(c(longitude, latitude, nearest_school_dist_kft, nearest_park_dist_kft, nearest_major_road_dist_kft,
                  nearest_cta_stop_dist_kft, lake_michigan_dist_kft), is.finite)) |>
  check_dates()

sales <- read_parquet("../input/sales.parquet") |>
  mutate(distance = signed_dist_m / 0.3048, ward_pair = as.character(ward_pair_id),
         year_quarter = paste0(lubridate::year(sale_date), "-Q", lubridate::quarter(sale_date)),
         property_class_factor = factor(class)) |>
  filter(sale_price > 0, is.finite(distance), abs(distance) < 1500,
         is.finite(strictness_own), is.finite(strictness_neighbor), strictness_own != strictness_neighbor,
         !is.na(segment_id), segment_id != "", !is.na(ward_pair), ward_pair != "",
         if_all(c(longitude, latitude, log_sqft, log_land_sqft, log_building_age, log_bedrooms, log_baths,
                  has_garage, nearest_school_dist_ft, nearest_park_dist_ft, nearest_major_road_dist_ft,
                  nearest_cta_stop_dist_ft, lake_michigan_dist_ft), is.finite))

assignment_rows <- bind_rows(
  projects |> transmute(dataset = "density", id = project_id, date, ward, neighbor_ward, distance,
                        alderman_own, exact_own, alderman_neighbor, exact_neighbor, own_mismatch,
                        neighbor_mismatch, orientation_change, loses_score),
  rent |> transmute(dataset = "rent", id = rent_panel_id, date, ward, neighbor_ward, distance,
                    alderman_own, exact_own, alderman_neighbor, exact_neighbor, own_mismatch,
                    neighbor_mismatch, orientation_change, loses_score))
write_csv(assignment_rows |> filter(own_mismatch | neighbor_mismatch), "../output/assignment_mismatches.csv")
write_csv(assignment_rows |> mutate(main = abs(distance) < 500) |>
            summarise(n = n(), any_mismatch = sum(own_mismatch | neighbor_mismatch),
                      flips = sum(orientation_change, na.rm = TRUE), loses_score = sum(loses_score),
                      own_mismatch = sum(own_mismatch), neighbor_mismatch = sum(neighbor_mismatch),
                      .by = c(dataset, main)), "../output/assignment_summary.csv")

# Repeated formula construction lets each check change only its named choice.
fit_boundary <- function(data, outcome, controls, fixed_effects, bin_width = 100, bandwidth = 500) {
  data <- data |> filter(abs(distance) < bandwidth) |>
    mutate(distance_bin = floor(distance / bin_width), log_outcome = log(.data[[outcome]]))
  model <- feols(as.formula(paste0("log_outcome ~ i(distance_bin, ref = -1)",
                         if (nzchar(controls)) paste0(" + ", controls), " | ", fixed_effects)),
                 data = data, cluster = ~ward_pair, notes = FALSE, warn = FALSE)
  list(model = model, data = data)
}
summarize_fit <- function(fit, market, check, term = "distance_bin::0") {
  model <- fit$model; data <- fit$data
  ct <- coeftable(model)
  used <- obs(model)
  model_data <- data[used, ]
  # Shared-ward covariance: aggregate each observation's score at both endpoints,
  # then subtract pair aggregates once. No small-sample correction; diagnostic only.
  endpoints <- strsplit(gsub("_", "-", model_data$ward_pair), "-", fixed = TRUE)
  a <- vapply(endpoints, `[`, character(1), 1); b <- vapply(endpoints, `[`, character(1), 2)
  score_rows <- model$scores
  hessian <- model$hessian
  if (ncol(hessian) != length(coef(model))) {
    kept <- which(!is.na(model$collin.coef))
    hessian <- hessian[kept, kept, drop = FALSE]
    if (ncol(score_rows) != length(coef(model))) score_rows <- score_rows[, kept, drop = FALSE]
  }
  stopifnot(ncol(score_rows) == length(coef(model)), ncol(hessian) == length(coef(model)))
  ward_sums <- rowsum(rbind(score_rows, score_rows), c(a, b))
  pair_sums <- rowsum(score_rows, model_data$ward_pair)
  bread <- solve(hessian)
  pair_vcov <- bread %*% crossprod(pair_sums) %*% bread
  dyadic_vcov <- bread %*% (crossprod(ward_sums) - crossprod(pair_sums)) %*% bread
  j <- match(term, rownames(ct))
  pair_se <- sqrt(pair_vcov[j,j])
  reference_vcov <- vcov(model, cluster = ~ward_pair, ssc = ssc(adj = FALSE, cluster.adj = FALSE))
  stopifnot(isTRUE(all.equal(pair_se, sqrt(reference_vcov[j,j]), tolerance = 1e-6)))
  dyadic_se <- if (dyadic_vcov[j,j] >= 0) sqrt(dyadic_vcov[j,j]) else NA_real_
  tibble(market, check, estimate = ct[term, 1], se = ct[term, 2], p_value = ct[term, 4],
         n = nobs(model), rows_supplied = nrow(data), clusters = n_distinct(model_data$ward_pair),
         pair_se_uncorrected = pair_se, shared_ward_se_uncorrected = dyadic_se,
         shared_ward_se_same_correction = dyadic_se * ct[term, 2] / pair_se)
}

density_controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
rent_controls <- "log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor"
sales_controls <- "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor"
results <- list(); models <- list()
for (sample in c("all", "multifamily")) {
  d <- projects |> filter(sample == "all" | external_multifamily)
  for (outcome in c("density_far", "density_dupac")) {
    market <- paste(sample, outcome, sep = "_")
    for (check in c("baseline", "daily_aldermen", "no_demographics", "no_zoning", "bin50", "bin200", "bandwidth250", "bandwidth1000")) {
      fit <- fit_boundary(
        if (check == "daily_aldermen") d |> mutate(distance = distance_to_boundary_ft * exact_sign) |> filter(!loses_score) else d,
        outcome, if (check == "no_demographics") "" else density_controls,
        if (check == "no_zoning") "segment_id + construction_year" else "zone_group + segment_id + construction_year",
        bin_width = if (check == "bin50") 50 else if (check == "bin200") 200 else 100,
        bandwidth = if (check == "bandwidth250") 250 else if (check == "bandwidth1000") 1000 else 500)
      results[[paste(market, check)]] <- summarize_fit(fit, market, check)
      if (check == "baseline") models[[market]] <- fit
    }
  }
}
for (market in c("rent", "sales")) {
  d <- if (market == "rent") rent else sales
  controls <- if (market == "rent") rent_controls else sales_controls
  outcome <- if (market == "rent") "rent_price" else "sale_price"
  fixed_effects <- if (market == "rent") "segment_id^year_month" else "segment_id^year_quarter"
  for (check in c("baseline", "daily_aldermen", "bin50", "bin200", "bandwidth250", "bandwidth1000", "tail2000", "single_family")) {
    if (market == "rent" && check %in% c("tail2000", "single_family")) next
    if (market == "sales" && check == "daily_aldermen") next
    d_check <- d
    if (check == "daily_aldermen") d_check <- d |> mutate(distance = abs(distance) * exact_sign) |> filter(!loses_score)
    if (check == "tail2000") d_check <- d |> filter(sale_price_nominal / building_sqft <= 2000)
    if (check == "single_family") d_check <- d |> filter(class != 211)
    fit <- fit_boundary(d_check, outcome, controls, fixed_effects,
                        bin_width = if (check == "bin50") 50 else if (check == "bin200") 200 else 100,
                        bandwidth = if (check == "bandwidth250") 250 else if (check == "bandwidth1000") 1000 else 500)
    results[[paste(market, check)]] <- summarize_fit(fit, market, check)
    if (check == "baseline") models[[market]] <- fit
  }
}

permits <- read_parquet("../input/permits.parquet") |>
  filter(dist_m <= 152.4, relative_year >= -5, relative_year <= 5, stable_both,
         is.finite(strictness_change_frozen), !is.na(ward_pair_id), ward_pair_id != "") |>
  mutate(ward_pair = ward_pair_id, post = as.integer(relative_year >= 0),
         post_treatment = post * sign(strictness_change_frozen))
permits <- permits |>
  group_by(block_id) |>
  mutate(pre_count = sum(n_high_discretion_application[relative_year < 0])) |>
  ungroup()
for (check in c("baseline", "all_blocks", "omit2015")) {
  d <- permits
  if (check != "all_blocks") d <- d |> filter(pre_count > 0)
  if (check == "omit2015") d <- d |> filter(relative_year != 0)
  model <- fepois(n_high_discretion_application ~ post_treatment | block_id + ward_pair_id^year,
                  data = d, cluster = ~ward_pair, notes = FALSE)
  results[[paste("permits", check)]] <- summarize_fit(list(model = model, data = d), "permits", check, "post_treatment")
}

support <- lapply(names(models), function(market) {
  d <- models[[market]]$data
  fe <- if (market == "rent") interaction(d$segment_id, d$year_month, drop = TRUE) else if (market == "sales") interaction(d$segment_id, d$year_quarter, drop = TRUE) else d$segment_id
  tibble(fe = as.character(fe), side = sign(d$distance)) |>
    summarise(n = n(), sides = n_distinct(side), .by = fe) |>
    summarise(market, groups = n(), singleton_groups = sum(n == 1), singleton_rows = sum(n[n == 1]),
              one_side_rows = sum(n[sides == 1]), two_side_rows = sum(n[sides == 2]))
}) |> bind_rows()
write_csv(support, "../output/within_group_support.csv")
write_csv(bind_rows(results), "../output/model_checks.csv")
