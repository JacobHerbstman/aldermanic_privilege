# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/price_score_uncertainty/code")
# market <- "sales"
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# bin_width_ft <- 100
# controls <- "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor"
# fixed_effects <- "segment_id^year_quarter"
# cluster <- "ward_pair"
# fragile_thresholds <- c(0.75, 0.95)
# workers <- 4
library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 10L)
  market <- args[1]
  start_year <- as.integer(args[2])
  end_year <- as.integer(args[3])
  bandwidth_ft <- as.numeric(args[4])
  bin_width_ft <- as.numeric(args[5])
  controls <- args[6]
  fixed_effects <- args[7]
  cluster <- args[8]
  fragile_thresholds <- as.numeric(strsplit(args[9], ",", fixed = TRUE)[[1]])
  workers <- as.integer(args[10])
}
stopifnot(market %in% c("sales", "rent"), start_year <= end_year, end_year == 2022L,
  bandwidth_ft %% bin_width_ft == 0, all(fragile_thresholds > .5 & fragile_thresholds < 1), workers >= 1L)
setFixest_nthreads(1)
setDTthreads(1)

# Preserve each market's recorded sample, controls, property types, and time fixed effects.
if (market == "sales") {
  data <- as.data.table(arrow::read_parquet("../input/sales_with_hedonics_amenities.parquet"))
  stopifnot(!anyDuplicated(data$row_id))
  data[, `:=`(record_id = as.character(row_id), price = sale_price, year = as.integer(format(as.Date(sale_date), "%Y")),
    period = paste0(format(as.Date(sale_date), "%Y"), "-Q", (as.integer(format(as.Date(sale_date), "%m")) - 1L) %/% 3L + 1L),
    paper_distance = signed_dist_m / .3048, ward_pair = as.character(ward_pair_id), property_class_factor = factor(class))]
  data[, year_quarter := period]
}
if (market == "rent") {
  data <- as.data.table(arrow::read_parquet("../input/rental_rd_characteristics_panel_bw1500.parquet"))
  stopifnot(!anyDuplicated(data$rent_panel_id))
  data[, `:=`(record_id = as.character(rent_panel_id), price = rent_price, year = as.integer(format(as.Date(file_date), "%Y")),
    year_month = format(as.Date(file_date), "%Y-%m"), paper_distance = as.numeric(signed_dist), ward_pair = as.character(ward_pair_id),
    log_sqft = fifelse(is.finite(sqft) & sqft > 0, log(sqft), NA_real_), beds_factor = factor(beds),
    log_baths = fifelse(is.finite(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(fifelse(is.na(building_type_clean), "other", building_type_clean)))]
  data <- data[as.Date(assignment_date) >= as.Date("2003-05-01") & flag_clean_location_sample == TRUE & is.finite(beds) & beds >= 0]
}
control_names <- strsplit(controls, " + ", fixed = TRUE)[[1]]
numeric_controls <- setdiff(control_names, c("beds_factor", "building_type_factor", "property_class_factor"))
data <- data[year >= start_year & year <= end_year & is.finite(price) & price > 0 &
  is.finite(paper_distance) & abs(paper_distance) < bandwidth_ft & is.finite(strictness_own) &
  is.finite(strictness_neighbor) & strictness_own != strictness_neighbor &
  !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) & ward_pair != "" &
  is.finite(longitude) & is.finite(latitude) & Reduce(`&`, lapply(data[, ..numeric_controls], is.finite))]
# Keep only analysis columns before repeatedly fitting the large rental sample.
analysis_columns <- unique(c("record_id", "price", "paper_distance", "ward_pair", "segment_id", "alderman_own", "alderman_neighbor",
  control_names, all.vars(as.formula(paste("~", fixed_effects)))))
data <- data[, ..analysis_columns]
data[, `:=`(alderman_a = pmin(alderman_own, alderman_neighbor), alderman_b = pmax(alderman_own, alderman_neighbor))]
data[, pair := paste(alderman_a, alderman_b, sep = " / ")]
baseline_scores <- fread("../input/baseline_through2022.csv")
stopifnot(!anyDuplicated(baseline_scores$alderman))
data[, baseline_gap := baseline_scores$score[match(alderman_own, baseline_scores$alderman)] -
  baseline_scores$score[match(alderman_neighbor, baseline_scores$alderman)]]
stopifnot(all(is.finite(data$baseline_gap)), all(sign(data$baseline_gap) == sign(data$paper_distance)))
bin_edges <- seq(-bandwidth_ft, bandwidth_ft, bin_width_ft)
reference_bin <- bandwidth_ft / bin_width_ft
main_term <- paste0("distance_bin::", reference_bin + 1L)

# Reuse the paper's OLS specification for all draws, deletions, and reversals.
fit_price <- function(d, distance) {
  d <- copy(d)
  d[, distance_bin := cut(distance, bin_edges, labels = FALSE, include.lowest = TRUE, right = FALSE)]
  stopifnot(!anyNA(d$distance_bin))
  model <- feols(as.formula(sprintf("log(price) ~ i(distance_bin, ref = %d) + %s | %s", reference_bin, controls, fixed_effects)),
    d, cluster = as.formula(paste("~", cluster)), notes = FALSE)
  stopifnot(main_term %in% names(coef(model)))
  tab <- coeftable(model)[main_term, ]
  critical <- qt(.975, degrees_freedom(model, type = "t"))
  result <- data.table(market, estimate = unname(tab[1]), std_error = unname(tab[2]), p_value = unname(tab[4]),
    ci_low = unname(tab[1] - critical * tab[2]), ci_high = unname(tab[1] + critical * tab[2]),
    n = nobs(model), input_n = nrow(d), alderman_pairs = uniqueN(d$pair[obs(model)]), ward_pairs = uniqueN(d$ward_pair[obs(model)]))
  list(result = result, observations = obs(model))
}
baseline_fit <- fit_price(data, data$paper_distance)
baseline <- baseline_fit$result
data <- data[baseline_fit$observations]
reversed <- fit_price(data, -data$paper_distance)$result
stopifnot(abs(baseline$estimate + reversed$estimate) < 1e-6, abs(baseline$std_error - reversed$std_error) < 1e-6)
cat(market, "baseline:\n")
print(baseline)
pair_info <- data[, .(records = .N, wards = paste(sort(unique(ward_pair)), collapse = "; ")), by = .(pair, alderman_a, alderman_b)]
pair_info[, `:=`(market = market, baseline_gap = baseline_scores$score[match(alderman_a, baseline_scores$alderman)] -
  baseline_scores$score[match(alderman_b, baseline_scores$alderman)])]
pair_info[, baseline_stricter := fifelse(baseline_gap > 0, alderman_a, alderman_b)]
stability <- fread("../input/bootstrap_pairs.csv")[cutoff == end_year]
stopifnot(!anyDuplicated(stability[, .(method, pair)]))
for (method_name in c("property", "quarter", "year")) {
  s <- stability[method == method_name]
  pair_info[, paste0(method_name, "_retention") := s$retain_order[match(pair, s$pair)]]
}
stopifnot(!anyNA(pair_info))
SaveData(pair_info, c("market", "pair"), sprintf("../output/%s_pairs.csv", market))

# Apply the same fragile sets as earlier audits; retain all individual-pair results.
baseline[, `:=`(scenario = "baseline", method = "baseline", threshold = 0, pair = "all", selected_pairs = 0L, selected_records = 0L)]
selections <- list()
for (method_name in c("property", "quarter", "year")) for (threshold in fragile_thresholds) {
  selections[[length(selections) + 1L]] <- list(method = method_name, threshold = threshold, pair = "all",
    selected = pair_info[get(paste0(method_name, "_retention")) < threshold, pair])
}
for (selected_pair in pair_info$pair) selections[[length(selections) + 1L]] <- list(method = "individual", threshold = 0,
  pair = selected_pair, selected = selected_pair)
tests <- parallel::mclapply(selections, function(selection) {
  affected <- data$pair %in% selection$selected
  results <- list()
  for (action in c("drop", "flip")) {
    d <- if (action == "drop") data[!affected] else copy(data)
    distance <- d$paper_distance
    if (action == "flip") distance[d$pair %in% selection$selected] <- -distance[d$pair %in% selection$selected]
    fit <- fit_price(d, distance)
    if (action == "flip") stopifnot(identical(fit$observations, seq_len(nrow(data))))
    result <- fit$result
    result[, `:=`(scenario = paste0(action, if (selection$method == "individual") "_one" else "_fragile"),
      method = selection$method, threshold = selection$threshold, pair = selection$pair,
      selected_pairs = length(selection$selected), selected_records = sum(affected))]
    results[[action]] <- result
  }
  rbindlist(results)
}, mc.cores = workers, mc.set.seed = FALSE)
stopifnot(all(vapply(tests, is.data.table, logical(1))))
results <- rbindlist(c(list(baseline), tests))
results[, `:=`(percent_effect = 100 * expm1(estimate), percent_low = 100 * expm1(ci_low),
  percent_high = 100 * expm1(ci_high), change_log_coefficient = estimate - baseline$estimate)]
SaveData(results, c("market", "scenario", "method", "threshold", "pair"), sprintf("../output/%s_pair_sensitivity.csv", market))
cat(market, "all pair stress tests completed.\n")

# Use each complete citywide score vector on the same fitted observations.
scores <- as.data.table(arrow::read_parquet("../input/resamples_through2022.parquet"))
stopifnot(!anyDuplicated(scores[, .(method, draw, alderman)]))
draw_results <- list()
for (method_name in unique(scores$method)) {
  method_scores <- scores[method == method_name]
  draw_numbers <- sort(unique(method_scores$draw))
  for (first_draw in seq(1L, length(draw_numbers), by = 50L)) {
    draws <- draw_numbers[first_draw:min(first_draw + 49L, length(draw_numbers))]
    fits <- parallel::mclapply(draws, function(draw_number) {
      s <- method_scores[draw == draw_number]
      gap <- s$score[match(data$alderman_own, s$alderman)] - s$score[match(data$alderman_neighbor, s$alderman)]
      stopifnot(all(is.finite(gap)), all(gap != 0))
      fit <- fit_price(data, abs(data$paper_distance) * sign(gap))
      stopifnot(identical(fit$observations, seq_len(nrow(data))))
      result <- fit$result
      result[, `:=`(method = method_name, draw = draw_number, reversed_records = sum(sign(gap) != sign(data$baseline_gap)),
        reversed_pairs = uniqueN(data$pair[sign(gap) != sign(data$baseline_gap)]))]
      result
    }, mc.cores = workers, mc.set.seed = FALSE)
    stopifnot(all(vapply(fits, is.data.table, logical(1))))
    draw_results[[length(draw_results) + 1L]] <- rbindlist(fits)
    cat(market, method_name, "draws completed:", max(draws), "\n")
  }
}
SaveData(rbindlist(draw_results), c("market", "method", "draw"), sprintf("../output/%s_score_draws.csv", market))
