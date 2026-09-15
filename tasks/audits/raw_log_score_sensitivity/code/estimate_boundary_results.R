# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/raw_log_score_sensitivity/code")
# analysis_name <- "density_all"
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# bin_width_ft <- 100
# controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
# fixed_effects <- "zone_group + segment_id + construction_year"
# cluster <- "ward_pair"
library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 8L)
  analysis_name <- args[1]
  start_year <- as.integer(args[2])
  end_year <- as.integer(args[3])
  bandwidth_ft <- as.numeric(args[4])
  bin_width_ft <- as.numeric(args[5])
  controls <- args[6]
  fixed_effects <- args[7]
  cluster <- args[8]
}
stopifnot(analysis_name %in% c("density_all", "density_multifamily", "sales", "rent"),
  start_year <= end_year, end_year == 2022L, bandwidth_ft %% bin_width_ft == 0)
setFixest_nthreads(1)
setDTthreads(1)

# Reconstruct each existing outcome sample from its preserved source.
if (analysis_name %in% c("density_all", "density_multifamily")) {
  data <- fread("../input/new_construction_analysis_data.csv", colClasses = c(project_id = "character", ward_pair = "character", segment_id = "character"))
  stopifnot(!anyDuplicated(data$project_id))
  data[, `:=`(record_id = project_id, year = construction_year, paper_distance = signed_distance_m / .3048)]
  data <- data[density_eligible == TRUE & !is.na(zone_group)]
  if (analysis_name == "density_multifamily") data <- data[external_multifamily == TRUE]
  stopifnot(all(is.finite(data$density_far) & data$density_far > 0),
    all(is.finite(data$density_dupac) & data$density_dupac > 0))
  outcomes <- c("density_far", "density_dupac")
}
if (analysis_name == "sales") {
  data <- as.data.table(arrow::read_parquet("../input/sales_with_hedonics_amenities.parquet"))
  stopifnot(!anyDuplicated(data$row_id))
  data[, `:=`(record_id = as.character(row_id), year = as.integer(format(as.Date(sale_date), "%Y")),
    year_quarter = paste0(format(as.Date(sale_date), "%Y"), "-Q", (as.integer(format(as.Date(sale_date), "%m")) - 1L) %/% 3L + 1L),
    paper_distance = signed_dist_m / .3048, ward_pair = as.character(ward_pair_id), property_class_factor = factor(class))]
  data <- data[is.finite(sale_price) & sale_price > 0 & is.finite(longitude) & is.finite(latitude)]
  outcomes <- "sale_price"
}
if (analysis_name == "rent") {
  data <- as.data.table(arrow::read_parquet("../input/rental_rd_characteristics_panel_bw1500.parquet"))
  stopifnot(!anyDuplicated(data$rent_panel_id))
  data[, `:=`(record_id = as.character(rent_panel_id), year = as.integer(format(as.Date(file_date), "%Y")),
    year_month = format(as.Date(file_date), "%Y-%m"), paper_distance = as.numeric(signed_dist), ward_pair = as.character(ward_pair_id),
    log_sqft = fifelse(is.finite(sqft) & sqft > 0, log(sqft), NA_real_), beds_factor = factor(beds),
    log_baths = fifelse(is.finite(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(fifelse(is.na(building_type_clean), "other", building_type_clean)))]
  data <- data[as.Date(assignment_date) >= as.Date("2003-05-01") & flag_clean_location_sample == TRUE &
    is.finite(beds) & beds >= 0 & is.finite(rent_price) & rent_price > 0 & is.finite(longitude) & is.finite(latitude)]
  outcomes <- "rent_price"
}
control_names <- strsplit(controls, " + ", fixed = TRUE)[[1]]
numeric_controls <- setdiff(control_names, c("beds_factor", "building_type_factor", "property_class_factor"))
data <- data[year >= start_year & year <= end_year & is.finite(paper_distance) & abs(paper_distance) < bandwidth_ft &
  is.finite(strictness_own) & is.finite(strictness_neighbor) & strictness_own != strictness_neighbor &
  !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) & ward_pair != "" &
  Reduce(`&`, lapply(data[, ..numeric_controls], is.finite))]
data[, `:=`(alderman_a = pmin(alderman_own, alderman_neighbor), alderman_b = pmax(alderman_own, alderman_neighbor))]
data[, pair := paste(alderman_a, alderman_b, sep = " / ")]
raw <- fread("../input/raw_processing_pairs.csv")[market == analysis_name & cutoff == end_year & measure == "mean_log_days"]
stopifnot(!anyDuplicated(raw$pair), nrow(raw) > 0L, !any(raw$agreement == "tie"))
index <- match(data$pair, raw$pair)
stopifnot(!anyNA(index))
data[, `:=`(disagree = raw$agreement[index] == "reverse",
  raw_direction = sign(raw$raw_a[index] - raw$raw_b[index]) * fifelse(alderman_own == alderman_a, 1, -1),
  score_direction = sign(raw$score_a[index] - raw$score_b[index]) * fifelse(alderman_own == alderman_a, 1, -1))]
stopifnot(all(data$score_direction == sign(data$paper_distance)),
  all(data$disagree == (data$raw_direction != data$score_direction)))
analysis_columns <- unique(c("record_id", "pair", "paper_distance", "disagree", "raw_direction", "score_direction", "ward_pair",
  outcomes, control_names, all.vars(as.formula(paste("~", fixed_effects)))))
data <- data[, ..analysis_columns]
bin_edges <- seq(-bandwidth_ft, bandwidth_ft, bin_width_ft)
reference_bin <- bandwidth_ft / bin_width_ft
main_term <- paste0("distance_bin::", reference_bin + 1L)

# Fit the same distance-bin regression for all three comparisons and both density measures.
fit_boundary <- function(d, distance) {
  d <- copy(d)
  d[, distance_bin := cut(distance, bin_edges, labels = FALSE, include.lowest = TRUE, right = FALSE)]
  stopifnot(!anyNA(d$distance_bin))
  results <- list()
  fitted_rows <- NULL
  for (outcome in outcomes) {
    model <- feols(as.formula(sprintf("log(%s) ~ i(distance_bin, ref = %d) + %s | %s",
      outcome, reference_bin, controls, fixed_effects)), d, cluster = as.formula(paste("~", cluster)), notes = FALSE)
    stopifnot(main_term %in% names(coef(model)))
    if (is.null(fitted_rows)) fitted_rows <- obs(model) else stopifnot(identical(fitted_rows, obs(model)))
    tab <- coeftable(model)[main_term, ]
    critical <- qt(.975, degrees_freedom(model, type = "t"))
    results[[outcome]] <- data.table(analysis = analysis_name, outcome, estimate = unname(tab[1]), std_error = unname(tab[2]),
      p_value = unname(tab[4]), ci_low = unname(tab[1] - critical * tab[2]), ci_high = unname(tab[1] + critical * tab[2]),
      input_n = nrow(d), n = nobs(model), alderman_pairs = uniqueN(d$pair[fitted_rows]), ward_pairs = uniqueN(d[[cluster]][fitted_rows]))
  }
  list(results = rbindlist(results), observations = fitted_rows)
}
fit <- fit_boundary(data, data$paper_distance)
baseline <- fit$results
data <- data[fit$observations]
counts <- data[, .(observations = .N), by = pair]
stopifnot(setequal(counts$pair, raw$pair), all(counts$observations == raw$observations[match(counts$pair, raw$pair)]))
reversed <- fit_boundary(data, -data$paper_distance)$results
stopifnot(max(abs(baseline$estimate + reversed$estimate)) < 1e-6,
  max(abs(baseline$std_error - reversed$std_error)) < 1e-6)

baseline[, `:=`(scenario = "baseline", selected_pairs = 0L, selected_observations = 0L)]
results <- list(baseline)
for (action in c("drop_disagreements", "reverse_disagreements")) {
  d <- if (action == "drop_disagreements") data[disagree == FALSE] else copy(data)
  distance <- d$paper_distance
  if (action == "reverse_disagreements") {
    distance[d$disagree] <- -distance[d$disagree]
    stopifnot(all(sign(distance) == d$raw_direction))
  }
  fit <- fit_boundary(d, distance)
  if (action == "reverse_disagreements") stopifnot(identical(fit$observations, seq_len(nrow(data))))
  result <- fit$results
  result[, `:=`(scenario = action, selected_pairs = uniqueN(data$pair[data$disagree]), selected_observations = sum(data$disagree))]
  results[[action]] <- result
}
results <- rbindlist(results)
results[, `:=`(percent_effect = 100 * expm1(estimate), percent_low = 100 * expm1(ci_low), percent_high = 100 * expm1(ci_high))]
SaveData(results, c("analysis", "outcome", "scenario"), sprintf("../output/%s_results.csv", analysis_name))
print(results[, .(analysis, outcome, scenario, n, percent_effect, p_value)])
