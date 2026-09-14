# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/developer_score_results/code")
# sector <- "developer_verified"
# start_year <- 2006
# end_year <- 2022
# rent_start_year <- 2014
# remap_year <- 2015
# bandwidth_ft <- 500
# bin_width_ft <- 100
# event_window <- 5
# density_controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
# density_fe <- "zone_group + segment_id + construction_year"
# rent_controls <- "log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor"
# rent_fe <- "segment_id^year_month"
# sales_controls <- "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor"
# sales_fe <- "segment_id^year_quarter"
# boundary_cluster <- "ward_pair"
# permit_fe <- "block_id + ward_pair_id^year"
# permit_cluster <- "ward_pair_id"
library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")

args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 17L)
  sector <- args[1]
  start_year <- as.integer(args[2])
  end_year <- as.integer(args[3])
  rent_start_year <- as.integer(args[4])
  remap_year <- as.integer(args[5])
  bandwidth_ft <- as.numeric(args[6])
  bin_width_ft <- as.numeric(args[7])
  event_window <- as.integer(args[8])
  density_controls <- args[9]
  density_fe <- args[10]
  rent_controls <- args[11]
  rent_fe <- args[12]
  sales_controls <- args[13]
  sales_fe <- args[14]
  boundary_cluster <- args[15]
  permit_fe <- args[16]
  permit_cluster <- args[17]
}
stopifnot(start_year < remap_year, remap_year <= end_year, bandwidth_ft %% bin_width_ft == 0)
setFixest_nthreads(1)

# The score is the eligible cash share from the previously documented developer definition.
# Keep observed zeros. No campaign receipts means a missing score, not zero support.
receipts <- as.data.table(arrow::read_parquet("../input/donation_receipts.parquet"))
receipts <- receipts[year >= start_year & year <= end_year & in_office %in% TRUE &
  strict_candidate == TRUE & own_committee_transfer == FALSE & receipt_type %in% c("1A", "2A")]
score_parts <- list()
for (period in c("main", "pre_remap")) {
  d <- receipts[year <= if (period == "main") end_year else remap_year - 1L]
  score_parts[[period]] <- d[, .(eligible_dollars = sum(amount),
    developer_dollars = sum(amount[get(sector)]), developer_share = sum(amount[get(sector)]) / sum(amount),
    receipt_count = .N, first_year = min(year), last_year = max(year)), by = .(alderman = full_name)][, period := period]
}
scores <- rbindlist(score_parts)
stopifnot(!anyDuplicated(scores[, .(period, alderman)]), all(is.finite(scores$developer_share)))
main_scores <- scores[period == "main"]
pre_scores <- scores[period == "pre_remap"]
rm(receipts)

# Reuse this extraction for the six boundary specifications and the permit models.
coefficient_rows <- function(model, terms) {
  tab <- coeftable(model)
  stopifnot(all(terms %in% rownames(tab)))
  df <- degrees_freedom(model, type = "t")
  critical <- qt(.975, df)
  data.table(term = terms, estimate = tab[terms, 1], std_error = tab[terms, 2],
    p_value = 2 * pt(-abs(tab[terms, 1] / tab[terms, 2]), df),
    ci_low = tab[terms, 1] - critical * tab[terms, 2], ci_high = tab[terms, 1] + critical * tab[terms, 2],
    n = nobs(model))
}
estimates <- list(); profiles <- list(); coverage <- list()
versions <- c("paper_full", "paper_common", "developer_common")
bin_edges <- seq(-bandwidth_ft, bandwidth_ft, bin_width_ft)
bin_labels <- sprintf("bin_%02d", seq_len(length(bin_edges) - 1L))
reference_bin <- bin_labels[bandwidth_ft / bin_width_ft]
main_term <- paste0("distance_bin::", bin_labels[bandwidth_ft / bin_width_ft + 1L])

# Preserve the paper's sample filters, controls, fixed effects and geographic assignments.
# Only the sign of distance changes under the developer ranking.
for (market in c("density", "rent", "sales")) {
  if (market == "density") {
    data <- fread("../input/new_construction_analysis_data.csv", colClasses = c(project_id = "character", ward_pair = "character", segment_id = "character"))
    stopifnot(!anyDuplicated(data$project_id))
    data[, paper_distance := signed_distance_m / .3048]
    data <- data[construction_year >= start_year & construction_year <= end_year & density_eligible == TRUE &
      is.finite(paper_distance) & abs(paper_distance) < bandwidth_ft &
      !is.na(zone_group) & !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) & ward_pair != ""]
    controls <- density_controls; fixed_effects <- density_fe
  }
  if (market == "rent") {
    data <- as.data.table(arrow::read_parquet("../input/rental_rd_characteristics_panel_bw1500.parquet"))
    stopifnot(!anyDuplicated(data$rent_panel_id))
    data[, `:=`(year = as.integer(format(as.Date(file_date), "%Y")), year_month = format(as.Date(file_date), "%Y-%m"),
      paper_distance = as.numeric(signed_dist), ward_pair = as.character(ward_pair_id),
      log_sqft = fifelse(is.finite(sqft) & sqft > 0, log(sqft), NA_real_), beds_factor = factor(beds),
      log_baths = fifelse(is.finite(baths) & baths > 0, log(baths), NA_real_),
      building_type_factor = factor(fifelse(is.na(building_type_clean), "other", building_type_clean)))]
    data <- data[year >= rent_start_year & year <= end_year & is.finite(rent_price) & rent_price > 0 &
      is.finite(paper_distance) & abs(paper_distance) < bandwidth_ft & is.finite(strictness_own) &
      is.finite(strictness_neighbor) & strictness_own != strictness_neighbor &
      !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) & ward_pair != "" &
      as.Date(assignment_date) >= as.Date("2003-05-01") & flag_clean_location_sample == TRUE &
      is.finite(longitude) & is.finite(latitude) & is.finite(beds) & beds >= 0]
    controls <- rent_controls; fixed_effects <- rent_fe
  }
  if (market == "sales") {
    data <- as.data.table(arrow::read_parquet("../input/sales_with_hedonics_amenities.parquet"))
    stopifnot(!anyDuplicated(data$row_id))
    data[, `:=`(year = as.integer(format(as.Date(sale_date), "%Y")),
      year_quarter = paste0(format(as.Date(sale_date), "%Y"), "-Q", (as.integer(format(as.Date(sale_date), "%m")) - 1L) %/% 3L + 1L),
      paper_distance = signed_dist_m / .3048, ward_pair = as.character(ward_pair_id), property_class_factor = factor(class))]
    data <- data[year >= start_year & year <= end_year & is.finite(sale_price) & sale_price > 0 &
      is.finite(paper_distance) & abs(paper_distance) < bandwidth_ft & is.finite(strictness_own) &
      is.finite(strictness_neighbor) & strictness_own != strictness_neighbor &
      !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) & ward_pair != "" &
      is.finite(longitude) & is.finite(latitude)]
    controls <- sales_controls; fixed_effects <- sales_fe
  }
  numeric_controls <- setdiff(strsplit(controls, " + ", fixed = TRUE)[[1]], c("beds_factor", "building_type_factor", "property_class_factor"))
  data <- data[Reduce(`&`, lapply(data[, ..numeric_controls], is.finite))]
  data[, `:=`(developer_own = main_scores$developer_share[match(alderman_own, main_scores$alderman)],
               developer_neighbor = main_scores$developer_share[match(alderman_neighbor, main_scores$alderman)])]
  data[, missing_developer := !is.finite(developer_own) | !is.finite(developer_neighbor)]
  data[, developer_tie := !missing_developer & developer_own == developer_neighbor]
  data[, developer_sign := sign(developer_own - developer_neighbor)]
  coverage[[market]] <- data[, .(market, input_rows = .N, missing_score_rows = sum(missing_developer),
    tied_score_rows = sum(developer_tie), common_rows = sum(!missing_developer & !developer_tie),
    common_ordered_rows = sum(!missing_developer & !developer_tie),
    reversed_rows = sum(!missing_developer & !developer_tie & developer_sign != sign(paper_distance), na.rm = TRUE))]
  specs <- if (market == "density") CJ(sample = c("all", "multifamily"), outcome = c("density_far", "density_dupac")) else
    data.table(sample = "all", outcome = if (market == "rent") "rent_price" else "sale_price")
  for (j in seq_len(nrow(specs))) for (version in versions) {
    sample_name <- specs$sample[j]; outcome <- specs$outcome[j]
    d <- copy(data)
    if (market == "density" && sample_name == "multifamily") d <- d[external_multifamily == TRUE]
    if (version != "paper_full") d <- d[missing_developer == FALSE & developer_tie == FALSE]
    d[, running_distance := if (version == "developer_common") abs(paper_distance) * developer_sign else paper_distance]
    d[, distance_bin := cut(running_distance, bin_edges, bin_labels, include.lowest = TRUE, right = FALSE)]
    stopifnot(all(is.finite(d[[outcome]]) & d[[outcome]] > 0), !anyNA(d$distance_bin))
    model <- feols(as.formula(sprintf("log(%s) ~ i(distance_bin, ref = '%s') + %s | %s", outcome, reference_bin, controls, fixed_effects)),
      data = d, cluster = as.formula(paste("~", boundary_cluster)), notes = FALSE)
    if (version == "paper_common") common_obs <- obs(model)
    if (version == "developer_common") stopifnot(identical(obs(model), common_obs))
    result <- coefficient_rows(model, main_term)
    result[, `:=`(market = market, sample = sample_name, outcome = outcome, version = version, specification = "boundary_100ft")]
    estimates[[length(estimates) + 1L]] <- result
    curve <- coefficient_rows(model, grep("^distance_bin::", names(coef(model)), value = TRUE))
    curve[, x := (as.integer(sub("distance_bin::bin_", "", term)) - .5) * bin_width_ft - bandwidth_ft]
    curve <- rbind(curve, data.table(term = paste0("distance_bin::", reference_bin), estimate = 0, std_error = 0,
      p_value = NA_real_, ci_low = 0, ci_high = 0, n = nobs(model), x = -bin_width_ft / 2))
    curve[, `:=`(market = market, sample = sample_name, outcome = outcome, version = version)]
    profiles[[length(profiles) + 1L]] <- curve
  }
  rm(data)
}

# For the remap, require both boundary wards to have distinct pre-2015 developer scores.
# Drop tied/missing pairs together with their controls; do not relabel tied switches as controls.
data <- as.data.table(arrow::read_parquet("../input/permit_block_year_panel_2015.parquet"))
stopifnot(all(data$year - data$relative_year == remap_year))
ward_names <- unique(rbind(data[, .(ward = ward_origin, alderman = alderman_origin_2014)],
                          data[, .(ward = ward_dest, alderman = alderman_dest_2014)]))
ward_names <- ward_names[!is.na(ward) & !is.na(alderman)]
stopifnot(!anyDuplicated(ward_names$ward))
ward_names[, developer_share := pre_scores$developer_share[match(alderman, pre_scores$alderman)]]
data <- data[dist_m <= bandwidth_ft * .3048 & relative_year >= -event_window & relative_year <= event_window &
  !is.na(strictness_change_frozen) & !is.na(ward_pair_id) & ward_pair_id != "" & stable_both == TRUE]
stopifnot(!anyDuplicated(data[, .(block_id, year)]))
data[, pre_volume := sum(n_high_discretion_application[relative_year < 0]), by = block_id]
data <- data[pre_volume > 0]
data[, `:=`(outcome = n_high_discretion_application, post = as.integer(relative_year >= 0),
  paper_sign = sign(strictness_change_frozen),
  developer_origin = pre_scores$developer_share[match(alderman_origin_2014, pre_scores$alderman)],
  developer_dest = pre_scores$developer_share[match(alderman_dest_2014, pre_scores$alderman)])]
pairs <- unique(data[, .(ward_pair_id)])
pairs[, c("ward_a", "ward_b") := tstrsplit(ward_pair_id, "-", fixed = TRUE)]
pairs[, `:=`(score_a = ward_names$developer_share[match(as.numeric(ward_a), ward_names$ward)],
               score_b = ward_names$developer_share[match(as.numeric(ward_b), ward_names$ward)])]
pairs[, status := fcase(!is.finite(score_a) | !is.finite(score_b), "missing", score_a == score_b, "tie", default = "common")]
data[, pair_status := pairs$status[match(ward_pair_id, pairs$ward_pair_id)]]
data[, developer_sign := sign(developer_dest - developer_origin)]
stopifnot(!anyNA(data$pair_status), all(data[pair_status == "common", is.finite(developer_sign)]))
coverage[["permits"]] <- unique(data[, .(block_id, pair_status, paper_sign, developer_sign)])[,
  .(market = "permits", input_rows = .N, missing_score_rows = sum(pair_status == "missing"),
    tied_score_rows = sum(pair_status == "tie"), common_rows = sum(pair_status == "common"),
    common_ordered_rows = sum(pair_status == "common" & paper_sign != 0),
    reversed_rows = sum(pair_status == "common" & developer_sign != paper_sign, na.rm = TRUE))]
common_permit_obs <- list()
for (version in versions) {
  d <- copy(data)
  if (version != "paper_full") d <- d[pair_status == "common"]
  d[, direction := if (version == "developer_common") developer_sign else paper_sign]
  d[, `:=`(post_signed = post * direction, post_higher = post * as.integer(direction > 0), post_lower = post * as.integer(direction < 0))]
  for (specification in c("signed", "separate")) {
    rhs <- if (specification == "signed") "post_signed" else "post_higher + post_lower"
    model <- fepois(as.formula(sprintf("outcome ~ %s | %s", rhs, permit_fe)), data = d,
      cluster = as.formula(paste("~", permit_cluster)), notes = FALSE)
    if (version == "paper_common") common_permit_obs[[specification]] <- obs(model)
    if (version == "developer_common") stopifnot(identical(obs(model), common_permit_obs[[specification]]))
    result <- coefficient_rows(model, names(coef(model)))
    result[, `:=`(market = "permits", sample = "stable", outcome = "high_discretion", version = version, specification = specification)]
    estimates[[length(estimates) + 1L]] <- result
  }
  event <- fepois(as.formula(sprintf("outcome ~ i(relative_year, direction, ref = -1) | %s", permit_fe)),
    data = d, cluster = as.formula(paste("~", permit_cluster)), notes = FALSE)
  if (version == "paper_common") common_event_obs <- obs(event)
  if (version == "developer_common") stopifnot(identical(obs(event), common_event_obs))
  curve <- coefficient_rows(event, names(coef(event)))
  curve[, x := as.integer(sub(":direction", "", sub("relative_year::", "", term)))]
  curve <- rbind(curve, data.table(term = "reference", estimate = 0, std_error = 0, p_value = NA_real_, ci_low = 0, ci_high = 0, n = nobs(event), x = -1))
  curve[, `:=`(market = "permits", sample = "stable", outcome = "high_discretion", version = version)]
  pre_terms <- paste0("relative_year::", -event_window:-2, ":direction")
  pre_beta <- coef(event)[pre_terms]
  pre_vcov <- vcov(event)[pre_terms, pre_terms]
  pre_f <- drop(t(pre_beta) %*% solve(pre_vcov, pre_beta)) / length(pre_terms)
  curve[, pretrend_p_value := pf(pre_f, length(pre_terms), degrees_freedom(event, type = "t"), lower.tail = FALSE)]
  profiles[[length(profiles) + 1L]] <- curve
}
results <- rbindlist(estimates)
results[, `:=`(percent_effect = 100 * expm1(estimate), percent_low = 100 * expm1(ci_low), percent_high = 100 * expm1(ci_high))]
SaveData(scores, c("period", "alderman"), "../output/developer_scores.csv")
SaveData(results, c("market", "sample", "outcome", "version", "specification", "term"), "../output/developer_estimates.csv")
SaveData(rbindlist(profiles, fill = TRUE), c("market", "sample", "outcome", "version", "term"), "../output/developer_profiles.csv")
SaveData(rbindlist(coverage), "market", "../output/developer_coverage.csv")
