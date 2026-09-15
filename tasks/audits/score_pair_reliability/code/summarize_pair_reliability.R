# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/score_pair_reliability/code")
# bandwidth_ft <- 500
# bin_width_ft <- 100
# minimum_permits <- 30
# minimum_months <- 6
# density_controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
# rent_controls <- "log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor"
# sales_controls <- "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor"
# density_fe <- "zone_group + segment_id + construction_year"
# rent_fe <- "segment_id^year_month"
# sales_fe <- "segment_id^year_quarter"
# start_year <- 2006
# end_year <- 2022
# rent_start_year <- 2014
# remap_year <- 2015
# event_window <- 5

library(data.table)
library(fixest)
library(ggplot2)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 15L)
  bandwidth_ft <- as.numeric(args[1]); bin_width_ft <- as.numeric(args[2])
  minimum_permits <- as.integer(args[3]); minimum_months <- as.integer(args[4])
  density_controls <- args[5]; rent_controls <- args[6]; sales_controls <- args[7]
  density_fe <- args[8]; rent_fe <- args[9]; sales_fe <- args[10]
  start_year <- as.integer(args[11]); end_year <- as.integer(args[12])
  rent_start_year <- as.integer(args[13]); remap_year <- as.integer(args[14]); event_window <- as.integer(args[15])
}
setFixest_nthreads(1)
setDTthreads(1)

# Use the fitted observations of the paper's current boundary specifications.
# Counts describe representation in the sample; they are not regression leverage weights.
pair_parts <- list()
sample_counts <- list()
for (market in c("density", "rent", "sales")) {
  if (market == "density") {
    d <- fread("../input/new_construction_analysis_data.csv")
    stopifnot(!anyDuplicated(d$project_id))
    d[, running_distance := signed_distance_m / .3048]
    d <- d[construction_year >= start_year & construction_year <= end_year & density_eligible == TRUE]
    controls <- density_controls; fixed_effects <- density_fe; outcome <- "density_far"
  }
  if (market == "rent") {
    d <- as.data.table(arrow::read_parquet("../input/rental_rd_characteristics_panel_bw1500.parquet"))
    stopifnot(!anyDuplicated(d$rent_panel_id))
    d[, `:=`(year = as.integer(format(as.Date(file_date), "%Y")), year_month = format(as.Date(file_date), "%Y-%m"),
      running_distance = signed_dist, ward_pair = as.character(ward_pair_id),
      log_sqft = fifelse(is.finite(sqft) & sqft > 0, log(sqft), NA_real_), beds_factor = factor(beds),
      log_baths = fifelse(is.finite(baths) & baths > 0, log(baths), NA_real_),
      building_type_factor = factor(fifelse(is.na(building_type_clean), "other", building_type_clean)))]
    d <- d[year >= rent_start_year & year <= end_year & as.Date(assignment_date) >= as.Date("2003-05-01") &
      flag_clean_location_sample == TRUE & is.finite(longitude) & is.finite(latitude) & is.finite(beds) & beds >= 0]
    controls <- rent_controls; fixed_effects <- rent_fe; outcome <- "rent_price"
  }
  if (market == "sales") {
    d <- as.data.table(arrow::read_parquet("../input/sales_with_hedonics_amenities.parquet"))
    stopifnot(!anyDuplicated(d$row_id))
    d[, `:=`(year = as.integer(format(as.Date(sale_date), "%Y")),
      year_quarter = paste0(format(as.Date(sale_date), "%Y"), "-Q", (as.integer(format(as.Date(sale_date), "%m")) - 1L) %/% 3L + 1L),
      running_distance = signed_dist_m / .3048, ward_pair = as.character(ward_pair_id), property_class_factor = factor(class))]
    d <- d[year >= start_year & year <= end_year & is.finite(longitude) & is.finite(latitude)]
    controls <- sales_controls; fixed_effects <- sales_fe; outcome <- "sale_price"
  }
  numeric_controls <- setdiff(strsplit(controls, " + ", fixed = TRUE)[[1]],
    c("beds_factor", "building_type_factor", "property_class_factor"))
  d <- d[is.finite(running_distance) & abs(running_distance) < bandwidth_ft &
    is.finite(strictness_own) & is.finite(strictness_neighbor) & strictness_own != strictness_neighbor &
    !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) & ward_pair != "" &
    is.finite(get(outcome)) & get(outcome) > 0 & Reduce(`&`, lapply(d[, ..numeric_controls], is.finite))]
  if (market == "density") d <- d[!is.na(zone_group)]
  d[, distance_bin := cut(running_distance, seq(-bandwidth_ft, bandwidth_ft, bin_width_ft),
    labels = FALSE, include.lowest = TRUE, right = FALSE)]
  for (sample_name in if (market == "density") c("all", "multifamily") else "all") {
    s <- if (sample_name == "multifamily") d[external_multifamily == TRUE] else d
    model <- feols(as.formula(sprintf("log(%s) ~ i(distance_bin, ref = %d) + %s | %s", outcome,
      bandwidth_ft / bin_width_ft, controls, fixed_effects)), s, cluster = ~ward_pair, notes = FALSE)
    s <- s[obs(model)]
    s[, `:=`(alderman_a = pmin(alderman_own, alderman_neighbor), alderman_b = pmax(alderman_own, alderman_neighbor))]
    pair_parts[[length(pair_parts) + 1L]] <- s[, .(observations = .N), by = .(alderman_a, alderman_b)][,
      `:=`(market = if (market == "density") paste0("density_", sample_name) else market, cutoff = end_year)]
    sample_counts[[length(sample_counts) + 1L]] <- data.table(market, sample = sample_name, n = nobs(model))
  }
}
rm(d, s, model)

# The remap's informative comparisons are reassigned blocks; unchanged blocks have no ordering.
d <- as.data.table(arrow::read_parquet("../input/permit_block_year_panel_2015.parquet"))
d <- d[dist_m <= bandwidth_ft * .3048 & relative_year >= -event_window & relative_year <= event_window &
  !is.na(strictness_change_frozen) & !is.na(ward_pair_id) & ward_pair_id != "" & stable_both == TRUE]
stopifnot(!anyDuplicated(d[, .(block_id, year)]), all(d$year - d$relative_year == remap_year))
d[, pre_volume := sum(n_high_discretion_application[relative_year < 0]), by = block_id]
d <- d[pre_volume > 0]
d[, post_signed := as.integer(relative_year >= 0) * sign(strictness_change_frozen)]
model <- fepois(n_high_discretion_application ~ post_signed | block_id + ward_pair_id^year,
  d, cluster = ~ward_pair_id, notes = FALSE)
d <- unique(d[obs(model)][strictness_change_frozen != 0,
  .(block_id, alderman_a = pmin(alderman_origin_2014, alderman_dest_2014),
    alderman_b = pmax(alderman_origin_2014, alderman_dest_2014))])
pair_parts[[length(pair_parts) + 1L]] <- d[, .(observations = .N), by = .(alderman_a, alderman_b)][,
  `:=`(market = "permit_remap", cutoff = remap_year - 1L)]
pairs <- rbindlist(pair_parts)
stopifnot(!anyNA(pairs), !anyDuplicated(pairs[, .(market, cutoff, alderman_a, alderman_b)]))
pairs[, pair := paste(alderman_a, alderman_b, sep = " / ")]
SaveData(pairs, c("market", "cutoff", "alderman_a", "alderman_b"), "../output/comparison_pairs.csv")
SaveData(rbindlist(sample_counts), c("market", "sample"), "../output/fitted_samples.csv")

bootstrap_pairs <- list(); split_pairs <- list(); split_summary <- list(); rank_summary <- list()
for (cutoff_year in c(2014L, 2022L)) {
  baseline <- fread(sprintf("../output/baseline_through%d.csv", cutoff_year))
  p <- unique(pairs[cutoff == cutoff_year, .(pair, alderman_a, alderman_b)])
  p[, baseline_gap := baseline$score[match(alderman_a, baseline$alderman)] - baseline$score[match(alderman_b, baseline$alderman)]]
  stopifnot(all(is.finite(p$baseline_gap)), all(p$baseline_gap != 0))
  b <- as.data.table(arrow::read_parquet(sprintf("../output/resamples_through%d.parquet", cutoff_year)))
  for (method_name in unique(b$method)) {
    method_draws <- b[method == method_name]
    gaps <- lapply(sort(unique(method_draws$draw)), function(draw_number) {
      z <- method_draws[draw == draw_number]
      stopifnot(!anyDuplicated(z$alderman))
      (z$score[match(p$alderman_a, z$alderman)] - z$score[match(p$alderman_b, z$alderman)]) * sign(p$baseline_gap)
    })
    gaps <- do.call(cbind, gaps)
    stopifnot(all(is.finite(gaps)))
    q <- copy(p)
    q[, `:=`(method = method_name, cutoff = cutoff_year, draws = ncol(gaps),
      retain_order = rowMeans(gaps > 0), tie_share = rowMeans(gaps == 0),
      gap_low = apply(gaps, 1, quantile, .025), gap_high = apply(gaps, 1, quantile, .975),
      clusters = unique(method_draws$cluster_count))]
    q[, monte_carlo_se := sqrt(retain_order * (1 - retain_order) / draws)]
    bootstrap_pairs[[length(bootstrap_pairs) + 1L]] <- q
  }
  halves <- as.data.table(arrow::read_parquet(sprintf("../output/splits_through%d.parquet", cutoff_year)))
  for (method_name in unique(halves$method)) for (draw_number in sort(unique(halves[method == method_name, draw]))) {
    a <- halves[method == method_name & draw == draw_number & half == 1 & n_permits >= minimum_permits & months >= minimum_months]
    b <- halves[method == method_name & draw == draw_number & half == 2 & n_permits >= minimum_permits & months >= minimum_months]
    common <- merge(a[, .(alderman, score_a = score)], b[, .(alderman, score_b = score)], by = "alderman")
    stopifnot(!anyDuplicated(a$alderman), !anyDuplicated(b$alderman), !anyDuplicated(common$alderman))
    rank_summary[[length(rank_summary) + 1L]] <- common[, .(cutoff = cutoff_year, method = method_name,
      draw = draw_number, aldermen = .N, pearson = cor(score_a, score_b), spearman = cor(score_a, score_b, method = "spearman"))]
    q <- copy(p)
    q[, `:=`(gap_a = a$score[match(alderman_a, a$alderman)] - a$score[match(alderman_b, a$alderman)],
      gap_b = b$score[match(alderman_a, b$alderman)] - b$score[match(alderman_b, b$alderman)])]
    q[, `:=`(usable = is.finite(gap_a) & is.finite(gap_b), cutoff = cutoff_year, method = method_name, draw = draw_number)]
    q[, agree := usable & gap_a * gap_b > 0]
    split_pairs[[length(split_pairs) + 1L]] <- q
    for (market_name in pairs[cutoff == cutoff_year, unique(market)]) {
      weights <- pairs[cutoff == cutoff_year & market == market_name]
      z <- q[match(weights$pair, pair)]
      weights <- weights$observations
      split_summary[[length(split_summary) + 1L]] <- data.table(cutoff = cutoff_year, market = market_name,
        method = method_name, draw = draw_number, total_pairs = nrow(z), usable_pairs = sum(z$usable),
        total_observations = sum(weights), usable_observations = sum(weights[z$usable]),
        pair_agreement = if (any(z$usable)) mean(z$agree[z$usable]) else NA_real_,
        observation_agreement = if (any(z$usable)) weighted.mean(z$agree[z$usable], weights[z$usable]) else NA_real_)
    }
  }
}
bootstrap <- rbindlist(bootstrap_pairs)
split_details <- rbindlist(split_pairs)
splits <- rbindlist(split_summary)
ranks <- rbindlist(rank_summary)
SaveData(bootstrap, c("cutoff", "method", "pair"), "../output/bootstrap_pairs.csv")
SaveData(split_details, c("cutoff", "method", "draw", "pair"), "../output/split_pairs.parquet")
SaveData(splits, c("cutoff", "market", "method", "draw"), "../output/split_summary.csv")
SaveData(ranks, c("cutoff", "method", "draw"), "../output/split_rank_correlations.csv")

boot_summary <- list()
for (market_name in unique(pairs$market)) for (method_name in unique(bootstrap$method)) {
  p <- pairs[market == market_name]
  z <- bootstrap[cutoff == unique(p$cutoff) & method == method_name]
  z <- z[match(p$pair, pair)]
  stopifnot(!anyNA(z$retain_order))
  boot_summary[[length(boot_summary) + 1L]] <- data.table(market = market_name, method = method_name,
    pairs = nrow(z), observations = sum(p$observations),
    mean_retention = weighted.mean(z$retain_order, p$observations),
    share_pairs_above95 = mean(z$retain_order >= .95),
    share_observations_above95 = weighted.mean(z$retain_order >= .95, p$observations),
    share_observations_interval_above0 = weighted.mean(z$gap_low > 0, p$observations))
}
boot_summary <- rbindlist(boot_summary)
SaveData(boot_summary, c("market", "method"), "../output/bootstrap_summary.csv")

labels <- c(density_all = "All construction", density_multifamily = "Multifamily construction",
  rent = "Rents", sales = "Sale prices", permit_remap = "Permit remap")
split_labels <- c(random_properties = "Random properties", random_quarters = "Random quarters within years",
  odd_even_years = "Odd vs even years", early_late = "Early vs late years")
summary <- splits[, .(runs = .N, pairs = as.numeric(median(usable_pairs)), total_pairs = first(total_pairs),
  coverage = median(usable_observations / total_observations),
  agreement = median(observation_agreement), low = min(observation_agreement), high = max(observation_agreement)), by = .(market, method)]
summary[, `:=`(comparison = labels[market], split = split_labels[method])]
plot <- ggplot(summary, aes(100 * agreement, factor(split, levels = rev(unname(split_labels))))) +
  geom_vline(xintercept = 50, color = "grey70", linetype = "dashed") +
  geom_linerange(aes(xmin = 100 * low, xmax = 100 * high), color = "#517f99", linewidth = 1) +
  geom_point(color = "#164d69", size = 2.5) + facet_wrap(~comparison, ncol = 2) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(x = "Observations whose alderman pair has the same ordering in both halves (%)", y = NULL,
    title = "Does the local ordering repeat in separate permit samples?",
    subtitle = "Median and full range across random splits; agreement is conditional on both aldermen having enough data.",
    caption = sprintf("Each half requires %d permits and %d observed months per alderman.\nCounts weight buildings, transactions, or reassigned blocks; they are not regression weights.", minimum_permits, minimum_months)) +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 8))
ggsave("../output/split_agreement.pdf", plot, width = 11, height = 8, bg = "white")
ggsave("../output/split_agreement.png", plot, width = 11, height = 8, dpi = 140, bg = "white")

# Keep the full tables available, including unfavorable splits and poorly supported pairs.
html_table <- function(d) {
  escape <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
  paste0("<table><thead><tr>", paste0("<th>", escape(names(d)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(apply(as.data.frame(d), 1, function(row)
      paste0("<tr>", paste0("<td>", escape(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
}
display_boot <- boot_summary[, .(Comparison = labels[market], Reweight = method,
  `Mean order retention (%)` = round(100 * mean_retention, 1),
  `Observations with ≥95% retention (%)` = round(100 * share_observations_above95, 1))]
display_split <- summary[, .(Comparison = comparison, Split = split, Runs = runs,
  `Usable pairs (median)` = paste(pairs, "of", total_pairs), `Observation coverage (%)` = round(100 * coverage, 1),
  `Same ordering (%)` = round(100 * agreement, 1),
  `Across-run range (%)` = sprintf("%.1f–%.1f", 100 * low, 100 * high))]
display_rank <- ranks[, .(Aldermen = as.numeric(median(aldermen)), `Median rank correlation` = round(median(spearman), 3)), by = .(cutoff, method)]
detail <- bootstrap[method == "year"][order(retain_order), .(Period = cutoff, Pair = pair,
  `Baseline gap (SD)` = round(abs(baseline_gap), 3), `Annual order retention (%)` = round(100 * retain_order, 1),
  `95% draw interval` = sprintf("%.2f to %.2f", gap_low, gap_high))]
writeLines(paste0('<!doctype html><meta charset="utf-8"><title>Local score reliability</title><style>body{font:16px/1.5 system-ui;max-width:1200px;margin:35px auto;padding:0 20px;color:#183445}h1,h2{line-height:1.2}table{border-collapse:collapse;width:100%;font-size:14px;margin:20px 0}th,td{text-align:left;border-bottom:1px solid #d9e2e7;padding:8px}th{background:#edf3f6}img{width:100%}a{color:#176184}</style>',
  '<h1>How reliable are the local alderman comparisons?</h1><p>For multifamily construction, the two separate property samples agree on which alderman is stricter for <strong>',
  round(100 * summary[market == "density_multifamily" & method == "random_properties", agreement], 1),
  '% of buildings</strong>. Separate calendar-quarter samples agree for ',
  round(100 * summary[market == "density_multifamily" & method == "random_quarters", agreement], 1),
  '%. These are medians across the random splits, with sample coverage shown below.</p>',
  '<p>The submitted two-stage score is held fixed as the method. Every resample refits neighborhood controls, alderman effects, and shrinkage. The all-one-weight fit reproduces both published scores to within 10⁻⁸.</p>',
  '<h2>Reweight the permits and re-estimate the score</h2><p>', unique(bootstrap$draws), ' positive-weight draws for each method and score period. Properties share weights across their recorded permits. Calendar quarters and years share weights across the whole city. Retention is the fraction of draws retaining the baseline order, not the probability that the order is correct. Annual results have only 17 time blocks through 2022 and nine through 2014. These are exploratory, pointwise stability summaries, not simultaneous confidence statements or the combined score-and-outcome bootstrap.</p>',
  html_table(display_boot), '<h2>Separate permit samples</h2><p>Both halves fit their own adjustment coefficients and shrinkage. Observed workload is held fixed; outcomes and residuals are never carried from one half to the other. Recorded parcels stay together in property splits. Permits without a valid PIN are separate singleton groups, so shared projects without recorded parcel links may remain in both halves. Disjoint observations may still share shocks; these splits do not prove statistical independence.</p>',
  '<img src="split_agreement.png" alt="Ordering agreement across sample splits">', html_table(display_split),
  '<p>Random properties and quarters use ', max(ranks[method == "random_properties", draw]), ' predetermined seeds. Random-quarter halves each contain two quarters from every year. Early/late means 2006–2014 versus 2015–2022 for the full score, and 2006–2010 versus 2011–2014 for the frozen score. Each reported split comparison requires at least ',
  minimum_permits, ' permits and ', minimum_months, ' months for each alderman in each half. Missing pairs are excluded from agreement and counted in coverage. Later entrants and early departures therefore limit the early/late comparison.</p>',
  '<h2>Citywide rank correlations, for context</h2>', html_table(display_rank),
  '<h2>Every local pair under annual reweighting</h2>', html_table(detail),
  '<p>Sources and assumptions: <a href="../README.md">audit documentation</a>. Download <a href="bootstrap_pairs.csv">all resampling results by pair</a>, <a href="split_summary.csv">all split runs</a>, or <a href="comparison_pairs.csv">sample representation by pair</a>. No production scores, housing estimates, or manuscript files are changed.</p>'), "../output/pair_reliability.html")
