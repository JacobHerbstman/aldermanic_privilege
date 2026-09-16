# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_fixed_effects/code")
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# bin_width_ft <- 100
# samples <- c("all", "multifamily")
# outcomes <- c("density_far", "density_dupac")
# spatial_choices <- c("segment", "border_pair")
# time_choices <- c("year", "joint_service", "none")
# zoning_choices <- c("yes", "no")
# controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
# cluster <- "ward_pair"
library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 11L)
  start_year <- as.integer(args[1]); end_year <- as.integer(args[2])
  bandwidth_ft <- as.numeric(args[3]); bin_width_ft <- as.numeric(args[4])
  samples <- strsplit(args[5], ",", fixed = TRUE)[[1]]
  outcomes <- strsplit(args[6], ",", fixed = TRUE)[[1]]
  spatial_choices <- strsplit(args[7], ",", fixed = TRUE)[[1]]
  time_choices <- strsplit(args[8], ",", fixed = TRUE)[[1]]
  zoning_choices <- strsplit(args[9], ",", fixed = TRUE)[[1]]
  controls <- args[10]; cluster <- args[11]
}
setDTthreads(1)
setFixest_nthreads(1)
d <- fread("../input/construction.csv", colClasses = c(project_id = "character", ward_pair = "character", segment_id = "character"))
terms <- fread("../input/alderman_terms.csv")
baseline <- rbind(fread("../input/baseline_all.csv"), fread("../input/baseline_multifamily.csv"))[scenario == "baseline"]
stopifnot(!anyDuplicated(d$project_id), !anyDuplicated(terms[, .(ward, start_date)]), bandwidth_ft %% bin_width_ft == 0)
control_names <- strsplit(controls, " + ", fixed = TRUE)[[1]]
d[, distance_ft := signed_distance_m / .3048]
d <- d[construction_year >= start_year & construction_year <= end_year & density_eligible == TRUE &
  is.finite(distance_ft) & abs(distance_ft) < bandwidth_ft &
  is.finite(strictness_own) & is.finite(strictness_neighbor) & strictness_own != strictness_neighbor &
  !is.na(zone_group) & !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) & ward_pair != "" &
  Reduce(`&`, lapply(d[, ..control_names], is.finite))]
stopifnot(all(is.finite(d$density_far) & d$density_far > 0), all(is.finite(d$density_dupac) & d$density_dupac > 0))
d[, construction_date := as.IDate(construction_date)]
terms[, `:=`(start_date = as.IDate(start_date), end_date = as.IDate(end_date))]
setorder(terms, ward, start_date)
stopifnot(all(terms$start_date <= terms$end_date),
  !any(terms[, start_date <= shift(end_date), by = ward]$V1, na.rm = TRUE))
# The term file records uninterrupted incumbencies, not a new row at reelection.
stopifnot(!any(terms[, alderman == shift(alderman) & start_date == shift(end_date) + 1, by = ward]$V1, na.rm = TRUE))
for (side in c("own", "neighbor")) {
  wards <- if (side == "own") d$ward else d$neighbor_ward
  matches <- integer(nrow(d)); term_index <- integer(nrow(d))
  for (i in seq_len(nrow(terms))) {
    selected <- which(wards == terms$ward[i] & d$construction_date >= terms$start_date[i] & d$construction_date <= terms$end_date[i])
    matches[selected] <- matches[selected] + 1L
    term_index[selected] <- i
  }
  stopifnot(all(matches == 1L), all(terms$alderman[term_index] == d[[paste0("alderman_", side)]]))
  d[, (paste0(side, "_term")) := term_index]
}
# Split a ward-number pair when the map changes. Joint service also splits at either turnover.
d[, border_pair := paste(era, ward_pair, sep = ":")]
d[, joint_service := paste(border_pair, pmin(own_term, neighbor_term), pmax(own_term, neighbor_term), sep = ":")]
reference_bin <- bandwidth_ft / bin_width_ft
main_term <- paste0("distance_bin::", reference_bin + 1L)
d[, distance_bin := cut(distance_ft, seq(-bandwidth_ft, bandwidth_ft, bin_width_ft), labels = FALSE, right = FALSE, include.lowest = TRUE)]
results <- list()
for (sample_name in samples) {
  sample_data <- d[sample_name == "all" | external_multifamily == TRUE]
  for (spatial in spatial_choices) for (time in time_choices) for (zoning in zoning_choices) {
    fixed_effects <- if (spatial == "segment") "segment_id" else "border_pair"
    if (time == "year") fixed_effects <- c(fixed_effects, "construction_year")
    if (time == "joint_service") {
      # Joint-service FE already contain the border-pair FE.
      fixed_effects <- c(if (spatial == "segment") "segment_id", "joint_service")
    }
    if (zoning == "yes") fixed_effects <- c(fixed_effects, "zone_group")
    for (outcome_name in outcomes) {
      formula <- as.formula(sprintf("log(%s) ~ i(distance_bin, ref = %d) + %s | %s",
        outcome_name, reference_bin, controls, paste(fixed_effects, collapse = " + ")))
      model <- feols(formula, sample_data, cluster = as.formula(paste("~", cluster)), notes = FALSE)
      b <- coeftable(model)[main_term, ]
      ci <- as.matrix(confint(model, parm = main_term))
      if (spatial == "segment" & time == "year" & zoning == "yes") {
        reference <- baseline[analysis == paste0("density_", sample_name) & outcome == outcome_name]
        stopifnot(nrow(reference) == 1L, abs(b[1] - reference$estimate) < 1e-8,
          abs(b[2] - reference$std_error) < 1e-8, nobs(model) == reference$n)
      }
      results[[length(results) + 1L]] <- data.table(sample = sample_name, outcome = outcome_name,
        spatial_fe = spatial, time_fe = time, zoning_fe = zoning, estimate = unname(b[1]),
        std_error = unname(b[2]), p_value = unname(b[4]), percent_effect = 100 * expm1(b[1]),
        percent_low = 100 * expm1(unname(ci[1])), percent_high = 100 * expm1(unname(ci[2])),
        input_n = nrow(sample_data), n = nobs(model), border_pairs = uniqueN(sample_data$border_pair),
        joint_service_periods = uniqueN(sample_data$joint_service),
        ward_clusters = uniqueN(sample_data[[cluster]]), fixed_effects = paste(fixed_effects, collapse = " + "))
    }
  }
}
results <- rbindlist(results)
SaveData(results, c("sample", "outcome", "spatial_fe", "time_fe", "zoning_fe"), "../output/density_fixed_effects.csv")
# One compact report for the complete requested grid, with p-values alongside magnitudes.
html <- c('<!doctype html><meta charset="utf-8"><title>Density fixed effects</title>',
  '<style>body{font:16px/1.5 system-ui;margin:35px;color:#203543;max-width:1200px}table{border-collapse:collapse}th,td{padding:9px;border-bottom:1px solid #ccd6dc;text-align:left}th{background:#edf2f5}</style>',
  '<h1>Density estimates under alternative fixed effects</h1>',
  '<p>Exploratory comparison using the current data and original stringency scores. Each number is the percentage difference for 0–100 feet on the more-stringent side relative to 0–100 feet on the less-stringent side, estimated using observations within 500 feet. Parentheses contain p-values, clustered by ward pair.</p>',
  '<p>Every specification starts with the same buildings and five demographic controls. Both FAR and DUPAC must be usable, and zoning must be recorded even when its controls are omitted. Border pairs distinguish ward-map eras. Joint service means the same two incumbents within that map; reelection alone does not split a period. The usual fixed-effect singleton removal can change estimation N.</p>')
latex <- character()
for (sample_name in samples) {
  table <- dcast(results[sample == sample_name], spatial_fe + time_fe ~ outcome + zoning_fe,
    value.var = c("percent_effect", "p_value", "n"))
  display <- table[, .(`Spatial FE` = spatial_fe, `Time FE` = time_fe,
    `FAR, zoning` = sprintf("%.2f%% (p=%.3f), N=%d", percent_effect_density_far_yes, p_value_density_far_yes, n_density_far_yes),
    `FAR, no zoning` = sprintf("%.2f%% (p=%.3f), N=%d", percent_effect_density_far_no, p_value_density_far_no, n_density_far_no),
    `DUPAC, zoning` = sprintf("%.2f%% (p=%.3f), N=%d", percent_effect_density_dupac_yes, p_value_density_dupac_yes, n_density_dupac_yes),
    `DUPAC, no zoning` = sprintf("%.2f%% (p=%.3f), N=%d", percent_effect_density_dupac_no, p_value_density_dupac_no, n_density_dupac_no))]
  html <- c(html, paste0('<h2>', sample_name, '</h2><table><tr>', paste0('<th>', names(display), '</th>', collapse = ''), '</tr>'),
    apply(display, 1, function(row) paste0('<tr>', paste0('<td>', row, '</td>', collapse = ''), '</tr>')), '</table>')
  latex <- c(latex, paste0('\\par\\medskip\\noindent\\textbf{', ifelse(sample_name == 'all', 'All construction', 'Multifamily construction'),
    ' ($N=', unique(results[sample == sample_name, n]), '$).}\\par\\smallskip'),
    '\\noindent\\resizebox{\\textwidth}{!}{\\begin{tabular}{llrrrr}\\hline',
    'Spatial FE & Time FE & FAR, zoning & FAR, no zoning & DUPAC, zoning & DUPAC, no zoning \\\\ \\hline')
  for (i in seq_len(nrow(display))) {
    row <- as.character(display[i])
    row <- sub(', N=[0-9]+', '', row)
    row <- gsub('_', ' ', row, fixed = TRUE)
    row <- gsub('%', '\\%', row, fixed = TRUE)
    latex <- c(latex, paste0(paste(row, collapse = ' & '), ' \\\\'))
  }
  latex <- c(latex, '\\hline\\end{tabular}}')
  print(display)
}
html <- c(html, '<p>Removing segment or calendar-year effects changes what variation identifies the estimate. It is not automatically a gain in precision. These specifications do not repair counts, lot areas, construction eligibility or dates that change the assigned alderman or map.</p>',
  '<p><a href="density_fixed_effects.csv">All coefficients, confidence intervals and sample sizes</a></p>')
writeLines(html, "../output/density_fixed_effects.html")
writeLines(latex, "../output/density_fixed_effects.tex")
