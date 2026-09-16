# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_ordering_drivers/code")
# sample_name <- "multifamily"
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# bin_width_ft <- 100
# controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
# fixed_effects <- "zone_group + segment_id + construction_year"
# cluster <- "ward_pair"
# workers <- 4
# joint_projects <- 5
library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 10L)
  sample_name <- args[1]
  start_year <- as.integer(args[2])
  end_year <- as.integer(args[3])
  bandwidth_ft <- as.numeric(args[4])
  bin_width_ft <- as.numeric(args[5])
  controls <- args[6]
  fixed_effects <- args[7]
  cluster <- args[8]
  workers <- as.integer(args[9])
  joint_projects <- as.integer(args[10])
}
stopifnot(sample_name %in% c("all", "multifamily"), bandwidth_ft %% bin_width_ft == 0, workers >= 1L, joint_projects >= 1L)
setDTthreads(1)
setFixest_nthreads(1)
before <- fread("../input/new_construction_analysis_data.csv", colClasses = c(project_id = "character", ward_pair = "character", segment_id = "character"))
after <- fread("../input/corrected_construction_analysis_data.csv", colClasses = c(project_id = "character", ward_pair = "character", segment_id = "character"))
screen <- fread("../input/measurement_screen.csv")
review <- fread("../input/count_land_review.csv")
stopifnot(!anyDuplicated(before$project_id), !anyDuplicated(after$project_id),
  !anyDuplicated(screen$project_id), !anyDuplicated(review$project_id))
control_names <- strsplit(controls, " + ", fixed = TRUE)[[1]]
bin_edges <- seq(-bandwidth_ft, bandwidth_ft, bin_width_ft)
reference_bin <- bandwidth_ft / bin_width_ft
main_term <- paste0("distance_bin::", reference_bin + 1L)
formula <- as.formula(sprintf("log(density_dupac) ~ i(distance_bin, ref = %d) + %s | %s",
  reference_bin, controls, fixed_effects))

# Apply the unchanged common FAR/DUPAC sample to both versions.
select_sample <- function(d) {
  d <- copy(d)
  d[, paper_distance := signed_distance_m / .3048]
  d <- d[construction_year >= start_year & construction_year <= end_year & density_eligible == TRUE &
    (sample_name == "all" | external_multifamily == TRUE) & is.finite(paper_distance) & abs(paper_distance) < bandwidth_ft &
    is.finite(strictness_own) & is.finite(strictness_neighbor) & strictness_own != strictness_neighbor &
    !is.na(zone_group) & !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) & ward_pair != "" &
    Reduce(`&`, lapply(d[, ..control_names], is.finite))]
  stopifnot(all(is.finite(d$density_far) & d$density_far > 0), all(is.finite(d$density_dupac) & d$density_dupac > 0))
  d[, distance_bin := cut(paper_distance, bin_edges, labels = FALSE, include.lowest = TRUE, right = FALSE)]
  d[, pair := paste(pmin(alderman_own, alderman_neighbor), pmax(alderman_own, alderman_neighbor), sep = " / ")]
  d
}
before <- select_sample(before)
after <- select_sample(after)

fit_dupac <- function(d) {
  stopifnot(!anyDuplicated(d$project_id))
  model <- feols(formula, d, cluster = as.formula(paste("~", cluster)), notes = FALSE, warn = FALSE)
  tab <- coeftable(model)[main_term, ]
  data.table(estimate = unname(tab[1]), std_error = unname(tab[2]), p_value = unname(tab[4]),
    percent_effect = 100 * expm1(tab[1]), input_n = nrow(d), n = nobs(model))
}
old_fit <- fit_dupac(before)
new_fit <- fit_dupac(after)
old_reference <- fread(sprintf("../input/density_%s_results.csv", sample_name))[scenario == "baseline" & outcome == "density_dupac"]
new_reference <- fread(sprintf("../input/corrected_density_%s_results.csv", sample_name))[scenario == "baseline" & outcome == "density_dupac"]
stopifnot(abs(old_fit$estimate - old_reference$estimate) < 1e-8, abs(old_fit$std_error - old_reference$std_error) < 1e-8,
  old_fit$n == old_reference$n, abs(new_fit$estimate - new_reference$estimate) < 1e-8,
  abs(new_fit$std_error - new_reference$std_error) < 1e-8, new_fit$n == new_reference$n)

# Replace each affected project's entire regression record, including its eligibility.
# These one-at-a-time changes need not sum to the total change.
model_columns <- unique(c("project_id", "density_dupac", "distance_bin", control_names,
  all.vars(as.formula(paste("~", fixed_effects))), cluster))
ids <- union(before$project_id, after$project_id)
old_index <- match(ids, before$project_id)
new_index <- match(ids, after$project_id)
changed <- is.na(old_index) | is.na(new_index)
for (column in setdiff(model_columns, "project_id")) {
  x <- before[[column]][old_index]; y <- after[[column]][new_index]
  changed <- changed | xor(is.na(x), is.na(y)) | (!is.na(x) & !is.na(y) & x != y)
}
changed_ids <- ids[changed]
stopifnot(all(changed_ids %in% review$project_id))
corrections <- list(copy(old_fit)[, `:=`(scenario = "before", project_id = "all")],
  copy(new_fit)[, `:=`(scenario = "after", project_id = "all")])
for (id in changed_ids) {
  d <- rbind(before[project_id != id], after[project_id == id], use.names = TRUE)
  corrections[[length(corrections) + 1L]] <- fit_dupac(d)[, `:=`(scenario = "apply_one_to_old", project_id = id)]
  d <- rbind(after[project_id != id], before[project_id == id], use.names = TRUE)
  corrections[[length(corrections) + 1L]] <- fit_dupac(d)[, `:=`(scenario = "undo_one_in_current", project_id = id)]
}

# Separate the two known community-count errors from all other corrections.
madison <- c("commercial_17084440270000", "commercial_17084450160000")
stopifnot(all(madison %in% before$project_id), all(madison %in% after$project_id))
d <- copy(before)
d[project_id %in% madison, density_dupac := after$density_dupac[match(project_id, after$project_id)]]
corrections[[length(corrections) + 1L]] <- fit_dupac(d)[, `:=`(scenario = "madison_counts_only_in_old", project_id = "both_madison")]
d <- rbind(before[!project_id %in% madison], after[project_id %in% madison], use.names = TRUE)
corrections[[length(corrections) + 1L]] <- fit_dupac(d)[, `:=`(scenario = "madison_all_changes_in_old", project_id = "both_madison")]
d <- rbind(after[!project_id %in% madison], before[project_id %in% madison], use.names = TRUE)
corrections[[length(corrections) + 1L]] <- fit_dupac(d)[, `:=`(scenario = "all_except_madison", project_id = "both_madison")]
corrections <- rbindlist(corrections)
corrections[, `:=`(sample = sample_name, before_effect = old_fit$percent_effect, current_effect = new_fit$percent_effect,
  address = screen$address[match(project_id, screen$project_id)],
  old_units = before$dwelling_units[match(project_id, before$project_id)],
  new_units = after$dwelling_units[match(project_id, after$project_id)],
  old_land_sqft = before$land_sqft[match(project_id, before$project_id)],
  new_land_sqft = after$land_sqft[match(project_id, after$project_id)],
  old_year = before$construction_year[match(project_id, before$project_id)],
  new_year = after$construction_year[match(project_id, after$project_id)])]
corrections[, `:=`(change_from_before_pp = percent_effect - before_effect, change_from_current_pp = percent_effect - current_effect)]
print(corrections[scenario %in% c("before", "after", "madison_counts_only_in_old", "madison_all_changes_in_old", "all_except_madison"),
  .(sample, scenario, percent_effect, p_value, n)])

# Re-estimate after deleting each current project, then each named local comparison.
# An influential record is not thereby wrong or eligible for exclusion.
stopifnot(new_fit$n == nrow(after))
projects <- parallel::mclapply(after$project_id, function(id) {
  fit_dupac(after[project_id != id])[, project_id := id]
}, mc.cores = workers, mc.set.seed = FALSE)
stopifnot(all(vapply(projects, is.data.table, logical(1))))
projects <- rbindlist(projects)
index <- match(projects$project_id, after$project_id)
projects[, `:=`(sample = sample_name, current_effect = new_fit$percent_effect,
  address = screen$address[match(project_id, screen$project_id)], pair = after$pair[index],
  alderman_own = after$alderman_own[index], ward = after$ward[index], construction_year = after$construction_year[index],
  dwelling_units = after$dwelling_units[index], land_sqft = after$land_sqft[index], density_dupac = after$density_dupac[index],
  signed_distance_ft = after$paper_distance[index],
  in_recent_review = project_id %in% review$project_id,
  screen_reasons = screen$screen_reasons[match(project_id, screen$project_id)],
  recorded_decision = after$decision_reason[index])]
projects[, `:=`(change_pp = percent_effect - current_effect, additional_omitted = new_fit$n - n - 1L)]
SaveData(projects, c("sample", "project_id"), sprintf("../output/%s_dupac_projects.csv", sample_name))
# This outcome-selected group is an influence diagnostic, never a cleaning rule.
selected <- projects[order(-change_pp, project_id), head(project_id, joint_projects)]
joint <- fit_dupac(after[!project_id %in% selected])
joint[, `:=`(sample = sample_name, scenario = "drop_most_influential_current", project_id = paste(selected, collapse = ";"),
  before_effect = old_fit$percent_effect, current_effect = new_fit$percent_effect,
  change_from_before_pp = percent_effect - old_fit$percent_effect, change_from_current_pp = percent_effect - new_fit$percent_effect)]
corrections <- rbind(corrections, joint, fill = TRUE)
SaveData(corrections, c("sample", "scenario", "project_id"), sprintf("../output/%s_dupac_corrections.csv", sample_name))
print(joint[, .(sample, scenario, percent_effect, p_value, n)])
pairs <- parallel::mclapply(sort(unique(after$pair)), function(name) {
  fit_dupac(after[pair != name])[, `:=`(pair = name, removed_projects = sum(after$pair == name))]
}, mc.cores = workers, mc.set.seed = FALSE)
stopifnot(all(vapply(pairs, is.data.table, logical(1))))
pairs <- rbindlist(pairs)
pairs[, `:=`(sample = sample_name, current_effect = new_fit$percent_effect,
  change_pp = percent_effect - new_fit$percent_effect, additional_omitted = new_fit$n - n - removed_projects)]
SaveData(pairs, c("sample", "pair"), sprintf("../output/%s_dupac_pairs.csv", sample_name))
print(projects[order(-change_pp), head(.SD, 5), .SDcols = c("sample", "address", "percent_effect", "change_pp")])
