# setwd("tasks/working_paper_release_audit/code")
source("../../setup_environment/code/packages.R")

before <- readr::read_csv("../reference/new_construction_analysis_data_before_reconnection.csv",
  col_types = readr::cols(project_id = "c", ward_pair = "c", segment_id = "c", .default = readr::col_guess()))
after <- readr::read_csv("../input/current_construction_analysis.csv",
  col_types = readr::cols(project_id = "c", ward_pair = "c", segment_id = "c", .default = readr::col_guess()))
comparison <- readr::read_csv("../output/construction_estimation_project_changes.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(before$project_id), !anyDuplicated(after$project_id), !anyDuplicated(comparison$project_id))

# The production estimator refreshes score orientation from the same score file.
scores <- readr::read_csv("../input/scores.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(scores$alderman))
for (version in c("before", "after")) {
  data <- get(version)
  own <- scores$uncertainty_index[match(data$alderman_own, scores$alderman)]
  neighbor <- scores$uncertainty_index[match(data$alderman_neighbor, scores$alderman)]
  data$signed_distance_m <- abs(data$signed_distance_m) * sign(own - neighbor)
  assign(version, data)
}

# Sequential accounting exercise, not a causal decomposition. Hold the common
# project IDs fixed while replacing outcomes, years, geography and controls.
results <- list()
for (sample_name in c("all", "multifamily")) {
  for (outcome in c("far", "dupac")) {
    old_ids <- comparison$project_id[dplyr::coalesce(comparison[[paste0(outcome, "_sample_before")]], FALSE) &
      (sample_name == "all" | dplyr::coalesce(comparison$external_multifamily_before, FALSE))]
    new_ids <- comparison$project_id[dplyr::coalesce(comparison[[paste0(outcome, "_sample_after")]], FALSE) &
      (sample_name == "all" | dplyr::coalesce(comparison$external_multifamily_after, FALSE))]
    common <- intersect(old_ids, new_ids)
    old <- before[match(old_ids, before$project_id), ]
    new <- after[match(new_ids, after$project_id), ]
    old_common <- before[match(common, before$project_id), ]
    new_common <- after[match(common, after$project_id), ]
    stopifnot(!anyNA(old$project_id), !anyNA(new$project_id), identical(old_common$project_id, new_common$project_id))
    stages <- list(old_sample = old, remove_departing_records = old_common)
    updated <- old_common
    updated[[paste0("density_", outcome)]] <- new_common[[paste0("density_", outcome)]]
    stages$update_density_measurements <- updated
    updated$construction_year <- new_common$construction_year
    stages$update_construction_years <- updated
    for (field in c("signed_distance_m", "segment_id", "ward_pair")) updated[[field]] <- new_common[[field]]
    stages$update_locations_and_boundary_assignments <- updated
    for (field in c("zone_group", "share_white_own", "share_black_own", "median_hh_income_own",
      "share_bach_plus_own", "homeownership_rate_own")) updated[[field]] <- new_common[[field]]
    stages$update_zoning_and_ward_controls <- updated
    stages$new_common_records <- new_common
    if (sample_name == "all") {
      stages$add_entering_individual_homes <- new[new$project_id %in% common | !new$external_multifamily, ]
    }
    stages$add_entering_records <- new
    for (stage in names(stages)) {
      data <- stages[[stage]] |>
        dplyr::mutate(log_outcome = log(.data[[paste0("density_", outcome)]]),
          distance_bin = cut(signed_distance_m / 0.3048, breaks = seq(-500, 500, by = 100),
            labels = sprintf("bin_%02d", 1:10), include.lowest = TRUE, right = FALSE))
      stopifnot(all(is.finite(data$log_outcome)), !anyNA(data$distance_bin))
      model <- fixest::feols(log_outcome ~ i(distance_bin, ref = "bin_05") +
        share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
        zone_group + segment_id + construction_year, data = data, cluster = ~ward_pair, warn = FALSE, notes = FALSE)
      ct <- fixest::coeftable(model)["distance_bin::bin_06", ]
      results[[length(results) + 1L]] <- tibble::tibble(sample = sample_name, outcome, stage,
        n = nobs(model), estimate = unname(ct[1]), standard_error = unname(ct[2]), p_value = unname(ct[4]))
    }
  }
}
results <- dplyr::bind_rows(results) |>
  dplyr::group_by(sample, outcome) |>
  dplyr::mutate(change_from_previous = estimate - dplyr::lag(estimate)) |>
  dplyr::ungroup()
readr::write_csv(results, "../output/density_change_decomposition.csv")
