# setwd("tasks/working_paper_release_audit/code")

source("../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../input/projects.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    ward_pair = readr::col_character(),
    segment_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(projects$project_id) > 0L) {
  stop("New-construction data must be unique by project ID.")
}

scores <- readr::read_csv(
  "../input/scores.csv",
  show_col_types = FALSE
) |>
  dplyr::select(alderman, uncertainty_index)
if (anyDuplicated(scores$alderman) > 0L) {
  stop("Alderman scores must be unique by alderman.")
}

projects <- projects |>
  dplyr::select(-strictness_own, -strictness_neighbor) |>
  dplyr::left_join(
    scores |>
      dplyr::rename(alderman_own = alderman, strictness_own = uncertainty_index),
    by = "alderman_own",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    scores |>
      dplyr::rename(alderman_neighbor = alderman, strictness_neighbor = uncertainty_index),
    by = "alderman_neighbor",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    signed_distance_m = abs(signed_distance_m) * sign(strictness_own - strictness_neighbor)
  )

panel_specs <- tibble::tribble(
  ~sample, ~outcome, ~panel_title,
  "all", "density_far", "Floor-area ratio\nAll residential new construction",
  "multifamily", "density_far", "Floor-area ratio\nNew multifamily construction",
  "all", "density_dupac", "Dwelling units per acre\nAll residential new construction",
  "multifamily", "density_dupac", "Dwelling units per acre\nNew multifamily construction"
)

decisions <- readr::read_csv("../input/density_denominator_decisions.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(decisions$project_id),
  all(decisions$decision == "exclude_density"), all(decisions$project_id %in% projects$project_id))
comparison <- list()
for (scenario in c("current", "exclude_land_only", "exclude_unresolved")) {
  scenario_projects <- projects
  if (scenario != "current") {
    excluded_ids <- if (scenario == "exclude_land_only") {
      decisions$project_id[decisions$reason_code == "unresolved_land_allocation"]
    } else {
      decisions$project_id
    }
    scenario_projects <- scenario_projects |>
      dplyr::mutate(allow_far = allow_far & !project_id %in% excluded_ids,
        allow_dupac = allow_dupac & !project_id %in% excluded_ids)
  }
for (i in seq_len(nrow(panel_specs))) {
  sample_name <- panel_specs$sample[i]
  outcome <- panel_specs$outcome[i]

  model_data <- scenario_projects |>
    dplyr::filter(
      construction_year >= 2006L,
      construction_year <= 2022L,
      within_500ft,
      dwelling_units > 0,
      sample_name == "all" | external_multifamily,
      allow_far,
      allow_dupac,
      is.finite(density_far),
      density_far > 0,
      is.finite(density_dupac),
      density_dupac > 0,
      is.finite(share_white_own),
      is.finite(share_black_own),
      is.finite(median_hh_income_own),
      is.finite(share_bach_plus_own),
      is.finite(homeownership_rate_own),
      !is.na(zone_group),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      ward_pair != ""
    ) |>
    dplyr::mutate(
      running_distance_ft = signed_distance_m / 0.3048,
      log_outcome = log(.data[[outcome]]),
      distance_bin = cut(
        running_distance_ft,
        breaks = seq(-500, 500, by = 100),
        labels = sprintf("bin_%02d", 1:10),
        include.lowest = TRUE,
        right = FALSE
      )
    ) |>
    dplyr::filter(
      abs(running_distance_ft) < 500,
      !is.na(distance_bin)
    )

  model <- fixest::feols(
    log_outcome ~
      i(distance_bin, ref = "bin_05") +
      share_white_own +
      share_black_own +
      median_hh_income_own +
      share_bach_plus_own +
      homeownership_rate_own |
      zone_group + segment_id + construction_year,
    data = model_data,
    cluster = ~ward_pair,
    warn = FALSE,
    notes = FALSE
  )


  coefficients <- fixest::coeftable(model)
  keep <- grepl("^distance_bin::", rownames(coefficients))
  comparison[[length(comparison) + 1L]] <- tibble::tibble(
    scenario, sample = sample_name, outcome,
    term = rownames(coefficients)[keep],
    estimate = coefficients[keep, "Estimate"],
    std_error = coefficients[keep, "Std. Error"],
    p_value = coefficients[keep, "Pr(>|t|)"],
    percent_difference = 100 * (exp(estimate) - 1),
    n_projects = stats::nobs(model),
    n_ward_pairs = dplyr::n_distinct(model_data$ward_pair)
  )
}
}
comparison <- dplyr::bind_rows(comparison)
stopifnot(!anyDuplicated(comparison[c("scenario", "sample", "outcome", "term")]))
readr::write_csv(comparison, "../output/density_exclusion_comparison.csv")
