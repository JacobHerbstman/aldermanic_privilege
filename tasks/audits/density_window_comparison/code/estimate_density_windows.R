# setwd("tasks/audits/density_window_comparison/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
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
  "../input/alderman_uncertainty_index_through2022.csv",
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
  "all", "density_dupac", "DUPAC\nAll residential new construction",
  "multifamily", "density_dupac", "DUPAC\nNew multifamily construction"
)

results <- list()
for (i in seq_len(nrow(panel_specs))) {
  sample_name <- panel_specs$sample[i]
  outcome <- panel_specs$outcome[i]
  eligible <- projects |>
    dplyr::filter(
      construction_year >= 2006L, construction_year <= 2022L, within_500ft,
      sample_name == "all" | external_multifamily,
      if (outcome == "density_far") allow_far else allow_dupac,
      is.finite(.data[[outcome]]), .data[[outcome]] > 0,
      is.finite(share_white_own), is.finite(share_black_own),
      is.finite(median_hh_income_own), is.finite(share_bach_plus_own),
      is.finite(homeownership_rate_own), !is.na(zone_group),
      !is.na(segment_id), segment_id != "", !is.na(ward_pair), ward_pair != "",
      is.finite(strictness_own), is.finite(strictness_neighbor),
      strictness_own != strictness_neighbor
    ) |>
    dplyr::mutate(distance_ft = signed_distance_m / 0.3048,
      more_stringent = as.integer(strictness_own > strictness_neighbor),
      log_outcome = log(.data[[outcome]]))
  for (window_ft in c(100, 200, 300, 400, 500)) {
    model_data <- eligible |> dplyr::filter(abs(distance_ft) < window_ft)
    stopifnot(dplyr::n_distinct(model_data$more_stringent) == 2L)
    model <- fixest::feols(
      log_outcome ~ more_stringent + share_white_own + share_black_own +
        median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
        zone_group + segment_id + construction_year,
      data = model_data, cluster = ~ward_pair, notes = FALSE
    )
    stopifnot("more_stringent" %in% names(coef(model)))
    fit_rows <- model_data[fixest::obs(model), ]
    coefficient <- fixest::coeftable(model)["more_stringent", ]
    interval <- confint(model, parm = "more_stringent", level = 0.95)
    results[[length(results) + 1L]] <- tibble::tibble(
      sample = sample_name, outcome = outcome, window_ft = window_ft,
      estimate = unname(coefficient[1]), std_error = unname(coefficient[2]),
      p_value = unname(coefficient[4]), ci_low = interval[1, 1], ci_high = interval[1, 2],
      n = nobs(model), n_more_stringent = sum(fit_rows$more_stringent == 1L),
      n_less_stringent = sum(fit_rows$more_stringent == 0L),
      ward_pairs = dplyr::n_distinct(fit_rows$ward_pair),
      segments = dplyr::n_distinct(fit_rows$segment_id)
    )
  }
}
results <- dplyr::bind_rows(results) |>
  dplyr::mutate(percent_difference = 100 * expm1(estimate),
    percent_ci_low = 100 * expm1(ci_low), percent_ci_high = 100 * expm1(ci_high))
stopifnot(nrow(results) == 20L, !anyDuplicated(results[c("sample", "outcome", "window_ft")]),
  all(results$n == results$n_more_stringent + results$n_less_stringent),
  all(is.finite(results$estimate)), all(results$std_error > 0))
readr::write_csv(results, "../output/density_window_estimates.csv")
