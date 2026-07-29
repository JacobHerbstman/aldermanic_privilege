# setwd("tasks/audits/density_adjusted_figure/code")

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

segments <- readr::read_csv(
  "../input/segment_classification.csv",
  show_col_types = FALSE,
  col_select = c(
    segment_id,
    segment_length_ft,
    expressway_overlap_ft,
    major_overlap_arterial_ft,
    water_area_share,
    waterway_overlap_ft,
    park_area_share,
    cemetery_area_share
  ),
  col_types = readr::cols(
    segment_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(segments$segment_id) > 0) {
  stop("Segment classifications are not unique by segment ID.")
}

projects <- projects |>
  dplyr::left_join(
    segments,
    by = "segment_id",
    relationship = "many-to-one"
  )

if (anyNA(projects$expressway_overlap_ft)) {
  stop("Some construction projects lack boundary-feature classifications.")
}

projects <- projects |>
  dplyr::mutate(
    true_distance_ft = signed_distance_m / 0.3048,
    distance_bin = cut(
      true_distance_ft,
      breaks = seq(-500, 500, by = 100),
      labels = sprintf("bin_%02d", 1:10),
      include.lowest = TRUE,
      right = FALSE
    ),
    expressway_share = expressway_overlap_ft / segment_length_ft,
    arterial_share = major_overlap_arterial_ft / segment_length_ft,
    water_share = pmax(
      waterway_overlap_ft / segment_length_ft,
      water_area_share
    ),
    barrier_share = pmax(expressway_share, water_share),
    park_water_share = pmin(
      1,
      water_area_share +
        park_area_share +
        waterway_overlap_ft / segment_length_ft
    ),
    physical_barrier_share = pmin(
      1,
      park_water_share + cemetery_area_share
    ),
    existing_segment_drop = (
      physical_barrier_share >= 0.50 |
        expressway_share >= 0.40 |
        arterial_share >= 0.75 |
        waterway_overlap_ft > 0 |
        cemetery_area_share > 0
    ),
    share_based_segment_drop = (
      physical_barrier_share >= 0.50 |
        expressway_share >= 0.40 |
        arterial_share >= 0.75
    )
  ) |>
  dplyr::filter(
    construction_year >= 2006,
    construction_year <= 2022,
    within_500ft,
    !is.na(distance_bin),
    dwelling_units > 0,
    allow_far,
    allow_dupac,
    is.finite(density_far),
    density_far > 0,
    is.finite(density_dupac),
    density_dupac > 0,
    is.finite(pair_average_score),
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
  )

sample_rules <- tibble::tribble(
  ~boundary_sample, ~rule, ~barrier_threshold, ~drops_arterials,
  "All boundary segments", "all", NA_real_, FALSE,
  "Old any-contact rule", "barrier", 0.00, FALSE,
  "At least 25 percent barrier overlap", "barrier", 0.25, FALSE,
  "At least 50 percent barrier overlap", "barrier", 0.50, FALSE,
  "At least 75 percent barrier overlap", "barrier", 0.75, FALSE,
  "50 percent barrier or arterial overlap", "barrier_arterial", 0.50, TRUE,
  "Existing segment-pruning rule", "existing", NA_real_, TRUE,
  "Share-based segment-pruning rule", "share_based", NA_real_, TRUE
)

panel_specs <- tidyr::crossing(
  sample = c("all", "multifamily"),
  outcome = c("density_far", "density_dupac")
)

results <- list()

for (rule_i in seq_len(nrow(sample_rules))) {
  for (panel_i in seq_len(nrow(panel_specs))) {
    keep_segment <- switch(
      sample_rules$rule[rule_i],
      all = rep(TRUE, nrow(projects)),
      barrier = if (sample_rules$barrier_threshold[rule_i] == 0) {
        projects$barrier_share <= 0
      } else {
        projects$barrier_share <
          sample_rules$barrier_threshold[rule_i]
      },
      barrier_arterial = (
        projects$barrier_share <
          sample_rules$barrier_threshold[rule_i] &
          projects$arterial_share < 0.50
      ),
      existing = !projects$existing_segment_drop,
      share_based = !projects$share_based_segment_drop
    )
    keep_segment[is.na(keep_segment)] <- FALSE

    model_data <- projects[keep_segment, ] |>
      dplyr::filter(
        panel_specs$sample[panel_i] == "all" | external_multifamily
      ) |>
      dplyr::mutate(
        log_outcome = log(.data[[panel_specs$outcome[panel_i]]]),
        distance_bin = droplevels(distance_bin)
      )

    model <- fixest::feols(
      log_outcome ~
        i(distance_bin, ref = "bin_05") +
        pair_average_score +
        share_white_own +
        share_black_own +
        median_hh_income_own +
        share_bach_plus_own +
        homeownership_rate_own |
        zone_group + segment_id + construction_year,
      data = model_data,
      cluster = ~ward_pair
    )

    coefficient_table <- fixest::coeftable(model)
    coefficient_name <- "distance_bin::bin_06"
    if (!coefficient_name %in% rownames(coefficient_table)) {
      stop("The nearest more-stringent bin was not estimated.")
    }

    results[[length(results) + 1L]] <- tibble::tibble(
      boundary_sample = sample_rules$boundary_sample[rule_i],
      sample = panel_specs$sample[panel_i],
      outcome = panel_specs$outcome[panel_i],
      estimate = coefficient_table[coefficient_name, "Estimate"],
      std_error = coefficient_table[coefficient_name, "Std. Error"],
      p_value = coefficient_table[coefficient_name, "Pr(>|t|)"],
      n_projects = stats::nobs(model),
      n_ward_pairs = dplyr::n_distinct(model_data$ward_pair),
      n_segments = dplyr::n_distinct(model_data$segment_id),
      barrier_threshold = sample_rules$barrier_threshold[rule_i],
      drops_arterials = sample_rules$drops_arterials[rule_i]
    )
  }
}

readr::write_csv(
  dplyr::bind_rows(results),
  "../output/density_nonparametric_boundary_feature_robustness.csv"
)
