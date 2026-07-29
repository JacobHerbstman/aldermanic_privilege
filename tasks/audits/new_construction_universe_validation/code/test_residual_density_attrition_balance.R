# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    cutoff == 2022,
    variant == "all_covariates"
  ) |>
  dplyr::select(alderman, score)

if (anyDuplicated(scores$alderman)) {
  stop("Score input is not uniquely keyed by alderman.")
}
score_map <- tibble::deframe(scores)

retained <- readr::read_csv(
  "../output/provisional_validated_density_input.csv",
  show_col_types = FALSE,
  col_select = c(
    project_id,
    construction_year,
    ward_pair,
    distance_to_boundary_ft,
    within_500ft,
    segment_id,
    alderman_own,
    alderman_neighbor,
    external_multifamily
  ),
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_year = readr::col_integer(),
    ward_pair = readr::col_character(),
    distance_to_boundary_ft = readr::col_double(),
    within_500ft = readr::col_logical(),
    segment_id = readr::col_character(),
    alderman_own = readr::col_character(),
    alderman_neighbor = readr::col_character(),
    external_multifamily = readr::col_logical()
  )
) |>
  dplyr::filter(
    within_500ft,
    external_multifamily
  ) |>
  dplyr::transmute(
    observation_id = project_id,
    sample_status = "retained_density_project",
    omitted = 0L,
    year = construction_year,
    ward_pair,
    segment_id,
    distance_to_boundary_ft,
    alderman_own,
    alderman_neighbor
  )

permit_chains <- readr::read_csv(
  "../input/permit_first_chain_inventory.csv",
  show_col_types = FALSE,
  col_select = c(
    permit_chain_id,
    representative_x_3435,
    representative_y_3435,
    application_alderman,
    application_neighbor_ward,
    application_ward_pair,
    application_boundary_distance_ft,
    application_era,
    representative_application_date
  ),
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    representative_x_3435 = readr::col_double(),
    representative_y_3435 = readr::col_double(),
    application_alderman = readr::col_character(),
    application_neighbor_ward = readr::col_integer(),
    application_ward_pair = readr::col_character(),
    application_boundary_distance_ft = readr::col_double(),
    application_era = readr::col_character(),
    representative_application_date = readr::col_date()
  )
)

if (anyDuplicated(permit_chains$permit_chain_id)) {
  stop("Permit-chain inventory is not one row per chain.")
}

alderman_terms <- readr::read_csv(
  "../input/chicago_alderman_terms.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    ward = readr::col_integer(),
    alderman = readr::col_character(),
    start_date = readr::col_date(),
    end_date = readr::col_date()
  )
)

permit_date_min <- min(
  permit_chains$representative_application_date,
  na.rm = TRUE
)
permit_date_max <- max(
  permit_chains$representative_application_date,
  na.rm = TRUE
)
alderman_daily <- alderman_terms |>
  dplyr::mutate(
    term_start = pmax(start_date, permit_date_min),
    term_end = pmin(end_date, permit_date_max)
  ) |>
  dplyr::filter(term_start <= term_end) |>
  dplyr::mutate(
    application_date = purrr::map2(
      term_start,
      term_end,
      ~ seq(.x, .y, by = "day")
    )
  ) |>
  tidyr::unnest(application_date) |>
  dplyr::select(
    ward,
    application_date,
    neighbor_alderman = alderman
  )

omitted <- readr::read_csv(
  "../output/residual_completed_density_attrition.csv",
  show_col_types = FALSE,
  col_select = c(
    permit_chain_id,
    inside_500ft,
    possible_multifamily
  ),
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    inside_500ft = readr::col_logical(),
    possible_multifamily = readr::col_logical()
  )
) |>
  dplyr::filter(
    inside_500ft,
    possible_multifamily
  ) |>
  dplyr::inner_join(
    permit_chains,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    alderman_daily,
    by = c(
      "application_neighbor_ward" = "ward",
      "representative_application_date" = "application_date"
    ),
    relationship = "many-to-one"
  )

segments <- purrr::map(
  c("1998_2002", "2003_2014", "2015_2023", "post_2023"),
  ~ sf::st_read(
    "../input/boundary_segments_1320ft.gpkg",
    layer = .x,
    quiet = TRUE
  )
) |>
  dplyr::bind_rows() |>
  dplyr::filter(valid_segment) |>
  dplyr::select(
    ward_pair = ward_pair_id,
    application_era = era,
    segment_id = analysis_segment_id
  )

omitted_points <- sf::st_as_sf(
  omitted,
  coords = c("representative_x_3435", "representative_y_3435"),
  crs = 3435,
  remove = FALSE
)
distance_matrix <- as.matrix(sf::st_distance(omitted_points, segments))
eligible_segment <- outer(
  omitted$application_ward_pair,
  segments$ward_pair,
  `==`
) &
  outer(
    omitted$application_era,
    segments$application_era,
    `==`
  )
distance_matrix[!eligible_segment] <- Inf

if (any(!apply(is.finite(distance_matrix), 1, any))) {
  stop("At least one omitted permit could not be assigned to a segment.")
}

nearest_segment <- max.col(-distance_matrix, ties.method = "first")
omitted <- omitted |>
  dplyr::mutate(segment_id = segments$segment_id[nearest_segment]) |>
  dplyr::transmute(
    observation_id = permit_chain_id,
    sample_status = "completed_permit_without_density_fields",
    omitted = 1L,
    year = lubridate::year(representative_application_date),
    ward_pair = application_ward_pair,
    segment_id,
    distance_to_boundary_ft = application_boundary_distance_ft,
    alderman_own = application_alderman,
    alderman_neighbor = neighbor_alderman
  )

balance_sample <- dplyr::bind_rows(retained, omitted) |>
  dplyr::mutate(
    score_own = unname(score_map[alderman_own]),
    score_neighbor = unname(score_map[alderman_neighbor]),
    side = as.integer(score_own > score_neighbor),
    continuous_score_difference = (score_own - score_neighbor) / 2,
    pair_average_score = (score_own + score_neighbor) / 2,
    distance_m = distance_to_boundary_ft * 0.3048,
    lenient_dist = distance_m * as.integer(side == 0L),
    strict_dist = distance_m * as.integer(side == 1L)
  ) |>
  dplyr::filter(
    is.finite(score_own),
    is.finite(score_neighbor),
    is.finite(pair_average_score),
    !is.na(side),
    !is.na(segment_id),
    segment_id != ""
  )

model_specs <- tibble::tribble(
  ~treatment,   ~fixed_effects,
  "binary",     "segment_year",
  "continuous", "segment_year",
  "binary",     "ward_pair_year",
  "continuous", "ward_pair_year"
)

results <- vector("list", nrow(model_specs))
for (i in seq_len(nrow(model_specs))) {
  treatment_term <- if (model_specs$treatment[i] == "binary") {
    "side"
  } else {
    "continuous_score_difference"
  }
  fixed_effect_terms <- if (
    model_specs$fixed_effects[i] == "segment_year"
  ) {
    "segment_id + year"
  } else {
    "ward_pair + year"
  }

  model <- fixest::feols(
    stats::as.formula(paste0(
      "omitted ~ ",
      treatment_term,
      " + pair_average_score + lenient_dist + strict_dist | ",
      fixed_effect_terms
    )),
    data = balance_sample,
    cluster = ~ward_pair
  )
  coefficient <- fixest::coeftable(model)[treatment_term, ]
  removed <- model$obs_selection$obsRemoved
  estimation_rows <- if (is.null(removed)) {
    seq_len(nrow(balance_sample))
  } else {
    setdiff(seq_len(nrow(balance_sample)), abs(as.integer(removed)))
  }
  results[[i]] <- tibble::tibble(
    treatment = model_specs$treatment[i],
    fixed_effects = model_specs$fixed_effects[i],
    estimate = unname(coefficient["Estimate"]),
    se = unname(coefficient["Std. Error"]),
    p_value = unname(coefficient["Pr(>|t|)"]),
    n_obs = stats::nobs(model),
    omitted_observations = sum(balance_sample$omitted[estimation_rows]),
    ward_pairs = dplyr::n_distinct(
      balance_sample$ward_pair[estimation_rows]
    ),
    segments = dplyr::n_distinct(
      balance_sample$segment_id[estimation_rows]
    )
  )
}

counts <- balance_sample |>
  dplyr::count(sample_status, side, name = "observations") |>
  dplyr::group_by(sample_status) |>
  dplyr::mutate(share = observations / sum(observations)) |>
  dplyr::ungroup()

readr::write_csv(
  balance_sample,
  "../output/density_attrition_balance_sample.csv",
  na = ""
)
readr::write_csv(
  dplyr::bind_rows(results),
  "../output/density_attrition_balance_results.csv",
  na = ""
)
readr::write_csv(
  counts,
  "../output/density_attrition_treatment_counts.csv",
  na = ""
)
