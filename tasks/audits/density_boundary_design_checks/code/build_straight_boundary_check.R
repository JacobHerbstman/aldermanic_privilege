# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_boundary_design_checks/code")

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
) |>
  dplyr::filter(
    construction_year >= 2006L,
    construction_year <= 2022L,
    within_500ft,
    dwelling_units > 0,
    allow_far,
    allow_dupac,
    is.finite(density_far),
    density_far > 0,
    is.finite(density_dupac),
    density_dupac > 0,
    is.finite(pair_average_score),
    dplyr::if_all(
      dplyr::all_of(c(
        "share_white_own",
        "share_black_own",
        "median_hh_income_own",
        "share_bach_plus_own",
        "homeownership_rate_own"
      )),
      is.finite
    ),
    !is.na(zone_group),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != ""
  ) |>
  dplyr::mutate(
    era = stringr::str_extract(
      segment_id,
      "1998_2002|2003_2014|2015_2023|post_2023"
    ),
    true_distance_ft = signed_distance_m / 0.3048,
    distance_bin = cut(
      true_distance_ft,
      breaks = seq(-500, 500, by = 100),
      labels = sprintf("bin_%02d", 1:10),
      include.lowest = TRUE,
      right = FALSE
    )
  )

if (anyNA(projects$era)) {
  stop("Some projects have segment IDs without a recognized ward-map era.")
}
if (anyDuplicated(projects$project_id) > 0) {
  stop("The 500-foot density sample must be unique by project ID.")
}

centroids <- sf::st_read("../input/project_centroids.gpkg", quiet = TRUE) |>
  sf::st_transform(3435) |>
  dplyr::select(project_id)
sf::st_geometry(centroids) <- "geometry"

if (anyDuplicated(centroids$project_id) > 0) {
  stop("Project centroid input must be unique by project ID.")
}

missing_centroids <- dplyr::anti_join(
  projects |>
    dplyr::select(project_id),
  sf::st_drop_geometry(centroids),
  by = "project_id"
)

if (nrow(missing_centroids) > 0) {
  recovered_coordinates <- readr::read_csv(
    "../input/final_new_construction_audit_ledger.csv",
    show_col_types = FALSE,
    col_types = readr::cols(
      project_id = readr::col_character(),
      x_3435 = readr::col_double(),
      y_3435 = readr::col_double(),
      .default = readr::col_skip()
    )
  ) |>
    dplyr::semi_join(missing_centroids, by = "project_id") |>
    dplyr::filter(is.finite(x_3435), is.finite(y_3435))

  if (nrow(recovered_coordinates) != nrow(missing_centroids)) {
    stop("Some final-sample projects lack coordinates in both approved sources.")
  }

  recovered_centroids <- sf::st_as_sf(
    recovered_coordinates,
    coords = c("x_3435", "y_3435"),
    crs = 3435
  ) |>
    dplyr::select(project_id)

  centroids <- rbind(centroids, recovered_centroids)
}

project_sf <- centroids |>
  dplyr::inner_join(projects, by = "project_id", relationship = "one-to-one")

if (nrow(project_sf) != nrow(projects)) {
  missing_ids <- dplyr::anti_join(
    projects,
    sf::st_drop_geometry(centroids),
    by = "project_id"
  )
  stop(sprintf(
    "%s density projects lack a project centroid.",
    nrow(missing_ids)
  ))
}

boundary_layers <- c("1998_2002", "2003_2014", "2015_2023", "post_2023")
boundaries <- do.call(
  rbind,
  lapply(
    boundary_layers,
    function(layer_name) {
      sf::st_read(
        "../input/ward_pair_boundaries.gpkg",
        layer = layer_name,
        quiet = TRUE
      ) |>
        sf::st_transform(3435) |>
        dplyr::select(era, ward_pair_id)
    }
  )
)

if (anyDuplicated(sf::st_drop_geometry(boundaries)[c("era", "ward_pair_id")]) > 0) {
  stop("Ward-pair boundaries must be unique by era and ward pair.")
}

project_sf$nearest_boundary_distance_ft <- NA_real_
project_sf$left_endpoint_distance_m <- NA_real_
project_sf$right_endpoint_distance_m <- NA_real_

group_key <- interaction(
  project_sf$era,
  project_sf$ward_pair,
  drop = TRUE,
  lex.order = TRUE
)

for (idx in split(seq_len(nrow(project_sf)), group_key)) {
  boundary_row <- boundaries |>
    dplyr::filter(
      era == project_sf$era[idx[1]],
      ward_pair_id == project_sf$ward_pair[idx[1]]
    )

  if (nrow(boundary_row) != 1L) {
    stop(sprintf(
      "Expected one boundary for %s in %s; found %s.",
      project_sf$ward_pair[idx[1]],
      project_sf$era[idx[1]],
      nrow(boundary_row)
    ))
  }

  boundary_geometry <- sf::st_geometry(boundary_row)[[1]]
  repeated_boundary <- sf::st_sfc(
    rep(list(boundary_geometry), length(idx)),
    crs = sf::st_crs(boundary_row)
  )
  nearest_lines <- sf::st_nearest_points(
    sf::st_geometry(project_sf[idx, ]),
    repeated_boundary,
    pairwise = TRUE
  )

  point_xy <- matrix(NA_real_, nrow = length(idx), ncol = 2)
  boundary_xy <- matrix(NA_real_, nrow = length(idx), ncol = 2)
  for (j in seq_along(idx)) {
    nearest_coordinates <- sf::st_coordinates(nearest_lines[j])
    point_xy[j, ] <- nearest_coordinates[1, c("X", "Y")]
    boundary_xy[j, ] <- nearest_coordinates[
      nrow(nearest_coordinates),
      c("X", "Y")
    ]
  }

  normal_vector <- boundary_xy - point_xy
  normal_length <- sqrt(rowSums(normal_vector^2))
  if (any(!is.finite(normal_length) | normal_length <= 0)) {
    stop("A project lies directly on its assigned boundary, so a local tangent cannot be defined.")
  }

  tangent_unit <- cbind(
    -normal_vector[, 2] / normal_length,
    normal_vector[, 1] / normal_length
  )
  half_line_ft <- 50 / 0.3048
  left_xy <- boundary_xy - half_line_ft * tangent_unit
  right_xy <- boundary_xy + half_line_ft * tangent_unit

  left_points <- sf::st_sfc(
    lapply(seq_len(nrow(left_xy)), function(j) sf::st_point(left_xy[j, ])),
    crs = sf::st_crs(boundary_row)
  )
  right_points <- sf::st_sfc(
    lapply(seq_len(nrow(right_xy)), function(j) sf::st_point(right_xy[j, ])),
    crs = sf::st_crs(boundary_row)
  )

  project_sf$nearest_boundary_distance_ft[idx] <- normal_length
  project_sf$left_endpoint_distance_m[idx] <- as.numeric(
    sf::st_distance(left_points, sf::st_geometry(boundary_row))
  ) * 0.3048
  project_sf$right_endpoint_distance_m[idx] <- as.numeric(
    sf::st_distance(right_points, sf::st_geometry(boundary_row))
  ) * 0.3048
}

project_sf <- project_sf |>
  dplyr::mutate(
    straight_boundary = (
      left_endpoint_distance_m <= 15 &
        right_endpoint_distance_m <= 15
    ),
    distance_reconstruction_error_ft = abs(
      nearest_boundary_distance_ft - distance_to_boundary_ft
    )
  )

if (max(project_sf$distance_reconstruction_error_ft, na.rm = TRUE) > 1) {
  stop("Project centroids do not reproduce the assigned boundary distance within one foot.")
}

segment_features <- readr::read_csv(
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
) |>
  dplyr::transmute(
    segment_id,
    expressway_share = expressway_overlap_ft / segment_length_ft,
    arterial_share = major_overlap_arterial_ft / segment_length_ft,
    water_share = pmax(
      waterway_overlap_ft / segment_length_ft,
      water_area_share
    ),
    physical_feature_share = pmin(
      1,
      water_area_share +
        park_area_share +
        cemetery_area_share +
        waterway_overlap_ft / segment_length_ft
    )
  )

if (anyDuplicated(segment_features$segment_id) > 0) {
  stop("Segment features must be unique by segment ID.")
}

project_sf <- project_sf |>
  dplyr::left_join(
    segment_features,
    by = "segment_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    simple_overlap_keep = pmax(expressway_share, water_share) < 0.50,
    share_based_keep = (
      physical_feature_share < 0.50 &
        expressway_share < 0.40 &
        arterial_share < 0.75
    )
  )

if (anyNA(project_sf$simple_overlap_keep) || anyNA(project_sf$share_based_keep)) {
  stop("Some projects lack segment overlap measures.")
}

classification <- project_sf |>
  sf::st_drop_geometry() |>
  dplyr::select(
    project_id,
    ward_pair,
    segment_id,
    era,
    nearest_boundary_distance_ft,
    left_endpoint_distance_m,
    right_endpoint_distance_m,
    straight_boundary,
    distance_reconstruction_error_ft,
    expressway_share,
    water_share,
    physical_feature_share,
    arterial_share,
    simple_overlap_keep,
    share_based_keep
  )

readr::write_csv(
  classification,
  "../output/density_straight_boundary_classification.csv"
)

sample_rules <- tibble::tribble(
  ~boundary_sample, ~keep_variable,
  "Current boundary sample", "all",
  "Less than 50 percent expressway or water overlap", "simple_overlap_keep",
  "Share-based feature restriction", "share_based_keep",
  "Straight local boundary", "straight_boundary",
  "Straight and share-based feature restriction", "straight_share"
)

panel_specs <- tidyr::crossing(
  sample = c("all", "multifamily"),
  outcome = c("density_far", "density_dupac")
)

results <- list()
model_frame <- sf::st_drop_geometry(project_sf)

for (rule_i in seq_len(nrow(sample_rules))) {
  for (panel_i in seq_len(nrow(panel_specs))) {
    keep <- switch(
      sample_rules$keep_variable[rule_i],
      all = rep(TRUE, nrow(model_frame)),
      simple_overlap_keep = model_frame$simple_overlap_keep,
      share_based_keep = model_frame$share_based_keep,
      straight_boundary = model_frame$straight_boundary,
      straight_share = (
        model_frame$straight_boundary &
          model_frame$share_based_keep
      )
    )

    model_data <- model_frame |>
      dplyr::filter(
        keep,
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
      cluster = ~ward_pair,
      warn = FALSE,
      notes = FALSE
    )

    coefficient_table <- fixest::coeftable(model)
    coefficient_name <- "distance_bin::bin_06"
    if (!coefficient_name %in% rownames(coefficient_table)) {
      stop("The nearest more-stringent distance bin was not estimated.")
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
      n_segments = dplyr::n_distinct(model_data$segment_id)
    )
  }
}

readr::write_csv(
  dplyr::bind_rows(results),
  "../output/density_straight_boundary_results.csv"
)
