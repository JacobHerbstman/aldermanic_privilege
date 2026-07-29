# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_boundary_design_checks/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/amenity_distance_helpers.R")

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
    true_distance_ft = signed_distance_m / 0.3048,
    distance_bin = cut(
      true_distance_ft,
      breaks = seq(-500, 500, by = 100),
      labels = sprintf("bin_%02d", 1:10),
      include.lowest = TRUE,
      right = FALSE
    )
  )

if (anyDuplicated(projects$project_id) > 0) {
  stop("The 500-foot density sample must be unique by project ID.")
}

straightness <- readr::read_csv(
  "../output/density_straight_boundary_classification.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id,
    straight_boundary,
    simple_overlap_keep,
    share_based_keep
  )

if (anyDuplicated(straightness$project_id) > 0) {
  stop("Straight-boundary classifications must be unique by project ID.")
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
  dplyr::inner_join(projects, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(straightness, by = "project_id", relationship = "one-to-one")

if (nrow(project_sf) != nrow(projects) || anyNA(project_sf$straight_boundary)) {
  stop("Some density projects lack centroids or straight-boundary classifications.")
}

schools <- read_amenity_layer("../input/schools_2015.gpkg")
parks <- read_amenity_layer("../input/parks.gpkg")
major_streets <- read_amenity_layer("../input/major_streets.gpkg")
lake <- lake_michigan_geom("../input/gis_osm_water_a_free_1.shp")
cbd <- sf::st_sfc(
  sf::st_point(c(-87.6313, 41.8837)),
  crs = 4326
) |>
  sf::st_transform(3435)

project_sf$distance_to_cbd_miles <- as.numeric(
  sf::st_distance(project_sf, cbd)
) / 5280
project_sf$distance_to_school_miles <- nearest_distance_ft(
  project_sf,
  schools,
  label = "density projects"
) / 5280
project_sf$distance_to_park_miles <- nearest_distance_ft(
  project_sf,
  parks,
  label = "density projects"
) / 5280
project_sf$distance_to_major_road_miles <- nearest_distance_ft(
  project_sf,
  major_streets,
  label = "density projects"
) / 5280
project_sf$distance_to_lake_miles <- nearest_distance_ft(
  project_sf,
  lake,
  label = "density projects"
) / 5280

readr::write_csv(
  project_sf |>
    sf::st_drop_geometry() |>
    dplyr::select(
      project_id,
      straight_boundary,
      simple_overlap_keep,
      share_based_keep,
      distance_to_cbd_miles,
      distance_to_school_miles,
      distance_to_park_miles,
      distance_to_lake_miles
    ),
  "../output/density_boundary_characteristics.csv"
)

block_groups <- sf::st_read(
  "../input/block_group_geometry_2019.gpkg",
  quiet = TRUE
) |>
  sf::st_make_valid() |>
  sf::st_transform(3435) |>
  dplyr::mutate(GEOID = as.character(GEOID))

if (anyDuplicated(block_groups$GEOID) > 0) {
  stop("Block-group geometry must be unique by GEOID.")
}

block_group_hits <- sf::st_intersects(project_sf, block_groups)
hit_counts <- lengths(block_group_hits)
if (any(hit_counts != 1L)) {
  stop(sprintf(
    "Block-group assignment is not unique: %s missing and %s multiple matches.",
    sum(hit_counts == 0L),
    sum(hit_counts > 1L)
  ))
}
project_sf$GEOID <- block_groups$GEOID[
  vapply(block_group_hits, `[[`, integer(1), 1L)
]

block_group_controls <- readr::read_csv(
  "../input/block_group_controls.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    GEOID = readr::col_character(),
    year = readr::col_integer(),
    .default = readr::col_double()
  )
) |>
  dplyr::rename(
    percent_white_bg = percent_white,
    percent_black_bg = percent_black,
    percent_hispanic_bg = percent_hispanic,
    homeownership_rate_bg = homeownership_rate,
    median_rent_bg = median_rent,
    median_home_value_bg = median_home_value,
    median_income_bg = median_income,
    share_bach_plus_bg = share_bach_plus,
    avg_household_size_bg = avg_household_size,
    median_age_bg = median_age,
    population_density_bg = population_density
  )

if (anyDuplicated(block_group_controls[c("GEOID", "year")]) > 0) {
  stop("Block-group controls must be unique by GEOID and year.")
}

project_sf <- project_sf |>
  dplyr::left_join(
    block_group_controls,
    by = c("GEOID", "construction_year" = "year"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    acs_period = if_else(construction_year < 2015L, "2014 ACS", "2019 ACS")
  )

amenity_catalog <- tibble::tribble(
  ~covariate, ~label, ~scale,
  "distance_to_cbd_miles", "Distance to downtown (miles)", 1,
  "distance_to_school_miles", "Distance to school (miles)", 1,
  "distance_to_park_miles", "Distance to park (miles)", 1,
  "distance_to_major_road_miles", "Distance to major road (miles)", 1,
  "distance_to_lake_miles", "Distance to Lake Michigan (miles)", 1
)

acs_catalog <- tibble::tribble(
  ~covariate, ~label, ~scale,
  "percent_white_bg", "Share White", 1,
  "percent_black_bg", "Share Black", 1,
  "percent_hispanic_bg", "Share Hispanic", 1,
  "homeownership_rate_bg", "Homeownership rate", 1,
  "median_income_bg", "Median household income ($10,000)", 10000,
  "share_bach_plus_bg", "Bachelor's degree or higher share", 1,
  "median_rent_bg", "Median gross rent ($100)", 100,
  "median_home_value_bg", "Median home value ($100,000)", 100000,
  "avg_household_size_bg", "Average household size", 1,
  "median_age_bg", "Median age", 1
)

boundary_samples <- tibble::tribble(
  ~boundary_sample, ~straight_only,
  "Current boundary sample", FALSE,
  "Straight local boundary", TRUE
)

sample_types <- c("All construction", "Multifamily")

estimate_continuity <- function(data, catalog, cluster_formula) {
  rows <- list()

  for (i in seq_len(nrow(catalog))) {
    covariate_name <- catalog$covariate[i]
    scale_value <- catalog$scale[i]
    model_data <- data |>
      dplyr::mutate(
        continuity_outcome = .data[[covariate_name]] / scale_value,
        distance_bin = droplevels(distance_bin)
      ) |>
      dplyr::filter(is.finite(continuity_outcome))

    model <- fixest::feols(
      continuity_outcome ~ i(distance_bin, ref = "bin_05") |
        segment_id + construction_year,
      data = model_data,
      cluster = cluster_formula,
      warn = FALSE,
      notes = FALSE
    )

    coefficient_table <- fixest::coeftable(model)
    coefficient_name <- "distance_bin::bin_06"
    if (!coefficient_name %in% rownames(coefficient_table)) {
      stop(sprintf(
        "The nearest more-stringent bin was not estimated for %s.",
        covariate_name
      ))
    }

    nearest_lenient <- model_data$distance_bin == "bin_05"
    nearest_stringent <- model_data$distance_bin == "bin_06"
    outcome_sd <- stats::sd(model_data$continuity_outcome)

    rows[[length(rows) + 1L]] <- tibble::tibble(
      covariate = covariate_name,
      label = catalog$label[i],
      nearest_lenient_mean = mean(
        model_data$continuity_outcome[nearest_lenient]
      ),
      nearest_stringent_mean = mean(
        model_data$continuity_outcome[nearest_stringent]
      ),
      discontinuity = coefficient_table[coefficient_name, "Estimate"],
      std_error = coefficient_table[coefficient_name, "Std. Error"],
      p_value = coefficient_table[coefficient_name, "Pr(>|t|)"],
      standardized_discontinuity = (
        coefficient_table[coefficient_name, "Estimate"] / outcome_sd
      ),
      n_projects = stats::nobs(model),
      n_ward_pairs = dplyr::n_distinct(model_data$ward_pair),
      n_segments = dplyr::n_distinct(model_data$segment_id),
      n_block_groups = dplyr::n_distinct(model_data$GEOID)
    )
  }

  dplyr::bind_rows(rows)
}

amenity_results <- list()
acs_results <- list()

for (boundary_i in seq_len(nrow(boundary_samples))) {
  for (sample_name in sample_types) {
    model_data <- project_sf |>
      sf::st_drop_geometry() |>
      dplyr::filter(
        !boundary_samples$straight_only[boundary_i] | straight_boundary,
        sample_name == "All construction" | external_multifamily
      )

    amenity_results[[length(amenity_results) + 1L]] <- estimate_continuity(
      model_data,
      amenity_catalog,
      ~ward_pair
    ) |>
      dplyr::mutate(
        boundary_sample = boundary_samples$boundary_sample[boundary_i],
        sample = sample_name,
        .before = 1
      )

    block_group_model_data <- model_data |>
      dplyr::group_by(GEOID) |>
      dplyr::mutate(
        block_group_observed_on_both_sides =
          dplyr::n_distinct(side) > 1L
      ) |>
      dplyr::ungroup()

    acs_results[[length(acs_results) + 1L]] <- estimate_continuity(
      block_group_model_data,
      acs_catalog,
      ~ward_pair + GEOID
    ) |>
      dplyr::mutate(
        boundary_sample = boundary_samples$boundary_sample[boundary_i],
        sample = sample_name,
        block_group_sample = "All assigned block groups",
        .before = 1
      )

    acs_results[[length(acs_results) + 1L]] <- estimate_continuity(
      block_group_model_data |>
        dplyr::filter(!block_group_observed_on_both_sides),
      acs_catalog,
      ~ward_pair + GEOID
    ) |>
      dplyr::mutate(
        boundary_sample = boundary_samples$boundary_sample[boundary_i],
        sample = sample_name,
        block_group_sample = "Block groups observed on one side only",
        .before = 1
      )
  }
}

amenity_results <- dplyr::bind_rows(amenity_results)
acs_results <- dplyr::bind_rows(acs_results)

readr::write_csv(
  amenity_results,
  "../output/density_amenity_continuity.csv"
)
readr::write_csv(
  acs_results,
  "../output/density_acs_continuity.csv"
)

acs_diagnostics <- list()
for (sample_name in sample_types) {
  sample_data <- project_sf |>
    sf::st_drop_geometry() |>
    dplyr::filter(
      sample_name == "All construction" | external_multifamily
    ) |>
    dplyr::group_by(GEOID) |>
    dplyr::mutate(
      block_group_spans_sides = dplyr::n_distinct(side) > 1L,
      block_group_spans_segments = dplyr::n_distinct(segment_id) > 1L,
      block_group_spans_pairs = dplyr::n_distinct(ward_pair) > 1L
    ) |>
    dplyr::ungroup()

  acs_diagnostics[[length(acs_diagnostics) + 1L]] <- tibble::tibble(
    sample = sample_name,
    project_rows = nrow(sample_data),
    unique_block_groups = dplyr::n_distinct(sample_data$GEOID),
    block_groups_spanning_both_sides = dplyr::n_distinct(
      sample_data$GEOID[sample_data$block_group_spans_sides]
    ),
    share_projects_in_both_side_block_groups = mean(
      sample_data$block_group_spans_sides
    ),
    block_groups_spanning_multiple_segments = dplyr::n_distinct(
      sample_data$GEOID[sample_data$block_group_spans_segments]
    ),
    share_projects_in_multi_segment_block_groups = mean(
      sample_data$block_group_spans_segments
    ),
    block_groups_spanning_multiple_ward_pairs = dplyr::n_distinct(
      sample_data$GEOID[sample_data$block_group_spans_pairs]
    ),
    share_projects_in_multi_pair_block_groups = mean(
      sample_data$block_group_spans_pairs
    ),
    projects_using_2014_acs = sum(sample_data$acs_period == "2014 ACS"),
    projects_using_2019_acs = sum(sample_data$acs_period == "2019 ACS")
  )
}

missing_diagnostics <- tidyr::crossing(
  sample = sample_types,
  covariate = acs_catalog$covariate
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    missing_projects = {
      sample_data <- project_sf |>
        sf::st_drop_geometry() |>
        dplyr::filter(
          sample == "All construction" | external_multifamily
        )
      sum(!is.finite(sample_data[[covariate]]))
    }
  ) |>
  dplyr::ungroup()

readr::write_csv(
  dplyr::bind_rows(acs_diagnostics) |>
    tidyr::pivot_longer(
      -sample,
      names_to = "diagnostic",
      values_to = "value"
    ) |>
    dplyr::bind_rows(
      missing_diagnostics |>
        dplyr::transmute(
          sample,
          diagnostic = paste0("missing_", covariate),
          value = missing_projects
        )
    ),
  "../output/density_acs_assignment_diagnostics.csv"
)

stars <- function(p_value) {
  dplyr::case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

table_panel <- function(data, panel_title) {
  lines <- c(
    sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", panel_title)
  )
  for (i in seq_len(nrow(data))) {
    lines <- c(
      lines,
      sprintf(
        "%s & %.3f & %.3f & %.3f%s & (%.3f) \\\\",
        data$label[i],
        data$nearest_lenient_mean[i],
        data$nearest_stringent_mean[i],
        data$discontinuity[i],
        stars(data$p_value[i]),
        data$std_error[i]
      )
    )
  }
  lines
}

write_continuity_table <- function(boundary_sample_name, output_path) {
  table_lines <- c(
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    " & Nearest less-stringent bin & Nearest more-stringent bin & Discontinuity & SE \\\\",
    "\\midrule"
  )

  for (sample_name in sample_types) {
    table_lines <- c(
      table_lines,
      table_panel(
        amenity_results |>
          dplyr::filter(
            boundary_sample == boundary_sample_name,
            sample == sample_name
          ),
        paste(sample_name, "location characteristics")
      ),
      "\\addlinespace",
      table_panel(
        acs_results |>
          dplyr::filter(
            boundary_sample == boundary_sample_name,
            sample == sample_name,
            block_group_sample == "All assigned block groups"
          ),
        paste(sample_name, "block-group characteristics")
      ),
      if (sample_name == "All construction") "\\midrule" else character()
    )
  }

  writeLines(
    c(
      table_lines,
      "\\bottomrule",
      "\\end{tabular}"
    ),
    output_path
  )
}

write_continuity_table(
  "Current boundary sample",
  "../output/density_covariate_continuity.tex"
)
write_continuity_table(
  "Straight local boundary",
  "../output/density_covariate_continuity_straight.tex"
)
