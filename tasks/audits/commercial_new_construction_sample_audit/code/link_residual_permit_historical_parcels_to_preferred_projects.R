# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

exact_matches <- readr::read_csv(
  "../output/residual_permit_historical_parcel_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    historical_pin14 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(stringr::str_length(historical_pin14) == 14L) |>
  dplyr::transmute(
    request_id,
    permit_chain_id,
    application_year,
    target_year,
    object_id,
    historical_pin14,
    historical_parcel_match_method = "containing_polygon",
    historical_parcel_distance_ft = 0
  )
nearest_matches <- readr::read_csv(
  "../output/residual_permit_validated_nearest_historical_parcel_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    historical_pin14 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(stringr::str_length(historical_pin14) == 14L) |>
  dplyr::transmute(
    request_id,
    permit_chain_id,
    application_year,
    target_year,
    object_id,
    historical_pin14,
    historical_parcel_match_method = nearest_validation_method,
    historical_parcel_distance_ft = nearest_distance_ft
  )
matches <- dplyr::bind_rows(exact_matches, nearest_matches) |>
  dplyr::distinct(
    request_id,
    object_id,
    historical_pin14,
    .keep_all = TRUE
  ) |>
  dplyr::mutate(
    parcel_key = paste(target_year, object_id, sep = ":")
  )

exact_parcels <- sf::st_read(
  "../output/residual_permit_historical_parcels.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)
nearest_parcels <- sf::st_read(
  "../output/residual_permit_nearest_historical_parcels.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)
parcels <- rbind(exact_parcels, nearest_parcels) |>
  dplyr::mutate(
    parcel_key = paste(target_year, object_id, sep = ":")
  ) |>
  dplyr::arrange(parcel_key) |>
  dplyr::distinct(parcel_key, .keep_all = TRUE)

preferred_centroids <- sf::st_read(
  "../output/preferred_new_construction_project_centroids.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)
preferred_ledger <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
chain_points <- sf::st_read(
  "../output/permit_first_unmatched_residential_queue.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::select(permit_chain_id)

if (anyDuplicated(parcels$parcel_key) ||
    anyDuplicated(preferred_centroids$project_id) ||
    anyDuplicated(preferred_ledger$project_id) ||
    anyDuplicated(chain_points$permit_chain_id) ||
    !setequal(preferred_centroids$project_id, preferred_ledger$project_id)) {
  stop("Historical parcel lineage keys are invalid.", call. = FALSE)
}

centroid_rows <- sf::st_intersects(parcels, preferred_centroids)
parcel_project_links <- purrr::map2_dfr(
  parcels$parcel_key,
  centroid_rows,
  function(parcel_key_value, project_rows) {
    if (length(project_rows) == 0) {
      return(
        tibble::tibble(
          parcel_key = parcel_key_value,
          project_id = NA_character_
        )
      )
    }
    tibble::tibble(
      parcel_key = parcel_key_value,
      project_id = preferred_centroids$project_id[project_rows]
    )
  }
) |>
  dplyr::filter(!is.na(project_id)) |>
  dplyr::left_join(
    preferred_ledger |>
      dplyr::select(
        project_id,
        source_family,
        construction_year,
        dwelling_units,
        building_sqft,
        land_sqft,
        source_addresses,
        component_pins
      ),
    by = "project_id",
    relationship = "many-to-one"
  )

links_by_parcel <- split(
  parcel_project_links,
  parcel_project_links$parcel_key
)
permit_project_links <- purrr::map_dfr(
  seq_len(nrow(matches)),
  function(match_row) {
    row <- matches[match_row, ]
    candidates <- links_by_parcel[[row$parcel_key]]
    if (is.null(candidates) || nrow(candidates) == 0) {
      return(
        row |>
          dplyr::mutate(
            project_id = NA_character_,
            source_family = NA_character_,
            construction_year = NA_real_,
            dwelling_units = NA_real_,
            building_sqft = NA_real_,
            land_sqft = NA_real_,
            source_addresses = NA_character_,
            component_pins = NA_character_
          )
      )
    }
    dplyr::bind_cols(
      row[rep(1, nrow(candidates)), ],
      candidates |>
        dplyr::select(-parcel_key)
    )
  }
) |>
  dplyr::filter(!is.na(project_id)) |>
  dplyr::mutate(
    project_year_gap = construction_year - application_year,
    plausible_same_construction_episode = dplyr::between(
      project_year_gap,
      -1L,
      4L
    )
  )

point_index <- match(
  permit_project_links$permit_chain_id,
  chain_points$permit_chain_id
)
project_index <- match(
  permit_project_links$project_id,
  preferred_centroids$project_id
)
permit_project_links$permit_to_project_centroid_ft <- units::drop_units(
  sf::st_distance(
    chain_points[point_index, ],
    preferred_centroids[project_index, ],
    by_element = TRUE
  )
)

chain_summary <- matches |>
  dplyr::distinct(permit_chain_id) |>
  dplyr::left_join(
    permit_project_links |>
      dplyr::group_by(permit_chain_id) |>
      dplyr::summarise(
        historical_parcel_project_candidates = dplyr::n_distinct(
          project_id
        ),
        plausible_historical_parcel_projects = dplyr::n_distinct(
          project_id[plausible_same_construction_episode]
        ),
        plausible_historical_project_ids = paste(
          sort(unique(project_id[
            plausible_same_construction_episode
          ])),
          collapse = "/"
        ),
        minimum_permit_to_project_centroid_ft = min(
          permit_to_project_centroid_ft,
          na.rm = TRUE
        ),
        .groups = "drop"
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        historical_parcel_project_candidates,
        plausible_historical_parcel_projects
      ),
      ~ dplyr::coalesce(.x, 0L)
    ),
    minimum_permit_to_project_centroid_ft = dplyr::if_else(
      is.infinite(minimum_permit_to_project_centroid_ft),
      NA_real_,
      minimum_permit_to_project_centroid_ft
    )
  )

summary <- dplyr::bind_rows(
  tibble::tibble(
    section = "coverage",
    metric = c(
      "permit_chains_with_historical_parcel",
      "historical_parcels",
      "permit_project_candidate_links",
      "chains_with_project_in_historical_parcel",
      "chains_with_plausible_project_episode"
    ),
    value = c(
      dplyr::n_distinct(matches$permit_chain_id),
      dplyr::n_distinct(matches$parcel_key),
      nrow(permit_project_links),
      sum(chain_summary$historical_parcel_project_candidates > 0),
      sum(chain_summary$plausible_historical_parcel_projects > 0)
    )
  )
)

readr::write_csv(
  permit_project_links,
  "../output/residual_permit_historical_parcel_project_links.csv"
)
readr::write_csv(
  chain_summary,
  "../output/residual_permit_historical_parcel_project_chain_summary.csv"
)
readr::write_csv(
  summary,
  "../output/residual_permit_historical_parcel_project_summary.csv"
)
