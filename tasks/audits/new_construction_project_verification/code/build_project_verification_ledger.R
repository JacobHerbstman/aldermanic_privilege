# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../input/eligibility_uncorroborated_retained.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character(),
    project_id = readr::col_character(),
    construction_year = readr::col_double(),
    x_3435 = readr::col_double(),
    y_3435 = readr::col_double(),
    dwelling_units = readr::col_double(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    current_multifamily = readr::col_logical(),
    distance_to_boundary_ft = readr::col_double(),
    externally_reviewed = readr::col_logical(),
    within_500ft = readr::col_logical()
  )
) |>
  dplyr::filter(within_500ft) |>
  dplyr::select(
    project_id,
    source_family,
    source_addresses,
    addresses,
    current_property_addresses,
    component_pins,
    construction_year,
    x_3435,
    y_3435,
    dwelling_units,
    building_sqft,
    land_sqft,
    current_multifamily,
    distance_to_boundary_ft,
    ward_pair,
    decision_source,
    externally_reviewed,
    reviewer_notes,
    rule_evidence
  ) |>
  dplyr::mutate(
    review_address = dplyr::coalesce(
      source_addresses,
      addresses,
      current_property_addresses
    )
  )

if (nrow(projects) != 795L || anyDuplicated(projects$project_id)) {
  stop("The verification scope is not the expected 795 unique projects.")
}

project_components <- projects |>
  dplyr::select(project_id, construction_year, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::filter(!is.na(component_pins), component_pins != "") |>
  dplyr::distinct()

historical_addresses <- readr::read_csv(
  "../input/density_historical_address_records.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    year = readr::col_double(),
    property_address = readr::col_character(),
    .default = readr::col_skip()
  )
)

historical_address_rows <- vector("list", nrow(project_components))
for (i in seq_len(nrow(project_components))) {
  candidates <- historical_addresses |>
    dplyr::filter(pin == project_components$component_pins[i])
  if (nrow(candidates) == 0L) {
    historical_address_rows[[i]] <- tibble::tibble(
      project_id = project_components$project_id[i],
      selected_historical_address = NA_character_,
      selected_historical_address_year = NA_real_
    )
    next
  }
  candidates <- candidates |>
    dplyr::mutate(
      year_gap = abs(
        year - project_components$construction_year[i]
      )
    ) |>
    dplyr::arrange(year_gap, year, property_address) |>
    dplyr::slice(1)
  historical_address_rows[[i]] <- tibble::tibble(
    project_id = project_components$project_id[i],
    selected_historical_address = candidates$property_address,
    selected_historical_address_year = candidates$year
  )
}

historical_project_addresses <- dplyr::bind_rows(
  historical_address_rows
) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    selected_historical_address = paste(
      sort(unique(
        selected_historical_address[
          !is.na(selected_historical_address) &
            selected_historical_address != ""
        ]
      )),
      collapse = " / "
    ),
    selected_historical_address_year = suppressWarnings(min(
      selected_historical_address_year,
      na.rm = TRUE
    )),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    selected_historical_address = dplyr::na_if(
      selected_historical_address,
      ""
    ),
    selected_historical_address_year = dplyr::if_else(
      is.infinite(selected_historical_address_year),
      NA_real_,
      selected_historical_address_year
    )
  )

current_addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    prop_address_full = readr::col_character(),
    .default = readr::col_skip()
  )
)
if (anyDuplicated(current_addresses$pin)) {
  stop("The current parcel-address file is not uniquely keyed by PIN.")
}

current_project_addresses <- project_components |>
  dplyr::left_join(
    current_addresses,
    by = c("component_pins" = "pin"),
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    current_pin_address = paste(
      sort(unique(
        prop_address_full[
          !is.na(prop_address_full) &
            prop_address_full != ""
        ]
      )),
      collapse = " / "
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    current_pin_address = dplyr::na_if(current_pin_address, "")
  )

projects <- projects |>
  dplyr::left_join(
    historical_project_addresses,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    current_project_addresses,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    review_address = dplyr::coalesce(
      review_address,
      selected_historical_address,
      current_pin_address
    )
  )

permit_evidence <- readr::read_csv(
  "../input/multicard_permit_adjudication_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character(),
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    construction_year = readr::col_double(),
    permit_year = readr::col_double(),
    intersects_project_polygon = readr::col_logical(),
    exact_address_match = readr::col_logical()
  )
) |>
  dplyr::semi_join(
    projects |>
      dplyr::select(project_id),
    by = "project_id"
  ) |>
  dplyr::mutate(
    work_description_upper = stringr::str_squish(
      stringr::str_to_upper(dplyr::coalesce(work_description, ""))
    ),
    explicit_new_building = stringr::str_detect(
      work_description_upper,
      paste0(
        "\\bNEW CONSTRUCTION\\b|",
        "\\bCONSTRUCTION OF (?:A |AN )?NEW\\b|",
        "\\bCONSTRUCT(?:ION)? (?:A |AN )?NEW\\b|",
        "\\bERECT (?:A |AN )?NEW\\b|",
        "\\bERECT NEW\\b"
      )
    ),
    site_supported =
      dplyr::coalesce(intersects_project_polygon, FALSE) |
      dplyr::coalesce(exact_address_match, FALSE),
    year_supported =
      is.finite(permit_year) &
      abs(permit_year - construction_year) <= 1,
    construction_supported =
      explicit_new_building &
      site_supported &
      year_supported
  ) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    adjudicated_new_building_permit =
      any(construction_supported),
    adjudicated_new_building_permit_ids = paste(
      sort(unique(permit_id[construction_supported])),
      collapse = "/"
    ),
    adjudicated_new_building_permit_addresses = paste(
      sort(unique(permit_address[construction_supported])),
      collapse = " / "
    ),
    adjudicated_new_building_permit_descriptions = paste(
      sort(unique(work_description[construction_supported])),
      collapse = " || "
    ),
    .groups = "drop"
  )

external_evidence <- readr::read_csv(
  "../input/multicard_external_review_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character(),
    project_id = readr::col_character()
  )
) |>
  dplyr::filter(review_status == "complete") |>
  dplyr::semi_join(
    projects |>
      dplyr::select(project_id),
    by = "project_id"
  ) |>
  dplyr::transmute(
    project_id,
    external_project_corroboration = TRUE,
    external_review_address = review_address,
    external_source_1_url = source_1_url,
    external_source_2_url = source_2_url,
    external_reviewer_notes = reviewer_notes
  )

if (
  anyDuplicated(permit_evidence$project_id) ||
    anyDuplicated(external_evidence$project_id)
) {
  stop("Project permit or external evidence is not uniquely keyed.")
}

project_polygons <- sf::st_read(
  "../input/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::inner_join(
    projects |>
      dplyr::select(project_id),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::arrange(match(project_id, projects$project_id))

if (
  anyDuplicated(project_polygons$project_id) ||
    any(!sf::st_is_valid(project_polygons))
) {
  stop("Available project polygons are duplicated or invalid.")
}

project_points <- projects |>
  dplyr::anti_join(
    sf::st_drop_geometry(project_polygons) |>
      dplyr::select(project_id),
    by = "project_id"
  )
if (
  any(!is.finite(project_points$x_3435)) ||
    any(!is.finite(project_points$y_3435))
) {
  stop("Projects without polygons also lack audited coordinates.")
}
project_points <- project_points |>
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = TRUE
  ) |>
  dplyr::mutate(
    fallback_radius = sqrt(
      dplyr::coalesce(land_sqft, pi * 50^2) / pi
    ),
    fallback_radius = pmax(pmin(fallback_radius, 200), 25)
  )

project_sites <- dplyr::bind_rows(
  project_polygons |>
    dplyr::mutate(
      geometry_method = "historical_parcel_polygon"
    ) |>
    dplyr::select(project_id, geometry_method),
  sf::st_buffer(project_points, project_points$fallback_radius) |>
    dplyr::mutate(
      geometry_method = "audited_coordinate_area_buffer"
    ) |>
    dplyr::select(project_id, geometry_method)
) |>
  dplyr::arrange(match(project_id, projects$project_id))

if (
  nrow(project_sites) != nrow(projects) ||
    anyDuplicated(project_sites$project_id)
) {
  stop("Project verification sites are incomplete or duplicated.")
}

project_longitude_latitude <- projects |>
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = FALSE
  ) |>
  sf::st_transform(4326)
longitude_latitude <- sf::st_coordinates(
  project_longitude_latitude
)

spatial_permits <- sf::st_read(
  "../input/building_permits_clean.gpkg",
  query = paste(
    "SELECT",
    "id, permit, permit_status, permit_issued,",
    "application_start_date, issue_date,",
    "street_number, street_direction, street_name,",
    "work_description, geom",
    "FROM building_permits_clean",
    "WHERE permit_type = 'PERMIT - NEW CONSTRUCTION'",
    "AND permit_issued = 1"
  ),
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::mutate(
    application_year = as.integer(substr(
      application_start_date,
      1,
      4
    )),
    work_description_upper = stringr::str_squish(
      stringr::str_to_upper(dplyr::coalesce(work_description, ""))
    ),
    addition_or_accessory_scope = stringr::str_detect(
      work_description_upper,
      paste0(
        "\\b(ADDITION|ADDITIONS)\\b.*\\bEXISTING\\b|",
        "\\bEXISTING\\b.*\\b(ADDITION|ADDITIONS)\\b|",
        "\\bNEW (GARAGE|PORCH|DECK|FENCE|CANOPY|VESTIBULE|",
        "STAIR|STAIRS)\\b"
      )
    ),
    existing_building_scope = stringr::str_detect(
      work_description_upper,
      paste0(
        "\\b(DECONVERSION|CONVERSION|ALTERATION|ALTERATIONS|",
        "MODIFICATION|MODIFICATIONS|REMODEL|REMODELING|",
        "RENOVATION|RENOVATIONS|REHAB|BUILDOUT|BUILD-OUT)\\b",
        ".*\\bEXISTING\\b|",
        "\\bEXISTING\\b.*\\b(DECONVERSION|CONVERSION|",
        "ALTERATION|ALTERATIONS|MODIFICATION|MODIFICATIONS|",
        "REMODEL|REMODELING|RENOVATION|RENOVATIONS|REHAB|",
        "BUILDOUT|BUILD-OUT)\\b"
      )
    ),
    permit_scope_eligible =
      !addition_or_accessory_scope &
      !existing_building_scope
  )

spatial_permit_indices <- sf::st_is_within_distance(
  project_sites,
  spatial_permits,
  dist = 25
)

normalize_address <- function(address) {
  address |>
    stringr::str_to_upper() |>
    stringr::str_remove("\\bCHICAGO\\b.*$") |>
    stringr::str_remove("\\b(APT|UNIT|SUITE)\\b.*$") |>
    stringr::str_remove_all("[^A-Z0-9]")
}

project_address_keys <- lapply(
  projects$review_address,
  function(address) {
    if (is.na(address) || address == "") {
      return(character())
    }
    normalize_address(
      stringr::str_split(address, " / ", simplify = FALSE)[[1]]
    )
  }
)

spatial_permit_candidate_rows <- list()
candidate_index <- 0L
for (i in seq_len(nrow(projects))) {
  indices <- spatial_permit_indices[[i]]
  if (length(indices) == 0L) {
    next
  }

  candidates <- spatial_permits[indices, ] |>
    dplyr::mutate(
      year_supported =
        is.finite(application_year) &
        abs(application_year - projects$construction_year[i]) <= 1,
      construction_supported =
        year_supported &
        permit_scope_eligible
    ) |>
    dplyr::filter(construction_supported)
  if (nrow(candidates) == 0L) {
    next
  }

  permit_addresses <- paste(
    candidates$street_number,
    candidates$street_direction,
    candidates$street_name
  )
  distances <- as.numeric(sf::st_distance(
    project_sites[i, ],
    candidates,
    by_element = FALSE
  ))
  candidate_index <- candidate_index + 1L
  spatial_permit_candidate_rows[[candidate_index]] <- tibble::tibble(
    project_id = projects$project_id[i],
    permit_id = as.character(candidates$id),
    permit_number = candidates$permit,
    permit_address = permit_addresses,
    permit_description = candidates$work_description,
    exact_address_match =
      normalize_address(permit_addresses) %in%
        project_address_keys[[i]],
    distance_to_site_ft = distances
  )
}

spatial_permit_candidates <- dplyr::bind_rows(
  spatial_permit_candidate_rows
)
permit_assignments <- vector(
  "list",
  dplyr::n_distinct(spatial_permit_candidates$permit_id)
)
assignment_index <- 0L
for (
  permit_id in sort(unique(spatial_permit_candidates$permit_id))
) {
  candidates <- spatial_permit_candidates |>
    dplyr::filter(.data$permit_id == !!permit_id) |>
    dplyr::arrange(distance_to_site_ft, project_id)

  exact_candidates <- candidates |>
    dplyr::filter(exact_address_match)
  selected <- NULL
  assignment_method <- NA_character_
  if (nrow(exact_candidates) == 1L) {
    selected <- exact_candidates[1, ]
    assignment_method <- "unique_exact_address"
  } else if (nrow(candidates) == 1L) {
    selected <- candidates[1, ]
    assignment_method <- "unique_project_site"
  } else if (
    nrow(exact_candidates) == 0L &&
      nrow(candidates) >= 2L &&
      all(is.finite(candidates$distance_to_site_ft[1:2])) &&
      candidates$distance_to_site_ft[2] -
        candidates$distance_to_site_ft[1] > 5
  ) {
    selected <- candidates[1, ]
    assignment_method <- "uniquely_nearest_site"
  }

  if (is.null(selected)) {
    next
  }
  assignment_index <- assignment_index + 1L
  permit_assignments[[assignment_index]] <- selected |>
    dplyr::mutate(assignment_method = assignment_method)
}
permit_assignments <- dplyr::bind_rows(
  permit_assignments[seq_len(assignment_index)]
)

if (anyDuplicated(permit_assignments$permit_id)) {
  stop("A spatial permit was assigned to more than one project.")
}

spatial_permit_evidence <- projects |>
  dplyr::select(project_id) |>
  dplyr::left_join(
    permit_assignments |>
      dplyr::group_by(project_id) |>
      dplyr::summarise(
        spatial_new_building_permit = TRUE,
        spatial_new_building_permit_ids = paste(
          sort(unique(permit_number)),
          collapse = "/"
        ),
        spatial_new_building_permit_addresses = paste(
          sort(unique(permit_address)),
          collapse = " / "
        ),
        spatial_new_building_permit_descriptions = paste(
          sort(unique(permit_description)),
          collapse = " || "
        ),
        spatial_new_building_permit_assignment_methods = paste(
          sort(unique(assignment_method)),
          collapse = "/"
        ),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    spatial_new_building_permit =
      dplyr::coalesce(spatial_new_building_permit, FALSE)
  )

if (
  nrow(spatial_permit_evidence) != nrow(projects) ||
    anyDuplicated(spatial_permit_evidence$project_id)
) {
  stop("Spatial permit evidence is incomplete or duplicated.")
}

footprints_2008 <- sf::st_read(
  "../output/official_building_footprints_2008.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::rename(
    building_id = BLDG_ID,
    reported_year_built = YEAR_BUILT
  ) |>
  dplyr::mutate(
    building_id = as.character(building_id),
    reported_year_built = as.integer(reported_year_built),
    reported_year_built = dplyr::if_else(
      dplyr::between(reported_year_built, 1800L, 2008L),
      reported_year_built,
      NA_integer_
    )
  ) |>
  dplyr::select(building_id, reported_year_built)

footprints_2015 <- sf::st_read(
  "../output/official_building_footprints_2015.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::transmute(
    building_id = as.character(BLDG_ID),
    reported_year_built = as.integer(YEAR_BUILT),
    reported_year_built = dplyr::if_else(
      dplyr::between(reported_year_built, 1800L, 2015L),
      reported_year_built,
      NA_integer_
    )
  )

footprints_2022 <- sf::st_read(
  "../output/official_building_footprints_2022.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)

id_2022 <- names(footprints_2022)[
  tolower(names(footprints_2022)) %in% c("objectid", "globalid")
][1]
if (length(id_2022) != 1L || is.na(id_2022)) {
  stop("The 2022 footprint snapshot lacks a building identifier.")
}
footprints_2022 <- footprints_2022 |>
  dplyr::transmute(
    building_id = as.character(.data[[id_2022]]),
    reported_year_built = NA_integer_
  )

summarize_snapshot <- function(footprints, label) {
  candidate_indices <- sf::st_intersects(project_sites, footprints)
  empty_geometry <- sf::st_sfc(sf::st_polygon(), crs = 3435)[[1]]
  clipped_geometries <- vector("list", nrow(project_sites))
  rows <- vector("list", nrow(project_sites))

  for (i in seq_len(nrow(project_sites))) {
    indices <- candidate_indices[[i]]
    if (length(indices) == 0L) {
      clipped_geometries[[i]] <- empty_geometry
      rows[[i]] <- tibble::tibble(
        project_id = project_sites$project_id[i],
        matched_footprints = 0L,
        covered_area_sqft = 0,
        footprint_ids = "",
        reported_year_values = ""
      )
      next
    }

    clipped <- suppressWarnings(
      sf::st_intersection(
        sf::st_geometry(footprints[indices, ]),
        sf::st_geometry(project_sites[i, ])
      )
    )
    clipped <- clipped[!sf::st_is_empty(clipped)]
    if (length(clipped) == 0L) {
      clipped_geometries[[i]] <- empty_geometry
      rows[[i]] <- tibble::tibble(
        project_id = project_sites$project_id[i],
        matched_footprints = 0L,
        covered_area_sqft = 0,
        footprint_ids = "",
        reported_year_values = ""
      )
      next
    }

    clipped_geometries[[i]] <- sf::st_union(clipped)[[1]]
    years <- footprints$reported_year_built[indices]
    rows[[i]] <- tibble::tibble(
      project_id = project_sites$project_id[i],
      matched_footprints = length(indices),
      covered_area_sqft = as.numeric(
        sf::st_area(clipped_geometries[[i]])
      ),
      footprint_ids = paste(
        sort(unique(footprints$building_id[indices])),
        collapse = "/"
      ),
      reported_year_values = paste(
        sort(unique(years[is.finite(years)])),
        collapse = "/"
      )
    )
  }

  summary <- dplyr::bind_rows(rows)
  names(summary)[-1] <- paste0(label, "_", names(summary)[-1])
  list(summary = summary, geometry = clipped_geometries)
}

snapshot_2008 <- summarize_snapshot(footprints_2008, "snapshot_2008")
snapshot_2015 <- summarize_snapshot(footprints_2015, "snapshot_2015")
snapshot_2022 <- summarize_snapshot(footprints_2022, "snapshot_2022")

overlap_share <- function(first_geometry, second_geometry) {
  if (
    sf::st_is_empty(first_geometry) ||
      sf::st_is_empty(second_geometry)
  ) {
    return(0)
  }
  first_area <- as.numeric(sf::st_area(first_geometry))
  second_area <- as.numeric(sf::st_area(second_geometry))
  if (min(first_area, second_area) == 0) {
    return(0)
  }
  as.numeric(
    sf::st_area(
      suppressWarnings(sf::st_intersection(
        first_geometry,
        second_geometry
      ))
    )
  ) / min(first_area, second_area)
}

overlap_2008_2015 <- vapply(
  seq_len(nrow(projects)),
  function(i) {
    overlap_share(
      snapshot_2008$geometry[[i]],
      snapshot_2015$geometry[[i]]
    )
  },
  numeric(1)
)
overlap_2015_2022 <- vapply(
  seq_len(nrow(projects)),
  function(i) {
    overlap_share(
      snapshot_2015$geometry[[i]],
      snapshot_2022$geometry[[i]]
    )
  },
  numeric(1)
)

parse_year_values <- function(values) {
  if (is.na(values) || values == "") {
    return(integer())
  }
  as.integer(strsplit(values, "/", fixed = TRUE)[[1]])
}

ledger <- projects |>
  dplyr::left_join(
    permit_evidence,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    external_evidence,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    spatial_permit_evidence,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    snapshot_2008$summary,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    snapshot_2015$summary,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    snapshot_2022$summary,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    geometry_method = project_sites$geometry_method,
    project_polygon_area_sqft = as.numeric(
      sf::st_area(project_sites)
    ),
    snapshot_2008_coverage = pmin(
      snapshot_2008_covered_area_sqft / project_polygon_area_sqft,
      1
    ),
    snapshot_2015_coverage = pmin(
      snapshot_2015_covered_area_sqft / project_polygon_area_sqft,
      1
    ),
    snapshot_2022_coverage = pmin(
      snapshot_2022_covered_area_sqft / project_polygon_area_sqft,
      1
    ),
    snapshot_2008_2015_overlap_share = overlap_2008_2015,
    snapshot_2015_2022_overlap_share = overlap_2015_2022,
    prior_snapshot = dplyr::case_when(
      construction_year <= 2008 ~ NA_character_,
      construction_year <= 2015 ~ "2008",
      TRUE ~ "2015"
    ),
    later_snapshot = dplyr::case_when(
      construction_year <= 2008 ~ "2008",
      construction_year <= 2015 ~ "2015",
      TRUE ~ "2022"
    ),
    prior_coverage = dplyr::case_when(
      construction_year <= 2008 ~ NA_real_,
      construction_year <= 2015 ~ snapshot_2008_coverage,
      TRUE ~ snapshot_2015_coverage
    ),
    later_coverage = dplyr::case_when(
      construction_year <= 2008 ~ snapshot_2008_coverage,
      construction_year <= 2015 ~ snapshot_2015_coverage,
      TRUE ~ snapshot_2022_coverage
    ),
    later_footprint_area_sqft = dplyr::case_when(
      construction_year <= 2008 ~ snapshot_2008_covered_area_sqft,
      construction_year <= 2015 ~ snapshot_2015_covered_area_sqft,
      TRUE ~ snapshot_2022_covered_area_sqft
    ),
    prior_later_overlap_share = dplyr::case_when(
      construction_year <= 2008 ~ NA_real_,
      construction_year <= 2015 ~
        snapshot_2008_2015_overlap_share,
      TRUE ~ snapshot_2015_2022_overlap_share
    )
  )

year_corroborated <- vapply(
  seq_len(nrow(ledger)),
  function(i) {
    values <- if (ledger$construction_year[i] <= 2008) {
      ledger$snapshot_2008_reported_year_values[i]
    } else if (ledger$construction_year[i] <= 2015) {
      ledger$snapshot_2015_reported_year_values[i]
    } else {
      ""
    }
    years <- parse_year_values(values)
    any(abs(years - ledger$construction_year[i]) <= 1L)
  },
  logical(1)
)
maximum_2008_reported_year <- vapply(
  ledger$snapshot_2008_reported_year_values,
  function(values) {
    years <- parse_year_values(values)
    if (length(years) == 0L) {
      return(NA_real_)
    }
    max(years)
  },
  numeric(1)
)

ledger <- ledger |>
  dplyr::mutate(
    footprint_year_corroborated = year_corroborated,
    maximum_2008_reported_year = maximum_2008_reported_year,
    adjudicated_new_building_permit =
      dplyr::coalesce(adjudicated_new_building_permit, FALSE),
    external_project_corroboration =
      dplyr::coalesce(external_project_corroboration, FALSE),
    spatial_new_building_permit =
      dplyr::coalesce(spatial_new_building_permit, FALSE),
    strong_snapshot_appearance =
      geometry_method == "historical_parcel_polygon" &
      is.finite(prior_coverage) &
      prior_coverage <= 0.10 &
      later_coverage >= 0.20 &
      later_footprint_area_sqft >= 400,
    strong_snapshot_replacement =
      geometry_method == "historical_parcel_polygon" &
      is.finite(prior_coverage) &
      prior_coverage > 0.10 &
      later_coverage >= 0.20 &
      later_footprint_area_sqft >= 400 &
      prior_later_overlap_share <= 0.25,
    snapshot_reports_older_structure =
      construction_year <= 2008 &
      snapshot_2008_coverage >= 0.20 &
      is.finite(maximum_2008_reported_year) &
      maximum_2008_reported_year <= construction_year - 2,
    persistent_prior_footprint =
      is.finite(prior_coverage) &
      prior_coverage >= 0.20 &
      later_coverage >= 0.20 &
      prior_later_overlap_share >= 0.75,
    possible_contradictory_snapshot =
      snapshot_reports_older_structure |
      persistent_prior_footprint,
    verification_status = dplyr::case_when(
      adjudicated_new_building_permit ~
        "adjudicated_new_building_permit",
      spatial_new_building_permit ~
        "spatial_new_building_permit",
      strong_snapshot_appearance ~
        "independent_snapshot_appearance",
      strong_snapshot_replacement ~
        "independent_snapshot_replacement",
      footprint_year_corroborated ~
        "official_footprint_year_corroboration",
      external_project_corroboration ~
        "external_project_corroboration",
      TRUE ~ "unresolved_after_official_snapshots"
    ),
    verification_strength = dplyr::case_when(
      verification_status ==
        "adjudicated_new_building_permit" ~
        "strong_independent",
      verification_status ==
        "spatial_new_building_permit" ~
        "strong_independent",
      stringr::str_starts(verification_status, "independent_") ~
        "strong_independent",
      verification_status %in% c(
        "official_footprint_year_corroboration",
        "external_project_corroboration"
      ) ~
        "corroborating_not_independent",
      TRUE ~ "unresolved"
    ),
    verification_evidence = dplyr::case_when(
      adjudicated_new_building_permit ~ paste0(
        "Adjudicated site-linked permit ",
        adjudicated_new_building_permit_ids,
        " describes new construction within one year of ",
        construction_year,
        "."
      ),
      spatial_new_building_permit ~ paste0(
        "Issued City new-construction permit ",
        spatial_new_building_permit_ids,
        " is within 25ft of the historical project site and within one ",
        "year of ",
        construction_year,
        "."
      ),
      strong_snapshot_appearance ~ paste0(
        "Footprint coverage rises from ",
        round(100 * prior_coverage, 1),
        "% in ",
        prior_snapshot,
        " to ",
        round(100 * later_coverage, 1),
        "% in ",
        later_snapshot,
        "."
      ),
      strong_snapshot_replacement ~ paste0(
        "The ",
        prior_snapshot,
        " and ",
        later_snapshot,
        " site footprints overlap by ",
        round(100 * prior_later_overlap_share, 1),
        "% of the smaller footprint."
      ),
      footprint_year_corroborated ~ paste0(
        "The official ",
        later_snapshot,
        " footprint record reports a year built within one year of ",
        construction_year,
        "."
      ),
      external_project_corroboration ~ paste0(
        "Completed external review identifies the project at ",
        dplyr::coalesce(external_review_address, review_address),
        ", but does not independently establish its exact construction year."
      ),
      TRUE ~ paste0(
        "The official snapshots do not independently establish a new or ",
        "replacement structure in ",
        construction_year,
        "."
      )
    ),
    manual_review_priority = dplyr::case_when(
      verification_strength != "unresolved" ~ "not_required",
      current_multifamily & possible_contradictory_snapshot ~ "1",
      current_multifamily ~ "2",
      possible_contradictory_snapshot ~ "3",
      TRUE ~ "4"
    ),
    google_maps_url = paste0(
      "https://www.google.com/maps/search/?api=1&query=",
      round(longitude_latitude[, 2], 7),
      "%2C",
      round(longitude_latitude[, 1], 7)
    ),
    google_search_query = paste0(
      '"',
      dplyr::coalesce(review_address, component_pins),
      '" Chicago "',
      construction_year,
      '" construction'
    )
  ) |>
  dplyr::arrange(
    factor(manual_review_priority, levels = c("1", "2", "3", "4", "not_required")),
    construction_year,
    project_id
  )

if (
  nrow(ledger) != 795L ||
    anyDuplicated(ledger$project_id) ||
    any(is.na(ledger$verification_status))
) {
  stop("The project verification ledger failed its row-level contract.")
}

summary <- ledger |>
  dplyr::count(
    verification_status,
    verification_strength,
    source_family,
    current_multifamily,
    name = "projects"
  ) |>
  dplyr::arrange(
    verification_strength,
    source_family,
    dplyr::desc(current_multifamily)
  )

review_queue <- ledger |>
  dplyr::filter(verification_strength == "unresolved") |>
  dplyr::arrange(
    manual_review_priority,
    construction_year,
    project_id
  )

sensitivity <- tidyr::crossing(
  maximum_prior_coverage = c(0.05, 0.10, 0.20),
  minimum_later_coverage = c(0.15, 0.20, 0.30),
  maximum_overlap_share = c(0.15, 0.25, 0.35)
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    appearance_projects = sum(
      is.finite(ledger$prior_coverage) &
        ledger$prior_coverage <= maximum_prior_coverage &
        ledger$later_coverage >= minimum_later_coverage &
        ledger$later_footprint_area_sqft >= 400
    ),
    replacement_projects = sum(
      is.finite(ledger$prior_coverage) &
        ledger$prior_coverage > maximum_prior_coverage &
        ledger$later_coverage >= minimum_later_coverage &
        ledger$later_footprint_area_sqft >= 400 &
        ledger$prior_later_overlap_share <= maximum_overlap_share
    ),
    year_corroborated_projects = sum(
      ledger$footprint_year_corroborated
    ),
    unresolved_projects = sum(
      !(
        (
          is.finite(ledger$prior_coverage) &
            ledger$prior_coverage <= maximum_prior_coverage &
            ledger$later_coverage >= minimum_later_coverage &
            ledger$later_footprint_area_sqft >= 400
        ) |
          (
            is.finite(ledger$prior_coverage) &
              ledger$prior_coverage > maximum_prior_coverage &
              ledger$later_coverage >= minimum_later_coverage &
              ledger$later_footprint_area_sqft >= 400 &
              ledger$prior_later_overlap_share <= maximum_overlap_share
          ) |
          ledger$footprint_year_corroborated
      )
    )
  ) |>
  dplyr::ungroup()

readr::write_csv(
  ledger,
  "../output/project_verification_ledger.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/project_verification_summary.csv",
  na = ""
)
readr::write_csv(
  review_queue,
  "../output/project_verification_review_queue.csv",
  na = ""
)
readr::write_csv(
  sensitivity,
  "../output/snapshot_rule_sensitivity.csv",
  na = ""
)
