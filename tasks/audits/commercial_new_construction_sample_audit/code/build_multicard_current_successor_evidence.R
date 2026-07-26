# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

parse_apartments <- function(x) {
  value <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    is.na(value) | value == "" ~ NA_real_,
    value %in% c("none", "zero") ~ 0,
    value == "one" ~ 1,
    value == "two" ~ 2,
    value == "three" ~ 3,
    value == "four" ~ 4,
    value == "five" ~ 5,
    value == "six" ~ 6,
    TRUE ~ suppressWarnings(as.numeric(
      stringr::str_replace_all(value, "[^0-9.-]", "")
    ))
  )
}

projects <- readr::read_csv(
  "../output/multicard_project_evidence_base.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    geometry_project_id = readr::col_character(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id,
    geometry_project_id,
    project_pin = pin,
    construction_year,
    within_500ft
  )

construction_year_geometry <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  dplyr::inner_join(
    projects,
    by = c("project_id" = "geometry_project_id"),
    relationship = "one-to-one"
  ) |>
  dplyr::transmute(
    project_id = project_id.y,
    project_pin,
    construction_year,
    within_500ft,
    search_geometry_source = "construction_year_parcel"
  )

adjacent_year_geometry <- sf::st_read(
  "../output/residential_unresolved_adjacent_year_parcels.gpkg",
  quiet = TRUE
) |>
  dplyr::filter(project_id %in% projects$project_id) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(geom = sf::st_union(geom), .groups = "drop") |>
  dplyr::inner_join(
    projects |>
      dplyr::select(
        project_id,
        project_pin,
        construction_year,
        within_500ft
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(search_geometry_source = "accepted_adjacent_year_parcel")

missing_after_parcels <- projects |>
  dplyr::anti_join(
    dplyr::bind_rows(
      sf::st_drop_geometry(construction_year_geometry),
      sf::st_drop_geometry(adjacent_year_geometry)
    ) |>
      dplyr::distinct(project_id),
    by = "project_id"
  )

buffer_geometry <- sf::st_read(
  "../output/preferred_new_construction_project_centroids.gpkg",
  quiet = TRUE
) |>
  dplyr::inner_join(
    missing_after_parcels |>
      dplyr::select(
        project_id,
        project_pin,
        construction_year,
        within_500ft
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    geom = sf::st_buffer(geom, 150),
    search_geometry_source = "centroid_150ft_candidate_search"
  )

project_geometry <- dplyr::bind_rows(
  construction_year_geometry,
  adjacent_year_geometry,
  buffer_geometry
) |>
  dplyr::select(
    project_id,
    project_pin,
    construction_year,
    within_500ft,
    search_geometry_source
  )

if (nrow(project_geometry) != nrow(projects) ||
    anyDuplicated(project_geometry$project_id)) {
  stop("Multicard successor search geometry is incomplete.", call. = FALSE)
}

current_parcels <- data.table::fread(
  "../input/parcel_universe_2025_city.csv",
  select = c(
    "pin", "pin10", "tax_year", "class",
    "centroid_x_crs_3435", "centroid_y_crs_3435"
  ),
  colClasses = "character"
) |>
  tibble::as_tibble() |>
  dplyr::transmute(
    pin = stringr::str_pad(
      stringr::str_replace_all(pin, "[^0-9]", ""),
      14,
      pad = "0"
    ),
    pin10 = stringr::str_pad(
      stringr::str_replace_all(pin10, "[^0-9]", ""),
      10,
      pad = "0"
    ),
    tax_year = as.integer(tax_year),
    class = stringr::str_squish(class),
    x_3435 = as.numeric(centroid_x_crs_3435),
    y_3435 = as.numeric(centroid_y_crs_3435)
  ) |>
  dplyr::filter(is.finite(x_3435), is.finite(y_3435))

if (anyDuplicated(current_parcels$pin)) {
  stop("Current parcel PINs are not unique.", call. = FALSE)
}

current_parcels_sf <- current_parcels |>
  sf::st_as_sf(coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)

intersections <- sf::st_intersects(current_parcels_sf, project_geometry)
linked_rows <- which(lengths(intersections) > 0)

links <- tibble::tibble(
  parcel_row = rep(linked_rows, lengths(intersections[linked_rows])),
  project_row = unlist(intersections[linked_rows], use.names = FALSE)
) |>
  dplyr::transmute(
    project_id = project_geometry$project_id[project_row],
    project_pin = project_geometry$project_pin[project_row],
    construction_year = project_geometry$construction_year[project_row],
    within_500ft = project_geometry$within_500ft[project_row],
    search_geometry_source =
      project_geometry$search_geometry_source[project_row],
    current_pin = current_parcels$pin[parcel_row],
    current_pin10 = current_parcels$pin10[parcel_row],
    current_tax_year = current_parcels$tax_year[parcel_row],
    current_class = current_parcels$class[parcel_row],
    current_x_3435 = current_parcels$x_3435[parcel_row],
    current_y_3435 = current_parcels$y_3435[parcel_row]
  ) |>
  dplyr::distinct()

addresses <- data.table::fread(
  "../input/parcel_addresses_2025_chicago.csv",
  select = c("pin", "pin10", "prop_address_full"),
  colClasses = "character"
) |>
  tibble::as_tibble() |>
  dplyr::transmute(
    current_pin = stringr::str_pad(
      stringr::str_replace_all(pin, "[^0-9]", ""),
      14,
      pad = "0"
    ),
    current_pin10 = stringr::str_pad(
      stringr::str_replace_all(pin10, "[^0-9]", ""),
      10,
      pad = "0"
    ),
    current_address = stringr::str_squish(prop_address_full)
  ) |>
  dplyr::filter(current_address != "") |>
  dplyr::group_by(current_pin, current_pin10) |>
  dplyr::summarise(
    current_address = paste(
      sort(unique(current_address)),
      collapse = " / "
    ),
    .groups = "drop"
  )

current_residential <- data.table::fread(
  "../input/residential_improvement_characteristics_full.csv",
  select = c(
    "pin", "year", "card", "class", "char_yrblt",
    "char_bldg_sf", "char_land_sf", "char_apts"
  ),
  colClasses = "character"
) |>
  tibble::as_tibble() |>
  dplyr::transmute(
    current_pin = stringr::str_pad(
      stringr::str_replace_all(pin, "[^0-9]", ""),
      14,
      pad = "0"
    ),
    year = as.integer(year),
    card = as.integer(card),
    assessor_class = stringr::str_squish(class),
    year_built = as.integer(char_yrblt),
    building_sqft = as.numeric(char_bldg_sf),
    land_sqft = as.numeric(char_land_sf),
    apartment_count = parse_apartments(char_apts),
    dwelling_units = dplyr::case_when(
      stringr::str_detect(assessor_class, "^21[12]$") &
        apartment_count > 0 ~ apartment_count,
      stringr::str_detect(assessor_class, "^2") ~ 1,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::filter(year <= 2025) |>
  dplyr::group_by(current_pin) |>
  dplyr::filter(year == max(year, na.rm = TRUE)) |>
  dplyr::summarise(
    latest_assessor_year = max(year),
    latest_assessor_cards = dplyr::n_distinct(card),
    latest_assessor_classes = paste(
      sort(unique(stats::na.omit(assessor_class))),
      collapse = "/"
    ),
    latest_assessor_year_built = paste(
      sort(unique(stats::na.omit(year_built))),
      collapse = "/"
    ),
    latest_assessor_units = sum(dwelling_units, na.rm = TRUE),
    latest_assessor_building_sqft = sum(building_sqft, na.rm = TRUE),
    latest_assessor_land_sqft = suppressWarnings(
      max(land_sqft, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    latest_assessor_land_sqft = dplyr::if_else(
      is.infinite(latest_assessor_land_sqft),
      NA_real_,
      latest_assessor_land_sqft
    )
  )

final_components <- readr::read_csv(
  "../output/preferred_new_construction_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(component_project_id = project_id, component_pin) |>
  dplyr::distinct() |>
  dplyr::group_by(component_pin) |>
  dplyr::summarise(
    component_project_ids = paste(
      sort(unique(component_project_id)),
      collapse = "/"
    ),
    .groups = "drop"
  )

links <- links |>
  dplyr::left_join(
    addresses,
    by = c("current_pin", "current_pin10"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    current_residential,
    by = "current_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    final_components,
    by = c("current_pin" = "component_pin"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    represented_by_other_final_project =
      !is.na(component_project_ids) &
        !stringr::str_detect(
          component_project_ids,
          stringr::fixed(project_id)
        ),
    same_original_pin = current_pin == project_pin
  ) |>
  dplyr::arrange(project_id, current_pin, component_project_ids)

project_summary <- links |>
  dplyr::group_by(
    project_id,
    project_pin,
    construction_year,
    within_500ft,
    search_geometry_source
  ) |>
  dplyr::summarise(
    current_parcels = dplyr::n_distinct(current_pin),
    current_noncondo_parcels =
      dplyr::n_distinct(current_pin[current_class != "299"]),
    current_condo_pins =
      dplyr::n_distinct(current_pin[current_class == "299"]),
    distinct_current_addresses =
      dplyr::n_distinct(current_address[!is.na(current_address)]),
    current_addresses = paste(
      sort(unique(stats::na.omit(current_address))),
      collapse = " | "
    ),
    current_classes = paste(
      sort(unique(stats::na.omit(current_class))),
      collapse = "/"
    ),
    current_noncondo_assessor_units = sum(
      latest_assessor_units[current_class != "299"],
      na.rm = TRUE
    ),
    current_noncondo_assessor_building_sqft = sum(
      latest_assessor_building_sqft[current_class != "299"],
      na.rm = TRUE
    ),
    current_parcels_represented_elsewhere = dplyr::n_distinct(
      current_pin[represented_by_other_final_project %in% TRUE]
    ),
    other_final_projects = paste(
      sort(unique(stats::na.omit(
        component_project_ids[
          represented_by_other_final_project %in% TRUE
        ]
      ))),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(project_id)

summary <- tibble::tibble(
  metric = c(
    "multicard_projects",
    "projects_with_current_successor_parcels",
    "current_parcel_links",
    "projects_with_current_condo_pins",
    "projects_with_current_parcels_represented_elsewhere",
    "projects_using_centroid_candidate_search"
  ),
  value = c(
    nrow(projects),
    dplyr::n_distinct(links$project_id),
    dplyr::n_distinct(links[c("project_id", "current_pin")]),
    sum(project_summary$current_condo_pins > 0),
    sum(project_summary$current_parcels_represented_elsewhere > 0),
    sum(
      project_geometry$search_geometry_source ==
        "centroid_150ft_candidate_search"
    )
  )
)

readr::write_csv(
  links,
  "../output/multicard_current_successor_links.csv"
)
readr::write_csv(
  project_summary,
  "../output/multicard_current_successor_project_summary.csv"
)
readr::write_csv(
  summary,
  "../output/multicard_current_successor_evidence_summary.csv"
)
