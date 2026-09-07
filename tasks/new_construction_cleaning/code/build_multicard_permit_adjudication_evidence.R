# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/permit_unit_patterns.R")

review_projects <- readr::read_csv(
  "../output/multicard_final_review_bundle.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id,
    construction_year,
    within_500ft,
    review_address,
    target_cards,
    summed_card_units,
    summed_card_building_sqft
  )

episode_nodes <- readr::read_csv(
  "../output/multicard_episode_component_nodes.csv",
  show_col_types = FALSE
)
reviewed_components <- episode_nodes |>
  dplyr::filter(project_id %in% review_projects$project_id) |>
  dplyr::distinct(component_id)
episode_projects <- episode_nodes |>
  dplyr::semi_join(reviewed_components, by = "component_id") |>
  dplyr::select(project_id) |>
  dplyr::anti_join(review_projects, by = "project_id")
if (anyDuplicated(episode_nodes$project_id)) {
  stop("Each project must belong to one duplicate episode.")
}

ledger <- readr::read_csv(
  "../output/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id,
    construction_year,
    source_addresses,
    dwelling_units,
    building_sqft,
    x_3435,
    y_3435
  )

review_projects <- dplyr::bind_rows(
  review_projects,
  episode_projects |>
    dplyr::inner_join(
      ledger,
      by = "project_id",
      relationship = "one-to-one"
    ) |>
    dplyr::transmute(
      project_id,
      construction_year,
      within_500ft = NA,
      review_address = source_addresses,
      target_cards = NA,
      summed_card_units = dwelling_units,
      summed_card_building_sqft = building_sqft
    )
) |>
  dplyr::mutate(
    address_parts = stringr::str_match(
      stringr::str_to_upper(review_address),
      paste0(
        "^([0-9]+)\\s+([NSEW])\\s+",
        "(.+?\\s(?:ST|AVE|BLVD|RD|PL|CT|DR|PKWY|TER|LN))\\b"
      )
    ),
    project_street_number = address_parts[, 2],
    project_street_direction = address_parts[, 3],
    project_street_name = address_parts[, 4]
  ) |>
  dplyr::select(-address_parts)

project_polygons <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  dplyr::filter(project_id %in% review_projects$project_id) |>
  dplyr::select(project_id)

missing_geometry <- review_projects |>
  dplyr::anti_join(
    sf::st_drop_geometry(project_polygons),
    by = "project_id"
  ) |>
  dplyr::inner_join(
    ledger |>
      dplyr::select(project_id, x_3435, y_3435),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = FALSE
  )
sf::st_geometry(missing_geometry) <- sf::st_buffer(
  sf::st_geometry(missing_geometry),
  25
)
names(missing_geometry)[
  names(missing_geometry) == attr(missing_geometry, "sf_column")
] <- "geom"
sf::st_geometry(missing_geometry) <- "geom"
missing_geometry <- missing_geometry |>
  dplyr::select(project_id)

project_polygons <- dplyr::bind_rows(
  project_polygons,
  missing_geometry
) |>
  dplyr::inner_join(
    review_projects,
    by = "project_id",
    relationship = "one-to-one"
  )

if (
  nrow(project_polygons) != nrow(review_projects) ||
    anyDuplicated(project_polygons$project_id)
) {
  stop("Multicard permit-review geometry is incomplete.", call. = FALSE)
}

permits <- sf::st_read(
  "../output/building_permits_for_verification.gpkg",
  quiet = TRUE
) |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_upper(permit_type),
      "NEW CONSTRUCTION"
    ),
    !sf::st_is_empty(geom),
    !is.na(application_start_date) | !is.na(issue_date)
  ) |>
  dplyr::mutate(
    permit_year = dplyr::coalesce(
      lubridate::year(issue_date),
      lubridate::year(application_start_date)
    )
  ) |>
  dplyr::select(
    permit_id = id,
    pin,
    application_start_date,
    issue_date,
    permit_year,
    street_number,
    street_direction,
    street_name,
    work_description
  )

search_polygons <- project_polygons |>
  dplyr::mutate(search_geom = sf::st_buffer(geom, 250)) |>
  sf::st_set_geometry("search_geom")

intersections <- sf::st_intersects(search_polygons, permits)
project_rows <- which(lengths(intersections) > 0)

links <- tibble::tibble(
  project_row = rep(project_rows, lengths(intersections[project_rows])),
  permit_row = unlist(intersections[project_rows], use.names = FALSE)
) |>
  dplyr::transmute(
    project_id = search_polygons$project_id[project_row],
    permit_id = permits$permit_id[permit_row]
  ) |>
  dplyr::left_join(
    sf::st_drop_geometry(project_polygons),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    sf::st_drop_geometry(permits),
    by = "permit_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    project_polygons |>
      dplyr::select(project_id, project_geom = geom),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    permits |>
      dplyr::select(permit_id, permit_geom = geom),
    by = "permit_id",
    relationship = "many-to-one"
  ) |>
  sf::st_as_sf(sf_column_name = "project_geom", crs = 3435) |>
  dplyr::mutate(
    distance_to_project_ft = as.numeric(
      sf::st_distance(project_geom, permit_geom, by_element = TRUE)
    ),
    intersects_project_polygon = distance_to_project_ft == 0,
    exact_address_match =
      as.character(street_number) == project_street_number &
        stringr::str_to_upper(street_direction) ==
          project_street_direction &
        stringr::str_squish(stringr::str_to_upper(street_name)) ==
          project_street_name,
    permit_year_gap = permit_year - construction_year,
    permit_unit_mentions = vapply(
      stringr::str_extract_all(
        stringr::str_to_upper(work_description),
        paste0(
          "(?:\\b[0-9]{1,3}|\\([0-9]{1,3}\\))\\s*",
          "(?:D\\.?\\s*U\\.?|DWELLING(?:\\s+UNITS?)?|",
          "RESIDENTIAL\\s+UNITS?|UNITS?)\\b",
          "|", attached_house_count_pattern
        )
      ),
      function(x) paste(x, collapse = "/"),
      character(1)
    ),
    permit_unit_mentions = dplyr::if_else(
      permit_unit_mentions == "" &
        stringr::str_detect(
          stringr::str_to_upper(work_description),
          "SINGLE[- ]FAMILY|SINGLE FAMILY"
        ),
      "1 SINGLE-FAMILY",
      permit_unit_mentions
    ),
    permit_address = stringr::str_squish(paste(
      street_number,
      street_direction,
      street_name
    ))
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(-permit_geom) |>
  dplyr::arrange(
    dplyr::desc(within_500ft),
    project_id,
    distance_to_project_ft,
    permit_year,
    permit_id
  )

readr::write_csv(
  links,
  "../output/multicard_permit_adjudication_links.csv"
)
