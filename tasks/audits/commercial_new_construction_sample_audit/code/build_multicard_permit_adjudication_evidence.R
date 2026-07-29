# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

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

ravenswood_projects <- readr::read_csv(
  "../output/multicard_episode_component_nodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(component_id == "episode_103") |>
  dplyr::select(project_id)

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
  ravenswood_projects |>
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
  dplyr::distinct(project_id, .keep_all = TRUE) |>
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
  "../input/building_permits_clean.gpkg",
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
          "RESIDENTIAL\\s+UNITS?|UNITS?|TOWNHOMES?|TOWNHOUSES?)\\b"
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

project_summary <- review_projects |>
  dplyr::left_join(
    links |>
      dplyr::group_by(project_id) |>
      dplyr::summarise(
        nearby_new_construction_permits = dplyr::n_distinct(permit_id),
        permits_on_project_polygon = dplyr::n_distinct(
          permit_id[intersects_project_polygon]
        ),
        permits_at_exact_address = dplyr::n_distinct(
          permit_id[exact_address_match]
        ),
        permit_ids = paste(
          sort(unique(permit_id)),
          collapse = "/"
        ),
        permit_addresses = paste(
          sort(unique(permit_address)),
          collapse = " | "
        ),
        permit_unit_mentions = paste(
          sort(unique(
            permit_unit_mentions[permit_unit_mentions != ""]
          )),
          collapse = " | "
        ),
        polygon_permit_ids = paste(
          sort(unique(permit_id[intersects_project_polygon])),
          collapse = "/"
        ),
        polygon_permit_addresses = paste(
          sort(unique(permit_address[intersects_project_polygon])),
          collapse = " | "
        ),
        polygon_permit_unit_mentions = paste(
          sort(unique(
            permit_unit_mentions[
              intersects_project_polygon &
                permit_unit_mentions != ""
            ]
          )),
          collapse = " | "
        ),
        exact_address_permit_ids = paste(
          sort(unique(permit_id[exact_address_match])),
          collapse = "/"
        ),
        exact_address_permit_years = paste(
          sort(unique(permit_year[exact_address_match])),
          collapse = "/"
        ),
        exact_address_permit_unit_mentions = paste(
          sort(unique(
            permit_unit_mentions[
              exact_address_match &
                permit_unit_mentions != ""
            ]
          )),
          collapse = " | "
        ),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    nearby_new_construction_permits =
      dplyr::coalesce(nearby_new_construction_permits, 0L),
    permits_on_project_polygon =
      dplyr::coalesce(permits_on_project_polygon, 0L),
    permits_at_exact_address =
      dplyr::coalesce(permits_at_exact_address, 0L)
  ) |>
  dplyr::arrange(
    dplyr::desc(within_500ft),
    project_id
  )

summary <- tibble::tibble(
  metric = c(
    "review_projects",
    "projects_with_nearby_new_construction_permits",
    "projects_with_permits_on_project_polygon",
    "project_permit_links"
  ),
  value = c(
    nrow(review_projects),
    sum(project_summary$nearby_new_construction_permits > 0),
    sum(project_summary$permits_on_project_polygon > 0),
    nrow(links)
  )
)

readr::write_csv(
  links,
  "../output/multicard_permit_adjudication_links.csv"
)
readr::write_csv(
  project_summary,
  "../output/multicard_permit_adjudication_projects.csv"
)
readr::write_csv(
  summary,
  "../output/multicard_permit_adjudication_summary.csv"
)
