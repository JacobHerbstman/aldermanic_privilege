# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/project_verification_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_year = readr::col_double(),
    x_3435 = readr::col_double(),
    y_3435 = readr::col_double(),
    verification_status = readr::col_character(),
    .default = readr::col_character()
  )
)

project_sites <- sf::st_as_sf(
  projects,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)

permits <- sf::st_read(
  "../input/building_permits_clean.gpkg",
  query = paste(
    "SELECT",
    "id, permit, permit_type, permit_status,",
    "application_start_date, issue_date,",
    "street_number, street_direction, street_name,",
    "work_description, geom",
    "FROM building_permits_clean",
    "WHERE application_start_date >= '2002-01-01'",
    "AND application_start_date < '2024-01-01'",
    "AND permit_type IN (",
    "'PERMIT - NEW CONSTRUCTION',",
    "'PERMIT - WRECKING/DEMOLITION',",
    "'PERMIT - REINSTATE REVOKED PMT',",
    "'PERMIT - RENOVATION/ALTERATION'",
    ")"
  ),
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::mutate(
    permit_address = stringr::str_squish(paste(
      street_number,
      street_direction,
      street_name
    ))
  )

normalize_address <- function(address) {
  address |>
    stringr::str_to_upper() |>
    stringr::str_remove("\\bCHICAGO\\b.*$") |>
    stringr::str_remove("\\b(APT|UNIT|SUITE)\\b.*$") |>
    stringr::str_replace_all(
      "\\b(AVENUE|AVE)\\b",
      "AVE"
    ) |>
    stringr::str_replace_all(
      "\\b(STREET|ST)\\b",
      "ST"
    ) |>
    stringr::str_replace_all(
      "\\b(PLACE|PL)\\b",
      "PL"
    ) |>
    stringr::str_remove_all("[^A-Z0-9]")
}

project_addresses <- projects |>
  dplyr::select(
    project_id,
    source_addresses,
    addresses,
    current_property_addresses,
    review_address,
    selected_historical_address,
    current_pin_address
  ) |>
  tidyr::pivot_longer(
    -project_id,
    values_to = "project_address"
  ) |>
  dplyr::select(-name) |>
  tidyr::separate_longer_delim(project_address, delim = " / ") |>
  dplyr::mutate(address_key = normalize_address(project_address)) |>
  dplyr::filter(!is.na(address_key), address_key != "") |>
  dplyr::distinct(project_id, address_key)

permit_addresses <- permits |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    permit_row = dplyr::row_number(),
    address_key = normalize_address(permit_address)
  ) |>
  dplyr::filter(!is.na(address_key), address_key != "")

permit_rows_by_address <- split(
  permit_addresses$permit_row,
  permit_addresses$address_key
)
address_candidate_rows <- vector("list", nrow(project_addresses))
for (i in seq_len(nrow(project_addresses))) {
  permit_rows <- permit_rows_by_address[[
    project_addresses$address_key[i]
  ]]
  if (is.null(permit_rows)) {
    next
  }
  address_candidate_rows[[i]] <- tibble::tibble(
    project_id = project_addresses$project_id[i],
    permit_row = permit_rows,
    address_match = TRUE
  )
}
address_candidates <- dplyr::bind_rows(address_candidate_rows) |>
  dplyr::distinct(project_id, permit_row, .keep_all = TRUE)

spatial_indices <- sf::st_is_within_distance(
  project_sites,
  permits,
  dist = 25
)
spatial_candidates <- tibble::tibble(
  project_id = rep(projects$project_id, lengths(spatial_indices)),
  permit_row = unlist(spatial_indices)
) |>
  dplyr::mutate(spatial_match = TRUE)

permit_history <- dplyr::full_join(
  address_candidates,
  spatial_candidates,
  by = c("project_id", "permit_row"),
  relationship = "one-to-one"
) |>
  dplyr::left_join(
    projects |>
      dplyr::select(
        project_id,
        construction_year,
        verification_status
      ),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    permits |>
      sf::st_drop_geometry() |>
      dplyr::mutate(permit_row = dplyr::row_number()) |>
      dplyr::select(
        permit_row,
        permit_id = id,
        permit_number = permit,
        permit_type,
        permit_status,
        application_start_date,
        issue_date,
        permit_address,
        permit_description = work_description
      ),
    by = "permit_row",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    address_match = dplyr::coalesce(address_match, FALSE),
    spatial_match = dplyr::coalesce(spatial_match, FALSE),
    application_year = as.integer(substr(application_start_date, 1, 4)),
    application_year_gap = application_year - construction_year
  )

permit_project_counts <- permit_history |>
  dplyr::distinct(permit_id, project_id) |>
  dplyr::count(permit_id, name = "candidate_project_count")

permit_history <- permit_history |>
  dplyr::left_join(
    permit_project_counts,
    by = "permit_id",
    relationship = "many-to-one"
  ) |>
  dplyr::filter(
    verification_status == "unresolved_after_official_snapshots",
    dplyr::between(application_year_gap, -8L, 3L)
  ) |>
  dplyr::arrange(
    project_id,
    application_start_date,
    permit_number
  )

if (anyDuplicated(permit_history[c("project_id", "permit_id")])) {
  stop("Project permit-history rows are duplicated.")
}

readr::write_csv(
  permit_history,
  "../output/project_permit_history.csv",
  na = ""
)
