# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/project_verification_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character(),
    construction_year = readr::col_double(),
    x_3435 = readr::col_double(),
    y_3435 = readr::col_double(),
    current_multifamily = readr::col_logical()
  )
) |>
  dplyr::mutate(
    unresolved_project =
      verification_status == "unresolved_after_official_snapshots"
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
    "id, permit, application_start_date, issue_date,",
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
    application_year = as.integer(substr(application_start_date, 1, 4)),
    issue_year = as.integer(substr(issue_date, 1, 4)),
    permit_address = stringr::str_squish(paste(
      street_number,
      street_direction,
      street_name
    )),
    description_upper = stringr::str_squish(
      stringr::str_to_upper(dplyr::coalesce(work_description, ""))
    ),
    ineligible_scope = stringr::str_detect(
      description_upper,
      paste0(
        "\\b(ADDITION|ADDITIONS)\\b.*\\bEXISTING\\b|",
        "\\bEXISTING\\b.*\\b(ADDITION|ADDITIONS)\\b|",
        "\\bNEW (GARAGE|PORCH|DECK|FENCE|CANOPY|VESTIBULE|",
        "STAIR|STAIRS)\\b|",
        "\\b(DECONVERSION|CONVERSION|ALTERATION|ALTERATIONS|",
        "MODIFICATION|MODIFICATIONS|REMODEL|REMODELING|",
        "RENOVATION|RENOVATIONS|REHAB|BUILDOUT|BUILD-OUT)\\b",
        ".*\\bEXISTING\\b|",
        "\\bEXISTING\\b.*\\b(DECONVERSION|CONVERSION|",
        "ALTERATION|ALTERATIONS|MODIFICATION|MODIFICATIONS|",
        "REMODEL|REMODELING|RENOVATION|RENOVATIONS|REHAB|",
        "BUILDOUT|BUILD-OUT)\\b"
      )
    )
  ) |>
  dplyr::filter(!ineligible_scope)

normalize_address <- function(address) {
  address |>
    stringr::str_to_upper() |>
    stringr::str_remove("\\bCHICAGO\\b.*$") |>
    stringr::str_remove("\\b(APT|UNIT|SUITE)\\b.*$") |>
    stringr::str_remove_all("[^A-Z0-9]")
}

project_addresses <- projects |>
  dplyr::select(project_id, review_address) |>
  tidyr::separate_longer_delim(review_address, delim = " / ") |>
  dplyr::mutate(address_key = normalize_address(review_address)) |>
  dplyr::filter(address_key != "") |>
  dplyr::distinct(project_id, address_key)

permit_addresses <- permits |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    permit_row = dplyr::row_number(),
    address_key = normalize_address(permit_address)
  ) |>
  dplyr::filter(address_key != "")

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

candidates <- dplyr::full_join(
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
        review_address,
        current_multifamily,
        unresolved_project
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
        application_start_date,
        issue_date,
        application_year,
        issue_year,
        permit_address,
        permit_description = work_description
      ),
    by = "permit_row",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    address_match = dplyr::coalesce(address_match, FALSE),
    spatial_match = dplyr::coalesce(spatial_match, FALSE),
    application_lead_years = construction_year - application_year,
    issue_lead_years = construction_year - issue_year
  ) |>
  dplyr::filter(
    is.finite(application_lead_years),
    dplyr::between(application_lead_years, -1, 6)
  ) |>
  dplyr::arrange(
    project_id,
    dplyr::desc(address_match),
    abs(application_lead_years),
    permit_number
  )

permit_project_counts <- candidates |>
  dplyr::distinct(permit_id, project_id) |>
  dplyr::count(permit_id, name = "candidate_project_count")

candidates <- candidates |>
  dplyr::left_join(
    permit_project_counts,
    by = "permit_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    unique_project_match = candidate_project_count == 1L,
    extended_permit_support =
      unique_project_match &
      (address_match | spatial_match) &
      dplyr::between(application_lead_years, -1, 5)
  ) |>
  dplyr::filter(unresolved_project)

if (anyDuplicated(candidates[c("project_id", "permit_id")])) {
  stop("Extended permit candidates are duplicated.")
}

summary <- candidates |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    extended_permit_support = any(extended_permit_support),
    extended_permit_count = sum(extended_permit_support),
    extended_permit_numbers = paste(
      sort(unique(permit_number[extended_permit_support])),
      collapse = "/"
    ),
    minimum_application_lead_years = suppressWarnings(min(
      application_lead_years[extended_permit_support],
      na.rm = TRUE
    )),
    maximum_application_lead_years = suppressWarnings(max(
      application_lead_years[extended_permit_support],
      na.rm = TRUE
    )),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    minimum_application_lead_years = dplyr::if_else(
      is.infinite(minimum_application_lead_years),
      NA_real_,
      minimum_application_lead_years
    ),
    maximum_application_lead_years = dplyr::if_else(
      is.infinite(maximum_application_lead_years),
      NA_real_,
      maximum_application_lead_years
    )
  )

readr::write_csv(
  candidates,
  "../output/extended_permit_candidates.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/extended_permit_candidate_summary.csv",
  na = ""
)
