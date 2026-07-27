# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/reviewed_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_year = readr::col_double(),
    review_address = readr::col_character(),
    x_3435 = readr::col_double(),
    y_3435 = readr::col_double(),
    possible_contradictory_snapshot = readr::col_logical(),
    audit_decision = readr::col_character(),
    .default = readr::col_skip()
  )
  ) |>
  dplyr::filter(
    audit_decision == "retain_assessor_only_pending_review"
  ) |>
  dplyr::mutate(
    project_street_number = readr::parse_number(review_address),
    address_remainder = stringr::str_remove(
      stringr::str_to_upper(review_address),
      "^\\s*[0-9]+\\s+"
    ),
    project_street_direction = stringr::str_extract(
      address_remainder,
      "^(N|S|E|W)\\b"
    ),
    project_street_name = stringr::str_remove(
      address_remainder,
      "^(N|S|E|W)\\s+"
    ),
    project_street_name = stringr::str_extract(
      project_street_name,
      "^.*?\\b(ST|AVE|BLVD|RD|PL|DR|CT|PKWY)\\b"
    ),
    project_street_name = stringr::str_remove_all(
      project_street_name,
      "[^A-Z0-9]"
    ),
    street_key = paste(project_street_direction, project_street_name)
  )

permits <- DBI::dbConnect(
  RSQLite::SQLite(),
  "../input/building_permits_clean.gpkg"
)
new_building_permits <- DBI::dbGetQuery(
  permits,
  paste(
    "SELECT id, permit, application_start_date, issue_date,",
    "street_number, street_direction, street_name,",
    "xcoordinate, ycoordinate, work_description",
    "FROM building_permits_clean",
    "WHERE permit_type = 'PERMIT - NEW CONSTRUCTION'",
    "AND permit_issued = 1"
  )
)
DBI::dbDisconnect(permits)

new_building_permits <- new_building_permits |>
  dplyr::mutate(
    street_number = as.numeric(street_number),
    xcoordinate = as.numeric(xcoordinate),
    ycoordinate = as.numeric(ycoordinate),
    street_direction = stringr::str_to_upper(street_direction),
    street_name_key = stringr::str_remove_all(
      stringr::str_to_upper(street_name),
      "[^A-Z0-9]"
    ),
    street_key = paste(street_direction, street_name_key),
    application_year = as.integer(substr(application_start_date, 1, 4)),
    issue_year = as.integer(substr(issue_date, 1, 4)),
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

permit_rows_by_street <- split(
  seq_len(nrow(new_building_permits)),
  new_building_permits$street_key
)
candidate_rows <- vector("list", nrow(projects))
for (i in seq_len(nrow(projects))) {
  permit_rows <- permit_rows_by_street[[projects$street_key[i]]]
  if (is.null(permit_rows)) {
    next
  }
  candidate_rows[[i]] <- dplyr::bind_cols(
    projects[rep(i, length(permit_rows)), ] |>
      dplyr::select(-street_key),
    new_building_permits[permit_rows, ] |>
      dplyr::select(-street_key)
  )
}

candidates <- dplyr::bind_rows(candidate_rows) |>
  dplyr::mutate(
    street_number_difference = abs(
      project_street_number - street_number
    ),
    spatial_distance_ft = sqrt(
      (x_3435 - xcoordinate)^2 +
        (y_3435 - ycoordinate)^2
    ),
    application_lead_years = construction_year - application_year
  ) |>
  dplyr::filter(
    street_number_difference <= 25,
    spatial_distance_ft <= 250,
    dplyr::between(application_lead_years, -1, 6)
  ) |>
  dplyr::arrange(
    project_id,
    street_number_difference,
    abs(application_lead_years),
    permit
  ) |>
  dplyr::select(
    project_id,
    construction_year,
    review_address,
    permit_id = id,
    permit_number = permit,
    permit_address_number = street_number,
    project_street_number,
    street_number_difference,
    spatial_distance_ft,
    application_start_date,
    issue_date,
    application_year,
    issue_year,
    application_lead_years,
    permit_description = work_description
  )

if (anyDuplicated(candidates[c("project_id", "permit_id")])) {
  stop("Street-range permit candidates are duplicated.")
}

readr::write_csv(
  candidates,
  "../output/street_range_permit_candidates.csv",
  na = ""
)
