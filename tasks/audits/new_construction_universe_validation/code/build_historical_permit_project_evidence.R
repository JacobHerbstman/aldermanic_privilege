# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

ledger <- readr::read_csv(
  "../input/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE
)

component_pins <- ledger |>
  dplyr::select(project_id, source_family, construction_year, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::transmute(
    project_id,
    source_family,
    construction_year,
    component_pin = component_pins,
    pin10 = stringr::str_sub(component_pins, 1, 10)
  ) |>
  dplyr::distinct()

project_pin10 <- component_pins |>
  dplyr::select(project_id, source_family, construction_year, pin10) |>
  dplyr::distinct()

if (
  anyDuplicated(component_pins[c("project_id", "component_pin")]) ||
    any(nchar(component_pins$pin10) != 10) ||
    anyDuplicated(project_pin10$pin10)
) {
  stop("The component-PIN crosswalk failed validation.", call. = FALSE)
}

connection <- DBI::dbConnect(
  RSQLite::SQLite(),
  "../input/building_permits_clean.gpkg",
  flags = RSQLite::SQLITE_RO
)
on.exit(DBI::dbDisconnect(connection), add = TRUE)

DBI::dbWriteTable(
  connection,
  "desired_pin10",
  project_pin10 |>
    dplyr::distinct(pin10),
  temporary = TRUE,
  overwrite = TRUE
)

permits <- DBI::dbGetQuery(
  connection,
  paste(
    "SELECT",
    "p.pin AS pin10,",
    "p.id AS permit_id,",
    "p.permit AS permit_number,",
    "p.permit_status,",
    "p.permit_type,",
    "p.review_type,",
    "p.application_start_date,",
    "p.issue_date,",
    "p.street_number,",
    "p.street_direction,",
    "p.street_name,",
    "p.work_description,",
    "p.minor_permit",
    "FROM building_permits_clean p",
    "INNER JOIN desired_pin10 d ON p.pin = d.pin10",
    "WHERE p.application_start_date >= '2002-01-01'",
    "AND p.application_start_date < '2024-01-01'"
  )
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    application_start_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    application_year = lubridate::year(application_start_date),
    issue_year = lubridate::year(issue_date),
    work_description = stringr::str_squish(
      stringr::str_to_upper(dplyr::coalesce(work_description, ""))
    ),
    permit_address = stringr::str_squish(
      paste(
        format(street_number, trim = TRUE, scientific = FALSE),
        street_direction,
        street_name
      )
    ),
    explicit_new_building_raw = stringr::str_detect(
      work_description,
      paste0(
        "\\bNEW CONSTRUCTION\\b|",
        "\\bCONSTRUCTION OF (?:A |AN )?NEW\\b|",
        "\\bCONSTRUCT(?:ION)? (?:A |AN )?NEW\\b|",
        "\\bERECT (?:A |AN )?NEW\\b|",
        "\\bERECT (?:A |AN )?[0-9]+[ -]?STORY\\b|",
        "\\bERECT .*\\b(BUILDING|RESIDENCE|HOUSE|DWELLING)\\b|",
        "\\bFULL BUILDING PERMIT\\b"
      )
    ),
    addition_or_accessory_scope = stringr::str_detect(
      work_description,
      paste0(
        "\\b(ADDITION|ADDITIONS)\\b.*\\bEXISTING\\b|",
        "\\bEXISTING\\b.*\\b(ADDITION|ADDITIONS)\\b|",
        "\\bNEW (GARAGE|PORCH|DECK|FENCE|CANOPY|VESTIBULE|STAIR|STAIRS)\\b|",
        "\\b(GARAGE|PORCH|DECK|FENCE|CANOPY|VESTIBULE|STAIR|STAIRS)\\b",
        ".*\\bEXISTING\\b"
      )
    ),
    explicit_new_building =
      explicit_new_building_raw & !addition_or_accessory_scope,
    explicit_existing_building = stringr::str_detect(
      work_description,
      paste0(
        "\\bEXISTING\\b.*\\b(BUILDING|STRUCTURE|RESIDENCE|HOUSE)\\b|",
        "\\b(BUILDING|STRUCTURE|RESIDENCE|HOUSE)\\b.*\\bEXISTING\\b|",
        "\\bDECONVERSION OF EXISTING\\b|",
        "\\bCONVERSION OF EXISTING\\b"
      )
    ),
    substantive_existing_work = stringr::str_detect(
      work_description,
      paste0(
        "\\b(DECONVERSION|CONVERSION|ALTERATION|ALTERATIONS|REMODEL|",
        "REMODELING|RENOVATION|RENOVATIONS|REHAB|ADDITION|ADDITIONS)\\b"
      )
    ),
    non_new_construction_existing_work =
      permit_type != "PERMIT - NEW CONSTRUCTION" &
      substantive_existing_work,
    existing_building_work =
      (explicit_existing_building | non_new_construction_existing_work) &
      substantive_existing_work &
      !explicit_new_building,
    broad_existing_work =
      (
        stringr::str_detect(work_description, "\\bEXISTING\\b") |
          non_new_construction_existing_work
      ) &
      substantive_existing_work &
      !explicit_new_building,
    mixed_new_addition_scope =
      broad_existing_work &
      stringr::str_detect(
        work_description,
        paste0(
          "\\bNEW\\b.*\\b(BUILDING|BLDG|STORY|STORIES)\\b",
          ".*\\b(ADDITION|ADDITIONS)\\b|",
          "\\bNEW\\b.*\\b(ADDITION|ADDITIONS)\\b",
          ".*\\b(BUILDING|BLDG|STORY|STORIES)\\b"
        )
      ),
    valid_permit_status = permit_status %in% c(
      "COMPLETE",
      "ACTIVE",
      "PHASED PERMITTING"
    ),
    valid_new_construction_permit =
      permit_type == "PERMIT - NEW CONSTRUCTION" &
      valid_permit_status &
      !broad_existing_work &
      !addition_or_accessory_scope,
    issued_new_building_permit =
      !is.na(issue_date) &
      permit_type == "PERMIT - NEW CONSTRUCTION" &
      !broad_existing_work &
      !addition_or_accessory_scope,
    permit_number = as.character(permit_number)
  ) |>
  dplyr::inner_join(
    project_pin10,
    by = "pin10",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    application_year_gap = application_year - construction_year,
    in_construction_window = dplyr::between(
      application_year_gap,
      -5L,
      1L
    ),
    before_or_during_reported_construction =
      application_year_gap <= 0L &
      application_year_gap >= -5L,
    positive_new_building_evidence =
      in_construction_window &
      valid_permit_status &
      (explicit_new_building | valid_new_construction_permit),
    issued_new_building_evidence =
      in_construction_window &
      issued_new_building_permit,
    negative_existing_building_evidence =
      before_or_during_reported_construction &
      existing_building_work,
    broad_negative_existing_work =
      before_or_during_reported_construction &
      broad_existing_work,
    pure_negative_existing_work =
      broad_negative_existing_work &
      !mixed_new_addition_scope,
    mixed_negative_existing_work =
      broad_negative_existing_work &
      mixed_new_addition_scope,
    post_construction_existing_work =
      application_year_gap == 1L &
      existing_building_work
  )

collapse_values <- function(x) {
  values <- sort(unique(x[!is.na(x) & x != ""]))
  if (length(values) == 0) NA_character_ else paste(values, collapse = " | ")
}

minimum_value <- function(x) {
  values <- x[is.finite(x)]
  if (length(values) == 0) NA_real_ else min(values)
}

project_evidence <- permits |>
  dplyr::filter(in_construction_window) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    exact_pin_permit_records = dplyr::n_distinct(permit_id),
    exact_pin_permit_numbers = collapse_values(permit_number),
    exact_pin_permit_addresses = collapse_values(permit_address),
    exact_pin_positive_new_building =
      any(positive_new_building_evidence),
    exact_pin_issued_new_building =
      any(issued_new_building_evidence),
    exact_pin_negative_existing_building =
      any(negative_existing_building_evidence),
    exact_pin_broad_negative_existing_work =
      any(broad_negative_existing_work),
    exact_pin_pure_negative_existing_work =
      any(pure_negative_existing_work),
    exact_pin_mixed_negative_existing_work =
      any(mixed_negative_existing_work),
    exact_pin_post_construction_existing_work =
      any(post_construction_existing_work),
    exact_pin_positive_permit_numbers = collapse_values(
      permit_number[positive_new_building_evidence]
    ),
    exact_pin_negative_permit_numbers = collapse_values(
      permit_number[negative_existing_building_evidence]
    ),
    exact_pin_positive_descriptions = collapse_values(
      work_description[positive_new_building_evidence]
    ),
    exact_pin_issued_new_building_permit_numbers = collapse_values(
      permit_number[issued_new_building_evidence]
    ),
    exact_pin_issued_new_building_descriptions = collapse_values(
      work_description[issued_new_building_evidence]
    ),
    exact_pin_issued_new_building_issue_year_min = minimum_value(
      issue_year[issued_new_building_evidence]
    ),
    exact_pin_issued_new_building_issue_year_max = -minimum_value(
      -issue_year[issued_new_building_evidence]
    ),
    exact_pin_negative_descriptions = collapse_values(
      work_description[negative_existing_building_evidence]
    ),
    exact_pin_broad_negative_permit_numbers = collapse_values(
      permit_number[broad_negative_existing_work]
    ),
    exact_pin_broad_negative_descriptions = collapse_values(
      work_description[broad_negative_existing_work]
    ),
    exact_pin_pure_negative_application_year_max = -minimum_value(
      -application_year[pure_negative_existing_work]
    ),
    exact_pin_mixed_negative_application_year_max = -minimum_value(
      -application_year[mixed_negative_existing_work]
    ),
    exact_pin_application_year_min = min(application_year),
    exact_pin_application_year_max = max(application_year),
    .groups = "drop"
  )

readr::write_csv(
  project_evidence,
  "../output/historical_permit_project_evidence.csv",
  na = ""
)
