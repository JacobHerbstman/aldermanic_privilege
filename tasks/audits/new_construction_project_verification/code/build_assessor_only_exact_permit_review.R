# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/reviewed_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    audit_construction_year = readr::col_integer(),
    .default = readr::col_character()
  )
)

if (anyDuplicated(projects$project_id)) {
  stop("Assessor-principal project IDs are not unique.")
}

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
    "AND application_start_date < '2024-01-01'"
  ),
  quiet = TRUE
) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    permit_address = stringr::str_squish(paste(
      street_number,
      street_direction,
      street_name
    ))
  )

normalize_address <- function(address) {
  normalized <- address |>
    stringr::str_to_upper() |>
    stringr::str_remove("\\bCHICAGO\\b.*$") |>
    stringr::str_replace_all("\\b(AVENUE|AVE)\\b", "AVE") |>
    stringr::str_replace_all("\\b(STREET|ST)\\b", "ST") |>
    stringr::str_replace_all("\\b(PLACE|PL)\\b", "PL") |>
    stringr::str_replace_all("\\b(TERRACE|TER)\\b", "TER")

  parsed <- stringr::str_match(
    normalized,
    "^\\s*([0-9]+)\\s+(N|S|E|W)\\s+(.+?)\\s+" |>
      paste0("(AVE|BLVD|CT|DR|LN|PKWY|PL|RD|ST|TER)\\b")
  )

  dplyr::if_else(
    is.na(parsed[, 1]),
    normalized |>
      stringr::str_remove("\\b(APT|UNIT|SUITE)\\b.*$") |>
      stringr::str_remove_all("[^A-Z0-9]"),
    paste0(parsed[, 2], parsed[, 3], parsed[, 4], parsed[, 5])
  )
}

project_addresses <- projects |>
  dplyr::select(
    project_id,
    audit_construction_year,
    review_address,
    selected_historical_address,
    current_pin_address
  ) |>
  tidyr::pivot_longer(
    cols = c(
      review_address,
      selected_historical_address,
      current_pin_address
    ),
    values_to = "project_address"
  ) |>
  dplyr::transmute(
    project_id,
    audit_construction_year,
    address_key = normalize_address(project_address)
  ) |>
  dplyr::filter(!is.na(address_key), address_key != "") |>
  dplyr::distinct()

project_address_sites <- project_addresses |>
  dplyr::group_by(address_key) |>
  dplyr::summarise(
    project_ids = list(sort(unique(project_id))),
    .groups = "drop"
  )

if (anyDuplicated(project_address_sites$address_key)) {
  stop("Normalized project address keys are not unique by site.")
}

permit_history <- permits |>
  dplyr::mutate(
    address_key = normalize_address(permit_address),
    application_year = as.integer(substr(
      application_start_date,
      1,
      4
    )),
    description_upper = stringr::str_to_upper(
      dplyr::coalesce(work_description, "")
    )
  ) |>
  dplyr::inner_join(
    project_address_sites,
    by = "address_key",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest_longer(project_ids, values_to = "project_id") |>
  dplyr::left_join(
    projects |>
      dplyr::select(project_id, audit_construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    application_year_gap =
      application_year - audit_construction_year,
    new_construction_permit =
      permit_type == "PERMIT - NEW CONSTRUCTION",
    accessory_only =
      stringr::str_detect(
        description_upper,
        "(GARAGE|SHED|FENCE|DECK|PORCH)"
      ) &
      !stringr::str_detect(
        description_upper,
        "(RESIDEN|DWELL|HOUSE|TOWNHOME|TOWNHOUSE|BUILDING)"
      ),
    residential_new_construction =
      new_construction_permit & !accessory_only,
    existing_building_language =
      stringr::str_detect(
        description_upper,
        paste0(
          "EXISTING.{0,40}(BUILD|RESIDEN|HOUSE|HOME|STORY)|",
          "(BUILD|RESIDEN|HOUSE|HOME|STORY).{0,40}EXISTING|",
          "REPAIR.{0,100}PORCH"
        )
      ),
    demolition_permit =
      permit_type == "PERMIT - WRECKING/DEMOLITION"
  ) |>
  dplyr::select(
    project_id,
    audit_construction_year,
    permit_id = id,
    permit_number = permit,
    permit_type,
    permit_status,
    application_start_date,
    issue_date,
    application_year,
    application_year_gap,
    permit_address,
    permit_description = work_description,
    residential_new_construction,
    existing_building_language,
    demolition_permit
  ) |>
  dplyr::distinct(project_id, permit_id, .keep_all = TRUE) |>
  dplyr::arrange(
    project_id,
    application_start_date,
    permit_number
  )

if (anyDuplicated(permit_history[c("project_id", "permit_id")])) {
  stop("Exact-address project-permit rows are duplicated.")
}

permit_summary <- permit_history |>
  dplyr::group_by(project_id, audit_construction_year) |>
  dplyr::summarise(
    exact_permits = dplyr::n(),
    residential_new_construction_permits =
      sum(residential_new_construction),
    nearest_new_construction_year = if (
      any(residential_new_construction)
    ) {
      application_year[
        residential_new_construction
      ][which.min(abs(
        application_year_gap[residential_new_construction]
      ))]
    } else {
      NA_integer_
    },
    existing_building_permits_near_selected_year = sum(
      existing_building_language &
        dplyr::between(application_year_gap, -8L, 1L)
    ),
    demolition_permits = sum(demolition_permit),
    review_flag = dplyr::case_when(
      any(
        existing_building_language &
          dplyr::between(application_year_gap, -8L, 1L)
      ) ~ "existing_building_language_requires_review",
      any(
        residential_new_construction &
          dplyr::between(application_year_gap, -2L, 3L)
      ) ~ "new_construction_permit_support",
      TRUE ~ "no_decisive_exact_permit"
    ),
    evidence_permit_numbers = paste(
      permit_number[
        (existing_building_language &
          dplyr::between(application_year_gap, -8L, 1L)) |
          (residential_new_construction &
            dplyr::between(application_year_gap, -2L, 3L))
      ],
      collapse = "/"
    ),
    evidence_descriptions = paste(
      permit_description[
        (existing_building_language &
          dplyr::between(application_year_gap, -8L, 1L)) |
          (residential_new_construction &
            dplyr::between(application_year_gap, -2L, 3L))
      ],
      collapse = " | "
    ),
    .groups = "drop"
  ) |>
  dplyr::right_join(
    projects |>
      dplyr::select(
        project_id,
        audit_construction_year,
        review_address,
        audit_decision
      ),
    by = c("project_id", "audit_construction_year"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    exact_permits = dplyr::coalesce(exact_permits, 0L),
    residential_new_construction_permits = dplyr::coalesce(
      residential_new_construction_permits,
      0L
    ),
    existing_building_permits_near_selected_year =
      dplyr::coalesce(
        existing_building_permits_near_selected_year,
        0L
      ),
    demolition_permits = dplyr::coalesce(
      demolition_permits,
      0L
    ),
    review_flag = dplyr::coalesce(
      review_flag,
      "no_decisive_exact_permit"
    )
  ) |>
  dplyr::arrange(review_flag, project_id)

if (nrow(permit_summary) != nrow(projects)) {
  stop("Exact-address permit summary changed the Assessor-principal row count.")
}

readr::write_csv(
  permit_history,
  "../output/assessor_only_exact_permit_history.csv",
  na = ""
)
readr::write_csv(
  permit_summary,
  "../output/assessor_only_exact_permit_summary.csv",
  na = ""
)
