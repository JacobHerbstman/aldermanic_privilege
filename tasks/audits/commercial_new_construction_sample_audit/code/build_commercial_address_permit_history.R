# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_address <- function(x) {
  str_to_upper(coalesce(as.character(x), "")) %>%
    str_replace_all("\\bCHICAGO\\b", "") %>%
    str_replace_all("[^A-Z0-9 ]", " ") %>%
    str_replace_all(
      "\\b(STREET|ST|AVENUE|AVE|ROAD|RD|BOULEVARD|BLVD|COURT|CT|PLACE|PL|DRIVE|DR)\\b",
      ""
    ) %>%
    str_squish()
}

parse_address <- function(x) {
  normalized <- normalize_address(x)
  parts <- str_match(normalized, "^([0-9]+)(?:\\s+([0-9]+))?\\s+(.+)$")
  first_number <- suppressWarnings(as.integer(parts[, 2]))
  second_number <- suppressWarnings(as.integer(parts[, 3]))
  tibble::tibble(
    address_number_min = pmin(first_number, coalesce(second_number, first_number)),
    address_number_max = pmax(first_number, coalesce(second_number, first_number)),
    street_key = str_squish(parts[, 4])
  )
}

review_projects <- readr::read_csv(
  "../output/commercial_ground_up_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(
    current_distance_m <= 457.2,
    ground_up_review_required %in% TRUE
  ) %>%
  select(project_id, construction_year, selected_source_addresses) %>%
  tidyr::separate_rows(selected_source_addresses, sep = "\\s+/\\s+") %>%
  bind_cols(parse_address(.$selected_source_addresses)) %>%
  filter(
    is.finite(address_number_min),
    is.finite(address_number_max),
    street_key != ""
  ) %>%
  distinct(
    project_id,
    construction_year,
    selected_source_addresses,
    address_number_min,
    address_number_max,
    street_key
  )

permits <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  sf::st_drop_geometry() %>%
  filter(
    !is.na(application_start_date),
    !is.na(issue_date)
  ) %>%
  transmute(
    permit_id = as.character(id),
    permit_number = as.character(permit),
    permit_type,
    permit_status,
    application_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    work_description = str_squish(coalesce(work_description, ""))
  ) %>%
  bind_cols(parse_address(.$permit_address)) %>%
  filter(is.finite(address_number_min), street_key != "") %>%
  distinct(permit_id, address_number_min, street_key, .keep_all = TRUE)

permits_by_street <- split(permits, permits$street_key)
matches <- purrr::map_dfr(seq_len(nrow(review_projects)), function(i) {
  project <- review_projects[i, ]
  matched <- permits_by_street[[project$street_key]]
  if (is.null(matched) || nrow(matched) == 0) {
    return(tibble::tibble())
  }
  matched <- matched %>%
    filter(
      address_number_min >= project$address_number_min,
      address_number_min <= project$address_number_max,
      between(
        project$construction_year - lubridate::year(application_date),
        -3L,
        8L
      )
    )
  if (nrow(matched) == 0) {
    return(tibble::tibble())
  }
  bind_cols(
    project[rep(1, nrow(matched)), ],
    matched %>% select(-address_number_min, -address_number_max, -street_key)
  )
}) %>%
  distinct(project_id, permit_id, .keep_all = TRUE) %>%
  mutate(
    permit_evidence_type = case_when(
      permit_type == "PERMIT - NEW CONSTRUCTION" &
        str_detect(
          work_description,
          regex(
            "DWELLING|RESIDENTIAL|APARTMENT|MULTI[- ]?FAMILY|HOUSING|DORMITOR|SENIOR LIVING|\\b[0-9]{1,4}\\s*[- ]?\\s*(?:TOTAL\\s+)?(?:UNITS?|D\\.?U\\.?)\\b",
            ignore_case = TRUE
          )
        ) ~ "residential_new_construction",
      permit_type == "PERMIT - NEW CONSTRUCTION" ~ "other_new_construction",
      str_detect(
        work_description,
        regex(
          "CONVERT|CONVERSION|ADAPTIVE REUSE|CHANGE OF (USE|OCCUPANCY)|ADDITION TO|EXISTING BUILDING",
          ignore_case = TRUE
        )
      ) ~ "conversion_addition_or_existing_building",
      permit_type == "PERMIT - WRECKING/DEMOLITION" ~ "demolition",
      TRUE ~ "other_permit"
    )
  ) %>%
  select(
    project_id,
    construction_year,
    selected_source_addresses,
    permit_id,
    permit_number,
    permit_type,
    permit_status,
    application_date,
    issue_date,
    permit_address,
    permit_evidence_type,
    work_description
  ) %>%
  arrange(project_id, application_date, permit_number)

if (anyDuplicated(matches[c("project_id", "permit_id")]) > 0) {
  stop("Commercial address-permit history has duplicate project-permit keys.", call. = FALSE)
}

summary <- review_projects %>%
  distinct(project_id) %>%
  left_join(
    matches %>%
      group_by(project_id) %>%
      summarise(
        address_permits = n_distinct(permit_id),
        residential_new_construction_permits = n_distinct(
          permit_id[permit_evidence_type == "residential_new_construction"]
        ),
        other_new_construction_permits = n_distinct(
          permit_id[permit_evidence_type == "other_new_construction"]
        ),
        conversion_addition_permits = n_distinct(
          permit_id[permit_evidence_type == "conversion_addition_or_existing_building"]
        ),
        demolition_permits = n_distinct(
          permit_id[permit_evidence_type == "demolition"]
        ),
        permit_history = paste(
          paste0(
            permit_number,
            " [", permit_type, "; ", permit_status, "] ",
            work_description
          ),
          collapse = " || "
        ),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    across(
      c(
        address_permits,
        residential_new_construction_permits,
        other_new_construction_permits,
        conversion_addition_permits,
        demolition_permits
      ),
      ~ coalesce(.x, 0L)
    )
  ) %>%
  arrange(project_id)

if (anyDuplicated(summary$project_id) > 0) {
  stop("Commercial address-permit summary is not unique by project.", call. = FALSE)
}

readr::write_csv(matches, "../output/commercial_address_permit_history.csv")
readr::write_csv(summary, "../output/commercial_address_permit_history_summary.csv")
