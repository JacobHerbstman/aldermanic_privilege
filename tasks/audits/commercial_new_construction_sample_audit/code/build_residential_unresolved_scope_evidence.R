# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(
    source_family == "residential",
    review_scope %in% c(
      "review_geography_unresolved",
      "review_year_or_geography_unresolved"
    )
  ) %>%
  select(project_id, review_scope, geography_status, distance_to_boundary_ft)

queue <- readr::read_csv(
  "../output/residential_adjudication_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_row_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  inner_join(scope, by = "project_id", relationship = "one-to-one")

components <- queue %>%
  select(project_id, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  rename(pin = component_pins) %>%
  distinct(project_id, pin)

if (nrow(queue) != nrow(scope) ||
    !setequal(queue$project_id, scope$project_id) ||
    anyDuplicated(queue$project_id) > 0 ||
    anyDuplicated(components[c("project_id", "pin")]) > 0 ||
    anyDuplicated(components$pin) > 0) {
  stop("The unresolved residential scope or its component map is incomplete or nonunique.", call. = FALSE)
}

connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

pin_sql <- paste(
  DBI::dbQuoteString(connection, unique(components$pin)),
  collapse = ", "
)

history <- DBI::dbGetQuery(
  connection,
  paste0(
    "SELECT trim(pin) AS pin, trim(year) AS tax_year, trim(card) AS card_num, ",
    "trim(class) AS property_class, trim(char_yrblt) AS year_built, ",
    "trim(char_bldg_sf) AS building_sqft, trim(char_land_sf) AS land_sqft, ",
    "trim(char_apts) AS apartments_text, trim(char_type_resd) AS residence_type, ",
    "trim(char_use) AS residence_use, trim(tieback_proration_rate) AS pin_proration_rate, ",
    "trim(card_proration_rate) AS card_proration_rate, trim(row_id) AS row_id ",
    "FROM read_csv('../input/residential_improvement_characteristics_full.csv', ",
    "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000) ",
    "WHERE trim(pin) IN (", pin_sql, ")"
  )
) %>%
  as_tibble() %>%
  mutate(
    across(
      c(
        tax_year, card_num, year_built, building_sqft, land_sqft,
        pin_proration_rate, card_proration_rate
      ),
      ~ suppressWarnings(as.numeric(str_replace_all(.x, "[^0-9.-]", ""))
      )
    ),
    apartment_value = suppressWarnings(as.numeric(str_replace_all(apartments_text, "[^0-9.-]", ""))),
    dwelling_units = case_when(
      is.finite(apartment_value) & apartment_value > 0 ~ apartment_value,
      str_to_lower(apartments_text) == "one" ~ 1,
      str_to_lower(apartments_text) == "two" ~ 2,
      str_to_lower(apartments_text) == "three" ~ 3,
      str_to_lower(apartments_text) == "four" ~ 4,
      str_to_lower(apartments_text) == "five" ~ 5,
      str_to_lower(apartments_text) == "six" ~ 6,
      str_detect(residence_use, regex("^single", ignore_case = TRUE)) ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  inner_join(components, by = "pin", relationship = "many-to-one") %>%
  arrange(project_id, pin, tax_year, card_num)

duplicate_history_keys <- history %>%
  count(pin, tax_year, card_num, name = "rows") %>%
  filter(rows > 1)
missing_history_pins <- setdiff(components$pin, history$pin)

if (nrow(duplicate_history_keys) > 0 || any(!queue$project_id %in% history$project_id)) {
  stop(
    paste0(
      "Unresolved-scope assessor history is missing or nonunique by PIN-year-card. Duplicate keys: ",
      paste(head(paste(duplicate_history_keys$pin, duplicate_history_keys$tax_year, duplicate_history_keys$card_num, sep = ":"), 10), collapse = ", "),
      "; missing projects: ", paste(head(setdiff(queue$project_id, history$project_id), 10), collapse = ", ")
    ),
    call. = FALSE
  )
}

history_summary <- history %>%
  group_by(project_id) %>%
  summarise(
    assessor_year_values = paste(sort(unique(year_built[is.finite(year_built)])), collapse = "/"),
    study_period_year_values = paste(
      sort(unique(year_built[between(year_built, 2006, 2022)])),
      collapse = "/"
    ),
    source_pin_count = n_distinct(pin),
    source_card_count = n_distinct(paste(pin, card_num)),
    assessor_history = paste0(
      pin, ":", tax_year, ":card", card_num,
      " year=", coalesce(as.character(year_built), "missing"),
      "; class=", coalesce(property_class, "missing"),
      "; units=", coalesce(as.character(dwelling_units), "missing"),
      "; building_sqft=", coalesce(as.character(building_sqft), "missing"),
      "; land_sqft=", coalesce(as.character(land_sqft), "missing"),
      collapse = " || "
    ),
    .groups = "drop"
  )

missing_history_summary <- components %>%
  filter(pin %in% missing_history_pins) %>%
  group_by(project_id) %>%
  summarise(
    component_pins_without_source_rows = paste(sort(pin), collapse = "/"),
    .groups = "drop"
  )

tieback_snapshots <- readr::read_csv(
  "../output/residential_tieback_temporal_snapshots.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    tieback_lineage_id = readr::col_character(),
    member_pins = readr::col_character(),
    source_row_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  semi_join(queue, by = c("tieback_lineage_id" = "project_id")) %>%
  arrange(tieback_lineage_id, tax_year) %>%
  group_by(project_id = tieback_lineage_id) %>%
  summarise(
    tieback_snapshot_evidence = paste0(
      tax_year,
      " [", snapshot_review_reason, "] pins=", member_pins,
      "; proration=", signif(pin_proration_sum, 5),
      "; year=", coalesce(as.character(construction_year), "missing"),
      "; units=", coalesce(as.character(dwelling_units), "missing"),
      "; building_sqft=", coalesce(as.character(building_sqft), "missing"),
      "; land_sqft=", coalesce(as.character(land_sqft), "missing"),
      collapse = " || "
    ),
    .groups = "drop"
  )

multicard_evidence <- readr::read_csv(
  "../output/residential_multicard_cards.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    row_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  inner_join(components, by = "pin", relationship = "many-to-one") %>%
  semi_join(queue %>% filter(project_kind == "same_pin_multiple_cards"), by = "project_id") %>%
  arrange(project_id, tax_year, card_num) %>%
  group_by(project_id) %>%
  summarise(
    multicard_evidence = paste0(
      "card", card_num,
      " tax_year=", tax_year,
      "; year=", year_built,
      "; class=", class,
      "; units=", coalesce(as.character(num_apartments), "missing"),
      "; building_sqft=", coalesce(as.character(building_sqft), "missing"),
      "; land_sqft=", coalesce(as.character(land_sqft), "missing"),
      collapse = " || "
    ),
    .groups = "drop"
  )

component_projects <- components %>%
  mutate(source_project_id = paste0("residential_", pin))

permit_links <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    permit_chain_id = readr::col_character(),
    application_date = readr::col_date(),
    issue_date = readr::col_date(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "residential", directly_matched) %>%
  inner_join(
    component_projects,
    by = c("project_id" = "source_project_id"),
    relationship = "many-to-one"
  ) %>%
  select(-project_id) %>%
  rename(project_id = project_id.y)

permit_points <- sf::st_read(
  "../input/building_permits_clean.gpkg",
  quiet = TRUE
) %>%
  sf::st_drop_geometry() %>%
  transmute(
    permit_id = id,
    permit_x_3435 = xcoordinate,
    permit_y_3435 = ycoordinate
  )

permit_summary <- permit_links %>%
  left_join(permit_points, by = "permit_id", relationship = "many-to-one") %>%
  arrange(project_id, application_date, permit_number) %>%
  group_by(project_id) %>%
  summarise(
    permit_point_x_3435 = if_else(n_distinct(permit_x_3435, na.rm = TRUE) == 1, first(permit_x_3435), NA_real_),
    permit_point_y_3435 = if_else(n_distinct(permit_y_3435, na.rm = TRUE) == 1, first(permit_y_3435), NA_real_),
    exact_permit_evidence = paste0(
      permit_number,
      " [", coalesce(permit_status, "missing status"), "] ",
      application_date,
      "; ", permit_address,
      "; ", str_squish(work_description),
      collapse = " || "
    ),
    .groups = "drop"
  )

permit_units <- readr::read_csv(
  "../output/project_permit_chain_unit_mentions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "residential") %>%
  inner_join(
    component_projects,
    by = c("project_id" = "source_project_id"),
    relationship = "many-to-one"
  ) %>%
  group_by(project_id = project_id.y) %>%
  summarise(
    permit_unit_evidence = paste0(
      permit_number,
      " units=", unit_count,
      " [", str_squish(unit_mention), "]",
      collapse = " || "
    ),
    .groups = "drop"
  )

current_addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_character())
) %>%
  inner_join(components, by = "pin", relationship = "many-to-one") %>%
  group_by(project_id) %>%
  summarise(
    current_addresses = paste(sort(unique(prop_address_full)), collapse = "/"),
    .groups = "drop"
  )

historical_addresses <- readr::read_csv(
  "../input/density_parcel_address_selected_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) %>%
  inner_join(components, by = "pin", relationship = "many-to-one") %>%
  group_by(project_id) %>%
  summarise(
    historical_addresses = paste0(
      pin, "@", construction_year, "=", selected_address,
      collapse = "/"
    ),
    .groups = "drop"
  )

historical_points <- readr::read_csv(
  "../input/density_historical_exact_parcel_records.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) %>%
  inner_join(components, by = "pin", relationship = "many-to-one") %>%
  group_by(project_id) %>%
  summarise(
    historical_point_evidence = paste0(
      pin, "@", year, "=", centroid_x_crs_3435, ",", centroid_y_crs_3435,
      collapse = "/"
    ),
    .groups = "drop"
  )

evidence <- queue %>%
  select(
    project_id,
    project_kind,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    current_distance_m,
    current_within_1500ft,
    decision_reason,
    review_scope,
    geography_status,
    distance_to_boundary_ft
  ) %>%
  left_join(history_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(missing_history_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(tieback_snapshots, by = "project_id", relationship = "one-to-one") %>%
  left_join(multicard_evidence, by = "project_id", relationship = "one-to-one") %>%
  left_join(permit_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(permit_units, by = "project_id", relationship = "one-to-one") %>%
  left_join(current_addresses, by = "project_id", relationship = "one-to-one") %>%
  left_join(historical_addresses, by = "project_id", relationship = "one-to-one") %>%
  left_join(historical_points, by = "project_id", relationship = "one-to-one") %>%
  arrange(review_scope, project_id)

if (nrow(evidence) != nrow(queue) ||
    !setequal(evidence$project_id, queue$project_id) ||
    anyDuplicated(evidence$project_id) > 0 ||
    any(is.na(evidence$assessor_year_values))) {
  stop("Unresolved-scope evidence is incomplete or nonunique.", call. = FALSE)
}

summary <- bind_rows(
  evidence %>% count(review_scope, project_kind, name = "value") %>%
    transmute(section = "scope", metric = paste(review_scope, project_kind, sep = ":"), value),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "projects",
      "projects_with_study_period_year",
      "projects_with_exact_permit",
      "projects_with_permit_point",
      "projects_with_current_address",
      "projects_with_historical_address",
      "projects_with_historical_point",
      "component_pins_without_source_rows"
    ),
    value = c(
      nrow(evidence),
      sum(evidence$study_period_year_values != ""),
      sum(!is.na(evidence$exact_permit_evidence)),
      sum(is.finite(evidence$permit_point_x_3435) & is.finite(evidence$permit_point_y_3435)),
      sum(!is.na(evidence$current_addresses)),
      sum(!is.na(evidence$historical_addresses)),
      sum(!is.na(evidence$historical_point_evidence)),
      length(missing_history_pins)
    )
  )
)

readr::write_csv(history, "../output/residential_unresolved_scope_assessor_history.csv")
readr::write_csv(evidence, "../output/residential_unresolved_scope_evidence.csv")
readr::write_csv(summary, "../output/residential_unresolved_scope_evidence_summary.csv")
