# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

review_scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(source_family == "residential", review_scope == "review_within_1500ft") %>%
  select(project_id, distance_to_boundary_ft, within_500ft, review_scope)

candidates <- readr::read_csv(
  "../output/residential_adjudication_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  inner_join(review_scope, by = "project_id", relationship = "one-to-one")

components <- readr::read_csv(
  "../output/preferred_residential_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  semi_join(review_scope, by = "project_id") %>%
  distinct(project_id, component_pin)

if (anyDuplicated(candidates$project_id) > 0 ||
    anyDuplicated(review_scope$project_id) > 0 ||
    anyDuplicated(components[c("project_id", "component_pin")]) > 0 ||
    anyDuplicated(components$component_pin) > 0) {
  stop("Residential review inputs violate their project or component keys.", call. = FALSE)
}

tieback_temporal <- readr::read_csv(
  "../output/residential_tieback_temporal_lineage_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    tieback_lineage_id = readr::col_character(),
    selected_component_pins = readr::col_character(),
    all_lineage_pins = readr::col_character(),
    selected_source_row_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  rename(project_id = tieback_lineage_id)

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
  semi_join(review_scope, by = c("tieback_lineage_id" = "project_id")) %>%
  arrange(tieback_lineage_id, tax_year) %>%
  group_by(tieback_lineage_id) %>%
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
  ) %>%
  rename(project_id = tieback_lineage_id)

history <- readr::read_csv(
  "../output/residential_project_history_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) %>%
  inner_join(components, by = c("pin" = "component_pin"), relationship = "one-to-one")

if (anyDuplicated(history[c("project_id", "pin")]) > 0) {
  stop("Residential history is not unique by project-component PIN.", call. = FALSE)
}

history <- history %>%
  arrange(project_id, pin) %>%
  group_by(project_id) %>%
  summarise(
    component_history_evidence = paste0(
      pin,
      " year_built_values=", source_years,
      "; building_sqft=", source_building_areas,
      "; land_sqft=", source_land_areas,
      "; units=", source_unit_counts,
      "; max_concurrent_cards=", maximum_concurrent_cards,
      collapse = " || "
    ),
    .groups = "drop"
  )

multicard <- readr::read_csv(
  "../output/residential_multicard_cards.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    row_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  inner_join(components, by = c("pin" = "component_pin"), relationship = "many-to-one") %>%
  arrange(project_id, pin, tax_year, card_num) %>%
  group_by(project_id) %>%
  summarise(
    multicard_evidence = paste0(
      pin, ":card", card_num,
      " tax_year=", tax_year,
      "; year_built=", year_built,
      "; class=", class,
      "; units=", coalesce(as.character(num_apartments), "missing"),
      "; building_sqft=", coalesce(as.character(building_sqft), "missing"),
      "; land_sqft=", coalesce(as.character(land_sqft), "missing"),
      "; card_proration=", coalesce(as.character(card_proration_rate), "missing"),
      collapse = " || "
    ),
    .groups = "drop"
  )

component_projects <- components %>%
  mutate(source_project_id = paste0("residential_", component_pin))

permit_links <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    permit_number = readr::col_character(),
    application_date = readr::col_date(),
    issue_date = readr::col_date(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "residential") %>%
  inner_join(
    component_projects,
    by = c("project_id" = "source_project_id"),
    relationship = "many-to-one"
  ) %>%
  arrange(project_id.y, permit_chain_id, application_date, permit_number) %>%
  group_by(project_id = project_id.y) %>%
  summarise(
    permit_chain_evidence = paste0(
      permit_number,
      " [", permit_status, "] ",
      coalesce(as.character(application_date), "no application date"),
      " to ", coalesce(as.character(issue_date), "no issue date"),
      "; direct=", directly_matched,
      "; method=", coalesce(direct_match_method, "chain member"),
      "; address=", coalesce(permit_address, "missing"),
      "; ", str_squish(coalesce(work_description, "")),
      collapse = " || "
    ),
    .groups = "drop"
  )

permit_units <- readr::read_csv(
  "../output/project_permit_chain_unit_mentions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "residential") %>%
  inner_join(
    component_projects,
    by = c("project_id" = "source_project_id"),
    relationship = "many-to-one"
  ) %>%
  arrange(project_id.y, permit_chain_id, permit_number, mention_order) %>%
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

address_keys <- components %>%
  inner_join(
    candidates %>% select(project_id, construction_year),
    by = "project_id",
    relationship = "many-to-one"
  )

parcel_addresses <- readr::read_csv(
  "../input/density_parcel_address_selected_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    construction_year = readr::col_integer(),
    .default = readr::col_guess()
  )
) %>%
  inner_join(
    address_keys,
    by = c("pin" = "component_pin", "construction_year" = "construction_year"),
    relationship = "many-to-one"
  ) %>%
  distinct(
    project_id,
    pin,
    construction_year,
    selected_address,
    selected_address_year,
    selected_address_year_gap,
    address_selection_status
  ) %>%
  arrange(project_id, pin, selected_address_year_gap, selected_address) %>%
  group_by(project_id) %>%
  summarise(
    parcel_address_evidence = paste0(
      pin,
      " address=", coalesce(selected_address, "missing"),
      "; address_year=", coalesce(as.character(selected_address_year), "missing"),
      "; year_gap=", coalesce(as.character(selected_address_year_gap), "missing"),
      "; selection=", address_selection_status,
      collapse = " || "
    ),
    .groups = "drop"
  )

geocode_addresses <- readr::read_csv(
  "../output/preferred_historical_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "residential") %>%
  semi_join(review_scope, by = "project_id") %>%
  arrange(project_id, component_pin, selected_address_year_gap) %>%
  group_by(project_id) %>%
  summarise(
    geocode_address_evidence = paste0(
      component_pin,
      " address=", coalesce(selected_address, "missing"),
      "; address_year=", coalesce(as.character(selected_address_year), "missing"),
      "; selection=", address_selection_status,
      "; census=", census_status,
      collapse = " || "
    ),
    .groups = "drop"
  )

commercial_components <- readr::read_csv(
  "../output/preferred_commercial_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

commercial_candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    commercial_project_id = project_id,
    commercial_year = construction_year,
    commercial_units = dwelling_units,
    commercial_building_sqft = building_sqft,
    commercial_land_sqft = land_sqft,
    commercial_addresses = selected_source_addresses,
    commercial_status = candidate_status,
    commercial_reason = decision_reason
  )

commercial_overlap <- components %>%
  inner_join(
    commercial_components,
    by = c("component_pin" = "component_pin"),
    relationship = "one-to-many",
    suffix = c("", "_commercial")
  ) %>%
  transmute(project_id, commercial_project_id = project_id_commercial, component_pin) %>%
  distinct() %>%
  left_join(commercial_candidates, by = "commercial_project_id", relationship = "many-to-one") %>%
  arrange(project_id, commercial_project_id, component_pin) %>%
  group_by(project_id) %>%
  summarise(
    commercial_overlap_evidence = paste0(
      component_pin,
      " -> ", commercial_project_id,
      "; year=", commercial_year,
      "; units=", commercial_units,
      "; building_sqft=", commercial_building_sqft,
      "; land_sqft=", commercial_land_sqft,
      "; address=", commercial_addresses,
      "; status=", commercial_status,
      "; reason=", commercial_reason,
      collapse = " || "
    ),
    .groups = "drop"
  )

city_buildings <- readr::read_csv(
  "../output/residential_review_city_building_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

if (anyDuplicated(city_buildings$project_id) > 0) {
  stop("Residential City-building evidence is not unique by project.", call. = FALSE)
}

project_overlaps <- readr::read_csv(
  "../output/project_overlap_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(source_family == "residential") %>%
  select(-source_family)

if (anyDuplicated(project_overlaps$project_id) > 0) {
  stop("Residential project-overlap evidence is not unique by project.", call. = FALSE)
}

review <- candidates %>%
  left_join(tieback_temporal, by = "project_id", relationship = "one-to-one") %>%
  left_join(tieback_snapshots, by = "project_id", relationship = "one-to-one") %>%
  left_join(history, by = "project_id", relationship = "one-to-one") %>%
  left_join(multicard, by = "project_id", relationship = "one-to-one") %>%
  left_join(permit_links, by = "project_id", relationship = "one-to-one") %>%
  left_join(permit_units, by = "project_id", relationship = "one-to-one") %>%
  left_join(parcel_addresses, by = "project_id", relationship = "one-to-one") %>%
  left_join(geocode_addresses, by = "project_id", relationship = "one-to-one") %>%
  left_join(commercial_overlap, by = "project_id", relationship = "one-to-one") %>%
  left_join(city_buildings, by = "project_id", relationship = "one-to-one") %>%
  left_join(project_overlaps, by = "project_id", relationship = "one-to-one") %>%
  arrange(within_500ft, distance_to_boundary_ft, project_id)

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(
  names(review),
  regex(paste(prohibited_review_columns, collapse = "|"), ignore_case = TRUE)
))) {
  stop("Residential review bundle contains a prohibited analysis field.", call. = FALSE)
}
if (anyDuplicated(review$project_id) > 0 || nrow(review) != nrow(review_scope)) {
  stop("Residential review bundle is not one row per scoped project.", call. = FALSE)
}

summary <- bind_rows(
  review %>%
    count(project_kind, decision_reason, within_500ft, name = "value") %>%
    transmute(
      metric = paste(project_kind, decision_reason, within_500ft, sep = ":"),
      value
    ),
  tibble::tibble(metric = "projects", value = nrow(review))
)

readr::write_csv(review, "../output/residential_manual_review_bundle.csv")
readr::write_csv(summary, "../output/residential_manual_review_summary.csv")
