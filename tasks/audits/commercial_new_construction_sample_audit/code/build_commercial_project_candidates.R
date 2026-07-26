# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin <- function(x) {
  value <- str_replace_all(str_squish(as.character(x)), "[^0-9]", "")
  if_else(str_length(value) == 14, value, NA_character_)
}

single_finite_value <- function(x) {
  values <- sort(unique(x[is.finite(x)]))
  if (length(values) == 1) values else NA_real_
}

normalize_address <- function(x) {
  value <- str_to_upper(coalesce(x, "")) %>%
    str_replace_all("\\bCHICAGO\\b", "") %>%
    str_replace_all("[^A-Z0-9 ]", " ") %>%
    str_squish() %>%
    str_replace("\\b(STREET|ST|AVENUE|AVE|ROAD|RD|BOULEVARD|BLVD|COURT|CT|PLACE|PL|DRIVE|DR)$", "") %>%
    str_squish()
  if_else(str_detect(value, "^[0-9]+ [A-Z]"), value, NA_character_)
}

parse_component_pins <- function(keypin, pins_text) {
  source_text <- coalesce(pins_text, "")
  formatted <- str_extract_all(
    source_text,
    "(?<![0-9])[0-9]{2}-[0-9]{2}-[0-9]{3}-[0-9]{3}-[0-9]{4}(?![0-9])"
  )[[1]]
  parsed <- str_replace_all(formatted, "-", "")
  repair_applied <- FALSE

  malformed_final <- str_extract_all(
    source_text,
    "(?<![0-9])[0-9]{2}-[0-9]{2}-[0-9]{3}-[0-9]{3}-[0-9]{3}(?![0-9])"
  )[[1]]
  if (length(malformed_final) > 0) {
    parsed <- c(parsed, paste0(str_replace_all(malformed_final, "-", ""), "0"))
    repair_applied <- TRUE
  }

  if (str_detect(source_text, regex("\\bthru\\b", ignore_case = TRUE)) && length(parsed) >= 2) {
    first_pin <- parsed[1]
    last_pin <- parsed[length(parsed)]
    if (str_sub(first_pin, 1, 7) == str_sub(last_pin, 1, 7) &&
        str_sub(first_pin, 11, 14) == str_sub(last_pin, 11, 14)) {
      first_piece <- as.integer(str_sub(first_pin, 8, 10))
      last_piece <- as.integer(str_sub(last_pin, 8, 10))
      if (is.finite(first_piece) && is.finite(last_piece) &&
          last_piece >= first_piece && last_piece - first_piece <= 500) {
        parsed <- c(
          parsed,
          paste0(
            str_sub(first_pin, 1, 7),
            str_pad(seq(first_piece, last_piece), 3, pad = "0"),
            str_sub(first_pin, 11, 14)
          )
        )
        repair_applied <- TRUE
      }
    }
  }

  parsed <- sort(unique(c(keypin, parsed[!is.na(parsed) & str_length(parsed) == 14])))
  digit_residue <- source_text %>%
    str_replace_all("[0-9]{2}-[0-9]{2}-[0-9]{3}-[0-9]{3}-[0-9]{4}", "") %>%
    str_replace_all("[0-9]{2}-[0-9]{2}-[0-9]{3}-[0-9]{3}-[0-9]{3}", "") %>%
    str_replace_all("[^0-9]", "")

  tibble::tibble(
    component_pin = parsed,
    parser_repair_applied = repair_applied,
    parser_numeric_residue = digit_residue != ""
  )
}

chicago_townships <- c(
  "West Chicago", "South Chicago", "Jefferson", "North Chicago",
  "Lake View", "Rogers Park", "Hyde Park", "Lake"
)
apartment_unit_cols <- c("studiounits", "x1brunits", "x2brunits", "x3brunits", "x4brunits")

raw <- readr::read_csv(
  "../input/commercial_value_raw.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) %>%
  janitor::clean_names()

if (!"modelgroup" %in% names(raw) && "sheet" %in% names(raw)) {
  raw <- raw %>% rename(modelgroup = sheet)
}

raw <- raw %>%
  mutate(
    raw_row = row_number(),
    keypin = normalize_pin(keypin),
    across(
      any_of(c("year", apartment_unit_cols, "tot_units", "bldgsf", "landsf", "yearbuilt")),
      ~ suppressWarnings(as.numeric(str_replace_all(.x, "[^0-9.\\-]", "")))
    ),
    apartment_unit_sum = rowSums(pick(all_of(apartment_unit_cols)), na.rm = TRUE),
    reported_units = case_when(
      apartment_unit_sum > 0 ~ apartment_unit_sum,
      is.finite(tot_units) & tot_units > 0 ~ tot_units,
      TRUE ~ NA_real_
    ),
    chicago = township %in% chicago_townships,
    multifamily_model = str_detect(
      modelgroup,
      regex("Multifamily|Class3|Class9|Condos", ignore_case = TRUE)
    )
  ) %>%
  filter(chicago, multifamily_model, !is.na(keypin), is.finite(reported_units))

candidate_keypins <- raw %>%
  group_by(keypin) %>%
  summarise(any_post_1998_report = any(yearbuilt >= 1999, na.rm = TRUE), .groups = "drop") %>%
  filter(any_post_1998_report)

components <- purrr::pmap_dfr(
  raw %>% select(raw_row, keypin, pins),
  function(raw_row, keypin, pins) {
    parse_component_pins(keypin, pins) %>%
      mutate(raw_row = raw_row, keypin = keypin, .before = 1)
  }
) %>%
  distinct(raw_row, keypin, component_pin, .keep_all = TRUE)

graph_edges <- components %>%
  transmute(
    from = paste0("row:", raw_row),
    to = paste0("pin:", component_pin)
  )

commercial_graph <- igraph::graph_from_data_frame(graph_edges, directed = FALSE)
graph_membership <- igraph::components(commercial_graph)$membership
node_membership <- tibble::tibble(
  node = names(graph_membership),
  graph_component = as.integer(graph_membership)
)

row_families <- node_membership %>%
  filter(str_starts(node, "row:")) %>%
  transmute(raw_row = as.integer(str_remove(node, "^row:")), graph_component)

candidate_graph_components <- raw %>%
  filter(keypin %in% candidate_keypins$keypin) %>%
  select(raw_row) %>%
  inner_join(row_families, by = "raw_row", relationship = "one-to-one") %>%
  distinct(graph_component)

entity_rows <- raw %>%
  inner_join(row_families, by = "raw_row", relationship = "one-to-one") %>%
  filter(graph_component %in% candidate_graph_components$graph_component) %>%
  group_by(graph_component) %>%
  mutate(
    family_keypin = min(keypin),
    project_family_id = paste0("commercial_", family_keypin)
  ) %>%
  ungroup() %>%
  select(
    project_family_id,
    raw_row,
    keypin,
    valuation_year = year,
    yearbuilt,
    reported_units,
    apartment_unit_sum,
    source_tot_units = tot_units,
    bldgsf,
    landsf,
    address,
    modelgroup,
    property_type_use,
    property_name_description,
    pins
  )

entity_components <- components %>%
  inner_join(
    entity_rows %>% select(project_family_id, raw_row),
    by = "raw_row",
    relationship = "many-to-one"
  ) %>%
  select(
    project_family_id,
    raw_row,
    keypin,
    component_pin,
    parser_repair_applied,
    parser_numeric_residue
  )

family_vintage_rows <- entity_rows %>%
  group_by(project_family_id, valuation_year) %>%
  summarise(
    source_rows = n(),
    keypins = n_distinct(keypin),
    source_keypins = paste(sort(unique(keypin)), collapse = "/"),
    yearbuilt_values = n_distinct(yearbuilt, na.rm = TRUE),
    unit_values = n_distinct(reported_units, na.rm = TRUE),
    building_area_values = n_distinct(bldgsf, na.rm = TRUE),
    land_area_values = n_distinct(landsf, na.rm = TRUE),
    yearbuilt = single_finite_value(yearbuilt),
    reported_units = single_finite_value(reported_units),
    bldgsf = single_finite_value(bldgsf),
    landsf = single_finite_value(landsf),
    source_yearbuilt = paste(sort(unique(yearbuilt[is.finite(yearbuilt)])), collapse = "/"),
    source_units = paste(sort(unique(reported_units[is.finite(reported_units)])), collapse = "/"),
    source_building_areas = paste(sort(unique(bldgsf[is.finite(bldgsf)])), collapse = "/"),
    source_land_areas = paste(sort(unique(landsf[is.finite(landsf)])), collapse = "/"),
    within_vintage_conflict =
      keypins > 1 |
      yearbuilt_values > 1 |
      unit_values > 1 |
      building_area_values > 1 |
      land_area_values > 1,
    .groups = "drop"
  )

family_vintage_components <- entity_components %>%
  inner_join(
    entity_rows %>% select(project_family_id, raw_row, valuation_year),
    by = c("project_family_id", "raw_row"),
    relationship = "many-to-one"
  ) %>%
  group_by(project_family_id, valuation_year) %>%
  summarise(
    component_pins = n_distinct(component_pin),
    component_pin_list = paste(sort(unique(component_pin)), collapse = "/"),
    parser_repair_applied = any(parser_repair_applied),
    parser_numeric_residue = any(parser_numeric_residue),
    .groups = "drop"
  )

family_vintages <- family_vintage_rows %>%
  left_join(
    family_vintage_components,
    by = c("project_family_id", "valuation_year"),
    relationship = "one-to-one"
  )

vintage_2021 <- family_vintages %>%
  filter(valuation_year == 2021) %>%
  select(-valuation_year) %>%
  rename_with(~ paste0(.x, "_2021"), -project_family_id)

vintage_2024 <- family_vintages %>%
  filter(valuation_year == 2024) %>%
  select(-valuation_year) %>%
  rename_with(~ paste0(.x, "_2024"), -project_family_id)

cross_vintage_comparison <- full_join(
  vintage_2021,
  vintage_2024,
  by = "project_family_id",
  relationship = "one-to-one"
) %>%
  mutate(
    observed_2021 = !is.na(source_rows_2021),
    observed_2024 = !is.na(source_rows_2024),
    both_vintages = observed_2021 & observed_2024,
    within_vintage_conflict =
      coalesce(within_vintage_conflict_2021, FALSE) |
      coalesce(within_vintage_conflict_2024, FALSE),
    component_membership_changed =
      both_vintages & component_pin_list_2021 != component_pin_list_2024,
    yearbuilt_changed =
      both_vintages & is.finite(yearbuilt_2021) & is.finite(yearbuilt_2024) &
      yearbuilt_2021 != yearbuilt_2024,
    units_changed =
      both_vintages & is.finite(reported_units_2021) & is.finite(reported_units_2024) &
      reported_units_2021 != reported_units_2024,
    building_area_changed =
      both_vintages & is.finite(bldgsf_2021) & is.finite(bldgsf_2024) &
      bldgsf_2021 != bldgsf_2024,
    land_area_changed =
      both_vintages & is.finite(landsf_2021) & is.finite(landsf_2024) &
      landsf_2021 != landsf_2024,
    stable_physical_fields =
      both_vintages &
      !within_vintage_conflict &
      !component_membership_changed &
      !units_changed &
      !building_area_changed &
      !land_area_changed,
    review_reason = case_when(
      coalesce(parser_numeric_residue_2021, FALSE) |
        coalesce(parser_numeric_residue_2024, FALSE) ~ "unparsed_component_text",
      within_vintage_conflict ~ "within_vintage_entity_conflict",
      component_membership_changed ~ "component_membership_changed",
      !both_vintages ~ "one_vintage_only",
      yearbuilt_changed & stable_physical_fields ~ "year_recode_with_stable_physical_fields",
      units_changed | building_area_changed | land_area_changed ~ "physical_fields_changed",
      yearbuilt_changed ~ "yearbuilt_changed",
      TRUE ~ "stable_across_vintages"
    )
  )

commercial_address_family_candidates <- entity_rows %>%
  mutate(address_key = normalize_address(address)) %>%
  filter(!is.na(address_key)) %>%
  distinct(address_key, valuation_year, project_family_id, keypin, address) %>%
  group_by(address_key) %>%
  summarise(
    project_families = n_distinct(project_family_id),
    families_2021 = n_distinct(project_family_id[valuation_year == 2021]),
    families_2024 = n_distinct(project_family_id[valuation_year == 2024]),
    project_family_ids = paste(sort(unique(project_family_id)), collapse = "/"),
    keypins = paste(sort(unique(keypin)), collapse = "/"),
    source_addresses = paste(sort(unique(address)), collapse = " / "),
    .groups = "drop"
  ) %>%
  filter(project_families > 1, families_2021 > 0, families_2024 > 0) %>%
  arrange(address_key)

production <- readr::read_csv(
  "../input/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    yearbuilt = readr::col_double(),
    tot_units = readr::col_double(),
    bldgsf = readr::col_double(),
    landsf = readr::col_double(),
    address = readr::col_character(),
    .default = readr::col_skip()
  )
)

boundary_distance <- readr::read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    dist_to_boundary_m = readr::col_double(),
    .default = readr::col_skip()
  )
)

if (anyDuplicated(boundary_distance$pin) > 0) {
  stop("Boundary-distance input is not unique by PIN.", call. = FALSE)
}

production <- production %>%
  left_join(boundary_distance, by = c("pin" = "pin"), relationship = "one-to-one") %>%
  mutate(within_1500ft = !is.na(dist_to_boundary_m) & dist_to_boundary_m <= 457.2)

entity_keypin_family <- entity_rows %>%
  distinct(project_family_id, keypin)

if (anyDuplicated(entity_keypin_family$keypin) > 0) {
  stop("A commercial key PIN appears in more than one cross-vintage family.", call. = FALSE)
}

production_families <- production %>%
  transmute(
    keypin = pin,
    production_yearbuilt = yearbuilt,
    production_units = tot_units,
    production_bldgsf = bldgsf,
    production_landsf = landsf,
    production_address = address,
    dist_to_boundary_m,
    within_1500ft
  ) %>%
  inner_join(
    entity_keypin_family,
    by = "keypin",
    relationship = "many-to-one"
  ) %>%
  distinct(project_family_id, keypin, .keep_all = TRUE)

if (!setequal(production$pin, production_families$keypin)) {
  stop("One or more production commercial key PINs are missing from the family graph.", call. = FALSE)
}
if (anyDuplicated(production_families$keypin) > 0) {
  stop("A production commercial key PIN maps to multiple project families.", call. = FALSE)
}

family_rows <- entity_rows %>%
  group_by(project_family_id) %>%
  summarise(
    source_rows = n(),
    keypins = n_distinct(keypin),
    valuation_vintages = n_distinct(valuation_year, na.rm = TRUE),
    yearbuilt_values = n_distinct(yearbuilt, na.rm = TRUE),
    unit_values = n_distinct(reported_units, na.rm = TRUE),
    building_area_values = n_distinct(bldgsf, na.rm = TRUE),
    land_area_values = n_distinct(landsf, na.rm = TRUE),
    missing_building_area_rows = sum(!is.finite(bldgsf)),
    source_keypins = paste(sort(unique(keypin)), collapse = "/"),
    source_yearbuilt = paste(sort(unique(yearbuilt[is.finite(yearbuilt)])), collapse = "/"),
    source_units = paste(sort(unique(reported_units[is.finite(reported_units)])), collapse = "/"),
    .groups = "drop"
  )

family_components <- entity_components %>%
  group_by(project_family_id) %>%
  summarise(
    component_pins = n_distinct(component_pin),
    component_pin_list = paste(sort(unique(component_pin)), collapse = "/"),
    parser_repair_applied = any(parser_repair_applied),
    parser_numeric_residue = any(parser_numeric_residue),
    .groups = "drop"
  )

family_production <- production_families %>%
  group_by(project_family_id) %>%
  summarise(
    production_entities = n_distinct(keypin),
    production_keypins = paste(sort(unique(keypin)), collapse = "/"),
    any_within_1500ft = any(within_1500ft),
    minimum_boundary_distance_m = suppressWarnings(min(dist_to_boundary_m, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    minimum_boundary_distance_m = if_else(
      is.infinite(minimum_boundary_distance_m),
      NA_real_,
      minimum_boundary_distance_m
    )
  )

family_review <- family_rows %>%
  left_join(family_components, by = "project_family_id", relationship = "one-to-one") %>%
  left_join(family_production, by = "project_family_id", relationship = "one-to-one") %>%
  left_join(
    cross_vintage_comparison %>%
      select(
        project_family_id,
        both_vintages,
        within_vintage_conflict,
        component_membership_changed,
        yearbuilt_changed,
        units_changed,
        building_area_changed,
        land_area_changed,
        stable_physical_fields,
        cross_vintage_review_reason = review_reason
      ),
    by = "project_family_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    production_entities = coalesce(production_entities, 0L),
    any_within_1500ft = coalesce(any_within_1500ft, FALSE),
    requires_review =
      production_entities > 1 |
      keypins > 1 |
      yearbuilt_values > 1 |
      unit_values > 1 |
      building_area_values > 1 |
      land_area_values > 1 |
      missing_building_area_rows > 0 |
      parser_repair_applied |
      parser_numeric_residue,
    legacy_review_reason = case_when(
      production_entities > 1 ~ "overlapping_production_entities",
      keypins > 1 ~ "cross_vintage_entity_membership",
      parser_numeric_residue ~ "unparsed_component_text",
      parser_repair_applied ~ "repaired_component_pin",
      yearbuilt_values > 1 ~ "conflicting_yearbuilt",
      unit_values > 1 ~ "conflicting_units",
      building_area_values > 1 ~ "conflicting_building_area",
      land_area_values > 1 ~ "conflicting_land_area",
      missing_building_area_rows > 0 ~ "missing_building_area",
      TRUE ~ "stable_entity_candidate"
    ),
    review_reason = cross_vintage_review_reason
  ) %>%
  arrange(desc(any_within_1500ft), desc(requires_review), project_family_id)

summary <- tibble::tribble(
  ~metric, ~value,
  "commercial_project_families", nrow(family_review),
  "production_commercial_entities", nrow(production),
  "production_project_families", sum(family_review$production_entities > 0),
  "project_families_within_1500ft", sum(family_review$any_within_1500ft),
  "review_families_within_1500ft", sum(family_review$any_within_1500ft & family_review$requires_review),
  "families_with_multiple_production_entities", sum(family_review$production_entities > 1),
  "families_with_cross_vintage_keypins", sum(family_review$keypins > 1),
  "families_with_component_parser_repairs", sum(family_review$parser_repair_applied),
  "families_with_unparsed_component_text", sum(family_review$parser_numeric_residue),
  "families_with_year_recode_and_stable_physical_fields",
    sum(family_review$review_reason == "year_recode_with_stable_physical_fields"),
  "families_with_changed_physical_fields",
    sum(family_review$review_reason == "physical_fields_changed"),
  "families_with_changed_component_membership",
    sum(family_review$review_reason == "component_membership_changed"),
  "families_with_within_vintage_conflicts",
    sum(family_review$review_reason == "within_vintage_entity_conflict"),
  "cross_family_address_candidates",
    nrow(commercial_address_family_candidates)
)

readr::write_csv(summary, "../output/commercial_project_candidate_summary.csv")
readr::write_csv(family_review, "../output/commercial_project_family_review.csv")
readr::write_csv(family_vintages, "../output/commercial_family_vintage_summary.csv")
readr::write_csv(cross_vintage_comparison, "../output/commercial_cross_vintage_comparison.csv")
readr::write_csv(commercial_address_family_candidates, "../output/commercial_address_family_candidates.csv")
readr::write_csv(entity_rows, "../output/commercial_entity_version_candidates.csv")
readr::write_csv(entity_components, "../output/commercial_entity_component_candidates.csv")
readr::write_csv(production_families, "../output/commercial_production_family_members.csv")
