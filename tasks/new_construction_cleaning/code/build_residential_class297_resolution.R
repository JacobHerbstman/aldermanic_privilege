# setwd("tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
requests <- readr::read_csv("../output/residential_successor_condo_requests.csv",
  col_types = readr::cols(project_id = readr::col_character(), pin10 = readr::col_character(), .default = readr::col_guess()))

# Completed condo evidence also applies to earlier residential records. A tied
# parcel group additionally requires a completed permit confirming one building.
projects <- readr::read_csv("../output/preferred_residential_project_candidates.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    class_values = readr::col_character(), .default = readr::col_guess())) %>%
  filter(candidate_status == "review_required", project_kind == "class_297" | str_detect(class_values, "297") |
    (project_kind %in% c("single_pin_single_card", "tieback_building") & project_id %in% requests$project_id))
inventory <- readr::read_csv("../output/residential_project_candidate_inventory.csv",
  col_types = readr::cols(pin = readr::col_character(), year_built = readr::col_double(),
    num_apartments = readr::col_double(), .default = readr::col_skip()))
stopifnot(!anyDuplicated(inventory$pin))
component_counts <- projects %>% select(project_id, component_pins, construction_year) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  left_join(inventory, by = c("component_pins" = "pin"), relationship = "many-to-one") %>%
  group_by(project_id) %>% summarise(
    confirmed_component_units = if (all(is.finite(num_apartments) & num_apartments > 0 &
      year_built == construction_year) && n_distinct(num_apartments) == 1)
      first(num_apartments) else NA_real_, .groups = "drop")
projects <- projects %>% left_join(component_counts, by = "project_id", relationship = "one-to-one") %>%
  mutate(dwelling_units = coalesce(dwelling_units, confirmed_component_units))
condos <- readr::read_csv("../input/construction_condominium_history.csv",
  col_types = readr::cols(pin = readr::col_character(), pin10 = readr::col_character(), row_id = readr::col_character(),
    year = readr::col_double(), char_yrblt = readr::col_double(), char_building_sf = readr::col_double(),
    char_land_sf = readr::col_double(), char_building_pins = readr::col_double(),
    char_building_non_units = readr::col_double(), is_parking_space = readr::col_logical(),
    is_common_area = readr::col_logical(), .default = readr::col_skip()))
stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(requests[c("project_id", "pin10")]),
  !anyDuplicated(condos[c("pin", "year")]), !anyNA(condos$row_id))

# A completed new-building permit can confirm the finished residential count
# when the earlier development record is incomplete or includes commercial space.
permit_links <- readr::read_csv("../output/project_permit_chain_links.csv",
  col_types = readr::cols(.default = readr::col_character()))
mentions <- readr::read_csv("../output/project_permit_chain_unit_mentions.csv",
  col_types = readr::cols(permit_number = readr::col_character(),
    permit_chain_id = readr::col_character(), unit_count = readr::col_double(), .default = readr::col_skip())) %>%
  distinct(permit_chain_id, permit_number, unit_count)
permit_records <- permit_links %>% select(permit_chain_id, permit_number,
  permit_type, permit_status, work_description) %>% distinct()
stopifnot(!anyDuplicated(permit_records[c("permit_chain_id", "permit_number")]))
permit_confirmation <- vector("list", nrow(projects))
for (i in seq_len(nrow(projects))) {
  component_sources <- paste0("residential_", str_split(projects$component_pins[i], "/")[[1]])
  chains <- unique(c(permit_links$permit_chain_id[permit_links$project_id %in%
    c(projects$project_id[i], component_sources)], str_split(coalesce(projects$permit_chain_ids[i], ""), "/")[[1]]))
  chains <- chains[!is.na(chains) & chains != ""]
  history <- permit_records %>% filter(permit_chain_id %in% chains)
  counts <- mentions %>% filter(permit_chain_id %in% chains, is.finite(unit_count), unit_count > 0)
  full_building <- history %>% filter(permit_status == "COMPLETE", permit_type == "PERMIT - NEW CONSTRUCTION",
    str_detect(work_description, regex("ERECT|NEW CONSTRUCTION|NEW [0-9]+ STORY", ignore_case = TRUE)),
    !str_detect(work_description, regex("REVISION|ALTERATION|CONVERT|EXISTING|FOUNDATION ONLY", ignore_case = TRUE)))
  confirmed <- length(chains) == 1 && nrow(full_building) == 1 &&
    n_distinct(counts$unit_count) == 1 && any(counts$permit_number %in% full_building$permit_number)
  permit_confirmation[[i]] <- tibble(source_project_id = projects$project_id[i],
    confirmed_permit_units = if (confirmed) first(counts$unit_count) else NA_real_,
    permit_chain_evidence = paste(sort(chains), collapse = "/"),
    permit_unit_evidence = paste(sort(unique(paste0(counts$permit_number, ":", counts$unit_count))), collapse = "/"))
}
permit_confirmation <- bind_rows(permit_confirmation)
stopifnot(!anyDuplicated(permit_confirmation$source_project_id))

# Unit records repeat whole-building areas. Count residential units, but never
# add repeated building or land areas. Keep every measurement in one assessment.
snapshots <- condos %>% group_by(pin10, year) %>% summarise(
  records = n(), residential_pin_records = sum(!is_parking_space & !is_common_area, na.rm = TRUE),
  parking_pin_records = sum(is_parking_space %in% TRUE),
  complete_membership = all(!is.na(is_parking_space) & !is.na(is_common_area)) &&
    all(!is.na(char_building_pins) & char_building_pins == n()) &&
    all(!is.na(char_building_non_units) & char_building_non_units == sum(is_parking_space | is_common_area)),
  year_values = n_distinct(char_yrblt[!is_parking_space & !is_common_area], na.rm = FALSE),
  building_values = n_distinct(char_building_sf[!is_parking_space & !is_common_area], na.rm = FALSE),
  land_values = n_distinct(char_land_sf[!is_parking_space & !is_common_area], na.rm = FALSE),
  construction_year = first(char_yrblt[!is_parking_space & !is_common_area]),
  building_sqft = first(char_building_sf[!is_parking_space & !is_common_area]),
  land_sqft = first(char_land_sf[!is_parking_space & !is_common_area]),
  source_rows = paste(sort(row_id), collapse = "/"), .groups = "drop") %>%
  mutate(complete_density_measurements = complete_membership & residential_pin_records > 0 &
    year_values == 1 & is.finite(construction_year) & land_values == 1 & is.finite(land_sqft) & land_sqft > 1,
    complete_measurements = complete_membership & residential_pin_records > 0 &
    year_values == 1 & is.finite(construction_year) & building_values == 1 &
    is.finite(building_sqft) & building_sqft > 1 & land_values == 1 & is.finite(land_sqft) & land_sqft > 1,
    report_priority = case_when(year == 2022 ~ 1L, year == 2025 ~ 2L, TRUE ~ 3L)) %>%
  filter(year %in% c(2022, 2025)) %>% arrange(pin10, desc(complete_density_measurements), desc(complete_measurements), report_priority) %>%
  group_by(pin10) %>% slice_head(n = 1) %>% ungroup()
stopifnot(!anyDuplicated(snapshots$pin10))

# Completed records supply the year. Reviewed buildings were handled upstream.
links <- requests %>% group_by(project_id) %>% mutate(successor_buildings = n()) %>% ungroup() %>%
  left_join(snapshots, by = "pin10", relationship = "many-to-one")
decisions <- projects %>% transmute(source_project_id = project_id, component_pins,
  candidate_year = construction_year, candidate_units = dwelling_units, candidate_building_sqft = building_sqft,
    requires_permit_confirmation = project_kind == "tieback_building" & !str_detect(class_values, "297")) %>%
  left_join(links, by = c("source_project_id" = "project_id"), relationship = "one-to-many") %>%
  left_join(permit_confirmation, by = "source_project_id", relationship = "many-to-one") %>%
  mutate(completed_assessor_year = construction_year,
    confirmed_identity = coalesce(successor_buildings == 1 & projects_per_condo_base == 1 &
      complete_membership & residential_pin_records > 0 & year_values == 1 & is.finite(construction_year), FALSE),
    confirmed_single_building = coalesce(successor_buildings == 1 & projects_per_condo_base == 1 &
      complete_membership & year_values == 1 & is.finite(construction_year) &
      (coalesce(!requires_permit_confirmation & candidate_units == residential_pin_records, FALSE) |
       coalesce(confirmed_permit_units == residential_pin_records, FALSE)), FALSE),
    decision_action = case_when(
      confirmed_identity & (!requires_permit_confirmation | confirmed_single_building) & (construction_year < 2006 | construction_year > 2022) ~ "exclude_outside_study_period",
      confirmed_single_building & complete_density_measurements & between(construction_year, 2006, 2022) ~ "retain_successor_evidence",
      TRUE ~ "unresolved"),
    final_project_id = if_else(decision_action == "retain_successor_evidence", paste0("residential_condo_", pin10), NA_character_),
    dwelling_units = residential_pin_records,
    allow_dupac = decision_action == "retain_successor_evidence",
    allow_far = allow_dupac & complete_measurements,
    building_sqft = if_else(allow_far, building_sqft, NA_real_),
    membership_source = paste0("complete_condominium_base:", pin10),
    year_source = paste0("condominium_assessment:", year, ":", pin10),
    units_source = paste0("condominium_assessment:", year, ":", pin10),
    building_source = units_source, land_source = units_source,
    decision_reason = case_when(
      decision_action == "exclude_outside_study_period" ~ "One uniquely matched completed condominium building reports construction outside 2006–2022. Use its completed Assessor year unless a recorded stronger completion decision overrides it.",
      decision_action == "retain_successor_evidence" ~ "Complete condominium assessment confirms the unit count and supplies the construction-year proxy unless a recorded stronger completion decision overrides it; all residential records report identical land area. Floor area is used only when consistently reported in the same assessment.",
      is.na(pin10) ~ "No matched completed condominium building.",
      successor_buildings != 1 | projects_per_condo_base != 1 ~ "The source parcel and completed buildings do not have a unique one-to-one match.",
      !coalesce(complete_membership, FALSE) ~ "The unit records do not establish complete building membership.",
      !coalesce(year_values == 1 & is.finite(construction_year), FALSE) ~ "The completed records do not supply a consistent construction year.",
      !confirmed_single_building ~ "The earlier count and unambiguous completed-permit evidence do not confirm the finished residential unit count.",
      TRUE ~ "Whole-building measurements are missing or inconsistent within the assessment."),
    evidence_ids = paste0("condo_base:", pin10, ";assessment_rows:", source_rows,
      if_else(coalesce(confirmed_permit_units == residential_pin_records, FALSE),
        paste0(";completed_permit_units:", permit_unit_evidence), "")),
    confidence = if_else(allow_dupac, "source_agreement", "unresolved"),
    distance_to_boundary_ft = NA_real_) %>%
  select(source_project_id, final_project_id, decision_action, component_pins, condo_base = pin10,
    construction_year, candidate_year, completed_assessor_year, candidate_units, confirmed_permit_units, dwelling_units, building_sqft, land_sqft, allow_far, allow_dupac,
    membership_source, year_source, units_source, building_source, land_source, decision_reason,
    evidence_ids, confidence, distance_to_boundary_ft, permit_chain_evidence, permit_unit_evidence,
    condo_cohort_year = year, condo_parking_pins = parking_pin_records, condo_land_distinct_values = land_values) %>%
  arrange(source_project_id, condo_base)
stopifnot(setequal(projects$project_id, decisions$source_project_id),
  !anyDuplicated(decisions[c("source_project_id", "condo_base")]),
  !anyDuplicated(na.omit(decisions$final_project_id)))
source_disposition <- decisions %>% group_by(source_project_id) %>% summarise(
  decision_rows = n(), actions = paste(sort(unique(decision_action)), collapse = "/"),
  retained_projects = sum(allow_dupac), final_project_ids = paste(sort(na.omit(final_project_id)), collapse = "/"), .groups = "drop")
SaveData(decisions, character(), "../output/residential_class297_resolution.csv")
SaveData(source_disposition, c("source_project_id"), "../output/residential_class297_source_disposition.csv")
