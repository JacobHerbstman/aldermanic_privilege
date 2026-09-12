# setwd("tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
# Assessment selection is complete. Apply recorded building decisions once,
# then determine which density measurements have usable inputs.
residential_candidates <- readr::read_csv("../output/residential_selected_assessments.csv", na = "NA",
  col_types = readr::cols(project_id = "c", component_pins = "c", class_values = "c",
    source_row_ids = "c", permit_numbers = "c", .default = readr::col_guess()))
stopifnot(!anyDuplicated(residential_candidates$project_id))
permit_links <- readr::read_csv("../output/project_permit_chain_links.csv",
  col_types = readr::cols(project_id = "c", permit_number = "c", .default = readr::col_guess())) %>%
  filter(source_family == "residential", permit_type == "PERMIT - NEW CONSTRUCTION")

modifications <- readr::read_csv("../input/construction_modifications.csv",
  col_types = readr::cols(construction_year = "d", dwelling_units = "d", building_sqft = "d",
    land_sqft = "d", reported_land_sqft = "d", allow_far = "l", allow_dupac = "l",
    .default = readr::col_character())) %>%
  filter(source_family == "residential", application_stage == "building_measurements")

# Approved individual lots replace development-wide land repeated on a home.
reviewed_land <- modifications %>% filter(application_scope == "land_area") %>%
  transmute(project_id = source_project_id, reported_land_sqft, land_sqft)
stopifnot(!anyDuplicated(reviewed_land$project_id),
  all(reviewed_land$project_id %in% residential_candidates$project_id))
i <- match(reviewed_land$project_id, residential_candidates$project_id)
stopifnot(all(residential_candidates$land_sqft[i] == reviewed_land$reported_land_sqft),
  all(is.finite(reviewed_land$land_sqft) & reviewed_land$land_sqft > 0))
residential_candidates$land_sqft[i] <- reviewed_land$land_sqft
residential_candidates$land_source[i] <- paste0("reviewed_land_area:", reviewed_land$project_id)

# Recorded alterations and conversions are not new buildings. Apply these
# existing exclusions here instead of leaving them in the unfinished review branch.
source_decisions <- readr::read_csv("../input/residential_source_decisions.csv",
  col_types = readr::cols(.default = readr::col_character()))
eligibility_decisions <- readr::read_csv("../input/eligibility_manual_exceptions.csv",
  show_col_types = FALSE) %>% filter(manual_action %in% c("exclude", "exclude_unverified_construction")) %>%
  transmute(project_id, action = if_else(manual_action == "exclude", "exclude_not_ground_up", manual_action),
    decision_reason = reason)
overlap_decisions <- readr::read_csv("../input/residential_overlap_decisions.csv", show_col_types = FALSE)
overlap_exclusions <- overlap_decisions %>% filter(overlap_action == "exclude_not_new_construction") %>%
  transmute(project_id = source_project_id, action = "exclude_not_ground_up", decision_reason)
not_new <- bind_rows(source_decisions %>% transmute(project_id = source_project_id, action, decision_reason),
  eligibility_decisions, overlap_exclusions) %>%
  filter(action %in% c("exclude_not_ground_up", "exclude_unverified_construction", "exclude_unbuilt", "exclude_recorded_review"))
stopifnot(!anyDuplicated(not_new$project_id))
reviewed_exclusion <- match(residential_candidates$project_id, not_new$project_id)
apply_exclusion <- residential_candidates$candidate_status %in% c("review_required", "retain_mechanical", "defer_to_commercial_reconciliation") &
  !is.na(reviewed_exclusion)
residential_candidates$candidate_status[apply_exclusion] <- if_else(
  not_new$action[reviewed_exclusion[apply_exclusion]] == "exclude_not_ground_up",
  "exclude_not_new_construction", not_new$action[reviewed_exclusion[apply_exclusion]])
residential_candidates$decision_reason[apply_exclusion] <-
  not_new$decision_reason[reviewed_exclusion[apply_exclusion]]

# A one-square-foot area is a source placeholder, not a measured building or lot.
placeholder <- with(residential_candidates,
  candidate_status == "retain_mechanical" &
    ((!is.na(building_sqft) & building_sqft <= 1) |
     (!is.na(land_sqft) & land_sqft <= 1)))
residential_candidates$candidate_status[placeholder] <- "exclude_unusable_measurement"
residential_candidates$decision_reason[placeholder] <- "source_area_placeholder_not_usable_density_measurement"

# Apply already recorded complete building measurements at the project-selection
# stage. Shared final IDs produce one building, with the old sources suppressed.
recorded_buildings <- modifications %>% filter(application_scope != "land_area")
stopifnot(!anyDuplicated(recorded_buildings[c("source_project_id", "final_project_id")]),
  all(recorded_buildings$application_scope %in% c("unresolved_building", "selected_building", "selected_fields")),
  all(recorded_buildings$source_project_id %in% residential_candidates$project_id),
  all(!is.na(recorded_buildings$evidence_ids) & nzchar(recorded_buildings$evidence_ids)))
recorded_buildings <- recorded_buildings %>% filter(application_scope %in% c("selected_building", "selected_fields") |
  source_project_id %in% residential_candidates$project_id[residential_candidates$candidate_status == "review_required"])
# A source may split into several buildings, but may not receive competing corrections.
stopifnot(all(recorded_buildings %>% count(source_project_id, final_project_id) %>% pull(n) == 1L))
recorded_components <- readr::read_csv("../input/residential_class297_component_overrides.csv",
  col_types = readr::cols(.default = readr::col_character()))
stopifnot(!anyDuplicated(recorded_components$final_project_id))
completed_condo_pins <- readr::read_csv("../input/construction_condominium_history.csv",
  col_types = readr::cols(pin = readr::col_character(), pin10 = readr::col_character(),
    .default = readr::col_skip())) %>% distinct(pin, pin10)
split_sources <- recorded_buildings %>% count(source_project_id) %>% filter(n > 1)
stopifnot(all(recorded_buildings$final_project_id[recorded_buildings$source_project_id %in%
  split_sources$source_project_id] %in% recorded_components$final_project_id))
# Sparse field corrections preserve the rest of the chosen assessment. Each
# nonblank field has one owner; a reviewed year can coexist with a unit correction.
field_corrections <- recorded_buildings %>% filter(application_scope == "selected_fields")
for (j in seq_len(nrow(field_corrections))) {
  decision <- field_corrections[j, ]
  stopifnot(decision$source_project_id == decision$final_project_id)
  i <- match(decision$source_project_id, residential_candidates$project_id)
  stopifnot(!is.na(i), residential_candidates$candidate_status[i] == "retain_mechanical")
  for (field in c("construction_year", "dwelling_units", "building_sqft", "land_sqft")) {
    if (is.na(decision[[field]])) next
    source_field <- c(construction_year = "year_source", dwelling_units = "units_source",
      building_sqft = "building_source", land_sqft = "land_source")[[field]]
    stopifnot(!grepl("^(reviewed_|recorded_project_decision:)", residential_candidates[[source_field]][i]))
    residential_candidates[[field]][i] <- decision[[field]]
    residential_candidates[[source_field]][i] <- paste0("recorded_project_decision:", decision$decision_source, ":", decision$final_project_id)
  }
  residential_candidates$decision_reason[i] <- decision$decision_reason
}
for (id in unique(recorded_buildings$final_project_id[recorded_buildings$application_scope != "selected_fields"])) {
  decisions <- recorded_buildings %>% filter(final_project_id == id)
  fields <- c("construction_year", "dwelling_units", "building_sqft", "land_sqft", "allow_far", "allow_dupac")
  stopifnot(all(vapply(decisions[fields], dplyr::n_distinct, integer(1)) == 1L),
    between(decisions$construction_year[1], 2006L, 2022L),
    is.finite(decisions$dwelling_units[1]), decisions$dwelling_units[1] > 0,
    is.finite(decisions$land_sqft[1]), decisions$land_sqft[1] > 1,
    !decisions$allow_far[1] || (is.finite(decisions$building_sqft[1]) && decisions$building_sqft[1] > 1))
  old <- match(decisions$source_project_id, residential_candidates$project_id)
  stopifnot(!anyNA(old))
  # Reviewed years/measurements already applied during identity construction
  # cannot be overwritten by another building correction.
  previous_sources <- unlist(residential_candidates[old,
    c("year_source", "units_source", "building_source", "land_source")], use.names = FALSE)
  stopifnot(!any(grepl("^(reviewed_|recorded_project_decision:)", previous_sources)))
  row <- residential_candidates[old[1], ]
  row$project_id <- id
  pins <- sort(unique(unlist(strsplit(residential_candidates$component_pins[old], "/", fixed = TRUE))))
  component_decision <- match(id, recorded_components$final_project_id)
  if (!is.na(component_decision)) {
    reviewed_pins <- strsplit(recorded_components$component_pins[component_decision], "/", fixed = TRUE)[[1]]
    # A reviewed completed condo parcel can replace its former development PINs.
    # Require both the recorded final building identity and actual source records.
    completed_pins <- completed_condo_pins$pin[
      paste0("residential_condo_", completed_condo_pins$pin10) == id]
    # A reviewed permit may also identify a separate accessory land parcel.
    permit_pins <- character()
    supporting_permit <- recorded_components$supporting_permit_number[component_decision]
    if (!is.na(supporting_permit)) {
      stopifnot(grepl("^[0-9]+$", supporting_permit),
        any(permit_links$project_id %in% decisions$source_project_id &
          permit_links$permit_number == supporting_permit &
          permit_links$directly_matched & permit_links$direct_match_method == "exact_pin"))
      permit_record <- sf::st_read("../output/building_permits_for_verification.gpkg",
        query = paste0("SELECT pin FROM building_permits_clean WHERE permit = '",
          supporting_permit, "'"), quiet = TRUE)
      stopifnot(nrow(permit_record) == 1L)
      permit_pins <- paste0(trimws(strsplit(permit_record$pin, "|", fixed = TRUE)[[1]]), "0000")
    }
    stopifnot(all(reviewed_pins %in% c(pins, permit_pins)) || all(reviewed_pins %in% completed_pins))
    pins <- reviewed_pins
  }
  row$component_pins <- paste(pins, collapse = "/")
  row$component_count <- length(pins)
  row$project_kind <- "reviewed_multi_parcel_building"
  for (field in c("construction_year", "dwelling_units", "building_sqft", "land_sqft"))
    row[[field]] <- decisions[[field]][1]
  for (field in c("year_source", "units_source", "building_source", "land_source"))
    row[[field]] <- paste0("recorded_project_decision:", decisions$decision_source[1], ":", id)
  row$candidate_status <- "retain_mechanical"
  row$decision_reason <- paste(sort(unique(decisions$decision_reason)), collapse = " | ")
  row$replacement_project_ids <- NA_character_
  row$replacement_check <- NA_character_
  if (id %in% decisions$source_project_id) {
    stopifnot(length(old) == 1L)
    residential_candidates[old, ] <- row
  } else {
    stopifnot(!id %in% residential_candidates$project_id)
    residential_candidates$candidate_status[old] <- "exclude_replaced_by_recorded_project"
    for (j in old) residential_candidates$replacement_project_ids[j] <- paste(
      recorded_buildings$final_project_id[recorded_buildings$source_project_id == residential_candidates$project_id[j]],
      collapse = "/")
    residential_candidates$replacement_check[old] <- "recorded_complete_building_decision"
    residential_candidates <- bind_rows(residential_candidates, row)
  }
}

# Suppress a condo predecessor only when its recorded replacement measurements
# agree with the commercial decision supplying the complete project.
commercial_replacements <- source_decisions %>% filter(action == "replace_by_commercial")
stopifnot(!anyDuplicated(commercial_replacements$source_project_id),
  !anyNA(commercial_replacements$final_project_id))
for (i in seq_len(nrow(commercial_replacements))) {
  decision <- commercial_replacements[i, ]
  old <- match(decision$source_project_id, residential_candidates$project_id)
  stopifnot(!is.na(old))
  residential_candidates$candidate_status[old] <- "exclude_replaced_by_recorded_project"
  residential_candidates$replacement_project_ids[old] <- decision$final_project_id
  residential_candidates$replacement_check[old] <- "recorded_complete_commercial_project"
  residential_candidates$decision_reason[old] <- decision$decision_reason
}

# Carry forward a recorded duplicate decision only when the retained successor
# still has the same unit count and building area. Conflicting decisions stay open.
overlap_decisions <- overlap_decisions %>% filter(overlap_action == "replace_by_residential_successor")
stopifnot(!anyDuplicated(overlap_decisions$source_project_id))
for (i in seq_len(nrow(overlap_decisions))) {
  old <- match(overlap_decisions$source_project_id[i], residential_candidates$project_id)
  successor <- match(overlap_decisions$replacement_project_id[i], residential_candidates$project_id)
  if (is.na(old) || is.na(successor)) next
  if (residential_candidates$candidate_status[old] != "review_required") next
  same_building <- residential_candidates$candidate_status[successor] == "retain_mechanical" &&
    isTRUE(residential_candidates$building_sqft[old] == residential_candidates$building_sqft[successor]) &&
    isTRUE(residential_candidates$dwelling_units[old] == residential_candidates$dwelling_units[successor])
  if (same_building) {
    residential_candidates$candidate_status[old] <- "exclude_source_duplicate_keep_successors"
    residential_candidates$decision_reason[old] <- overlap_decisions$decision_reason[i]
    residential_candidates$replacement_project_ids[old] <- overlap_decisions$replacement_project_id[i]
    residential_candidates$replacement_check[old] <- "recorded_identity_and_matching_units_and_building_area"
  }
}

# Recorded identity reviews can retire an old development record even when its
# obsolete cards do not reproduce the completed homes' measurements.
source_dispositions <- readr::read_csv("../input/residential_unresolved_source_dispositions.csv",
  col_types = readr::cols(.default = readr::col_character()))
reviewed_duplicates <- source_dispositions %>% filter(disposition == "exclude_source_duplicate_keep_successors", !is.na(final_project_ids))
stopifnot(!anyDuplicated(reviewed_duplicates$source_project_id))
for (j in seq_len(nrow(reviewed_duplicates))) {
  old <- match(reviewed_duplicates$source_project_id[j], residential_candidates$project_id)
  ids <- strsplit(reviewed_duplicates$final_project_ids[j], "/", fixed = TRUE)[[1]]
  successors <- match(ids, residential_candidates$project_id)
  stopifnot(!is.na(old), !anyNA(successors), !anyDuplicated(ids), !old %in% successors,
    all(residential_candidates$candidate_status[successors] == "retain_mechanical"))
  residential_candidates$candidate_status[old] <- "exclude_source_duplicate_keep_successors"
  residential_candidates$replacement_project_ids[old] <- paste(sort(ids), collapse = "/")
  residential_candidates$replacement_check[old] <- "recorded_completed_home_identity_review"
  residential_candidates$decision_reason[old] <- reviewed_duplicates$decision_reason[j]
}

if (anyDuplicated(residential_candidates$project_id) > 0) {
  stop("Preferred residential candidate IDs are not unique.", call. = FALSE)
}

residential_candidates <- residential_candidates %>% mutate(
  allow_dupac = candidate_status == "retain_mechanical" &
    is.finite(dwelling_units) & dwelling_units > 0 & is.finite(land_sqft) & land_sqft > 1,
  allow_far = allow_dupac & is.finite(building_sqft) & building_sqft > 1)

# Preserve outcome-specific restrictions in a reviewed building decision.
reviewed_flags <- recorded_buildings %>% select(final_project_id, allow_far, allow_dupac) %>% distinct()
stopifnot(!anyDuplicated(reviewed_flags$final_project_id))
flag <- match(residential_candidates$project_id, reviewed_flags$final_project_id)
residential_candidates$allow_far <- residential_candidates$allow_far & coalesce(reviewed_flags$allow_far[flag], TRUE)
residential_candidates$allow_dupac <- residential_candidates$allow_dupac & coalesce(reviewed_flags$allow_dupac[flag], TRUE)

# A recorded shared-site problem can withhold density without deleting buildings.
density_holds <- bind_rows(
  source_dispositions %>% filter(disposition == "withhold_density") %>% select(source_project_id, decision_reason),
  source_decisions %>% filter(action == "withhold_density") %>% select(source_project_id, decision_reason))
stopifnot(!anyDuplicated(density_holds$source_project_id),
  all(density_holds$source_project_id %in% residential_candidates$project_id))
hold <- match(density_holds$source_project_id, residential_candidates$project_id)
residential_candidates$allow_far[hold] <- FALSE
residential_candidates$allow_dupac[hold] <- FALSE
residential_candidates$decision_reason[hold] <- density_holds$decision_reason
# An approved measurement exclusion closes an unresolved source without inventing
# a complete building or allowing a second source to retain it for density.
withheld_unresolved <- hold[residential_candidates$candidate_status[hold] %in%
  c("review_required", "defer_to_commercial_reconciliation")]
residential_candidates$candidate_status[withheld_unresolved] <- "exclude_density_unresolved"


# Superseded combined records remain in the candidate ledger, not in building membership.
component_rows <- residential_candidates %>%
  filter(decision_reason != "source_replaced_by_reviewed_assessor_buildings",
    !candidate_status %in% c("exclude_replaced_by_recorded_project", "exclude_source_duplicate_keep_successors")) %>%
  select(project_id, source_family, project_kind, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  rename(component_pin = component_pins) %>%
  distinct(project_id, component_pin, .keep_all = TRUE) %>%
  arrange(project_id, component_pin)

component_conflicts <- component_rows %>%
  group_by(component_pin) %>%
  summarise(projects = n_distinct(project_id), .groups = "drop") %>%
  filter(projects > 1)

if (nrow(component_conflicts) > 0) {
  stop(
    paste0(
      "Residential component PINs belong to multiple candidate projects: ",
      paste(head(component_conflicts$component_pin, 10), collapse = ", ")
    ),
    call. = FALSE
  )
}

adjudication_queue <- residential_candidates %>%
  filter(
    candidate_status %in% c(
      "review_required",
      "defer_to_commercial_reconciliation"
    ),
    is.na(construction_year) | between(construction_year, 2006L, 2022L)
  ) %>%
  select(
    project_id,
    project_kind,
    component_pins,
    component_count,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    class_values,
    source_row_ids,
    permit_chain_ids,
    permit_numbers,
    year_source,
    units_source,
    building_source,
    land_source,
    current_distance_m,
    current_within_1500ft,
    candidate_status,
    decision_reason
  )

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(adjudication_queue), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Residential adjudication queue contains a prohibited analysis field.", call. = FALSE)
}


SaveData(residential_candidates, c("project_id"), "../output/preferred_residential_project_candidates.csv")
SaveData(component_rows, c("project_id", "component_pin"), "../output/preferred_residential_project_components.csv")
SaveData(adjudication_queue, c("project_id"), "../output/residential_adjudication_queue.csv")
