# setwd("tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
candidates <- readr::read_csv("../output/preferred_residential_project_candidates.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    class_values = readr::col_character(), .default = readr::col_guess()))
decisions <- readr::read_csv("../input/residential_overlap_decisions.csv",
  col_types = readr::cols(.default = readr::col_character()))
commercial <- readr::read_csv("../output/preferred_commercial_projects.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    source_row_ids = readr::col_character(), .default = readr::col_guess()))
commercial_sources <- readr::read_csv("../output/preferred_commercial_project_candidates.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    .default = readr::col_skip())) %>% left_join(
    readr::read_csv("../output/preferred_commercial_source_disposition.csv",
      col_types = readr::cols(source_project_id = readr::col_character(), disposition = readr::col_character(),
        .default = readr::col_skip())), by = c("project_id" = "source_project_id"), relationship = "one-to-one")
stopifnot(!anyDuplicated(candidates$project_id), !anyDuplicated(decisions$source_project_id),
  !anyDuplicated(commercial$project_id), nrow(readr::problems(decisions)) == 0)

resolution <- candidates %>% filter(candidate_status == "defer_to_commercial_reconciliation") %>%
  transmute(source_project_id = project_id, project_kind, component_pins, construction_year,
    dwelling_units, building_sqft, land_sqft) %>%
  left_join(decisions, by = "source_project_id", relationship = "one-to-one") %>%
  mutate(overlap_action = coalesce(overlap_action, "unresolved"),
    decision_reason = coalesce(decision_reason, "Residential and commercial records require a building-level comparison."))

# A reviewed replacement must cover every parcel in the residential source.
# The automatic replacement above requires identical parcel sets; a recorded
# decision may retire a partial residential copy of a larger measured building.
for (i in seq_len(nrow(resolution))) {
  row <- resolution[i, ]
  pins <- sort(str_split(row$component_pins, "/")[[1]])
  if (row$overlap_action == "unresolved") {
    same_site <- commercial %>% filter(component_pins == row$component_pins,
      construction_year == row$construction_year, is.finite(building_sqft), building_sqft > 1,
      is.finite(dwelling_units), dwelling_units > 0)
    if (nrow(same_site) == 1 && row$project_kind == "residential_commercial_overlap" &&
        is.finite(row$building_sqft) && row$building_sqft <= 1) {
      resolution$overlap_action[i] <- "replace_by_commercial"
      resolution$replacement_project_id[i] <- same_site$project_id
      resolution$decision_reason[i] <- "The residential source lacks building area; the retained commercial building has the identical parcel set and construction year with usable measurements."
      resolution$evidence_ids[i] <- same_site$project_id
    } else {
      related <- commercial_sources %>% filter(vapply(str_split(component_pins, "/"),
        function(x) any(x %in% pins), logical(1)))
      if (nrow(related) > 0 && all(!is.na(related$disposition) & related$disposition == "excluded_replaced_by_residential")) {
        resolution$overlap_action[i] <- "retain_residential_resolution"
        resolution$decision_reason[i] <- "The recorded commercial decision explicitly defers to this residential building; retain its complete Assessor measurements."
        resolution$evidence_ids[i] <- paste(sort(related$project_id), collapse = "/")
      }
    }
    row <- resolution[i, ]
  }
  if (row$overlap_action == "replace_by_commercial") {
    replacement <- commercial %>% filter(project_id == row$replacement_project_id)
    compatible <- nrow(replacement) == 1 &&
      all(pins %in% str_split(replacement$component_pins, "/")[[1]]) &&
      is.finite(replacement$construction_year) && is.finite(replacement$dwelling_units) &&
      replacement$dwelling_units > 0
    if (!compatible) {
      resolution$overlap_action[i] <- "unresolved"
      resolution$decision_reason[i] <- "The recorded commercial replacement no longer covers every residential source parcel with a usable dwelling count."
    }
  } else if (row$overlap_action == "retain_residential_resolution") {
    other <- commercial %>% filter(vapply(str_split(component_pins, "/"),
      function(x) any(x %in% pins), logical(1)))
    compatible <- nrow(other) == 0 && all(is.finite(c(row$construction_year,
      row$dwelling_units, row$building_sqft, row$land_sqft))) &&
      row$dwelling_units > 0 && row$building_sqft > 1 && row$land_sqft > 1
    if (!compatible) {
      resolution$overlap_action[i] <- "unresolved"
      resolution$decision_reason[i] <- "The recorded residential retention has incomplete measurements or still overlaps a selected commercial project."
    }
  } else {
    stopifnot(row$overlap_action == "unresolved")
  }
}
stopifnot(!anyDuplicated(resolution$source_project_id),
  setequal(resolution$source_project_id,
    candidates$project_id[candidates$candidate_status == "defer_to_commercial_reconciliation"]))
SaveData(arrange(resolution, source_project_id), c("source_project_id"), "../output/residential_overlap_resolution.csv")
