# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/prepare_new_construction/code")
# first_construction_year <- 2006
# last_construction_year <- 2022
# preferred_assessment_year <- 2022
# fallback_assessment_year <- 2025
# episode_year_window <- 2
# maximum_building_gap <- 0.02
# successor_point_tolerance_ft <- 1
# successor_land_tolerance <- 0.005
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/assessor_classification.R")
source("../../shared/code/normalize_chicago_address.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(first_construction_year, last_construction_year, preferred_assessment_year,
  fallback_assessment_year, episode_year_window, maximum_building_gap, successor_point_tolerance_ft, successor_land_tolerance)
stopifnot(length(args) == 8L)
first_construction_year <- as.integer(args[1])
last_construction_year <- as.integer(args[2])
preferred_assessment_year <- as.integer(args[3])
fallback_assessment_year <- as.integer(args[4])
episode_year_window <- as.integer(args[5])
maximum_building_gap <- as.numeric(args[6])
successor_point_tolerance_ft <- as.numeric(args[7])
successor_land_tolerance <- as.numeric(args[8])
stopifnot(first_construction_year <= last_construction_year)

buildings <- read_csv("../output/assessor_buildings.csv", col_types = cols(project_id = "c",
  component_pins = "c", source_row_ids = "c", class_values = "c", earlier_rows = "c", earlier_pins = "c"))
records <- read_csv("../output/assessor_measurement_records.csv", col_types = cols(source_row_id = "c",
  pin = "c", class = "c", component_pins = "c"))
changes <- read_csv("../input/recorded_building_changes.csv", col_types = cols(
  construction_year = "d", dwelling_units = "d", building_sqft = "d", land_sqft = "d",
  allow_far = "l", allow_dupac = "l", multifamily = "l", defer_to_residential = "l", reported_construction_year = "d",
  units_unusable = "l", building_area_unusable = "l", land_area_unusable = "l",
  reported_land_sqft = "d", location_year = "i", location_target_year = "i", historical_location_year = "i",
  zoning_year = "i", .default = "c"))
stopifnot(!anyDuplicated(buildings$project_id), !anyDuplicated(changes$project_id),
  !anyDuplicated(records[c("source_family", "source_row_id")]),
  all(changes$action %in% c("update", "replace", "exclude")))
selected_assessments <- buildings

# Apply the recorded decisions here. First read measurements for the source
# rows explicitly selected by a review; the correction file stores their IDs.
for (i in which(!is.na(changes$assessment_aggregation))) {
  row_ids <- strsplit(changes$assessment_rows[i], "/", fixed = TRUE)[[1]]
  chosen <- records |> filter(source_family == changes$source_family[i], source_row_id %in% row_ids)
  stopifnot(nrow(chosen) == length(unique(row_ids)))
  if (changes$assessment_aggregation[i] %in% "sum_buildings_distinct_land") {
    stopifnot(n_distinct(chosen$tax_year) == 1,
      all(chosen |> group_by(pin) |> summarise(n = n_distinct(land_sqft), .groups = "drop") |> pull(n) == 1))
    values <- c(construction_year = if (n_distinct(chosen$construction_year) == 1) chosen$construction_year[1] else NA_real_,
      dwelling_units = sum(chosen$dwelling_units), building_sqft = sum(chosen$building_sqft),
      land_sqft = sum(chosen |> distinct(pin, land_sqft) |> pull(land_sqft)))
  } else if (changes$assessment_aggregation[i] %in% "one_source_row") {
    stopifnot(nrow(chosen) == 1)
    values <- unlist(chosen[1, c("construction_year", "dwelling_units", "building_sqft", "land_sqft")])
  } else {
    next
  }
  for (field in names(values)) {
    if (is.na(changes[[field]][i])) changes[[field]][i] <- values[[field]]
  }
  if (is.na(changes$component_pins[i]))
    changes$component_pins[i] <- paste(sort(unique(unlist(strsplit(chosen$component_pins, "/", fixed = TRUE)))), collapse = "/")
}

# A replacement starts from its source building, then receives the recorded
# identity and values. One parent can be replaced by several individual homes.
replacements <- changes |> filter(action == "replace")
source_ids <- str_split(replacements$source_project_ids, "/")
source_index <- vapply(source_ids, function(ids) {
  matches <- match(ids, buildings$project_id)
  if (all(is.na(matches))) NA_integer_ else matches[which(!is.na(matches))[1]]
}, integer(1))
new_rows <- buildings[source_index, ]
new_rows$project_id <- replacements$project_id
new_rows$source_family <- coalesce(replacements$source_family, new_rows$source_family)
new_rows$project_kind <- coalesce(new_rows$project_kind, "recorded_building")
for (i in seq_len(nrow(new_rows))) {
  old <- buildings |> filter(project_id %in% source_ids[[i]])
  if (is.na(replacements$component_pins[i]) && nrow(old) > 0)
    new_rows$component_pins[i] <- paste(sort(unique(unlist(strsplit(old$component_pins, "/", fixed = TRUE)))), collapse = "/")
}
removed <- unique(c(unlist(source_ids), replacements$project_id))
buildings <- bind_rows(buildings |> filter(!project_id %in% removed), new_rows) |>
  mutate(included = !project_id %in% changes$project_id[changes$action == "exclude"])
stopifnot(!anyDuplicated(buildings$project_id))

# The recorded instruction to use reported land selects an Assessor measurement,
# never a map area. Earlier component lots must cover exactly the same property.
for (i in which(changes$land_selection %in% "reported_same_property")) {
  current <- buildings |> filter(project_id == changes$project_id[i])
  stopifnot(nrow(current) == 1)
  pins <- strsplit(coalesce(changes$component_pins[i], current$component_pins), "/", fixed = TRUE)[[1]]
  candidate <- selected_assessments |> filter(project_id == changes$project_id[i])
  earlier <- records |> filter(source_family == "commercial", source_project_id == changes$project_id[i], tax_year == 2021)
  old_pins <- unlist(strsplit(earlier$component_pins, "/", fixed = TRUE))
  same_candidate <- nrow(candidate) == 1 && setequal(strsplit(candidate$component_pins, "/", fixed = TRUE)[[1]], pins)
  complete_earlier <- nrow(earlier) > 0 && !anyDuplicated(old_pins) && setequal(old_pins, pins) &&
    all(is.finite(earlier$land_sqft) & earlier$land_sqft > 1)
  if (!is.na(changes$land_sqft[i])) next
  if (same_candidate && is.finite(candidate$land_sqft) && candidate$land_sqft > 1) {
    changes$land_sqft[i] <- candidate$land_sqft
  } else if (complete_earlier) {
    changes$land_sqft[i] <- sum(earlier$land_sqft)
    if (nrow(earlier) > 1) {
      units <- coalesce(changes$dwelling_units[i], current$dwelling_units)
      floor <- coalesce(changes$building_sqft[i], current$building_sqft)
      if (all(is.finite(earlier$building_sqft) & earlier$building_sqft > 1) &&
        all(is.finite(earlier$dwelling_units)) && isTRUE(sum(earlier$dwelling_units) == units)) {
        changes$building_sqft[i] <- sum(earlier$building_sqft)
      } else if (!isTRUE(sum(earlier$building_sqft) == floor)) {
        changes$allow_far[i] <- FALSE
      }
    }
  } else {
    changes$land_area_unusable[i] <- TRUE
    changes$allow_far[i] <- FALSE
    changes$allow_dupac[i] <- FALSE
  }
}


# Resolve only measurements explicitly selected during the recorded reviews.
# Ordinary unreviewed buildings are never written to this decision input.
changes <- changes |> left_join(new_rows |> select(project_id, selected_component_pins = component_pins),
  by = "project_id", relationship = "one-to-one") |>
  mutate(component_pins = coalesce(component_pins, selected_component_pins)) |>
  select(-selected_component_pins, -assessment_aggregation, -land_selection)
write_csv(changes, "/tmp/construction-rewrite-20260912/recorded_building_changes_resolved.csv")
