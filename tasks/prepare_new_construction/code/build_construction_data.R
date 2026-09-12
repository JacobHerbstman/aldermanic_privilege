# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/prepare_new_construction/code")
# first_construction_year <- 2006
# last_construction_year <- 2022
# preferred_assessment_year <- 2022
# fallback_assessment_year <- 2025
# episode_year_window <- 2
# maximum_building_gap <- 0.02
# successor_point_tolerance_ft <- 1
# successor_land_tolerance <- 0.005
# location_rounding_ft <- 0.001
# boundary_window_ft <- 1500
# main_boundary_window_ft <- 500
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/assessor_classification.R")
source("../../shared/code/normalize_chicago_address.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(first_construction_year, last_construction_year, preferred_assessment_year,
  fallback_assessment_year, episode_year_window, maximum_building_gap, successor_point_tolerance_ft, successor_land_tolerance,
  location_rounding_ft, boundary_window_ft, main_boundary_window_ft)
stopifnot(length(args) == 11L)
first_construction_year <- as.integer(args[1])
last_construction_year <- as.integer(args[2])
preferred_assessment_year <- as.integer(args[3])
fallback_assessment_year <- as.integer(args[4])
episode_year_window <- as.integer(args[5])
maximum_building_gap <- as.numeric(args[6])
successor_point_tolerance_ft <- as.numeric(args[7])
successor_land_tolerance <- as.numeric(args[8])
location_rounding_ft <- as.numeric(args[9])
boundary_window_ft <- as.numeric(args[10])
main_boundary_window_ft <- as.numeric(args[11])
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
buildings <- buildings |> mutate(assessor_construction_year = construction_year,
  assessor_component_pins = component_pins)
# Apply the recorded corrections once, including changes to building identity.
# A replacement starts from its original source; unmodified fields are retained.
replacements <- changes |> filter(action == "replace")
source_ids <- str_split(replacements$source_project_ids, "/")
source_index <- match(replacements$project_id, buildings$project_id)
for (i in which(is.na(source_index))) {
  matches <- na.omit(match(source_ids[[i]], buildings$project_id))
  if (length(matches)) source_index[i] <- matches[1]
}
new_rows <- buildings[source_index, ]
new_rows$project_id <- replacements$project_id
new_rows$source_family <- coalesce(replacements$source_family, new_rows$source_family)
new_rows$project_kind <- coalesce(new_rows$project_kind, "recorded_building")
removed <- unique(c(unlist(source_ids), replacements$project_id))
buildings <- bind_rows(buildings |> filter(!project_id %in% removed), new_rows) |>
  mutate(included = !project_id %in% changes$project_id[changes$action == "exclude"])
stopifnot(!anyDuplicated(buildings$project_id))

# Corrected and uncorrected buildings now use the same columns and calculations.
corrections <- changes |>
  select(project_id, source_project_ids, defer_to_residential, component_pins, construction_year, dwelling_units, building_sqft, land_sqft,
    allow_far, allow_dupac, multifamily, units_unusable, building_area_unusable, land_area_unusable,
    reported_construction_year, evidence_ids, location_source, location_id, location_year, location_target_year,
    corrected_address, historical_location_pin, historical_location_year, zoning_group, zoning_year, zoning_source, zoning_note,
    decision_references, decision_reason) |>
  rename_with(~ paste0("recorded_", .x), -project_id)
buildings <- buildings |> left_join(corrections, by = "project_id", relationship = "one-to-one") |>
  mutate(source_project_ids = coalesce(recorded_source_project_ids, project_id),
    defer_to_residential = recorded_defer_to_residential %in% TRUE,
    component_pins = coalesce(recorded_component_pins, component_pins),
    construction_year = coalesce(recorded_construction_year, construction_year),
    dwelling_units = if_else(recorded_units_unusable %in% TRUE, NA_real_, coalesce(recorded_dwelling_units, dwelling_units)),
    building_sqft = if_else(recorded_building_area_unusable %in% TRUE, NA_real_, coalesce(recorded_building_sqft, building_sqft)),
    land_sqft = if_else(recorded_land_area_unusable %in% TRUE, NA_real_, coalesce(recorded_land_sqft, land_sqft)),
    allow_far = coalesce(recorded_allow_far, source_allows_far, TRUE), allow_dupac = coalesce(recorded_allow_dupac, TRUE),
    multifamily = recorded_multifamily,
    reviewed_measurements = recorded_allow_far %in% TRUE | recorded_allow_dupac %in% TRUE,
    reported_construction_year = recorded_reported_construction_year, evidence_ids = recorded_evidence_ids,
    location_source = recorded_location_source, location_id = recorded_location_id,
    location_year = recorded_location_year, location_target_year = recorded_location_target_year,
    corrected_address = recorded_corrected_address, historical_location_pin = recorded_historical_location_pin,
    historical_location_year = recorded_historical_location_year,
    zoning_group = recorded_zoning_group, zoning_year = recorded_zoning_year,
    zoning_source = recorded_zoning_source, zoning_note = recorded_zoning_note,
    decision_references = recorded_decision_references,
    decision_reason = recorded_decision_reason) |> select(-starts_with("recorded_"))

# Remove old property numbers only when a unique current home has the same
# address and lot, and its construction predates the old assessment.
current_parcels <- read_csv("../input/parcel_universe_2025_city.csv", col_types = cols(
  pin = "c", pin10 = "c", longitude = "d", latitude = "d", class = "c", .default = col_skip()))
current_addresses <- read_csv("../input/parcel_addresses_2025_chicago.csv", col_types = cols(
  pin = "c", prop_address_full = "c", .default = col_skip()))
historical_addresses <- read_csv("../input/density_historical_address_records.csv", col_types = cols(
  pin = "c", year = "i", property_address = "c", .default = col_skip()))
historical_points <- bind_rows(
  read_csv("../input/geocoding_parcel_history.csv", col_types = cols(pin = "c", year = "i",
    lon = "d", lat = "d", row_id = "c", .default = col_skip())),
  read_csv("../input/predecessor_parcel_history.csv", col_types = cols(pin = "c", year = "i",
    lon = "d", lat = "d", row_id = "c", .default = col_skip())),
  read_csv("../input/density_historical_parcel_records.csv", col_types = cols(pin = "c", year = "i",
    longitude = "d", latitude = "d", row_id = "c", .default = col_skip())) |> rename(lon = longitude, lat = latitude)
) |> filter(is.finite(lon), is.finite(lat)) |> distinct()
stopifnot(!anyDuplicated(current_parcels$pin), !anyDuplicated(current_addresses$pin))
periods <- records |> filter(source_family == "residential", !is.na(building_sqft) | !is.na(dwelling_units)) |>
  group_by(pin) |> summarise(first_assessment = min(tax_year), last_assessment = max(tax_year), .groups = "drop")
homes <- buildings |> filter(included, project_kind == "single_pin_single_card", dwelling_units == 1,
  building_sqft > 1, land_sqft > 1, between(construction_year, first_construction_year, last_construction_year)) |>
  left_join(periods, by = c("component_pins" = "pin"), relationship = "one-to-one")
old_points <- historical_points |> inner_join(homes |> filter(!component_pins %in% current_parcels$pin) |>
    select(pin = component_pins, last_assessment), by = "pin", relationship = "many-to-one") |>
  filter(year <= last_assessment) |> distinct(pin, year, lon, lat) |> group_by(pin) |>
  filter(year == max(year)) |> filter(n() == 1) |> ungroup()
old_homes <- homes |> inner_join(old_points, by = c("component_pins" = "pin"), relationship = "one-to-one")
new_homes <- homes |> inner_join(current_parcels, by = c("component_pins" = "pin"), relationship = "one-to-one") |>
  filter(is.finite(longitude), is.finite(latitude))
old_geometry <- st_as_sf(old_homes, coords = c("lon", "lat"), crs = 4326) |> st_transform(3435)
new_geometry <- st_as_sf(new_homes, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
hits <- st_is_within_distance(old_geometry, new_geometry, dist = successor_point_tolerance_ft)
replacements <- tibble(old_project_id = character(), new_project_ids = character())
for (i in which(lengths(hits) == 1)) {
  old <- old_homes[i, ]; new <- new_homes[hits[[i]], ]
  addresses <- historical_addresses |> filter(pin == old$component_pins, year <= old$last_assessment)
  if (!nrow(addresses)) next
  addresses <- addresses |> filter(year == max(year))
  old_address <- unique(str_remove(str_to_upper(str_squish(addresses$property_address)), " [A-Z]$"))
  new_address <- current_addresses$prop_address_full[match(new$component_pins, current_addresses$pin)] |>
    str_squish() |> str_to_upper() |> str_remove(" [A-Z]$")
  same_home <- length(old_address) == 1 && !is.na(old_address) && !is.na(new_address) &&
    old_address != "" && old_address == new_address && new$first_assessment == old$last_assessment + 1 &&
    new$construction_year <= old$first_assessment && abs(new$land_sqft / old$land_sqft - 1) <= successor_land_tolerance
  if (isTRUE(same_home)) replacements <- add_row(replacements,
    old_project_id = old$project_id, new_project_ids = new$project_id)
}
replacements <- replacements |> add_count(new_project_ids) |> filter(n == 1) |> select(-n)
buildings <- buildings |> filter(!project_id %in% replacements$old_project_id)

# A former parcel containing several house cards is redundant when every home
# has a current individual record with the same year and floor area.
parcels <- bind_rows(
  st_read("../input/preferred_historical_parcel_source.gpkg", quiet = TRUE) |>
    transmute(pin = pin14, map_year = target_year, object_id),
  st_read("../input/preferred_predecessor_parcel_source.gpkg", quiet = TRUE) |>
    transmute(pin = predecessor_pin14, map_year = target_year, object_id)
) |> st_transform(3435)
stopifnot(all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
individuals <- buildings |> filter(project_kind == "single_pin_single_card") |>
  inner_join(current_parcels, by = c("component_pins" = "pin"), relationship = "one-to-one") |>
  filter(is.finite(longitude), is.finite(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
parents <- buildings |> filter(project_kind == "same_pin_multiple_cards", !component_pins %in% current_parcels$pin)
parent_replacements <- vector("list", nrow(parents))
for (i in seq_len(nrow(parents))) {
  parent <- parents[i, ]
  cards <- records |> filter(source_row_id %in% strsplit(parent$source_row_ids, "/", fixed = TRUE)[[1]])
  if (nrow(cards) < 2 || n_distinct(cards$tax_year) != 1 ||
      any(!cards$class %in% single_family_assessor_classes) ||
      any(!is.finite(cards$building_sqft) | cards$building_sqft <= 0)) next
  shapes <- parcels |> filter(pin == parent$component_pins, map_year <= max(cards$tax_year))
  if (!nrow(shapes)) next
  shapes <- shapes |> filter(map_year == max(map_year))
  if (!nrow(shapes) || !all(lengths(st_equals(shapes)) == nrow(shapes))) next
  children <- individuals[lengths(st_within(individuals, shapes[1, ])) == 1, ] |>
    filter(between(construction_year, min(cards$construction_year) - episode_year_window,
      max(cards$construction_year) + episode_year_window)) |> arrange(building_sqft, construction_year, project_id)
  cards <- cards |> arrange(building_sqft, construction_year, card_num)
  if (nrow(children) != nrow(cards) || any(!children$dwelling_units %in% 1) ||
      any(!children$included) || any(!between(children$construction_year, first_construction_year, last_construction_year)) ||
      any(!is.finite(children$building_sqft)) || any(!is.finite(children$land_sqft) | children$land_sqft <= 1)) next
  if (all(abs(children$building_sqft / cards$building_sqft - 1) <= maximum_building_gap) &&
      all(abs(children$construction_year - cards$construction_year) <= episode_year_window))
    parent_replacements[[i]] <- tibble(old_project_id = parent$project_id,
      new_project_ids = paste(sort(children$project_id), collapse = "/"))
}
parent_replacements <- bind_rows(parent_replacements)
used <- table(unlist(strsplit(parent_replacements$new_project_ids, "/", fixed = TRUE)))
parent_replacements <- parent_replacements |>
  filter(!vapply(strsplit(new_project_ids, "/", fixed = TRUE), function(ids) any(used[ids] > 1), logical(1)))
replacements <- bind_rows(replacements, parent_replacements)
buildings <- buildings |> filter(!project_id %in% replacements$old_project_id)

# Match completed condo buildings to the earlier property. The finished unit
# records must form one complete building, and its count must be confirmed.
condo_rows <- read_csv("../input/construction_condominium_history.csv", col_types = cols(
  pin = "c", pin10 = "c", row_id = "c", year = "d", char_yrblt = "d", char_building_sf = "d",
  char_land_sf = "d", char_building_pins = "d", char_building_non_units = "d",
  is_parking_space = "l", is_common_area = "l", .default = col_skip()))
stopifnot(!anyDuplicated(condo_rows[c("pin", "year")]))
completed <- condo_rows |> group_by(pin10, tax_year = year) |> summarise(
  dwelling_units = sum(!is_parking_space & !is_common_area, na.rm = TRUE),
  complete_membership = all(!is.na(is_parking_space) & !is.na(is_common_area)) &&
    all(!is.na(char_building_pins) & char_building_pins == n()) &&
    all(!is.na(char_building_non_units) & char_building_non_units == sum(is_parking_space | is_common_area)),
  year_values = n_distinct(char_yrblt[!is_parking_space & !is_common_area]),
  building_values = n_distinct(char_building_sf[!is_parking_space & !is_common_area]),
  land_values = n_distinct(char_land_sf[!is_parking_space & !is_common_area]),
  construction_year = first(char_yrblt[!is_parking_space & !is_common_area]),
  building_sqft = first(char_building_sf[!is_parking_space & !is_common_area]),
  land_sqft = first(char_land_sf[!is_parking_space & !is_common_area]),
  source_row_ids = paste(sort(row_id), collapse = "/"), .groups = "drop") |>
  mutate(complete_density = complete_membership & dwelling_units > 0 & year_values == 1 &
    is.finite(construction_year) & land_values == 1 & is.finite(land_sqft) & land_sqft > 1,
    complete_floor = complete_density & building_values == 1 & is.finite(building_sqft) & building_sqft > 1) |>
  filter(tax_year %in% c(preferred_assessment_year, fallback_assessment_year)) |>
  arrange(pin10, desc(complete_density), desc(complete_floor), desc(tax_year == preferred_assessment_year)) |>
  distinct(pin10, .keep_all = TRUE)
permit_records <- read_csv("../output/building_permit_evidence.csv", col_types = cols(
  permit_number = "c", permit_chain_id = "c", project_ids = "c", direct_project_ids = "c",
  permit_units = "d", .default = col_guess()))
parents <- buildings |> filter(included, source_family == "residential", is.na(decision_references),
  project_kind == "class_297" | str_detect(class_values, "297") |
    !is.finite(dwelling_units) | !is.finite(building_sqft) | !is.finite(land_sqft))
condo_points <- current_parcels |> filter(class == "299", is.finite(longitude), is.finite(latitude)) |>
  distinct(pin10, longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
links <- vector("list", nrow(parents))
for (i in seq_len(nrow(parents))) {
  parent <- parents[i, ]; pins <- strsplit(parent$component_pins, "/", fixed = TRUE)[[1]]
  shapes <- parcels |> filter(map_year == parent$construction_year, pin %in% pins)
  for (pin in setdiff(pins, shapes$pin)) {
    reference <- current_parcels |> filter(.data$pin == .env$pin, is.finite(longitude), is.finite(latitude))
    if (nrow(reference) != 1) next
    point <- st_as_sf(reference, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
    old <- parcels |> filter(map_year == parent$construction_year)
    hit <- st_within(point, old)[[1]]
    if (length(hit) == 1) shapes <- bind_rows(shapes, mutate(old[hit, ], pin = .env$pin))
  }
  if (!setequal(shapes$pin, pins)) next
  site <- st_union(shapes)
  found <- condo_points[lengths(st_intersects(condo_points, site)) > 0, ] |> st_drop_geometry() |>
    distinct(pin10)
  if (nrow(found)) links[[i]] <- mutate(found, source_project_id = parent$project_id)
}
links <- bind_rows(links) |> add_count(pin10, name = "parents_per_building") |>
  add_count(source_project_id, name = "buildings_per_parent")
condo_replacements <- vector("list", nrow(parents))
condo_retirements <- character()
for (i in seq_len(nrow(parents))) {
  parent <- parents[i, ]
  matched <- links |> filter(source_project_id == parent$project_id, parents_per_building == 1, buildings_per_parent == 1)
  if (nrow(matched) != 1) next
  finished <- completed |> filter(pin10 == matched$pin10)
  if (nrow(finished) != 1 || !finished$complete_membership || finished$dwelling_units <= 0 ||
      finished$year_values != 1 || !is.finite(finished$construction_year)) next
  source_ids <- c(parent$project_id, paste0("residential_", strsplit(parent$component_pins, "/", fixed = TRUE)[[1]]))
  evidence <- permit_records[vapply(strsplit(permit_records$project_ids, "/", fixed = TRUE),
    function(ids) any(ids %in% source_ids), logical(1)), ]
  full <- evidence |> filter(permit_status == "COMPLETE", permit_type == "PERMIT - NEW CONSTRUCTION",
    str_detect(work_description, regex("ERECT|NEW CONSTRUCTION|NEW [0-9]+ STORY", TRUE)),
    !str_detect(work_description, regex("REVISION|ALTERATION|CONVERT|EXISTING|FOUNDATION ONLY", TRUE)))
  permit_confirms <- n_distinct(evidence$permit_chain_id) == 1 && nrow(full) == 1 &&
    isTRUE(full$permit_units == finished$dwelling_units)
  needs_permit <- parent$project_kind == "tieback_building" && !str_detect(parent$class_values, "297")
  count_confirms <- (!needs_permit && isTRUE(parent$dwelling_units == finished$dwelling_units)) || permit_confirms
  outside <- !between(finished$construction_year, first_construction_year, last_construction_year)
  if (outside && (!needs_permit || count_confirms)) {
    condo_retirements <- c(condo_retirements, parent$project_id)
  } else if (count_confirms && finished$complete_density) {
    parent$project_id <- paste0("residential_condo_", finished$pin10)
    parent[c("construction_year", "dwelling_units", "building_sqft", "land_sqft", "source_row_ids", "tax_year")] <-
      finished[c("construction_year", "dwelling_units", "building_sqft", "land_sqft", "source_row_ids", "tax_year")]
    parent$building_sqft <- if (finished$complete_floor) finished$building_sqft else NA_real_
    parent$allow_far <- finished$complete_floor; parent$allow_dupac <- TRUE
    condo_replacements[[i]] <- parent
    condo_retirements <- c(condo_retirements, parents$project_id[i])
  }
}
buildings <- bind_rows(buildings |> filter(!project_id %in% condo_retirements), bind_rows(condo_replacements))
if (anyDuplicated(buildings$project_id)) stop("Competing completed-building identities: ",
  paste(unique(buildings$project_id[duplicated(buildings$project_id)]), collapse = ", "))

# When a reviewed commercial rollup defers to residential buildings, retain
# their complete residential measurements if no commercial copy remains.
commercial_components <- buildings |> filter(source_family == "commercial") |>
  select(project_id, component_pins, included, defer_to_residential) |>
  separate_longer_delim(component_pins, delim = "/")
for (i in which(buildings$project_kind == "residential_commercial_overlap")) {
  pins <- strsplit(buildings$component_pins[i], "/", fixed = TRUE)[[1]]
  related <- commercial_components |> filter(component_pins %in% pins)
  if (nrow(related) && all(related$defer_to_residential) && !any(related$included))
    buildings$reviewed_measurements[i] <- TRUE
}

# Every retained observation must describe a complete building. Reviewed
# outcome-specific exceptions can keep a building with one unusable density measure.
buildings <- buildings |> mutate(complete_assessment =
  is.finite(dwelling_units) & dwelling_units > 0 & is.finite(building_sqft) & building_sqft > 1 &
    is.finite(land_sqft) & land_sqft > 1,
  source_eligible = source_family == "commercial" | reviewed_measurements |
    str_starts(project_id, "residential_condo_") |
    (complete_assessment & project_kind != "residential_commercial_overlap" &
      !(project_kind == "tieback_building" & str_detect(coalesce(class_values, ""), "297")) &
      !(project_kind == "class_297" & (component_count > 1 | conflicting_permit_counts %in% TRUE))))

buildings <- buildings |> filter(included, source_eligible, between(construction_year, first_construction_year, last_construction_year)) |>
  mutate(allow_dupac = allow_dupac & is.finite(dwelling_units) & dwelling_units > 0 & is.finite(land_sqft) & land_sqft > 1,
    allow_far = allow_far & is.finite(building_sqft) & building_sqft > 1 & is.finite(land_sqft) & land_sqft > 1,
    far = if_else(allow_far, building_sqft / land_sqft, NA_real_),
    dupac = if_else(allow_dupac, dwelling_units * 43560 / land_sqft, NA_real_)) |>
  arrange(source_family, project_id)
# If a reviewed identity has no old project key, its component parcels still
# supply the Assessor class under the same assessment-year priority.
missing_classes <- buildings |> filter(is.na(class_values), source_family == "residential") |>
  select(project_id, component_pins, construction_year) |> separate_longer_delim(component_pins, delim = "/") |>
  inner_join(records |> filter(source_family == "residential", !is.na(class)),
    by = c("component_pins" = "pin", "construction_year"), relationship = "one-to-many") |>
  mutate(priority = case_when(tax_year <= preferred_assessment_year ~ 1L,
    tax_year <= fallback_assessment_year ~ 2L, TRUE ~ 3L)) |>
  arrange(project_id, component_pins, priority, desc(tax_year)) |> distinct(project_id, component_pins, .keep_all = TRUE) |>
  group_by(project_id) |> summarise(component_classes = paste(sort(unique(class)), collapse = "/"), .groups = "drop")
buildings <- buildings |> left_join(missing_classes, by = "project_id", relationship = "one-to-one") |>
  mutate(class_values = coalesce(class_values, component_classes)) |> select(-component_classes)

# A townhouse group remains single-family even when it contains several homes.
buildings <- buildings |> mutate(
  external_multifamily = case_when(
    dwelling_units <= 1 ~ FALSE,
    !is.na(multifamily) ~ multifamily,
    source_family == "commercial" ~ coalesce(dwelling_units > 1, FALSE),
    str_detect(coalesce(class_values, ""), "(^|/)(211|212|297)($|/)") ~ TRUE,
    str_detect(coalesce(class_values, ""), paste0("(^|/)(", paste(single_family_assessor_classes, collapse = "|"), ")($|/)")) ~ FALSE,
    class_values %in% c("EX", "OA2") ~ dwelling_units > 1,
    TRUE ~ NA),
  multifamily_source = if_else(!is.na(multifamily), "recorded_building_type", "selected_assessor_class_and_finished_home_count"))

# Locate the finished buildings using the construction-year parcel maps.
# Parcels establish location and identity; lot areas remain source measurements.
direct_parcels <- st_read("../input/preferred_historical_parcel_source.gpkg", quiet = TRUE) |>
  st_transform(3435) |> group_by(target_year, pin14) |>
  summarise(object_ids = paste(sort(unique(object_id)), collapse = "/"), .groups = "drop")
predecessors <- bind_rows(
  st_read("../input/preferred_predecessor_parcel_source.gpkg", quiet = TRUE) |>
    transmute(target_year, object_id, pin14 = predecessor_pin14),
  st_read("../input/history_reference_parcels.gpkg", quiet = TRUE) |>
    transmute(target_year, object_id, pin14 = predecessor_pin14)
) |> st_transform(3435) |> distinct(target_year, object_id, .keep_all = TRUE)
requests <- buildings |> mutate(unchanged_assessor_identity = construction_year == assessor_construction_year &
    component_pins == assessor_component_pins) |>
  select(project_id, source_family, project_kind, unchanged_assessor_identity, target_year = construction_year, component_pins) |>
  separate_longer_delim(component_pins, delim = "/") |> rename(component_pin = component_pins)
stopifnot(!anyDuplicated(requests[c("project_id", "component_pin")]))
matched <- match(paste(requests$target_year, requests$component_pin),
  paste(direct_parcels$target_year, direct_parcels$pin14))
base_keys <- paste(direct_parcels$target_year, substr(direct_parcels$pin14, 1, 10))
unique_bases <- !duplicated(base_keys) & !duplicated(base_keys, fromLast = TRUE)
base_match <- match(paste(requests$target_year, substr(requests$component_pin, 1, 10)), base_keys[unique_bases])
# A unique base PIN can recover the historical parcel for an unchanged identity.
# Reviewed commercial identities require their exact parcels or cited permits.
use_base <- is.na(matched) & (requests$source_family == "residential" | requests$unchanged_assessor_identity %in% TRUE)
matched[use_base] <- which(unique_bases)[base_match[use_base]]
component_geometry <- st_sf(requests[!is.na(matched), ] |>
  mutate(parcel_pin = direct_parcels$pin14[na.omit(matched)],
    parcel_evidence = direct_parcels$object_ids[na.omit(matched)]),
  geometry = st_geometry(direct_parcels)[na.omit(matched)])

# Read the recorded geocoder answers. Chicago requires one exact street-address
# point with score 100; Census requires one exact full street-address match.
chicago <- read_csv("../input/address_geocodes_chicago.csv", col_types = cols(.default = "c"))
census <- read_csv("../input/address_geocodes_census.csv", col_types = cols(.default = "c"))
address_points <- vector("list", nrow(chicago) + nrow(census))
for (i in seq_len(nrow(chicago))) {
  result <- jsonlite::fromJSON(chicago$response_json[i], simplifyVector = FALSE)
  stopifnot(is.null(result$error), !isTRUE(result$exceededTransferLimit))
  candidates <- result$candidates
  if (length(candidates)) stopifnot(result$spatialReference$wkid %in% c(3435, 102671))
  exact <- Filter(function(x) isTRUE(x$score == 100 && x$attributes$Addr_type == "PointAddress" &&
    x$attributes$Loc_name == "CHI_singleaddr" &&
    x$attributes$AddNum == str_extract(chicago$selected_address[i], "^[0-9]+") &&
    is.finite(x$location$x) && is.finite(x$location$y)), candidates)
  if (length(exact) != 1 || !isTRUE(geocode_street_address(exact[[1]]$address) ==
    geocode_street_address(chicago$selected_address[i]))) next
  address_points[[i]] <- tibble(address = chicago$selected_address[i], geocoder = "Chicago",
    x = exact[[1]]$location$x, y = exact[[1]]$location$y)
}
for (i in seq_len(nrow(census))) {
  result <- jsonlite::fromJSON(census$response_json[i], simplifyVector = FALSE)$result$addressMatches
  if (length(result) != 1 || geocode_street_address(result[[1]]$matchedAddress) !=
      geocode_street_address(census$selected_address[i])) next
  if (!isTRUE(between(result[[1]]$coordinates$x, -88, -87.5) &&
    between(result[[1]]$coordinates$y, 41.6, 42.1))) next
  point <- st_sfc(st_point(c(result[[1]]$coordinates$x, result[[1]]$coordinates$y)), crs = 4326) |> st_transform(3435)
  xy <- st_coordinates(point)
  address_points[[nrow(chicago) + i]] <- tibble(address = census$selected_address[i], geocoder = "Census", x = xy[1,1], y = xy[1,2])
}
address_points <- bind_rows(address_points) |> mutate(address = normalize_address(address)) |>
  arrange(address, desc(geocoder == "Chicago")) |> distinct(address, .keep_all = TRUE)

historical_points$point_source_pin <- historical_points$pin
previous_points <- replacements |> filter(!str_detect(new_project_ids, "/"), str_starts(old_project_id, "residential_")) |>
  transmute(old_pin = str_remove(old_project_id, "^residential_"), new_pin = str_remove(new_project_ids, "^residential_"))
historical_points <- bind_rows(historical_points, historical_points |> inner_join(previous_points,
  by = c("pin" = "old_pin"), relationship = "many-to-one") |> mutate(pin = new_pin) |> select(-new_pin))
current_points <- current_parcels |> filter(is.finite(longitude), is.finite(latitude)) |>
  transmute(pin, year = 2025L, lon = longitude, lat = latitude, point_source_pin = pin, row_id = NA_character_)
point_history <- bind_rows(historical_points, current_points) |>
  group_by(pin, year, point_source_pin) |> filter(n_distinct(paste(lon, lat)) == 1) |>
  summarise(lon = first(lon), lat = first(lat), row_id = paste(sort(unique(na.omit(row_id))), collapse = "/"), .groups = "drop")
point_history <- st_as_sf(point_history, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |> st_transform(3435)
xy <- st_coordinates(point_history)
point_history <- st_drop_geometry(point_history) |> mutate(x = xy[,1], y = xy[,2])
point_rows <- split(seq_len(nrow(point_history)), point_history$pin)
measurement_rows <- split(seq_len(nrow(records)), records$pin)

# Use an exact parcel coordinate to find a unique old polygon. If that fails,
# try the exact PIN one year later, still using the construction-year map.
missing_geometry <- vector("list", nrow(requests))
for (i in which(is.na(matched))) {
  request <- requests[i, ]; building <- buildings[match(request$project_id, buildings$project_id), ]
  current <- point_history |> filter(pin == request$component_pin, year == 2025, point_source_pin == pin)
  historical <- point_history |> filter(pin == request$component_pin, year == request$target_year, point_source_pin == pin)
  reference <- if (nrow(historical) == 1) historical else current
  if (!is.na(building$historical_location_pin)) reference <- point_history |>
    filter(pin == building$historical_location_pin, year == 2025, point_source_pin == pin)
  address <- historical_addresses |> filter(pin == request$component_pin) |>
    arrange(abs(year - request$target_year), year > request$target_year, desc(year))
  address <- if (nrow(address)) address$property_address[1] else NA_character_
  address <- coalesce(building$corrected_address, address)
  geocode <- address_points |> filter(address == normalize_address(.env$address))
  if (nrow(reference) != 1 && nrow(geocode) == 1) reference <- geocode
  next_year <- point_history |> filter(pin == request$component_pin, year == request$target_year + 1,
    point_source_pin == pin)
  alternatives <- bind_rows(if (nrow(reference) == 1) reference |> select(x, y),
    if (nrow(next_year) == 1) next_year |> select(x, y)) |> distinct()
  shapes <- predecessors |> filter(target_year == request$target_year)
  if (request$source_family == "commercial") {
    if (!isTRUE(request$unchanged_assessor_identity)) alternatives <- alternatives[0, ]
    next_parcel <- direct_parcels |> filter(pin14 == request$component_pin, target_year == request$target_year + 1)
    if (nrow(next_parcel) == 1) {
      xy <- st_coordinates(st_centroid(next_parcel))
      alternatives <- bind_rows(tibble(x = xy[1,1], y = xy[1,2]), alternatives)
    }
  }
  for (j in seq_len(nrow(alternatives))) {
    point <- st_as_sf(alternatives[j, ], coords = c("x", "y"), crs = 3435)
    hit <- st_within(point, shapes)[[1]]
    if (!length(hit) || !all(lengths(st_equals(shapes[hit, ])) == length(hit))) next
    hit <- hit[1]
    missing_geometry[[i]] <- st_sf(request |>
      mutate(parcel_pin = shapes$pin14[hit], parcel_evidence = as.character(shapes$object_id[hit])),
      geometry = st_geometry(shapes)[hit])
    break
  }
}
component_geometry <- bind_rows(component_geometry, bind_rows(missing_geometry))
complete <- requests |> count(project_id, name = "requested") |>
  left_join(st_drop_geometry(component_geometry) |> count(project_id, name = "found"),
    by = "project_id", relationship = "one-to-one") |> filter(requested == found)
project_geometry <- component_geometry |> semi_join(complete, by = "project_id") |>
  group_by(project_id) |> summarise(parcel_pins = paste(sort(unique(parcel_pin)), collapse = "/"),
    geometry_evidence = paste(sort(unique(parcel_evidence)), collapse = "/"), .groups = "drop")
centroids <- st_centroid(project_geometry) |> mutate(geometry_source = "historical_parcel_centroid")

# An individual home must have its own point when the old parcel covers several
# homes. A later point is usable only when the same PIN, year, units and areas agree.
individuals <- buildings |> filter(project_kind == "single_pin_single_card") |>
  inner_join(st_drop_geometry(project_geometry) |> select(project_id, parcel_pins),
    by = "project_id", relationship = "one-to-one") |> filter(component_pins != parcel_pins)
locations <- unclass(st_geometry(centroids))
for (i in seq_len(nrow(individuals))) {
  building <- individuals[i, ]; j <- match(building$project_id, centroids$project_id)
  site <- project_geometry[match(building$project_id, project_geometry$project_id), ]
  centroids$geometry_source[j] <- "unresolved_individual_location"
  points <- point_history |> filter(pin == building$component_pins,
    year %in% c(building$construction_year, building$construction_year + 1)) |>
    arrange(year, desc(point_source_pin == pin))
  point <- NULL
  if (nrow(points)) {
    points <- points |> filter(year == min(year), point_source_pin == first(point_source_pin)) |> distinct(x, y, .keep_all = TRUE)
    if (nrow(points) == 1) {
      point <- st_as_sf(points, coords = c("x", "y"), crs = 3435)
      if (!length(st_within(point, site)[[1]])) point <- NULL
    }
  }
  if (!is.null(point)) {
    locations[[j]] <- st_geometry(point)[[1]]
    centroids$geometry_source[j] <- if (points$point_source_pin != building$component_pins)
      "verified_previous_pin_historical_point" else if (points$year == building$construction_year)
      "exact_pin_construction_year_point" else "exact_pin_next_year_point"
    next
  }
  same_building <- records[measurement_rows[[building$component_pins]], ] |>
    filter(source_family == "residential", building_sqft > 1) |> add_count(tax_year) |> filter(n == 1, construction_year %in% c(building$construction_year, building$reported_construction_year),
      building_sqft == building$building_sqft, land_sqft == building$land_sqft,
      if_else(class %in% single_family_assessor_classes, 1, dwelling_units) == building$dwelling_units)
  points <- point_history |> filter(pin == building$component_pins, point_source_pin == pin,
    year %in% same_building$tax_year, year >= building$construction_year) |> arrange(desc(year))
  if (nrow(points)) {
    point <- st_as_sf(points[1, ], coords = c("x", "y"), crs = 3435)
    if (as.numeric(st_distance(point, site, by_element = TRUE)) <= location_rounding_ft) {
      locations[[j]] <- st_geometry(point)[[1]]
      centroids$geometry_source[j] <- "same_property_later_exact_parcel_point"
      next
    }
  }
  address <- historical_addresses |> filter(pin == building$component_pins) |>
    arrange(abs(year - building$construction_year), year > building$construction_year, desc(year))
  address <- coalesce(building$corrected_address, if (nrow(address)) address$property_address[1] else NA_character_)
  geocode <- address_points |> filter(address == normalize_address(.env$address), geocoder == "Chicago")
  if (nrow(geocode) != 1) next
  point <- st_as_sf(geocode, coords = c("x", "y"), crs = 3435)
  if (as.numeric(st_distance(point, site, by_element = TRUE)) <= location_rounding_ft) {
    locations[[j]] <- st_geometry(point)[[1]]
    centroids$geometry_source[j] <- "verified_chicago_individual_address_point"
  }
}

st_geometry(centroids) <- st_sfc(locations, crs = 3435)

location_permits <- st_read("../output/building_permits_for_verification.gpkg",
  query = "SELECT permit, permit_type, permit_status, work_description, longitude, latitude, geom FROM building_permits_clean WHERE permit_type = 'PERMIT - NEW CONSTRUCTION' AND permit_status = 'COMPLETE'",
  quiet = TRUE) |> st_drop_geometry() |> rename(permit_number = permit)

# A recorded completed new-building permit can locate a commercial project when
# all of its cited building permits identify one point.
for (i in which(buildings$source_family == "commercial" & (buildings$allow_far | buildings$allow_dupac) & !buildings$project_id %in% centroids$project_id)) {
  ids <- str_extract_all(coalesce(buildings$evidence_ids[i], ""), "(?<![0-9])1[0-9]{8}(?![0-9])")[[1]]
  source <- location_permits |> filter(permit_number %in% ids, permit_status == "COMPLETE",
    permit_type == "PERMIT - NEW CONSTRUCTION",
    str_detect(work_description, regex("RESIDENTIAL (BUILDING|TOWER)|APARTMENT (BUILDING|TOWER)|FULL BUILDING|FULL PERMIT|CONSTRUCT NEW", TRUE)))
  points <- source |> distinct(longitude, latitude)
  if (nrow(points) != 1 || any(!is.finite(unlist(points)))) next
  point <- st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
  centroids <- bind_rows(centroids, st_sf(project_id = buildings$project_id[i],
    geometry_source = "completed_permits_in_recorded_building_decision",
    geometry_evidence = paste(sort(source$permit_number), collapse = "/"), geometry = st_geometry(point)))
}

# Explicit source references selected during cleaning determine the building point.
for (i in which(!is.na(buildings$location_source))) {
  building <- buildings[i, ]
  if (!is.na(building$location_target_year)) stopifnot(building$construction_year == building$location_target_year)
  if (building$location_source == "completed_permit") {
    source <- location_permits |> filter(permit_number == building$location_id,
      permit_status == "COMPLETE", permit_type == "PERMIT - NEW CONSTRUCTION")
    stopifnot(nrow(source) == 1)
    point <- st_as_sf(source, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
  } else {
    source <- point_history |> filter(pin == building$location_id, year == building$location_year, point_source_pin == pin)
    stopifnot(nrow(source) == 1)
    point <- st_as_sf(source, coords = c("x", "y"), crs = 3435)
  }
  centroids <- centroids |> filter(project_id != building$project_id)
  centroids <- bind_rows(centroids, st_sf(project_id = building$project_id,
    geometry_source = building$location_source, geometry_evidence = building$location_id,
    geometry = st_geometry(point)))
}
centroids <- centroids |> filter(geometry_source != "unresolved_individual_location") |> arrange(project_id)
stopifnot(!anyDuplicated(centroids$project_id))
xy <- st_coordinates(centroids)
buildings <- buildings |> left_join(st_drop_geometry(centroids) |>
    mutate(x_3435 = xy[,1], y_3435 = xy[,2]) |> select(-parcel_pins),
    by = "project_id", relationship = "one-to-one") |>
  mutate(location_resolved = is.finite(x_3435) & is.finite(y_3435),
    allow_far = allow_far & location_resolved, allow_dupac = allow_dupac & location_resolved,
    far = if_else(allow_far, far, NA_real_), dupac = if_else(allow_dupac, dupac, NA_real_))

# Assign the ward and nearest ward boundary using the construction-year map.
source("../../shared/code/canonical_geometry_helpers.R")
points <- centroids |> inner_join(buildings |> select(project_id, construction_year),
  by = "project_id", relationship = "one-to-one") |>
  mutate(construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year))
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) |> st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel, eras = unique(points$era))
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = unique(points$era))
for (era_value in unique(points$era))
  stopifnot(all(lengths(st_within(points[points$era == era_value, ], ward_maps[[era_value]])) == 1L))
assignment <- assign_points_to_boundaries(points, points$era, ward_maps, boundary_lines, chunk_n = 2000L)
located <- bind_cols(st_drop_geometry(points), assignment) |>
  transmute(project_id, construction_year, construction_date, boundary_year, era,
    ward, neighbor_ward, ward_pair = ward_pair_id, distance_to_boundary_ft = dist_ft,
    within_1500ft = dist_ft <= boundary_window_ft, within_500ft = dist_ft <= main_boundary_window_ft)
boundary_scope <- buildings |>
  select(source_family, project_id, construction_year, allow_far, allow_dupac, location_resolved) |>
  left_join(located, by = c("project_id", "construction_year"), relationship = "one-to-one")
components <- buildings |> select(project_id, source_family, project_kind, component_pins) |>
  separate_longer_delim(component_pins, delim = "/") |> rename(component_pin = component_pins)
stopifnot(!anyDuplicated(components[c("project_id", "component_pin")]),
  all(is.finite(located$distance_to_boundary_ft)), !anyNA(located$ward_pair))
buildings <- buildings |> select(project_id, source_family, source_project_ids, source_addresses,
  component_pins, project_kind, construction_year, dwelling_units, building_sqft, land_sqft,
  allow_far, allow_dupac, far, dupac, class_values, external_multifamily, multifamily_source,
  source_row_ids, tax_year, evidence_ids, decision_references, decision_reason,
  geometry_source, geometry_evidence, x_3435, y_3435, location_resolved,
  zoning_group, zoning_year, zoning_source, zoning_note)
SaveData(buildings, "project_id", "../output/preferred_new_construction_project_ledger.csv")
SaveData(components, c("project_id", "component_pin"), "../output/preferred_new_construction_project_components.csv")
SaveData(boundary_scope, "project_id", "../output/preferred_new_construction_boundary_scope.csv")
SaveData(centroids, "project_id", "../output/preferred_new_construction_project_centroids.gpkg", delete_dsn = TRUE, quiet = TRUE)
project_geometry <- project_geometry |> left_join(buildings |> select(project_id, target_year = construction_year),
  by = "project_id", relationship = "one-to-one")
SaveData(project_geometry, "project_id", "../output/preferred_project_year_geometry.gpkg", delete_dsn = TRUE, quiet = TRUE)
