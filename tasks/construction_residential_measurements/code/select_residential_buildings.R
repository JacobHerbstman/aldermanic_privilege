# setwd("tasks/construction_residential_measurements/code")
# episode_year_window <- 2
# maximum_building_gap <- 0.02
# successor_land_tolerance <- 0.005

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(episode_year_window, maximum_building_gap, successor_land_tolerance)
if (length(args) != 3L) stop("Expected 3 specification arguments from Makefile.")
episode_year_window <- as.integer(args[1])
maximum_building_gap <- as.numeric(args[2])
successor_land_tolerance <- as.numeric(args[3])

# Build residential successor condo requests

review_projects <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    project_kind = readr::col_character(),
    candidate_status = readr::col_character(),
    .default = readr::col_skip()
  )
) %>%
  filter(candidate_status == "review_required") %>%
  select(project_id, project_kind)

if (anyDuplicated(review_projects$project_id) > 0) {
  stop("Residential review projects are not unique.", call. = FALSE)
}

project_geometry <- sf::st_read(
  "../input/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  filter(project_id %in% review_projects$project_id) %>%
  select(project_id, target_year)

if (anyDuplicated(project_geometry$project_id) > 0) {
  stop("Residential review geometry is not unique by project.", call. = FALSE)
}

missing_geometry <- review_projects %>%
  sf::st_drop_geometry() %>%
  anti_join(
    project_geometry %>% sf::st_drop_geometry() %>% select(project_id),
    by = "project_id"
  )

# Projects without historical geometry remain unlinked; absence of a spatial
# successor is not evidence that no successor exists.


current_parcels <- data.table::fread(
  "../input/parcel_universe_2025_city.csv",
  select = c(
    "pin", "pin10", "tax_year", "class",
    "centroid_x_crs_3435", "centroid_y_crs_3435"
  ),
  colClasses = "character"
) %>%
  as_tibble() %>%
  transmute(
    pin = str_pad(str_replace_all(pin, "[^0-9]", ""), 14, pad = "0"),
    pin10 = str_pad(str_replace_all(pin10, "[^0-9]", ""), 10, pad = "0"),
    tax_year = as.integer(tax_year),
    class = str_squish(class),
    x_3435 = as.numeric(centroid_x_crs_3435),
    y_3435 = as.numeric(centroid_y_crs_3435)
  ) %>%
  filter(!is.na(x_3435), !is.na(y_3435))

if (anyDuplicated(current_parcels$pin) > 0) {
  stop("Current parcel PINs are not unique.", call. = FALSE)
}

current_parcels_sf <- current_parcels %>%
  sf::st_as_sf(coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)

intersections <- sf::st_intersects(current_parcels_sf, project_geometry)
linked_rows <- which(lengths(intersections) > 0)

current_parcel_links <- tibble::tibble(
  current_row = rep(linked_rows, lengths(intersections[linked_rows])),
  project_row = unlist(intersections[linked_rows], use.names = FALSE)
) %>%
  mutate(
    project_id = project_geometry$project_id[project_row],
    target_year = project_geometry$target_year[project_row],
    pin = current_parcels$pin[current_row],
    pin10 = current_parcels$pin10[current_row],
    tax_year = current_parcels$tax_year[current_row],
    class = current_parcels$class[current_row]
  ) %>%
  select(project_id, target_year, pin, pin10, tax_year, class) %>%
  distinct() %>%
  group_by(pin) %>%
  mutate(projects_per_current_pin = n_distinct(project_id)) %>%
  ungroup() %>%
  arrange(project_id, pin)

spatial_condo_requests <- current_parcel_links %>%
  filter(class == "299") %>%
  left_join(review_projects, by = "project_id", relationship = "many-to-one") %>%
  distinct(project_id, project_kind, target_year, pin10) %>%
  mutate(
    link_method = "current_centroid_in_construction_year_polygon",
    link_reason = NA_character_
  )

condo_requests <- spatial_condo_requests %>%
  distinct(project_id, pin10, .keep_all = TRUE) %>%
  group_by(pin10) %>%
  mutate(projects_per_condo_base = n_distinct(project_id)) %>%
  ungroup() %>%
  arrange(pin10, project_id)

stopifnot(!anyDuplicated(current_parcel_links[c("project_id", "pin")]),
  !anyDuplicated(condo_requests[c("project_id", "pin10")]))
SaveData(current_parcel_links, c("project_id", "pin"), "../output/residential_review_current_parcel_links.csv")
SaveData(condo_requests, c("project_id", "pin10"), "../output/residential_successor_condo_requests.csv")

# Build residential tieback episode resolution

stopifnot(is.finite(episode_year_window), episode_year_window >= 0,
  is.finite(maximum_building_gap), maximum_building_gap >= 0, maximum_building_gap < 1,
  is.finite(successor_land_tolerance), successor_land_tolerance >= 0, successor_land_tolerance < 1)

candidates <- readr::read_csv("../output/preferred_residential_project_candidates.csv",
  col_types = readr::cols(component_pins = readr::col_character(), class_values = readr::col_character(),
    source_row_ids = readr::col_character(), .default = readr::col_guess()))
parents <- candidates %>% filter(candidate_status == "review_required", project_kind == "tieback_building",
  !str_detect(class_values, "297"))
children <- candidates %>% filter(candidate_status == "retain_mechanical", component_count == 1,
  dwelling_units == 1, is.finite(building_sqft), building_sqft > 1)
project_polygons <- sf::st_read("../input/preferred_project_year_geometry.gpkg", quiet = TRUE)
polygons <- project_polygons %>% filter(project_id %in% parents$project_id)
points <- sf::st_read("../input/preferred_project_year_centroids.gpkg", quiet = TRUE) %>%
  filter(project_id %in% children$project_id,
    location_source != "former_parcel_centroid_unresolved_individual")
stopifnot(!anyDuplicated(candidates$project_id), !anyDuplicated(project_polygons$project_id),
  !anyDuplicated(points$project_id), sf::st_crs(polygons)$epsg == 3435, sf::st_crs(points)$epsg == 3435)
within <- sf::st_intersects(polygons, points)
con <- DBI::dbConnect(duckdb::duckdb())
parent_pins <- parents %>% select(component_pins) %>% tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  distinct(pin = component_pins)
DBI::dbWriteTable(con, "parent_pins", parent_pins)
history <- DBI::dbGetQuery(con, "SELECT h.pin, h.tax_year, h.card_num, h.year_built,
 h.building_sqft, h.num_apartments, h.single_v_multi_family, h.row_id,
 h.proration_key_pin, h.pin_num_cards
 FROM read_parquet('../input/residential_assessor_history.parquet') h
 INNER JOIN parent_pins p ON h.pin=p.pin
 WHERE h.proration_key_pin IS NOT NULL")
child_pins <- children %>% filter(project_id %in% points$project_id[unique(unlist(within))]) %>%
  distinct(pin = component_pins)
DBI::dbWriteTable(con, "child_pins", child_pins)
child_history <- DBI::dbGetQuery(con, "SELECT h.pin, h.tax_year, h.card_num, h.year_built,
 h.building_sqft, h.num_apartments, h.single_v_multi_family, h.row_id
 FROM read_parquet('../input/residential_assessor_history.parquet') h
 INNER JOIN child_pins p ON h.pin=p.pin")
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(!anyDuplicated(history[c("pin", "tax_year", "card_num")]))

resolution <- parents %>% transmute(source_project_id = project_id, decision_action = "unresolved",
  replacement_project_ids = NA_character_, assessment_year = NA_integer_, successor_assessment_year = NA_integer_,
  parent_cards = NA_integer_,
  successor_count = 0L, maximum_building_gap = NA_real_, evidence_ids = NA_character_,
  decision_reason = "No complete repeated house-card assessment matches the retained individual homes.")
# Require the same complete card list on every tied parcel in one assessment.
# Then compare its house count and sorted floor areas with the individual homes
# inside the historical site. This suppresses a duplicate parent; it creates no
# home, area, construction year, or tax-allocation-based measurement.
for (i in seq_len(nrow(parents))) {
  parent <- parents[i, ]
  j <- match(parent$project_id, polygons$project_id)
  if (is.na(j)) next
  successors <- children %>% filter(project_id %in% points$project_id[within[[j]]],
    abs(construction_year - parent$construction_year) <= episode_year_window) %>% arrange(building_sqft, project_id)
  resolution$successor_count[i] <- nrow(successors)
  if (nrow(successors) == 0) next
  # Parcel coverage establishes identity only; density retains reported land.
  # Distinct individual lots must partition the entire former site, with no
  # material overlap, uncovered land or extension beyond the former boundary.
  lots <- project_polygons %>% filter(project_id %in% successors$project_id)
  complete_partition <- FALSE
  if (nrow(lots) == nrow(successors)) {
    site <- sf::st_geometry(polygons[j, ])
    site_area <- as.numeric(sf::st_area(site))
    lot_union <- sf::st_union(lots)
    overlap_area <- sum(as.numeric(sf::st_area(lots))) - sum(as.numeric(sf::st_area(lot_union)))
    uncovered_area <- sum(as.numeric(sf::st_area(sf::st_difference(site, lot_union))))
    outside_area <- sum(as.numeric(sf::st_area(sf::st_difference(lot_union, site))))
    complete_partition <- is.finite(site_area) && site_area > 0 &&
      max(overlap_area, uncovered_area, outside_area) / site_area <= successor_land_tolerance
  }
  records <- history %>% filter(pin %in% str_split(parent$component_pins, "/")[[1]])
  child_records <- child_history %>% inner_join(
    successors %>% select(pin = component_pins, selected_year = construction_year),
    by = "pin", relationship = "many-to-one") %>%
    filter(year_built == selected_year, single_v_multi_family == "Single-Family",
      is.finite(building_sqft), building_sqft > 1,
      is.na(num_apartments) | num_apartments <= 1) %>%
    group_by(pin, tax_year) %>% filter(n() == 1) %>% ungroup()
  years <- sort(unique(records$tax_year), decreasing = TRUE)
  for (year in years) {
    snapshot <- records %>% filter(tax_year == year)
    if (!setequal(snapshot$pin, str_split(parent$component_pins, "/")[[1]]) || anyNA(snapshot$year_built) || any(snapshot$year_built != parent$construction_year) ||
        any(!is.finite(snapshot$building_sqft) | snapshot$building_sqft <= 1) ||
        any(is.na(snapshot$single_v_multi_family) | snapshot$single_v_multi_family != "Single-Family") ||
        any(is.finite(snapshot$num_apartments) & snapshot$num_apartments > 1)) next
    profiles <- snapshot %>% group_by(card_num) %>% summarise(pins = n_distinct(pin),
      floor_values = n_distinct(building_sqft), building_sqft = first(building_sqft), .groups = "drop") %>%
      arrange(building_sqft, card_num)
    if (any(profiles$floor_values != 1) || nrow(profiles) != nrow(successors)) next
    # Some tied parcels copy only part of the common card list. Require one
    # complete list and agreement among every copy before using site coverage.
    card_lists <- snapshot %>% group_by(pin) %>% summarise(cards = n(),
      declared_complete = all(pin_num_cards == n()), .groups = "drop")
    if (complete_partition && n_distinct(snapshot$proration_key_pin) == 1 &&
        all(card_lists$declared_complete %in% TRUE) && any(card_lists$cards == nrow(profiles))) {
      resolution$decision_action[i] <- "replace_by_residential_successors"
      resolution$replacement_project_ids[i] <- paste(sort(successors$project_id), collapse = "/")
      resolution$assessment_year[i] <- year
      resolution$parent_cards[i] <- nrow(profiles)
      resolution$evidence_ids[i] <- paste(sort(c(snapshot$row_id, successors$source_row_ids,
        paste0("historical_parcel_partition:", parent$project_id))), collapse = "/")
      resolution$decision_reason[i] <- "Retained individual house lots partition the entire former site without material gaps or overlap. The complete common house-card list agrees in house count and construction episode; keep the individual homes once despite later floor-area revisions."
      break
    }
    if (any(profiles$pins != n_distinct(snapshot$pin))) next
    # Identity may be established in an earlier complete assessment of the
    # individual houses. Later measurement revisions do not create new houses.
    matched_year <- NA_integer_
    matched_rows <- character()
    for (child_year in sort(unique(child_records$tax_year), decreasing = TRUE)) {
      child_snapshot <- child_records %>% filter(tax_year == child_year) %>% arrange(building_sqft, pin)
      if (!setequal(child_snapshot$pin, successors$component_pins) || nrow(child_snapshot) != nrow(profiles)) next
      gap <- abs(profiles$building_sqft - child_snapshot$building_sqft) /
        pmax(profiles$building_sqft, child_snapshot$building_sqft)
      if (all(gap <= maximum_building_gap)) {
        matched_year <- child_year
        matched_rows <- child_snapshot$row_id
        break
      }
    }
    if (is.na(matched_year)) next
    resolution$decision_action[i] <- "replace_by_residential_successors"
    resolution$replacement_project_ids[i] <- paste(sort(successors$project_id), collapse = "/")
    resolution$assessment_year[i] <- year
    resolution$successor_assessment_year[i] <- matched_year
    resolution$parent_cards[i] <- nrow(profiles)
    resolution$maximum_building_gap[i] <- max(gap)
    resolution$evidence_ids[i] <- paste(sort(c(snapshot$row_id, matched_rows)), collapse = "/")
    resolution$decision_reason[i] <- "The tied parcels repeat one complete house-card list; retained individual homes inside the historical site match its count, construction episode, and floor areas. Keep the individual homes once."
    break
  }
}
SaveData(resolution, c("source_project_id"), "../output/residential_tieback_episode_resolution.csv")

# Build residential class297 resolution

requests <- readr::read_csv("../output/residential_successor_condo_requests.csv",
  col_types = readr::cols(project_id = readr::col_character(), pin10 = readr::col_character(), .default = readr::col_guess()))

# Completed condo evidence also applies to earlier residential records. A tied
# parcel group additionally requires a completed permit confirming one building.
projects <- readr::read_csv("../output/preferred_residential_project_candidates.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    class_values = readr::col_character(), .default = readr::col_guess())) %>%
  filter(candidate_status == "review_required", project_kind == "class_297" | str_detect(class_values, "297") |
    (project_kind %in% c("single_pin_single_card", "tieback_building") & project_id %in% requests$project_id))
inventory <- readr::read_csv("../input/residential_project_candidate_inventory.csv",
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
permit_links <- readr::read_csv("../input/project_permit_chain_links.csv",
  col_types = readr::cols(.default = readr::col_character()))
mentions <- readr::read_csv("../input/project_permit_chain_unit_mentions.csv",
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

# Build residential overlap resolution

candidates <- readr::read_csv("../output/preferred_residential_project_candidates.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    class_values = readr::col_character(), .default = readr::col_guess()))
decisions <- readr::read_csv("../input/residential_overlap_decisions.csv",
  col_types = readr::cols(.default = readr::col_character()))
commercial <- readr::read_csv("../input/preferred_commercial_projects.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    source_row_ids = readr::col_character(), .default = readr::col_guess()))
commercial_sources <- readr::read_csv("../input/preferred_commercial_project_candidates.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    .default = readr::col_skip())) %>% left_join(
    readr::read_csv("../input/preferred_commercial_source_disposition.csv",
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

# Select residential buildings

candidates <- readr::read_csv("../output/preferred_residential_project_candidates.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    class_values = readr::col_character(), source_row_ids = readr::col_character(), .default = readr::col_guess()))
condos <- readr::read_csv("../output/residential_class297_resolution.csv",
  col_types = readr::cols(component_pins = readr::col_character(), condo_base = readr::col_character(),
    .default = readr::col_guess()))
overlap <- readr::read_csv("../output/residential_overlap_resolution.csv",
  col_types = readr::cols(component_pins = readr::col_character(), .default = readr::col_guess()))
houses <- readr::read_csv("../output/residential_tieback_episode_resolution.csv",
  col_types = readr::cols(.default = readr::col_character()))
stopifnot(!anyDuplicated(candidates$project_id), !anyDuplicated(overlap$source_project_id),
  !any(houses$source_project_id[houses$decision_action != "unresolved"] %in% condos$source_project_id))
# Matched condominium buildings use the completed-building review above.
houses <- houses %>% filter(!source_project_id %in% condos$source_project_id)

# Only supported decisions produce a building. Unfinished reviews remain explicit
# dispositions, rather than silently becoming exclusions or incomplete buildings.
retained <- condos %>% filter(decision_action == "retain_successor_evidence") %>%
  transmute(project_id = final_project_id, source_project_id, component_pins,
    construction_year, dwelling_units, building_sqft, land_sqft, allow_far, allow_dupac,
    membership_source, year_source, units_source, building_source, land_source,
    evidence_ids, decision_reason, confidence, decision_source = "completed_condominium_assessment") %>%
  bind_rows(overlap %>% filter(overlap_action == "retain_residential_resolution") %>%
    transmute(project_id = source_project_id, source_project_id, component_pins,
      construction_year, dwelling_units, building_sqft, land_sqft, allow_far = TRUE, allow_dupac = TRUE,
      membership_source = "recorded_residential_overlap_decision",
      year_source = "selected_residential_assessment", units_source = year_source,
      building_source = year_source, land_source = year_source,
      evidence_ids, decision_reason, confidence, decision_source = "recorded_overlap_decision")) %>%
  left_join(candidates %>% select(source_project_id = project_id, project_kind, class_values),
    by = "source_project_id", relationship = "one-to-one")
stopifnot(!anyDuplicated(retained$project_id), !anyDuplicated(retained$source_project_id),
  all(retained$construction_year >= 2006 & retained$construction_year <= 2022),
  all(retained$dwelling_units > 0 & retained$land_sqft > 1),
  all(!retained$allow_far | (is.finite(retained$building_sqft) & retained$building_sqft > 1)))
review <- retained %>% rename(source_project_ids = source_project_id) %>%
  mutate(geometry_source_project_ids = source_project_ids) %>% arrange(project_id)
components <- review %>% select(project_id, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  rename(component_pin = component_pins) %>% arrange(project_id, component_pin)
stopifnot(!anyDuplicated(components$component_pin))

condo_dispositions <- condos %>% group_by(source_project_id) %>%
  summarise(resolution_action = if (all(decision_action == "retain_successor_evidence")) "retain_successor_evidence" else if (all(decision_action == "exclude_outside_study_period")) "exclude_outside_study_period" else "unresolved",
    decision_reason = paste(sort(unique(decision_reason)), collapse = " | "),
    evidence_ids = paste(sort(unique(evidence_ids[!is.na(evidence_ids)])), collapse = " | "),
    .groups = "drop")
decisions <- bind_rows(condo_dispositions,
  overlap %>% transmute(source_project_id, resolution_action = overlap_action, decision_reason, evidence_ids),
  houses %>% transmute(source_project_id, resolution_action = decision_action, decision_reason, evidence_ids))
stopifnot(!anyDuplicated(decisions$source_project_id))
dispositions <- candidates %>% filter(candidate_status %in% c("review_required", "defer_to_commercial_reconciliation")) %>%
  transmute(source_project_id = project_id, prior_reason = decision_reason) %>%
  left_join(decisions, by = "source_project_id", relationship = "one-to-one") %>%
  left_join(retained %>% select(source_project_id, final_project_ids = project_id),
    by = "source_project_id", relationship = "one-to-one") %>%
  left_join(overlap %>% select(source_project_id, replacement_project_id),
    by = "source_project_id", relationship = "one-to-one") %>%
  left_join(houses %>% select(source_project_id, house_replacements = replacement_project_ids),
    by = "source_project_id", relationship = "one-to-one") %>%
  mutate(resolution_action = coalesce(resolution_action, "unresolved"),
    decision_reason = coalesce(decision_reason, prior_reason),
    final_project_ids = coalesce(final_project_ids,
      if_else(resolution_action == "replace_by_commercial", replacement_project_id, NA_character_),
      if_else(resolution_action == "replace_by_residential_successors", house_replacements, NA_character_)),
    final_disposition = case_when(
      resolution_action == "exclude_outside_study_period" ~ "excluded_outside_study_period",
      source_project_id %in% retained$source_project_id ~ "retained_as_resolved_project",
      resolution_action == "replace_by_commercial" ~ "replaced_by_commercial_project",
      resolution_action == "replace_by_residential_successors" ~ "replaced_by_existing_residential_project",
      TRUE ~ "review_required")) %>%
  select(source_project_id, final_disposition, final_project_ids, resolution_action, decision_reason, evidence_ids) %>%
  arrange(source_project_id)
stopifnot(!anyDuplicated(dispositions$source_project_id),
  all(!is.na(dispositions$final_project_ids[!dispositions$final_disposition %in% c("review_required", "excluded_outside_study_period")])))
SaveData(dispositions, c("source_project_id"), "../output/residential_review_source_dispositions.csv")

stopifnot(!anyDuplicated(candidates$project_id), !anyDuplicated(review$project_id),
  !anyDuplicated(dispositions$source_project_id),
  setequal(dispositions$source_project_id,
    candidates$project_id[candidates$candidate_status %in% c("review_required", "defer_to_commercial_reconciliation")]))

# Validate commercial replacements against the completed commercial dataset;
# this check does not copy or reapply its measurement corrections.
commercial <- readr::read_csv("../input/preferred_commercial_projects.csv",
  col_types = readr::cols(project_id = "c", construction_year = "d", dwelling_units = "d", land_sqft = "d",
    .default = readr::col_skip()))
replacements <- candidates %>% filter(replacement_check == "recorded_complete_commercial_project") %>%
  select(project_id, replacement_project_ids) %>% left_join(commercial,
    by = c("replacement_project_ids" = "project_id"), relationship = "many-to-one")
stopifnot(!anyDuplicated(commercial$project_id),
  all(between(replacements$construction_year, 2006, 2022)),
  all(is.finite(replacements$dwelling_units) & replacements$dwelling_units > 0),
  all(is.finite(replacements$land_sqft) & replacements$land_sqft > 1))

# This is the selected residential dataset. Unresolved sources remain in the
# complete source-disposition table and do not silently become analysis rows.
mechanical <- candidates %>% filter(candidate_status == "retain_mechanical") %>%
  transmute(project_id, source_project_ids = project_id, geometry_source_project_ids = project_id,
    component_pins, project_kind, construction_year, dwelling_units, building_sqft, land_sqft,
    allow_far, allow_dupac, class_values, membership_source = "preferred_residential_candidate",
    year_source, units_source, building_source, land_source, evidence_ids = source_row_ids,
    decision_reason, confidence = "source_rule", decision_source = "residential_candidate")
# Recorded cross-source replacements remove the residential copy of a building.
cross_source_decisions <- readr::read_csv(
  "../input/residential_additional_candidate_decisions.csv",
  show_col_types = FALSE) %>% filter(decision == "replace_by_commercial")
stopifnot(!anyDuplicated(cross_source_decisions$candidate_project_id),
  !anyNA(cross_source_decisions$replacement_project_ids))
projects <- bind_rows(mechanical, review) %>%
  filter(!project_id %in% cross_source_decisions$candidate_project_id) %>% arrange(project_id)
stopifnot(!anyDuplicated(projects$project_id), !anyNA(projects$construction_year),
  all(between(projects$construction_year, 2006, 2022)),
  all(!projects$allow_dupac | (is.finite(projects$dwelling_units) & projects$dwelling_units > 0 &
    is.finite(projects$land_sqft) & projects$land_sqft > 1)),
  all(!projects$allow_far | (is.finite(projects$building_sqft) & projects$building_sqft > 1 &
    is.finite(projects$land_sqft) & projects$land_sqft > 1)))
components <- projects %>% select(project_id, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>% rename(component_pin = component_pins)
stopifnot(!anyDuplicated(components$component_pin))

SaveData(projects, c("project_id"), "../output/residential_selected_buildings.csv")
