# setwd("tasks/new_construction_cleaning/code")
# episode_year_window <- 2
# maximum_building_gap <- 0.02
# successor_land_tolerance <- 0.005
source("../../setup_environment/code/packages.R")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3)
episode_year_window <- as.integer(args[1])
maximum_building_gap <- as.numeric(args[2])
successor_land_tolerance <- as.numeric(args[3])
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
project_polygons <- sf::st_read("../output/preferred_project_year_geometry.gpkg", quiet = TRUE)
polygons <- project_polygons %>% filter(project_id %in% parents$project_id)
points <- sf::st_read("../output/preferred_project_year_centroids.gpkg", quiet = TRUE) %>%
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
readr::write_csv(resolution, "../output/residential_tieback_episode_resolution.csv")
