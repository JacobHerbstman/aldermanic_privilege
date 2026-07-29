# setwd("tasks/audits/historical_zoning_validation/code")

library(dplyr)
library(jsonlite)
library(readr)
library(sf)
library(stringr)

snapshot_zone_group <- function(zone_code) {
  zone_code <- str_to_upper(as.character(zone_code))
  case_when(
    str_detect(zone_code, "^RS-?") ~ "Single-Family Residential",
    str_detect(zone_code, "^(RT|RM)-?") ~ "Multi-Family Residential",
    str_detect(zone_code, "^B-?[1-7]-") ~ "Neighborhood Mixed-Use",
    str_detect(zone_code, "^C-?[1-7]-") ~ "Commercial",
    str_detect(zone_code, "^M-?[1-7]-") ~ "Industrial",
    str_starts(zone_code, "DX-") | str_starts(zone_code, "DR-") |
      str_starts(zone_code, "DS-") | str_starts(zone_code, "DC-") ~ "Downtown",
    str_starts(zone_code, "PD") ~ "Planned Development",
    str_starts(zone_code, "PMD") ~ "Planned Manufacturing",
    str_starts(zone_code, "POS") ~ "Open Space",
    TRUE ~ "Other"
  )
}

normalize_application <- function(value) {
  value <- str_to_upper(as.character(value))
  value <- str_replace_all(value, "[^A-Z0-9]", "")
  if_else(is.na(value) | value == "", NA_character_, value)
}

split_groups <- function(value) {
  if (is.na(value) || !nzchar(str_trim(value))) return(character())
  sort(unique(str_trim(str_split(value, ";", simplify = FALSE)[[1]])))
}

map_keys <- function(value) {
  if (is.na(value)) return(character())
  matches <- str_match_all(
    str_to_upper(value),
    "\\b([0-9IL]{1,2})\\s*-?\\s*([A-Z0-9])\\b"
  )[[1]]
  if (nrow(matches) == 0) return(character())
  number <- str_replace_all(matches[, 2], "[IL]", "1")
  suffix <- str_replace_all(matches[, 3], "1", "I")
  sort(unique(paste0(number, "-", suffix)))
}

historical_origins <- function(clause_json, final_group) {
  if (is.na(clause_json) || !nzchar(clause_json)) return(character())
  transitions <- tryCatch(
    fromJSON(clause_json, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(transitions)) return(character())

  reverse_edges <- list()
  for (transition in transitions) {
    destination <- transition$destination_group
    if (is.null(destination)) next
    for (source in transition$source_groups) {
      reverse_edges[[destination]] <- unique(c(reverse_edges[[destination]], source))
    }
  }

  ancestors <- character()
  frontier <- final_group
  while (length(frontier) > 0) {
    current <- frontier[[1]]
    frontier <- frontier[-1]
    parents <- reverse_edges[[current]]
    if (is.null(parents)) next
    new_parents <- setdiff(parents, ancestors)
    ancestors <- c(ancestors, new_parents)
    frontier <- c(frontier, new_parents)
  }

  roots <- ancestors[vapply(
    ancestors,
    function(group) {
      parents <- reverse_edges[[group]]
      is.null(parents) || length(setdiff(parents, group)) == 0
    },
    logical(1)
  )]
  if (length(roots) > 0) return(sort(unique(roots)))
  sort(unique(reverse_edges[[final_group]]))
}

zoning <- st_read(
  "/vsizip/../input/zoning_nov2012.zip/Zoning_nov2012.shp",
  quiet = TRUE
) %>%
  st_transform(3435)
ordinance_date_text <- as.character(zoning$ORDINANCE1)
ordinance_date_text[str_starts(ordinance_date_text, "-")] <- NA_character_
zoning <- zoning %>%
  transmute(
    zoning_polygon_id = row_number(),
    zone_code_2012 = ZONE_CLASS,
    zone_group_2012 = snapshot_zone_group(ZONE_CLASS),
    application_key_2012 = normalize_application(ORDINANCE_),
    ordinance_date_2012 = as.Date(ordinance_date_text),
    geometry
  )
zoning$source_geometry_valid <- st_is_valid(zoning)
zoning <- st_make_valid(zoning)

map_index <- st_read("../input/zoning_map_index.geojson", quiet = TRUE) %>%
  st_transform(3435) %>%
  transmute(zoning_map_grid = vapply(ZONE_MAP, function(value) {
    keys <- map_keys(value)
    if (length(keys) == 0) NA_character_ else keys[[1]]
  }, character(1)))

polygon_map_rows <- st_intersects(zoning, map_index)
zoning$zoning_map_grids <- vapply(
  polygon_map_rows,
  function(rows) paste(sort(unique(na.omit(map_index$zoning_map_grid[rows]))), collapse = ";"),
  character(1)
)

historical_events <- read_csv(
  "../output/historical_zoning_ordinances_2006_2012.csv",
  col_types = cols(.default = col_character())
) %>%
  transmute(
    event_date = as.Date(journal_meeting_date),
    application_key = normalize_application(application_key),
    event_source = "clerk_journal_full_ordinance",
    source_priority = 1L,
    journal_filename,
    pdf_page_number,
    map_numbers_raw,
    event_map_keys = lapply(map_numbers_raw, map_keys),
    source_groups,
    destination_groups,
    clause_count = as.integer(clause_count),
    clause_transitions_json,
    transition_available = !is.na(clause_transitions_json) & clause_transitions_json != ""
  )

modern_events <- read_csv(
  "../output/historical_zoning_matter_parsing.csv",
  col_types = cols(.default = col_character())
) %>%
  transmute(
    event_date = as.Date(matter_passed_date),
    application_key = normalize_application(application_key),
    event_source = "elms_ordinance",
    source_priority = 2L,
    journal_filename = NA_character_,
    pdf_page_number = NA_character_,
    map_numbers_raw = NA_character_,
    event_map_keys = rep(list(character()), n()),
    source_groups = from_groups,
    destination_groups = to_groups,
    clause_count = as.integer(amendment_clause_count),
    clause_transitions_json = NA_character_,
    transition_available = !is.na(from_groups) & from_groups != "" &
      !is.na(to_groups) & to_groups != ""
  )

events <- bind_rows(historical_events, modern_events) %>%
  mutate(
    event_destination_groups = lapply(destination_groups, split_groups)
  ) %>%
  filter(
    event_date >= as.Date("2006-01-01"),
    event_date <= as.Date("2012-10-31"),
    !is.na(application_key)
  )

selected_event_row <- rep(NA_integer_, nrow(zoning))
event_selection_status <- rep("anchor_predates_2006_or_undated", nrow(zoning))
post_2006_rows <- which(
  !is.na(zoning$ordinance_date_2012) &
    zoning$ordinance_date_2012 >= as.Date("2006-01-01")
)

for (row in post_2006_rows) {
  polygon_maps <- str_split(zoning$zoning_map_grids[row], ";", simplify = FALSE)[[1]]
  polygon_maps <- polygon_maps[nzchar(polygon_maps)]
  candidates <- which(
    events$event_date == zoning$ordinance_date_2012[row] &
      events$application_key == zoning$application_key_2012[row]
  )
  selection_method <- "exact_date_application"

  if (length(candidates) == 0) {
    candidates <- which(events$event_date == zoning$ordinance_date_2012[row])
    candidates <- candidates[vapply(
      events$event_map_keys[candidates],
      function(keys) length(intersect(keys, polygon_maps)) > 0,
      logical(1)
    )]
    candidates <- candidates[vapply(
      events$event_destination_groups[candidates],
      function(groups) zoning$zone_group_2012[row] %in% groups,
      logical(1)
    )]
    selection_method <- "date_map_destination_fallback"
  }

  if (length(candidates) > 1) {
    map_candidates <- candidates[vapply(
      events$event_map_keys[candidates],
      function(keys) length(intersect(keys, polygon_maps)) > 0,
      logical(1)
    )]
    if (length(map_candidates) > 0) candidates <- map_candidates

    destination_candidates <- candidates[vapply(
      events$event_destination_groups[candidates],
      function(groups) zoning$zone_group_2012[row] %in% groups,
      logical(1)
    )]
    if (length(destination_candidates) > 0) candidates <- destination_candidates

    parsed_candidates <- candidates[events$transition_available[candidates]]
    if (length(parsed_candidates) > 0) candidates <- parsed_candidates

    if (length(candidates) > 1) {
      candidates <- candidates[
        events$source_priority[candidates] == min(events$source_priority[candidates])
      ]
    }
  }

  if (length(candidates) == 1) {
    selected_event_row[row] <- candidates
    event_selection_status[row] <- selection_method
  } else if (length(candidates) == 0) {
    event_selection_status[row] <- "event_not_found"
  } else {
    event_selection_status[row] <- "event_ambiguous"
  }
}

zoning$event_selection_status <- event_selection_status
zoning$event_application_key <- NA_character_
zoning$event_source <- NA_character_
zoning$event_journal_filename <- NA_character_
zoning$event_pdf_page_number <- NA_character_
zoning$event_map_numbers <- NA_character_
zoning$event_source_groups <- NA_character_
zoning$event_destination_groups <- NA_character_
zoning$event_clause_count <- NA_integer_
zoning$candidate_origin_groups <- NA_character_

matched_rows <- which(!is.na(selected_event_row))
matched_events <- selected_event_row[matched_rows]
zoning$event_application_key[matched_rows] <- events$application_key[matched_events]
zoning$event_source[matched_rows] <- events$event_source[matched_events]
zoning$event_journal_filename[matched_rows] <- events$journal_filename[matched_events]
zoning$event_pdf_page_number[matched_rows] <- as.integer(events$pdf_page_number[matched_events])
zoning$event_map_numbers[matched_rows] <- events$map_numbers_raw[matched_events]
zoning$event_source_groups[matched_rows] <- events$source_groups[matched_events]
zoning$event_destination_groups[matched_rows] <- events$destination_groups[matched_events]
zoning$event_clause_count[matched_rows] <- as.integer(events$clause_count[matched_events])

zoning$candidate_zone_group_2006 <- zoning$zone_group_2012
zoning$reconstruction_status <- "anchor_predates_2006_or_undated"
zoning$january_2006_evidence <- "no_ordinance_dated_2006_or_later_on_2012_polygon"

for (row in post_2006_rows) {
  event_row <- selected_event_row[row]
  if (is.na(event_row)) {
    zoning$candidate_zone_group_2006[row] <- NA_character_
    zoning$reconstruction_status[row] <- event_selection_status[row]
    zoning$january_2006_evidence[row] <- "unresolved"
    next
  }

  if (events$event_source[event_row] == "clerk_journal_full_ordinance") {
    origins <- historical_origins(
      events$clause_transitions_json[event_row],
      zoning$zone_group_2012[row]
    )
  } else {
    origins <- split_groups(events$source_groups[event_row])
  }
  zoning$candidate_origin_groups[row] <- paste(origins, collapse = ";")
  if (length(origins) == 1) {
    zoning$candidate_zone_group_2006[row] <- origins[[1]]
    zoning$reconstruction_status[row] <- "unique_immediate_prior_group"
    zoning$january_2006_evidence[row] <- "latest_post_2006_ordinance_reversed"
  } else if (length(origins) > 1) {
    zoning$candidate_zone_group_2006[row] <- NA_character_
    zoning$reconstruction_status[row] <- "multiple_immediate_prior_groups"
    zoning$january_2006_evidence[row] <- "unresolved"
  } else {
    zoning$candidate_zone_group_2006[row] <- NA_character_
    zoning$reconstruction_status[row] <- "immediate_prior_group_not_parsed"
    zoning$january_2006_evidence[row] <- "unresolved"
  }
}

zoning <- zoning %>%
  mutate(
    reconstruction_resolved = !is.na(candidate_zone_group_2006),
    broad_group_changed_since_2006 = reconstruction_resolved &
      candidate_zone_group_2006 != zone_group_2012,
    polygon_area_sqft = as.numeric(st_area(geometry))
  )

st_write(
  zoning,
  "../output/historical_zoning_2006_candidate.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

zoning %>%
  st_drop_geometry() %>%
  filter(!reconstruction_resolved) %>%
  select(
    zoning_polygon_id,
    zone_code_2012,
    zone_group_2012,
    application_key_2012,
    ordinance_date_2012,
    zoning_map_grids,
    event_selection_status,
    event_source,
    event_source_groups,
    event_destination_groups,
    candidate_origin_groups,
    reconstruction_status,
    polygon_area_sqft
  ) %>%
  write_csv("../output/historical_zoning_2006_candidate_unresolved.csv")

zoning %>%
  st_drop_geometry() %>%
  count(reconstruction_status, reconstruction_resolved, wt = polygon_area_sqft, name = "area_sqft") %>%
  left_join(
    zoning %>%
      st_drop_geometry() %>%
      count(reconstruction_status, reconstruction_resolved, name = "polygons"),
    by = c("reconstruction_status", "reconstruction_resolved")
  ) %>%
  mutate(
    polygon_share = polygons / sum(polygons),
    area_share = area_sqft / sum(area_sqft)
  ) %>%
  arrange(!reconstruction_resolved, desc(polygons)) %>%
  write_csv("../output/historical_zoning_2006_candidate_summary.csv")
