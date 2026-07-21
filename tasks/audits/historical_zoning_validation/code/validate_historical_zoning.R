# setwd("tasks/audits/historical_zoning_validation/code")

library(dplyr)
library(readr)
library(sf)
library(stringr)
library(tidyr)

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

event_zone_groups <- function(zone_text) {
  if (is.na(zone_text) || !nzchar(str_trim(zone_text))) {
    return(character())
  }

  zone_text <- str_to_upper(zone_text) %>%
    str_replace_all("[\u2013\u2014]", "-") %>%
    str_replace_all("\\bRESIDENFIAL\\b", "RESIDENTIAL") %>%
    str_replace_all("\\bPLAIMED\\b", "PLANNED") %>%
    str_replace_all("\\b8([1-7])\\s*-", "B\\1-") %>%
    str_replace_all("\\bN[I1]\\s*1\\s*-", "M1-") %>%
    str_replace_all("\\bBI-", "B1-") %>%
    str_replace_all("\\bCI-", "C1-") %>%
    str_replace_all("\\bMI-", "M1-") %>%
    str_replace_all("\\bM\\s*[LI]\\s*-\\s*[LI]\\b", "M1-1") %>%
    str_replace_all("\\bM\\s*[L1I]\\s*-\\s*([1-3])\\b", "M1-\\1") %>%
    str_replace_all("\\bCL-", "C1-") %>%
    str_replace_all("\\bBL-", "B1-") %>%
    str_replace_all("\\bML-", "M1-")

  groups <- character()
  if (str_detect(zone_text, "PLANNED MANUFACTURING|\\bPMD\\b")) {
    groups <- c(groups, "Planned Manufacturing")
  }
  if (str_detect(zone_text, "PARKS? AND OPEN SPACE|OPEN SPACE DISTRICT|\\bPOS\\b")) {
    groups <- c(groups, "Open Space")
  }
  if (str_detect(zone_text, "PLANNED DEVELOPMENT|\\bPD\\b")) {
    groups <- c(groups, "Planned Development")
  }
  if (str_detect(zone_text, "\\bRS\\s*-?\\s*[1-8](?:\\b|\\.)|SINGLE-UNIT|DETACHED HOUSE")) {
    groups <- c(groups, "Single-Family Residential")
  }
  if (str_detect(zone_text, "\\b(?:RT|RM)\\s*-?\\s*[1-8](?:\\b|\\.)|TWO-FLAT|MULTI-UNIT")) {
    groups <- c(groups, "Multi-Family Residential")
  }
  if (str_detect(zone_text, "\\bB[1-7]\\s*-?\\s*\\d|NEIGHBORHOOD MIXED-USE")) {
    groups <- c(groups, "Neighborhood Mixed-Use")
  }
  if (str_detect(zone_text, "\\bC[1-5]\\s*-?\\s*\\d|COMMERCIAL DISTRICT")) {
    groups <- c(groups, "Commercial")
  }
  if (str_detect(zone_text, "\\bM[1-3]\\s*-?\\s*\\d|MANUFACTURING DISTRICT")) {
    groups <- c(groups, "Industrial")
  }
  if (str_detect(zone_text, "\\b(?:DX|DR|DS|DC)\\s*-?\\s*\\d|DOWNTOWN .* DISTRICT")) {
    groups <- c(groups, "Downtown")
  }
  if (str_detect(zone_text, "TRANSPORTATION DISTRICT|^\\s*T\\s*$")) {
    groups <- c(groups, "Other")
  }
  unique(groups)
}

parse_ordinance_transition <- function(page_text) {
  empty_result <- list(
    ordinance_from_groups = character(),
    ordinance_to_groups = character(),
    amendment_clause_count = 0L,
    ordinance_transition_status = "not_parsed",
    ordinance_clause_transitions = NA_character_
  )
  if (is.na(page_text) || !nzchar(str_trim(page_text))) {
    return(empty_result)
  }

  ordinance_text <- page_text %>%
    str_to_upper() %>%
    str_replace_all("[\u2013\u2014]", "-") %>%
    str_replace_all("\\bTHOSC\\b", "THOSE") %>%
    str_replace_all("\\bDEVELOPMCNT\\b", "DEVELOPMENT") %>%
    str_replace_all("\\bARNENDED\\b", "AMENDED") %>%
    str_replace_all("\\bSHOV(?:M|VOI|/II)\\b", "SHOWN") %>%
    str_replace_all("\\bTLIE\\b", "THE") %>%
    str_replace_all("AS \\.SHOWN", "AS SHOWN") %>%
    str_replace_all("HEREIN-\\s*ABOVE", "HEREINABOVE") %>%
    str_replace_all("\\bTD THE DESIGNATION\\b", "TO THE DESIGNATION") %>%
    str_replace_all("\\bLO THOSE OF\\b", "TO THOSE OF") %>%
    str_squish()
  clause_starts <- str_locate_all(
    ordinance_text,
    paste0(
      "AMENDED BY CHANG[A-Z]{1,10}|",
      "SECTION\\s*[0-9IVXL]+\\s*[.:]\\s*",
      "(?:THAT THE CHICAGO ZONING ORDINANCE BE AMENDED BY )?CHANG[A-Z]{1,10}"
    )
  )[[1]]
  if (nrow(clause_starts) == 0) {
    return(empty_result)
  }

  from_groups <- character()
  clause_from_groups <- list()
  destination_groups <- list()
  for (i in seq_len(nrow(clause_starts))) {
    clause_end <- if (i < nrow(clause_starts)) clause_starts[i + 1, 1] - 1L else nchar(ordinance_text)
    clause <- str_sub(ordinance_text, clause_starts[i, 1], clause_end)
    source_end <- str_locate(
      clause,
      paste0(
        "SYMBOLS?(?: AND INDICATIONS?)?(?: AS)? SHOWN (?:ON|IN)|",
        "AS SHOWN (?:ON|IN) (?:ZONING )?MAP|",
        "SYMBOLS?(?: AND INDICATIONS?)? WITHIN THE AREA HEREINABOVE DESCRIBED|",
        "SYMBOLS?(?: AND INDICATIONS?)? ESTABLISHED IN SECTION [0-9IVX]+ ABOVE"
      )
    )[1]
    destination_location <- str_locate(clause, "TO (?:THOSE OF|THE DESIGNATION OF)")
    destination_start <- destination_location[2]
    if (is.na(source_end) || is.na(destination_start)) {
      next
    }

    source_text <- str_sub(clause, 1, source_end - 1L)
    destination_text <- str_sub(clause, destination_start + 1L)
    destination_end <- str_locate(
      destination_text,
      "SECTION\\s+[0-9IVX]+|THIS ORDINANCE|AND A CORRESPONDING USE DISTRICT"
    )[1]
    if (!is.na(destination_end)) {
      destination_text <- str_sub(destination_text, 1, destination_end - 1L)
    } else {
      destination_text <- str_sub(destination_text, 1, 500)
    }

    source_groups <- event_zone_groups(source_text)
    clause_destination_groups <- event_zone_groups(destination_text)
    if (length(source_groups) > 0 && length(clause_destination_groups) == 1L) {
      from_groups <- union(from_groups, source_groups)
      clause_from_groups[[length(clause_from_groups) + 1L]] <- source_groups
      destination_groups[[length(destination_groups) + 1L]] <- clause_destination_groups
    }
  }

  if (length(destination_groups) == 0) {
    return(empty_result)
  }
  destination_values <- vapply(destination_groups, `[`, character(1), 1)
  terminal_destinations <- vapply(from_groups, function(origin_group) {
    current_group <- origin_group
    visited_groups <- character()
    for (step in seq_len(length(destination_groups) + 1L)) {
      matching_clauses <- vapply(
        clause_from_groups,
        function(groups) current_group %in% groups,
        FUN.VALUE = logical(1)
      )
      next_groups <- unique(destination_values[matching_clauses])
      next_groups <- setdiff(next_groups, current_group)
      if (length(next_groups) == 0L) {
        return(current_group)
      }
      if (length(next_groups) > 1L || next_groups %in% visited_groups) {
        return(NA_character_)
      }
      visited_groups <- c(visited_groups, current_group)
      current_group <- next_groups
    }
    NA_character_
  }, FUN.VALUE = character(1))

  transition_status <- case_when(
    length(destination_groups) == 1L ~ "single_clause",
    !anyNA(terminal_destinations) && length(unique(terminal_destinations)) == 1L ~ "common_terminal",
    TRUE ~ "independent_or_ambiguous"
  )
  usable_destination <- if (transition_status == "independent_or_ambiguous") {
    character()
  } else {
    unique(terminal_destinations)
  }
  clause_transitions <- vapply(
    seq_along(destination_groups),
    function(i) paste(paste(clause_from_groups[[i]], collapse = "+"), destination_values[i], sep = ">"),
    FUN.VALUE = character(1)
  )

  list(
    ordinance_from_groups = from_groups,
    ordinance_to_groups = usable_destination,
    amendment_clause_count = length(destination_groups),
    ordinance_transition_status = transition_status,
    ordinance_clause_transitions = paste(clause_transitions, collapse = "|")
  )
}

normalize_application_number <- function(application_number) {
  application_number <- str_to_upper(as.character(application_number)) %>%
    str_replace_all("[^A-Z0-9]", "") %>%
    str_replace("(?<=\\d)T1$", "")
  na_if(application_number, "")
}

normalize_pin <- function(pin) {
  str_pad(str_replace_all(as.character(pin), "[^0-9]", ""), 14, pad = "0")
}

extract_title_addresses <- function(matter_title) {
  address_pattern <- paste0(
    "\\b([0-9]{1,5})(?:\\s*-\\s*([0-9]{1,5}))?\\s+",
    "(N|NORTH|S|SOUTH|E|EAST|W|WEST)\\s+",
    "([A-Z0-9 .-]+?)\\s+",
    "(ST|STREET|AVE|AVENUE|BLVD|BOULEVARD|RD|ROAD|PL|PLACE|DR|DRIVE|CT|COURT|PKWY|PARKWAY)\\b"
  )
  title_text <- str_to_upper(coalesce(matter_title, ""))
  address_matches <- str_match_all(title_text, address_pattern)[[1]]

  range_start <- as.integer(address_matches[, 2])
  range_end_text <- address_matches[, 3]
  range_end <- mapply(
    function(start_value, end_text) {
      if (is.na(end_text)) {
        return(start_value)
      }
      start_text <- as.character(start_value)
      if (nchar(end_text) < nchar(start_text)) {
        end_text <- paste0(
          str_sub(start_text, 1, nchar(start_text) - nchar(end_text)),
          end_text
        )
      }
      as.integer(end_text)
    },
    range_start,
    range_end_text,
    USE.NAMES = FALSE
  )

  parsed_addresses <- if (nrow(address_matches) == 0L) {
    tibble()
  } else {
    tibble(
      range_start,
      range_end,
      street_direction = str_sub(address_matches[, 4], 1, 1),
      street_name = str_squish(address_matches[, 5])
    )
  }

  shared_street_pattern <- paste0(
    "((?:[0-9]{1,5}\\s*(?:,|AND)\\s*)+)([0-9]{1,5})\\s+",
    "(N|NORTH|S|SOUTH|E|EAST|W|WEST)\\s+",
    "([A-Z0-9 .-]+?)\\s+",
    "(ST|STREET|AVE|AVENUE|BLVD|BOULEVARD|RD|ROAD|PL|PLACE|DR|DRIVE|CT|COURT|PKWY|PARKWAY)\\b"
  )
  shared_matches <- str_match_all(title_text, shared_street_pattern)[[1]]
  shared_addresses <- if (nrow(shared_matches) == 0L) {
    tibble()
  } else {
    bind_rows(lapply(seq_len(nrow(shared_matches)), function(i) {
      house_numbers <- c(
        str_extract_all(shared_matches[i, 2], "[0-9]{1,5}")[[1]],
        shared_matches[i, 3]
      )
      tibble(
        range_start = as.integer(house_numbers),
        range_end = as.integer(house_numbers),
        street_direction = str_sub(shared_matches[i, 4], 1, 1),
        street_name = str_squish(shared_matches[i, 5])
      )
    }))
  }

  bind_rows(parsed_addresses, shared_addresses) %>%
    distinct()
}

parse_numbered_subareas <- function(page_text) {
  ordinance_text <- str_to_upper(coalesce(page_text, "")) %>%
    str_replace_all("[\u2013\u2014]", "-") %>%
    str_replace_all("\\bRESIDENFIAL\\b", "RESIDENTIAL") %>%
    str_replace_all("\\bPLAIMED\\b", "PLANNED") %>%
    str_replace_all("\\bM\\s*[LI]\\s*-\\s*[LI]\\b", "M1-1") %>%
    str_replace_all("\\bM\\s*[L1I]\\s*-\\s*([1-3])\\b", "M1-\\1") %>%
    str_replace_all("\\bBL-", "B1-")
  section_starts <- str_locate_all(ordinance_text, "SECTION\\s+[0-9]+\\s*[.:]")[[1]]
  if (nrow(section_starts) == 0L) {
    return(tibble())
  }

  bind_rows(lapply(seq_len(nrow(section_starts)), function(i) {
    section_end <- if (i < nrow(section_starts)) section_starts[i + 1L, 1] - 1L else nchar(ordinance_text)
    section_text <- str_sub(ordinance_text, section_starts[i, 1], section_end)
    destination_location <- str_locate(section_text, "TO (?:THOSE OF|THE DESIGNATION OF)")
    destination_start <- destination_location[1]
    if (is.na(destination_start)) {
      return(tibble())
    }

    source_groups <- event_zone_groups(str_sub(section_text, 1, destination_start - 1L))
    destination_groups <- event_zone_groups(
      str_sub(section_text, destination_location[2] + 1L, destination_location[2] + 250L)
    )
    common_address_start <- str_locate(section_text, "COMMON ADDRESS(?: OF PROPERTY)?\\s*:")[1]
    if (is.na(common_address_start)) {
      return(tibble())
    }
    common_addresses <- extract_title_addresses(str_sub(section_text, common_address_start))
    if (length(source_groups) == 0L || length(destination_groups) != 1L || nrow(common_addresses) == 0L) {
      return(tibble())
    }

    common_addresses %>%
      mutate(
        from_groups_override = paste(source_groups, collapse = ";"),
        to_group_override = destination_groups[1]
      )
  }))
}

parse_ordinance_subareas <- function(page_text) {
  ordinance_text <- str_to_upper(coalesce(page_text, "")) %>%
    str_replace_all("[\u2013\u2014]", "-") %>%
    str_replace_all("\\bM\\s*[LI]\\s*-\\s*[LI]\\b", "M1-1") %>%
    str_replace_all("\\bM\\s*[L1I]\\s*-\\s*([1-3])\\b", "M1-\\1")
  subarea_starts <- str_locate_all(ordinance_text, "(?:^|\\n)\\s*[A-Z]\\)\\s+FROM THE")[[1]]
  if (nrow(subarea_starts) == 0L) {
    return(tibble())
  }

  bind_rows(lapply(seq_len(nrow(subarea_starts)), function(i) {
    subarea_end <- if (i < nrow(subarea_starts)) subarea_starts[i + 1L, 1] - 1L else nchar(ordinance_text)
    subarea_text <- str_sub(ordinance_text, subarea_starts[i, 1], subarea_end)
    destination_start <- str_locate(subarea_text, "TO THOSE OF")[1]
    if (is.na(destination_start)) {
      return(tibble())
    }

    source_groups <- event_zone_groups(str_sub(subarea_text, 1, destination_start - 1L))
    destination_text <- str_sub(subarea_text, destination_start)
    destination_groups <- event_zone_groups(str_sub(destination_text, 1, 250))
    subarea_addresses <- extract_title_addresses(destination_text)
    if (length(source_groups) == 0L || length(destination_groups) != 1L || nrow(subarea_addresses) == 0L) {
      return(tibble())
    }

    subarea_addresses %>%
      mutate(
        from_groups_override = paste(source_groups, collapse = ";"),
        to_group_override = destination_groups[1]
      )
  }))
}

contains_group <- function(group_list, group) {
  if (is.na(group_list) || is.na(group) || !nzchar(group_list)) {
    return(FALSE)
  }
  group %in% str_split(group_list, fixed(";"), simplify = FALSE)[[1]]
}

apply_event_window <- function(project_groups, event_links, start_date, end_date) {
  state <- project_groups %>%
    transmute(
      pin,
      predicted_group = starting_group,
      linked_event_count = 0L,
      applied_event_count = 0L,
      applied_matter_ids = NA_character_
    )

  window_links <- event_links %>%
    filter(matter_passed_date > start_date, matter_passed_date <= end_date) %>%
    arrange(matter_passed_date, matter_id, pin)

  event_log <- window_links %>%
    distinct(matter_id, matter_passed_date, from_groups, to_groups, to_group) %>%
    mutate(
      linked_projects = 0L,
      origin_matches = 0L,
      group_changes_applied = 0L
    )

  for (matter in unique(window_links$matter_id)) {
    matter_links <- window_links %>% filter(matter_id == matter)
    state_rows <- match(matter_links$pin, state$pin)
    state$linked_event_count[state_rows] <- state$linked_event_count[state_rows] + 1L

    current_groups <- state$predicted_group[state_rows]
    origin_matches <- mapply(
      contains_group,
      matter_links$from_groups,
      current_groups,
      USE.NAMES = FALSE
    )
    usable_destination <- !is.na(matter_links$to_group)
    changes_group <- usable_destination & origin_matches & current_groups != matter_links$to_group
    rows_to_change <- state_rows[changes_group]

    if (length(rows_to_change) > 0) {
      state$predicted_group[rows_to_change] <- matter_links$to_group[changes_group]
      state$applied_event_count[rows_to_change] <- state$applied_event_count[rows_to_change] + 1L
      prior_ids <- state$applied_matter_ids[rows_to_change]
      state$applied_matter_ids[rows_to_change] <- ifelse(
        is.na(prior_ids),
        matter,
        paste(prior_ids, matter, sep = ";")
      )
    }

    event_log[event_log$matter_id == matter, "linked_projects"] <- nrow(matter_links)
    event_log[event_log$matter_id == matter, "origin_matches"] <- sum(origin_matches)
    event_log[event_log$matter_id == matter, "group_changes_applied"] <- sum(changes_group)
  }

  list(state = state, event_log = event_log)
}

find_nearest_plausible_event <- function(mismatch_rows, projects, event_points, start_date, end_date) {
  output <- mismatch_rows %>%
    transmute(
      validation,
      pin,
      candidate_matter_id = NA_character_,
      candidate_distance_ft = NA_real_,
      candidate_matched_pin = NA_character_,
      candidate_parcel_match_method = NA_character_,
      candidate_matter_title = NA_character_,
      candidate_address = NA_character_
    )

  for (i in seq_len(nrow(mismatch_rows))) {
    if (mismatch_rows$start_zone_group[i] == mismatch_rows$end_zone_group[i]) {
      next
    }

    plausible <- event_points %>%
      filter(
        matter_passed_date > start_date,
        matter_passed_date <= end_date,
        to_group == mismatch_rows$end_zone_group[i]
      )
    snapshot_ordinance_date <- mismatch_rows$end_ordinance_date[i]
    if (!is.na(snapshot_ordinance_date) && snapshot_ordinance_date > as.Date("1900-01-01")) {
      same_date <- plausible %>% filter(matter_passed_date == snapshot_ordinance_date)
      if (nrow(same_date) > 0) {
        plausible <- same_date
      }
    }
    if (nrow(plausible) == 0) {
      next
    }
    origin_matches <- vapply(
      plausible$from_groups,
      contains_group,
      group = mismatch_rows$start_zone_group[i],
      FUN.VALUE = logical(1)
    )
    plausible <- plausible[origin_matches, ]
    if (nrow(plausible) == 0) {
      next
    }

    project_row <- match(mismatch_rows$pin[i], projects$pin)
    nearest_row <- st_nearest_feature(projects[project_row, ], plausible)
    output$candidate_matter_id[i] <- plausible$matter_id[nearest_row]
    output$candidate_distance_ft[i] <- as.numeric(
      st_distance(projects[project_row, ], plausible[nearest_row, ], by_element = TRUE)
    )
    output$candidate_matched_pin[i] <- plausible$matched_pin[nearest_row]
    output$candidate_parcel_match_method[i] <- plausible$parcel_match_method[nearest_row]
    output$candidate_matter_title[i] <- plausible$matter_title[nearest_row]
    output$candidate_address[i] <- plausible$address_raw[nearest_row]
  }
  output
}

projects <- st_read("../input/parcels_with_geometry.gpkg", quiet = TRUE) %>%
  filter(construction_year >= 2006, construction_year <= 2022) %>%
  mutate(pin = normalize_pin(pin)) %>%
  select(
    pin,
    construction_year,
    unitscount,
    dist_to_boundary,
    ward_pair,
    geom
  )

if (anyDuplicated(projects$pin)) {
  stop("Density-project PINs are not unique.", call. = FALSE)
}
if (st_crs(projects)$epsg != 3435) {
  stop("Density-project coordinates are not in EPSG:3435.", call. = FALSE)
}

zoning_2012 <- st_read(
  "/vsizip/../input/zoning_nov2012.zip/Zoning_nov2012.shp",
  quiet = TRUE
) %>%
  select(
    zone_code_2012 = ZONE_CLASS,
    ordinance_number_2012 = ORDINANCE_,
    ordinance_date_2012 = ORDINANCE1
  )
zoning_2014 <- st_read(
  "/vsizip/../input/zoning_sep2014.zip/Zoning.shp",
  quiet = TRUE
) %>%
  select(
    zone_code_2014 = ZONE_CLASS,
    ordinance_date_2014 = ORDINANCE_
  )
zoning_2016 <- st_read(
  "/vsizip/../input/zoning_jan2016.zip/zoning_2016_01.shp",
  quiet = TRUE
) %>%
  select(
    zone_code_2016 = ZONE_CLASS,
    ordinance_number_2016 = ORDINANCE_,
    ordinance_date_2016 = ORDINANCE1
  )
zoning_2025 <- st_read("../input/zoning_sep2025.geojson", quiet = TRUE) %>%
  select(
    zone_code_2025 = zone_class,
    ordinance_number_2025 = ordinance,
    ordinance_date_2025 = ordinance_1,
    clerk_document_2025 = clerk_docn
  )

if (st_crs(zoning_2012) != st_crs(projects)) zoning_2012 <- st_transform(zoning_2012, st_crs(projects))
if (st_crs(zoning_2014) != st_crs(projects)) zoning_2014 <- st_transform(zoning_2014, st_crs(projects))
if (st_crs(zoning_2016) != st_crs(projects)) zoning_2016 <- st_transform(zoning_2016, st_crs(projects))
if (st_crs(zoning_2025) != st_crs(projects)) zoning_2025 <- st_transform(zoning_2025, st_crs(projects))

project_count <- nrow(projects)
projects <- st_join(projects, zoning_2012, left = TRUE, largest = TRUE)
projects <- st_join(projects, zoning_2014, left = TRUE, largest = TRUE)
projects <- st_join(projects, zoning_2016, left = TRUE, largest = TRUE)
projects <- st_join(projects, zoning_2025, left = TRUE, largest = TRUE)
if (nrow(projects) != project_count) {
  stop("A historical zoning join changed the density-project row count.", call. = FALSE)
}
if (anyNA(projects$zone_code_2012) || anyNA(projects$zone_code_2014) || anyNA(projects$zone_code_2016)) {
  stop("At least one density project is outside a historical zoning snapshot.", call. = FALSE)
}

projects <- projects %>%
  mutate(
    zone_group_2012 = snapshot_zone_group(zone_code_2012),
    zone_group_2014 = snapshot_zone_group(zone_code_2014),
    zone_group_2016 = snapshot_zone_group(zone_code_2016),
    zone_group_2025 = snapshot_zone_group(zone_code_2025),
    within_500ft = dist_to_boundary <= 500,
    multifamily = unitscount > 1
  )
project_lonlat <- st_coordinates(st_transform(projects, 4326))
projects <- projects %>%
  mutate(
    longitude = project_lonlat[, "X"],
    latitude = project_lonlat[, "Y"]
  )

matters <- read_csv(
  "../input/zoning_matters_far_20101101_20201231.csv",
  col_types = cols(.default = col_character())
)

ordinance_pages <- read_csv(
  "../input/pdf_text_20101101_20260212.csv",
  col_types = cols(.default = col_character())
) %>%
  mutate(page_number = as.integer(page_number))

ordinance_documents <- ordinance_pages %>%
  filter(page_number <= 4L) %>%
  arrange(matter_id, page_number) %>%
  summarise(page_text = paste(page_text, collapse = "\n"), .by = matter_id)

numbered_subarea_documents <- ordinance_pages %>%
  filter(page_number <= 8L) %>%
  arrange(matter_id, page_number) %>%
  summarise(page_text = paste(page_text, collapse = "\n"), .by = matter_id)

lettered_matter_subareas <- bind_rows(lapply(seq_len(nrow(ordinance_documents)), function(i) {
  parse_ordinance_subareas(ordinance_documents$page_text[i]) %>%
    mutate(matter_id = ordinance_documents$matter_id[i], .before = 1L)
})) %>%
  distinct()

numbered_matter_subareas <- bind_rows(lapply(seq_len(nrow(numbered_subarea_documents)), function(i) {
  parse_numbered_subareas(numbered_subarea_documents$page_text[i]) %>%
    mutate(matter_id = numbered_subarea_documents$matter_id[i], .before = 1L)
})) %>%
  distinct()

matter_subareas <- lettered_matter_subareas

ordinance_text <- ordinance_documents %>%
  mutate(
    ordinance_application_number = str_extract(
      str_to_upper(page_text),
      "\\bA\\s*-?\\s*[0-9]{4}\\b"
    ) %>% str_replace_all("[^A-Z0-9]", ""),
    parsed_transition = lapply(page_text, parse_ordinance_transition),
    ordinance_from_group_values = lapply(parsed_transition, `[[`, "ordinance_from_groups"),
    ordinance_to_group_values = lapply(parsed_transition, `[[`, "ordinance_to_groups"),
    amendment_clause_count = vapply(parsed_transition, `[[`, integer(1), "amendment_clause_count"),
    ordinance_transition_status = vapply(parsed_transition, `[[`, character(1), "ordinance_transition_status"),
    ordinance_clause_transitions = vapply(parsed_transition, `[[`, character(1), "ordinance_clause_transitions")
  ) %>%
  select(-page_text, -parsed_transition)

matters <- matters %>%
  left_join(ordinance_text, by = "matter_id", relationship = "one-to-one") %>%
  mutate(
    matter_passed_date = as.Date(matter_passed_date),
    application_number = coalesce(app_number, ordinance_application_number),
    application_key = normalize_application_number(application_number),
    raw_from_group_values = lapply(from_zoning_raw, event_zone_groups),
    raw_to_group_values = lapply(to_zoning_raw, event_zone_groups),
    ordinance_from_group_values = lapply(
      ordinance_from_group_values,
      function(x) if (is.null(x)) character() else x
    ),
    ordinance_to_group_values = lapply(
      ordinance_to_group_values,
      function(x) if (is.null(x)) character() else x
    ),
    ordinance_transition_status = coalesce(ordinance_transition_status, "not_parsed"),
    matter_document_key = str_remove(matter_id, "^S(?=O)"),
    from_group_values = Map(union, raw_from_group_values, ordinance_from_group_values),
    to_group_values = Map(
      function(raw_groups, ordinance_groups, transition_status) {
        if (transition_status == "independent_or_ambiguous") {
          character()
        } else if (length(ordinance_groups) == 1L) {
          ordinance_groups
        } else {
          raw_groups
        }
      },
      raw_to_group_values,
      ordinance_to_group_values,
      ordinance_transition_status
    ),
    from_groups = vapply(from_group_values, paste, collapse = ";", FUN.VALUE = character(1)),
    to_groups = vapply(to_group_values, paste, collapse = ";", FUN.VALUE = character(1)),
    raw_from_groups = vapply(raw_from_group_values, paste, collapse = ";", FUN.VALUE = character(1)),
    raw_to_groups = vapply(raw_to_group_values, paste, collapse = ";", FUN.VALUE = character(1)),
    ordinance_from_groups = vapply(ordinance_from_group_values, paste, collapse = ";", FUN.VALUE = character(1)),
    ordinance_to_groups = vapply(ordinance_to_group_values, paste, collapse = ";", FUN.VALUE = character(1)),
    amendment_clause_count = replace_na(amendment_clause_count, 0L),
    from_group_count = lengths(from_group_values),
    to_group_count = lengths(to_group_values),
    to_group = if_else(to_group_count == 1L, vapply(to_group_values, `[`, character(1), 1), NA_character_)
  ) %>%
  select(
    matter_id,
    matter_document_key,
    matter_passed_date,
    matter_title,
    address_raw,
    application_number,
    application_key,
    from_zoning_raw,
    to_zoning_raw,
    raw_from_groups,
    raw_to_groups,
    ordinance_from_groups,
    ordinance_to_groups,
    amendment_clause_count,
    ordinance_transition_status,
    ordinance_clause_transitions,
    from_groups,
    to_groups,
    from_group_count,
    to_group_count,
    to_group
  )

if (anyDuplicated(matters$matter_id)) {
  stop("Rezoning matters are not unique by matter_id.", call. = FALSE)
}
if (anyDuplicated(matters$matter_document_key)) {
  stop("Rezoning matters are not unique after removing substitute prefixes.", call. = FALSE)
}

committee_application_crosswalk <- tribble(
  ~application_key, ~snapshot_ordinance_date, ~matter_id, ~crosswalk_source_url,
  "A7914", as.Date("2013-07-24"), "O2013-5434", "https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public/document_uploads/journals-proceedings/2013/07_24_13.pdf",
  "A7916", as.Date("2013-12-11"), "SO2013-5435", "https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2013_12_11_VI_VII_VIII.pdf",
  "MA181", as.Date("2014-05-28"), "SO2014-2421", "https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2014_05_28_VI_VII.pdf",
  "A8086", as.Date("2015-10-28"), "O2015-134", "https://media.legistar.com/chic/meetings/F6A1A4CF-5323-405D-B9CF-53D8E346EEF0/Zoning%20Supplemental_20151023102317.pdf",
  "A8158", as.Date("2015-09-24"), "O2015-5413", "https://occprodstoragev1.blob.core.usgovcloudapi.net/lsmeetingattachmentspublic/d622403c-0334-4ea6-b658-db8bcc8657fc.pdf"
) %>%
  mutate(
    crosswalk_source = "zoning_committee_agenda",
    source_priority = 2L,
    linked_project_count = NA_integer_
  )

parcel_matches <- read_csv(
  "../input/rezoning_parcel_matches_20101101_20201231.csv",
  col_types = cols(.default = col_character())
) %>%
  transmute(
    matter_id,
    matched_pin = normalize_pin(matched_pin),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    parcel_match_method
  )

if (anyDuplicated(parcel_matches[c("matter_id", "matched_pin")])) {
  stop("Rezoning parcel matches are not unique by matter and PIN.", call. = FALSE)
}

project_keys <- projects %>%
  st_drop_geometry() %>%
  transmute(pin)

project_address_source <- read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  col_types = cols(.default = col_character()),
  col_select = c(pin, prop_address_full)
) %>%
  mutate(pin = normalize_pin(pin)) %>%
  inner_join(project_keys, by = "pin", relationship = "one-to-one")
if (anyDuplicated(project_address_source$pin)) {
  stop("Current parcel addresses are not unique by PIN.", call. = FALSE)
}

project_addresses <- bind_rows(lapply(seq_len(nrow(project_address_source)), function(i) {
  parsed_address <- extract_title_addresses(project_address_source$prop_address_full[i])
  if (nrow(parsed_address) == 0L) {
    return(tibble())
  }
  parsed_address %>%
    slice(1L) %>%
    transmute(
      pin = project_address_source$pin[i],
      house_number = range_start,
      street_direction,
      street_name
    )
})) %>%
  inner_join(project_keys, by = "pin", relationship = "one-to-one")

matter_title_addresses <- bind_rows(lapply(seq_len(nrow(matters)), function(i) {
  extract_title_addresses(matters$matter_title[i]) %>%
    mutate(matter_id = matters$matter_id[i], .before = 1L)
}))

project_addresses_by_street <- split(
  project_addresses,
  paste(project_addresses$street_direction, project_addresses$street_name, sep = "|")
)

subarea_address_links <- bind_rows(lapply(seq_len(nrow(matter_subareas)), function(i) {
  matter_subarea <- matter_subareas[i, ]
  street_key <- paste(matter_subarea$street_direction, matter_subarea$street_name, sep = "|")
  street_projects <- project_addresses_by_street[[street_key]]
  if (is.null(street_projects)) {
    return(tibble())
  }
  street_projects %>%
    filter(
      house_number >= matter_subarea$range_start,
      house_number <= matter_subarea$range_end,
      matter_subarea$range_start %% 2L != matter_subarea$range_end %% 2L |
        house_number %% 2L == matter_subarea$range_start %% 2L
    ) %>%
    transmute(
      pin,
      matter_id = matter_subarea$matter_id,
      matched_pin = pin,
      project_match_method = "ordinance_subarea_address",
      project_event_distance_ft = 0,
      parcel_match_method = "ordinance_subarea_address",
      from_groups_override = matter_subarea$from_groups_override,
      to_group_override = matter_subarea$to_group_override
    )
})) %>%
  distinct(pin, matter_id, .keep_all = TRUE)

title_address_links <- bind_rows(lapply(seq_len(nrow(matter_title_addresses)), function(i) {
  matter_address <- matter_title_addresses[i, ]
  street_key <- paste(matter_address$street_direction, matter_address$street_name, sep = "|")
  street_projects <- project_addresses_by_street[[street_key]]
  if (is.null(street_projects)) {
    return(tibble())
  }
  street_projects %>%
    filter(
      street_direction == matter_address$street_direction,
      street_name == matter_address$street_name,
      house_number >= matter_address$range_start,
      house_number <= matter_address$range_end,
      matter_address$range_start %% 2L != matter_address$range_end %% 2L |
        house_number %% 2L == matter_address$range_start %% 2L
    ) %>%
    transmute(
      pin,
      matter_id = matter_address$matter_id,
      matched_pin = pin,
      project_match_method = "exact_title_address",
      project_event_distance_ft = 0,
      parcel_match_method = "title_address_range"
    )
})) %>%
  distinct(pin, matter_id, .keep_all = TRUE)

event_points <- parcel_matches %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3435)
event_keys <- event_points %>%
  st_drop_geometry()
diagnostic_event_points <- event_points %>%
  inner_join(matters, by = "matter_id", relationship = "many-to-one")

exact_pin_links <- project_keys %>%
  inner_join(
    event_keys %>% select(matter_id, matched_pin, parcel_match_method),
    by = c("pin" = "matched_pin"),
    relationship = "one-to-many"
  ) %>%
  transmute(
    pin,
    matter_id,
    matched_pin = pin,
    project_match_method = "exact_pin",
    project_event_distance_ft = 0,
    parcel_match_method
  )

current_event_polygon_links <- projects %>%
  st_drop_geometry() %>%
  transmute(
    pin,
    matter_document_key = clerk_document_2025,
    current_event_date = as.Date(ordinance_date_2025),
    current_event_destination = zone_group_2025
  ) %>%
  filter(!is.na(matter_document_key), nzchar(matter_document_key)) %>%
  inner_join(
    matters %>% select(matter_id, matter_document_key, matter_passed_date, to_group),
    by = "matter_document_key",
    relationship = "many-to-one"
  ) %>%
  filter(
    current_event_date == matter_passed_date,
    current_event_destination == to_group
  ) %>%
  transmute(
    pin,
    matter_id,
    matched_pin = pin,
    project_match_method = "surviving_current_event_polygon",
    project_event_distance_ft = 0,
    parcel_match_method = "official_zoning_polygon"
  )

current_application_polygon_links <- projects %>%
  st_drop_geometry() %>%
  transmute(
    pin,
    application_key = normalize_application_number(ordinance_number_2025),
    snapshot_ordinance_date = as.Date(ordinance_date_2025),
    current_event_destination = zone_group_2025
  ) %>%
  filter(!is.na(application_key), !is.na(snapshot_ordinance_date)) %>%
  inner_join(
    committee_application_crosswalk %>%
      select(application_key, snapshot_ordinance_date, matter_id),
    by = c("application_key", "snapshot_ordinance_date"),
    relationship = "many-to-one"
  ) %>%
  inner_join(
    matters %>% select(matter_id, matter_passed_date, to_group),
    by = "matter_id",
    relationship = "many-to-one"
  ) %>%
  filter(
    snapshot_ordinance_date == matter_passed_date,
    current_event_destination == to_group
  ) %>%
  transmute(
    pin,
    matter_id,
    matched_pin = pin,
    project_match_method = "surviving_current_event_polygon",
    project_event_distance_ft = 0,
    parcel_match_method = "official_zoning_polygon"
  )

current_event_polygon_links <- bind_rows(
  current_event_polygon_links,
  current_application_polygon_links
) %>%
  distinct(pin, matter_id, .keep_all = TRUE)

nearby_event_rows <- st_is_within_distance(projects, event_points, dist = 500)
nearby_project_rows <- rep(seq_len(nrow(projects)), lengths(nearby_event_rows))
nearby_event_rows <- unlist(nearby_event_rows, use.names = FALSE)
nearby_event_links <- tibble(
  pin = projects$pin[nearby_project_rows],
  matter_id = event_points$matter_id[nearby_event_rows],
  matched_pin = event_points$matched_pin[nearby_event_rows],
  project_match_method = "nearby_parcel",
  project_event_distance_ft = as.numeric(st_distance(
    projects[nearby_project_rows, ],
    event_points[nearby_event_rows, ],
    by_element = TRUE
  )),
  parcel_match_method = event_points$parcel_match_method[nearby_event_rows]
  ) %>%
  arrange(pin, matter_id, project_event_distance_ft) %>%
  distinct(pin, matter_id, .keep_all = TRUE)

event_links <- bind_rows(
  subarea_address_links,
  exact_pin_links,
  title_address_links,
  current_event_polygon_links,
  nearby_event_links %>% filter(project_event_distance_ft <= 0.01)
) %>%
  mutate(project_match_rank = case_when(
    project_match_method == "ordinance_subarea_address" ~ 1L,
    project_match_method == "exact_pin" ~ 2L,
    project_match_method == "exact_title_address" ~ 3L,
    project_match_method == "surviving_current_event_polygon" ~ 4L,
    TRUE ~ 5L
  )) %>%
  arrange(pin, matter_id, project_match_rank, project_event_distance_ft) %>%
  distinct(pin, matter_id, .keep_all = TRUE) %>%
  select(-project_match_rank) %>%
  inner_join(matters, by = "matter_id", relationship = "many-to-one") %>%
  mutate(
    from_groups = coalesce(from_groups_override, from_groups),
    to_group = coalesce(to_group_override, to_group),
    to_groups = coalesce(to_group_override, to_groups)
  ) %>%
  select(-from_groups_override, -to_group_override)

validation_2012_2014 <- apply_event_window(
  projects %>% st_drop_geometry() %>% transmute(pin, starting_group = zone_group_2012),
  event_links,
  as.Date("2012-10-31"),
  as.Date("2014-07-30")
)
validation_2012_2016 <- apply_event_window(
  projects %>% st_drop_geometry() %>% transmute(pin, starting_group = zone_group_2012),
  event_links,
  as.Date("2012-10-31"),
  as.Date("2015-11-18")
)
validation_2014_2016 <- apply_event_window(
  projects %>% st_drop_geometry() %>% transmute(pin, starting_group = zone_group_2014),
  event_links,
  as.Date("2014-07-30"),
  as.Date("2015-11-18")
)

project_comparison <- projects %>%
  st_drop_geometry() %>%
  left_join(
    validation_2012_2014$state %>%
      rename_with(~paste0(.x, "_2012_2014"), -pin),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  left_join(
    validation_2012_2016$state %>%
      rename_with(~paste0(.x, "_2012_2016"), -pin),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  left_join(
    validation_2014_2016$state %>%
      rename_with(~paste0(.x, "_2014_2016"), -pin),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  mutate(
    agreement_2012_2014 = predicted_group_2012_2014 == zone_group_2014,
    agreement_2012_2016 = predicted_group_2012_2016 == zone_group_2016,
    agreement_2014_2016 = predicted_group_2014_2016 == zone_group_2016,
    true_change_2012_2014 = zone_group_2012 != zone_group_2014,
    true_change_2012_2016 = zone_group_2012 != zone_group_2016,
    true_change_2014_2016 = zone_group_2014 != zone_group_2016,
    predicted_change_2012_2014 = predicted_group_2012_2014 != zone_group_2012,
    predicted_change_2012_2016 = predicted_group_2012_2016 != zone_group_2012,
    predicted_change_2014_2016 = predicted_group_2014_2016 != zone_group_2014,
    date_consistent_change_2012_2014 = true_change_2012_2014 &
      as.Date(ordinance_date_2014) > as.Date("2012-10-31") &
      as.Date(ordinance_date_2014) <= as.Date("2014-07-30"),
    date_consistent_change_2012_2016 = true_change_2012_2016 &
      as.Date(ordinance_date_2016) > as.Date("2012-10-31") &
      as.Date(ordinance_date_2016) <= as.Date("2015-11-18"),
    date_consistent_change_2014_2016 = true_change_2014_2016 &
      as.Date(ordinance_date_2016) > as.Date("2014-07-30") &
      as.Date(ordinance_date_2016) <= as.Date("2015-11-18")
  )

validation_specs <- tribble(
  ~validation, ~start_group, ~end_group, ~predicted_group, ~agreement, ~true_change, ~predicted_change, ~date_consistent_change,
  "2012 to 2014", "zone_group_2012", "zone_group_2014", "predicted_group_2012_2014", "agreement_2012_2014", "true_change_2012_2014", "predicted_change_2012_2014", "date_consistent_change_2012_2014",
  "2012 to 2016", "zone_group_2012", "zone_group_2016", "predicted_group_2012_2016", "agreement_2012_2016", "true_change_2012_2016", "predicted_change_2012_2016", "date_consistent_change_2012_2016",
  "2014 to 2016", "zone_group_2014", "zone_group_2016", "predicted_group_2014_2016", "agreement_2014_2016", "true_change_2014_2016", "predicted_change_2014_2016", "date_consistent_change_2014_2016"
)

sample_specs <- tribble(
  ~sample, ~sample_filter,
  "All density-project PINs", "all",
  "Density-project PINs within 500ft", "within_500ft",
  "Multifamily PINs within 500ft", "multifamily_within_500ft"
)

validation_summary <- bind_rows(lapply(seq_len(nrow(validation_specs)), function(i) {
  spec <- validation_specs[i, ]
  bind_rows(lapply(seq_len(nrow(sample_specs)), function(j) {
    sample_spec <- sample_specs[j, ]
    sample_data <- project_comparison
    if (sample_spec$sample_filter == "within_500ft") {
      sample_data <- sample_data %>% filter(within_500ft)
    }
    if (sample_spec$sample_filter == "multifamily_within_500ft") {
      sample_data <- sample_data %>% filter(within_500ft, multifamily)
    }

    start_group <- sample_data[[spec$start_group]]
    end_group <- sample_data[[spec$end_group]]
    predicted_group <- sample_data[[spec$predicted_group]]
    agreement <- sample_data[[spec$agreement]]
    true_change <- sample_data[[spec$true_change]]
    predicted_change <- sample_data[[spec$predicted_change]]
    date_consistent_change <- replace_na(sample_data[[spec$date_consistent_change]], FALSE)

    tibble(
      validation = spec$validation,
      sample = sample_spec$sample,
      projects = nrow(sample_data),
      true_group_changes = sum(true_change),
      predicted_group_changes = sum(predicted_change),
      exact_group_assignments = sum(agreement),
      overall_agreement_rate = mean(agreement),
      exact_changed_assignments = sum(agreement & true_change),
      changed_assignment_rate = if_else(
        sum(true_change) > 0,
        sum(agreement & true_change) / sum(true_change),
        NA_real_
      ),
      missed_group_changes = sum(true_change & predicted_group == start_group),
      wrong_destination_group = sum(true_change & predicted_group != start_group & predicted_group != end_group),
      false_positive_group_changes = sum(!true_change & predicted_change),
      date_consistent_group_changes = sum(date_consistent_change),
      exact_date_consistent_assignments = sum(agreement & date_consistent_change),
      date_consistent_assignment_rate = if_else(
        sum(date_consistent_change) > 0,
        sum(agreement & date_consistent_change) / sum(date_consistent_change),
        NA_real_
      ),
      retrospective_or_undated_snapshot_differences = sum(true_change & !date_consistent_change)
    )
  }))
}))

event_coverage <- bind_rows(
  validation_2012_2014$event_log %>% mutate(validation = "2012 to 2014"),
  validation_2012_2016$event_log %>% mutate(validation = "2012 to 2016"),
  validation_2014_2016$event_log %>% mutate(validation = "2014 to 2016")
) %>%
  left_join(
    matters %>% select(matter_id, matter_title, address_raw, from_zoning_raw, to_zoning_raw),
    by = "matter_id",
    relationship = "many-to-one"
  ) %>%
  relocate(validation, matter_id, matter_passed_date)

mismatches <- bind_rows(
  project_comparison %>%
    filter(!agreement_2012_2014) %>%
    transmute(
      validation = "2012 to 2014",
      across(c(pin, construction_year, unitscount, dist_to_boundary, ward_pair, within_500ft, multifamily, longitude, latitude)),
      start_zone_code = zone_code_2012,
      end_zone_code = zone_code_2014,
      start_ordinance_date = ordinance_date_2012,
      end_ordinance_date = ordinance_date_2014,
      start_ordinance_number = ordinance_number_2012,
      end_ordinance_number = NA_character_,
      start_zone_group = zone_group_2012,
      end_zone_group = zone_group_2014,
      predicted_zone_group = predicted_group_2012_2014,
      linked_event_count = linked_event_count_2012_2014,
      applied_event_count = applied_event_count_2012_2014,
      applied_matter_ids = applied_matter_ids_2012_2014
    ),
  project_comparison %>%
    filter(!agreement_2012_2016) %>%
    transmute(
      validation = "2012 to 2016",
      across(c(pin, construction_year, unitscount, dist_to_boundary, ward_pair, within_500ft, multifamily, longitude, latitude)),
      start_zone_code = zone_code_2012,
      end_zone_code = zone_code_2016,
      start_ordinance_date = ordinance_date_2012,
      end_ordinance_date = ordinance_date_2016,
      start_ordinance_number = ordinance_number_2012,
      end_ordinance_number = ordinance_number_2016,
      start_zone_group = zone_group_2012,
      end_zone_group = zone_group_2016,
      predicted_zone_group = predicted_group_2012_2016,
      linked_event_count = linked_event_count_2012_2016,
      applied_event_count = applied_event_count_2012_2016,
      applied_matter_ids = applied_matter_ids_2012_2016
    ),
  project_comparison %>%
    filter(!agreement_2014_2016) %>%
    transmute(
      validation = "2014 to 2016",
      across(c(pin, construction_year, unitscount, dist_to_boundary, ward_pair, within_500ft, multifamily, longitude, latitude)),
      start_zone_code = zone_code_2014,
      end_zone_code = zone_code_2016,
      start_ordinance_date = ordinance_date_2014,
      end_ordinance_date = ordinance_date_2016,
      start_ordinance_number = NA_character_,
      end_ordinance_number = ordinance_number_2016,
      start_zone_group = zone_group_2014,
      end_zone_group = zone_group_2016,
      predicted_zone_group = predicted_group_2014_2016,
      linked_event_count = linked_event_count_2014_2016,
      applied_event_count = applied_event_count_2014_2016,
      applied_matter_ids = applied_matter_ids_2014_2016
  )
)

nearest_candidates <- bind_rows(
  find_nearest_plausible_event(
    mismatches %>% filter(validation == "2012 to 2014", within_500ft),
    projects,
    diagnostic_event_points,
    as.Date("2012-10-31"),
    as.Date("2014-07-30")
  ),
  find_nearest_plausible_event(
    mismatches %>% filter(validation == "2012 to 2016", within_500ft),
    projects,
    diagnostic_event_points,
    as.Date("2012-10-31"),
    as.Date("2015-11-18")
  ),
  find_nearest_plausible_event(
    mismatches %>% filter(validation == "2014 to 2016", within_500ft),
    projects,
    diagnostic_event_points,
    as.Date("2014-07-30"),
    as.Date("2015-11-18")
  )
)
mismatches <- mismatches %>%
  left_join(nearest_candidates, by = c("validation", "pin"), relationship = "one-to-one") %>%
  mutate(
    date_consistent_snapshot_change = case_when(
      validation == "2012 to 2014" ~ project_comparison$date_consistent_change_2012_2014[match(pin, project_comparison$pin)],
      validation == "2012 to 2016" ~ project_comparison$date_consistent_change_2012_2016[match(pin, project_comparison$pin)],
      validation == "2014 to 2016" ~ project_comparison$date_consistent_change_2014_2016[match(pin, project_comparison$pin)]
    )
  )

matter_parsing <- matters %>%
  select(
    matter_id,
    matter_document_key,
    matter_passed_date,
    matter_title,
    address_raw,
    application_number,
    application_key,
    from_zoning_raw,
    to_zoning_raw,
    raw_from_groups,
    raw_to_groups,
    ordinance_from_groups,
    ordinance_to_groups,
    amendment_clause_count,
    ordinance_transition_status,
    ordinance_clause_transitions,
    from_groups,
    to_groups,
    from_group_count,
    to_group_count
  )

event_address_ranges <- bind_rows(
  matter_title_addresses %>%
    inner_join(
      matters %>% select(matter_id, from_groups, to_group),
      by = "matter_id",
      relationship = "many-to-one"
    ) %>%
    mutate(address_source = "matter_title", exact_link_eligible = TRUE),
  lettered_matter_subareas %>%
    transmute(
      matter_id,
      range_start,
      range_end,
      street_direction,
      street_name,
      from_groups = from_groups_override,
      to_group = to_group_override,
      address_source = "lettered_subarea",
      exact_link_eligible = TRUE
    ),
  numbered_matter_subareas %>%
    transmute(
      matter_id,
      range_start,
      range_end,
      street_direction,
      street_name,
      from_groups = from_groups_override,
      to_group = to_group_override,
      address_source = "numbered_section",
      exact_link_eligible = FALSE
    )
) %>%
  distinct()

linkage_specs <- crossing(
  linkage_rule = c("all_nearby", "same_pin7", "same_pin8"),
  linkage_threshold_ft = c(0, 25, 50, 100, 150, 250, 500)
)
linkage_sensitivity <- bind_rows(lapply(seq_len(nrow(linkage_specs)), function(linkage_spec_row) {
  linkage_rule <- linkage_specs$linkage_rule[linkage_spec_row]
  linkage_threshold_ft <- linkage_specs$linkage_threshold_ft[linkage_spec_row]
  threshold_nearby_links <- nearby_event_links %>%
    filter(project_event_distance_ft <= linkage_threshold_ft)
  if (linkage_rule == "same_pin7") {
    threshold_nearby_links <- threshold_nearby_links %>%
      filter(str_sub(pin, 1, 7) == str_sub(matched_pin, 1, 7))
  }
  if (linkage_rule == "same_pin8") {
    threshold_nearby_links <- threshold_nearby_links %>%
      filter(str_sub(pin, 1, 8) == str_sub(matched_pin, 1, 8))
  }

  threshold_links <- bind_rows(
    subarea_address_links,
    exact_pin_links,
    title_address_links,
    current_event_polygon_links,
    threshold_nearby_links
  ) %>%
    mutate(project_match_rank = case_when(
      project_match_method == "ordinance_subarea_address" ~ 1L,
      project_match_method == "exact_pin" ~ 2L,
      project_match_method == "exact_title_address" ~ 3L,
      project_match_method == "surviving_current_event_polygon" ~ 4L,
      TRUE ~ 5L
    )) %>%
    arrange(pin, matter_id, project_match_rank, project_event_distance_ft) %>%
    distinct(pin, matter_id, .keep_all = TRUE) %>%
    select(-project_match_rank) %>%
    inner_join(matters, by = "matter_id", relationship = "many-to-one") %>%
    mutate(
      from_groups = coalesce(from_groups_override, from_groups),
      to_group = coalesce(to_group_override, to_group),
      to_groups = coalesce(to_group_override, to_groups)
    ) %>%
    select(-from_groups_override, -to_group_override)

  threshold_states <- list(
    "2012 to 2014" = apply_event_window(
      projects %>% st_drop_geometry() %>% transmute(pin, starting_group = zone_group_2012),
      threshold_links,
      as.Date("2012-10-31"),
      as.Date("2014-07-30")
    )$state,
    "2012 to 2016" = apply_event_window(
      projects %>% st_drop_geometry() %>% transmute(pin, starting_group = zone_group_2012),
      threshold_links,
      as.Date("2012-10-31"),
      as.Date("2015-11-18")
    )$state,
    "2014 to 2016" = apply_event_window(
      projects %>% st_drop_geometry() %>% transmute(pin, starting_group = zone_group_2014),
      threshold_links,
      as.Date("2014-07-30"),
      as.Date("2015-11-18")
    )$state
  )

  bind_rows(lapply(seq_len(nrow(validation_specs)), function(i) {
    spec <- validation_specs[i, ]
    threshold_comparison <- projects %>%
      st_drop_geometry() %>%
      left_join(
        threshold_states[[spec$validation]],
        by = "pin",
        relationship = "one-to-one"
      )

    bind_rows(lapply(seq_len(nrow(sample_specs)), function(j) {
      sample_spec <- sample_specs[j, ]
      sample_data <- threshold_comparison
      if (sample_spec$sample_filter == "within_500ft") {
        sample_data <- sample_data %>% filter(within_500ft)
      }
      if (sample_spec$sample_filter == "multifamily_within_500ft") {
        sample_data <- sample_data %>% filter(within_500ft, multifamily)
      }

      start_group <- sample_data[[spec$start_group]]
      end_group <- sample_data[[spec$end_group]]
      predicted_group <- sample_data$predicted_group
      agreement <- predicted_group == end_group
      true_change <- start_group != end_group
      predicted_change <- predicted_group != start_group
      date_consistent_change <- replace_na(
        project_comparison[[spec$date_consistent_change]][match(sample_data$pin, project_comparison$pin)],
        FALSE
      )

      tibble(
        linkage_rule,
        linkage_threshold_ft,
        validation = spec$validation,
        sample = sample_spec$sample,
        projects = nrow(sample_data),
        true_group_changes = sum(true_change),
        date_consistent_group_changes = sum(date_consistent_change),
        predicted_group_changes = sum(predicted_change),
        exact_changed_assignments = sum(agreement & true_change),
        changed_assignment_rate = sum(agreement & true_change) / sum(true_change),
        exact_date_consistent_assignments = sum(agreement & date_consistent_change),
        date_consistent_assignment_rate = sum(agreement & date_consistent_change) /
          sum(date_consistent_change),
        false_positive_group_changes = sum(!true_change & predicted_change)
      )
    }))
  }))
}))

snapshot_application_projects <- project_comparison %>%
  filter(date_consistent_change_2014_2016) %>%
  transmute(
    pin,
    within_500ft,
    multifamily,
    start_zone_group = zone_group_2014,
    end_zone_group = zone_group_2016,
    snapshot_application_number = ordinance_number_2016,
    application_key = normalize_application_number(ordinance_number_2016),
    snapshot_ordinance_date = as.Date(ordinance_date_2016)
  )

direct_application_crosswalk <- matters %>%
  filter(
    !is.na(application_key),
    matter_passed_date > as.Date("2014-07-30"),
    matter_passed_date <= as.Date("2015-11-18")
  ) %>%
  distinct(application_key, matter_passed_date, matter_id) %>%
  add_count(application_key, matter_passed_date, name = "candidate_matter_count") %>%
  filter(candidate_matter_count == 1L) %>%
  transmute(
    application_key,
    snapshot_ordinance_date = matter_passed_date,
    matter_id,
    crosswalk_source = "ordinance_application_number",
    crosswalk_source_url = NA_character_,
    source_priority = 3L,
    linked_project_count = NA_integer_
  )

inferred_application_crosswalk <- snapshot_application_projects %>%
  select(pin, application_key, snapshot_ordinance_date) %>%
  inner_join(
    event_links %>% select(pin, matter_id, matter_passed_date),
    by = "pin",
    relationship = "one-to-many"
  ) %>%
  filter(snapshot_ordinance_date == matter_passed_date) %>%
  distinct(application_key, snapshot_ordinance_date, matter_id, pin) %>%
  summarise(
    linked_project_count = n_distinct(pin),
    .by = c(application_key, snapshot_ordinance_date, matter_id)
  ) %>%
  add_count(application_key, snapshot_ordinance_date, name = "candidate_matter_count") %>%
  filter(candidate_matter_count == 1L) %>%
  transmute(
    application_key,
    snapshot_ordinance_date,
    matter_id,
    crosswalk_source = "exact_project_parcel",
    crosswalk_source_url = NA_character_,
    source_priority = 1L,
    linked_project_count
  )

application_crosswalk_candidates <- bind_rows(
  inferred_application_crosswalk,
  committee_application_crosswalk,
  direct_application_crosswalk
) %>%
  inner_join(
    matters %>%
      select(
        matter_id,
        matter_passed_date,
        application_number,
        from_groups,
        to_groups,
        to_group,
        ordinance_transition_status,
        matter_title,
        address_raw
      ),
    by = "matter_id",
    relationship = "many-to-one"
  )

application_crosswalk_candidate_summary <- application_crosswalk_candidates %>%
  summarise(
    candidate_matter_ids = paste(sort(unique(matter_id)), collapse = ";"),
    candidate_sources = paste(sort(unique(crosswalk_source)), collapse = ";"),
    crosswalk_conflict = n_distinct(matter_id) > 1L,
    .by = c(application_key, snapshot_ordinance_date)
  )

application_crosswalk <- application_crosswalk_candidates %>%
  arrange(application_key, snapshot_ordinance_date, source_priority, matter_id) %>%
  distinct(application_key, snapshot_ordinance_date, .keep_all = TRUE) %>%
  left_join(
    application_crosswalk_candidate_summary,
    by = c("application_key", "snapshot_ordinance_date"),
    relationship = "one-to-one"
  ) %>%
  select(-source_priority)

if (anyDuplicated(application_crosswalk[c("application_key", "snapshot_ordinance_date")])) {
  stop("The application crosswalk is not unique by application and ordinance date.", call. = FALSE)
}

application_validation <- snapshot_application_projects %>%
  left_join(
    application_crosswalk,
    by = c("application_key", "snapshot_ordinance_date"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    application_match = !is.na(matter_id),
    origin_group_match = mapply(contains_group, from_groups, start_zone_group),
    destination_group_match = to_group == end_zone_group,
    transition_match = application_match & origin_group_match & destination_group_match
  )

write_csv(validation_summary, "../output/historical_zoning_validation_summary.csv", na = "")
write_csv(project_comparison, "../output/historical_zoning_project_comparison.csv", na = "")
write_csv(event_coverage, "../output/historical_zoning_event_coverage.csv", na = "")
write_csv(mismatches, "../output/historical_zoning_mismatches.csv", na = "")
write_csv(matter_parsing, "../output/historical_zoning_matter_parsing.csv", na = "")
write_csv(
  bind_rows(
    lettered_matter_subareas %>%
      mutate(subarea_parser = "lettered_subarea", eligible_for_exact_address_link = TRUE),
    numbered_matter_subareas %>%
      mutate(
        subarea_parser = "numbered_section",
        eligible_for_exact_address_link = FALSE
      )
  ),
  "../output/historical_zoning_subarea_transitions.csv",
  na = ""
)
write_csv(event_address_ranges, "../output/historical_zoning_event_address_ranges.csv", na = "")
write_csv(
  event_links %>%
    select(
      pin,
      matter_id,
      matter_passed_date,
      project_match_method,
      matched_pin,
      project_event_distance_ft,
      parcel_match_method,
      from_groups,
      to_group
    ),
  "../output/historical_zoning_event_links.csv",
  na = ""
)
write_csv(linkage_sensitivity, "../output/historical_zoning_linkage_sensitivity.csv", na = "")
write_csv(application_validation, "../output/historical_zoning_application_validation.csv", na = "")
write_csv(application_crosswalk, "../output/historical_zoning_application_crosswalk.csv", na = "")
