# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/historical_zoning_validation/code")

source("../../../setup_environment/code/packages.R")

if (!requireNamespace("curl", quietly = TRUE)) {
  stop("The curl R package is required for the historical parcel reconciliation.", call. = FALSE)
}

normalize_pin <- function(pin) {
  str_pad(str_replace_all(as.character(pin), "[^0-9]", ""), 14, pad = "0")
}

normalize_application <- function(application_number) {
  application_number <- str_to_upper(as.character(application_number)) %>%
    str_replace_all("[^A-Z0-9]", "") %>%
    str_replace("(?<=\\d)T1$", "")
  na_if(application_number, "")
}

fetch_socrata <- function(base_url, parameters, request_label) {
  query <- paste0(
    base_url,
    "?",
    paste(
      paste0(
        URLencode(names(parameters), reserved = TRUE),
        "=",
        URLencode(unname(parameters), reserved = TRUE)
      ),
      collapse = "&"
    )
  )

  response <- NULL
  for (attempt in 1:5) {
    response <- tryCatch(curl::curl_fetch_memory(query), error = function(e) NULL)
    if (!is.null(response) && response$status_code == 200L) {
      break
    }
    Sys.sleep(attempt)
  }
  if (is.null(response) || response$status_code != 200L) {
    stop(sprintf("Cook County query failed for %s.", request_label), call. = FALSE)
  }

  payload <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
  if (!is.data.frame(payload) || nrow(payload) == 0L) {
    return(tibble())
  }
  as_tibble(payload)
}

fetch_successor_parcel_polygon <- function(pin10, request_label) {
  polygon_sources <- tribble(
    ~parcel_polygon_year, ~dataset_id,
    2021L, "77tz-riq7",
    2020L, "2yvh-uwrw",
    2019L, "c5mi-ck9v"
  )

  for (source_row in seq_len(nrow(polygon_sources))) {
    parameters <- c(
      "$select" = "pin10,the_geom",
      "$where" = sprintf("pin10='%s'", pin10),
      "$limit" = "2"
    )
    query <- paste0(
      "https://datacatalog.cookcountyil.gov/resource/",
      polygon_sources$dataset_id[source_row],
      ".geojson?",
      paste(
        paste0(
          URLencode(names(parameters), reserved = TRUE),
          "=",
          URLencode(unname(parameters), reserved = TRUE)
        ),
        collapse = "&"
      )
    )

    response <- NULL
    for (attempt in 1:5) {
      response <- tryCatch(curl::curl_fetch_memory(query), error = function(e) NULL)
      if (!is.null(response) && response$status_code == 200L) {
        break
      }
      Sys.sleep(attempt)
    }
    if (is.null(response) || response$status_code != 200L) {
      stop(sprintf("Cook County parcel-polygon query failed for %s.", request_label), call. = FALSE)
    }

    geojson_file <- tempfile(fileext = ".geojson")
    writeBin(response$content, geojson_file)
    parcel_polygon <- st_read(geojson_file, quiet = TRUE)
    unlink(geojson_file)
    if (nrow(parcel_polygon) > 1L) {
      stop(sprintf("Multiple parcel polygons were returned for %s.", request_label), call. = FALSE)
    }
    if (nrow(parcel_polygon) == 1L) {
      parcel_polygon$parcel_polygon_year <- polygon_sources$parcel_polygon_year[source_row]
      return(parcel_polygon)
    }
  }
  st_sf(parcel_polygon_year = integer(), geometry = st_sfc(crs = 4326))
}

parse_property_address <- function(address) {
  address_match <- str_match(
    str_to_upper(coalesce(address, "")),
    paste0(
      "^\\s*([0-9]{1,5})\\s+",
      "(N|NORTH|S|SOUTH|E|EAST|W|WEST)\\s+",
      "([A-Z0-9 .-]+?)\\s+",
      "(ST|STREET|AVE|AVENUE|BLVD|BOULEVARD|RD|ROAD|PL|PLACE|DR|DRIVE|CT|COURT|PKWY|PARKWAY)\\b"
    )
  )
  tibble(
    house_number = suppressWarnings(as.integer(address_match[, 2])),
    street_direction = str_sub(address_match[, 3], 1, 1),
    street_name = str_squish(address_match[, 4])
  )
}

mismatches <- read_csv(
  "../output/historical_zoning_mismatches.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  filter(
    validation %in% c("2012 to 2014", "2014 to 2016"),
    date_consistent_snapshot_change
  ) %>%
  select(
    validation,
    pin,
    construction_year,
    unitscount,
    dist_to_boundary,
    ward_pair,
    within_500ft,
    multifamily,
    longitude,
    latitude,
    start_zone_group,
    end_zone_group,
    end_ordinance_date
  )

project_comparison <- read_csv(
  "../output/historical_zoning_project_comparison.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  select(pin, snapshot_application_number = ordinance_number_2016)

matter_parsing <- read_csv(
  "../output/historical_zoning_matter_parsing.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  mutate(
    matter_passed_date = as.Date(matter_passed_date),
    application_key = normalize_application(application_key),
    to_group = if_else(as.integer(to_group_count) == 1L, to_groups, NA_character_)
  )

application_crosswalk <- read_csv(
  "../output/historical_zoning_application_crosswalk.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(
    application_key,
    snapshot_ordinance_date = as.Date(snapshot_ordinance_date),
    crosswalk_matter_id = matter_id,
    crosswalk_source,
    crosswalk_source_url
  )

targets <- mismatches %>%
  left_join(project_comparison, by = "pin", relationship = "many-to-one") %>%
  mutate(
    event_date = as.Date(end_ordinance_date),
    application_key = normalize_application(snapshot_application_number)
  ) %>%
  left_join(
    application_crosswalk,
    by = c("application_key", "event_date" = "snapshot_ordinance_date"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    matter_parsing %>%
      filter(!is.na(application_key)) %>%
      distinct(application_key, matter_passed_date, matter_id) %>%
      add_count(application_key, matter_passed_date, name = "matter_count") %>%
      filter(matter_count == 1L) %>%
      transmute(
        application_key,
        event_date = matter_passed_date,
        direct_matter_id = matter_id
      ),
    by = c("application_key", "event_date"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    snapshot_expected_matter_id = case_when(
      event_date == as.Date("2014-05-28") &
        application_key %in% c("A8057", "18471", "MA181") ~ "SO2014-2421",
      TRUE ~ coalesce(crosswalk_matter_id, direct_matter_id)
    ),
    crosswalk_source = case_when(
      event_date == as.Date("2014-05-28") &
        application_key %in% c("A8057", "18471", "MA181") ~ "official_journal_date_and_transition",
      TRUE ~ crosswalk_source
    )
  )

parcel_records <- vector("list", nrow(targets))
for (i in seq_len(nrow(targets))) {
  latitude_window <- 600 / 364000
  longitude_window <- 600 / (364000 * cos(targets$latitude[i] * pi / 180))
  parcel_records[[i]] <- fetch_socrata(
    "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json",
    c(
      "$select" = "pin,pin10,year,class,lon,lat,x_3435,y_3435,row_id",
      "$where" = sprintf(
        paste0(
          "year=%d and triad_name='City' and ",
          "lat between %.8f and %.8f and lon between %.8f and %.8f"
        ),
        as.integer(format(targets$event_date[i], "%Y")),
        targets$latitude[i] - latitude_window,
        targets$latitude[i] + latitude_window,
        targets$longitude[i] - longitude_window,
        targets$longitude[i] + longitude_window
      ),
      "$order" = "pin",
      "$limit" = "5000"
    ),
    sprintf("project %s", targets$pin[i])
  ) %>%
    transmute(
      validation = targets$validation[i],
      target_pin = targets$pin[i],
      candidate_pin = normalize_pin(pin),
      candidate_pin10 = as.character(pin10),
      parcel_year = suppressWarnings(as.integer(year)),
      parcel_class = as.character(class),
      candidate_longitude = suppressWarnings(as.numeric(lon)),
      candidate_latitude = suppressWarnings(as.numeric(lat)),
      candidate_x_3435 = suppressWarnings(as.numeric(x_3435)),
      candidate_y_3435 = suppressWarnings(as.numeric(y_3435)),
      parcel_row_id = as.character(row_id)
    )
  message(sprintf("Historical parcel neighborhoods: %d of %d", i, nrow(targets)))
}

candidates <- bind_rows(parcel_records)
if (nrow(candidates) == 0L) {
  stop("No historical parcel candidates were returned.", call. = FALSE)
}
if (anyDuplicated(candidates[c("validation", "target_pin", "candidate_pin", "parcel_year")])) {
  stop("Historical parcel candidates are not unique by validation, target, candidate PIN, and year.", call. = FALSE)
}

address_requests <- candidates %>%
  distinct(candidate_pin, parcel_year) %>%
  arrange(parcel_year, candidate_pin) %>%
  group_by(parcel_year) %>%
  group_split()
address_records <- list()
for (year_rows in address_requests) {
  parcel_year <- unique(year_rows$parcel_year)
  pin_chunks <- split(year_rows$candidate_pin, ceiling(seq_len(nrow(year_rows)) / 100L))
  for (pin_chunk in pin_chunks) {
    address_records[[length(address_records) + 1L]] <- fetch_socrata(
      "https://datacatalog.cookcountyil.gov/resource/3723-97qp.json",
      c(
        "$select" = "pin,year,prop_address_full,row_id",
        "$where" = sprintf(
          "year=%d and pin in(%s)",
          parcel_year,
          paste(sprintf("'%s'", pin_chunk), collapse = ",")
        ),
        "$order" = "pin",
        "$limit" = "5000"
      ),
      sprintf("parcel addresses in %d", parcel_year)
    ) %>%
      transmute(
        candidate_pin = normalize_pin(pin),
        parcel_year = suppressWarnings(as.integer(year)),
        historical_address = as.character(prop_address_full),
        address_row_id = as.character(row_id)
      )
  }
}

historical_addresses <- bind_rows(address_records)
if (anyDuplicated(historical_addresses[c("candidate_pin", "parcel_year")])) {
  stop("Historical parcel addresses are not unique by PIN and year.", call. = FALSE)
}

event_parcels <- read_csv(
  "../input/rezoning_parcel_matches_20101101_20201231.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(
    candidate_matter_id = matter_id,
    candidate_pin = normalize_pin(matched_pin)
  ) %>%
  distinct()

event_ranges <- read_csv(
  "../output/historical_zoning_event_address_ranges.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(
    candidate_matter_id = matter_id,
    range_start = as.integer(range_start),
    range_end = as.integer(range_end),
    street_direction,
    street_name,
    range_from_groups = from_groups,
    range_to_group = to_group,
    address_source
  )

plausible_matters <- bind_rows(lapply(seq_len(nrow(targets)), function(i) {
  date_matters <- matter_parsing %>%
    filter(matter_passed_date == targets$event_date[i])
  if (nrow(date_matters) == 0L) {
    return(tibble())
  }
  origin_matches <- vapply(
    date_matters$from_groups,
    function(groups) {
      targets$start_zone_group[i] %in%
        str_split(coalesce(groups, ""), fixed(";"))[[1]]
    },
    FUN.VALUE = logical(1)
  )
  date_matters[origin_matches, ] %>%
    transmute(
      validation = targets$validation[i],
      target_pin = targets$pin[i],
      candidate_matter_id = matter_id,
      candidate_matter_title = matter_title,
      candidate_from_groups = from_groups,
      candidate_to_group = to_group
    )
}))

parcel_candidates <- candidates %>%
  left_join(historical_addresses, by = c("candidate_pin", "parcel_year"), relationship = "many-to-one") %>%
  left_join(
    targets %>%
      select(
        validation,
        target_pin = pin,
        project_longitude = longitude,
        project_latitude = latitude,
        snapshot_expected_matter_id,
        start_zone_group,
        end_zone_group
      ),
    by = c("validation", "target_pin"),
    relationship = "many-to-one"
  )

project_points <- st_as_sf(
  parcel_candidates,
  coords = c("project_longitude", "project_latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)
candidate_points <- st_as_sf(
  parcel_candidates,
  coords = c("candidate_longitude", "candidate_latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)
parcel_candidates$candidate_distance_ft <- as.numeric(st_distance(
  project_points,
  candidate_points,
  by_element = TRUE
))

parsed_addresses <- parse_property_address(parcel_candidates$historical_address)
parcel_candidates <- bind_cols(parcel_candidates, parsed_addresses)

candidates <- bind_rows(lapply(seq_len(nrow(plausible_matters)), function(i) {
  parcel_candidates %>%
    filter(
      validation == plausible_matters$validation[i],
      target_pin == plausible_matters$target_pin[i]
    ) %>%
    mutate(
      candidate_matter_id = plausible_matters$candidate_matter_id[i],
      candidate_matter_title = plausible_matters$candidate_matter_title[i],
      candidate_from_groups = plausible_matters$candidate_from_groups[i],
      candidate_to_group = plausible_matters$candidate_to_group[i]
    )
}))

candidates$exact_event_pin <- mapply(
  function(matter_id, candidate_pin) {
    any(
      event_parcels$candidate_matter_id == matter_id &
        event_parcels$candidate_pin == candidate_pin
    )
  },
  candidates$candidate_matter_id,
  candidates$candidate_pin,
  USE.NAMES = FALSE
)

candidates$address_range_destination <- mapply(
  function(matter_id, origin_group, house_number, direction, street_value) {
    if (is.na(house_number) || is.na(direction) || is.na(street_value)) {
      return(NA_character_)
    }
    possible_ranges <- event_ranges %>%
      filter(
        candidate_matter_id == matter_id,
        street_direction == direction,
        street_name == street_value,
        house_number >= range_start,
        house_number <= range_end,
        range_start %% 2L != range_end %% 2L | house_number %% 2L == range_start %% 2L
    )
    if (nrow(possible_ranges) == 0L) {
      return(NA_character_)
    }
    origin_match <- vapply(
      possible_ranges$range_from_groups,
      function(groups) origin_group %in% str_split(coalesce(groups, ""), fixed(";"))[[1]],
      FUN.VALUE = logical(1)
    )
    destinations <- unique(possible_ranges$range_to_group[origin_match])
    destinations <- destinations[!is.na(destinations)]
    if (length(destinations) == 1L) destinations else NA_character_
  },
  candidates$candidate_matter_id,
  candidates$start_zone_group,
  candidates$house_number,
  candidates$street_direction,
  candidates$street_name,
  USE.NAMES = FALSE
)

candidates <- candidates %>%
  mutate(
    exact_event_pin = exact_event_pin & !is.na(candidate_to_group),
    historical_address_range = !is.na(address_range_destination),
    evidence_destination_group = case_when(
      historical_address_range ~ address_range_destination,
      exact_event_pin ~ candidate_to_group,
      TRUE ~ NA_character_
    ),
    lineage_evidence = case_when(
      exact_event_pin ~ "exact_event_pin",
      historical_address_range ~ "historical_address_range",
      TRUE ~ "none"
    )
  ) %>%
  arrange(target_pin, desc(exact_event_pin), desc(historical_address_range), candidate_distance_ft)

direct_candidate_matters <- candidates %>%
  filter(lineage_evidence != "none", candidate_pin == target_pin) %>%
  distinct(
    validation,
    target_pin,
    candidate_matter_id,
    evidence_destination_group,
    .keep_all = TRUE
  ) %>%
  add_count(validation, target_pin, name = "evidence_matter_count")

direct_candidates <- direct_candidate_matters %>%
  filter(evidence_matter_count == 1L) %>%
  mutate(successor_polygon_year = NA_integer_) %>%
  select(
    validation,
    target_pin,
    selected_matter_id = candidate_matter_id,
    selected_destination_group = evidence_destination_group,
    selected_candidate_pin = candidate_pin,
    selected_historical_address = historical_address,
    selected_candidate_distance_ft = candidate_distance_ft,
    successor_polygon_year,
    lineage_evidence
  )

successor_targets <- targets %>%
  transmute(
    validation,
    target_pin = pin,
    target_pin10 = str_sub(pin, 1L, 10L),
    start_zone_group,
    end_zone_group
  ) %>%
  anti_join(
    direct_candidates %>% select(validation, target_pin),
    by = c("validation", "target_pin")
  )

successor_evidence <- list()
for (i in seq_len(nrow(successor_targets))) {
  target_candidates <- parcel_candidates %>%
    filter(
      validation == successor_targets$validation[i],
      target_pin == successor_targets$target_pin[i],
      !is.na(candidate_longitude),
      !is.na(candidate_latitude)
    )
  if (
    nrow(target_candidates) == 0L ||
      any(target_candidates$candidate_pin == successor_targets$target_pin[i])
  ) {
    next
  }

  successor_polygon <- fetch_successor_parcel_polygon(
    successor_targets$target_pin10[i],
    successor_targets$target_pin[i]
  )
  if (nrow(successor_polygon) != 1L) {
    next
  }
  if (st_crs(successor_polygon)$epsg != 4326) {
    successor_polygon <- st_transform(successor_polygon, 4326)
  }

  candidate_points <- st_as_sf(
    target_candidates,
    coords = c("candidate_longitude", "candidate_latitude"),
    crs = 4326,
    remove = FALSE
  )
  target_candidates <- target_candidates %>%
    mutate(in_successor_polygon = as.vector(st_within(candidate_points, successor_polygon, sparse = FALSE)))

  polygon_evidence <- candidates %>%
    filter(
      validation == successor_targets$validation[i],
      target_pin == successor_targets$target_pin[i],
      candidate_pin %in% target_candidates$candidate_pin[target_candidates$in_successor_polygon],
      lineage_evidence != "none"
    ) %>%
    distinct(candidate_matter_id, evidence_destination_group, .keep_all = TRUE)
  if (nrow(polygon_evidence) > 0L) {
    for (j in seq_len(nrow(polygon_evidence))) {
      successor_evidence[[length(successor_evidence) + 1L]] <- tibble(
        validation = successor_targets$validation[i],
        target_pin = successor_targets$target_pin[i],
        selected_matter_id = polygon_evidence$candidate_matter_id[j],
        selected_destination_group = polygon_evidence$evidence_destination_group[j],
        selected_candidate_pin = polygon_evidence$candidate_pin[j],
        selected_historical_address = polygon_evidence$historical_address[j],
        selected_candidate_distance_ft = polygon_evidence$candidate_distance_ft[j],
        successor_polygon_year = successor_polygon$parcel_polygon_year[1],
        lineage_evidence = paste0("successor_polygon_", polygon_evidence$lineage_evidence[j])
      )
    }
  }

  possible_matter_ids <- plausible_matters %>%
    filter(
      validation == successor_targets$validation[i],
      target_pin == successor_targets$target_pin[i]
    ) %>%
    pull(candidate_matter_id)
  possible_ranges <- event_ranges %>%
    filter(candidate_matter_id %in% possible_matter_ids, !is.na(range_to_group))
  if (nrow(possible_ranges) == 0L) {
    next
  }
  possible_ranges <- possible_ranges[vapply(
    possible_ranges$range_from_groups,
    function(groups) {
      successor_targets$start_zone_group[i] %in%
        str_split(coalesce(groups, ""), fixed(";"))[[1]]
    },
    FUN.VALUE = logical(1)
  ), ]

  for (j in seq_len(nrow(possible_ranges))) {
    parcel_pool <- target_candidates %>%
      filter(
        in_successor_polygon,
        street_direction == possible_ranges$street_direction[j],
        street_name == possible_ranges$street_name[j],
        !is.na(house_number)
      )
    predecessor_parcels <- bind_rows(
      parcel_pool %>%
        filter(house_number <= possible_ranges$range_start[j]) %>%
        arrange(desc(house_number), candidate_distance_ft) %>%
        slice(1L),
      parcel_pool %>%
        filter(house_number >= possible_ranges$range_end[j]) %>%
        arrange(house_number, candidate_distance_ft) %>%
        slice(1L)
    ) %>%
      distinct(candidate_pin, .keep_all = TRUE)
    if (
      n_distinct(predecessor_parcels$candidate_pin) < 2L ||
        min(predecessor_parcels$house_number) > possible_ranges$range_start[j] ||
        max(predecessor_parcels$house_number) < possible_ranges$range_end[j]
    ) {
      next
    }

    successor_evidence[[length(successor_evidence) + 1L]] <- tibble(
      validation = successor_targets$validation[i],
      target_pin = successor_targets$target_pin[i],
      selected_matter_id = possible_ranges$candidate_matter_id[j],
      selected_destination_group = possible_ranges$range_to_group[j],
      selected_candidate_pin = paste(sort(unique(predecessor_parcels$candidate_pin)), collapse = ";"),
      selected_historical_address = paste(sort(unique(predecessor_parcels$historical_address)), collapse = "; "),
      selected_candidate_distance_ft = min(predecessor_parcels$candidate_distance_ft),
      successor_polygon_year = successor_polygon$parcel_polygon_year[1],
      lineage_evidence = "successor_polygon_address_bracket"
    )
  }
}

successor_candidates <- bind_rows(successor_evidence) %>%
  distinct(
    validation,
    target_pin,
    selected_matter_id,
    selected_destination_group,
    .keep_all = TRUE
  ) %>%
  add_count(validation, target_pin, name = "evidence_matter_count") %>%
  filter(evidence_matter_count == 1L) %>%
  select(-evidence_matter_count)

selected_candidates <- bind_rows(
  direct_candidates,
  successor_candidates
) %>%
  distinct(validation, target_pin, .keep_all = TRUE)

nearest_candidates <- parcel_candidates %>%
  distinct(validation, target_pin, .keep_all = TRUE) %>%
  select(
    validation,
    target_pin,
    nearest_candidate_pin = candidate_pin,
    nearest_historical_address = historical_address,
    nearest_candidate_distance_ft = candidate_distance_ft
  )

reconciliation <- targets %>%
  rename(target_pin = pin) %>%
  left_join(selected_candidates, by = c("validation", "target_pin"), relationship = "one-to-one") %>%
  left_join(nearest_candidates, by = c("validation", "target_pin"), relationship = "one-to-one") %>%
  mutate(
    selected_matter_matches_snapshot = selected_matter_id == snapshot_expected_matter_id,
    reconciliation_status = case_when(
      is.na(lineage_evidence) ~ "unresolved",
      selected_destination_group == end_zone_group ~ "reconciled_correct_destination",
      TRUE ~ "reconciled_wrong_destination"
    )
  )

strict_summary <- read_csv(
  "../output/historical_zoning_validation_summary.csv",
  show_col_types = FALSE
) %>%
  filter(validation %in% c("2012 to 2014", "2014 to 2016"))

reconciled_counts <- bind_rows(
  reconciliation %>%
    mutate(sample = "All density-project PINs"),
  reconciliation %>%
    filter(within_500ft) %>%
    mutate(sample = "Density-project PINs within 500ft"),
  reconciliation %>%
    filter(within_500ft, multifamily) %>%
    mutate(sample = "Multifamily PINs within 500ft")
) %>%
  summarise(
    lineage_reconciled_assignments = sum(reconciliation_status == "reconciled_correct_destination"),
    unresolved_assignments = sum(reconciliation_status != "reconciled_correct_destination"),
    .by = c(validation, sample)
  )

forward_validation_summary <- strict_summary %>%
  left_join(reconciled_counts, by = c("validation", "sample"), relationship = "one-to-one") %>%
  mutate(
    lineage_reconciled_assignments = coalesce(lineage_reconciled_assignments, 0L),
    unresolved_assignments = coalesce(unresolved_assignments, 0L),
    adjusted_exact_date_consistent_assignments =
      exact_date_consistent_assignments + lineage_reconciled_assignments,
    adjusted_date_consistent_assignment_rate =
      adjusted_exact_date_consistent_assignments / date_consistent_group_changes
  ) %>%
  select(
    validation,
    sample,
    date_consistent_group_changes,
    exact_date_consistent_assignments,
    date_consistent_assignment_rate,
    lineage_reconciled_assignments,
    adjusted_exact_date_consistent_assignments,
    adjusted_date_consistent_assignment_rate,
    unresolved_assignments
  )

write_csv(candidates, "../output/historical_zoning_event_date_parcel_candidates.csv", na = "")
write_csv(reconciliation, "../output/historical_zoning_forward_reconciliation.csv", na = "")
write_csv(forward_validation_summary, "../output/historical_zoning_forward_validation_summary.csv", na = "")
