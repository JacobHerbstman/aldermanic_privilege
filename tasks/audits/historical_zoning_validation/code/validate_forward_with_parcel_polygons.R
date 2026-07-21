# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/historical_zoning_validation/code")

source("../../../setup_environment/code/packages.R")

if (!requireNamespace("curl", quietly = TRUE)) {
  stop("The curl R package is required for historical address queries.", call. = FALSE)
}

normalize_pin <- function(pin) {
  str_pad(str_replace_all(as.character(pin), "[^0-9]", ""), 14, pad = "0")
}

fetch_addresses <- function(pin_year) {
  if (nrow(pin_year) == 0L) {
    return(tibble(
      parent_pin = character(),
      parcel_year = integer(),
      historical_address = character()
    ))
  }

  requests <- pin_year %>%
    distinct(parent_pin, parcel_year) %>%
    arrange(parcel_year, parent_pin) %>%
    group_by(parcel_year) %>%
    group_split()
  records <- list()

  for (year_rows in requests) {
    parcel_year <- unique(year_rows$parcel_year)
    chunks <- split(year_rows$parent_pin, ceiling(seq_len(nrow(year_rows)) / 100L))
    for (pin_chunk in chunks) {
      parameters <- c(
        "$select" = "pin,year,prop_address_full",
        "$where" = sprintf(
          "year=%d and pin in(%s)",
          parcel_year,
          paste(sprintf("'%s'", pin_chunk), collapse = ",")
        ),
        "$order" = "pin",
        "$limit" = "5000"
      )
      query <- paste0(
        "https://datacatalog.cookcountyil.gov/resource/3723-97qp.json?",
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
        stop(sprintf("Historical address query failed for %d.", parcel_year), call. = FALSE)
      }

      payload <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
      if (is.data.frame(payload) && nrow(payload) > 0L) {
        records[[length(records) + 1L]] <- as_tibble(payload) %>%
          transmute(
            parent_pin = normalize_pin(pin),
            parcel_year = as.integer(year),
            historical_address = as.character(prop_address_full)
          )
      }
    }
  }

  addresses <- bind_rows(records) %>%
    distinct(parent_pin, parcel_year, .keep_all = TRUE)
  if (anyDuplicated(addresses[c("parent_pin", "parcel_year")])) {
    stop("Historical addresses are not unique by PIN and year.", call. = FALSE)
  }
  addresses
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

comparison <- read_csv(
  "../output/historical_zoning_project_comparison.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)

targets <- bind_rows(
  comparison %>%
    filter(date_consistent_change_2012_2014) %>%
    transmute(
      validation = "2012 to 2014",
      target_pin = normalize_pin(pin),
      construction_year,
      unitscount,
      dist_to_boundary,
      ward_pair,
      within_500ft,
      multifamily,
      start_zone_group = zone_group_2012,
      end_zone_group = zone_group_2014,
      event_date = as.Date(ordinance_date_2014),
      strict_exact_assignment = agreement_2012_2014,
      longitude,
      latitude
    ),
  comparison %>%
    filter(date_consistent_change_2014_2016) %>%
    transmute(
      validation = "2014 to 2016",
      target_pin = normalize_pin(pin),
      construction_year,
      unitscount,
      dist_to_boundary,
      ward_pair,
      within_500ft,
      multifamily,
      start_zone_group = zone_group_2014,
      end_zone_group = zone_group_2016,
      event_date = as.Date(ordinance_date_2016),
      strict_exact_assignment = agreement_2014_2016,
      longitude,
      latitude
    )
) %>%
  mutate(event_year = as.integer(format(event_date, "%Y"))) %>%
  arrange(validation, target_pin)

if (anyDuplicated(targets[c("validation", "target_pin")])) {
  stop("Changed projects are not unique by validation and PIN.", call. = FALSE)
}
if (!all(targets$event_year %in% 2012:2015)) {
  stop("A forward-validation event falls outside the available parcel years.", call. = FALSE)
}

parcel_files <- tribble(
  ~parcel_year, ~archive_name, ~layer_name,
  2012L, "parcel_2012.zip", "Historical_Parcels_-_2012",
  2013L, "parcel_2013.zip", "Parcels_-_Historical_-_2013",
  2014L, "parcel_2014.zip", "Historical_Parcels_-_2014",
  2015L, "parcel_2015.zip", "Historical_Parcels_-_2015"
)

target_geometries <- list()
missing_target_rows <- list()

for (parcel_year in sort(unique(targets$event_year))) {
  year_targets <- targets %>%
    filter(event_year == parcel_year)
  source <- parcel_files %>%
    filter(.data$parcel_year == !!parcel_year)
  pin_values <- paste(sprintf("'%s'", unique(year_targets$target_pin)), collapse = ",")
  query <- sprintf(
    "SELECT pin14, pin10 FROM \"%s\" WHERE pin14 IN (%s)",
    source$layer_name,
    pin_values
  )
  exact_polygons <- st_read(
    paste0("/vsizip/../input/", source$archive_name),
    query = query,
    quiet = TRUE
  ) %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON", warn = FALSE) %>%
    mutate(
      target_pin = normalize_pin(pin14),
      target_geometry_year = parcel_year,
      target_geometry_source = "exact_event_year_pin"
    ) %>%
    select(target_pin, target_geometry_year, target_geometry_source)

  if (anyDuplicated(exact_polygons$target_pin)) {
    exact_polygons <- exact_polygons %>%
      group_by(target_pin) %>%
      summarise(
        target_geometry_year = first(target_geometry_year),
        target_geometry_source = first(target_geometry_source),
        do_union = TRUE,
        .groups = "drop"
      )
  }
  exact_polygons <- exact_polygons %>%
    inner_join(
      year_targets %>% select(validation, target_pin),
      by = "target_pin",
      relationship = "one-to-one"
    ) %>%
    select(validation, target_pin, target_geometry_year, target_geometry_source)
  st_geometry(exact_polygons) <- "geometry"
  target_geometries[[length(target_geometries) + 1L]] <- exact_polygons
  missing_target_rows[[length(missing_target_rows) + 1L]] <- year_targets %>%
    filter(!target_pin %in% exact_polygons$target_pin)
}

missing_targets <- bind_rows(missing_target_rows) %>%
  distinct(validation, target_pin, .keep_all = TRUE)

successor_files <- tribble(
  ~parcel_year, ~dataset_path, ~layer_name, ~pin_field,
  2012L, "/vsizip/../input/parcel_2012.zip", "Historical_Parcels_-_2012", "pin14",
  2013L, "/vsizip/../input/parcel_2013.zip", "Parcels_-_Historical_-_2013", "pin14",
  2014L, "/vsizip/../input/parcel_2014.zip", "Historical_Parcels_-_2014", "pin14",
  2015L, "/vsizip/../input/parcel_2015.zip", "Historical_Parcels_-_2015", "pin14",
  2016L, "/vsizip/../input/parcel_2016.zip", "Parcels_-_Historical_-_2016", "name",
  2017L, "/vsizip/../input/parcel_2017.zip", "Historical_Parcels_-_2017", "name",
  2018L, "/vsizip/../input/parcel_2018.zip", "Historical_Parcels_-_2018", "name",
  2019L, "/vsizip/../input/parcel_2019.zip", "Historical_Parcels_-_2019", "pin14",
  2020L, "/vsizip/../input/parcel_2020.zip/Parcels2020.gdb", "Parcel2020_enhanced", "PIN14",
  2021L, "/vsizip/../input/parcel_2021.zip", "Historical_Parcels_-_2021", "name",
  2022L, "/vsizip/../input/parcel_2022.zip", "Historical_Parcels_-_2022", "name"
)
successor_geometries <- list()
for (i in seq_len(nrow(successor_files))) {
  pin_values <- paste(sprintf("'%s'", unique(missing_targets$target_pin)), collapse = ",")
  query <- sprintf(
    "SELECT %s FROM \"%s\" WHERE %s IN (%s)",
    successor_files$pin_field[i],
    successor_files$layer_name[i],
    successor_files$pin_field[i],
    pin_values
  )
  polygons <- st_read(
    successor_files$dataset_path[i],
    query = query,
    quiet = TRUE
  )
  polygons <- polygons[
    as.character(st_geometry_type(polygons)) %in% c("POLYGON", "MULTIPOLYGON"),
  ] %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON", warn = FALSE) %>%
    mutate(
      target_pin = normalize_pin(.data[[successor_files$pin_field[i]]]),
      target_geometry_year = successor_files$parcel_year[i]
    ) %>%
    select(target_pin, target_geometry_year) %>%
    group_by(target_pin, target_geometry_year) %>%
    summarise(do_union = TRUE, .groups = "drop") %>%
    inner_join(
      missing_targets %>% select(validation, target_pin, construction_year),
      by = "target_pin",
      relationship = "one-to-many"
    )
  st_geometry(polygons) <- "geometry"
  successor_geometries[[length(successor_geometries) + 1L]] <- polygons
}

successor_geometries <- do.call(rbind, successor_geometries) %>%
  mutate(
    year_distance = abs(target_geometry_year - construction_year),
    before_construction = target_geometry_year < construction_year
  ) %>%
  arrange(validation, target_pin, year_distance, before_construction, target_geometry_year) %>%
  group_by(validation, target_pin) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(target_geometry_source = "later_exact_pin") %>%
  select(validation, target_pin, target_geometry_year, target_geometry_source)
target_geometries[[length(target_geometries) + 1L]] <- successor_geometries

target_geometries <- bind_rows(target_geometries) %>%
  st_transform(3435) %>%
  group_by(validation, target_pin) %>%
  summarise(
    target_geometry_year = first(target_geometry_year),
    target_geometry_source = first(target_geometry_source),
    do_union = TRUE,
    .groups = "drop"
  )

target_geometry_status <- targets %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(3435) %>%
  left_join(
    st_drop_geometry(target_geometries),
    by = c("validation", "target_pin"),
    relationship = "many-to-one"
  ) %>%
  mutate(target_geometry_found = !is.na(target_geometry_source))

polygon_lineage <- list()
annual_parent_polygons <- list()

for (parcel_year in sort(unique(targets$event_year))) {
  year_targets <- targets %>%
    filter(event_year == parcel_year)
  year_targets <- target_geometries %>%
    inner_join(
      year_targets,
      by = c("validation", "target_pin"),
      relationship = "many-to-one"
    )
  if (nrow(year_targets) == 0L) {
    next
  }

  source <- parcel_files %>%
    filter(.data$parcel_year == !!parcel_year)
  spatial_filter <- st_bbox(year_targets) %>%
    st_as_sfc() %>%
    st_as_text(digits = 12)
  parent_polygons <- st_read(
    paste0("/vsizip/../input/", source$archive_name),
    query = sprintf("SELECT pin14, pin10 FROM \"%s\"", source$layer_name),
    wkt_filter = spatial_filter,
    quiet = TRUE
  ) %>%
    mutate(
      parent_pin = normalize_pin(pin14),
      parcel_year = parcel_year
    ) %>%
    select(parent_pin, parcel_year)

  intersections <- st_intersects(year_targets, parent_polygons)
  relevant_parent_rows <- sort(unique(unlist(intersections)))
  annual_parent_polygons[[as.character(parcel_year)]] <-
    parent_polygons[relevant_parent_rows, ] %>%
    group_by(parent_pin) %>%
    summarise(parcel_year = first(parcel_year), do_union = TRUE, .groups = "drop")
  for (i in seq_len(nrow(year_targets))) {
    if (length(intersections[[i]]) == 0L) {
      next
    }
    for (parent_row in intersections[[i]]) {
      overlap_area <- as.numeric(st_area(st_intersection(
        st_make_valid(st_geometry(year_targets)[i]),
        st_make_valid(st_geometry(parent_polygons)[parent_row])
      )))
      if (length(overlap_area) == 0L || sum(overlap_area) <= 1) {
        next
      }
      polygon_lineage[[length(polygon_lineage) + 1L]] <- tibble(
        validation = year_targets$validation[i],
        target_pin = year_targets$target_pin[i],
        event_date = year_targets$event_date[i],
        parcel_year = parcel_year,
        parent_pin = parent_polygons$parent_pin[parent_row],
        overlap_area_sqft = sum(overlap_area),
        target_area_sqft = as.numeric(st_area(st_geometry(year_targets)[i])),
        parent_area_sqft = as.numeric(st_area(st_geometry(parent_polygons)[parent_row])),
        target_geometry_year = year_targets$target_geometry_year[i],
        target_geometry_source = year_targets$target_geometry_source[i]
      )
    }
  }
}

polygon_lineage <- bind_rows(polygon_lineage) %>%
  summarise(
    overlap_area_sqft = sum(overlap_area_sqft),
    target_area_sqft = first(target_area_sqft),
    parent_area_sqft = sum(parent_area_sqft),
    target_geometry_year = first(target_geometry_year),
    target_geometry_source = first(target_geometry_source),
    .by = c(validation, target_pin, event_date, parcel_year, parent_pin)
  ) %>%
  mutate(
    target_overlap_share = overlap_area_sqft / target_area_sqft,
    parent_overlap_share = overlap_area_sqft / parent_area_sqft
  ) %>%
  arrange(validation, target_pin, desc(overlap_area_sqft), parent_pin)

historical_addresses <- fetch_addresses(
  polygon_lineage %>% select(parent_pin, parcel_year)
)
parsed_addresses <- parse_property_address(historical_addresses$historical_address)
historical_addresses <- bind_cols(historical_addresses, parsed_addresses)

matter_parsing <- read_csv(
  "../output/historical_zoning_matter_parsing.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  mutate(
    matter_passed_date = as.Date(matter_passed_date),
    matter_destination_group = if_else(as.integer(to_group_count) == 1L, to_groups, NA_character_)
  )

event_ranges <- read_csv(
  "../output/historical_zoning_event_address_ranges.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(
    matter_id,
    range_start = as.integer(range_start),
    range_end = as.integer(range_end),
    street_direction,
    street_name,
    range_from_groups = from_groups,
    range_to_group = to_group
  )

event_geocodes <- read_csv(
  "../input/rezoning_parcel_matches_20101101_20201231.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(
    matter_id,
    matched_pin = normalize_pin(matched_pin),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  ) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  distinct() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(3435)

geocode_parent_evidence <- list()
for (parcel_year in names(annual_parent_polygons)) {
  year_parents <- annual_parent_polygons[[parcel_year]]
  year_matters <- matter_parsing %>%
    filter(as.integer(format(matter_passed_date, "%Y")) == as.integer(parcel_year)) %>%
    pull(matter_id)
  year_points <- event_geocodes %>%
    filter(matter_id %in% year_matters)
  if (nrow(year_points) == 0L) {
    next
  }
  parent_hits <- st_intersects(year_points, year_parents)
  for (i in seq_len(nrow(year_points))) {
    if (length(parent_hits[[i]]) == 0L) {
      next
    }
    geocode_parent_evidence[[length(geocode_parent_evidence) + 1L]] <- tibble(
      matter_id = year_points$matter_id[i],
      parent_pin = year_parents$parent_pin[parent_hits[[i]]],
      parcel_year = as.integer(parcel_year),
      geocode_in_parent_polygon = TRUE
    )
  }
}
geocode_parent_evidence <- bind_rows(geocode_parent_evidence) %>%
  distinct(matter_id, parent_pin, parcel_year, .keep_all = TRUE)

exact_pin_evidence <- event_geocodes %>%
  st_drop_geometry() %>%
  transmute(matter_id, parent_pin = matched_pin) %>%
  distinct()

lineage_evidence <- list()
for (i in seq_len(nrow(polygon_lineage))) {
  target <- targets %>%
    filter(
      validation == polygon_lineage$validation[i],
      target_pin == polygon_lineage$target_pin[i]
    )
  parent_address <- historical_addresses %>%
    filter(
      parent_pin == polygon_lineage$parent_pin[i],
      parcel_year == polygon_lineage$parcel_year[i]
    )
  possible_matters <- matter_parsing %>%
    filter(matter_passed_date == target$event_date) %>%
    filter(vapply(
      from_groups,
      function(groups) target$start_zone_group %in% str_split(coalesce(groups, ""), fixed(";"))[[1]],
      FUN.VALUE = logical(1)
    ))

  for (matter_row in seq_len(nrow(possible_matters))) {
    matter_id <- possible_matters$matter_id[matter_row]
    exact_pin <- any(
      exact_pin_evidence$matter_id == matter_id &
        exact_pin_evidence$parent_pin == polygon_lineage$parent_pin[i]
    )
    geocode_polygon <- any(
      geocode_parent_evidence$matter_id == matter_id &
        geocode_parent_evidence$parent_pin == polygon_lineage$parent_pin[i] &
        geocode_parent_evidence$parcel_year == polygon_lineage$parcel_year[i]
    )

    range_destination <- NA_character_
    if (nrow(parent_address) == 1L && !is.na(parent_address$house_number)) {
      possible_ranges <- event_ranges %>%
        filter(
          .data$matter_id == !!matter_id,
          street_direction == parent_address$street_direction,
          street_name == parent_address$street_name,
          parent_address$house_number >= range_start,
          parent_address$house_number <= range_end,
          range_start %% 2L != range_end %% 2L |
            parent_address$house_number %% 2L == range_start %% 2L
        )
      if (nrow(possible_ranges) > 0L) {
        origin_match <- vapply(
          possible_ranges$range_from_groups,
          function(groups) target$start_zone_group %in% str_split(coalesce(groups, ""), fixed(";"))[[1]],
          FUN.VALUE = logical(1)
        )
        destinations <- unique(possible_ranges$range_to_group[origin_match])
        destinations <- destinations[!is.na(destinations)]
        if (length(destinations) == 1L) {
          range_destination <- destinations
        }
      }
    }

    if (!exact_pin && !geocode_polygon && is.na(range_destination)) {
      next
    }
    destination <- coalesce(range_destination, possible_matters$matter_destination_group[matter_row])
    if (is.na(destination)) {
      next
    }

    lineage_evidence[[length(lineage_evidence) + 1L]] <- tibble(
      validation = target$validation,
      target_pin = target$target_pin,
      event_date = target$event_date,
      parent_pin = polygon_lineage$parent_pin[i],
      matter_id,
      selected_destination_group = destination,
      exact_event_pin = exact_pin,
      event_geocode_in_parent_polygon = geocode_polygon,
      historical_address_range = !is.na(range_destination),
      overlap_area_sqft = polygon_lineage$overlap_area_sqft[i],
      target_overlap_share = polygon_lineage$target_overlap_share[i],
      parent_overlap_share = polygon_lineage$parent_overlap_share[i]
    )
  }
}

lineage_evidence <- bind_rows(lineage_evidence)

address_bracket_evidence <- list()
for (i in seq_len(nrow(targets))) {
  target_parcels <- polygon_lineage %>%
    filter(
      validation == targets$validation[i],
      target_pin == targets$target_pin[i]
    ) %>%
    left_join(
      historical_addresses,
      by = c("parent_pin", "parcel_year"),
      relationship = "many-to-one"
    )
  if (nrow(target_parcels) < 2L) {
    next
  }

  possible_matters <- matter_parsing %>%
    filter(matter_passed_date == targets$event_date[i]) %>%
    filter(vapply(
      from_groups,
      function(groups) targets$start_zone_group[i] %in% str_split(coalesce(groups, ""), fixed(";"))[[1]],
      FUN.VALUE = logical(1)
    ))
  possible_ranges <- event_ranges %>%
    filter(matter_id %in% possible_matters$matter_id, !is.na(range_to_group))
  if (nrow(possible_ranges) == 0L) {
    next
  }
  possible_ranges <- possible_ranges[vapply(
    possible_ranges$range_from_groups,
    function(groups) targets$start_zone_group[i] %in% str_split(coalesce(groups, ""), fixed(";"))[[1]],
    FUN.VALUE = logical(1)
  ), ]

  for (range_row in seq_len(nrow(possible_ranges))) {
    parcel_pool <- target_parcels %>%
      filter(
        street_direction == possible_ranges$street_direction[range_row],
        street_name == possible_ranges$street_name[range_row],
        !is.na(house_number)
      )
    bracket_parcels <- bind_rows(
      parcel_pool %>%
        filter(house_number <= possible_ranges$range_start[range_row]) %>%
        arrange(desc(house_number), desc(overlap_area_sqft)) %>%
        slice(1L),
      parcel_pool %>%
        filter(house_number >= possible_ranges$range_end[range_row]) %>%
        arrange(house_number, desc(overlap_area_sqft)) %>%
        slice(1L)
    ) %>%
      distinct(parent_pin, .keep_all = TRUE)
    if (
      n_distinct(bracket_parcels$parent_pin) < 2L ||
        min(bracket_parcels$house_number) > possible_ranges$range_start[range_row] ||
        max(bracket_parcels$house_number) < possible_ranges$range_end[range_row]
    ) {
      next
    }

    address_bracket_evidence[[length(address_bracket_evidence) + 1L]] <- tibble(
      validation = targets$validation[i],
      target_pin = targets$target_pin[i],
      event_date = targets$event_date[i],
      parent_pin = paste(sort(bracket_parcels$parent_pin), collapse = ";"),
      matter_id = possible_ranges$matter_id[range_row],
      selected_destination_group = possible_ranges$range_to_group[range_row],
      exact_event_pin = FALSE,
      event_geocode_in_parent_polygon = FALSE,
      historical_address_range = TRUE,
      overlap_area_sqft = sum(bracket_parcels$overlap_area_sqft),
      target_overlap_share = sum(bracket_parcels$target_overlap_share),
      parent_overlap_share = max(bracket_parcels$parent_overlap_share)
    )
  }
}

lineage_evidence <- bind_rows(lineage_evidence, bind_rows(address_bracket_evidence)) %>%
  distinct(validation, target_pin, matter_id, selected_destination_group, .keep_all = TRUE)

selected_matters <- lineage_evidence %>%
  distinct(validation, target_pin, matter_id, selected_destination_group) %>%
  add_count(validation, target_pin, name = "evidence_matter_count") %>%
  filter(evidence_matter_count == 1L) %>%
  select(-evidence_matter_count)

reconciliation <- targets %>%
  left_join(
    st_drop_geometry(target_geometry_status) %>%
      select(validation, target_pin, target_geometry_found, target_geometry_year, target_geometry_source),
    by = c("validation", "target_pin"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    selected_matters,
    by = c("validation", "target_pin"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    polygon_lineage %>%
      distinct(validation, target_pin) %>%
      mutate(event_year_polygon_lineage = TRUE),
    by = c("validation", "target_pin"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    event_year_polygon_lineage = coalesce(event_year_polygon_lineage, FALSE),
    reconciliation_status = case_when(
      !target_geometry_found ~ "missing_target_polygon",
      is.na(matter_id) ~ "unresolved_or_ambiguous_event",
      selected_destination_group == end_zone_group ~ "reconciled_correct_destination",
      TRUE ~ "reconciled_wrong_destination"
    ),
    final_forward_status = case_when(
      strict_exact_assignment & event_year_polygon_lineage ~ "strict_exact_polygon_backed",
      strict_exact_assignment ~ "strict_exact_missing_polygon_lineage",
      reconciliation_status == "reconciled_correct_destination" ~ "polygon_recovered",
      TRUE ~ reconciliation_status
    )
  )

summary <- bind_rows(
  reconciliation %>% mutate(sample = "All density-project PINs"),
  reconciliation %>% filter(within_500ft) %>% mutate(sample = "Density-project PINs within 500ft"),
  reconciliation %>% filter(within_500ft, multifamily) %>% mutate(sample = "Multifamily PINs within 500ft")
) %>%
  summarise(
    date_consistent_group_changes = n(),
    target_polygon_assignments = sum(target_geometry_found),
    event_year_polygon_lineages = sum(event_year_polygon_lineage),
    strict_exact_assignments = sum(strict_exact_assignment),
    strict_exact_polygon_backed = sum(final_forward_status == "strict_exact_polygon_backed"),
    independent_polygon_event_assignments = sum(reconciliation_status == "reconciled_correct_destination"),
    polygon_recovered_assignments = sum(final_forward_status == "polygon_recovered"),
    final_exact_assignments = sum(final_forward_status %in% c(
      "strict_exact_polygon_backed",
      "polygon_recovered"
    )),
    final_exact_rate = final_exact_assignments / date_consistent_group_changes,
    unresolved_assignments = date_consistent_group_changes - final_exact_assignments,
    .by = c(validation, sample)
  )

write_csv(polygon_lineage, "../output/historical_zoning_polygon_lineage.csv", na = "")
write_csv(lineage_evidence, "../output/historical_zoning_polygon_event_evidence.csv", na = "")
write_csv(reconciliation, "../output/historical_zoning_polygon_reconciliation.csv", na = "")
write_csv(summary, "../output/historical_zoning_polygon_validation_summary.csv", na = "")
