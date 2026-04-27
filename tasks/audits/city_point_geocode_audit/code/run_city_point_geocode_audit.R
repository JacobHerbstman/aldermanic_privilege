# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/city_point_geocode_audit/code")

source("../../../setup_environment/code/packages.R")

sf_use_s2(FALSE)

exact_hotspot_min_n <- 5L
rounded_hotspot_digits <- 5L
rounded_hotspot_min_n <- 10L

chicago_lat_min <- 41
chicago_lat_max <- 43
chicago_lon_min <- -89
chicago_lon_max <- -87

fill_coords_from_xy <- function(df) {
  needs_conversion <- (
    is.na(df$latitude) | is.na(df$longitude)
  ) &
    is.finite(df$x_coord) &
    is.finite(df$y_coord)

  if (!any(needs_conversion)) {
    return(df)
  }

  converted_sf <- st_as_sf(
    df[needs_conversion, c("x_coord", "y_coord")],
    coords = c("x_coord", "y_coord"),
    crs = 3435,
    remove = FALSE
  ) %>%
    st_transform(4326)

  converted_coords <- st_coordinates(converted_sf)
  df$longitude[needs_conversion] <- converted_coords[, "X"]
  df$latitude[needs_conversion] <- converted_coords[, "Y"]

  df
}

collapse_top_categories <- function(df) {
  if (!"category_value" %in% names(df)) {
    return(NA_character_)
  }

  if (!any(!is.na(df$category_value) & df$category_value != "")) {
    return(NA_character_)
  }

  df %>%
    mutate(category_value = if_else(
      is.na(category_value) | category_value == "",
      "<missing>",
      category_value
    )) %>%
    count(category_value, sort = TRUE, name = "n_records") %>%
    slice_head(n = 3) %>%
    mutate(label = paste0(category_value, " (", n_records, ")")) %>%
    pull(label) %>%
    paste(collapse = " | ")
}

build_hotspot_table <- function(points_df, dataset_key, dataset_name, row_count) {
  empty_hotspot_table <- tibble(
    dataset_key = character(),
    dataset_name = character(),
    hotspot_type = character(),
    latitude = numeric(),
    longitude = numeric(),
    n_records = integer(),
    share_of_dataset = numeric(),
    top_categories = character()
  )

  valid_points <- points_df %>%
    filter(is.finite(latitude), is.finite(longitude))

  if (nrow(valid_points) == 0) {
    return(empty_hotspot_table)
  }

  exact_counts <- valid_points %>%
    count(latitude, longitude, name = "n_records") %>%
    filter(n_records >= exact_hotspot_min_n)

  exact_hotspots <- tibble()
  if (nrow(exact_counts) > 0) {
    exact_categories <- valid_points %>%
      semi_join(exact_counts, by = c("latitude", "longitude")) %>%
      group_by(latitude, longitude) %>%
      reframe(top_categories = collapse_top_categories(pick(everything())))

    exact_hotspots <- exact_counts %>%
      left_join(exact_categories, by = c("latitude", "longitude")) %>%
      mutate(
        dataset_key = dataset_key,
        dataset_name = dataset_name,
        hotspot_type = "exact",
        share_of_dataset = n_records / row_count
      ) %>%
      select(dataset_key, dataset_name, hotspot_type, latitude, longitude, n_records, share_of_dataset, top_categories)
  }

  rounded_points <- valid_points %>%
    mutate(
      latitude_round = round(latitude, rounded_hotspot_digits),
      longitude_round = round(longitude, rounded_hotspot_digits)
    )

  rounded_counts <- rounded_points %>%
    count(latitude_round, longitude_round, name = "n_records") %>%
    filter(n_records >= rounded_hotspot_min_n)

  rounded_hotspots <- tibble()
  if (nrow(rounded_counts) > 0) {
    rounded_categories <- rounded_points %>%
      semi_join(rounded_counts, by = c("latitude_round", "longitude_round")) %>%
      group_by(latitude_round, longitude_round) %>%
      reframe(top_categories = collapse_top_categories(pick(everything())))

    rounded_hotspots <- rounded_counts %>%
      left_join(rounded_categories, by = c("latitude_round", "longitude_round")) %>%
      transmute(
        dataset_key = dataset_key,
        dataset_name = dataset_name,
        hotspot_type = "rounded_5dp",
        latitude = latitude_round,
        longitude = longitude_round,
        n_records = n_records,
        share_of_dataset = n_records / row_count,
        top_categories = top_categories
      )
  }

  bind_rows(empty_hotspot_table, exact_hotspots, rounded_hotspots) %>%
    arrange(dataset_key, desc(n_records), hotspot_type, latitude, longitude)
}

build_block_assignability <- function(
  permits_sf,
  blocks_sf,
  hotspot_lookup,
  block_vintage
) {
  permits_projected <- permits_sf %>%
    st_transform(3435)

  blocks_projected <- blocks_sf %>%
    st_transform(3435)

  joined <- st_join(
    permits_projected %>%
      select(permit_row, id, pin, permit_type, permit_status, high_discretion,
             permit_issued, application_year, issue_year, latitude, longitude),
    blocks_projected %>% select(block_id),
    join = st_within
  )

  joined_df <- joined %>%
    st_drop_geometry()

  duplicate_matches <- joined_df %>%
    count(permit_row, name = "n_matches") %>%
    filter(n_matches > 1)

  if (nrow(duplicate_matches) > 0) {
    stop(
      sprintf(
        "Found %d permits with multiple %s block matches in the audit task.",
        nrow(duplicate_matches),
        block_vintage
      ),
      call. = FALSE
    )
  }

  assignability <- joined_df %>%
    left_join(hotspot_lookup, by = c("latitude", "longitude")) %>%
    mutate(
      block_vintage = block_vintage,
      missing_pin_flag = is.na(pin) | pin == "",
      hotspot_total_n = coalesce(hotspot_total_n, 0L)
    )

  nearest_block_id <- rep(NA_character_, nrow(assignability))
  nearest_block_distance_m <- rep(NA_real_, nrow(assignability))

  needs_nearest <- which(
    is.na(assignability$block_id) &
      is.finite(assignability$latitude) &
      is.finite(assignability$longitude) &
      assignability$latitude >= chicago_lat_min &
      assignability$latitude <= chicago_lat_max &
      assignability$longitude >= chicago_lon_min &
      assignability$longitude <= chicago_lon_max
  )

  if (length(needs_nearest) > 0) {
    unmatched_sf <- permits_projected[match(assignability$permit_row[needs_nearest], permits_projected$permit_row), ]
    nearest_idx <- st_nearest_feature(unmatched_sf, blocks_projected)

    nearest_block_id[needs_nearest] <- blocks_projected$block_id[nearest_idx]
    nearest_block_distance_m[needs_nearest] <- as.numeric(st_distance(
      unmatched_sf,
      blocks_projected[nearest_idx, ],
      by_element = TRUE
    ))
  }

  assignability %>%
    mutate(
      nearest_block_id = nearest_block_id,
      nearest_block_distance_m = nearest_block_distance_m,
      reason_code = case_when(
        !is.finite(latitude) | !is.finite(longitude) ~ "missing_coordinate",
        latitude < chicago_lat_min |
          latitude > chicago_lat_max |
          longitude < chicago_lon_min |
          longitude > chicago_lon_max ~ "outside_broad_chicago_bbox",
        !is.na(block_id) ~ "matched_within_block",
        TRUE ~ "unmatched_non_hotspot"
      )
    ) %>%
    select(
      block_vintage, id, pin, permit_type, permit_status, permit_issued,
      high_discretion, application_year, issue_year, latitude, longitude,
      missing_pin_flag, block_id, nearest_block_id, nearest_block_distance_m,
      hotspot_total_n, reason_code
    ) %>%
    arrange(application_year, permit_type, id)
}

summarize_unmatched_clusters <- function(assignability_df, block_vintage_label) {
  assignability_df %>%
    filter(
      reason_code == "unmatched_non_hotspot",
      is.finite(latitude),
      is.finite(longitude)
    ) %>%
    mutate(
      latitude_round_3 = round(latitude, 3),
      longitude_round_3 = round(longitude, 3),
      permit_type = if_else(is.na(permit_type) | permit_type == "", "<missing>", permit_type)
    ) %>%
    group_by(block_vintage = block_vintage_label, latitude_round_3, longitude_round_3) %>%
    summarise(
      n_permits = n(),
      n_high_discretion = sum(high_discretion == 1, na.rm = TRUE),
      n_missing_pin = sum(missing_pin_flag, na.rm = TRUE),
      median_nearest_block_distance_m = median(nearest_block_distance_m, na.rm = TRUE),
      max_nearest_block_distance_m = max(nearest_block_distance_m, na.rm = TRUE),
      top_permit_types = paste(
        permit_type %>%
          table() %>%
          sort(decreasing = TRUE) %>%
          head(3) %>%
          names(),
        collapse = " | "
      ),
      .groups = "drop"
    ) %>%
    arrange(desc(n_permits), desc(n_high_discretion), latitude_round_3, longitude_round_3)
}

format_unmatched_cluster_line <- function(cluster_df, block_vintage_label) {
  if (nrow(cluster_df) == 0) {
    return(sprintf("- %s unmatched non-hotspot permits: none.", block_vintage_label))
  }

  top_clusters <- cluster_df %>%
    slice_head(n = 3) %>%
    mutate(cluster_label = sprintf("%.3f, %.3f (%s permits)", latitude_round_3, longitude_round_3, format(n_permits, big.mark = ","))) %>%
    pull(cluster_label)

  sprintf(
    "- %s unmatched non-hotspot permits: %s total, led by %s.",
    block_vintage_label,
    format(sum(cluster_df$n_permits), big.mark = ","),
    paste(top_clusters, collapse = "; ")
  )
}

extract_reason_total <- function(reason_summary_df, reason_code_value) {
  value <- reason_summary_df %>%
    filter(reason_code == reason_code_value) %>%
    pull(n_permits)

  if (length(value) == 0) {
    return(0L)
  }

  value[[1]]
}

message("Loading city boundary...")
city_boundary <- st_read("../input/city_boundary.geojson", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)
city_boundary_geom <- st_union(city_boundary)
city_boundary_projected <- st_transform(city_boundary_geom, 3435)

message("Loading raw building permits...")
raw_permits <- read_csv("../input/building_permits_raw.csv", show_col_types = FALSE) %>%
  mutate(
    latitude = suppressWarnings(as.numeric(LATITUDE)),
    longitude = suppressWarnings(as.numeric(LONGITUDE)),
    x_coord = suppressWarnings(as.numeric(XCOORDINATE)),
    y_coord = suppressWarnings(as.numeric(YCOORDINATE)),
    permit_type = as.character(PERMIT_TYPE),
    permit_status = as.character(PERMIT_STATUS),
    pin = as.character(PIN_LIST),
    application_date = as.Date(APPLICATION_START_DATE, format = "%m/%d/%Y"),
    application_year = suppressWarnings(as.integer(format(application_date, "%Y"))),
    permit_issued = case_when(
      permit_status %in% c("COMPLETE", "ACTIVE", "PHASED PERMITTING") ~ 1L,
      permit_status %in% c("EXPIRED", "CANCELLED", "REVOKED", "SUSPENDED") ~ 0L,
      TRUE ~ NA_integer_
    )
  )
raw_permits <- fill_coords_from_xy(raw_permits)

raw_permit_points <- raw_permits %>%
  transmute(
    row_id = row_number(),
    latitude,
    longitude,
    category_value = permit_type
  )

message("Loading cleaned building permits...")
clean_permits_sf <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  mutate(
    pin = as.character(pin),
    permit_type = as.character(permit_type),
    permit_status = as.character(permit_status),
    application_year = suppressWarnings(as.integer(format(as.Date(application_start_date_ym), "%Y"))),
    issue_year = suppressWarnings(as.integer(format(as.Date(issue_date_ym), "%Y")))
  )

clean_permits <- clean_permits_sf %>%
  st_drop_geometry()

clean_permit_points <- clean_permits %>%
  transmute(
    row_id = row_number(),
    latitude,
    longitude,
    category_value = permit_type
  )

message("Loading affordable rental housing...")
affordable_housing <- read_csv("../input/affordable_rental_housing.csv", show_col_types = FALSE) %>%
  mutate(
    latitude = suppressWarnings(as.numeric(Latitude)),
    longitude = suppressWarnings(as.numeric(Longitude)),
    x_coord = suppressWarnings(as.numeric(`X Coordinate`)),
    y_coord = suppressWarnings(as.numeric(`Y Coordinate`)),
    category_value = as.character(`Property Type`)
  )
affordable_housing <- fill_coords_from_xy(affordable_housing)

affordable_housing_points <- affordable_housing %>%
  transmute(
    row_id = row_number(),
    latitude,
    longitude,
    category_value
  )

message("Loading CPD facilities...")
cpd_facilities_sf <- st_read("../input/cpd_facilities.geojson", quiet = TRUE) %>%
  st_transform(4326)
cpd_facility_coords <- st_coordinates(cpd_facilities_sf)

cpd_facilities_points <- cpd_facilities_sf %>%
  st_drop_geometry() %>%
  transmute(
    row_id = row_number(),
    latitude = cpd_facility_coords[, "Y"],
    longitude = cpd_facility_coords[, "X"],
    category_value = as.character(facility_t)
  )

message("Loading CPS school locations...")
cps_schools_sf <- st_read("../input/cps_school_locations.geojson", quiet = TRUE) %>%
  st_transform(4326)
cps_school_coords <- st_coordinates(cps_schools_sf)

cps_schools_points <- cps_schools_sf %>%
  st_drop_geometry() %>%
  transmute(
    row_id = row_number(),
    latitude = cps_school_coords[, "Y"],
    longitude = cps_school_coords[, "X"],
    category_value = as.character(sch_type)
  )

dataset_inventory <- tibble(
  dataset_key = c(
    "building_permits_raw",
    "building_permits_clean",
    "affordable_rental_housing",
    "cpd_facilities",
    "cps_school_locations"
  ),
  dataset_name = c(
    "Building permits (raw city CSV)",
    "Building permits (current cleaned output)",
    "Affordable rental housing developments",
    "CPD facilities",
    "CPS school locations"
  ),
  source_file = c(
    "../input/building_permits_raw.csv",
    "../input/building_permits_clean.gpkg",
    "../input/affordable_rental_housing.csv",
    "../input/cpd_facilities.geojson",
    "../input/cps_school_locations.geojson"
  ),
  source_origin = c(
    "city_raw",
    "task_output",
    "city_raw",
    "city_raw",
    "city_raw"
  ),
  source_kind = c(
    "csv",
    "gpkg",
    "csv",
    "geojson",
    "geojson"
  ),
  geometry_type = c(
    "tabular_point_fields",
    "point_geometry",
    "tabular_point_fields",
    "POINT",
    "POINT"
  ),
  coordinate_mode = c(
    "latitude_longitude_with_xy_fallback",
    "point_geometry_with_latitude_longitude_columns",
    "latitude_longitude_with_xy_fallback",
    "point_geometry",
    "point_geometry"
  ),
  category_field = c(
    "PERMIT_TYPE",
    "permit_type",
    "Property Type",
    "facility_t",
    "sch_type"
  ),
  row_count = c(
    nrow(raw_permit_points),
    nrow(clean_permit_points),
    nrow(affordable_housing_points),
    nrow(cpd_facilities_points),
    nrow(cps_schools_points)
  )
)

audit_point_datasets <- list(
  building_permits_raw = raw_permit_points,
  building_permits_clean = clean_permit_points,
  affordable_rental_housing = affordable_housing_points,
  cpd_facilities = cpd_facilities_points,
  cps_school_locations = cps_schools_points
)

coordinate_quality_rows <- vector("list", nrow(dataset_inventory))
coordinate_hotspot_rows <- vector("list", nrow(dataset_inventory))

for (dataset_i in seq_len(nrow(dataset_inventory))) {
  dataset_key <- dataset_inventory$dataset_key[dataset_i]
  dataset_name <- dataset_inventory$dataset_name[dataset_i]
  points_df <- audit_point_datasets[[dataset_key]]
  row_count <- nrow(points_df)

  valid_points <- points_df %>%
    filter(is.finite(latitude), is.finite(longitude))

  bbox_failures <- valid_points %>%
    filter(
      latitude < chicago_lat_min |
        latitude > chicago_lat_max |
        longitude < chicago_lon_min |
        longitude > chicago_lon_max
    )

  city_boundary_failure_rows <- 0L
  if (nrow(valid_points) > 0) {
    valid_points_sf <- st_as_sf(valid_points, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
      st_transform(3435)
    within_city <- st_within(valid_points_sf, city_boundary_projected, sparse = FALSE)[, 1]
    city_boundary_failure_rows <- sum(!within_city)
  }

  hotspot_table <- build_hotspot_table(points_df, dataset_key, dataset_name, row_count)

  coordinate_quality_rows[[dataset_i]] <- tibble(
    dataset_key = dataset_key,
    dataset_name = dataset_name,
    row_count = row_count,
    valid_coordinate_rows = nrow(valid_points),
    missing_coordinate_rows = row_count - nrow(valid_points),
    zero_coordinate_rows = sum(valid_points$latitude == 0 | valid_points$longitude == 0),
    outside_broad_chicago_bbox_rows = nrow(bbox_failures),
    city_boundary_failure_rows = city_boundary_failure_rows,
    exact_hotspot_coordinate_pairs = sum(hotspot_table$hotspot_type == "exact"),
    exact_hotspot_rows = sum(hotspot_table$n_records[hotspot_table$hotspot_type == "exact"]),
    rounded_hotspot_coordinate_pairs = sum(hotspot_table$hotspot_type == "rounded_5dp"),
    rounded_hotspot_rows = sum(hotspot_table$n_records[hotspot_table$hotspot_type == "rounded_5dp"])
  )

  coordinate_hotspot_rows[[dataset_i]] <- hotspot_table
}

coordinate_quality_summary <- bind_rows(coordinate_quality_rows)
coordinate_hotspots <- bind_rows(coordinate_hotspot_rows)

write_csv(dataset_inventory, "../output/dataset_inventory.csv")
write_csv(coordinate_quality_summary, "../output/coordinate_quality_summary.csv")
write_csv(coordinate_hotspots, "../output/coordinate_hotspots.csv")

message("Building permit hotspot tables...")
raw_permit_exact_hotspots <- raw_permits %>%
  filter(is.finite(latitude), is.finite(longitude)) %>%
  count(latitude, longitude, name = "hotspot_total_n") %>%
  filter(hotspot_total_n >= exact_hotspot_min_n)

clean_permit_exact_hotspots <- clean_permits %>%
  filter(is.finite(latitude), is.finite(longitude)) %>%
  count(latitude, longitude, name = "hotspot_total_n") %>%
  filter(hotspot_total_n >= exact_hotspot_min_n)

raw_permit_hotspot_groups <- raw_permits %>%
  inner_join(raw_permit_exact_hotspots, by = c("latitude", "longitude")) %>%
  mutate(
    dataset_version = "raw",
    missing_pin_flag = is.na(pin) | pin == ""
  ) %>%
  count(
    dataset_version, latitude, longitude, hotspot_total_n,
    permit_type, permit_status, permit_issued, missing_pin_flag,
    application_year,
    name = "n_permits"
  )

clean_permit_hotspot_groups <- clean_permits %>%
  inner_join(clean_permit_exact_hotspots, by = c("latitude", "longitude")) %>%
  mutate(
    dataset_version = "cleaned",
    missing_pin_flag = is.na(pin) | pin == ""
  ) %>%
  count(
    dataset_version, latitude, longitude, hotspot_total_n,
    permit_type, permit_status, permit_issued, missing_pin_flag,
    application_year,
    name = "n_permits"
  )

permit_coordinate_hotspots <- bind_rows(
  raw_permit_hotspot_groups,
  clean_permit_hotspot_groups
) %>%
  arrange(dataset_version, desc(hotspot_total_n), latitude, longitude, desc(n_permits))

write_csv(permit_coordinate_hotspots, "../output/permit_coordinate_hotspots.csv")

message("Loading block geometries for permit assignability...")
clean_issued_permits_sf <- clean_permits_sf %>%
  filter(permit_issued == 1L) %>%
  mutate(permit_row = row_number())

blocks_2010 <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_make_valid() %>%
  st_transform(st_crs(clean_issued_permits_sf)) %>%
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id)) %>%
  distinct(block_id, .keep_all = TRUE)

blocks_2020 <- read_csv("../input/census_blocks_2020.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_make_valid() %>%
  st_transform(st_crs(clean_issued_permits_sf)) %>%
  rename(block_id = GEOID20) %>%
  mutate(block_id = as.character(block_id))

clean_hotspot_lookup <- clean_permit_exact_hotspots %>%
  select(latitude, longitude, hotspot_total_n)

permit_block_assignability_2010 <- build_block_assignability(
  permits_sf = clean_issued_permits_sf,
  blocks_sf = blocks_2010,
  hotspot_lookup = clean_hotspot_lookup,
  block_vintage = "2010"
)

permit_block_assignability_2020 <- build_block_assignability(
  permits_sf = clean_issued_permits_sf,
  blocks_sf = blocks_2020,
  hotspot_lookup = clean_hotspot_lookup,
  block_vintage = "2020"
)

write_csv(permit_block_assignability_2010, "../output/permit_block_assignability_2010.csv")
write_csv(permit_block_assignability_2020, "../output/permit_block_assignability_2020.csv")

permit_block_assignability_summary <- bind_rows(
  permit_block_assignability_2010,
  permit_block_assignability_2020
) %>%
  group_by(
    block_vintage, reason_code, permit_type, high_discretion,
    missing_pin_flag, application_year
  ) %>%
  summarise(
    n_permits = n(),
    median_nearest_block_distance_m = if (all(is.na(nearest_block_distance_m))) NA_real_ else median(nearest_block_distance_m, na.rm = TRUE),
    max_nearest_block_distance_m = if (all(is.na(nearest_block_distance_m))) NA_real_ else max(nearest_block_distance_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(block_vintage, reason_code, desc(n_permits), permit_type, application_year)

write_csv(
  permit_block_assignability_summary,
  "../output/permit_block_assignability_summary.csv"
)

permit_2010_reason_summary <- permit_block_assignability_2010 %>%
  count(reason_code, name = "n_permits") %>%
  arrange(desc(n_permits))

permit_2020_reason_summary <- permit_block_assignability_2020 %>%
  count(reason_code, name = "n_permits") %>%
  arrange(desc(n_permits))

permit_unmatched_clusters_2010 <- summarize_unmatched_clusters(
  permit_block_assignability_2010,
  "2010"
)

permit_unmatched_clusters_2020 <- summarize_unmatched_clusters(
  permit_block_assignability_2020,
  "2020"
)

clean_permit_boundary_failures <- coordinate_quality_summary %>%
  filter(dataset_key == "building_permits_clean") %>%
  pull(city_boundary_failure_rows)

other_city_summaries <- coordinate_quality_summary %>%
  filter(!dataset_key %in% c("building_permits_raw", "building_permits_clean")) %>%
  mutate(
    summary_line = sprintf(
      "- %s: %s rows, %s missing coords, %s bbox failures, %s city-boundary failures, %s exact-hotspot rows across %s coordinate pairs.",
      dataset_name,
      format(row_count, big.mark = ","),
      format(missing_coordinate_rows, big.mark = ","),
      format(outside_broad_chicago_bbox_rows, big.mark = ","),
      format(city_boundary_failure_rows, big.mark = ","),
      format(exact_hotspot_rows, big.mark = ","),
      format(exact_hotspot_coordinate_pairs, big.mark = ",")
    )
  ) %>%
  pull(summary_line)

other_city_recommendations <- coordinate_quality_summary %>%
  filter(!dataset_key %in% c("building_permits_raw", "building_permits_clean")) %>%
  mutate(
    action_class = case_when(
      outside_broad_chicago_bbox_rows > 0 | city_boundary_failure_rows > 0 ~ "upstream invalid-geocode flag",
      TRUE ~ "no action needed"
    ),
    action_line = sprintf(
      "- %s: %s.",
      dataset_name,
      action_class
    )
  ) %>%
  pull(action_line)

report_lines <- c(
  "# City Point-Geocode Audit",
  "",
  sprintf("Generated on %s.", Sys.Date()),
  "",
  "## Inventory",
  "",
  sprintf(
    "Audited %d city point-style datasets plus the current cleaned permit output.",
    nrow(dataset_inventory) - 1L
  ),
  "",
  "## Main Permit Findings",
  "",
  sprintf(
    "- 2010 block assignability: %s matched, %s unmatched.",
    format(extract_reason_total(permit_2010_reason_summary, "matched_within_block"), big.mark = ","),
    format(extract_reason_total(permit_2010_reason_summary, "unmatched_non_hotspot"), big.mark = ",")
  ),
  sprintf(
    "- 2020 block assignability: %s matched, %s unmatched.",
    format(extract_reason_total(permit_2020_reason_summary, "matched_within_block"), big.mark = ","),
    format(extract_reason_total(permit_2020_reason_summary, "unmatched_non_hotspot"), big.mark = ",")
  ),
  format_unmatched_cluster_line(permit_unmatched_clusters_2010, "2010"),
  format_unmatched_cluster_line(permit_unmatched_clusters_2020, "2020"),
  sprintf(
    "- Current cleaned permits still have %s city-boundary failures under the audit boundary check.",
    format(clean_permit_boundary_failures, big.mark = ",")
  ),
  sprintf(
    "- The main `create_event_study_permit_data` pipeline should remain paused for substantive use until this audit is reviewed."
  ),
  "",
  "## Other City Point Datasets",
  "",
  other_city_summaries,
  "",
  "## Recommended Next Actions",
  "",
  "- Building permits residual unmatched block-driver clusters: candidate for future reassignment robustness check.",
  "- Building permits any still-unmatched coordinates in block-based analyses: downstream task-local exclusion.",
  "- Building permits out-of-city or city-boundary-failing coordinates: upstream invalid-geocode flag.",
  other_city_recommendations
)

writeLines(report_lines, "../output/geocode_audit_report.md")
