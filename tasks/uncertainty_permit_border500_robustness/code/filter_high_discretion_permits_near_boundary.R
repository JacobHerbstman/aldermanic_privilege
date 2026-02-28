source("../../setup_environment/code/packages.R")

threshold_ft <- 500

input_permits_path <- "../input/building_permits_clean_raw.gpkg"
input_ward_path <- "../input/ward_panel.gpkg"
output_filtered_permits_path <- "../output/building_permits_clean_border500_filtered.gpkg"
output_summary_path <- "../output/high_discretion_boundary_filter_summary.csv"
output_detail_path <- "../output/high_discretion_boundary_distances.csv"

message("=== Border-500 high-discretion permit filter ===")
message("Loading permits and ward geometries...")

permits_raw <- st_read(input_permits_path, quiet = TRUE)
ward_panel_raw <- st_read(input_ward_path, quiet = TRUE)

required_permit_cols <- c("id", "high_discretion", "application_start_date_ym")
missing_permit_cols <- setdiff(required_permit_cols, names(permits_raw))
if (length(missing_permit_cols) > 0) {
  stop(
    paste0(
      "Missing required permit columns: ",
      paste(missing_permit_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

required_ward_cols <- c("ward", "year")
missing_ward_cols <- setdiff(required_ward_cols, names(ward_panel_raw))
if (length(missing_ward_cols) > 0) {
  stop(
    paste0(
      "Missing required ward columns: ",
      paste(missing_ward_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

# Use a projected CRS in feet for consistent distance calculations.
distance_crs <- 3435
permits <- st_transform(permits_raw, distance_crs)
ward_panel <- st_transform(ward_panel_raw, distance_crs)

parse_yearmon_safe <- function(x) {
  if (inherits(x, "yearmon")) return(x)
  if (inherits(x, c("POSIXct", "POSIXt", "Date"))) {
    return(as.yearmon(as.Date(x)))
  }
  x_chr <- as.character(x)
  ym <- suppressWarnings(as.yearmon(x_chr))
  needs_date_parse <- is.na(ym)
  if (any(needs_date_parse)) {
    parsed_dates <- suppressWarnings(as.Date(x_chr[needs_date_parse]))
    ym[needs_date_parse] <- suppressWarnings(as.yearmon(parsed_dates))
  }
  ym
}

make_pair_id <- function(a, b) {
  a_int <- suppressWarnings(as.integer(a))
  b_int <- suppressWarnings(as.integer(b))
  out <- ifelse(
    !is.na(a_int) & !is.na(b_int),
    paste(pmin(a_int, b_int), pmax(a_int, b_int), sep = "-"),
    paste(
      pmin(as.character(a), as.character(b)),
      pmax(as.character(a), as.character(b)),
      sep = "-"
    )
  )
  out
}

build_map_polygons <- function(ward_sf, target_year) {
  out <- ward_sf %>%
    filter(year == target_year) %>%
    select(ward) %>%
    group_by(ward) %>%
    summarise(.groups = "drop")

  if (nrow(out) == 0) {
    stop(
      sprintf("No ward geometry rows found for target year %s.", target_year),
      call. = FALSE
    )
  }
  out
}

get_boundaries <- function(ward_sf) {
  ward_sf <- st_make_valid(ward_sf)
  adjacency <- st_touches(ward_sf)
  edge_list <- list()
  edge_idx <- 1L

  for (i in seq_along(adjacency)) {
    neighbors <- adjacency[[i]]
    neighbors <- neighbors[neighbors > i]
    if (length(neighbors) == 0) next

    geom_i <- st_geometry(ward_sf[i, ])
    for (j in neighbors) {
      geom_j <- st_geometry(ward_sf[j, ])
      shared <- suppressWarnings(st_intersection(st_boundary(geom_i), st_boundary(geom_j)))
      if (length(shared) == 0 || all(st_is_empty(shared))) next

      shared_lines <- suppressWarnings(st_collection_extract(shared, "LINESTRING"))
      if (length(shared_lines) == 0 || all(st_is_empty(shared_lines))) next
      shared_lines <- suppressWarnings(st_cast(shared_lines, "LINESTRING"))
      if (length(shared_lines) == 0) next

      shared_lines <- shared_lines[as.numeric(st_length(shared_lines)) > 0]
      if (length(shared_lines) == 0) next

      edge_list[[edge_idx]] <- st_sf(
        ward_a = ward_sf$ward[i],
        ward_b = ward_sf$ward[j],
        geometry = shared_lines
      )
      edge_idx <- edge_idx + 1L
    }
  }

  if (length(edge_list) == 0) {
    return(st_sf(
      ward_a = ward_sf$ward[0],
      ward_b = ward_sf$ward[0],
      geometry = st_sfc(crs = st_crs(ward_sf))
    ))
  }

  st_as_sf(bind_rows(edge_list), crs = st_crs(ward_sf))
}

calc_boundary_distance <- function(points_sf, ward_polys, ward_lines, map_period_label) {
  if (nrow(points_sf) == 0) {
    return(tibble())
  }

  joined <- st_join(points_sf %>% select(id), ward_polys %>% select(ward), join = st_within) %>%
    filter(!is.na(ward)) %>%
    group_by(id) %>%
    slice_head(n = 1) %>%
    ungroup()

  if (nrow(joined) == 0) {
    return(tibble())
  }

  joined <- joined %>%
    mutate(
      dist_to_boundary_ft = NA_real_,
      neighbor_ward = NA_character_,
      ward_pair_id = NA_character_,
      map_period = map_period_label
    )

  ward_values <- sort(unique(as.character(joined$ward)))

  for (w in ward_values) {
    idx <- which(as.character(joined$ward) == w)
    if (length(idx) == 0) next

    edges_w <- ward_lines %>%
      filter(as.character(ward_a) == w | as.character(ward_b) == w)
    if (nrow(edges_w) == 0) next

    nearest_idx <- st_nearest_feature(joined[idx, ], edges_w)
    nearest_edges <- edges_w[nearest_idx, ]
    dists <- st_distance(joined[idx, ], nearest_edges, by_element = TRUE)

    neighbor_vals <- ifelse(
      as.character(nearest_edges$ward_a) == w,
      as.character(nearest_edges$ward_b),
      as.character(nearest_edges$ward_a)
    )

    joined$dist_to_boundary_ft[idx] <- as.numeric(dists)
    joined$neighbor_ward[idx] <- neighbor_vals
    joined$ward_pair_id[idx] <- make_pair_id(w, neighbor_vals)
  }

  joined %>%
    st_drop_geometry() %>%
    transmute(
      id,
      map_period,
      ward_assigned = as.character(ward),
      neighbor_ward,
      ward_pair_id,
      dist_to_boundary_ft = as.numeric(dist_to_boundary_ft)
    )
}

ward_years <- sort(unique(as.integer(ward_panel$year)))
latest_year <- max(ward_years, na.rm = TRUE)

map_pre2015 <- build_map_polygons(ward_panel, 2014L)
map_2015_2023 <- build_map_polygons(ward_panel, 2016L)
map_post2023 <- build_map_polygons(ward_panel, latest_year)

lines_pre2015 <- get_boundaries(map_pre2015)
lines_2015_2023 <- get_boundaries(map_2015_2023)
lines_post2023 <- get_boundaries(map_post2023)

message(
  sprintf(
    "Boundary segments by era: pre_2015=%d, 2015_2023=%d, post_2023=%d",
    nrow(lines_pre2015), nrow(lines_2015_2023), nrow(lines_post2023)
  )
)

permits <- permits %>%
  mutate(application_month = parse_yearmon_safe(application_start_date_ym))

permits_high_discretion <- permits %>%
  filter(high_discretion == 1) %>%
  mutate(
    era = case_when(
      is.na(application_month) ~ "missing_application_month",
      application_month < as.yearmon("2015-05") ~ "pre_2015",
      application_month < as.yearmon("2023-05") ~ "2015_2023",
      TRUE ~ "post_2023"
    )
  )

message("High-discretion permits: ", nrow(permits_high_discretion))

dist_pre2015 <- calc_boundary_distance(
  permits_high_discretion %>% filter(era == "pre_2015"),
  map_pre2015,
  lines_pre2015,
  "pre_2015"
)
dist_2015_2023 <- calc_boundary_distance(
  permits_high_discretion %>% filter(era == "2015_2023"),
  map_2015_2023,
  lines_2015_2023,
  "2015_2023"
)
dist_post2023 <- calc_boundary_distance(
  permits_high_discretion %>% filter(era == "post_2023"),
  map_post2023,
  lines_post2023,
  "post_2023"
)

distance_details <- bind_rows(
  dist_pre2015,
  dist_2015_2023,
  dist_post2023
) %>%
  distinct(id, .keep_all = TRUE)

high_discretion_distance <- permits_high_discretion %>%
  st_drop_geometry() %>%
  select(id, era) %>%
  left_join(distance_details, by = "id") %>%
  mutate(
    boundary_distance_available = !is.na(dist_to_boundary_ft),
    drop_within_500ft = boundary_distance_available & dist_to_boundary_ft <= threshold_ft
  ) %>%
  mutate(
    map_period = coalesce(map_period, "not_assigned"),
    ward_assigned = coalesce(ward_assigned, "not_assigned"),
    neighbor_ward = coalesce(neighbor_ward, "not_assigned"),
    ward_pair_id = coalesce(ward_pair_id, "not_assigned")
  )

bad_drops <- high_discretion_distance %>%
  filter(drop_within_500ft, dist_to_boundary_ft > threshold_ft)
if (nrow(bad_drops) > 0) {
  stop("Found dropped permits with dist_to_boundary_ft > 500.", call. = FALSE)
}

drop_ids <- high_discretion_distance %>%
  filter(drop_within_500ft) %>%
  distinct(id) %>%
  pull(id)

permits_filtered <- permits_raw %>%
  filter(!(high_discretion == 1 & id %in% drop_ids))

safe_max_dropped <- function(distance_vec, drop_flag_vec) {
  vals <- distance_vec[drop_flag_vec & !is.na(distance_vec)]
  if (length(vals) == 0) return(NA_real_)
  max(vals)
}

summary_overall <- high_discretion_distance %>%
  summarise(
    era = "overall",
    n_total_permits_input = nrow(permits_raw),
    n_high_discretion = n(),
    n_with_distance = sum(boundary_distance_available),
    n_without_distance = sum(!boundary_distance_available),
    n_dropped_within_500ft = sum(drop_within_500ft),
    n_kept_high_discretion = n() - n_dropped_within_500ft,
    max_dropped_distance_ft = safe_max_dropped(dist_to_boundary_ft, drop_within_500ft)
  )

summary_by_era <- high_discretion_distance %>%
  group_by(era) %>%
  summarise(
    n_total_permits_input = nrow(permits_raw),
    n_high_discretion = n(),
    n_with_distance = sum(boundary_distance_available),
    n_without_distance = sum(!boundary_distance_available),
    n_dropped_within_500ft = sum(drop_within_500ft),
    n_kept_high_discretion = n() - n_dropped_within_500ft,
    max_dropped_distance_ft = safe_max_dropped(dist_to_boundary_ft, drop_within_500ft),
    .groups = "drop"
  )

summary_out <- bind_rows(summary_overall, summary_by_era)

write_csv(
  high_discretion_distance %>%
    select(
      id, era, map_period, ward_assigned, neighbor_ward, ward_pair_id,
      dist_to_boundary_ft, boundary_distance_available, drop_within_500ft
    ),
  output_detail_path
)

write_csv(summary_out, output_summary_path)

st_write(
  permits_filtered,
  output_filtered_permits_path,
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Dropped high-discretion permits within 500ft: ", length(drop_ids))
message("Filtered permits output rows: ", nrow(permits_filtered))
message("Saved filtered permits: ", output_filtered_permits_path)
message("Saved summary: ", output_summary_path)
message("Saved detail: ", output_detail_path)
