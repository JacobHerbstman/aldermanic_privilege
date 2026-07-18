# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

segment_radius_m <- 457.2

segment_layers <- load_segment_line_layers(
  "../input/boundary_segments_1320ft.gpkg"
)
zoning <- st_read(
  "../input/zoning_data_clean.gpkg",
  quiet = TRUE
) %>%
  select(fresh_zone_code = zone_code) %>%
  st_make_valid() %>%
  st_transform(3435)

assign_placebo_segments <- function(points) {
  points$segment_id <- assign_points_to_nearest_segments(
    points_sf = points,
    era_values = canonical_era_from_boundary_year(points$boundary_year),
    pair_values = normalize_pair_dash(points$ward_pair),
    segment_layers = segment_layers,
    max_distance = units::set_units(segment_radius_m, "m"),
    chunk_n = 50000L
  )
  points
}

assign_fresh_zoning <- function(points) {
  joined <- points %>%
    st_transform(3435) %>%
    st_join(zoning, left = TRUE, largest = TRUE)
  if (nrow(joined) != nrow(points) || anyDuplicated(joined$pin) > 0) {
    stop("Fresh zoning assignment changed extended-sample row cardinality.", call. = FALSE)
  }
  joined
}

current_attributes <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    .default = col_guess()
  )
) %>%
  filter(dist_to_boundary_m <= segment_radius_m) %>%
  rename(production_segment_id = segment_id)

current_geometry <- st_read(
  "../input/parcels_with_geometry.gpkg",
  quiet = TRUE
) %>%
  mutate(pin = as.character(pin)) %>%
  select(pin)

if (anyDuplicated(current_geometry$pin) > 0) {
  stop("Current parcel geometry is not unique by PIN.", call. = FALSE)
}

current <- current_geometry %>%
  inner_join(current_attributes, by = "pin", relationship = "one-to-one")
if (nrow(current) != nrow(current_attributes)) {
  stop("A current-coordinate density row is missing its geometry.", call. = FALSE)
}
current <- assign_placebo_segments(current) %>%
  assign_fresh_zoning() %>%
  mutate(placebo_coordinate_source = "current_parcel")

lineage_pins <- read_csv(
  "../input/density_project_lineage.csv",
  show_col_types = FALSE,
  col_types = cols(member_pins = col_character(), .default = col_guess())
) %>%
  left_join(
    read_csv(
      "../input/density_parcel_address_lineage_evidence.csv",
      show_col_types = FALSE
    ) %>%
      select(project_key, address_audit_recommendation),
    by = "project_key",
    relationship = "one-to-one"
  ) %>%
  filter(
    recommended_action == "candidate_for_recovery",
    address_audit_recommendation == "candidate_for_recovery"
  ) %>%
  select(project_key, member_pins) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  rename(pin = member_pins)

if (anyDuplicated(lineage_pins$pin) > 0) {
  stop("A historical PIN belongs to multiple retained lineage projects.", call. = FALSE)
}

historical <- read_csv(
  "../input/density_historical_recovered_exact_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    source_class = col_character(),
    .default = col_guess()
  )
) %>%
  inner_join(lineage_pins, by = "pin", relationship = "one-to-one") %>%
  filter(dist_to_boundary_m <= segment_radius_m) %>%
  rename(production_segment_id = segment_id)

historical <- st_as_sf(
  historical,
  coords = c("centroid_x_crs_3435", "centroid_y_crs_3435"),
  crs = 3435,
  remove = FALSE
) %>%
  assign_placebo_segments() %>%
  assign_fresh_zoning() %>%
  mutate(placebo_coordinate_source = "historical_parcel")

address <- read_csv(
  "../input/density_parcel_address_recovered_model_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    source_class = col_character(),
    .default = col_guess()
  )
) %>%
  filter(dist_to_boundary_m <= segment_radius_m) %>%
  rename(production_segment_id = segment_id)

address <- st_as_sf(
  address,
  coords = c("address_x_crs_3435", "address_y_crs_3435"),
  crs = 3435,
  remove = FALSE
) %>%
  assign_placebo_segments() %>%
  assign_fresh_zoning() %>%
  mutate(placebo_coordinate_source = "historical_address")

placebo_sample <- bind_rows(
  st_drop_geometry(current),
  st_drop_geometry(historical),
  st_drop_geometry(address)
)

if (anyDuplicated(placebo_sample$pin) > 0) {
  stop("The extended density placebo sample contains duplicate PINs.", call. = FALSE)
}

placebo_sample %>%
  arrange(pin) %>%
  write_csv("../output/density_placebo_1000ft_sample.csv")

placebo_sample %>%
  mutate(
    distance_band = case_when(
      dist_to_boundary_m <= 152.4 ~ "0-500ft",
      dist_to_boundary_m <= 304.8 ~ "500-1000ft",
      TRUE ~ "1000-1500ft"
    ),
    segment_assigned = !is.na(segment_id) & segment_id != ""
  ) %>%
  count(placebo_coordinate_source, distance_band, segment_assigned) %>%
  arrange(placebo_coordinate_source, distance_band, desc(segment_assigned)) %>%
  write_csv("../output/density_placebo_1000ft_sample_summary.csv")
