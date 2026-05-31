# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_characteristics/code")
# bandwidth_ft <- 500

source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <bandwidth_ft>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))

rent <- read_parquet("../input/rent_with_ward_distances.parquet") %>%
  as_tibble()

if (!"rent_panel_id" %in% names(rent)) {
  stop("Rental input must include rent_panel_id.", call. = FALSE)
}
if (any(is.na(rent$rent_panel_id) | rent$rent_panel_id == "")) {
  stop("Rental input contains missing rent_panel_id values.", call. = FALSE)
}
if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Rental input must be unique by rent_panel_id.", call. = FALSE)
}
if (!all(c("longitude", "latitude") %in% names(rent))) {
  stop("Rental input must include longitude and latitude from the audited geometry task.", call. = FALSE)
}
if (!"signed_dist" %in% names(rent) && "signed_dist_m" %in% names(rent)) {
  rent <- rent %>% mutate(signed_dist = signed_dist_m / 0.3048)
}
if (!"signed_dist" %in% names(rent)) {
  stop("Rental input must include signed_dist in feet or signed_dist_m in meters.", call. = FALSE)
}

for (flag_col in c(
  "flag_location_questionable",
  "flag_modal_assignment_missing",
  "flag_modal_changes_ward",
  "flag_modal_changes_neighbor_ward",
  "flag_modal_changes_pair",
  "flag_modal_dist_diff_gt100ft",
  "flag_rd_location_questionable"
)) {
  if (!flag_col %in% names(rent)) {
    rent[[flag_col]] <- FALSE
  }
  rent[[flag_col]] <- coalesce(as.logical(rent[[flag_col]]), FALSE)
}

rent <- rent %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    right = as.integer(signed_dist_ft >= 0),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    is_multifamily = as.integer(building_type_clean == "multi_family"),
    is_single_family = as.integer(building_type_clean == "single_family"),
    is_condo = as.integer(building_type_clean == "condo"),
    is_townhouse = as.integer(building_type_clean == "townhouse"),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(is.finite(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    flag_clean_location_sample = !flag_location_questionable &
      !flag_modal_assignment_missing &
      !flag_modal_changes_ward &
      !flag_modal_changes_neighbor_ward &
      !flag_modal_changes_pair &
      !flag_modal_dist_diff_gt100ft,
    flag_no_modal_pair_change_sample = !flag_modal_assignment_missing & !flag_modal_changes_pair,
    flag_no_modal_ward_change_sample = !flag_modal_assignment_missing &
      !flag_modal_changes_ward &
      !flag_modal_changes_neighbor_ward,
    flag_no_questionable_address_sample = !flag_location_questionable
  ) %>%
  filter(
    !is.na(file_date),
    year >= 2014,
    year <= 2022,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= bandwidth_ft,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    is.finite(longitude),
    is.finite(latitude)
  )

if (nrow(rent) == 0) {
  stop("No rental observations remain in the RD window.", call. = FALSE)
}
if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("RD-window rental panel must remain unique by rent_panel_id.", call. = FALSE)
}

coords <- build_unique_coordinate_amenity_table(
  rent,
  "longitude",
  "latitude",
  "../input/schools_2015.gpkg",
  "../input/parks.gpkg",
  "../input/major_streets.gpkg",
  "../input/gis_osm_water_a_free_1.shp",
  chunk_n = 100000L,
  distance_units = "feet"
)

cta_stops <- read_amenity_layer("../input/cta_stops.gpkg") %>%
  mutate(
    station_id = as.character(station_id),
    active_from_date = as.Date(active_from_date),
    active_to_date = as.Date(active_to_date)
  )

if (!all(c("active_from_date", "active_to_date") %in% names(cta_stops))) {
  stop("CTA stop layer must include active_from_date and active_to_date.", call. = FALSE)
}
if (any(is.na(cta_stops$active_from_date))) {
  stop("CTA stop layer has missing active_from_date values.", call. = FALSE)
}

coords_month <- rent %>%
  distinct(longitude, latitude, year_month) %>%
  mutate(
    file_month_start = as.Date(paste0(year_month, "-01")),
    file_month_end = lubridate::ceiling_date(file_month_start, "month") - lubridate::days(1)
  )

coords_month_sf <- st_as_sf(
  coords_month,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)

cta_distances <- bind_rows(lapply(split(coords_month_sf, coords_month_sf$year_month), function(month_points) {
  month_start <- unique(month_points$file_month_start)
  month_end <- unique(month_points$file_month_end)
  if (length(month_start) != 1L || length(month_end) != 1L) {
    stop("CTA month split has non-unique month dates.", call. = FALSE)
  }

  active_cta <- cta_stops %>%
    filter(
      active_from_date <= month_end,
      is.na(active_to_date) | active_to_date >= month_start
    )
  if (nrow(active_cta) == 0) {
    stop(sprintf("No active CTA stations for %s.", unique(month_points$year_month)), call. = FALSE)
  }

  nearest_idx <- st_nearest_feature(month_points, active_cta)
  nearest_cta <- active_cta[nearest_idx, ]
  nearest_meta <- st_drop_geometry(nearest_cta)

  st_drop_geometry(month_points) %>%
    transmute(longitude, latitude, year_month) %>%
    mutate(
      nearest_cta_stop_dist_ft = as.numeric(st_distance(month_points, nearest_cta, by_element = TRUE)),
      nearest_cta_station_id = nearest_meta$station_id,
      nearest_cta_station_name = nearest_meta$longname,
      nearest_cta_lines = nearest_meta$lines
    )
}))

if (anyDuplicated(cta_distances[c("longitude", "latitude", "year_month")]) > 0) {
  stop("CTA distance table is not unique by coordinate-month.", call. = FALSE)
}

rent <- rent %>%
  left_join(coords, by = c("longitude", "latitude"), relationship = "many-to-one") %>%
  left_join(cta_distances, by = c("longitude", "latitude", "year_month"), relationship = "many-to-one") %>%
  mutate(
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
  )

if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Amenity join expanded rent_panel_id rows.", call. = FALSE)
}

write_parquet(
  as.data.frame(rent),
  sprintf("../output/rental_rd_characteristics_panel_bw%s.parquet", bandwidth_label)
)
