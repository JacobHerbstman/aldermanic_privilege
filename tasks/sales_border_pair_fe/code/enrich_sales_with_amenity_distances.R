# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")

chunk_n <- 100000L

sales <- read_parquet("../input/sales_with_hedonics.parquet") %>% as_tibble()
if (!"sale_date" %in% names(sales)) {
  stop("Sales input must include sale_date.", call. = FALSE)
}
sales <- sales %>% mutate(sale_date = as.Date(sale_date))
if (any(is.na(sales$sale_date))) {
  stop("Sales input has missing sale dates.", call. = FALSE)
}

coords <- build_unique_coordinate_amenity_table(
  sales,
  "longitude",
  "latitude",
  "../input/schools_2015.gpkg",
  "../input/parks.gpkg",
  "../input/major_streets.gpkg",
  "../input/gis_osm_water_a_free_1.shp",
  chunk_n
)

cta_stops <- read_amenity_layer("../input/cta_stops.gpkg") %>%
  mutate(
    active_from_date = as.Date(active_from_date),
    active_to_date = as.Date(active_to_date)
  )
if (!all(c("active_from_date", "active_to_date") %in% names(cta_stops))) {
  stop("CTA stop layer must include active_from_date and active_to_date.", call. = FALSE)
}
if (any(is.na(cta_stops$active_from_date))) {
  stop("CTA stop layer has missing active_from_date values.", call. = FALSE)
}

coords_date <- sales %>%
  distinct(longitude, latitude, sale_date)
coords_date_sf <- st_as_sf(
  coords_date,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)

cta_distance_rows <- list()
network_change_dates <- sort(unique(c(
  cta_stops$active_from_date,
  cta_stops$active_to_date + 1
)))
network_change_dates <- network_change_dates[!is.na(network_change_dates)]
network_group <- findInterval(
  as.numeric(coords_date_sf$sale_date),
  as.numeric(network_change_dates)
)
coords_by_network <- split(seq_len(nrow(coords_date_sf)), network_group)
for (network_i in names(coords_by_network)) {
  row_i <- coords_by_network[[network_i]]
  date_points <- coords_date_sf[row_i, ]
  sale_date_i <- date_points$sale_date[1]
  active_cta <- cta_stops %>%
    filter(
      active_from_date <= sale_date_i,
      is.na(active_to_date) | active_to_date >= sale_date_i
    )
  if (nrow(active_cta) == 0) {
    stop(sprintf("No active CTA stations on %s.", sale_date_i), call. = FALSE)
  }

  nearest_idx <- st_nearest_feature(date_points, active_cta)
  nearest_cta <- active_cta[nearest_idx, ]
  cta_distance_rows[[length(cta_distance_rows) + 1L]] <- st_drop_geometry(date_points) %>%
    transmute(
      longitude,
      latitude,
      sale_date,
      nearest_cta_stop_dist_ft = as.numeric(st_distance(date_points, nearest_cta, by_element = TRUE))
    )
}
cta_distances <- bind_rows(cta_distance_rows)
if (anyDuplicated(cta_distances[c("longitude", "latitude", "sale_date")]) > 0) {
  stop("CTA distance table must be unique by coordinate-date.", call. = FALSE)
}

sales_out <- append_amenity_distances(sales, coords, "longitude", "latitude") %>%
  left_join(
    cta_distances,
    by = c("longitude", "latitude", "sale_date"),
    relationship = "many-to-one"
  )
if (nrow(sales_out) != nrow(sales)) {
  stop("Sales amenity joins changed the row count.", call. = FALSE)
}
if (any(!is.finite(sales_out$nearest_cta_stop_dist_ft))) {
  stop("Sales have missing CTA distances.", call. = FALSE)
}

write_parquet(as.data.frame(sales_out), "../output/sales_with_hedonics_amenities.parquet")
