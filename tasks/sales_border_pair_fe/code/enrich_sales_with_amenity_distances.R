# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")

chunk_n <- 100000L

sales <- read_parquet("../input/sales_with_hedonics.parquet") %>% as_tibble()
if (!"year_month" %in% names(sales)) {
  stop("Sales input must include year_month.", call. = FALSE)
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

coords_month <- sales %>%
  distinct(longitude, latitude, year_month) %>%
  mutate(
    month_start = as.Date(paste0(year_month, "-01")),
    month_end = lubridate::ceiling_date(month_start, "month") - lubridate::days(1)
  )
coords_month_sf <- st_as_sf(
  coords_month,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)

cta_distance_rows <- list()
coords_by_month <- split(coords_month_sf, coords_month_sf$year_month)
for (month_i in names(coords_by_month)) {
  month_points <- coords_by_month[[month_i]]
  month_start <- unique(month_points$month_start)
  month_end <- unique(month_points$month_end)
  if (length(month_start) != 1L || length(month_end) != 1L) {
    stop("CTA month split has non-unique month dates.", call. = FALSE)
  }

  active_cta <- cta_stops %>%
    filter(
      active_from_date <= month_end,
      is.na(active_to_date) | active_to_date >= month_start
    )
  if (nrow(active_cta) == 0) {
    stop(sprintf("No active CTA stations for %s.", month_i), call. = FALSE)
  }

  nearest_idx <- st_nearest_feature(month_points, active_cta)
  nearest_cta <- active_cta[nearest_idx, ]
  cta_distance_rows[[length(cta_distance_rows) + 1L]] <- st_drop_geometry(month_points) %>%
    transmute(
      longitude,
      latitude,
      year_month,
      nearest_cta_stop_dist_ft = as.numeric(st_distance(month_points, nearest_cta, by_element = TRUE))
    )
}
cta_distances <- bind_rows(cta_distance_rows)
if (anyDuplicated(cta_distances[c("longitude", "latitude", "year_month")]) > 0) {
  stop("CTA distance table must be unique by coordinate-month.", call. = FALSE)
}

sales_out <- append_amenity_distances(sales, coords, "longitude", "latitude") %>%
  left_join(
    cta_distances,
    by = c("longitude", "latitude", "year_month"),
    relationship = "many-to-one"
  )
if (nrow(sales_out) != nrow(sales)) {
  stop("Sales amenity joins changed the row count.", call. = FALSE)
}
if (any(!is.finite(sales_out$nearest_cta_stop_dist_ft))) {
  stop("Sales have missing CTA distances.", call. = FALSE)
}

write_parquet(as.data.frame(sales_out), "../output/sales_with_hedonics_amenities.parquet")
