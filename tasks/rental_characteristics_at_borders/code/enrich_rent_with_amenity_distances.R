source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# Rscript enrich_rent_with_amenity_distances.R ../input/rent_with_ward_distances.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../output/rent_with_ward_distances_amenities.parquet ../output/rental_amenity_distance_diagnostics.csv
# =======================================================================================

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 7) {
  input_parquet <- cli_args[1]
  schools_gpkg <- cli_args[2]
  parks_gpkg <- cli_args[3]
  major_streets_gpkg <- cli_args[4]
  water_shp <- cli_args[5]
  output_parquet <- cli_args[6]
  output_diag_csv <- cli_args[7]
} else {
  if (!exists("input_parquet") || !exists("schools_gpkg") || !exists("parks_gpkg") ||
      !exists("major_streets_gpkg") || !exists("water_shp") ||
      !exists("output_parquet") || !exists("output_diag_csv")) {
    stop(
      "FATAL: Script requires 7 args: <input_parquet> <schools_gpkg> <parks_gpkg> <major_streets_gpkg> <water_shp> <output_parquet> <output_diag_csv>",
      call. = FALSE
    )
  }
}

chunk_n <- 100000L

read_amenity_layer <- function(path) {
  st_read(path, quiet = TRUE) %>%
    st_zm(drop = TRUE, what = "ZM") %>%
    st_make_valid() %>%
    st_transform(3435)
}

nearest_distance_ft <- function(point_sf, target_sf, chunk_size = 100000L) {
  if (nrow(point_sf) == 0) {
    return(numeric())
  }

  out <- numeric(nrow(point_sf))
  starts <- seq(1L, nrow(point_sf), by = chunk_size)

  for (s in starts) {
    e <- min(s + chunk_size - 1L, nrow(point_sf))
    idx <- s:e
    if (nrow(target_sf) == 1L) {
      out[idx] <- as.numeric(st_distance(point_sf[idx, ], target_sf))
    } else {
      nearest_idx <- st_nearest_feature(point_sf[idx, ], target_sf)
      nearest_geom <- target_sf[nearest_idx, ]
      out[idx] <- as.numeric(st_distance(point_sf[idx, ], nearest_geom, by_element = TRUE))
    }
    message(sprintf("  Processed %s / %s points", format(e, big.mark = ","), format(nrow(point_sf), big.mark = ",")))
  }

  out
}

lake_michigan_geom <- function(path) {
  water <- st_read(path, quiet = TRUE) %>%
    st_zm(drop = TRUE, what = "ZM") %>%
    st_make_valid() %>%
    st_transform(3435)

  if (!"name" %in% names(water)) {
    stop("Lake water shapefile is missing the `name` column.", call. = FALSE)
  }

  lake <- water %>%
    filter(!is.na(name), str_to_lower(name) == "lake michigan")

  if (nrow(lake) == 0) {
    stop("Could not find Lake Michigan in the water shapefile.", call. = FALSE)
  }

  lake_boundary <- lake %>%
    st_union() %>%
    st_boundary() %>%
    st_simplify(dTolerance = 50)

  st_as_sf(tibble(name = "Lake Michigan"), geometry = st_sfc(lake_boundary, crs = st_crs(lake)))
}

message("Loading rental listings...")
rent <- read_parquet(input_parquet) %>% as_tibble()
message(sprintf("Listings loaded: %s", format(nrow(rent), big.mark = ",")))

message("Building unique coordinate table...")
coords <- rent %>%
  transmute(longitude, latitude) %>%
  filter(is.finite(longitude), is.finite(latitude)) %>%
  distinct()
message(sprintf("Unique coordinates: %s", format(nrow(coords), big.mark = ",")))

coords_sf <- st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(3435)

message("Loading amenity layers...")
schools <- read_amenity_layer(schools_gpkg)
parks <- read_amenity_layer(parks_gpkg)
major_streets <- read_amenity_layer(major_streets_gpkg)
lake <- lake_michigan_geom(water_shp)

message("Computing school distances...")
coords$nearest_school_dist_ft <- nearest_distance_ft(coords_sf, schools, chunk_n)

message("Computing park distances...")
coords$nearest_park_dist_ft <- nearest_distance_ft(coords_sf, parks, chunk_n)

message("Computing major-road distances...")
coords$nearest_major_road_dist_ft <- nearest_distance_ft(coords_sf, major_streets, chunk_n)

message("Computing lake distances...")
coords$lake_michigan_dist_ft <- nearest_distance_ft(coords_sf, lake, chunk_n)

rent_out <- as.data.table(rent)
setDT(coords)
setkey(rent_out, longitude, latitude)
setkey(coords, longitude, latitude)
rent_out <- coords[rent_out]

diagnostics <- bind_rows(
  tibble(
    metric = c(
      "nearest_school_dist_ft",
      "nearest_park_dist_ft",
      "nearest_major_road_dist_ft",
      "lake_michigan_dist_ft"
    )
  ) %>%
    rowwise() %>%
    mutate(
      n_listings = nrow(rent_out),
      n_unique_coords = nrow(coords),
      n_nonmissing = sum(!is.na(rent_out[[metric]])),
      share_nonmissing = mean(!is.na(rent_out[[metric]])),
      min_ft = min(rent_out[[metric]], na.rm = TRUE),
      p50_ft = median(rent_out[[metric]], na.rm = TRUE),
      p90_ft = quantile(rent_out[[metric]], 0.90, na.rm = TRUE),
      mean_ft = mean(rent_out[[metric]], na.rm = TRUE),
      max_ft = max(rent_out[[metric]], na.rm = TRUE)
    ) %>%
    ungroup(),
  tibble(
    metric = "metadata",
    n_listings = nrow(rent_out),
    n_unique_coords = nrow(coords),
    n_nonmissing = NA_real_,
    share_nonmissing = NA_real_,
    min_ft = NA_real_,
    p50_ft = NA_real_,
    p90_ft = NA_real_,
    mean_ft = NA_real_,
    max_ft = NA_real_
  )
)

write_parquet(as.data.frame(rent_out), output_parquet)
write_csv(diagnostics, output_diag_csv)

message(sprintf("Saved: %s", output_parquet))
message(sprintf("Saved: %s", output_diag_csv))
