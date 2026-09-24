# setwd("tasks/audits/footprint_floor_area_check/code")
# min_height_ft <- 15
# last_construction_year <- 2021
# large_building_units <- 20
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(min_height_ft, last_construction_year, large_building_units)
stopifnot(length(args) == 3L)
min_height_ft <- as.numeric(args[1])
last_construction_year <- as.integer(args[2])
large_building_units <- as.integer(args[3])

# Can footprint area times height stand in for floor area where the Assessor records none (condominium buildings of
# LARGE_BUILDING_UNITS or more)? Buildings built before the 2022 imagery are compared with the Assessor's floor area.
buildings <- read_csv("../input/permit_construction.csv", col_types = cols(building_id = "c", record_ids = "c",
    member_permit_numbers = "c", ward_pair = "c", .default = col_guess())) |>
  filter(construction_year <= last_construction_year, !is.na(record_ids), dwelling_units > 0, land_sqft > 0) |>
  mutate(kind = case_when(source == "condominium" ~ "condominium", multifamily %in% TRUE ~ "rental", TRUE ~ "single_family"),
    size = if_else(dwelling_units >= large_building_units, "large", "small"))

# Each building covers the footprints that contain one of its measured parcels' centroids. Structures lower than
# MIN_HEIGHT_FT (garages, sheds) are not buildings. A footprint reached by two buildings cannot be divided between
# them, so those buildings are not compared.
parcel_points <- buildings |> select(building_id, record_ids) |> separate_longer_delim(record_ids, "/") |>
  distinct(building_id, pin10 = substr(record_ids, 1, 10)) |>
  inner_join(read_csv("../input/parcel_centroids.csv", col_types = cols(pin10 = "c", .default = "d")) |>
    filter(is.finite(x_3435), is.finite(y_3435)) |> select(pin10, x_3435, y_3435), by = "pin10", relationship = "many-to-one") |>
  st_as_sf(coords = c("x_3435", "y_3435"), crs = 3435)
footprints <- st_read("../input/building_footprints_2022_chicago.gpkg", quiet = TRUE) |> filter(height_ft >= min_height_ft)
hits <- st_join(parcel_points, footprints |> select(object_id, footprint_sqft, height_ft), join = st_within, left = FALSE) |>
  st_drop_geometry() |> distinct(building_id, object_id, footprint_sqft, height_ft)
shared_footprints <- hits |> count(object_id) |> filter(n > 1)
matched <- hits |> group_by(building_id) |> filter(!any(object_id %in% shared_footprints$object_id)) |>
  summarise(footprints = n(), footprint_sqft = sum(footprint_sqft), volume_cuft = sum(footprint_sqft * height_ft),
    .groups = "drop")

# Floor area per cubic foot is calibrated on rentals with Assessor floor area. Large rentals are also calibrated on
# the other large rentals alone (leave one out), since large buildings are the ones to fill.
compared <- buildings |> inner_join(matched, by = "building_id", relationship = "one-to-one") |>
  mutate(building_sqft = if_else(building_sqft > 0, building_sqft, NA_real_), ratio = building_sqft / volume_cuft)
rental_ratio <- median(compared$ratio[compared$kind == "rental"], na.rm = TRUE)
large_rental_ratios <- compared$ratio[compared$kind == "rental" & compared$size == "large" & !is.na(compared$ratio)]
compared <- compared |>
  mutate(estimate_sqft = volume_cuft * rental_ratio,
    large_estimate_sqft = if_else(kind == "rental" & size == "large" & !is.na(ratio),
      volume_cuft * map_dbl(ratio, \(r) median(large_rental_ratios[-match(r, large_rental_ratios)])),
      volume_cuft * median(large_rental_ratios)),
    error = estimate_sqft / building_sqft - 1, large_error = large_estimate_sqft / building_sqft - 1,
    assessor_far = building_sqft / land_sqft, footprint_far = estimate_sqft / land_sqft)
SaveData(compared |> select(building_id, route, kind, size, dwelling_units, building_sqft, land_sqft, footprints,
    footprint_sqft, volume_cuft, estimate_sqft, large_estimate_sqft, error, large_error, assessor_far, footprint_far),
  "building_id", "../output/footprint_floor_area_buildings.csv")

# Errors by building type and size; the large-rental calibration applies to large buildings.
summary <- bind_rows(
    compared |> mutate(calibration = "all_rentals", e = error),
    compared |> filter(size == "large") |> mutate(calibration = "large_rentals", e = large_error)) |>
  group_by(calibration, kind, size) |>
  summarise(buildings = n(), with_assessor_floor_area = sum(!is.na(e)), median_error = median(e, na.rm = TRUE),
    median_abs_error = median(abs(e), na.rm = TRUE), within_10pct = mean(abs(e) <= 0.1, na.rm = TRUE),
    within_20pct = mean(abs(e) <= 0.2, na.rm = TRUE),
    log_far_correlation = if (sum(!is.na(e)) > 2) cor(log(assessor_far), log(footprint_far), use = "complete.obs") else NA_real_,
    median_footprint_far = median(footprint_far), .groups = "drop") |>
  mutate(floor_area_per_cuft = if_else(calibration == "all_rentals", rental_ratio, median(large_rental_ratios)))
SaveData(summary, c("calibration", "kind", "size"), "../output/footprint_floor_area_summary.csv")
