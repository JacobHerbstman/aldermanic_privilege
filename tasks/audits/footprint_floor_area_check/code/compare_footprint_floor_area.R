# setwd("tasks/audits/footprint_floor_area_check/code")
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

min_height_ft <- 15               # lower structures (garages, sheds) are not buildings
last_construction_year <- 2021L   # buildings finished before the 2022 imagery
size_breaks <- c(20L, 50L, 100L)  # unit counts splitting the size groups
min_building_height_ft <- 50      # the fill rule: mid-rise buildings
max_building_height_ft <- 100
min_lot_coverage <- 0.5           # whose footprints fit their lots
max_lot_coverage <- 1.2

# Can footprint area times height stand in for floor area where the Assessor records none (condominium buildings of
# the first of SIZE_BREAKS units or more)? Buildings built before the 2022 imagery are compared with the Assessor's
# floor area, by size in units split at SIZE_BREAKS.
buildings <- read_csv("../input/permit_construction.csv", col_types = cols(building_id = "c", record_ids = "c",
    member_permit_numbers = "c", ward_pair = "c", .default = col_guess())) |>
  filter(construction_year <= last_construction_year, !is.na(record_ids), dwelling_units > 0, land_sqft > 0) |>
  mutate(kind = case_when(source == "condominium" ~ "condominium", multifamily %in% TRUE ~ "rental", TRUE ~ "single_family"),
    large = dwelling_units >= size_breaks[1],
    size = as.character(cut(dwelling_units, c(0, size_breaks - 1, Inf),
      labels = c(paste0("under_", size_breaks[1]), paste0(head(size_breaks, -1), "_", size_breaks[-1] - 1),
        paste0(tail(size_breaks, 1), "_plus")))))

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
  summarise(footprints = n(), volume_cuft = sum(footprint_sqft * height_ft), footprint_sqft = sum(footprint_sqft),
    .groups = "drop")

# Only Assessor floor area is compared; the construction data fill some condominiums from these footprints.
# Floor area per cubic foot is calibrated on rentals with Assessor floor area, and on large rentals passing the fill
# rule (leave one out for those rentals themselves), since large buildings are the ones to fill.
compared <- buildings |> inner_join(matched, by = "building_id", relationship = "one-to-one") |>
  mutate(building_sqft = if_else(floor_area_source %in% "assessor" & building_sqft > 0, building_sqft, NA_real_),
    ratio = building_sqft / volume_cuft,
    # The construction data fill floor area only for mid-rise buildings whose footprints fit their lots.
    fill_rule = between(volume_cuft / footprint_sqft, min_building_height_ft, max_building_height_ft) &
      between(footprint_sqft / land_sqft, min_lot_coverage, max_lot_coverage))
rental_ratio <- median(compared$ratio[compared$kind == "rental"], na.rm = TRUE)
large_rental_ratios <- compared$ratio[compared$kind == "rental" & compared$large & compared$fill_rule & !is.na(compared$ratio)]
compared <- compared |>
  mutate(estimate_sqft = volume_cuft * rental_ratio,
    large_estimate_sqft = if_else(kind == "rental" & large & fill_rule & !is.na(ratio),
      volume_cuft * map_dbl(ratio, \(r) median(large_rental_ratios[-match(r, large_rental_ratios)])),
      volume_cuft * median(large_rental_ratios)),
    error = estimate_sqft / building_sqft - 1, large_error = large_estimate_sqft / building_sqft - 1,
    assessor_far = building_sqft / land_sqft, footprint_far = estimate_sqft / land_sqft)
SaveData(compared |> select(building_id, route, kind, size, large, fill_rule, dwelling_units, building_sqft, land_sqft, footprints,
    footprint_sqft, volume_cuft, estimate_sqft, large_estimate_sqft, error, large_error, assessor_far, footprint_far),
  "building_id", "../output/footprint_floor_area_buildings.csv")

# Errors by building type, size and fill rule; the large-rental calibration (large rentals passing the fill rule)
# applies to buildings passing and failing it.
summary <- bind_rows(
    compared |> mutate(calibration = "all_rentals", e = error),
    compared |> mutate(calibration = "large_rentals", e = large_error)) |>
  group_by(calibration, kind, size, fill_rule) |>
  summarise(buildings = n(), with_assessor_floor_area = sum(!is.na(e)), median_error = median(e, na.rm = TRUE),
    median_abs_error = median(abs(e), na.rm = TRUE), within_10pct = mean(abs(e) <= 0.1, na.rm = TRUE),
    within_20pct = mean(abs(e) <= 0.2, na.rm = TRUE),
    log_far_correlation = if (sum(!is.na(e)) > 2) cor(log(assessor_far), log(footprint_far), use = "complete.obs") else NA_real_,
    median_footprint_far = median(footprint_far), .groups = "drop") |>
  mutate(floor_area_per_cuft = if_else(calibration == "all_rentals", rental_ratio, median(large_rental_ratios)))
SaveData(summary, c("calibration", "kind", "size", "fill_rule"), "../output/footprint_floor_area_summary.csv")
