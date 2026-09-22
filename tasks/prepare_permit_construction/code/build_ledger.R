# setwd("tasks/prepare_permit_construction/code")
# timing <- "permit_issue"
# boundary_window_ft <- 1500
# main_boundary_window_ft <- 500
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/canonical_geometry_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(timing, boundary_window_ft, main_boundary_window_ft)
stopifnot(length(args) == 3L)
timing <- args[1]
boundary_window_ft <- as.numeric(args[2])
main_boundary_window_ft <- as.numeric(args[3])
stopifnot(timing %in% c("permit_issue", "assessor_year"))

# Permit timing uses the first permit's issue date; Assessor timing uses June 15 of the reported year built.
# Buildings dated after the last recorded ward map are outside the panel.
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) |> st_transform(3435)
buildings <- read_csv("../output/permit_buildings.csv", col_types = cols(building_id = "c", permit_number = "c",
    member_permit_numbers = "c", record_ids = "c", issue_date = "D", .default = col_guess())) |>
  filter(status == "measured", is.finite(latitude), is.finite(longitude)) |>
  mutate(construction_date = if (timing == "permit_issue") issue_date else as.Date(sprintf("%d-06-15", assessor_year_built)),
    construction_year = as.integer(format(construction_date, "%Y")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)) |>
  filter(canonical_map_year_for_era(era) %in% ward_panel$year)

# Each building sits at the centroid of its measured parcels; permit coordinates are geocoded at the street frontage.
# The permit point is used only when no measured parcel remains in the 2025 parcel universe.
centroids <- read_csv("../input/parcel_universe_2025_city.csv", col_types = cols(pin10 = "c", .default = "d"),
    col_select = c(pin10, centroid_x_crs_3435, centroid_y_crs_3435)) |>
  filter(is.finite(centroid_x_crs_3435), is.finite(centroid_y_crs_3435)) |>
  group_by(pin10) |> summarise(x = mean(centroid_x_crs_3435), y = mean(centroid_y_crs_3435), .groups = "drop")
building_centroids <- buildings |> select(building_id, record_ids) |> separate_longer_delim(record_ids, "/") |>
  mutate(pin10 = substr(record_ids, 1, 10)) |> distinct(building_id, pin10) |>
  inner_join(centroids, by = "pin10", relationship = "many-to-one") |>
  group_by(building_id) |> summarise(parcel_x = mean(x), parcel_y = mean(y), .groups = "drop")
permit_points <- st_coordinates(st_transform(st_as_sf(buildings, coords = c("longitude", "latitude"), crs = 4326), 3435))
buildings <- buildings |> mutate(permit_x = permit_points[, 1], permit_y = permit_points[, 2]) |>
  left_join(building_centroids, by = "building_id", relationship = "one-to-one") |>
  mutate(location_source = if_else(is.na(parcel_x), "permit_point", "parcel_centroid"),
    x_3435 = coalesce(parcel_x, permit_x), y_3435 = coalesce(parcel_y, permit_y))

# Ward and nearest ward-pair boundary from the map in effect on the construction date.
points <- st_as_sf(buildings, coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)
ward_maps <- load_canonical_ward_maps(ward_panel, eras = unique(points$era))
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = unique(points$era))
assignment <- assign_points_to_boundaries(points, points$era, ward_maps, boundary_lines, chunk_n = 2000L)

ledger <- bind_cols(st_drop_geometry(points), assignment) |>
  mutate(ward_pair = ward_pair_id, distance_to_boundary_ft = dist_ft,
    within_1500ft = dist_ft <= boundary_window_ft, within_500ft = dist_ft <= main_boundary_window_ft) |>
  select(building_id, permit_number, member_permit_numbers, source, record_ids, issue_date, assessor_year_built,
    construction_date, construction_year, boundary_year, era, ward, neighbor_ward, ward_pair, distance_to_boundary_ft,
    within_1500ft, within_500ft, location_source, x_3435, y_3435, dwelling_units, building_sqft, land_sqft, allow_far, allow_dupac,
    far, dupac, multifamily) |>
  arrange(construction_date, building_id)
SaveData(ledger, "building_id", sprintf("../output/permit_construction_%s.csv", timing), na = "")
