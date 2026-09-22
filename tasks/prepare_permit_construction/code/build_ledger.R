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

# Permit timing uses the first permit's issue date; Assessor timing, and Assessor-only buildings, use June 15 of the
# reported year built. Buildings dated after the last recorded ward map are outside the panel.
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) |> st_transform(3435)
buildings <- read_csv("../output/construction_buildings.csv", col_types = cols(building_id = "c", permit_number = "c",
    member_permit_numbers = "c", record_ids = "c", issue_date = "D", .default = col_guess())) |>
  filter(status %in% c("measured", "measured_without_permit"), is.finite(x_3435), is.finite(y_3435)) |>
  mutate(date_source = if_else(timing == "permit_issue" & !is.na(issue_date), "permit_issue_date", "assessor_year_built"),
    construction_date = if_else(date_source == "permit_issue_date", issue_date, make_date(assessor_year_built, 6L, 15L)),
    construction_year = as.integer(format(construction_date, "%Y")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)) |>
  filter(canonical_map_year_for_era(era) %in% ward_panel$year)

# Ward and nearest ward-pair boundary from the map in effect on the construction date.
points <- st_as_sf(buildings, coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)
ward_maps <- load_canonical_ward_maps(ward_panel, eras = unique(points$era))
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = unique(points$era))
assignment <- assign_points_to_boundaries(points, points$era, ward_maps, boundary_lines, chunk_n = 2000L)

ledger <- bind_cols(st_drop_geometry(points), assignment) |>
  mutate(ward_pair = ward_pair_id, distance_to_boundary_ft = dist_ft,
    within_1500ft = dist_ft <= boundary_window_ft, within_500ft = dist_ft <= main_boundary_window_ft) |>
  select(building_id, route, permit_number, member_permit_numbers, source, record_ids, issue_date, assessor_year_built,
    date_source, construction_date, construction_year, boundary_year, era, ward, neighbor_ward, ward_pair, distance_to_boundary_ft,
    within_1500ft, within_500ft, location_source, x_3435, y_3435, dwelling_units, building_sqft, land_sqft, allow_far, allow_dupac,
    far, dupac, multifamily) |>
  arrange(construction_date, building_id)
SaveData(ledger, "building_id", sprintf("../output/permit_construction_%s.csv", timing), na = "")
