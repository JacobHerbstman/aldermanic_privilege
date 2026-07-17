# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

recovered <- read_csv(
  "../output/density_parcel_address_unlocated_recovery.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), source_class = col_character(), parcel_class = col_character(),
    current_address_pins = col_character(), closest_source_candidate_pin = col_character(),
    .default = col_guess()
  )
) %>%
  filter(
    address_recovery_status ==
      "recover_unique_address_confirmed_by_parcel_history"
  ) %>%
  mutate(
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )

if (nrow(recovered) == 0) {
  stop("No unique address-recovered buildings are available for boundary assignment.", call. = FALSE)
}
if (anyDuplicated(recovered$pin) > 0) {
  stop("Address-recovered buildings are not unique by original PIN.", call. = FALSE)
}

recovered_sf <- st_as_sf(
  recovered,
  coords = c("address_x_crs_3435", "address_y_crs_3435"),
  crs = 3435,
  remove = FALSE
)

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

assignments <- assign_points_to_boundaries(
  points_sf = recovered_sf,
  era_values = recovered_sf$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 2000L
)

bind_cols(st_drop_geometry(recovered_sf), assignments) %>%
  transmute(
    pin,
    construction_year,
    unitscount,
    multifamily,
    selected_address,
    address_x_crs_3435,
    address_y_crs_3435,
    ward,
    other_ward = neighbor_ward,
    ward_pair = ward_pair_id,
    dist_to_boundary_m = dist_m,
    dist_to_boundary_ft = dist_ft,
    within_500ft = dist_m <= 152.4
  ) %>%
  arrange(dist_to_boundary_m, pin) %>%
  write_csv("../output/density_parcel_address_recovered_boundaries.csv")
