# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

recovered <- read_csv(
  "../output/density_parcel_address_unlocated_recovery.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  filter(
    address_recovery_status ==
      "recover_unique_address_confirmed_by_parcel_history"
  ) %>%
  mutate(
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year),
    yearmon_key = as.character(as.yearmon(construction_date)),
    density_far = if_else(arealotsf > 0, areabuilding / arealotsf, NA_real_),
    density_dupac = if_else(
      arealotsf > 0 & unitscount > 0,
      43560 * unitscount / arealotsf,
      NA_real_
    ),
    sample_source = "recovered_address_coordinate",
    coordinate_rule = "historical_address_to_unique_current_location",
    project_key = paste0("address_", pin)
  )

if (nrow(recovered) == 0) {
  stop("No unique address-recovered buildings are available.", call. = FALSE)
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
segment_layers <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg")

assignments <- assign_points_to_boundaries(
  points_sf = recovered_sf,
  era_values = recovered_sf$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 2000L
)

recovered_sf <- bind_cols(recovered_sf, assignments) %>%
  rename(
    other_ward = neighbor_ward,
    ward_pair = ward_pair_id,
    dist_to_boundary_m = dist_m,
    dist_to_boundary = dist_ft
  )

recovered_sf$segment_id <- assign_points_to_nearest_segments(
  points_sf = recovered_sf,
  era_values = recovered_sf$era,
  pair_values = recovered_sf$ward_pair,
  segment_layers = segment_layers,
  max_distance = units::set_units(457.2, "m"),
  chunk_n = 50000L
)

zoning <- st_read("../input/zoning_data_clean.gpkg", quiet = TRUE) %>%
  select(zone_code) %>%
  st_make_valid() %>%
  st_transform(3435)
zoning_join <- recovered_sf %>%
  select(pin) %>%
  st_transform(st_crs(zoning)) %>%
  st_join(zoning, left = TRUE, largest = TRUE) %>%
  st_drop_geometry()

if (nrow(zoning_join) != nrow(recovered_sf) || anyDuplicated(zoning_join$pin) > 0) {
  stop("Zoning join changed address-recovered row cardinality.", call. = FALSE)
}

alderman_panel <- read_csv(
  "../input/chicago_alderman_panel.csv",
  show_col_types = FALSE
)
alderman_lookup <- alderman_panel %>%
  transmute(
    ward = as.integer(ward),
    yearmon_key = as.character(as.yearmon(month, format = "%b %Y")),
    alderman
  )
if (anyDuplicated(alderman_lookup[c("ward", "yearmon_key")]) > 0) {
  stop("Alderman lookup is not unique by ward-month.", call. = FALSE)
}

ward_controls <- read_csv(
  "../input/ward_controls_2000_2023.csv",
  show_col_types = FALSE
)
if (anyDuplicated(ward_controls[c("ward", "year")]) > 0) {
  stop("Ward controls are not unique by ward-year.", call. = FALSE)
}

scores <- read_csv(
  "../input/alderman_uncertainty_index_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index)
if (anyDuplicated(scores$alderman) > 0) {
  stop("Score lookup is not unique by alderman.", call. = FALSE)
}

recovered_sf %>%
  st_drop_geometry() %>%
  left_join(zoning_join, by = "pin", relationship = "one-to-one") %>%
  left_join(
    alderman_lookup,
    by = c("ward", "yearmon_key"),
    relationship = "many-to-one"
  ) %>%
  rename(alderman_own = alderman) %>%
  left_join(
    alderman_lookup %>% rename(alderman_neighbor = alderman),
    by = c("other_ward" = "ward", "yearmon_key"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    ward_controls,
    by = c("ward", "construction_year" = "year"),
    relationship = "many-to-one"
  ) %>%
  rename_with(
    ~ paste0(.x, "_own"),
    all_of(setdiff(names(ward_controls), c("ward", "year")))
  ) %>%
  left_join(
    scores,
    by = c("alderman_own" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(strictness_own = score) %>%
  left_join(
    scores,
    by = c("alderman_neighbor" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_distance_m = dist_to_boundary_m * sign,
    signed_distance = dist_to_boundary * sign
  ) %>%
  arrange(pin) %>%
  write_csv("../output/density_parcel_address_recovered_model_rows.csv")
