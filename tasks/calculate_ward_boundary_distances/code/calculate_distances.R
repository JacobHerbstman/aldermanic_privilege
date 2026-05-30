# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/calculate_ward_boundary_distances/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

synthetic_date_mode <- tolower(Sys.getenv("SYNTHETIC_CONSTRUCTION_DATE_MODE", "june15"))
max_construction_year <- suppressWarnings(as.integer(Sys.getenv("MAX_CONSTRUCTION_YEAR", "2026")))
max_construction_month <- Sys.getenv("MAX_CONSTRUCTION_MONTH", "2026-04")

if (!synthetic_date_mode %in% c("june15", "dynamic_turnover")) {
  stop("SYNTHETIC_CONSTRUCTION_DATE_MODE must be one of: june15, dynamic_turnover", call. = FALSE)
}
if (!is.finite(max_construction_year)) {
  stop("MAX_CONSTRUCTION_YEAR must be a valid integer year.", call. = FALSE)
}
if (!grepl("^\\d{4}-\\d{2}$", max_construction_month)) {
  stop("MAX_CONSTRUCTION_MONTH must use YYYY-MM format.", call. = FALSE)
}
max_construction_month_date <- as.Date(paste0(max_construction_month, "-15"))
if (year(max_construction_month_date) != max_construction_year) {
  stop("MAX_CONSTRUCTION_MONTH year must match MAX_CONSTRUCTION_YEAR.", call. = FALSE)
}
max_construction_month_num <- month(max_construction_month_date)

parcels <- st_read("../input/geocoded_residential_data.gpkg", quiet = TRUE) %>%
  filter(!is.na(yearbuilt), yearbuilt >= 1999 & yearbuilt <= max_construction_year) %>%
  mutate(
    parcel_row_id = row_number(),
    construction_date = if_else(
      yearbuilt == max_construction_year,
      max_construction_month_date,
      as.Date(paste0(yearbuilt, "-06-15"))
    )
  )

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
canonical_ward_maps <- load_canonical_ward_maps(ward_panel)

canonical_boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE)

ward_controls <- read_csv("../input/ward_controls.csv", show_col_types = FALSE)
ward_controls_max_year <- max(ward_controls$year, na.rm = TRUE)
if (!is.finite(ward_controls_max_year)) {
  stop("Ward controls have no valid year coverage.", call. = FALSE)
}

bg_controls <- read_csv("../input/block_group_controls.csv", show_col_types = FALSE)
bg_controls_max_year <- max(bg_controls$year, na.rm = TRUE)
if (!is.finite(bg_controls_max_year)) {
  stop("Block group controls have no valid year coverage.", call. = FALSE)
}

if (st_crs(parcels) != st_crs(ward_panel)) {
  parcels <- st_transform(parcels, st_crs(ward_panel))
}

zoning_data <- st_read("../input/zoning_data_clean.gpkg", quiet = TRUE)

if (st_crs(zoning_data) != st_crs(ward_panel)) {
  zoning_data <- st_transform(zoning_data, st_crs(ward_panel))
}

n_parcels_before_zoning <- nrow(parcels)
parcels <- parcels %>%
  st_join(
    zoning_data %>% dplyr::select(
      zone_code, floor_area_ratio, lot_area_per_unit, maximum_building_height
    ),
    left = TRUE, largest = TRUE
  )
if (nrow(parcels) != n_parcels_before_zoning) {
  stop("Zoning spatial join changed the parcel row count.", call. = FALSE)
}

block_groups <- st_read("../input/block_group_geometry_2019.gpkg", quiet = TRUE)

if (!("GEOID" %in% names(block_groups))) {
  stop("Block-group geometry input is missing GEOID.", call. = FALSE)
}
block_groups <- block_groups[, "GEOID", drop = FALSE]

geometry_column <- attr(block_groups, "sf_column")
if (is.null(geometry_column) || !(geometry_column %in% names(block_groups))) {
  stop("Block-group geometry input has no valid geometry column.", call. = FALSE)
}

if (nrow(block_groups) == 0) {
  stop("No block-group geometries found in ../input/block_group_geometry_2019.gpkg", call. = FALSE)
}
if (any(is.na(block_groups$GEOID) | block_groups$GEOID == "")) {
  stop("Block-group geometry input has missing GEOID values.", call. = FALSE)
}
if (any(duplicated(block_groups$GEOID))) {
  stop("Block-group geometry input GEOID values are not unique.", call. = FALSE)
}
if (any(st_is_empty(st_geometry(block_groups)))) {
  stop("Block-group geometry input contains empty geometries.", call. = FALSE)
}
if (st_crs(block_groups) != st_crs(parcels)) {
  block_groups <- st_transform(block_groups, st_crs(parcels))
}

n_parcels_before_bg <- nrow(parcels)
parcels <- parcels %>%
  st_join(block_groups, left = TRUE)
if (nrow(parcels) != n_parcels_before_bg) {
  stop("Block-group spatial join changed the parcel row count.", call. = FALSE)
}

alderman_lookup <- alderman_panel %>%
  select(ward, month, alderman) %>%
  mutate(
    month_yearmon = as.yearmon(month, format = "%b %Y"),
    year = year(as.Date(month_yearmon)),
    yearmon_key = as.character(month_yearmon)
  )
if (anyDuplicated(alderman_lookup[c("ward", "yearmon_key")]) > 0) {
  stop("Alderman panel has duplicate ward-month rows.", call. = FALSE)
}

alderman_tenure_lookup <- alderman_panel %>%
  mutate(month_yearmon = as.yearmon(month, format = "%b %Y")) %>%
  group_by(ward, alderman) %>%
  summarise(
    first_month = min(month_yearmon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(alderman_ward_key = paste(ward, alderman, sep = "_"))
if (anyDuplicated(alderman_tenure_lookup$alderman_ward_key) > 0) {
  stop("Alderman tenure lookup has duplicate ward-alderman rows.", call. = FALSE)
}

if (anyDuplicated(ward_controls[c("ward", "year")]) > 0) {
  stop("Ward controls have duplicate ward-year rows.", call. = FALSE)
}
if (anyDuplicated(bg_controls[c("GEOID", "year")]) > 0) {
  stop("Block-group controls have duplicate GEOID-year rows.", call. = FALSE)
}

if (synthetic_date_mode == "dynamic_turnover") {
  era_levels <- canonical_era_levels()

  assignment_by_era <- bind_rows(lapply(era_levels, function(era_i) {
    assign_points_to_boundaries(
      points_sf = parcels,
      era_values = rep(era_i, nrow(parcels)),
      ward_maps = canonical_ward_maps,
      boundary_lines = canonical_boundaries,
      chunk_n = 2000L
    ) %>%
      rename(
        assigned_ward = ward,
        ward_pair = ward_pair_id,
        dist_to_boundary = dist_ft
      ) %>%
      mutate(parcel_row_id = parcels$parcel_row_id, era = era_i)
  }))

  month_grid <- parcels %>%
    st_drop_geometry() %>%
    transmute(parcel_row_id, pin = as.character(pin), construction_year = yearbuilt) %>%
    tidyr::crossing(month_num = 1:12) %>%
    filter(construction_year < max_construction_year | month_num <= max_construction_month_num) %>%
    mutate(
      construction_date_candidate = as.Date(sprintf("%04d-%02d-15", construction_year, month_num)),
      construction_yearmon = as.yearmon(construction_date_candidate),
      yearmon_key = as.character(construction_yearmon),
      boundary_year = canonical_boundary_year_from_date(construction_date_candidate),
      era = canonical_era_from_boundary_year(boundary_year)
    ) %>%
    left_join(
      assignment_by_era %>%
        select(parcel_row_id, era, assigned_ward, ward_pair, dist_to_boundary),
      by = c("parcel_row_id", "era"),
      relationship = "many-to-one"
    ) %>%
    mutate(
      wards_in_pair = str_split_fixed(ward_pair, "_", 2),
      ward_a = suppressWarnings(as.integer(wards_in_pair[, 1])),
      ward_b = suppressWarnings(as.integer(wards_in_pair[, 2])),
      other_ward = if_else(
        !is.na(assigned_ward),
        if_else(assigned_ward == ward_a, ward_b, ward_a),
        NA_integer_
      )
    ) %>%
    left_join(
      alderman_lookup %>%
        select(ward, yearmon_key, alderman) %>%
        rename(alderman_own_candidate = alderman),
      by = c("assigned_ward" = "ward", "yearmon_key"),
      relationship = "many-to-one"
    ) %>%
    left_join(
      alderman_lookup %>%
        select(ward, yearmon_key, alderman) %>%
        rename(alderman_neighbor_candidate = alderman),
      by = c("other_ward" = "ward", "yearmon_key"),
      relationship = "many-to-one"
    )

  june_reference <- month_grid %>%
    filter(month_num == 6) %>%
    transmute(
      parcel_row_id,
      construction_date_june = construction_date_candidate,
      boundary_year_june = boundary_year,
      assigned_ward_june = assigned_ward,
      ward_pair_june = ward_pair,
      dist_to_boundary_june = dist_to_boundary,
      other_ward_june = other_ward,
      alderman_own_june = alderman_own_candidate,
      alderman_neighbor_june = alderman_neighbor_candidate
    )

  month_grid <- month_grid %>%
    left_join(june_reference, by = "parcel_row_id", relationship = "many-to-one") %>%
    mutate(
      changed_boundary_year = coalesce(boundary_year, -999L) != coalesce(boundary_year_june, -999L),
      changed_assigned_ward = coalesce(assigned_ward, -999L) != coalesce(assigned_ward_june, -999L),
      changed_other_ward = coalesce(other_ward, -999L) != coalesce(other_ward_june, -999L),
      changed_alderman_own = coalesce(alderman_own_candidate, "__NA__") != coalesce(alderman_own_june, "__NA__"),
      changed_alderman_neighbor = coalesce(alderman_neighbor_candidate, "__NA__") != coalesce(alderman_neighbor_june, "__NA__"),
      changed_any = changed_boundary_year |
        changed_assigned_ward |
        changed_other_ward |
        changed_alderman_own |
        changed_alderman_neighbor,
      month_distance = abs(month_num - 6L)
    )

  selected_months <- month_grid %>%
    arrange(parcel_row_id, desc(changed_any), month_distance, month_num) %>%
    group_by(parcel_row_id) %>%
    slice(1) %>%
    ungroup()

  parcels_with_distances <- parcels %>%
    select(-construction_date) %>%
    left_join(
      selected_months %>%
        select(
          parcel_row_id,
          construction_date = construction_date_candidate,
          boundary_year,
          era,
          assigned_ward,
          ward_pair,
          dist_to_boundary
        ),
      by = "parcel_row_id",
      relationship = "one-to-one"
    )
} else {
  parcels <- parcels %>%
    mutate(
      boundary_year = canonical_boundary_year_from_date(construction_date),
      era = canonical_era_from_boundary_year(boundary_year)
    )

  boundary_assignments <- assign_points_to_boundaries(
    points_sf = parcels,
    era_values = parcels$era,
    ward_maps = canonical_ward_maps,
    boundary_lines = canonical_boundaries,
    chunk_n = 2000L
  ) %>%
    rename(
      assigned_ward = ward,
      ward_pair = ward_pair_id,
      dist_to_boundary = dist_ft
    )

  parcels_with_distances <- bind_cols(parcels, boundary_assignments)
}

final_dataset <- parcels_with_distances %>%
  mutate(
    dist_to_boundary_m = as.numeric(dist_to_boundary) * 0.3048,
    construction_yearmon = as.yearmon(construction_date),
    yearmon_key = as.character(construction_yearmon)
  ) %>%
  left_join(alderman_lookup,
    by = c("assigned_ward" = "ward", "yearmon_key"),
    relationship = "many-to-one"
  ) %>%
  rename(alderman_own = alderman) %>%
  mutate(
    alderman_ward_key = paste(assigned_ward, alderman_own, sep = "_")
  ) %>%
  left_join(alderman_tenure_lookup,
    by = "alderman_ward_key",
    relationship = "many-to-one"
  ) %>%
  mutate(
    alderman_tenure_months = ifelse(
      !is.na(first_month) & !is.na(construction_yearmon),
      as.numeric((construction_yearmon - first_month) * 12),
      NA_real_
    )
  ) %>%
  select(
    pin, geom, GEOID,
    construction_year = yearbuilt, construction_date, boundary_year, dist_to_boundary, dist_to_boundary_m,
    ward = assigned_ward, ward_pair,
    alderman = alderman_own, alderman_tenure_months,
    arealotsf, areabuilding, bedroomscount, unitscount, storiescount, residential,
    zone_code, floor_area_ratio, lot_area_per_unit, maximum_building_height,
    yearmon_key
  )

final_dataset <- final_dataset %>%
  mutate(
    density_far = if_else(arealotsf > 0, areabuilding / arealotsf, NA_real_),
    density_lapu = if_else(unitscount > 0, arealotsf / unitscount, NA_real_),
    density_bcr = if_else(!is.na(storiescount) & storiescount > 0 & arealotsf > 0,
      (areabuilding / storiescount) / arealotsf, NA_real_
    ),
    density_lps = if_else(!is.na(storiescount) & storiescount > 0, arealotsf / storiescount, NA_real_),
    density_spu = if_else(unitscount > 0, areabuilding / unitscount, NA_real_),
    density_dupac = if_else(
      arealotsf > 0 & unitscount > 0,
      43560 * unitscount / arealotsf,
      NA_real_
    )
  )

st_write(final_dataset, "../output/parcels_with_geometry.gpkg", delete_dsn = TRUE, quiet = TRUE)

final_dataset <- as_tibble(st_drop_geometry(final_dataset)) %>%
  rename(alderman_own = alderman)

final_dataset_signed <- final_dataset %>%
  mutate(
    wards_in_pair = str_split_fixed(ward_pair, "_", 2),
    ward_a = as.integer(wards_in_pair[, 1]),
    ward_b = as.integer(wards_in_pair[, 2]),
    other_ward = if_else(ward == ward_a, ward_b, ward_a),
    ward_controls_year = pmin(construction_year, ward_controls_max_year),
    bg_controls_year = pmin(construction_year, bg_controls_max_year)
  ) %>%
  left_join(
    ward_controls,
    by = c("ward" = "ward", "ward_controls_year" = "year"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    ward_controls,
    by = c("other_ward" = "ward", "ward_controls_year" = "year"),
    suffix = c("_own", "_neighbor"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    alderman_lookup %>% rename(alderman_neighbor = alderman),
    by = c("other_ward" = "ward", "yearmon_key"),
    relationship = "many-to-one"
  ) %>%
  dplyr::select(-contains("wards_in_pair"), -ward_controls_year) %>%
  left_join(
    bg_controls %>%
      mutate(GEOID = as.character(GEOID)) %>%
      rename_with(~ paste0(., "_bg"), -c(GEOID, year)),
    by = c("GEOID", "bg_controls_year" = "year"),
    relationship = "many-to-one"
  ) %>%
  dplyr::select(-bg_controls_year)

boundary_year_checks <- final_dataset_signed %>%
  mutate(
    era = canonical_era_from_boundary_year(boundary_year),
    expected_boundary_year = canonical_boundary_year_from_date(construction_date),
    expected_era = canonical_era_from_boundary_year(expected_boundary_year),
    boundary_year_matches_construction_date = boundary_year == expected_boundary_year,
    era_matches_boundary_year = era == expected_era
  )
if (any(!boundary_year_checks$boundary_year_matches_construction_date, na.rm = TRUE) ||
    any(!boundary_year_checks$era_matches_boundary_year, na.rm = TRUE)) {
  stop("Parcel boundary-year convention check failed.", call. = FALSE)
}

write_csv(final_dataset_signed, "../output/parcels_pre_scores.csv")
