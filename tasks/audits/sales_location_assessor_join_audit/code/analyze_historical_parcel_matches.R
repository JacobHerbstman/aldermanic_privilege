# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_location_assessor_join_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

suppressMessages(sf_use_s2(FALSE))

fallback_cases <- fread(
  "../output/sales_coordinate_fallback_cases.csv",
  colClasses = list(character = c("row_id", "pin_norm", "sale_date"))
)
historical_parcels <- fread(
  "../output/historical_parcel_records.csv",
  colClasses = list(character = c("pin"))
)
if (anyDuplicated(historical_parcels[, .(pin, year)]) > 0) {
  stop("Historical parcel records must be unique by PIN-year.", call. = FALSE)
}

historical_parcels[, coordinate_key := fifelse(
  is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435),
  sprintf("%.6f|%.6f", centroid_x_crs_3435, centroid_y_crs_3435),
  NA_character_
)]
pin_coordinate_counts <- historical_parcels[, .(
  n_historical_years = .N,
  n_historical_coordinates = uniqueN(coordinate_key[!is.na(coordinate_key)])
), by = pin]

historical_exact <- historical_parcels[, .(
  pin_norm = pin,
  year,
  longitude,
  latitude,
  centroid_x_crs_3435,
  centroid_y_crs_3435
)]
matched_cases <- merge(
  fallback_cases,
  historical_exact,
  by = c("pin_norm", "year"),
  all.x = TRUE,
  sort = FALSE
)
if (nrow(matched_cases) != nrow(fallback_cases)) {
  stop("Historical exact-year parcel join changed the number of missing sales.", call. = FALSE)
}
matched_cases <- merge(
  matched_cases,
  pin_coordinate_counts,
  by.x = "pin_norm",
  by.y = "pin",
  all.x = TRUE,
  sort = FALSE
)
matched_cases[, exact_year_coordinate :=
  is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435)]
matched_cases[, sale_date_use := as.Date(substr(sale_date, 1L, 10L))]
matched_cases[, era := canonical_era_from_date(sale_date_use, allow_pre_2003 = TRUE)]
matched_cases[, `:=`(
  ward = NA_integer_,
  neighbor_ward = NA_integer_,
  ward_pair_id = NA_character_,
  dist_m = NA_real_,
  dist_ft = NA_real_,
  segment_id = NA_character_,
  alderman_own = NA_character_,
  alderman_neighbor = NA_character_,
  strictness_own = NA_real_,
  strictness_neighbor = NA_real_
)]

recovered_idx <- which(matched_cases$exact_year_coordinate)
if (length(recovered_idx) > 0) {
  recovered_points <- st_as_sf(
    matched_cases[recovered_idx, .(
      row_id,
      centroid_x_crs_3435,
      centroid_y_crs_3435
    )],
    coords = c("centroid_x_crs_3435", "centroid_y_crs_3435"),
    crs = 3435,
    remove = FALSE
  )

  ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
    st_transform(3435)
  ward_maps <- load_canonical_ward_maps(ward_panel)
  boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")
  boundary_assignment <- assign_points_to_boundaries(
    points_sf = recovered_points,
    era_values = matched_cases$era[recovered_idx],
    ward_maps = ward_maps,
    boundary_lines = boundary_lines,
    chunk_n = 5000L
  )

  matched_cases[recovered_idx, `:=`(
    ward = boundary_assignment$ward,
    neighbor_ward = boundary_assignment$neighbor_ward,
    ward_pair_id = boundary_assignment$ward_pair_id,
    dist_m = boundary_assignment$dist_m,
    dist_ft = boundary_assignment$dist_ft
  )]

  segment_layers <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg")
  matched_cases[recovered_idx, segment_id := assign_points_to_nearest_segments(
    points_sf = recovered_points,
    era_values = matched_cases$era[recovered_idx],
    pair_values = matched_cases$ward_pair_id[recovered_idx],
    segment_layers = segment_layers,
    max_distance = units::set_units(457.2, "m"),
    chunk_n = 50000L
  )]
}

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month)) %>%
  select(ward, month, alderman) %>%
  distinct()
if (anyDuplicated(alderman_panel[, c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}
scores <- read_csv("../input/aldermen_uncertainty_scores_through2022.csv", show_col_types = FALSE) %>%
  select(alderman, strictness = uncertainty_index)
if (anyDuplicated(scores$alderman) > 0) {
  stop("Through-2022 score input must be unique by alderman.", call. = FALSE)
}

matched_cases[, month_join := as.yearmon(sale_date_use)]
matched_cases <- as.data.table(matched_cases %>%
  left_join(
    alderman_panel,
    by = c("ward" = "ward", "month_join" = "month"),
    relationship = "many-to-one"
  ) %>%
  rename(alderman_own_join = alderman) %>%
  left_join(
    alderman_panel,
    by = c("neighbor_ward" = "ward", "month_join" = "month"),
    relationship = "many-to-one"
  ) %>%
  rename(alderman_neighbor_join = alderman) %>%
  left_join(
    scores,
    by = c("alderman_own_join" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(strictness_own_join = strictness) %>%
  left_join(
    scores,
    by = c("alderman_neighbor_join" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(strictness_neighbor_join = strictness))

matched_cases[, `:=`(
  alderman_own = alderman_own_join,
  alderman_neighbor = alderman_neighbor_join,
  strictness_own = strictness_own_join,
  strictness_neighbor = strictness_neighbor_join
)]
matched_cases[, rd_geometry_complete :=
  exact_year_coordinate &
    is.finite(dist_ft) & dist_ft <= 500 &
    !is.na(segment_id) & segment_id != "" &
    !is.na(strictness_own) & !is.na(strictness_neighbor) &
    strictness_own != strictness_neighbor]

historical_match_summary <- data.table(
  measure = c(
    "missing_current_universe_sales",
    "exact_sale_year_coordinate_recovered",
    "no_exact_sale_year_coordinate",
    "recovered_pin_has_multiple_historical_coordinates",
    "recovered_within_500ft_of_ward_boundary",
    "recovered_with_complete_500ft_rd_geometry"
  ),
  n = c(
    nrow(matched_cases),
    matched_cases[exact_year_coordinate == TRUE, .N],
    matched_cases[exact_year_coordinate == FALSE, .N],
    matched_cases[exact_year_coordinate == TRUE & n_historical_coordinates > 1L, .N],
    matched_cases[exact_year_coordinate == TRUE & is.finite(dist_ft) & dist_ft <= 500, .N],
    matched_cases[rd_geometry_complete == TRUE, .N]
  )
)
historical_match_summary[, share_of_missing_sales := n / nrow(matched_cases)]

historical_match_cases <- matched_cases[, .(
  row_id,
  sale_date,
  year,
  pin_norm,
  exact_year_coordinate,
  n_historical_years,
  n_historical_coordinates,
  longitude,
  latitude,
  ward,
  neighbor_ward,
  ward_pair_id,
  dist_ft,
  segment_id,
  alderman_own,
  alderman_neighbor,
  strictness_own,
  strictness_neighbor,
  rd_geometry_complete
)]
setorder(historical_match_cases, -rd_geometry_complete, dist_ft, year, pin_norm)

fwrite(historical_match_summary, "../output/historical_parcel_match_summary.csv")
fwrite(historical_match_cases, "../output/historical_parcel_match_cases.csv")
