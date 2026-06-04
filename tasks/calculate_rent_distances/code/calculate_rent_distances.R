# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/calculate_rent_distances/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

crs_projected <- 3435

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  st_transform(crs_projected)
canonical_ward_maps <- load_canonical_ward_maps(ward_panel)
canonical_boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))
alderman_lookup <- alderman_panel %>%
  select(ward, month, alderman) %>%
  distinct()
if (anyDuplicated(alderman_lookup[, c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}

ds <- arrow::open_dataset("../input/chicago_rent_panel.parquet")
quality_ds <- arrow::open_dataset("../input/chicago_rent_panel_quality_flags.parquet")
quality_cols <- c(
  "rent_panel_id",
  "geometry_latitude",
  "geometry_longitude",
  "modal_latitude",
  "modal_longitude",
  "flag_geometry_uses_address_location_correction",
  "flag_location_questionable",
  "location_quality_status",
  "quality_flag_severity"
)
missing_quality_cols <- setdiff(quality_cols, names(quality_ds))
if (length(missing_quality_cols) > 0) {
  stop(
    sprintf("RentHub quality flags missing required columns: %s.", paste(missing_quality_cols, collapse = ", ")),
    call. = FALSE
  )
}

years <- 2014:2022
results_list <- list()

for (i in seq_along(years)) {
  yr <- years[i]

  df_chunk <- ds %>%
    filter(year(file_date) == yr) %>%
    select(
      any_of(c(
        "rent_panel_id", "episode_id", "analysis_key", "key_source", "property_id",
        "unit_id", "property_key", "floorplan_key", "address_norm", "address_missing",
        "month_start", "first_observed_date", "first_observed_rent", "active_days",
        "distinct_daily_rents", "multi_rent_days", "same_rent_repeat_days",
        "raw_rows_month", "episode_start_date", "episode_end_date",
        "episode_obs_days", "episode_n_rent_values", "episode_median_rent"
      )),
      id, rent_price, building_type, any_of("building_type_clean"),
      beds, baths, sqft, laundry, gym, any_of(c("doorman", "furnished", "pool")),
      year_built, available_date, file_date, latitude, longitude
    ) %>%
    collect()

  if (nrow(df_chunk) > 0) {
    quality_chunk <- quality_ds %>%
      filter(year == yr) %>%
      select(all_of(quality_cols)) %>%
      collect()
    if (anyDuplicated(quality_chunk$rent_panel_id) > 0) {
      stop(sprintf("Quality flags are not unique by rent_panel_id for year %d.", yr), call. = FALSE)
    }
    df_chunk <- df_chunk %>%
      left_join(quality_chunk, by = "rent_panel_id", relationship = "many-to-one")
    n_missing_quality <- sum(is.na(df_chunk$quality_flag_severity))
    if (n_missing_quality > 0) {
      stop(sprintf(
        "Missing RentHub quality flags for %d panel rows in year %d.",
        n_missing_quality,
        yr
      ), call. = FALSE)
    }
  }

  if (nrow(df_chunk) > 0) {
    df_chunk <- df_chunk %>%
      mutate(
        renthub_latitude = latitude,
        renthub_longitude = longitude,
        geometry_latitude = coalesce(geometry_latitude, latitude),
        geometry_longitude = coalesce(geometry_longitude, longitude),
        flag_geometry_uses_address_location_correction = coalesce(
          flag_geometry_uses_address_location_correction,
          FALSE
        )
      ) %>%
      filter(is.finite(geometry_latitude), is.finite(geometry_longitude), !is.na(file_date))

    if (nrow(df_chunk) > 0) {
      pts <- st_as_sf(df_chunk, coords = c("geometry_longitude", "geometry_latitude"), crs = 4326) %>%
        st_transform(crs_projected) %>%
        mutate(
          boundary_year = canonical_boundary_year_from_date(file_date),
          era = canonical_era_from_date(file_date, allow_pre_2003 = FALSE)
        )

      boundary_assignments <- assign_points_to_boundaries(
        points_sf = pts,
        era_values = pts$era,
        ward_maps = canonical_ward_maps,
        boundary_lines = canonical_boundaries,
        chunk_n = 5000L
      )

      processed_chunk <- bind_cols(pts, boundary_assignments) %>%
        filter(!is.na(ward), !is.na(ward_pair_id))

      if (nrow(processed_chunk) > 0) {
        results_list[[length(results_list) + 1]] <- processed_chunk
      }
    }
  }

  invisible(gc())
}

if (length(results_list) == 0) {
  stop("No rental observations received a ward-pair assignment.", call. = FALSE)
}
results_sf <- bind_rows(results_list)

final_df <- results_sf %>%
  st_transform(4326) %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  as_tibble()

if (!"rent_panel_id" %in% names(final_df)) {
  stop("Rental output must include rent_panel_id.", call. = FALSE)
}
if (any(is.na(final_df$rent_panel_id) | final_df$rent_panel_id == "")) {
  stop("Rental output contains missing rent_panel_id values.", call. = FALSE)
}
if (anyDuplicated(final_df$rent_panel_id) > 0) {
  stop("Rental output must be unique by rent_panel_id.", call. = FALSE)
}
if (max(final_df$file_date, na.rm = TRUE) > as.Date("2022-12-31")) {
  stop("Rental distance task is scoped to 2014-2022 but found post-2022 rows.", call. = FALSE)
}
if (any(final_df$boundary_year == 2024L, na.rm = TRUE) || any(final_df$era == "post_2023", na.rm = TRUE)) {
  stop("Rental distance task should not contain post-2023 boundary assignments.", call. = FALSE)
}

modal_base <- final_df %>%
  filter(
    is.finite(dist_m),
    dist_m <= 500 * 0.3048,
    is.finite(modal_longitude),
    is.finite(modal_latitude)
  ) %>%
  select(
    rent_panel_id, file_date, ward, neighbor_ward, ward_pair_id, dist_m,
    longitude, latitude, modal_longitude, modal_latitude
  )

if (nrow(modal_base) > 0) {
  modal_pts <- st_as_sf(
    modal_base,
    coords = c("modal_longitude", "modal_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(crs_projected) %>%
    mutate(era = canonical_era_from_date(file_date, allow_pre_2003 = FALSE))

  modal_assignments <- assign_points_to_boundaries(
    points_sf = modal_pts,
    era_values = modal_pts$era,
    ward_maps = canonical_ward_maps,
    boundary_lines = canonical_boundaries,
    chunk_n = 5000L
  ) %>%
    rename(
      modal_ward = ward,
      modal_neighbor_ward = neighbor_ward,
      modal_ward_pair_id = ward_pair_id,
      modal_dist_m = dist_m,
      modal_dist_ft = dist_ft
    )

  modal_sensitivity <- bind_cols(st_drop_geometry(modal_pts), modal_assignments) %>%
    mutate(
      raw_ward_pair_dash = normalize_pair_dash(ward_pair_id),
      modal_ward_pair_dash = normalize_pair_dash(modal_ward_pair_id),
      raw_dist_ft = dist_m / 0.3048,
      flag_modal_assignment_missing = is.na(modal_ward) | is.na(modal_ward_pair_id),
      flag_modal_changes_ward = !flag_modal_assignment_missing & ward != modal_ward,
      flag_modal_changes_neighbor_ward = !flag_modal_assignment_missing & neighbor_ward != modal_neighbor_ward,
      flag_modal_changes_pair = !flag_modal_assignment_missing & raw_ward_pair_dash != modal_ward_pair_dash,
      flag_modal_dist_diff_gt100ft = is.finite(modal_dist_ft) & abs(modal_dist_ft - raw_dist_ft) > 100
    ) %>%
    select(
      rent_panel_id, file_date, ward, neighbor_ward, ward_pair_id,
      modal_ward, modal_neighbor_ward, modal_ward_pair_id,
      raw_dist_ft, modal_dist_ft, dist_m, modal_dist_m,
      longitude, latitude, modal_longitude, modal_latitude,
      flag_modal_assignment_missing, flag_modal_changes_ward,
      flag_modal_changes_neighbor_ward, flag_modal_changes_pair,
      flag_modal_dist_diff_gt100ft
    )
} else {
  modal_sensitivity <- tibble(
    rent_panel_id = character(),
    file_date = as.Date(character()),
    ward = integer(),
    neighbor_ward = integer(),
    ward_pair_id = character(),
    modal_ward = integer(),
    modal_neighbor_ward = integer(),
    modal_ward_pair_id = character(),
    raw_dist_ft = numeric(),
    modal_dist_ft = numeric(),
    dist_m = numeric(),
    modal_dist_m = numeric(),
    longitude = numeric(),
    latitude = numeric(),
    modal_longitude = numeric(),
    modal_latitude = numeric(),
    flag_modal_assignment_missing = logical(),
    flag_modal_changes_ward = logical(),
    flag_modal_changes_neighbor_ward = logical(),
    flag_modal_changes_pair = logical(),
    flag_modal_dist_diff_gt100ft = logical()
  )
}

modal_flags <- modal_sensitivity %>%
  transmute(
    rent_panel_id,
    flag_modal_sensitivity_checked = TRUE,
    modal_ward,
    modal_neighbor_ward,
    modal_ward_pair_id,
    modal_dist_m,
    modal_dist_ft,
    flag_modal_assignment_missing,
    flag_modal_changes_ward,
    flag_modal_changes_neighbor_ward,
    flag_modal_changes_pair,
    flag_modal_dist_diff_gt100ft
  )
if (anyDuplicated(modal_flags$rent_panel_id) > 0) {
  stop("Modal-coordinate sensitivity output is not unique by rent_panel_id.", call. = FALSE)
}

final_df <- final_df %>%
  left_join(modal_flags, by = "rent_panel_id", relationship = "one-to-one") %>%
  mutate(
    flag_modal_sensitivity_checked = coalesce(flag_modal_sensitivity_checked, FALSE),
    flag_modal_assignment_missing = coalesce(flag_modal_assignment_missing, FALSE),
    flag_modal_changes_ward = coalesce(flag_modal_changes_ward, FALSE),
    flag_modal_changes_neighbor_ward = coalesce(flag_modal_changes_neighbor_ward, FALSE),
    flag_modal_changes_pair = coalesce(flag_modal_changes_pair, FALSE),
    flag_modal_dist_diff_gt100ft = coalesce(flag_modal_dist_diff_gt100ft, FALSE)
  )

if (!"flag_location_questionable" %in% names(final_df)) {
  final_df <- final_df %>%
    mutate(
      flag_location_questionable = coalesce(flag_address_location_unstable, FALSE) |
        coalesce(flag_coordinate_only_generic_pile, FALSE),
      location_quality_status = if_else(
        flag_location_questionable,
        "questionable_location",
        "not_flagged"
      )
    )
}

final_df <- final_df %>%
  mutate(
    flag_location_questionable = coalesce(flag_location_questionable, FALSE),
    flag_rd_location_questionable = flag_location_questionable |
      flag_modal_assignment_missing |
      flag_modal_changes_ward |
      flag_modal_changes_neighbor_ward |
      flag_modal_changes_pair |
      flag_modal_dist_diff_gt100ft
  )

final_df <- final_df %>%
  mutate(month_join = as.yearmon(file_date))

final_df <- final_df %>%
  left_join(alderman_lookup,
    by = c("ward" = "ward", "month_join" = "month"),
    relationship = "many-to-one"
  ) %>%
  rename(alderman_own = alderman)

final_df <- final_df %>%
  left_join(alderman_lookup,
    by = c("neighbor_ward" = "ward", "month_join" = "month"),
    relationship = "many-to-one"
  ) %>%
  rename(alderman_neighbor = alderman)

missing_aldermen <- final_df %>%
  summarise(
    missing_own = sum(is.na(alderman_own) | alderman_own == ""),
    missing_neighbor = sum(is.na(alderman_neighbor) | alderman_neighbor == "")
  )
if (missing_aldermen$missing_own > 0 || missing_aldermen$missing_neighbor > 0) {
  warning(sprintf(
    "Missing alderman lookup after rental boundary assignment: own=%d neighbor=%d. Keeping rows flagged for downstream score diagnostics.",
    missing_aldermen$missing_own,
    missing_aldermen$missing_neighbor
  ), call. = FALSE)
}

final_df <- final_df %>%
  mutate(
    flag_missing_own_alderman = is.na(alderman_own) | alderman_own == "",
    flag_missing_neighbor_alderman = is.na(alderman_neighbor) | alderman_neighbor == "",
    flag_missing_alderman_lookup = flag_missing_own_alderman | flag_missing_neighbor_alderman
  )

final_df <- final_df %>%
  mutate(rent_year_month = format(file_date, "%Y-%m"))

cpi_raw <- read_csv("../input/fred_cpi_cuura207sa0.csv", show_col_types = FALSE)
if (!all(c("observation_date", "CUURA207SA0") %in% names(cpi_raw))) {
  stop("CPI input missing expected columns for series CUURA207SA0.", call. = FALSE)
}

cpi_start_month <- as.Date(format(min(final_df$file_date, na.rm = TRUE), "%Y-%m-01"))
cpi_end_month <- as.Date(format(max(final_df$file_date, na.rm = TRUE), "%Y-%m-01"))
cpi <- tibble(observation_date = seq(cpi_start_month, cpi_end_month, by = "month")) %>%
  left_join(
    cpi_raw %>%
      transmute(
        observation_date = as.Date(observation_date),
        cpi_value = suppressWarnings(as.numeric(CUURA207SA0))
      ) %>%
      filter(!is.na(observation_date)),
    by = "observation_date"
  ) %>%
  arrange(observation_date)

n_missing_cpi <- sum(is.na(cpi$cpi_value))
if (n_missing_cpi > 0) {
  idx_known <- which(!is.na(cpi$cpi_value))
  if (length(idx_known) < 2) {
    stop("Not enough non-missing CPI observations to interpolate.", call. = FALSE)
  }
  cpi_interp <- approx(
    x = idx_known,
    y = cpi$cpi_value[idx_known],
    xout = seq_len(nrow(cpi)),
    method = "linear",
    rule = 1
  )$y
  cpi$cpi_value <- if_else(is.na(cpi$cpi_value), cpi_interp, cpi$cpi_value)
}
if (anyNA(cpi$cpi_value)) {
  stop("CPI has unresolved endpoint gaps after interpolation.", call. = FALSE)
}

cpi_2022 <- cpi %>%
  filter(format(observation_date, "%Y") == "2022") %>%
  pull(cpi_value)
if (length(cpi_2022) == 0 || !all(is.finite(cpi_2022))) {
  stop("Unable to compute base CPI for year 2022.", call. = FALSE)
}
base_cpi <- mean(cpi_2022)
if (!is.finite(base_cpi) || base_cpi <= 0) {
  stop("Computed invalid base CPI for year 2022.", call. = FALSE)
}

cpi_deflator <- cpi %>%
  transmute(
    rent_year_month = format(observation_date, "%Y-%m"),
    rent_price_cpi_chi_all_items = cpi_value,
    rent_price_deflator_to_2022 = base_cpi / cpi_value
  )

final_df <- final_df %>%
  left_join(cpi_deflator, by = "rent_year_month", relationship = "many-to-one")

n_missing_deflator <- sum(!is.finite(final_df$rent_price_deflator_to_2022))
if (n_missing_deflator > 0) {
  stop(sprintf(
    "Missing/invalid rent CPI deflator for %d observations.",
    n_missing_deflator
  ), call. = FALSE)
}

final_df <- final_df %>%
  mutate(
    rent_price_nominal = rent_price,
    rent_price = rent_price_nominal * rent_price_deflator_to_2022
  )

if (!"building_type_clean" %in% names(final_df)) {
  stop("Rental panel must provide audited building_type_clean.", call. = FALSE)
}
n_missing_building_type_clean <- sum(is.na(final_df$building_type_clean) | final_df$building_type_clean == "")
if (n_missing_building_type_clean > 0) {
  stop(sprintf(
    "Rental panel has %d missing building_type_clean values. Fix process_rent_data upstream.",
    n_missing_building_type_clean
  ), call. = FALSE)
}

final_df <- final_df %>%
  select(-any_of("dist_ft"))

write_parquet(final_df, "../output/rent_pre_scores_full.parquet")
