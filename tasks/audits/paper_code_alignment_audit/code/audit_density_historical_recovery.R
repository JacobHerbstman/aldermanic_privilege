# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")
source("../../../_lib/border_pair_helpers.R")

bandwidth_m <- 152.4
demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

normalize_zone_code <- function(x) {
  x %>%
    toupper() %>%
    str_replace_all("[\u2010-\u2015]", "-") %>%
    str_squish() %>%
    str_replace_all("\\s+", "") %>%
    str_replace("^([BCM]\\d)-\\.(\\d+)$", "\\1-\\2") %>%
    str_replace("^([BCM]\\d)\\.(\\d+)$", "\\1-\\2") %>%
    str_replace("^([A-Z]{2,})(\\d+(?:\\.\\d+)?)$", "\\1-\\2") %>%
    str_replace("^([BCM])-([1-7])-(\\d+(?:\\.\\d+)?)$", "\\1\\2-\\3") %>%
    str_replace("^R-(\\d)$", "R\\1") %>%
    str_replace("^C-4$", "C4") %>%
    str_replace_all("-{2,}", "-")
}

cases <- read_csv(
  "../output/density_historical_coordinate_cases.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)
historical <- read_csv(
  "../output/density_historical_parcel_records.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)

coordinate_candidates <- cases %>%
  inner_join(historical, by = "pin", relationship = "one-to-many") %>%
  mutate(
    year_gap = year - yearbuilt,
    absolute_year_gap = abs(year_gap)
  )

exact_coordinates <- coordinate_candidates %>%
  filter(year == yearbuilt) %>%
  distinct(pin, .keep_all = TRUE) %>%
  mutate(coordinate_rule = "exact_construction_year")

nearest_coordinates <- coordinate_candidates %>%
  group_by(pin) %>%
  arrange(absolute_year_gap, desc(year <= yearbuilt), desc(year), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(coordinate_rule = "nearest_available_year")

coordinate_selection <- bind_rows(exact_coordinates, nearest_coordinates) %>%
  select(
    coordinate_rule, pin, yearbuilt, coordinate_year = year,
    year_gap, longitude, latitude, source, source_class, source_card,
    subdivision_id, unitscount
  )
write_csv(coordinate_selection, "../output/density_historical_coordinate_selection.csv")

ward_panel <- st_read("../../../ward_panel_create/output/ward_panel.gpkg", quiet = TRUE)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers(
  "../../../border_segment_creation/output/ward_pair_boundaries.gpkg"
)
segment_layers <- load_segment_line_layers(
  "../../../border_segment_creation/output/boundary_segments_1320ft.gpkg"
)

alderman_panel <- read_csv(
  "../../../create_alderman_data/output/chicago_alderman_panel.csv",
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
  "../../../create_ward_controls/output/ward_controls_2000_2023.csv",
  show_col_types = FALSE
)
if (anyDuplicated(ward_controls[c("ward", "year")]) > 0) {
  stop("Ward controls are not unique by ward-year.", call. = FALSE)
}

scores <- read_csv(
  "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index)
if (anyDuplicated(scores$alderman) > 0) {
  stop("Score lookup is not unique by alderman.", call. = FALSE)
}

raw_zoning <- st_read(
  "../../../zoning_data_cleaning/input/raw_zoning_data.geojson",
  quiet = TRUE
) %>%
  transmute(zone_code = normalize_zone_code(zone_class)) %>%
  filter(!is.na(zone_code)) %>%
  st_make_valid() %>%
  st_transform(3435)

existing <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    sample_source = "existing_current_2025_coordinate",
    coordinate_rule = "current_2025_coordinate"
  )

build_recovered_rows <- function(selected_coordinates) {
  recovered <- selected_coordinates %>%
    mutate(
      construction_year = as.integer(yearbuilt),
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
      sample_source = "recovered_historical_coordinate"
    )

  recovered_sf <- st_as_sf(
    recovered,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(3435)

  assignments <- assign_points_to_boundaries(
    points_sf = recovered_sf,
    era_values = recovered_sf$era,
    ward_maps = ward_maps,
    boundary_lines = boundary_lines,
    chunk_n = 2000L
  )

  recovered_sf <- bind_cols(recovered_sf, assignments) %>%
    rename(
      ward = ward,
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
    max_distance = units::set_units(250, "m"),
    chunk_n = 50000L
  )

  zoning_join <- recovered_sf %>%
    select(pin) %>%
    st_transform(st_crs(raw_zoning)) %>%
    st_join(raw_zoning, left = TRUE, largest = TRUE) %>%
    st_drop_geometry()
  if (nrow(zoning_join) != nrow(recovered_sf) || anyDuplicated(zoning_join$pin) > 0) {
    stop("Raw zoning join changed recovered-row cardinality.", call. = FALSE)
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
    rename_with(~ paste0(.x, "_own"), all_of(setdiff(names(ward_controls), c("ward", "year")))) %>%
    left_join(scores, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
    rename(strictness_own = score) %>%
    left_join(scores, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
    rename(strictness_neighbor = score) %>%
    mutate(
      sign = case_when(
        strictness_own > strictness_neighbor ~ 1,
        strictness_own < strictness_neighbor ~ -1,
        TRUE ~ NA_real_
      ),
      signed_distance_m = dist_to_boundary_m * sign,
      signed_distance = dist_to_boundary * sign
    )
}

recovered_exact <- build_recovered_rows(exact_coordinates)
recovered_nearest <- build_recovered_rows(nearest_coordinates)

current_geometry <- st_read(
  "../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg",
  quiet = TRUE
) %>%
  mutate(
    pin = as.character(pin),
    construction_year = as.integer(construction_year)
  )
current_parcel_metadata <- read_csv(
  "../../../download_parcel_universe_data/output/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), pin10 = col_character(), subdivision_id = col_character(),
    .default = col_guess()
  )
) %>%
  select(
    pin,
    current_pin10 = pin10,
    current_parcel_class = class,
    current_subdivision_id = subdivision_id
  )
current_geometry <- current_geometry %>%
  left_join(current_parcel_metadata, by = "pin", relationship = "many-to-one")

recovered_exact_sf <- st_as_sf(
  recovered_exact,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)

nearest_current_rows <- list()
for (construction_year_i in sort(unique(recovered_exact_sf$construction_year))) {
  recovered_year <- recovered_exact_sf[
    recovered_exact_sf$construction_year == construction_year_i,
  ]
  current_year <- current_geometry[
    current_geometry$construction_year == construction_year_i,
  ]
  if (nrow(recovered_year) == 0 || nrow(current_year) == 0) {
    next
  }

  nearest_index <- st_nearest_feature(recovered_year, current_year)
  nearest_distance_ft <- as.numeric(st_distance(
    recovered_year,
    current_year[nearest_index, ],
    by_element = TRUE
  ))
  recovered_attributes <- st_drop_geometry(recovered_year)
  current_attributes <- st_drop_geometry(current_year[nearest_index, ])

  nearest_current_rows[[as.character(construction_year_i)]] <- tibble(
    pin = recovered_attributes$pin,
    construction_year = construction_year_i,
    source = recovered_attributes$source,
    source_class = recovered_attributes$source_class,
    historical_subdivision_id = recovered_attributes$subdivision_id,
    nearest_current_pin = current_attributes$pin,
    current_pin10 = current_attributes$current_pin10,
    current_parcel_class = current_attributes$current_parcel_class,
    current_subdivision_id = current_attributes$current_subdivision_id,
    nearest_current_distance_ft = nearest_distance_ft,
    lot_area_relative_gap = abs(
      recovered_attributes$arealotsf - current_attributes$arealotsf
    ) / pmax(recovered_attributes$arealotsf, current_attributes$arealotsf),
    building_area_relative_gap = abs(
      recovered_attributes$areabuilding - current_attributes$areabuilding
    ) / pmax(recovered_attributes$areabuilding, current_attributes$areabuilding),
    unit_count_gap = abs(
      recovered_attributes$unitscount - current_attributes$unitscount
    )
  )
}

nearest_current_matches <- bind_rows(nearest_current_rows) %>%
  mutate(
    same_subdivision =
      !is.na(historical_subdivision_id) & historical_subdivision_id != "" &
      !is.na(current_subdivision_id) & current_subdivision_id != "" &
      historical_subdivision_id == current_subdivision_id,
    same_pin_prefix_8 = substr(pin, 1, 8) == substr(nearest_current_pin, 1, 8),
    similar_building = building_area_relative_gap <= 0.05 & unit_count_gap == 0,
    likely_successor_narrow =
      nearest_current_distance_ft <= 1 |
      (same_subdivision & similar_building & nearest_current_distance_ft <= 50),
    likely_successor_broad =
      likely_successor_narrow |
      (same_subdivision & nearest_current_distance_ft <= 100) |
      (same_pin_prefix_8 & nearest_current_distance_ft <= 100) |
      (similar_building & nearest_current_distance_ft <= 50)
  )
write_csv(
  nearest_current_matches,
  "../output/density_historical_nearest_current_matches.csv"
)

recovered_exact <- recovered_exact %>%
  left_join(
    nearest_current_matches %>%
      select(pin, likely_successor_narrow, likely_successor_broad),
    by = "pin",
    relationship = "one-to-one"
  )

coordinate_stability <- coordinate_candidates %>%
  inner_join(
    exact_coordinates %>%
      select(
        pin,
        exact_x = centroid_x_crs_3435,
        exact_y = centroid_y_crs_3435
      ),
    by = "pin",
    relationship = "many-to-one"
  ) %>%
  mutate(
    distance_from_exact_year_ft = sqrt(
      (centroid_x_crs_3435 - exact_x)^2 +
      (centroid_y_crs_3435 - exact_y)^2
    )
  ) %>%
  group_by(pin, yearbuilt) %>%
  summarise(
    historical_years = n_distinct(year),
    earliest_coordinate_year = min(year),
    latest_coordinate_year = max(year),
    maximum_coordinate_shift_ft = max(distance_from_exact_year_ft),
    .groups = "drop"
  )
write_csv(
  coordinate_stability,
  "../output/density_historical_coordinate_stability.csv"
)

assignment_summary <- bind_rows(
  recovered_exact %>% mutate(coordinate_rule = "exact_construction_year"),
  recovered_nearest %>% mutate(coordinate_rule = "nearest_available_year")
) %>%
  group_by(coordinate_rule) %>%
  summarise(
    selected = n(),
    ward_assigned = sum(!is.na(ward)),
    boundary_assigned = sum(!is.na(ward_pair) & is.finite(dist_to_boundary_m)),
    within_500ft = sum(dist_to_boundary_m <= bandwidth_m, na.rm = TRUE),
    segment_assigned = sum(!is.na(segment_id) & segment_id != ""),
    both_scores = sum(is.finite(strictness_own) & is.finite(strictness_neighbor)),
    model_ready_all = sum(
      dist_to_boundary_m <= bandwidth_m &
        !is.na(segment_id) & segment_id != "" &
        !is.na(zone_code) &
        is.finite(strictness_own) & is.finite(strictness_neighbor),
      na.rm = TRUE
    ),
    model_ready_multifamily = sum(
      unitscount > 1 &
        dist_to_boundary_m <= bandwidth_m &
        !is.na(segment_id) & segment_id != "" &
        !is.na(zone_code) &
        is.finite(strictness_own) & is.finite(strictness_neighbor),
      na.rm = TRUE
    ),
    .groups = "drop"
  )
write_csv(assignment_summary, "../output/density_historical_assignment_summary.csv")

analysis_samples <- list(
  existing_paper_sample = existing,
  existing_plus_exact_year = bind_rows(existing, recovered_exact),
  existing_plus_exact_narrow_unmatched = bind_rows(
    existing,
    recovered_exact %>% filter(!likely_successor_narrow)
  ),
  existing_plus_exact_broad_unmatched = bind_rows(
    existing,
    recovered_exact %>% filter(!likely_successor_broad)
  ),
  existing_plus_nearest_year = bind_rows(existing, recovered_nearest)
)

model_rows <- list()
membership_rows <- list()
for (sample_name in names(analysis_samples)) {
  parcels <- analysis_samples[[sample_name]] %>%
    ensure_meter_distance_columns() %>%
    mutate(
      zone_group = zone_group_from_code(zone_code),
      side = as.integer(signed_distance_m > 0),
      lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
      strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
    ) %>%
    filter(
      construction_year >= 2006,
      construction_year <= 2022,
      arealotsf > 1,
      areabuilding > 1,
      dist_to_boundary_m <= bandwidth_m,
      !is.na(ward_pair),
      is.finite(signed_distance_m),
      !is.na(zone_code),
      !is.na(segment_id),
      segment_id != ""
    )

  for (sample_filter in c("all", "multifamily")) {
    model_data <- if (sample_filter == "all") {
      parcels %>% filter(unitscount > 0)
    } else {
      parcels %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac")) {
      model_data_outcome <- model_data %>%
        filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
        mutate(outcome_value = log(.data[[outcome]]))

      for (treatment in c("continuous", "binary")) {
        treatment_var <- if (treatment == "continuous") "strictness_own" else "side"
        model <- feols(
          as.formula(paste0(
            "outcome_value ~ ", treatment_var, " + lenient_dist + strict_dist + ",
            paste(demographic_controls, collapse = " + "),
            " | zone_group + segment_id + construction_year"
          )),
          data = model_data_outcome,
          cluster = ~ward_pair
        )
        coefficient_table <- coeftable(model)
        used <- obs(model)

        model_rows[[length(model_rows) + 1L]] <- tibble(
          analysis_sample = sample_name,
          construction_sample = sample_filter,
          outcome,
          treatment,
          estimate = unname(coefficient_table[treatment_var, "Estimate"]),
          se = unname(coefficient_table[treatment_var, "Std. Error"]),
          p_value = unname(coefficient_table[treatment_var, "Pr(>|t|)"]),
          n = nobs(model),
          recovered_n = sum(
            model_data_outcome$sample_source[used] == "recovered_historical_coordinate"
          ),
          ward_pairs = n_distinct(model_data_outcome$ward_pair[used])
        )

        membership_rows[[length(membership_rows) + 1L]] <- model_data_outcome[used, ] %>%
          transmute(
            analysis_sample = sample_name,
            construction_sample = sample_filter,
            outcome,
            treatment,
            pin,
            sample_source,
            coordinate_rule,
            construction_year,
            ward,
            ward_pair,
            segment_id,
            dist_to_boundary_m,
            unitscount,
            zone_code
          )
      }
    }
  }
}

write_csv(bind_rows(model_rows), "../output/density_historical_recovery_models.csv")
write_csv(bind_rows(membership_rows), "../output/density_historical_recovery_membership.csv")
write_csv(recovered_exact, "../output/density_historical_recovered_exact_rows.csv")
