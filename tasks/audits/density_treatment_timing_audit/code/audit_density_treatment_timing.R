# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_treatment_timing_audit/code")
# bandwidth_m <- 152.4
# min_construction_year <- 2006
# max_construction_year <- 2022
# max_spatial_match_ft <- 100
# max_permit_lead_years <- 8
# max_permit_lag_years <- 1

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")
source("../../../_lib/canonical_geometry_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    bandwidth_m,
    min_construction_year,
    max_construction_year,
    max_spatial_match_ft,
    max_permit_lead_years,
    max_permit_lag_years
  )
}
if (length(cli_args) != 6) {
  stop(
    paste(
      "Script requires six arguments:",
      "<bandwidth_m> <min_construction_year> <max_construction_year>",
      "<max_spatial_match_ft> <max_permit_lead_years> <max_permit_lag_years>."
    ),
    call. = FALSE
  )
}

bandwidth_m <- suppressWarnings(as.numeric(cli_args[1]))
min_construction_year <- suppressWarnings(as.integer(cli_args[2]))
max_construction_year <- suppressWarnings(as.integer(cli_args[3]))
max_spatial_match_ft <- suppressWarnings(as.numeric(cli_args[4]))
max_permit_lead_years <- suppressWarnings(as.integer(cli_args[5]))
max_permit_lag_years <- suppressWarnings(as.integer(cli_args[6]))

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be positive.", call. = FALSE)
}
if (!is.finite(min_construction_year) || !is.finite(max_construction_year) ||
    min_construction_year > max_construction_year) {
  stop("Construction-year bounds are invalid.", call. = FALSE)
}
if (!is.finite(max_spatial_match_ft) || max_spatial_match_ft <= 0) {
  stop("max_spatial_match_ft must be positive.", call. = FALSE)
}
if (!is.finite(max_permit_lead_years) || max_permit_lead_years < 0 ||
    !is.finite(max_permit_lag_years) || max_permit_lag_years < 0) {
  stop("Permit lead and lag windows must be nonnegative.", call. = FALSE)
}

parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  mutate(
    pin = as.character(pin),
    pin10 = str_sub(pin, 1, 10),
    ward = suppressWarnings(as.integer(ward)),
    construction_year = suppressWarnings(as.integer(construction_year)),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 0,
    construction_year >= min_construction_year,
    construction_year <= max_construction_year,
    dist_to_boundary_m <= bandwidth_m,
    !is.na(segment_id),
    segment_id != ""
  )

if (anyDuplicated(parcels$pin) > 0) {
  stop("Analytical parcel input is not unique by PIN.", call. = FALSE)
}

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(
    ward = suppressWarnings(as.integer(ward)),
    panel_month = as.Date(as.yearmon(month, format = "%b %Y"))
  ) %>%
  filter(!is.na(ward), !is.na(panel_month), !is.na(alderman)) %>%
  arrange(ward, panel_month) %>%
  group_by(ward) %>%
  mutate(old_alderman = lag(alderman)) %>%
  ungroup()

regular_term_dates <- tibble(
  turnover_year = c(2007L, 2011L, 2015L, 2019L),
  regular_term_date = as.Date(c("2007-05-21", "2011-05-16", "2015-05-18", "2019-05-20"))
)

turnover_events <- alderman_panel %>%
  filter(
    !is.na(old_alderman),
    alderman != old_alderman,
    year(panel_month) >= min_construction_year,
    year(panel_month) <= max_construction_year
  ) %>%
  transmute(
    ward,
    turnover_year = year(panel_month),
    panel_change_month = panel_month,
    old_alderman,
    new_alderman = alderman
  ) %>%
  left_join(regular_term_dates, by = "turnover_year", relationship = "many-to-one") %>%
  mutate(
    regular_election_turnover = !is.na(regular_term_date) & month(panel_change_month) == 6,
    turnover_date = if_else(regular_election_turnover, regular_term_date, panel_change_month),
    turnover_type = if_else(regular_election_turnover, "regular_election", "off_cycle_or_panel_break")
  ) %>%
  select(-regular_term_date)

turnovers <- turnover_events %>%
  arrange(ward, turnover_year, turnover_date) %>%
  group_by(ward, turnover_year) %>%
  summarise(
    panel_change_month = max(panel_change_month),
    old_alderman = first(old_alderman),
    new_alderman = last(new_alderman),
    regular_election_turnover = any(regular_election_turnover),
    turnover_date = max(turnover_date),
    turnover_events = n(),
    turnover_type = case_when(
      turnover_events > 1 ~ "multiple_changes",
      regular_election_turnover ~ "regular_election",
      TRUE ~ "off_cycle_or_panel_break"
    ),
    .groups = "drop"
  )

parcels <- parcels %>%
  left_join(
    turnovers,
    by = c("ward", "construction_year" = "turnover_year"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    turnovers %>%
      transmute(
        other_ward = ward,
        construction_year = turnover_year,
        neighbor_regular_turnover_ward_year = regular_election_turnover
      ),
    by = c("other_ward", "construction_year"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    turnover_ward_year = !is.na(turnover_date),
    regular_turnover_ward_year = coalesce(regular_election_turnover, FALSE),
    neighbor_regular_turnover_ward_year = coalesce(
      neighbor_regular_turnover_ward_year,
      FALSE
    ),
    regular_turnover_boundary_year = regular_turnover_ward_year |
      neighbor_regular_turnover_ward_year,
    production_proxy_date = as.Date(paste0(construction_year, "-06-15"))
  )

parcel_geometry <- st_read("../input/parcels_with_geometry.gpkg", quiet = TRUE) %>%
  select(pin)
parcel_geometry$pin <- as.character(parcel_geometry$pin)
if (anyDuplicated(parcel_geometry$pin) > 0) {
  stop("Parcel geometry input is not unique by PIN.", call. = FALSE)
}

audit_parcels <- parcels %>%
  filter(turnover_ward_year | construction_year == 2015L) %>%
  mutate(audit_parcel_id = row_number())

audit_parcels_sf <- parcel_geometry %>%
  inner_join(
    audit_parcels,
    by = "pin",
    relationship = "one-to-one"
  )
if (nrow(audit_parcels_sf) != nrow(audit_parcels)) {
  stop("Not every timing-audit parcel has exactly one geometry.", call. = FALSE)
}

audit_coordinates <- st_coordinates(audit_parcels_sf)
audit_parcels_sf$parcel_x <- audit_coordinates[, "X"]
audit_parcels_sf$parcel_y <- audit_coordinates[, "Y"]

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
canonical_ward_maps <- load_canonical_ward_maps(ward_panel)
old_2015_map <- canonical_ward_maps[["2003_2014"]]
new_2015_map <- canonical_ward_maps[["2015_2023"]]

if (st_crs(old_2015_map) != st_crs(audit_parcels_sf)) {
  old_2015_map <- st_transform(old_2015_map, st_crs(audit_parcels_sf))
}
if (st_crs(new_2015_map) != st_crs(audit_parcels_sf)) {
  new_2015_map <- st_transform(new_2015_map, st_crs(audit_parcels_sf))
}

old_ward_matches <- st_within(audit_parcels_sf, old_2015_map)
new_ward_matches <- st_within(audit_parcels_sf, new_2015_map)
audit_parcels_sf$ward_under_2003_map <- vapply(
  old_ward_matches,
  function(i) if (length(i) == 1) as.integer(old_2015_map$ward[i]) else NA_integer_,
  integer(1)
)
audit_parcels_sf$ward_under_2015_map <- vapply(
  new_ward_matches,
  function(i) if (length(i) == 1) as.integer(new_2015_map$ward[i]) else NA_integer_,
  integer(1)
)
audit_parcels_sf$ward_map_changes_in_2015 <- with(
  audit_parcels_sf,
  construction_year == 2015L &
    !is.na(ward_under_2003_map) &
    !is.na(ward_under_2015_map) &
    ward_under_2003_map != ward_under_2015_map
)

permit_text <- data.table::fread(
  cmd = "gzip -dc ../input/building_permits_text_features.csv.gz",
  select = c("id", "address_key", "pin_list"),
  colClasses = c(id = "character", address_key = "character", pin_list = "character")
) %>%
  as_tibble() %>%
  mutate(id = as.character(id))
if (anyDuplicated(permit_text$id) > 0) {
  stop("Permit text features are not unique by permit ID.", call. = FALSE)
}

permit_sf <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  filter(permit_type == "PERMIT - NEW CONSTRUCTION") %>%
  mutate(id = as.character(id)) %>%
  left_join(permit_text, by = "id", relationship = "many-to-one")

if (st_crs(permit_sf) != st_crs(audit_parcels_sf)) {
  permit_sf <- st_transform(permit_sf, st_crs(audit_parcels_sf))
}

permit_coordinates <- st_coordinates(permit_sf)
permit_rows <- permit_sf %>%
  st_drop_geometry() %>%
  transmute(
    id,
    address_key,
    pin_text = coalesce(pin_list, as.character(pin)),
    application_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    permit_x = permit_coordinates[, "X"],
    permit_y = permit_coordinates[, "Y"]
  ) %>%
  mutate(
    pin10_primary = str_extract(coalesce(pin_text, ""), "[0-9]{9,14}"),
    pin10_primary = str_pad(pin10_primary, width = 10, side = "left", pad = "0"),
    pin10_primary = str_sub(pin10_primary, 1, 10),
    project_event_date = coalesce(issue_date, application_date),
    project_group_key = case_when(
      str_detect(pin10_primary, "^[0-9]{10}$") & !is.na(address_key) & address_key != "" ~
        paste0("PIN_", pin10_primary, "_ADDR_", address_key),
      str_detect(pin10_primary, "^[0-9]{10}$") ~ paste0("PIN_", pin10_primary),
      !is.na(address_key) & address_key != "" ~ paste0("ADDR_", address_key),
      TRUE ~ paste0("ID_", id)
    )
  ) %>%
  arrange(project_group_key, project_event_date, id) %>%
  group_by(project_group_key) %>%
  mutate(
    days_since_previous = as.numeric(project_event_date - lag(project_event_date)),
    new_project = is.na(days_since_previous) | days_since_previous > 730,
    project_cluster = cumsum(new_project),
    project_id = paste0(project_group_key, "_", str_pad(project_cluster, 3, pad = "0"))
  ) %>%
  ungroup() %>%
  select(-project_event_date, -project_group_key, -days_since_previous, -new_project, -project_cluster)

permit_projects <- permit_rows %>%
  group_by(project_id) %>%
  summarise(
    address_key = first(na.omit(address_key), default = NA_character_),
    first_application_date = min(application_date, na.rm = TRUE),
    first_issue_date = min(issue_date, na.rm = TRUE),
    last_issue_date = max(issue_date, na.rm = TRUE),
    permit_records = n(),
    project_x = median(permit_x, na.rm = TRUE),
    project_y = median(permit_y, na.rm = TRUE),
    coordinate_spread_ft = max(sqrt((permit_x - project_x)^2 + (permit_y - project_y)^2), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    first_application_date = if_else(is.infinite(first_application_date), as.Date(NA), first_application_date),
    first_issue_date = if_else(is.infinite(first_issue_date), as.Date(NA), first_issue_date),
    last_issue_date = if_else(is.infinite(last_issue_date), as.Date(NA), last_issue_date),
    project_event_date = coalesce(first_issue_date, first_application_date),
    project_row = row_number()
  )

project_pin_bridge <- permit_rows %>%
  select(project_id, pin_text) %>%
  mutate(pin_token = str_extract_all(coalesce(pin_text, ""), "[0-9]{9,14}")) %>%
  tidyr::unnest_longer(pin_token, values_to = "pin_token", keep_empty = FALSE) %>%
  mutate(
    pin_token = str_pad(pin_token, width = 10, side = "left", pad = "0"),
    pin10 = str_sub(pin_token, 1, 10)
  ) %>%
  filter(str_detect(pin10, "^[0-9]{10}$")) %>%
  distinct(project_id, pin10)

project_sf <- st_as_sf(
  permit_projects,
  coords = c("project_x", "project_y"),
  crs = st_crs(audit_parcels_sf),
  remove = FALSE
)

audit_parcel_table <- audit_parcels_sf %>%
  st_drop_geometry() %>%
  mutate(
    permit_window_start = as.Date(paste0(construction_year - max_permit_lead_years, "-01-01")),
    permit_window_end = as.Date(paste0(construction_year + max_permit_lag_years, "-12-31"))
  )
if (anyDuplicated(audit_parcel_table$pin10) > 0) {
  stop("Timing-audit parcels are not unique by PIN10.", call. = FALSE)
}

exact_candidates <- audit_parcel_table %>%
  select(
    audit_parcel_id,
    pin10,
    parcel_x,
    parcel_y,
    production_proxy_date,
    permit_window_start,
    permit_window_end
  ) %>%
  inner_join(project_pin_bridge, by = "pin10", relationship = "one-to-many") %>%
  inner_join(permit_projects, by = "project_id", relationship = "many-to-one") %>%
  filter(
    !is.na(project_event_date),
    project_event_date >= permit_window_start,
    project_event_date <= permit_window_end
  ) %>%
  mutate(
    match_distance_ft = sqrt((parcel_x - project_x)^2 + (parcel_y - project_y)^2),
    event_date_gap_days = abs(as.numeric(project_event_date - production_proxy_date))
  ) %>%
  arrange(audit_parcel_id, match_distance_ft, event_date_gap_days, project_id) %>%
  group_by(audit_parcel_id) %>%
  mutate(
    exact_candidate_count = n(),
    second_exact_distance_ft = nth(match_distance_ft, 2, default = Inf),
    second_exact_gap_days = nth(event_date_gap_days, 2, default = Inf)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    exact_match_quality = case_when(
      match_distance_ft > 250 ~ "exact_pin_location_outlier_review",
      exact_candidate_count == 1 ~ "exact_pin_unique",
      second_exact_distance_ft - match_distance_ft >= 25 ~ "exact_pin_multiple_clear_location",
      second_exact_gap_days - event_date_gap_days >= 365 ~ "exact_pin_multiple_clear_date",
      TRUE ~ "exact_pin_multiple_review"
    )
  )

spatial_index <- st_is_within_distance(
  audit_parcels_sf,
  project_sf,
  dist = max_spatial_match_ft
)
spatial_candidates <- tibble(
  audit_parcel_id = rep(audit_parcels_sf$audit_parcel_id, lengths(spatial_index)),
  project_row = unlist(spatial_index, use.names = FALSE)
) %>%
  left_join(
    audit_parcel_table %>%
      select(
        audit_parcel_id,
        parcel_x,
        parcel_y,
        production_proxy_date,
        permit_window_start,
        permit_window_end
      ),
    by = "audit_parcel_id",
    relationship = "many-to-one"
  ) %>%
  left_join(
    permit_projects,
    by = "project_row",
    relationship = "many-to-one"
  ) %>%
  filter(
    !is.na(project_event_date),
    project_event_date >= permit_window_start,
    project_event_date <= permit_window_end
  ) %>%
  mutate(
    match_distance_ft = sqrt((parcel_x - project_x)^2 + (parcel_y - project_y)^2),
    event_date_gap_days = abs(as.numeric(project_event_date - production_proxy_date))
  ) %>%
  arrange(audit_parcel_id, match_distance_ft, event_date_gap_days, project_id) %>%
  group_by(audit_parcel_id) %>%
  mutate(
    spatial_candidate_count = n(),
    second_spatial_distance_ft = nth(match_distance_ft, 2, default = Inf)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    audit_parcel_id,
    project_id,
    address_key,
    first_application_date,
    first_issue_date,
    last_issue_date,
    permit_records,
    project_x,
    project_y,
    coordinate_spread_ft,
    project_event_date,
    match_distance_ft,
    event_date_gap_days,
    spatial_candidate_count,
    second_spatial_distance_ft,
    spatial_match_quality = case_when(
      match_distance_ft <= 25 & second_spatial_distance_ft - match_distance_ft >= 25 ~ "spatial_within_25ft_clear",
      match_distance_ft <= 50 & second_spatial_distance_ft - match_distance_ft >= 25 ~ "spatial_within_50ft_clear",
      match_distance_ft <= 25 ~ "spatial_within_25ft_review",
      match_distance_ft <= 50 ~ "spatial_within_50ft_review",
      TRUE ~ "spatial_50_to_100ft_review"
    )
  )

selected_matches <- audit_parcel_table %>%
  left_join(
    exact_candidates %>%
      select(
        audit_parcel_id,
        exact_project_id = project_id,
        exact_address_key = address_key,
        exact_first_application_date = first_application_date,
        exact_first_issue_date = first_issue_date,
        exact_last_issue_date = last_issue_date,
        exact_permit_records = permit_records,
        exact_coordinate_spread_ft = coordinate_spread_ft,
        exact_match_distance_ft = match_distance_ft,
        exact_candidate_count,
        exact_match_quality
      ),
    by = "audit_parcel_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    spatial_candidates %>%
      rename_with(~ paste0("spatial_", .), -audit_parcel_id),
    by = "audit_parcel_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    match_method = case_when(
      !is.na(exact_project_id) ~ "exact_pin",
      !is.na(spatial_project_id) ~ "spatial",
      TRUE ~ "unmatched"
    ),
    match_quality = case_when(
      match_method == "exact_pin" ~ exact_match_quality,
      match_method == "spatial" ~ spatial_spatial_match_quality,
      TRUE ~ "unmatched"
    ),
    matched_project_id = coalesce(exact_project_id, spatial_project_id),
    matched_address = coalesce(exact_address_key, spatial_address_key),
    matched_application_date = coalesce(exact_first_application_date, spatial_first_application_date),
    matched_issue_date = coalesce(exact_first_issue_date, spatial_first_issue_date),
    matched_last_issue_date = coalesce(exact_last_issue_date, spatial_last_issue_date),
    matched_permit_records = coalesce(exact_permit_records, spatial_permit_records),
    matched_distance_ft = coalesce(exact_match_distance_ft, spatial_match_distance_ft),
    matched_coordinate_spread_ft = coalesce(
      exact_coordinate_spread_ft,
      spatial_coordinate_spread_ft
    ),
    match_requires_review = str_detect(match_quality, "review") | match_quality == "unmatched",
    application_relative_to_turnover = case_when(
      !turnover_ward_year ~ "not_turnover_ward_year",
      is.na(matched_application_date) ~ "missing",
      matched_application_date < turnover_date ~ "before_turnover",
      TRUE ~ "after_turnover"
    ),
    issue_relative_to_turnover = case_when(
      !turnover_ward_year ~ "not_turnover_ward_year",
      is.na(matched_issue_date) ~ "missing",
      matched_issue_date < turnover_date ~ "before_turnover",
      TRUE ~ "after_turnover"
    ),
    production_relative_to_turnover = case_when(
      !turnover_ward_year ~ "not_turnover_ward_year",
      production_proxy_date < turnover_date ~ "before_turnover",
      TRUE ~ "after_turnover"
    ),
    application_disagrees_with_production_side = turnover_ward_year &
      application_relative_to_turnover %in% c("before_turnover", "after_turnover") &
      application_relative_to_turnover != production_relative_to_turnover,
    issue_disagrees_with_production_side = turnover_ward_year &
      issue_relative_to_turnover %in% c("before_turnover", "after_turnover") &
      issue_relative_to_turnover != production_relative_to_turnover,
    application_relative_to_2015_map = case_when(
      construction_year != 2015L ~ "not_2015",
      is.na(matched_application_date) ~ "missing",
      matched_application_date < as.Date("2015-05-18") ~ "before_2015_map",
      TRUE ~ "after_2015_map"
    ),
    issue_relative_to_2015_map = case_when(
      construction_year != 2015L ~ "not_2015",
      is.na(matched_issue_date) ~ "missing",
      matched_issue_date < as.Date("2015-05-18") ~ "before_2015_map",
      TRUE ~ "after_2015_map"
    )
  )

match_output <- selected_matches %>%
  select(
    audit_parcel_id,
    pin,
    pin10,
    construction_year,
    ward,
    ward_pair,
    unitscount,
    density_far,
    density_dupac,
    dist_to_boundary,
    segment_id,
    alderman_own,
    alderman_neighbor,
    strictness_own,
    strictness_neighbor,
    turnover_ward_year,
    regular_turnover_ward_year,
    turnover_type,
    turnover_events,
    turnover_date,
    old_alderman,
    new_alderman,
    production_proxy_date,
    production_relative_to_turnover,
    ward_under_2003_map,
    ward_under_2015_map,
    ward_map_changes_in_2015,
    match_method,
    match_quality,
    match_requires_review,
    matched_project_id,
    matched_address,
    matched_application_date,
    matched_issue_date,
    matched_last_issue_date,
    matched_permit_records,
    matched_distance_ft,
    matched_coordinate_spread_ft,
    exact_candidate_count,
    spatial_spatial_candidate_count,
    application_relative_to_turnover,
    issue_relative_to_turnover,
    application_disagrees_with_production_side,
    issue_disagrees_with_production_side,
    application_relative_to_2015_map,
    issue_relative_to_2015_map
  ) %>%
  arrange(construction_year, ward, pin)

summary_rows <- bind_rows(
  tibble(
    scope = "baseline_500ft",
    group = "all",
    metric = c("parcels", "multifamily_parcels", "turnover_ward_year_parcels", "turnover_ward_year_multifamily"),
    value = c(
      nrow(parcels),
      sum(parcels$unitscount > 1),
      sum(parcels$turnover_ward_year),
      sum(parcels$turnover_ward_year & parcels$unitscount > 1)
    )
  ),
  match_output %>%
    count(match_method, name = "value") %>%
    transmute(scope = "audit_parcels", group = match_method, metric = "parcels", value),
  match_output %>%
    count(match_quality, name = "value") %>%
    transmute(scope = "audit_parcels", group = match_quality, metric = "parcels", value),
  match_output %>%
    filter(match_method == "exact_pin") %>%
    summarise(
      median_distance_ft = median(matched_distance_ft),
      p90_distance_ft = quantile(matched_distance_ft, 0.90, names = FALSE),
      p95_distance_ft = quantile(matched_distance_ft, 0.95, names = FALSE),
      max_distance_ft = max(matched_distance_ft)
    ) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    mutate(scope = "exact_pin_distance_calibration", group = "all") %>%
    select(scope, group, metric, value),
  match_output %>%
    filter(turnover_ward_year) %>%
    count(application_relative_to_turnover, name = "value") %>%
    transmute(
      scope = "turnover_ward_year",
      group = application_relative_to_turnover,
      metric = "candidate_application_date_classification",
      value
    ),
  match_output %>%
    filter(turnover_ward_year) %>%
    count(issue_relative_to_turnover, name = "value") %>%
    transmute(
      scope = "turnover_ward_year",
      group = issue_relative_to_turnover,
      metric = "candidate_issue_date_classification",
      value
    ),
  match_output %>%
    filter(turnover_ward_year) %>%
    summarise(
      automated_clear_matches = sum(!match_requires_review),
      review_candidates = sum(match_requires_review & match_method != "unmatched"),
      unmatched = sum(match_method == "unmatched"),
      clear_application_before_turnover = sum(
        !match_requires_review & application_relative_to_turnover == "before_turnover"
      ),
      clear_application_after_turnover = sum(
        !match_requires_review & application_relative_to_turnover == "after_turnover"
      ),
      clear_application_disagreements = sum(
        !match_requires_review & application_disagrees_with_production_side,
        na.rm = TRUE
      ),
      clear_issue_disagreements = sum(
        !match_requires_review & issue_disagrees_with_production_side,
        na.rm = TRUE
      )
    ) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    mutate(scope = "turnover_ward_year", group = "all") %>%
    select(scope, group, metric, value),
  match_output %>%
    filter(construction_year == 2015L) %>%
    summarise(
      parcels = n(),
      ward_map_changes = sum(ward_map_changes_in_2015, na.rm = TRUE),
      automated_clear_matches = sum(!match_requires_review),
      review_candidates = sum(match_requires_review & match_method != "unmatched"),
      unmatched = sum(match_method == "unmatched"),
      clear_application_before_map = sum(
        !match_requires_review & application_relative_to_2015_map == "before_2015_map"
      ),
      clear_application_after_map = sum(
        !match_requires_review & application_relative_to_2015_map == "after_2015_map"
      )
    ) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    mutate(scope = "construction_year_2015", group = "all") %>%
    select(scope, group, metric, value),
  match_output %>%
    filter(turnover_ward_year) %>%
    count(construction_year, match_method, name = "value") %>%
    transmute(
      scope = "turnover_by_year",
      group = paste(construction_year, match_method, sep = "_"),
      metric = "parcels",
      value
    ),
  match_output %>%
    filter(turnover_ward_year) %>%
    mutate(
      review_status = case_when(
        match_method == "unmatched" ~ "unmatched",
        match_requires_review ~ "review",
        TRUE ~ "automated_clear"
      )
    ) %>%
    count(construction_year, review_status, name = "value") %>%
    transmute(
      scope = "turnover_quality_by_year",
      group = paste(construction_year, review_status, sep = "_"),
      metric = "parcels",
      value
  )
)

alderman_month_lookup <- alderman_panel %>%
  select(ward, panel_month, alderman) %>%
  mutate(exact_month_alderman = TRUE) %>%
  complete(ward, panel_month = sort(unique(alderman_panel$panel_month))) %>%
  arrange(ward, panel_month) %>%
  group_by(ward) %>%
  fill(alderman, .direction = "down") %>%
  ungroup() %>%
  mutate(exact_month_alderman = coalesce(exact_month_alderman, FALSE))
if (anyDuplicated(alderman_month_lookup[c("ward", "panel_month")]) > 0) {
  stop("Alderman panel is not unique by ward-month.", call. = FALSE)
}

score_lookup <- read_csv("../input/aldermen_uncertainty_scores.csv", show_col_types = FALSE) %>%
  select(alderman, uncertainty_index)
if (anyDuplicated(score_lookup$alderman) > 0) {
  stop("Stringency-score input is not unique by alderman.", call. = FALSE)
}

opposite_spec_data <- list(
  opposite_regular_turnover_fixed_geography = parcels %>%
    mutate(
      opposite_assignment = regular_turnover_boundary_year,
      opposite_date = if_else(
        opposite_assignment,
        as.Date(paste0(construction_year, "-04-15")),
        as.Date(NA)
      )
    ),
  opposite_regular_turnover_and_2015_fixed_geography = parcels %>%
    mutate(
      opposite_assignment = regular_turnover_boundary_year | construction_year == 2015L,
      opposite_date = if_else(
        opposite_assignment,
        as.Date(paste0(construction_year, "-04-15")),
        as.Date(NA)
      )
    )
)

opposite_model_samples <- list()
opposite_summary_rows <- list()
for (specification in names(opposite_spec_data)) {
  assignment <- opposite_spec_data[[specification]] %>%
    filter(opposite_assignment) %>%
    mutate(opposite_month = as.Date(format(opposite_date, "%Y-%m-01"))) %>%
    select(pin, ward, other_ward, opposite_date, opposite_month) %>%
    left_join(
      alderman_month_lookup %>% rename(
        opposite_alderman_own = alderman,
        opposite_alderman_own_exact_month = exact_month_alderman
      ),
      by = c("ward", "opposite_month" = "panel_month"),
      relationship = "many-to-one"
    ) %>%
    left_join(
      alderman_month_lookup %>% rename(
        opposite_alderman_neighbor = alderman,
        opposite_alderman_neighbor_exact_month = exact_month_alderman
      ),
      by = c("other_ward" = "ward", "opposite_month" = "panel_month"),
      relationship = "many-to-one"
    ) %>%
    left_join(
      score_lookup %>% rename(
        opposite_alderman_own = alderman,
        opposite_strictness_own = uncertainty_index
      ),
      by = "opposite_alderman_own",
      relationship = "many-to-one"
    ) %>%
    left_join(
      score_lookup %>% rename(
        opposite_alderman_neighbor = alderman,
        opposite_strictness_neighbor = uncertainty_index
      ),
      by = "opposite_alderman_neighbor",
      relationship = "many-to-one"
    )

  if (any(is.na(assignment$opposite_alderman_own)) ||
      any(is.na(assignment$opposite_alderman_neighbor))) {
    stop(sprintf("%s has missing April alderman assignments.", specification), call. = FALSE)
  }
  if (any(is.na(assignment$opposite_strictness_own)) ||
      any(is.na(assignment$opposite_strictness_neighbor))) {
    stop(sprintf("%s has April aldermen without stringency scores.", specification), call. = FALSE)
  }

  opposite_model_samples[[specification]] <- parcels %>%
    left_join(
      assignment %>%
        select(
          pin,
          opposite_date,
          opposite_alderman_own,
          opposite_alderman_neighbor,
          opposite_strictness_own,
          opposite_strictness_neighbor
        ),
      by = "pin",
      relationship = "one-to-one"
    ) %>%
    mutate(
      opposite_assignment = !is.na(opposite_date),
      strictness_own = if_else(
        opposite_assignment,
        opposite_strictness_own,
        strictness_own
      ),
      strictness_neighbor = if_else(
        opposite_assignment,
        opposite_strictness_neighbor,
        strictness_neighbor
      ),
      sign = case_when(
        strictness_own > strictness_neighbor ~ 1,
        strictness_own < strictness_neighbor ~ -1,
        TRUE ~ NA_real_
      ),
      signed_distance = dist_to_boundary * sign,
      signed_distance_m = dist_to_boundary_m * sign
    )

  if (any(is.na(opposite_model_samples[[specification]]$sign))) {
    stop(sprintf("%s creates observations with undefined treatment sign.", specification), call. = FALSE)
  }

  opposite_summary_rows[[specification]] <- tibble(
    scope = "opposite_assignment",
    group = specification,
    metric = c(
      "reassigned_parcels",
      "reassigned_multifamily_parcels",
      "own_alderman_carried_from_prior_month",
      "neighbor_alderman_carried_from_prior_month"
    ),
    value = c(
      nrow(assignment),
      sum(assignment$pin %in% parcels$pin[parcels$unitscount > 1]),
      sum(!assignment$opposite_alderman_own_exact_month),
      sum(!assignment$opposite_alderman_neighbor_exact_month)
    )
  )
}
summary_rows <- bind_rows(summary_rows, bind_rows(opposite_summary_rows))

model_samples <- list(
  baseline = parcels,
  drop_regular_turnover_ward_years = parcels %>% filter(!regular_turnover_ward_year),
  drop_all_turnover_ward_years = parcels %>% filter(!turnover_ward_year),
  drop_all_2015 = parcels %>% filter(construction_year != 2015L),
  drop_all_turnover_and_2015 = parcels %>% filter(!turnover_ward_year, construction_year != 2015L)
)
model_samples <- c(model_samples, opposite_model_samples)

controls <- c(
  "strictness_own",
  "lenient_dist",
  "strict_dist",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

robustness_rows <- list()
for (specification in names(model_samples)) {
  model_data <- model_samples[[specification]] %>%
    mutate(
      lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
      strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
    )

  for (sample_name in c("all", "multifamily")) {
    sample_data <- model_data
    if (sample_name == "multifamily") {
      sample_data <- sample_data %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac")) {
      formula_i <- as.formula(paste0(
        "log(", outcome, ") ~ ",
        paste(controls, collapse = " + "),
        " | zone_group + segment_id + construction_year"
      ))
      model_i <- feols(formula_i, data = sample_data, cluster = ~ward_pair)
      coefficient_i <- coeftable(model_i)
      robustness_rows[[length(robustness_rows) + 1L]] <- tibble(
        specification,
        sample = sample_name,
        outcome,
        estimate = unname(coefficient_i["strictness_own", "Estimate"]),
        standard_error = unname(coefficient_i["strictness_own", "Std. Error"]),
        p_value = unname(coefficient_i["strictness_own", "Pr(>|t|)"]),
        observations = nobs(model_i),
        ward_pairs = n_distinct(sample_data$ward_pair)
      )
    }
  }
}
robustness_output <- bind_rows(robustness_rows)

write_csv(summary_rows, "../output/density_timing_audit_summary.csv")
write_csv(match_output, "../output/density_timing_project_matches.csv")
write_csv(robustness_output, "../output/density_timing_robustness.csv")
