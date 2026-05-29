# --- Interactive Test Block ---
# setwd("tasks/within_ward_strictness/code")
# spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"

source("../../setup_environment/code/packages.R")
source("../../_lib/alderman_uncertainty_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(spec)
}

if (length(args) != 1) {
  stop("FATAL: Script requires 1 arg: <uncertainty_spec>", call. = FALSE)
}

spec <- args[1]

spec_max_year <- str_match(spec, "through([0-9]{4})")[, 2]
max_permit_year <- ifelse(is.na(spec_max_year), NA_integer_, as.integer(spec_max_year))
if (!is.finite(max_permit_year)) {
  stop("Could not parse through-year from spec.", call. = FALSE)
}

config <- default_uncertainty_config()
config$include_porch <- str_detect(spec, "porchTRUE")
config$ca_fe <- FALSE
config$permit_type_fe <- TRUE
config$review_type_fe <- TRUE
config$volume_ctrl <- "LAG1"
config$volume_stage <- "BOTH"
config$two_stage <- FALSE

signs_regex <- "SIGNS"

message("=== Residualized High-vs-Low Processing-Time Correlation Check ===")
message("Spec: ", spec)
message("Through-year cutoff: ", max_permit_year)

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
ward_controls <- read_csv("../input/ward_controls.csv", show_col_types = FALSE)
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))
if (anyDuplicated(alderman_panel[c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}
if (anyDuplicated(ward_controls[c("ward", "year")]) > 0) {
  stop("Ward controls must be unique by ward-year.", call. = FALSE)
}

cta_stations <- st_read("../input/cta_stations.geojson", quiet = TRUE)
water_osm <- st_read("../input/gis_osm_water_a_free_1.shp", quiet = TRUE)

if (st_crs(cta_stations) != st_crs(ward_panel)) {
  cta_stations <- st_transform(cta_stations, st_crs(ward_panel))
}
if (st_crs(water_osm) != st_crs(ward_panel)) {
  water_osm <- st_transform(water_osm, st_crs(ward_panel))
}

permits <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  mutate(
    application_start_date_ym = as.yearmon(application_start_date_ym),
    application_year = year(as.Date(application_start_date_ym))
  ) %>%
  filter(
    !is.na(application_start_date_ym),
    !is.na(application_year),
    application_year <= max_permit_year,
    processing_time > 0,
    high_discretion == 1 | (high_discretion == 0 & !str_detect(str_to_upper(permit_type), signs_regex))
  )

if (!config$include_porch) {
  permits <- permits %>% filter(!str_detect(str_to_upper(permit_type), "PORCH"))
}

if (st_crs(permits) != st_crs(ward_panel)) {
  permits <- st_transform(permits, st_crs(ward_panel))
}

ward_geoms_map1 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
if (anyDuplicated(st_drop_geometry(ward_geoms_map1)$ward) > 0) {
  stop("2014 ward geometries must be unique by ward.", call. = FALSE)
}

ward_geoms_map2 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
if (anyDuplicated(st_drop_geometry(ward_geoms_map2)$ward) > 0) {
  stop("2016 ward geometries must be unique by ward.", call. = FALSE)
}

ward_geoms_map3 <- ward_panel %>%
  filter(year == max(year)) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
if (anyDuplicated(st_drop_geometry(ward_geoms_map3)$ward) > 0) {
  stop("Latest ward geometries must be unique by ward.", call. = FALSE)
}

permits_pre2015 <- permits %>%
  filter(application_start_date_ym < as.yearmon("2015-05"))

permits_2015_2023 <- permits %>%
  filter(
    application_start_date_ym >= as.yearmon("2015-05"),
    application_start_date_ym < as.yearmon("2023-05")
  )

permits_post2023 <- permits %>%
  filter(application_start_date_ym >= as.yearmon("2023-05"))

permits_ward_pre2015 <- if (nrow(permits_pre2015) == 0) {
  permits_pre2015 %>% st_drop_geometry()
} else {
  st_join(permits_pre2015, ward_geoms_map1, join = st_within) %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}
if (anyDuplicated(permits_ward_pre2015$id) > 0) {
  stop("Pre-2015 ward spatial join assigned a permit to multiple wards.", call. = FALSE)
}

permits_ward_2015_2023 <- if (nrow(permits_2015_2023) == 0) {
  permits_2015_2023 %>% st_drop_geometry()
} else {
  st_join(permits_2015_2023, ward_geoms_map2, join = st_within) %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}
if (anyDuplicated(permits_ward_2015_2023$id) > 0) {
  stop("2015-2023 ward spatial join assigned a permit to multiple wards.", call. = FALSE)
}

permits_ward_post2023 <- if (nrow(permits_post2023) == 0) {
  permits_post2023 %>% st_drop_geometry()
} else {
  st_join(permits_post2023, ward_geoms_map3, join = st_within) %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}
if (anyDuplicated(permits_ward_post2023$id) > 0) {
  stop("Post-2023 ward spatial join assigned a permit to multiple wards.", call. = FALSE)
}

permits_ward_data <- bind_rows(permits_ward_pre2015, permits_ward_2015_2023, permits_ward_post2023)
if (anyDuplicated(permits_ward_data$id) > 0) {
  stop("Combined ward-assigned permit data must be unique by permit id.", call. = FALSE)
}

permits_with_alderman <- permits_ward_data %>%
  left_join(alderman_panel, by = c("ward", "application_start_date_ym" = "month"), relationship = "many-to-one") %>%
  filter(!is.na(alderman))

max_control_year <- max(ward_controls$year, na.rm = TRUE)
ward_controls_extended <- if (max_permit_year > max_control_year) {
  bind_rows(
    ward_controls,
    ward_controls %>%
      filter(year == max_control_year) %>%
      select(-year) %>%
      crossing(year = (max_control_year + 1):max_permit_year)
  )
} else {
  ward_controls
}

permits_with_controls <- permits_with_alderman %>%
  left_join(ward_controls_extended, by = c("ward", "application_year" = "year"), relationship = "many-to-one")

permit_points <- permits %>%
  select(id) %>%
  semi_join(permits_with_controls %>% select(id), by = "id")

metric_crs <- 26916
permit_points_m <- st_transform(permit_points, metric_crs)
cta_stations_m <- st_transform(cta_stations, metric_crs)
water_osm_m <- st_transform(water_osm, metric_crs)
cbd_m <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) %>%
  st_transform(metric_crs)

dist_cbd_km <- as.numeric(units::set_units(st_distance(permit_points_m, cbd_m), "m")) / 1000
n_rail_stations_800m <- lengths(st_is_within_distance(permit_points_m, cta_stations_m, dist = 800))

lake_michigan_m <- water_osm_m %>%
  filter(!is.na(name) & tolower(name) == "lake michigan") %>%
  st_make_valid() %>%
  st_union()

dist_lake_km <- as.numeric(units::set_units(st_distance(permit_points_m, lake_michigan_m), "m")) / 1000

place_controls <- tibble(
  id = permit_points$id,
  dist_cbd_km = dist_cbd_km,
  dist_lake_km = dist_lake_km,
  n_rail_stations_800m = n_rail_stations_800m
)
if (anyDuplicated(place_controls$id) > 0) {
  stop("Place controls must be unique by permit id.", call. = FALSE)
}

permits_prepared_raw <- permits_with_controls %>%
  left_join(place_controls, by = "id", relationship = "many-to-one") %>%
  mutate(
    month = application_start_date_ym,
    year = application_year,
    log_processing_time = if_else(processing_time > 0, log(processing_time), NA_real_),
    log_reported_cost = {
      out <- rep(NA_real_, length(reported_cost))
      positive_cost <- !is.na(reported_cost) & reported_cost > 0
      out[positive_cost] <- log(reported_cost[positive_cost])
      out
    },
    permit_type_clean = if_else(is.na(permit_type), "unknown", as.character(permit_type)),
    review_type_clean = if_else(is.na(review_type), "unknown", as.character(review_type)),
    is_porch = str_detect(str_to_upper(permit_type), "PORCH"),
    group = if_else(high_discretion == 1, "high", "low")
  ) %>%
  filter(
    !is.na(alderman),
    !is.na(log_processing_time),
    !is.na(ward),
    !is.na(month)
  )

prepared <- prepare_uncertainty_sample(
  permits = permits_prepared_raw,
  include_porch = config$include_porch,
  volume_ctrl = config$volume_ctrl,
  volume_stage = config$volume_stage
)

covariates <- get_stage1_covariates(
  prepared$place_covariates,
  prepared$include_volume_stage1,
  prepared$volume_var,
  drop_covariates = c("share_bach_plus")
)

stage1 <- fit_stage1_model(
  permits = prepared$permits,
  stage1_outcome = "log_processing_time",
  covariates = covariates,
  fe_terms = get_stage1_fe_terms(config),
  variant_id = "high_low_residualized"
)

alderman_wide <- stage1$permits_for_reg %>%
  group_by(alderman, group) %>%
  summarise(
    n_permits = n(),
    mean_resid = mean(resid, na.rm = TRUE),
    mean_log_days = mean(log_processing_time, na.rm = TRUE),
    mean_days = mean(processing_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = group,
    values_from = c(n_permits, mean_resid, mean_log_days, mean_days),
    names_glue = "{.value}_{group}"
  ) %>%
  transmute(
    alderman,
    n_high = n_permits_high,
    n_low = n_permits_low,
    mean_resid_high = mean_resid_high,
    mean_resid_low = mean_resid_low,
    mean_log_high = mean_log_days_high,
    mean_log_low = mean_log_days_low,
    mean_days_high = mean_days_high,
    mean_days_low = mean_days_low
  ) %>%
  filter(!is.na(n_high), !is.na(n_low)) %>%
  mutate(harmonic_weight = 2 / ((1 / n_high) + (1 / n_low))) %>%
  arrange(alderman)

write_csv(
  alderman_wide,
  sprintf("../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_%s.csv", spec)
)
