# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/within_ward_strictness/code")
# spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"

source("../../setup_environment/code/packages.R")
source("../../_lib/alderman_uncertainty_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(spec)
}

if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <uncertainty_spec>.", call. = FALSE)
}

spec <- cli_args[1]

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

cta_stations <- st_read("../input/cta_stops.gpkg", quiet = TRUE) %>%
  mutate(
    active_from_date = as.Date(active_from_date),
    active_to_date = as.Date(active_to_date)
  )
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

ward_geoms_2014 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
if (anyDuplicated(st_drop_geometry(ward_geoms_2014)$ward) > 0) {
  stop("2014 ward geometries must be unique by ward.", call. = FALSE)
}

ward_geoms_2016 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
if (anyDuplicated(st_drop_geometry(ward_geoms_2016)$ward) > 0) {
  stop("2016 ward geometries must be unique by ward.", call. = FALSE)
}

ward_geoms_latest <- ward_panel %>%
  filter(year == max(year)) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
if (anyDuplicated(st_drop_geometry(ward_geoms_latest)$ward) > 0) {
  stop("Latest ward geometries must be unique by ward.", call. = FALSE)
}

permits_pre_2015_ward <- permits %>%
  filter(application_start_date_ym < as.yearmon("2015-05")) %>%
  st_join(ward_geoms_2014, join = st_within) %>%
  mutate(ward = ward.y) %>%
  select(-any_of(c("ward.x", "ward.y"))) %>%
  filter(!is.na(ward)) %>%
  st_drop_geometry()
if (anyDuplicated(permits_pre_2015_ward$id) > 0) {
  stop("Pre-2015 ward spatial join assigned a permit to multiple wards.", call. = FALSE)
}

permits_2015_2023_ward <- permits %>%
  filter(
    application_start_date_ym >= as.yearmon("2015-05"),
    application_start_date_ym < as.yearmon("2023-05")
  ) %>%
  st_join(ward_geoms_2016, join = st_within) %>%
  mutate(ward = ward.y) %>%
  select(-any_of(c("ward.x", "ward.y"))) %>%
  filter(!is.na(ward)) %>%
  st_drop_geometry()
if (anyDuplicated(permits_2015_2023_ward$id) > 0) {
  stop("2015-2023 ward spatial join assigned a permit to multiple wards.", call. = FALSE)
}

permits_post_2023_ward <- permits %>%
  filter(application_start_date_ym >= as.yearmon("2023-05")) %>%
  st_join(ward_geoms_latest, join = st_within) %>%
  mutate(ward = ward.y) %>%
  select(-any_of(c("ward.x", "ward.y"))) %>%
  filter(!is.na(ward)) %>%
  st_drop_geometry()
if (anyDuplicated(permits_post_2023_ward$id) > 0) {
  stop("Post-2023 ward spatial join assigned a permit to multiple wards.", call. = FALSE)
}

permits_ward_data <- bind_rows(permits_pre_2015_ward, permits_2015_2023_ward, permits_post_2023_ward)
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
  transmute(id, application_date = as.Date(application_start_date)) %>%
  semi_join(permits_with_controls %>% select(id), by = "id")

metric_crs <- 26916
permit_points_m <- st_transform(permit_points, metric_crs)
cta_stations_m <- st_transform(cta_stations, metric_crs)
water_osm_m <- st_transform(water_osm, metric_crs)
cbd_m <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) %>%
  st_transform(metric_crs)

dist_cbd_km <- as.numeric(units::set_units(st_distance(permit_points_m, cbd_m), "m")) / 1000

network_change_dates <- sort(unique(c(
  cta_stations_m$active_from_date,
  cta_stations_m$active_to_date + 1
)))
network_change_dates <- network_change_dates[!is.na(network_change_dates)]
network_group <- findInterval(
  as.numeric(permit_points_m$application_date),
  as.numeric(network_change_dates)
)
permit_rows <- split(seq_len(nrow(permit_points_m)), network_group)
n_rail_stations_800m <- integer(nrow(permit_points_m))

for (network_i in names(permit_rows)) {
  row_i <- permit_rows[[network_i]]
  application_date_i <- permit_points_m$application_date[row_i[1]]
  active_cta <- cta_stations_m %>%
    filter(
      active_from_date <= application_date_i,
      is.na(active_to_date) | active_to_date >= application_date_i
    )
  if (nrow(active_cta) == 0) {
    stop(sprintf("No active CTA stations on %s.", application_date_i), call. = FALSE)
  }
  n_rail_stations_800m[row_i] <- lengths(st_is_within_distance(
    permit_points_m[row_i, ],
    active_cta,
    dist = 800
  ))
}

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

score_df <- read_csv(
  sprintf("../input/alderman_uncertainty_index_%s.csv", spec),
  show_col_types = FALSE
) %>%
  transmute(alderman, uncertainty_index)
if (anyDuplicated(score_df$alderman) > 0) {
  stop("Scores must be unique by alderman.", call. = FALSE)
}

analysis_df <- score_df %>%
  inner_join(alderman_wide, by = "alderman", relationship = "one-to-one") %>%
  filter(
    is.finite(uncertainty_index),
    is.finite(mean_resid_low),
    is.finite(mean_log_low),
    is.finite(mean_days_low),
    is.finite(n_low),
    n_low > 0
  )

model <- feols(
  mean_resid_low ~ uncertainty_index,
  data = analysis_df,
  se = "hetero",
  warn = FALSE
)

coef_table <- coeftable(model)
p_value <- unname(coef_table["uncertainty_index", grep("^Pr\\(", colnames(coef_table), value = TRUE)[1]])
estimate <- unname(coef_table["uncertainty_index", "Estimate"])
std_error <- unname(coef_table["uncertainty_index", "Std. Error"])

estimate_display <- if (is.finite(estimate)) formatC(estimate, digits = 3, format = "f") else ""
se_display <- if (is.finite(std_error)) formatC(std_error, digits = 3, format = "f") else ""
stars_display <- case_when(
  is.na(p_value) ~ "",
  p_value < 0.01 ~ "***",
  p_value < 0.05 ~ "**",
  p_value < 0.10 ~ "*",
  TRUE ~ ""
)

writeLines(
  c(
    "\\begingroup",
    "\\centering",
    "\\begin{tabular}{lc}",
    "\\toprule",
    " & Low-discretion log days \\\\",
    " & (1) \\\\",
    "\\midrule",
    sprintf(
      "Stringency index & %s \\\\",
      paste0(estimate_display, stars_display)
    ),
    sprintf(
      " & %s \\\\",
      paste0("(", se_display, ")")
    ),
    "\\midrule",
    sprintf("Alderman observations & %s \\\\", format(nobs(model), big.mark = ",")),
    "Heteroskedasticity-robust SEs & Yes \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    paste0(
      "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: Regresses the alderman-level mean residualized low-discretion log processing time on the main standardized stringency score. ",
      "Standard errors are heteroskedasticity-robust. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
    ),
    "\\endgroup"
  ),
  sprintf("../output/low_discretion_falsification_compact_uncertainty_%s.tex", spec)
)
