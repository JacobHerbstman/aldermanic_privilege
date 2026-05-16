source("../../setup_environment/code/packages.R")
source("../../_lib/alderman_uncertainty_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/within_ward_strictness/code")
# spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"
# building_permits_input <- "../input/building_permits_clean.gpkg"
# ward_panel_input <- "../input/ward_panel.gpkg"
# alderman_panel_input <- "../input/chicago_alderman_panel.csv"
# ward_controls_input <- "../input/ward_controls.csv"
# cta_stations_input <- "../input/cta_stations.geojson"
# water_osm_input <- "../input/gis_osm_water_a_free_1.shp"
# output_wide_csv <- "../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# output_summary_csv <- "../output/residualized_low_vs_high_processing_correlation_summary_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# output_scatter_pdf <- "../output/residualized_low_vs_high_processing_scatter_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    spec,
    building_permits_input,
    ward_panel_input,
    alderman_panel_input,
    ward_controls_input,
    cta_stations_input,
    water_osm_input,
    output_wide_csv,
    output_summary_csv,
    output_scatter_pdf
  )
}

if (length(args) != 10) {
  stop(
    paste(
      "FATAL: Script requires 10 args:",
      "<spec> <building_permits_input> <ward_panel_input> <alderman_panel_input>",
      "<ward_controls_input> <cta_stations_input> <water_osm_input>",
      "<output_wide_csv> <output_summary_csv> <output_scatter_pdf>"
    ),
    call. = FALSE
  )
}

spec <- args[1]
building_permits_input <- args[2]
ward_panel_input <- args[3]
alderman_panel_input <- args[4]
ward_controls_input <- args[5]
cta_stations_input <- args[6]
water_osm_input <- args[7]
output_wide_csv <- args[8]
output_summary_csv <- args[9]
output_scatter_pdf <- args[10]

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

weighted_cor <- function(x, y, w) {
  keep <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(keep) < 2) {
    return(NA_real_)
  }

  x <- x[keep]
  y <- y[keep]
  w <- w[keep]
  w <- w / sum(w)
  x_centered <- x - sum(w * x)
  y_centered <- y - sum(w * y)
  cov_xy <- sum(w * x_centered * y_centered)
  var_x <- sum(w * x_centered^2)
  var_y <- sum(w * y_centered^2)

  if (!is.finite(var_x) || !is.finite(var_y) || var_x <= 0 || var_y <= 0) {
    return(NA_real_)
  }

  cov_xy / sqrt(var_x * var_y)
}

cor_row <- function(df, correlation_type, x_col, y_col, weights = NULL) {
  if (nrow(df) < 2) {
    return(tibble(correlation = correlation_type, n_aldermen = nrow(df), estimate = NA_real_))
  }

  estimate <- if (is.null(weights)) {
    cor(df[[x_col]], df[[y_col]], method = correlation_type, use = "complete.obs")
  } else {
    weighted_cor(df[[x_col]], df[[y_col]], df[[weights]])
  }

  tibble(correlation = correlation_type, n_aldermen = nrow(df), estimate = estimate)
}

message("=== Residualized High-vs-Low Processing-Time Correlation Check ===")
message("Spec: ", spec)
message("Through-year cutoff: ", max_permit_year)

ward_panel <- st_read(ward_panel_input, quiet = TRUE)
ward_controls <- read_csv(ward_controls_input, show_col_types = FALSE)
alderman_panel <- read_csv(alderman_panel_input, show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))
if (anyDuplicated(alderman_panel[c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}
if (anyDuplicated(ward_controls[c("ward", "year")]) > 0) {
  stop("Ward controls must be unique by ward-year.", call. = FALSE)
}

cta_stations <- st_read(cta_stations_input, quiet = TRUE)
water_osm <- st_read(water_osm_input, quiet = TRUE)

if (st_crs(cta_stations) != st_crs(ward_panel)) {
  cta_stations <- st_transform(cta_stations, st_crs(ward_panel))
}
if (st_crs(water_osm) != st_crs(ward_panel)) {
  water_osm <- st_transform(water_osm, st_crs(ward_panel))
}

permits <- st_read(building_permits_input, quiet = TRUE) %>%
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

permits_pre2015 <- permits %>%
  filter(application_start_date_ym < as.yearmon("2015-05"))

permits_2015_2022 <- permits %>%
  filter(application_start_date_ym >= as.yearmon("2015-05"))

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

permits_ward_2015_2022 <- if (nrow(permits_2015_2022) == 0) {
  permits_2015_2022 %>% st_drop_geometry()
} else {
  st_join(permits_2015_2022, ward_geoms_map2, join = st_within) %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}
if (anyDuplicated(permits_ward_2015_2022$id) > 0) {
  stop("2015-2022 ward spatial join assigned a permit to multiple wards.", call. = FALSE)
}

permits_ward_data <- bind_rows(permits_ward_pre2015, permits_ward_2015_2022)

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

write_csv(alderman_wide, output_wide_csv)

summary_rows <- bind_rows(
  cor_row(alderman_wide, "pearson", "mean_resid_high", "mean_resid_low"),
  cor_row(alderman_wide, "spearman", "mean_resid_high", "mean_resid_low"),
  cor_row(alderman_wide, "weighted_pearson_harmonic", "mean_resid_high", "mean_resid_low", "harmonic_weight")
)

write_csv(summary_rows, output_summary_csv)

plot_pearson <- summary_rows %>%
  filter(correlation == "pearson") %>%
  pull(estimate)

annotation_text <- paste0(
  "N = ", nrow(alderman_wide),
  "\nPearson r = ", formatC(plot_pearson, digits = 3, format = "f")
)

p_scatter <- ggplot(alderman_wide, aes(x = mean_resid_high, y = mean_resid_low)) +
  geom_point(color = "#2C7FB8", size = 2.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, color = "#D95F0E", linewidth = 0.7) +
  annotate(
    "text",
    x = Inf,
    y = -Inf,
    label = annotation_text,
    hjust = 1.02,
    vjust = -0.1,
    size = 3.6
  ) +
  labs(
    x = "Mean residual log days, high-discretion permits",
    y = "Mean residual log days, low-discretion permits",
    title = "Alderman Mean Residual Processing Times: High vs. Low Discretion",
    subtitle = "Residuals come from the baseline stage-1 controls with permit-type, review-type, and month fixed effects"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

ggsave(output_scatter_pdf, p_scatter, width = 7, height = 5)

message("Saved: ", output_wide_csv)
message("Saved: ", output_summary_csv)
message("Saved: ", output_scatter_pdf)
