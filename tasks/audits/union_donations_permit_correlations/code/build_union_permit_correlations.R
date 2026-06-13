# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/union_donations_permit_correlations/code")

source("../../../setup_environment/code/packages.R")

max_construction_year <- 2026
max_construction_month <- "2026-04"
start_analysis_year <- 2006
y_trim_percentile <- 0.975

normalize_name <- function(value) {
  text <- ifelse(is.na(value), "", as.character(value))
  text <- iconv(text, from = "", to = "UTF-8", sub = " ")
  text <- tolower(gsub("&", " and ", text, fixed = TRUE))
  text <- gsub("[^a-z0-9 ]+", " ", text)
  trimws(gsub("\\s+", " ", text))
}

validate_unique <- function(df, keys, label) {
  if (anyDuplicated(df[keys]) > 0) {
    stop(sprintf("%s must be unique by %s.", label, paste(keys, collapse = ", ")), call. = FALSE)
  }
}

safe_cor <- function(df, y_var, x_var, method) {
  sample_df <- df %>%
    filter(is.finite(.data[[y_var]]), is.finite(.data[[x_var]]))

  if (nrow(sample_df) < 3 || sd(sample_df[[y_var]]) == 0 || sd(sample_df[[x_var]]) == 0) {
    return(tibble(
      outcome = y_var,
      measure = x_var,
      method = method,
      n = nrow(sample_df),
      estimate = NA_real_,
      p_value = NA_real_
    ))
  }

  test <- suppressWarnings(cor.test(sample_df[[y_var]], sample_df[[x_var]], method = method, exact = FALSE))
  tibble(
    outcome = y_var,
    measure = x_var,
    method = method,
    n = nrow(sample_df),
    estimate = unname(test$estimate),
    p_value = test$p.value
  )
}

max_construction_month_date <- as.Date(paste0(max_construction_month, "-15"))

union_donations <- read_csv("../input/alderman_union_donations_main.csv", show_col_types = FALSE) %>%
  mutate(alderman_key = normalize_name(full_name)) %>%
  select(
    alderman_id,
    full_name,
    alderman_key,
    first_cycle_year,
    last_cycle_year,
    cycles_count,
    ward_numbers,
    total_receipts_amount,
    union_total_amount,
    union_share_total_receipts,
    construction_trades_amount,
    construction_trades_share_total_receipts,
    construction_trades_share_union_total,
    teacher_education_share_total_receipts,
    public_sector_service_share_total_receipts,
    generic_labor_share_total_receipts
  )
validate_unique(union_donations, "alderman_key", "union donation aggregate")

cycle_union_categories <- read_csv(
  "../input/alderman_cycle_union_category_aggregates_reviewed_named_recipient.csv",
  show_col_types = FALSE
) %>%
  mutate(
    alderman_key = normalize_name(full_name),
    cycle_year = as.integer(cycle_year),
    total_receipts_amount = as.numeric(total_receipts_amount),
    construction_trades_amount = as.numeric(construction_trades_amount),
    cycle_observation_date = as.Date(paste0(cycle_year, "-06-01"))
  )
validate_unique(cycle_union_categories, c("cycle_year", "alderman_key"), "cycle union category aggregate")

cpi_lookup <- read_csv("../input/fred_cpi_cuura207sa0.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  transmute(
    observation_date = as.Date(observation_date),
    cpi_value = suppressWarnings(as.numeric(CUURA207SA0))
  ) %>%
  filter(!is.na(observation_date))
validate_unique(cpi_lookup, "observation_date", "CPI input")

cpi <- tibble(
  observation_date = seq(
    min(cycle_union_categories$cycle_observation_date, na.rm = TRUE),
    max(cycle_union_categories$cycle_observation_date, na.rm = TRUE),
    by = "month"
  )
) %>%
  left_join(cpi_lookup, by = "observation_date", relationship = "one-to-one") %>%
  arrange(observation_date)

if (anyNA(cpi$cpi_value)) {
  idx_known <- which(!is.na(cpi$cpi_value))
  if (length(idx_known) < 2) {
    stop("Not enough non-missing CPI observations to interpolate.", call. = FALSE)
  }
  cpi$cpi_value <- if_else(
    is.na(cpi$cpi_value),
    approx(x = idx_known, y = cpi$cpi_value[idx_known], xout = seq_len(nrow(cpi)), method = "linear", rule = 1)$y,
    cpi$cpi_value
  )
}
if (anyNA(cpi$cpi_value)) {
  stop("CPI has unresolved endpoint gaps after interpolation.", call. = FALSE)
}

cpi_2022 <- cpi_lookup %>%
  filter(format(observation_date, "%Y") == "2022") %>%
  pull(cpi_value)
if (length(cpi_2022) == 0 || !all(is.finite(cpi_2022))) {
  stop("Unable to compute base CPI for year 2022.", call. = FALSE)
}
base_cpi <- mean(cpi_2022)
if (!is.finite(base_cpi) || base_cpi <= 0) {
  stop("Computed invalid base CPI for year 2022.", call. = FALSE)
}

trade_real_by_alderman <- cycle_union_categories %>%
  left_join(
    cpi %>%
      transmute(
        cycle_observation_date = observation_date,
        deflator_to_2022 = base_cpi / cpi_value
      ),
    by = "cycle_observation_date",
    relationship = "many-to-one"
  ) %>%
  group_by(alderman_key) %>%
  summarise(
    cycle_count_real_trade = n_distinct(cycle_year),
    cycle_years_real_trade = 4 * cycle_count_real_trade,
    construction_trades_real_2022_amount = sum(construction_trades_amount * deflator_to_2022, na.rm = TRUE),
    total_receipts_real_2022_amount = sum(total_receipts_amount * deflator_to_2022, na.rm = TRUE),
    construction_trades_real_2022_per_cycle_year = construction_trades_real_2022_amount / cycle_years_real_trade,
    total_receipts_real_2022_per_cycle_year = total_receipts_real_2022_amount / cycle_years_real_trade,
    .groups = "drop"
  )
validate_unique(trade_real_by_alderman, "alderman_key", "real trade dollar aggregate")

ward_controls <- read_csv("../input/ward_controls_2000_2023.csv", show_col_types = FALSE) %>%
  mutate(
    ward = as.integer(ward),
    year = as.integer(year),
    pop_total = as.numeric(pop_total),
    median_hh_income = as.numeric(median_hh_income),
    share_black = as.numeric(share_black),
    share_hisp = as.numeric(share_hisp),
    share_white = as.numeric(share_white),
    homeownership_rate = as.numeric(homeownership_rate)
  ) %>%
  select(
    ward,
    year,
    pop_total,
    median_hh_income,
    share_black,
    share_hisp,
    share_white,
    homeownership_rate
  )

max_control_year <- max(ward_controls$year, na.rm = TRUE)
if (max_construction_year > max_control_year) {
  ward_controls <- bind_rows(
    ward_controls,
    ward_controls %>%
      filter(year == max_control_year) %>%
      select(-year) %>%
      tidyr::crossing(year = (max_control_year + 1):max_construction_year)
  )
}
validate_unique(ward_controls, c("ward", "year"), "ward controls")

permits <- data.table::fread(
  "../input/permits_for_uncertainty_index.csv",
  select = c(
    "alderman",
    "month",
    "year",
    "permit_type_clean",
    "processing_time",
    "reported_cost",
    "dist_cbd_km",
    "dist_lake_km",
    "n_rail_stations_800m"
  ),
  showProgress = FALSE
) %>%
  as_tibble() %>%
  mutate(
    alderman_key = normalize_name(alderman),
    year = as.integer(year),
    processing_time = as.numeric(processing_time),
    reported_cost = as.numeric(reported_cost),
    dist_cbd_km = as.numeric(dist_cbd_km),
    dist_lake_km = as.numeric(dist_lake_km),
    n_rail_stations_800m = as.numeric(n_rail_stations_800m),
    permit_type_clean = as.character(permit_type_clean)
  ) %>%
  filter(year >= start_analysis_year, year <= max_construction_year)

permit_measures <- permits %>%
  group_by(alderman_key) %>%
  summarise(
    permit_alderman_name = min(alderman, na.rm = TRUE),
    first_permit_year = min(year, na.rm = TRUE),
    last_permit_year = max(year, na.rm = TRUE),
    permit_years_observed = n_distinct(year),
    high_discretion_permits = n(),
    high_discretion_permits_per_observed_year = high_discretion_permits / permit_years_observed,
    new_construction_permits = sum(permit_type_clean == "new_construction", na.rm = TRUE),
    new_construction_permits_per_observed_year = new_construction_permits / permit_years_observed,
    high_discretion_mean_processing_time = mean(processing_time, na.rm = TRUE),
    high_discretion_median_processing_time = median(processing_time, na.rm = TRUE),
    new_construction_mean_processing_time = mean(processing_time[permit_type_clean == "new_construction"], na.rm = TRUE),
    new_construction_median_processing_time = median(processing_time[permit_type_clean == "new_construction"], na.rm = TRUE),
    high_discretion_reported_cost = sum(reported_cost, na.rm = TRUE),
    new_construction_reported_cost = sum(if_else(permit_type_clean == "new_construction", reported_cost, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    new_construction_mean_processing_time = if_else(is.nan(new_construction_mean_processing_time), NA_real_, new_construction_mean_processing_time),
    new_construction_median_processing_time = if_else(is.nan(new_construction_median_processing_time), NA_real_, new_construction_median_processing_time)
  )
validate_unique(permit_measures, "alderman_key", "permit aggregate")

permit_place_year_controls <- permits %>%
  group_by(alderman_key, year) %>%
  summarise(
    year_dist_cbd_km = mean(dist_cbd_km, na.rm = TRUE),
    year_dist_lake_km = mean(dist_lake_km, na.rm = TRUE),
    year_n_rail_stations_800m = mean(n_rail_stations_800m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year_dist_cbd_km = if_else(is.nan(year_dist_cbd_km), NA_real_, year_dist_cbd_km),
    year_dist_lake_km = if_else(is.nan(year_dist_lake_km), NA_real_, year_dist_lake_km),
    year_n_rail_stations_800m = if_else(is.nan(year_n_rail_stations_800m), NA_real_, year_n_rail_stations_800m)
  )
validate_unique(permit_place_year_controls, c("alderman_key", "year"), "permit place controls by alderman-year")

parcels <- st_read("../input/geocoded_residential_data.gpkg", quiet = TRUE) %>%
  filter(
    !is.na(yearbuilt),
    yearbuilt >= start_analysis_year,
    yearbuilt <= max_construction_year
  ) %>%
  mutate(
    parcel_row_id = row_number(),
    construction_date = if_else(
      yearbuilt == max_construction_year,
      max_construction_month_date,
      as.Date(paste0(yearbuilt, "-06-15"))
    )
  )

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
if (st_crs(parcels) != st_crs(ward_panel)) {
  parcels <- st_transform(parcels, st_crs(ward_panel))
}

ward_geoms_2014 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
ward_geoms_2016 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
ward_geoms_latest <- ward_panel %>%
  filter(year == max(year)) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")

if (any(c(nrow(ward_geoms_2014), nrow(ward_geoms_2016), nrow(ward_geoms_latest)) != 50)) {
  stop("Expected 50 ward geometries for each map year.", call. = FALSE)
}

parcels_pre2015 <- parcels %>%
  filter(construction_date < as.Date("2015-05-01")) %>%
  st_join(ward_geoms_2014, join = st_within) %>%
  rename(assigned_ward = ward) %>%
  filter(!is.na(assigned_ward))
parcels_2015_2023 <- parcels %>%
  filter(construction_date >= as.Date("2015-05-01"), construction_date < as.Date("2023-05-01")) %>%
  st_join(ward_geoms_2016, join = st_within) %>%
  rename(assigned_ward = ward) %>%
  filter(!is.na(assigned_ward))
parcels_post2023 <- parcels %>%
  filter(construction_date >= as.Date("2023-05-01")) %>%
  st_join(ward_geoms_latest, join = st_within) %>%
  rename(assigned_ward = ward) %>%
  filter(!is.na(assigned_ward))

parcels_assigned <- bind_rows(parcels_pre2015, parcels_2015_2023, parcels_post2023)
if (anyDuplicated(parcels_assigned$parcel_row_id) > 0) {
  stop("Parcel ward assignment produced duplicate parcel rows.", call. = FALSE)
}

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(
    month_yearmon = as.yearmon(month, format = "%b %Y"),
    yearmon_key = as.character(month_yearmon),
    panel_year = as.integer(format(as.Date(month_yearmon), "%Y")),
    alderman_key = normalize_name(alderman)
  ) %>%
  select(ward, yearmon_key, month_yearmon, panel_year, alderman, alderman_key)
validate_unique(alderman_panel, c("ward", "yearmon_key"), "alderman panel")

episode_windows <- union_donations %>%
  transmute(
    alderman_key,
    episode_start_year = first_cycle_year,
    episode_end_year = pmin(last_cycle_year + 3, max_construction_year)
  )
validate_unique(episode_windows, "alderman_key", "alderman episode windows")

episode_ward_controls <- alderman_panel %>%
  left_join(episode_windows, by = "alderman_key", relationship = "many-to-one") %>%
  filter(
    !is.na(episode_start_year),
    panel_year >= episode_start_year,
    panel_year <= episode_end_year
  ) %>%
  left_join(
    ward_controls,
    by = c("ward", "panel_year" = "year"),
    relationship = "many-to-one"
  )

if (any(is.na(episode_ward_controls$median_hh_income))) {
  stop("Episode ward controls include rows without ward controls.", call. = FALSE)
}

episode_place_controls <- permit_place_year_controls %>%
  left_join(episode_windows, by = "alderman_key", relationship = "many-to-one") %>%
  filter(
    !is.na(episode_start_year),
    year >= episode_start_year,
    year <= episode_end_year
  ) %>%
  group_by(alderman_key) %>%
  summarise(
    ctrl_dist_cbd_km = mean(year_dist_cbd_km, na.rm = TRUE),
    ctrl_dist_lake_km = mean(year_dist_lake_km, na.rm = TRUE),
    ctrl_n_rail_stations_800m = mean(year_n_rail_stations_800m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ctrl_dist_cbd_km = if_else(is.nan(ctrl_dist_cbd_km), NA_real_, ctrl_dist_cbd_km),
    ctrl_dist_lake_km = if_else(is.nan(ctrl_dist_lake_km), NA_real_, ctrl_dist_lake_km),
    ctrl_n_rail_stations_800m = if_else(is.nan(ctrl_n_rail_stations_800m), NA_real_, ctrl_n_rail_stations_800m)
  )
validate_unique(episode_place_controls, "alderman_key", "episode place controls")

episode_controls <- episode_ward_controls %>%
  group_by(alderman_key) %>%
  summarise(
    tenure_months = n_distinct(yearmon_key),
    tenure_years = tenure_months / 12,
    ctrl_median_hh_income = mean(median_hh_income, na.rm = TRUE),
    ctrl_share_black = mean(share_black, na.rm = TRUE),
    ctrl_share_hisp = mean(share_hisp, na.rm = TRUE),
    ctrl_share_white = mean(share_white, na.rm = TRUE),
    ctrl_homeownership_rate = mean(homeownership_rate, na.rm = TRUE),
    ctrl_pop_total = mean(pop_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(episode_place_controls, by = "alderman_key", relationship = "one-to-one") %>%
  mutate(
    ctrl_median_hh_income_10k = ctrl_median_hh_income / 10000,
    ctrl_pop_total_10k = ctrl_pop_total / 10000
  )
validate_unique(episode_controls, "alderman_key", "episode controls")

parcel_measures <- parcels_assigned %>%
  st_drop_geometry() %>%
  mutate(
    construction_yearmon = as.yearmon(construction_date),
    yearmon_key = as.character(construction_yearmon),
    density_far = if_else(arealotsf > 0, areabuilding / arealotsf, NA_real_),
    density_dupac = if_else(arealotsf > 0 & unitscount > 0, 43560 * unitscount / arealotsf, NA_real_)
  ) %>%
  left_join(
    alderman_panel,
    by = c("assigned_ward" = "ward", "yearmon_key"),
    relationship = "many-to-one"
  ) %>%
  filter(!is.na(alderman)) %>%
  mutate(alderman_key = normalize_name(alderman)) %>%
  group_by(alderman_key) %>%
  summarise(
    parcel_alderman_name = min(alderman, na.rm = TRUE),
    new_buildings = n(),
    new_units = sum(unitscount, na.rm = TRUE),
    new_units_per_building = new_units / new_buildings,
    new_building_square_feet = sum(areabuilding, na.rm = TRUE),
    new_land_square_feet = sum(arealotsf, na.rm = TRUE),
    aggregate_far = new_building_square_feet / new_land_square_feet,
    aggregate_dupac = 43560 * new_units / new_land_square_feet,
    mean_building_far = mean(density_far, na.rm = TRUE),
    median_building_far = median(density_far, na.rm = TRUE),
    mean_building_dupac = mean(density_dupac, na.rm = TRUE),
    median_building_dupac = median(density_dupac, na.rm = TRUE),
    .groups = "drop"
  )
validate_unique(parcel_measures, "alderman_key", "residential-commercial parcel aggregate")

joined <- union_donations %>%
  left_join(permit_measures, by = "alderman_key", relationship = "one-to-one") %>%
  left_join(trade_real_by_alderman, by = "alderman_key", relationship = "one-to-one") %>%
  left_join(episode_controls, by = "alderman_key", relationship = "one-to-one") %>%
  left_join(parcel_measures, by = "alderman_key", relationship = "one-to-one") %>%
  mutate(
    log_real_total_receipts_per_cycle_year = log(1 + total_receipts_real_2022_per_cycle_year)
  ) %>%
  arrange(desc(union_share_total_receipts), full_name)
validate_unique(joined, "alderman_key", "union-permit-density audit join")

unmatched <- joined %>%
  filter(is.na(permit_alderman_name) | is.na(parcel_alderman_name)) %>%
  transmute(
    alderman_id,
    full_name,
    first_cycle_year,
    last_cycle_year,
    total_receipts_amount,
    union_share_total_receipts,
    matched_to_permits = !is.na(permit_alderman_name),
    matched_to_residential_commercial_parcels = !is.na(parcel_alderman_name)
  ) %>%
  arrange(full_name)

summary_stats <- joined %>%
  summarise(
    aldermen_in_union_data = n(),
    aldermen_matched_to_permit_data = sum(!is.na(permit_alderman_name)),
    aldermen_matched_to_residential_commercial_parcels = sum(!is.na(parcel_alderman_name)),
    mean_union_share = mean(union_share_total_receipts, na.rm = TRUE),
    median_union_share = median(union_share_total_receipts, na.rm = TRUE),
    min_union_share = min(union_share_total_receipts, na.rm = TRUE),
    max_union_share = max(union_share_total_receipts, na.rm = TRUE),
    mean_high_discretion_permits = mean(high_discretion_permits, na.rm = TRUE),
    median_high_discretion_permits = median(high_discretion_permits, na.rm = TRUE),
    mean_new_construction_permits = mean(new_construction_permits, na.rm = TRUE),
    median_new_construction_permits = median(new_construction_permits, na.rm = TRUE),
    mean_new_units = mean(new_units, na.rm = TRUE),
    median_new_units = median(new_units, na.rm = TRUE),
    mean_aggregate_far = mean(aggregate_far, na.rm = TRUE),
    mean_aggregate_dupac = mean(aggregate_dupac, na.rm = TRUE)
  )

measure_names <- c(
  "high_discretion_permits",
  "high_discretion_permits_per_observed_year",
  "new_construction_permits",
  "new_construction_permits_per_observed_year",
  "high_discretion_mean_processing_time",
  "high_discretion_median_processing_time",
  "new_construction_mean_processing_time",
  "new_construction_median_processing_time",
  "high_discretion_reported_cost",
  "new_construction_reported_cost",
  "new_buildings",
  "new_units",
  "new_units_per_building",
  "new_building_square_feet",
  "aggregate_far",
  "aggregate_dupac",
  "mean_building_far",
  "median_building_far",
  "mean_building_dupac",
  "median_building_dupac"
)

correlations <- bind_rows(lapply(measure_names, function(measure_name) {
  bind_rows(
    safe_cor(joined, "union_share_total_receipts", measure_name, "pearson"),
    safe_cor(joined, "union_share_total_receipts", measure_name, "spearman")
  )
})) %>%
  mutate(
    estimate = round(estimate, 4),
    p_value = round(p_value, 4)
  ) %>%
  arrange(method, desc(abs(estimate)), measure)

write_csv(correlations, "../output/union_donations_permit_correlations.csv")

percent_label <- function(value) {
  paste0(round(100 * value), "%")
}

correlation_labels <- correlations %>%
  select(measure, method, n, estimate) %>%
  tidyr::pivot_wider(names_from = method, values_from = c(n, estimate)) %>%
  mutate(
    correlation_label = paste0(
      "Pearson r = ", sprintf("%.2f", estimate_pearson),
      "\nSpearman r = ", sprintf("%.2f", estimate_spearman),
      "\nN = ", n_pearson
    )
  ) %>%
  select(measure, correlation_label)

y_trim_subtitle <- "Y-axis trims panel values above the 97.5th percentile; fit and correlations use visible points"

trim_y_by_measure <- function(plot_data) {
  plot_data %>%
    group_by(measure) %>%
    mutate(y_trim_cutoff = quantile(value, probs = y_trim_percentile, na.rm = TRUE, names = FALSE)) %>%
    ungroup() %>%
    filter(value <= y_trim_cutoff) %>%
    select(-y_trim_cutoff)
}

trimmed_union_correlation_labels <- function(plot_data) {
  bind_rows(lapply(unique(plot_data$measure), function(measure_name) {
    sample_df <- plot_data %>%
      filter(measure == measure_name)

    pearson <- NA_real_
    spearman <- NA_real_
    if (
      nrow(sample_df) >= 3 &&
        sd(sample_df$union_share_total_receipts) > 0 &&
        sd(sample_df$value) > 0
    ) {
      pearson <- unname(suppressWarnings(cor.test(
        sample_df$union_share_total_receipts,
        sample_df$value,
        method = "pearson",
        exact = FALSE
      )$estimate))
      spearman <- unname(suppressWarnings(cor.test(
        sample_df$union_share_total_receipts,
        sample_df$value,
        method = "spearman",
        exact = FALSE
      )$estimate))
    }

    tibble(
      measure = measure_name,
      measure_label = sample_df$measure_label[1],
      correlation_label = paste0(
        "Pearson r = ", sprintf("%.2f", pearson),
        "\nSpearman r = ", sprintf("%.2f", spearman),
        "\nN = ", nrow(sample_df)
      )
    )
  }))
}

plot_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(color = "gray15"),
    plot.title = element_text(face = "bold", color = "gray10"),
    plot.subtitle = element_text(color = "gray30"),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

score_specs <- tibble(
  score_spec = c("Paper score through 2022", "Extended score through 2026-04"),
  score_order = c(1L, 2L)
)

stringency_scores <- bind_rows(
  read_csv(
    "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
    show_col_types = FALSE
  ) %>%
    mutate(score_spec = "Paper score through 2022", score_order = 1L),
  read_csv(
    "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through202604.csv",
    show_col_types = FALSE
  ) %>%
    mutate(score_spec = "Extended score through 2026-04", score_order = 2L)
) %>%
  mutate(alderman_key = normalize_name(alderman)) %>%
  select(
    score_spec,
    score_order,
    alderman_key,
    score_alderman_name = alderman,
    score_n_permits = n_permits,
    score_mean_resid = mean_resid,
    score_alderman_fe_raw = alderman_fe_raw,
    score_alderman_se = alderman_se,
    score_shrinkage_B = shrinkage_B,
    uncertainty_index
  )

if (anyDuplicated(stringency_scores[c("score_spec", "alderman_key")]) > 0) {
  stop("Stringency score inputs must be unique by score_spec and alderman.", call. = FALSE)
}

stringency_join <- joined %>%
  select(
    alderman_id,
    full_name,
    alderman_key,
    first_cycle_year,
    last_cycle_year,
    total_receipts_amount,
    union_total_amount,
    union_share_total_receipts,
    construction_trades_amount,
    construction_trades_share_total_receipts,
    construction_trades_share_union_total,
    construction_trades_real_2022_amount,
    construction_trades_real_2022_per_cycle_year,
    total_receipts_real_2022_amount,
    total_receipts_real_2022_per_cycle_year,
    log_real_total_receipts_per_cycle_year,
    tenure_years,
    ctrl_median_hh_income_10k,
    ctrl_share_black,
    ctrl_share_hisp,
    ctrl_share_white,
    ctrl_homeownership_rate,
    ctrl_pop_total_10k,
    ctrl_dist_cbd_km,
    ctrl_dist_lake_km,
    ctrl_n_rail_stations_800m
  ) %>%
  tidyr::crossing(score_specs) %>%
  left_join(
    stringency_scores,
    by = c("alderman_key", "score_spec", "score_order"),
    relationship = "one-to-one"
  ) %>%
  arrange(score_order, desc(union_share_total_receipts), full_name)

stringency_regressions <- bind_rows(lapply(score_specs$score_spec, function(score_spec_i) {
  sample_i <- stringency_join %>%
    filter(
      score_spec == score_spec_i,
      is.finite(union_share_total_receipts),
      is.finite(uncertainty_index)
    )

  if (nrow(sample_i) < 3 || sd(sample_i$union_share_total_receipts) == 0 || sd(sample_i$uncertainty_index) == 0) {
    return(tibble(
      score_spec = score_spec_i,
      n = nrow(sample_i),
      coefficient_union_share = NA_real_,
      coefficient_union_share_10pp = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_,
      pearson = NA_real_,
      spearman = NA_real_
    ))
  }

  fit_i <- lm(uncertainty_index ~ union_share_total_receipts, data = sample_i)
  fit_summary_i <- summary(fit_i)
  coef_i <- fit_summary_i$coefficients["union_share_total_receipts", ]

  tibble(
    score_spec = score_spec_i,
    n = nrow(sample_i),
    coefficient_union_share = unname(coef_i[["Estimate"]]),
    coefficient_union_share_10pp = 0.1 * unname(coef_i[["Estimate"]]),
    std_error = unname(coef_i[["Std. Error"]]),
    p_value = unname(coef_i[["Pr(>|t|)"]]),
    r_squared = unname(fit_summary_i$r.squared),
    pearson = unname(cor(sample_i$union_share_total_receipts, sample_i$uncertainty_index, method = "pearson")),
    spearman = unname(cor(sample_i$union_share_total_receipts, sample_i$uncertainty_index, method = "spearman"))
  )
})) %>%
  left_join(score_specs, by = "score_spec", relationship = "one-to-one") %>%
  arrange(score_order) %>%
  select(-score_order)

stringency_plot_data <- stringency_join %>%
  filter(is.finite(union_share_total_receipts), is.finite(uncertainty_index)) %>%
  mutate(score_spec = factor(score_spec, levels = score_specs$score_spec))

stringency_plot_labels <- stringency_regressions %>%
  mutate(
    score_spec = factor(score_spec, levels = score_specs$score_spec),
    label = paste0(
      "Slope per 10pp = ", sprintf("%.2f", coefficient_union_share_10pp),
      "\nPearson r = ", sprintf("%.2f", pearson),
      "\nSpearman r = ", sprintf("%.2f", spearman),
      "\nN = ", n
    )
  )

stringency_plot <- ggplot(stringency_plot_data, aes(x = union_share_total_receipts, y = uncertainty_index)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = stringency_plot_labels,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.1,
    color = "gray20"
  ) +
  facet_wrap(~ score_spec) +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Union Donation Share and EB-Shrunk Stringency Score",
    subtitle = "Higher score means slower residualized permit processing after Stage 1 controls",
    x = "Union share of total receipts",
    y = "EB-shrunk stringency score"
  ) +
  plot_theme

ggsave("../output/union_share_stringency_score_scatter.pdf", stringency_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/union_share_stringency_score_scatter.png", stringency_plot, width = 9, height = 5.2, dpi = 300, bg = "white")
write_csv(stringency_join, "../output/union_share_stringency_score_join.csv")
write_csv(stringency_regressions, "../output/union_share_stringency_score_regression.csv")

trade_share_specs <- tibble(
  trade_share_var = c("construction_trades_share_total_receipts", "construction_trades_share_union_total"),
  trade_share_label = c("Construction-trades share of all receipts", "Construction-trades share of union receipts")
)

trade_stringency_join <- stringency_join %>%
  select(
    alderman_id,
    full_name,
    alderman_key,
    score_spec,
    score_order,
    uncertainty_index,
    construction_trades_share_total_receipts,
    construction_trades_share_union_total
  ) %>%
  tidyr::pivot_longer(
    cols = c(construction_trades_share_total_receipts, construction_trades_share_union_total),
    names_to = "trade_share_var",
    values_to = "trade_share"
  ) %>%
  left_join(trade_share_specs, by = "trade_share_var", relationship = "many-to-one") %>%
  arrange(score_order, trade_share_var, desc(trade_share), full_name)

trade_stringency_regressions <- bind_rows(lapply(score_specs$score_spec, function(score_spec_i) {
  bind_rows(lapply(trade_share_specs$trade_share_var, function(trade_share_var_i) {
    sample_i <- trade_stringency_join %>%
      filter(
        score_spec == score_spec_i,
        trade_share_var == trade_share_var_i,
        is.finite(trade_share),
        is.finite(uncertainty_index)
      )

    if (nrow(sample_i) < 3 || sd(sample_i$trade_share) == 0 || sd(sample_i$uncertainty_index) == 0) {
      return(tibble(
        score_spec = score_spec_i,
        trade_share_var = trade_share_var_i,
        n = nrow(sample_i),
        coefficient_trade_share = NA_real_,
        coefficient_trade_share_10pp = NA_real_,
        std_error = NA_real_,
        p_value = NA_real_,
        r_squared = NA_real_,
        pearson = NA_real_,
        spearman = NA_real_
      ))
    }

    fit_i <- lm(uncertainty_index ~ trade_share, data = sample_i)
    fit_summary_i <- summary(fit_i)
    coef_i <- fit_summary_i$coefficients["trade_share", ]

    tibble(
      score_spec = score_spec_i,
      trade_share_var = trade_share_var_i,
      n = nrow(sample_i),
      coefficient_trade_share = unname(coef_i[["Estimate"]]),
      coefficient_trade_share_10pp = 0.1 * unname(coef_i[["Estimate"]]),
      std_error = unname(coef_i[["Std. Error"]]),
      p_value = unname(coef_i[["Pr(>|t|)"]]),
      r_squared = unname(fit_summary_i$r.squared),
      pearson = unname(cor(sample_i$trade_share, sample_i$uncertainty_index, method = "pearson")),
      spearman = unname(cor(sample_i$trade_share, sample_i$uncertainty_index, method = "spearman"))
    )
  }))
})) %>%
  left_join(score_specs, by = "score_spec", relationship = "many-to-one") %>%
  left_join(trade_share_specs, by = "trade_share_var", relationship = "many-to-one") %>%
  arrange(score_order, trade_share_var) %>%
  select(-score_order)

trade_stringency_plot_data <- trade_stringency_join %>%
  filter(is.finite(trade_share), is.finite(uncertainty_index)) %>%
  mutate(
    score_spec = factor(score_spec, levels = score_specs$score_spec),
    trade_share_label = factor(trade_share_label, levels = trade_share_specs$trade_share_label)
  )

trade_stringency_plot_labels <- trade_stringency_regressions %>%
  mutate(
    score_spec = factor(score_spec, levels = score_specs$score_spec),
    trade_share_label = factor(trade_share_label, levels = trade_share_specs$trade_share_label),
    label = paste0(
      "Slope per 10pp = ", sprintf("%.2f", coefficient_trade_share_10pp),
      "\nPearson r = ", sprintf("%.2f", pearson),
      "\nSpearman r = ", sprintf("%.2f", spearman),
      "\nN = ", n
    )
  )

trade_stringency_plot <- ggplot(trade_stringency_plot_data, aes(x = trade_share, y = uncertainty_index)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = trade_stringency_plot_labels,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 2.7,
    color = "gray20"
  ) +
  facet_grid(score_spec ~ trade_share_label, scales = "free_x") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Construction-Trades Donation Share and EB-Shrunk Stringency Score",
    subtitle = "Higher score means slower residualized permit processing after Stage 1 controls",
    x = NULL,
    y = "EB-shrunk stringency score"
  ) +
  plot_theme

ggsave("../output/construction_trades_share_stringency_score_scatter.pdf", trade_stringency_plot, width = 10, height = 7, bg = "white")
ggsave("../output/construction_trades_share_stringency_score_scatter.png", trade_stringency_plot, width = 10, height = 7, dpi = 300, bg = "white")
write_csv(trade_stringency_join, "../output/construction_trades_share_stringency_score_join.csv")
write_csv(trade_stringency_regressions, "../output/construction_trades_share_stringency_score_regression.csv")

trade_share_score_data <- stringency_join %>%
  filter(
    is.finite(construction_trades_share_total_receipts),
    is.finite(uncertainty_index)
  ) %>%
  mutate(score_spec = factor(score_spec, levels = score_specs$score_spec))

trade_share_score_bins <- trade_share_score_data %>%
  group_by(score_spec) %>%
  arrange(construction_trades_share_total_receipts, full_name, .by_group = TRUE) %>%
  mutate(trade_share_bin = ntile(construction_trades_share_total_receipts, 10)) %>%
  group_by(score_spec, trade_share_bin) %>%
  summarise(
    n = n(),
    mean_construction_trades_share_total_receipts = mean(construction_trades_share_total_receipts),
    mean_uncertainty_index = mean(uncertainty_index),
    se_uncertainty_index = sd(uncertainty_index) / sqrt(n),
    ci_low = mean_uncertainty_index - 1.96 * se_uncertainty_index,
    ci_high = mean_uncertainty_index + 1.96 * se_uncertainty_index,
    .groups = "drop"
  )

trade_share_score_bins_plot <- ggplot(
  trade_share_score_bins,
  aes(x = mean_construction_trades_share_total_receipts, y = mean_uncertainty_index)
) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), color = "#276f86", width = 0.004, linewidth = 0.55) +
  geom_line(color = "#276f86", linewidth = 0.65) +
  geom_point(color = "#276f86", size = 2.4) +
  facet_wrap(~ score_spec) +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Binned Construction-Trades Share and EB-Shrunk Stringency Score",
    subtitle = "Ten quantile bins of construction-trades share; intervals are 95 percent CIs for bin means",
    x = "Mean construction-trades share of all receipts in bin",
    y = "Mean EB-shrunk stringency score"
  ) +
  plot_theme

ggsave("../output/construction_trades_share_score_binscatter.pdf", trade_share_score_bins_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_share_score_binscatter.png", trade_share_score_bins_plot, width = 9, height = 5.2, dpi = 300, bg = "white")
write_csv(trade_share_score_bins, "../output/construction_trades_share_score_bins.csv")

trade_share_score_rank_data <- trade_share_score_data %>%
  group_by(score_spec) %>%
  mutate(
    trade_share_percentile = percent_rank(construction_trades_share_total_receipts),
    score_percentile = percent_rank(uncertainty_index)
  ) %>%
  ungroup()

trade_share_score_rank_regressions <- bind_rows(lapply(score_specs$score_spec, function(score_spec_i) {
  sample_i <- trade_share_score_rank_data %>%
    filter(score_spec == score_spec_i)

  fit_i <- lm(score_percentile ~ trade_share_percentile, data = sample_i)
  fit_summary_i <- summary(fit_i)
  coef_i <- fit_summary_i$coefficients["trade_share_percentile", ]

  tibble(
    score_spec = score_spec_i,
    n = nrow(sample_i),
    rank_rank_slope = unname(coef_i[["Estimate"]]),
    std_error = unname(coef_i[["Std. Error"]]),
    p_value = unname(coef_i[["Pr(>|t|)"]]),
    r_squared = unname(fit_summary_i$r.squared),
    spearman = unname(cor(sample_i$construction_trades_share_total_receipts, sample_i$uncertainty_index, method = "spearman"))
  )
})) %>%
  left_join(score_specs, by = "score_spec", relationship = "many-to-one") %>%
  arrange(score_order) %>%
  select(-score_order)

trade_share_score_rank_labels <- trade_share_score_rank_regressions %>%
  mutate(
    score_spec = factor(score_spec, levels = score_specs$score_spec),
    label = paste0(
      "Rank slope = ", sprintf("%.2f", rank_rank_slope),
      "\nSpearman r = ", sprintf("%.2f", spearman),
      "\nN = ", n
    )
  )

trade_share_score_rank_plot <- ggplot(trade_share_score_rank_data, aes(x = trade_share_percentile, y = score_percentile)) +
  geom_abline(intercept = 0, slope = 1, color = "gray80", linewidth = 0.4) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = trade_share_score_rank_labels,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.1,
    color = "gray20"
  ) +
  facet_wrap(~ score_spec) +
  scale_x_continuous(labels = percent_label) +
  scale_y_continuous(labels = percent_label) +
  labs(
    title = "Rank-Rank Construction-Trades Share and EB-Shrunk Stringency Score",
    subtitle = "Each axis is the within-score-spec percentile rank",
    x = "Construction-trades share rank",
    y = "Stringency score rank"
  ) +
  plot_theme

ggsave("../output/construction_trades_share_score_rank_rank.pdf", trade_share_score_rank_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_share_score_rank_rank.png", trade_share_score_rank_plot, width = 9, height = 5.2, dpi = 300, bg = "white")
write_csv(trade_share_score_rank_regressions, "../output/construction_trades_share_score_rank_regression.csv")

trade_extensive_margin <- trade_share_score_data %>%
  mutate(trade_receipt_group = if_else(construction_trades_amount > 0, "Any trades receipts", "Zero trades receipts")) %>%
  group_by(score_spec, trade_receipt_group) %>%
  summarise(
    n = n(),
    mean_uncertainty_index = mean(uncertainty_index),
    se_uncertainty_index = sd(uncertainty_index) / sqrt(n),
    ci_low = mean_uncertainty_index - 1.96 * se_uncertainty_index,
    ci_high = mean_uncertainty_index + 1.96 * se_uncertainty_index,
    .groups = "drop"
  ) %>%
  mutate(
    score_spec = factor(score_spec, levels = score_specs$score_spec),
    trade_receipt_group = factor(trade_receipt_group, levels = c("Zero trades receipts", "Any trades receipts"))
  )

trade_extensive_plot <- ggplot(trade_extensive_margin, aes(x = trade_receipt_group, y = mean_uncertainty_index)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), color = "#276f86", width = 0.12, linewidth = 0.55) +
  geom_point(color = "#276f86", size = 2.6) +
  geom_text(aes(label = paste0("N = ", n)), nudge_y = 0.12, size = 3.1, color = "gray20") +
  facet_wrap(~ score_spec) +
  labs(
    title = "Extensive Margin: Any Construction-Trades Receipts",
    subtitle = "Mean EB-shrunk stringency score by whether the alderman has any construction-trades receipts",
    x = NULL,
    y = "Mean EB-shrunk stringency score"
  ) +
  plot_theme

ggsave("../output/construction_trades_score_extensive_margin.pdf", trade_extensive_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_score_extensive_margin.png", trade_extensive_plot, width = 9, height = 5.2, dpi = 300, bg = "white")
write_csv(trade_extensive_margin, "../output/construction_trades_score_extensive_margin.csv")

trade_log_dollar_data <- trade_share_score_data %>%
  mutate(
    log_real_trades_per_cycle_year = log(1 + construction_trades_real_2022_per_cycle_year),
    log_real_total_receipts_per_cycle_year = log(1 + total_receipts_real_2022_per_cycle_year)
  ) %>%
  filter(
    construction_trades_real_2022_per_cycle_year > 0,
    is.finite(log_real_trades_per_cycle_year),
    is.finite(log_real_total_receipts_per_cycle_year)
  )

trade_log_dollar_regressions <- bind_rows(lapply(score_specs$score_spec, function(score_spec_i) {
  sample_i <- trade_log_dollar_data %>%
    filter(score_spec == score_spec_i)

  fit_i <- lm(uncertainty_index ~ log_real_trades_per_cycle_year, data = sample_i)
  fit_summary_i <- summary(fit_i)
  coef_i <- fit_summary_i$coefficients["log_real_trades_per_cycle_year", ]

  tibble(
    score_spec = score_spec_i,
    n = nrow(sample_i),
    coefficient_log_real_trades_per_cycle_year = unname(coef_i[["Estimate"]]),
    std_error = unname(coef_i[["Std. Error"]]),
    p_value = unname(coef_i[["Pr(>|t|)"]]),
    r_squared = unname(fit_summary_i$r.squared),
    pearson = unname(cor(sample_i$log_real_trades_per_cycle_year, sample_i$uncertainty_index, method = "pearson")),
    spearman = unname(cor(sample_i$log_real_trades_per_cycle_year, sample_i$uncertainty_index, method = "spearman"))
  )
})) %>%
  left_join(score_specs, by = "score_spec", relationship = "many-to-one") %>%
  arrange(score_order) %>%
  select(-score_order)

trade_log_dollar_labels <- trade_log_dollar_regressions %>%
  mutate(
    score_spec = factor(score_spec, levels = score_specs$score_spec),
    label = paste0(
      "Slope = ", sprintf("%.2f", coefficient_log_real_trades_per_cycle_year),
      "\nPearson r = ", sprintf("%.2f", pearson),
      "\nSpearman r = ", sprintf("%.2f", spearman),
      "\nN = ", n
    )
  )

trade_log_dollar_plot <- ggplot(trade_log_dollar_data, aes(x = log_real_trades_per_cycle_year, y = uncertainty_index)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = trade_log_dollar_labels,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.1,
    color = "gray20"
  ) +
  facet_wrap(~ score_spec) +
  labs(
    title = "Intensive Margin: Construction-Trades Dollars and EB-Shrunk Stringency Score",
    subtitle = "Positive trades receipts only; dollars are CPI-deflated to 2022 and divided by four years per observed cycle",
    x = "Log(1 + real construction-trades dollars per cycle-year)",
    y = "EB-shrunk stringency score"
  ) +
  plot_theme

ggsave("../output/construction_trades_log_dollars_score_scatter.pdf", trade_log_dollar_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_log_dollars_score_scatter.png", trade_log_dollar_plot, width = 9, height = 5.2, dpi = 300, bg = "white")

fit_robust_slope <- function(sample_i, score_spec_i, model_i, estimator_i, formula_i, predictor_i, scale_i, scale_label_i) {
  sample_i <- sample_i %>%
    filter(is.finite(uncertainty_index), is.finite(.data[[predictor_i]]))

  if (nrow(sample_i) < 8 || sd(sample_i$uncertainty_index) == 0 || sd(sample_i[[predictor_i]]) == 0) {
    return(tibble(
      score_spec = score_spec_i,
      model = model_i,
      estimator = estimator_i,
      n = nrow(sample_i),
      predictor = predictor_i,
      coefficient = NA_real_,
      scaled_coefficient = NA_real_,
      scale_label = scale_label_i,
      std_error = NA_real_,
      scaled_std_error = NA_real_,
      p_value = NA_real_
    ))
  }

  if (estimator_i == "OLS") {
    fit_i <- lm(formula_i, data = sample_i)
    coef_i <- summary(fit_i)$coefficients[predictor_i, ]
    coefficient_i <- unname(coef_i[["Estimate"]])
    std_error_i <- unname(coef_i[["Std. Error"]])
    p_value_i <- unname(coef_i[["Pr(>|t|)"]])
  } else if (estimator_i == "Huber rlm") {
    fit_i <- MASS::rlm(formula_i, data = sample_i, maxit = 100)
    coef_i <- summary(fit_i)$coefficients[predictor_i, ]
    coefficient_i <- unname(coef_i[["Value"]])
    std_error_i <- unname(coef_i[["Std. Error"]])
    t_value_i <- coefficient_i / std_error_i
    p_value_i <- 2 * pt(abs(t_value_i), df = nrow(sample_i) - length(coef(fit_i)), lower.tail = FALSE)
  } else {
    fit_i <- quantreg::rq(formula_i, tau = 0.5, data = sample_i)
    coef_i <- summary(fit_i, se = "nid")$coefficients[predictor_i, ]
    coefficient_i <- unname(coef_i[["Value"]])
    std_error_i <- unname(coef_i[["Std. Error"]])
    p_value_i <- unname(coef_i[["Pr(>|t|)"]])
  }

  tibble(
    score_spec = score_spec_i,
    model = model_i,
    estimator = estimator_i,
    n = nrow(sample_i),
    predictor = predictor_i,
    coefficient = coefficient_i,
    scaled_coefficient = scale_i * coefficient_i,
    scale_label = scale_label_i,
    std_error = std_error_i,
    scaled_std_error = scale_i * std_error_i,
    p_value = p_value_i
  )
}

robust_slope_rows <- list()
for (score_spec_i in score_specs$score_spec) {
  share_sample_i <- trade_share_score_data %>%
    filter(score_spec == score_spec_i)
  level_sample_i <- trade_log_dollar_data %>%
    filter(score_spec == score_spec_i)

  for (estimator_i in c("OLS", "Huber rlm", "Median rq")) {
    robust_slope_rows[[length(robust_slope_rows) + 1]] <- fit_robust_slope(
      share_sample_i,
      score_spec_i,
      "share only",
      estimator_i,
      uncertainty_index ~ construction_trades_share_total_receipts,
      "construction_trades_share_total_receipts",
      0.1,
      "10 percentage-point trades-share increase"
    )
    robust_slope_rows[[length(robust_slope_rows) + 1]] <- fit_robust_slope(
      level_sample_i,
      score_spec_i,
      "log real trades per cycle-year plus log total receipts",
      estimator_i,
      uncertainty_index ~ log_real_trades_per_cycle_year + log_real_total_receipts_per_cycle_year,
      "log_real_trades_per_cycle_year",
      1,
      "one log-point trades-dollar increase"
    )
  }
}

trade_score_robust_slopes <- bind_rows(robust_slope_rows) %>%
  left_join(score_specs, by = "score_spec", relationship = "many-to-one") %>%
  arrange(score_order, model, estimator) %>%
  select(-score_order)

write_csv(trade_score_robust_slopes, "../output/construction_trades_score_robust_slopes.csv")

trade_share_score_leaveout <- bind_rows(lapply(score_specs$score_spec, function(score_spec_i) {
  sample_i <- trade_share_score_data %>%
    filter(score_spec == score_spec_i) %>%
    arrange(desc(construction_trades_share_total_receipts), full_name) %>%
    mutate(trade_share_rank_desc = row_number())

  p95_i <- quantile(sample_i$construction_trades_share_total_receipts, 0.95, na.rm = TRUE)
  bind_rows(lapply(c("all", "drop_top3", "drop_top5", "winsor_p95"), function(check_i) {
    sample_check_i <- sample_i
    x_i <- sample_check_i$construction_trades_share_total_receipts
    if (check_i == "drop_top3") {
      sample_check_i <- sample_check_i %>% filter(trade_share_rank_desc > 3)
      x_i <- sample_check_i$construction_trades_share_total_receipts
    }
    if (check_i == "drop_top5") {
      sample_check_i <- sample_check_i %>% filter(trade_share_rank_desc > 5)
      x_i <- sample_check_i$construction_trades_share_total_receipts
    }
    if (check_i == "winsor_p95") {
      x_i <- pmin(sample_check_i$construction_trades_share_total_receipts, p95_i)
    }

    if (nrow(sample_check_i) < 3 || sd(x_i) == 0 || sd(sample_check_i$uncertainty_index) == 0) {
      return(tibble(
        score_spec = score_spec_i,
        check = check_i,
        n = nrow(sample_check_i),
        slope_10pp = NA_real_,
        pearson = NA_real_,
        spearman = NA_real_
      ))
    }

    fit_i <- lm(sample_check_i$uncertainty_index ~ x_i)
    tibble(
      score_spec = score_spec_i,
      check = check_i,
      n = nrow(sample_check_i),
      slope_10pp = 0.1 * unname(coef(fit_i)[[2]]),
      pearson = unname(cor(x_i, sample_check_i$uncertainty_index, method = "pearson")),
      spearman = unname(cor(x_i, sample_check_i$uncertainty_index, method = "spearman"))
    )
  }))
})) %>%
  left_join(score_specs, by = "score_spec", relationship = "many-to-one") %>%
  arrange(score_order, check) %>%
  select(-score_order)

write_csv(trade_share_score_leaveout, "../output/construction_trades_score_leaveout_checks.csv")

trade_share_score_labeled_labels <- trade_share_score_leaveout %>%
  filter(check %in% c("all", "drop_top5")) %>%
  select(score_spec, check, slope_10pp, pearson, spearman) %>%
  tidyr::pivot_wider(names_from = check, values_from = c(slope_10pp, pearson, spearman)) %>%
  mutate(
    score_spec = factor(score_spec, levels = score_specs$score_spec),
    label = paste0(
      "All slope per 10pp = ", sprintf("%.2f", slope_10pp_all),
      "\nAll Pearson r = ", sprintf("%.2f", pearson_all),
      "\nDrop top 5 Pearson r = ", sprintf("%.2f", pearson_drop_top5),
      "\nDrop top 5 Spearman r = ", sprintf("%.2f", spearman_drop_top5)
    )
  )

trade_share_top_labels <- trade_share_score_data %>%
  group_by(score_spec) %>%
  arrange(desc(construction_trades_share_total_receipts), full_name, .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  mutate(label_hjust = if_else(construction_trades_share_total_receipts > 0.12, 1.05, -0.05))

trade_share_score_labeled_plot <- ggplot(
  trade_share_score_data,
  aes(x = construction_trades_share_total_receipts, y = uncertainty_index)
) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = trade_share_top_labels,
    aes(label = full_name, hjust = label_hjust),
    vjust = 0.5,
    size = 2.5,
    color = "gray15",
    check_overlap = TRUE
  ) +
  geom_text(
    data = trade_share_score_labeled_labels,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 2.8,
    color = "gray20"
  ) +
  facet_wrap(~ score_spec) +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Construction-Trades Share and EB-Shrunk Stringency Score: Labeled Influence Check",
    subtitle = "Top five construction-trades-share aldermen are labeled; note reports leave-out sensitivity",
    x = "Construction-trades share of all receipts",
    y = "EB-shrunk stringency score"
  ) +
  plot_theme

ggsave("../output/construction_trades_share_score_labeled_scatter.pdf", trade_share_score_labeled_plot, width = 10, height = 5.8, bg = "white")
ggsave("../output/construction_trades_share_score_labeled_scatter.png", trade_share_score_labeled_plot, width = 10, height = 5.8, dpi = 300, bg = "white")

residual_control_vars <- c(
  "ctrl_median_hh_income_10k",
  "ctrl_share_black",
  "ctrl_share_hisp",
  "ctrl_homeownership_rate",
  "ctrl_pop_total_10k",
  "ctrl_dist_cbd_km",
  "ctrl_dist_lake_km",
  "ctrl_n_rail_stations_800m",
  "log_real_total_receipts_per_cycle_year",
  "tenure_years"
)
residual_control_rhs <- paste(residual_control_vars, collapse = " + ")

coefficient_row <- function(model, term) {
  coef_table <- as.data.frame(coeftable(model))
  p_col <- grep("^Pr\\(", names(coef_table), value = TRUE)[1]
  tibble(
    coefficient = unname(coef_table[term, "Estimate"]),
    std_error = unname(coef_table[term, "Std. Error"]),
    p_value = unname(coef_table[term, p_col])
  )
}

residualize_trade_pair <- function(input_df, outcome_var_i, outcome_label_i, outcome_family_i, score_spec_i) {
  sample_i <- input_df %>%
    mutate(
      outcome_value = .data[[outcome_var_i]],
      x_value = construction_trades_share_total_receipts
    ) %>%
    filter(
      is.finite(outcome_value),
      is.finite(x_value),
      if_all(all_of(residual_control_vars), ~ is.finite(.x))
    )

  if (nrow(sample_i) < length(residual_control_vars) + 5 || sd(sample_i$outcome_value) == 0 || sd(sample_i$x_value) == 0) {
    output_data_i <- sample_i %>%
      transmute(
        outcome_family = outcome_family_i,
        outcome = outcome_var_i,
        outcome_label = outcome_label_i,
        score_spec = score_spec_i,
        full_name,
        construction_trades_share_total_receipts,
        outcome_value,
        x_residual = NA_real_,
        y_residual = NA_real_
      )

    output_summary_i <- tibble(
      outcome_family = outcome_family_i,
      outcome = outcome_var_i,
      outcome_label = outcome_label_i,
      score_spec = score_spec_i,
      n = nrow(sample_i),
      raw_pearson = NA_real_,
      raw_spearman = NA_real_,
      residual_pearson = NA_real_,
      residual_spearman = NA_real_,
      fwl_slope = NA_real_,
      fwl_slope_10pp = NA_real_,
      fwl_std_error = NA_real_,
      fwl_p_value = NA_real_,
      full_model_slope = NA_real_,
      full_model_slope_10pp = NA_real_,
      full_model_std_error = NA_real_,
      full_model_p_value = NA_real_,
      fwl_full_slope_difference = NA_real_
    )

    return(list(data = output_data_i, summary = output_summary_i))
  }

  y_resid_model_i <- feols(as.formula(paste0("outcome_value ~ ", residual_control_rhs)), data = sample_i, vcov = "hetero", warn = FALSE)
  x_resid_model_i <- feols(as.formula(paste0("x_value ~ ", residual_control_rhs)), data = sample_i, vcov = "hetero", warn = FALSE)

  sample_i <- sample_i %>%
    mutate(
      y_residual = as.numeric(resid(y_resid_model_i)),
      x_residual = as.numeric(resid(x_resid_model_i))
    )

  fwl_model_i <- feols(y_residual ~ x_residual, data = sample_i, vcov = "hetero", warn = FALSE)
  full_model_i <- feols(
    as.formula(paste0("outcome_value ~ x_value + ", residual_control_rhs)),
    data = sample_i,
    vcov = "hetero",
    warn = FALSE
  )

  fwl_coef_i <- coefficient_row(fwl_model_i, "x_residual")
  full_coef_i <- coefficient_row(full_model_i, "x_value")

  output_data_i <- sample_i %>%
    transmute(
      outcome_family = outcome_family_i,
      outcome = outcome_var_i,
      outcome_label = outcome_label_i,
      score_spec = score_spec_i,
      full_name,
      construction_trades_share_total_receipts,
      outcome_value,
      x_residual,
      y_residual
    )

  output_summary_i <- tibble(
    outcome_family = outcome_family_i,
    outcome = outcome_var_i,
    outcome_label = outcome_label_i,
    score_spec = score_spec_i,
    n = nrow(sample_i),
    raw_pearson = unname(cor(sample_i$x_value, sample_i$outcome_value, method = "pearson")),
    raw_spearman = unname(cor(sample_i$x_value, sample_i$outcome_value, method = "spearman")),
    residual_pearson = unname(cor(sample_i$x_residual, sample_i$y_residual, method = "pearson")),
    residual_spearman = unname(cor(sample_i$x_residual, sample_i$y_residual, method = "spearman")),
    fwl_slope = fwl_coef_i$coefficient,
    fwl_slope_10pp = 0.1 * fwl_coef_i$coefficient,
    fwl_std_error = fwl_coef_i$std_error,
    fwl_p_value = fwl_coef_i$p_value,
    full_model_slope = full_coef_i$coefficient,
    full_model_slope_10pp = 0.1 * full_coef_i$coefficient,
    full_model_std_error = full_coef_i$std_error,
    full_model_p_value = full_coef_i$p_value,
    fwl_full_slope_difference = fwl_coef_i$coefficient - full_coef_i$coefficient
  )

  list(data = output_data_i, summary = output_summary_i)
}

residual_raw_base <- joined %>%
  mutate(
    log_high_discretion_permits_per_observed_year = if_else(
      high_discretion_permits_per_observed_year > 0,
      log(high_discretion_permits_per_observed_year),
      NA_real_
    ),
    log_new_construction_permits_per_observed_year = if_else(
      new_construction_permits_per_observed_year > 0,
      log(new_construction_permits_per_observed_year),
      NA_real_
    ),
    log_new_units_per_tenure_year = if_else(
      tenure_years > 0,
      log(1 + new_units / tenure_years),
      NA_real_
    ),
    log_aggregate_dupac = if_else(aggregate_dupac >= 0, log(1 + aggregate_dupac), NA_real_),
    log_median_building_far = if_else(median_building_far >= 0, log(1 + median_building_far), NA_real_)
  )

residual_results <- list()
for (score_spec_i in score_specs$score_spec) {
  residual_results[[length(residual_results) + 1]] <- residualize_trade_pair(
    stringency_join %>% filter(score_spec == score_spec_i),
    "uncertainty_index",
    "EB-shrunk stringency score",
    "score",
    score_spec_i
  )
}

for (outcome_i in c(
  "log_high_discretion_permits_per_observed_year",
  "log_new_construction_permits_per_observed_year",
  "log_new_units_per_tenure_year",
  "log_aggregate_dupac",
  "log_median_building_far"
)) {
  outcome_label_i <- case_when(
    outcome_i == "log_high_discretion_permits_per_observed_year" ~ "Log high-discretion permits per observed year",
    outcome_i == "log_new_construction_permits_per_observed_year" ~ "Log new-construction permits per observed year",
    outcome_i == "log_new_units_per_tenure_year" ~ "Log new units per tenure year",
    outcome_i == "log_aggregate_dupac" ~ "Log aggregate dwelling units per acre",
    outcome_i == "log_median_building_far" ~ "Log median building FAR",
    TRUE ~ outcome_i
  )
  outcome_family_i <- case_when(
    outcome_i %in% c(
      "log_high_discretion_permits_per_observed_year",
      "log_new_construction_permits_per_observed_year"
    ) ~ "permit_count",
    TRUE ~ "development_density"
  )

  residual_results[[length(residual_results) + 1]] <- residualize_trade_pair(
    residual_raw_base,
    outcome_i,
    outcome_label_i,
    outcome_family_i,
    "Raw outcome through 2026"
  )
}

residualized_join <- bind_rows(lapply(residual_results, function(x) x$data))
residualized_correlations <- bind_rows(lapply(residual_results, function(x) x$summary))

write_csv(residualized_join, "../output/construction_trades_residualized_join.csv")
write_csv(residualized_correlations, "../output/construction_trades_residualized_correlations.csv")

residualized_leaveout <- residualized_join %>%
  group_by(outcome_family, outcome, outcome_label, score_spec) %>%
  arrange(desc(construction_trades_share_total_receipts), full_name, .by_group = TRUE) %>%
  mutate(raw_trade_share_rank_desc = row_number()) %>%
  group_modify(~ {
    bind_rows(lapply(c("all", "drop_top5", "drop_top12"), function(check_i) {
      sample_i <- .x
      if (check_i == "drop_top5") {
        sample_i <- sample_i %>% filter(raw_trade_share_rank_desc > 5)
      }
      if (check_i == "drop_top12") {
        sample_i <- sample_i %>% filter(raw_trade_share_rank_desc > 12)
      }

      if (nrow(sample_i) < 3 || sd(sample_i$x_residual) == 0 || sd(sample_i$y_residual) == 0) {
        return(tibble(
          check = check_i,
          n = nrow(sample_i),
          residual_slope = NA_real_,
          residual_slope_10pp = NA_real_,
          residual_pearson = NA_real_,
          residual_spearman = NA_real_
        ))
      }

      fit_i <- lm(y_residual ~ x_residual, data = sample_i)
      tibble(
        check = check_i,
        n = nrow(sample_i),
        residual_slope = unname(coef(fit_i)[["x_residual"]]),
        residual_slope_10pp = 0.1 * unname(coef(fit_i)[["x_residual"]]),
        residual_pearson = unname(cor(sample_i$x_residual, sample_i$y_residual, method = "pearson")),
        residual_spearman = unname(cor(sample_i$x_residual, sample_i$y_residual, method = "spearman"))
      )
    }))
  }) %>%
  ungroup()

write_csv(residualized_leaveout, "../output/construction_trades_residualized_leaveout_checks.csv")

ppml_rows <- bind_rows(lapply(c("high_discretion_permits", "new_construction_permits"), function(outcome_i) {
  sample_i <- joined %>%
    mutate(
      ppml_outcome = .data[[outcome_i]],
      ppml_offset = log(permit_years_observed)
    ) %>%
    filter(
      is.finite(ppml_outcome),
      ppml_outcome >= 0,
      is.finite(ppml_offset),
      is.finite(construction_trades_share_total_receipts),
      if_all(all_of(residual_control_vars), ~ is.finite(.x))
    )

  if (nrow(sample_i) < length(residual_control_vars) + 5 || sd(sample_i$construction_trades_share_total_receipts) == 0) {
    return(tibble(
      outcome = outcome_i,
      outcome_label = if_else(outcome_i == "high_discretion_permits", "High-discretion permits", "New-construction permits"),
      n = nrow(sample_i),
      coefficient_trade_share = NA_real_,
      coefficient_trade_share_10pp = NA_real_,
      percent_effect_10pp = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_
    ))
  }

  ppml_model_i <- fepois(
    as.formula(paste0("ppml_outcome ~ construction_trades_share_total_receipts + ", residual_control_rhs)),
    offset = ~ppml_offset,
    data = sample_i,
    vcov = "hetero",
    warn = FALSE
  )
  coef_i <- coefficient_row(ppml_model_i, "construction_trades_share_total_receipts")

  tibble(
    outcome = outcome_i,
    outcome_label = if_else(outcome_i == "high_discretion_permits", "High-discretion permits", "New-construction permits"),
    n = nrow(sample_i),
    coefficient_trade_share = coef_i$coefficient,
    coefficient_trade_share_10pp = 0.1 * coef_i$coefficient,
    coefficient_trade_share_10pp_low = 0.1 * (coef_i$coefficient - 1.96 * coef_i$std_error),
    coefficient_trade_share_10pp_high = 0.1 * (coef_i$coefficient + 1.96 * coef_i$std_error),
    percent_effect_10pp = 100 * (exp(0.1 * coef_i$coefficient) - 1),
    percent_effect_10pp_low = 100 * (exp(0.1 * (coef_i$coefficient - 1.96 * coef_i$std_error)) - 1),
    percent_effect_10pp_high = 100 * (exp(0.1 * (coef_i$coefficient + 1.96 * coef_i$std_error)) - 1),
    std_error = coef_i$std_error,
    p_value = coef_i$p_value
  )
}))

write_csv(ppml_rows, "../output/construction_trades_residualized_ppml.csv")

ppml_effect_plot_data <- ppml_rows %>%
  mutate(
    outcome_label = factor(outcome_label, levels = c("High-discretion permits", "New-construction permits")),
    label = paste0(
      sprintf("%.1f", percent_effect_10pp),
      "%\np = ",
      if_else(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
    )
  )

ppml_effect_plot <- ggplot(ppml_effect_plot_data, aes(x = percent_effect_10pp, y = outcome_label)) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.5) +
  geom_errorbarh(
    aes(xmin = percent_effect_10pp_low, xmax = percent_effect_10pp_high),
    height = 0.16,
    color = "#276f86",
    linewidth = 0.65
  ) +
  geom_point(color = "#276f86", size = 2.8) +
  geom_text(aes(label = label), nudge_y = 0.18, size = 3.1, color = "gray20") +
  labs(
    title = "PPML Effects on Permit Counts",
    subtitle = "Effect of a 10pp higher construction-trades share; 95 percent confidence intervals",
    x = "Percent change in permits per observed year",
    y = NULL
  ) +
  plot_theme

ggsave("../output/construction_trades_residualized_ppml_effects.pdf", ppml_effect_plot, width = 8.8, height = 4.6, bg = "white")
ggsave("../output/construction_trades_residualized_ppml_effects.png", ppml_effect_plot, width = 8.8, height = 4.6, dpi = 300, bg = "white")

ppml_prediction_rows <- bind_rows(lapply(c("high_discretion_permits", "new_construction_permits"), function(outcome_i) {
  sample_i <- joined %>%
    mutate(
      ppml_outcome = .data[[outcome_i]],
      ppml_offset = log(permit_years_observed)
    ) %>%
    filter(
      is.finite(ppml_outcome),
      ppml_outcome >= 0,
      is.finite(ppml_offset),
      is.finite(construction_trades_share_total_receipts),
      if_all(all_of(residual_control_vars), ~ is.finite(.x))
    )

  if (nrow(sample_i) < length(residual_control_vars) + 5 || sd(sample_i$construction_trades_share_total_receipts) == 0) {
    return(tibble())
  }

  ppml_model_i <- fepois(
    as.formula(paste0("ppml_outcome ~ construction_trades_share_total_receipts + ", residual_control_rhs)),
    offset = ~ppml_offset,
    data = sample_i,
    vcov = "hetero",
    warn = FALSE
  )

  grid_i <- tibble(
    construction_trades_share_total_receipts = seq(
      min(sample_i$construction_trades_share_total_receipts, na.rm = TRUE),
      max(sample_i$construction_trades_share_total_receipts, na.rm = TRUE),
      length.out = 100
    ),
    ppml_offset = 0
  )
  for (control_i in residual_control_vars) {
    grid_i[[control_i]] <- mean(sample_i[[control_i]], na.rm = TRUE)
  }

  grid_i %>%
    mutate(
      outcome = outcome_i,
      outcome_label = if_else(outcome_i == "high_discretion_permits", "High-discretion permits", "New-construction permits"),
      predicted_permits_per_year = as.numeric(predict(ppml_model_i, newdata = grid_i, type = "response"))
    )
}))

ppml_point_rows <- bind_rows(lapply(c("high_discretion_permits", "new_construction_permits"), function(outcome_i) {
  joined %>%
    transmute(
      outcome = outcome_i,
      outcome_label = if_else(outcome_i == "high_discretion_permits", "High-discretion permits", "New-construction permits"),
      construction_trades_share_total_receipts,
      permits_per_observed_year = if_else(
        is.finite(permit_years_observed) & permit_years_observed > 0,
        .data[[outcome_i]] / permit_years_observed,
        NA_real_
      )
    ) %>%
    filter(is.finite(construction_trades_share_total_receipts), is.finite(permits_per_observed_year))
}))

ppml_adjusted_rate_plot <- ggplot() +
  geom_point(
    data = ppml_point_rows,
    aes(x = construction_trades_share_total_receipts, y = permits_per_observed_year),
    color = "#276f86",
    alpha = 0.28,
    size = 1.8
  ) +
  geom_line(
    data = ppml_prediction_rows,
    aes(x = construction_trades_share_total_receipts, y = predicted_permits_per_year),
    color = "#9f3d3f",
    linewidth = 0.9
  ) +
  facet_wrap(~ outcome_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "PPML-Adjusted Permit Rates by Construction-Trades Share",
    subtitle = "Line holds episode controls at sample means and sets the offset to one observed year; dots are raw annual rates",
    x = "Construction-trades share of all receipts",
    y = "Permits per observed year"
  ) +
  plot_theme

ggsave("../output/construction_trades_residualized_ppml_adjusted_rates.pdf", ppml_adjusted_rate_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_residualized_ppml_adjusted_rates.png", ppml_adjusted_rate_plot, width = 9, height = 5.2, dpi = 300, bg = "white")

residual_score_data <- residualized_join %>%
  filter(outcome_family == "score") %>%
  mutate(score_spec = factor(score_spec, levels = score_specs$score_spec))

residual_score_bins <- residual_score_data %>%
  group_by(score_spec) %>%
  arrange(x_residual, full_name, .by_group = TRUE) %>%
  mutate(x_residual_bin = ntile(x_residual, 10)) %>%
  group_by(score_spec, x_residual_bin) %>%
  summarise(
    n = n(),
    mean_x_residual = mean(x_residual),
    mean_y_residual = mean(y_residual),
    se_y_residual = sd(y_residual) / sqrt(n),
    ci_low = mean_y_residual - 1.96 * se_y_residual,
    ci_high = mean_y_residual + 1.96 * se_y_residual,
    .groups = "drop"
  )

residual_score_bins_plot <- ggplot(residual_score_bins, aes(x = mean_x_residual, y = mean_y_residual)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), color = "#276f86", width = 0.0035, linewidth = 0.55) +
  geom_line(color = "#276f86", linewidth = 0.65) +
  geom_point(color = "#276f86", size = 2.4) +
  facet_wrap(~ score_spec) +
  labs(
    title = "Residualized Construction-Trades Share and EB-Shrunk Stringency Score",
    subtitle = "Ten bins of residualized trades share after residualizing both variables on episode controls",
    x = "Residualized construction-trades share",
    y = "Mean residualized EB-shrunk score"
  ) +
  plot_theme

ggsave("../output/construction_trades_residualized_score_binscatter.pdf", residual_score_bins_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_residualized_score_binscatter.png", residual_score_bins_plot, width = 9, height = 5.2, dpi = 300, bg = "white")

residual_score_rank_data <- residual_score_data %>%
  group_by(score_spec) %>%
  mutate(
    x_residual_percentile = percent_rank(x_residual),
    y_residual_percentile = percent_rank(y_residual)
  ) %>%
  ungroup()

residual_score_rank_labels <- residualized_correlations %>%
  filter(outcome_family == "score") %>%
  mutate(
    score_spec = factor(score_spec, levels = score_specs$score_spec),
    label = paste0(
      "Pearson r = ", sprintf("%.2f", residual_pearson),
      "\nSpearman r = ", sprintf("%.2f", residual_spearman),
      "\nN = ", n
    )
  )

residual_score_rank_plot <- ggplot(residual_score_rank_data, aes(x = x_residual_percentile, y = y_residual_percentile)) +
  geom_abline(intercept = 0, slope = 1, color = "gray80", linewidth = 0.4) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = residual_score_rank_labels,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.1,
    color = "gray20"
  ) +
  facet_wrap(~ score_spec) +
  scale_x_continuous(labels = percent_label) +
  scale_y_continuous(labels = percent_label) +
  labs(
    title = "Residualized Rank-Rank Construction-Trades Share and EB-Shrunk Stringency Score",
    subtitle = "Ranks are within score specification after double residualization",
    x = "Residualized construction-trades share rank",
    y = "Residualized stringency score rank"
  ) +
  plot_theme

ggsave("../output/construction_trades_residualized_score_rank_rank.pdf", residual_score_rank_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_residualized_score_rank_rank.png", residual_score_rank_plot, width = 9, height = 5.2, dpi = 300, bg = "white")

residual_score_labeled_labels <- residualized_leaveout %>%
  filter(outcome_family == "score", check %in% c("all", "drop_top5", "drop_top12")) %>%
  select(score_spec, check, residual_pearson, residual_spearman) %>%
  tidyr::pivot_wider(names_from = check, values_from = c(residual_pearson, residual_spearman)) %>%
  mutate(
    score_spec = factor(score_spec, levels = score_specs$score_spec),
    label = paste0(
      "All Pearson r = ", sprintf("%.2f", residual_pearson_all),
      "\nDrop top 5 r = ", sprintf("%.2f", residual_pearson_drop_top5),
      "\nDrop top 12 r = ", sprintf("%.2f", residual_pearson_drop_top12),
      "\nAll Spearman r = ", sprintf("%.2f", residual_spearman_all)
    )
  )

residual_score_top_labels <- residual_score_data %>%
  group_by(score_spec) %>%
  arrange(desc(construction_trades_share_total_receipts), full_name, .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  mutate(label_hjust = if_else(x_residual > 0, 1.05, -0.05))

residual_score_labeled_plot <- ggplot(residual_score_data, aes(x = x_residual, y = y_residual)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = residual_score_top_labels,
    aes(label = full_name, hjust = label_hjust),
    vjust = 0.5,
    size = 2.5,
    color = "gray15",
    check_overlap = TRUE
  ) +
  geom_text(
    data = residual_score_labeled_labels,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 2.8,
    color = "gray20"
  ) +
  facet_wrap(~ score_spec) +
  labs(
    title = "Residualized Construction-Trades Share and EB-Shrunk Stringency Score",
    subtitle = "Both variables residualized on the same episode controls; top raw trades-share aldermen labeled",
    x = "Residualized construction-trades share",
    y = "Residualized EB-shrunk stringency score"
  ) +
  plot_theme

ggsave("../output/construction_trades_residualized_score_labeled_scatter.pdf", residual_score_labeled_plot, width = 10, height = 5.8, bg = "white")
ggsave("../output/construction_trades_residualized_score_labeled_scatter.png", residual_score_labeled_plot, width = 10, height = 5.8, dpi = 300, bg = "white")

residual_permit_data <- residualized_join %>%
  filter(outcome_family == "permit_count")

residual_permit_labels <- residualized_correlations %>%
  filter(outcome_family == "permit_count") %>%
  mutate(
    label = paste0(
      "Slope per 10pp = ", sprintf("%.2f", fwl_slope_10pp),
      "\nPearson r = ", sprintf("%.2f", residual_pearson),
      "\nSpearman r = ", sprintf("%.2f", residual_spearman),
      "\nN = ", n
    )
  ) %>%
  select(outcome, outcome_label, label)

residual_permit_plot <- ggplot(residual_permit_data, aes(x = x_residual, y = y_residual)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = residual_permit_labels,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.1,
    color = "gray20"
  ) +
  facet_wrap(~ outcome_label, scales = "free_y") +
  labs(
    title = "Residualized Construction-Trades Share and Log Permit Counts",
    subtitle = "Both variables residualized on the same episode controls",
    x = "Residualized construction-trades share",
    y = NULL
  ) +
  plot_theme

ggsave("../output/construction_trades_residualized_permit_count_scatter.pdf", residual_permit_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_residualized_permit_count_scatter.png", residual_permit_plot, width = 9, height = 5.2, dpi = 300, bg = "white")

residual_permit_bins <- residual_permit_data %>%
  group_by(outcome, outcome_label) %>%
  arrange(x_residual, full_name, .by_group = TRUE) %>%
  mutate(x_residual_bin = ntile(x_residual, 10)) %>%
  group_by(outcome, outcome_label, x_residual_bin) %>%
  summarise(
    n = n(),
    mean_x_residual = mean(x_residual),
    mean_y_residual = mean(y_residual),
    se_y_residual = sd(y_residual) / sqrt(n),
    ci_low = mean_y_residual - 1.96 * se_y_residual,
    ci_high = mean_y_residual + 1.96 * se_y_residual,
    .groups = "drop"
  )

residual_permit_bins_plot <- ggplot(residual_permit_bins, aes(x = mean_x_residual, y = mean_y_residual)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), color = "#276f86", width = 0.0035, linewidth = 0.55) +
  geom_line(color = "#276f86", linewidth = 0.65) +
  geom_point(color = "#276f86", size = 2.4) +
  facet_wrap(~ outcome_label, scales = "free_y") +
  labs(
    title = "Binned Residualized Construction-Trades Share and Log Permit Counts",
    subtitle = "Ten bins of residualized trades share after residualizing both variables on episode controls",
    x = "Mean residualized construction-trades share in bin",
    y = NULL
  ) +
  plot_theme

ggsave("../output/construction_trades_residualized_permit_count_binscatter.pdf", residual_permit_bins_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_residualized_permit_count_binscatter.png", residual_permit_bins_plot, width = 9, height = 5.2, dpi = 300, bg = "white")

trade_raw_measure_labels <- tibble(
  measure = c(
    "high_discretion_mean_processing_time",
    "new_construction_mean_processing_time",
    "high_discretion_permits",
    "high_discretion_permits_per_observed_year",
    "new_construction_permits",
    "new_construction_permits_per_observed_year",
    "new_buildings",
    "new_units",
    "median_building_far",
    "aggregate_dupac"
  ),
  measure_label = c(
    "High-discretion mean processing days",
    "New-construction mean processing days",
    "High-discretion permits",
    "High-discretion permits per observed year",
    "New-construction permits",
    "New-construction permits per observed year",
    "New residential/commercial buildings",
    "New residential units",
    "Median building FAR",
    "Aggregate dwelling units per acre"
  )
)

trade_raw_correlations <- bind_rows(lapply(trade_raw_measure_labels$measure, function(measure_i) {
  sample_i <- joined %>%
    filter(
      is.finite(construction_trades_share_total_receipts),
      is.finite(.data[[measure_i]])
    )

  if (
    nrow(sample_i) < 3 ||
      sd(sample_i$construction_trades_share_total_receipts) == 0 ||
      sd(sample_i[[measure_i]]) == 0
  ) {
    return(tibble(
      predictor = "construction_trades_share_total_receipts",
      measure = measure_i,
      n = nrow(sample_i),
      coefficient_trade_share = NA_real_,
      coefficient_trade_share_10pp = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_,
      pearson = NA_real_,
      spearman = NA_real_
    ))
  }

  fit_i <- lm(sample_i[[measure_i]] ~ sample_i$construction_trades_share_total_receipts)
  fit_summary_i <- summary(fit_i)
  coef_i <- fit_summary_i$coefficients[2, ]

  tibble(
    predictor = "construction_trades_share_total_receipts",
    measure = measure_i,
    n = nrow(sample_i),
    coefficient_trade_share = unname(coef_i[["Estimate"]]),
    coefficient_trade_share_10pp = 0.1 * unname(coef_i[["Estimate"]]),
    std_error = unname(coef_i[["Std. Error"]]),
    p_value = unname(coef_i[["Pr(>|t|)"]]),
    r_squared = unname(fit_summary_i$r.squared),
    pearson = unname(cor(sample_i$construction_trades_share_total_receipts, sample_i[[measure_i]], method = "pearson")),
    spearman = unname(cor(sample_i$construction_trades_share_total_receipts, sample_i[[measure_i]], method = "spearman"))
  )
})) %>%
  left_join(trade_raw_measure_labels, by = "measure", relationship = "many-to-one") %>%
  select(
    predictor,
    measure,
    measure_label,
    n,
    coefficient_trade_share,
    coefficient_trade_share_10pp,
    std_error,
    p_value,
    r_squared,
    pearson,
    spearman
  )

trade_raw_label_data <- trade_raw_correlations %>%
  mutate(
    label = paste0(
      "Slope per 10pp = ", sprintf("%.1f", coefficient_trade_share_10pp),
      "\nPearson r = ", sprintf("%.2f", pearson),
      "\nSpearman r = ", sprintf("%.2f", spearman),
      "\nN = ", n
    )
  ) %>%
  select(measure, measure_label, label)

trade_processing_plot_data <- joined %>%
  select(
    full_name,
    construction_trades_share_total_receipts,
    high_discretion_mean_processing_time,
    new_construction_mean_processing_time
  ) %>%
  tidyr::pivot_longer(
    cols = c(high_discretion_mean_processing_time, new_construction_mean_processing_time),
    names_to = "measure",
    values_to = "value"
  ) %>%
  left_join(trade_raw_measure_labels, by = "measure", relationship = "many-to-one") %>%
  filter(is.finite(value), is.finite(construction_trades_share_total_receipts))

trade_processing_plot <- ggplot(trade_processing_plot_data, aes(x = construction_trades_share_total_receipts, y = value)) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = trade_raw_label_data %>% filter(measure %in% c("high_discretion_mean_processing_time", "new_construction_mean_processing_time")),
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.1,
    color = "gray20"
  ) +
  facet_wrap(~ measure_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Construction-Trades Share and Permit Processing Time",
    subtitle = "Construction-trades donations as a share of all campaign receipts",
    x = "Construction-trades share of all receipts",
    y = NULL
  ) +
  plot_theme

ggsave("../output/construction_trades_share_processing_time_scatter.pdf", trade_processing_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_share_processing_time_scatter.png", trade_processing_plot, width = 9, height = 5.2, dpi = 300, bg = "white")

trade_permit_count_plot_data <- joined %>%
  select(
    full_name,
    construction_trades_share_total_receipts,
    high_discretion_permits,
    high_discretion_permits_per_observed_year,
    new_construction_permits,
    new_construction_permits_per_observed_year
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      high_discretion_permits,
      high_discretion_permits_per_observed_year,
      new_construction_permits,
      new_construction_permits_per_observed_year
    ),
    names_to = "measure",
    values_to = "value"
  ) %>%
  left_join(trade_raw_measure_labels, by = "measure", relationship = "many-to-one") %>%
  filter(is.finite(value), is.finite(construction_trades_share_total_receipts))

trade_permit_count_plot <- ggplot(trade_permit_count_plot_data, aes(x = construction_trades_share_total_receipts, y = value)) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = trade_raw_label_data %>% filter(measure %in% c(
      "high_discretion_permits",
      "high_discretion_permits_per_observed_year",
      "new_construction_permits",
      "new_construction_permits_per_observed_year"
    )),
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 2.8,
    color = "gray20"
  ) +
  facet_wrap(~ measure_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Construction-Trades Share and Permit Counts",
    subtitle = "Construction-trades donations as a share of all campaign receipts",
    x = "Construction-trades share of all receipts",
    y = NULL
  ) +
  plot_theme

ggsave("../output/construction_trades_share_permit_counts_scatter.pdf", trade_permit_count_plot, width = 10, height = 7, bg = "white")
ggsave("../output/construction_trades_share_permit_counts_scatter.png", trade_permit_count_plot, width = 10, height = 7, dpi = 300, bg = "white")

trade_log_permit_count_data <- joined %>%
  transmute(
    full_name,
    construction_trades_share_total_receipts,
    log_high_discretion_permits_per_observed_year = if_else(
      high_discretion_permits_per_observed_year > 0,
      log(high_discretion_permits_per_observed_year),
      NA_real_
    ),
    log_new_construction_permits_per_observed_year = if_else(
      new_construction_permits_per_observed_year > 0,
      log(new_construction_permits_per_observed_year),
      NA_real_
    )
  ) %>%
  tidyr::pivot_longer(
    cols = c(log_high_discretion_permits_per_observed_year, log_new_construction_permits_per_observed_year),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure_label = case_when(
      measure == "log_high_discretion_permits_per_observed_year" ~ "Log high-discretion permits per observed year",
      measure == "log_new_construction_permits_per_observed_year" ~ "Log new-construction permits per observed year",
      TRUE ~ measure
    )
  ) %>%
  filter(is.finite(value), is.finite(construction_trades_share_total_receipts))

trade_log_permit_count_correlations <- bind_rows(lapply(unique(trade_log_permit_count_data$measure), function(measure_i) {
  sample_i <- trade_log_permit_count_data %>%
    filter(measure == measure_i)

  if (
    nrow(sample_i) < 3 ||
      sd(sample_i$construction_trades_share_total_receipts) == 0 ||
      sd(sample_i$value) == 0
  ) {
    return(tibble(
      predictor = "construction_trades_share_total_receipts",
      measure = measure_i,
      n = nrow(sample_i),
      coefficient_trade_share = NA_real_,
      coefficient_trade_share_10pp = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_,
      pearson = NA_real_,
      spearman = NA_real_
    ))
  }

  fit_i <- lm(value ~ construction_trades_share_total_receipts, data = sample_i)
  fit_summary_i <- summary(fit_i)
  coef_i <- fit_summary_i$coefficients["construction_trades_share_total_receipts", ]

  tibble(
    predictor = "construction_trades_share_total_receipts",
    measure = measure_i,
    n = nrow(sample_i),
    coefficient_trade_share = unname(coef_i[["Estimate"]]),
    coefficient_trade_share_10pp = 0.1 * unname(coef_i[["Estimate"]]),
    std_error = unname(coef_i[["Std. Error"]]),
    p_value = unname(coef_i[["Pr(>|t|)"]]),
    r_squared = unname(fit_summary_i$r.squared),
    pearson = unname(cor(sample_i$construction_trades_share_total_receipts, sample_i$value, method = "pearson")),
    spearman = unname(cor(sample_i$construction_trades_share_total_receipts, sample_i$value, method = "spearman"))
  )
})) %>%
  left_join(
    trade_log_permit_count_data %>%
      distinct(measure, measure_label),
    by = "measure",
    relationship = "many-to-one"
  ) %>%
  select(
    predictor,
    measure,
    measure_label,
    n,
    coefficient_trade_share,
    coefficient_trade_share_10pp,
    std_error,
    p_value,
    r_squared,
    pearson,
    spearman
  )

trade_log_permit_count_labels <- trade_log_permit_count_correlations %>%
  mutate(
    label = paste0(
      "Slope per 10pp = ", sprintf("%.2f", coefficient_trade_share_10pp),
      "\nPearson r = ", sprintf("%.2f", pearson),
      "\nSpearman r = ", sprintf("%.2f", spearman),
      "\nN = ", n
    )
  ) %>%
  select(measure, measure_label, label)

trade_log_permit_count_plot <- ggplot(
  trade_log_permit_count_data,
  aes(x = construction_trades_share_total_receipts, y = value)
) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = trade_log_permit_count_labels,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.1,
    color = "gray20"
  ) +
  facet_wrap(~ measure_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Construction-Trades Share and Log Permit Counts",
    subtitle = "Permit outcomes are log permits per observed year; all per-year counts are positive in this sample",
    x = "Construction-trades share of all receipts",
    y = NULL
  ) +
  plot_theme

ggsave("../output/construction_trades_share_log_permit_counts_scatter.pdf", trade_log_permit_count_plot, width = 9, height = 5.2, bg = "white")
ggsave("../output/construction_trades_share_log_permit_counts_scatter.png", trade_log_permit_count_plot, width = 9, height = 5.2, dpi = 300, bg = "white")
write_csv(trade_log_permit_count_correlations, "../output/construction_trades_share_log_permit_count_correlations.csv")

trade_units_density_plot_data <- joined %>%
  select(
    full_name,
    construction_trades_share_total_receipts,
    new_buildings,
    new_units,
    median_building_far,
    aggregate_dupac
  ) %>%
  tidyr::pivot_longer(
    cols = c(new_buildings, new_units, median_building_far, aggregate_dupac),
    names_to = "measure",
    values_to = "value"
  ) %>%
  left_join(trade_raw_measure_labels, by = "measure", relationship = "many-to-one") %>%
  filter(is.finite(value), is.finite(construction_trades_share_total_receipts))

trade_units_density_plot <- ggplot(trade_units_density_plot_data, aes(x = construction_trades_share_total_receipts, y = value)) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = trade_raw_label_data %>% filter(measure %in% c("new_buildings", "new_units", "median_building_far", "aggregate_dupac")),
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 2.8,
    color = "gray20"
  ) +
  facet_wrap(~ measure_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Construction-Trades Share and Residential/Commercial New Construction",
    subtitle = "Construction-trades donations as a share of all campaign receipts",
    x = "Construction-trades share of all receipts",
    y = NULL
  ) +
  plot_theme

ggsave("../output/construction_trades_share_units_density_scatter.pdf", trade_units_density_plot, width = 10, height = 7, bg = "white")
ggsave("../output/construction_trades_share_units_density_scatter.png", trade_units_density_plot, width = 10, height = 7, dpi = 300, bg = "white")
write_csv(trade_raw_correlations, "../output/construction_trades_share_raw_correlations.csv")

processing_plot_data <- joined %>%
  select(
    full_name,
    union_share_total_receipts,
    high_discretion_mean_processing_time,
    new_construction_mean_processing_time
  ) %>%
  tidyr::pivot_longer(
    cols = c(high_discretion_mean_processing_time, new_construction_mean_processing_time),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure_label = case_when(
      measure == "high_discretion_mean_processing_time" ~ "High-discretion mean processing days",
      measure == "new_construction_mean_processing_time" ~ "New-construction mean processing days",
      TRUE ~ measure
    )
  ) %>%
  filter(is.finite(value), is.finite(union_share_total_receipts))

processing_plot_labels <- processing_plot_data %>%
  distinct(measure, measure_label) %>%
  left_join(correlation_labels, by = "measure", relationship = "many-to-one")

processing_plot <- ggplot(processing_plot_data, aes(x = union_share_total_receipts, y = value)) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = processing_plot_labels,
    aes(x = Inf, y = Inf, label = correlation_label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.1,
    color = "gray20"
  ) +
  facet_wrap(~ measure_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Union Donation Share and Permit Processing Time",
    subtitle = "Alderman-level audit; permit measures through 2026",
    x = "Union share of total receipts",
    y = NULL
  ) +
  plot_theme

ggsave("../output/union_share_processing_time_scatter.png", processing_plot, width = 9, height = 5.2, dpi = 300, bg = "white")
ggsave("../output/union_share_processing_time_scatter.pdf", processing_plot, width = 9, height = 5.2, bg = "white")

processing_plot_data_ytrimmed <- trim_y_by_measure(processing_plot_data)
processing_plot_ytrimmed_labels <- trimmed_union_correlation_labels(processing_plot_data_ytrimmed)

processing_plot_ytrimmed <- ggplot(processing_plot_data_ytrimmed, aes(x = union_share_total_receipts, y = value)) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = processing_plot_ytrimmed_labels,
    aes(x = Inf, y = Inf, label = correlation_label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.1,
    color = "gray20"
  ) +
  facet_wrap(~ measure_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Union Donation Share and Permit Processing Time",
    subtitle = paste0("Alderman-level audit; permit measures through 2026\n", y_trim_subtitle),
    x = "Union share of total receipts",
    y = NULL
  ) +
  plot_theme

ggsave("../output/union_share_processing_time_scatter_ytrimmed.png", processing_plot_ytrimmed, width = 9, height = 5.2, dpi = 300, bg = "white")
ggsave("../output/union_share_processing_time_scatter_ytrimmed.pdf", processing_plot_ytrimmed, width = 9, height = 5.2, bg = "white")

permit_count_plot_data <- joined %>%
  select(
    full_name,
    union_share_total_receipts,
    high_discretion_permits,
    high_discretion_permits_per_observed_year,
    new_construction_permits,
    new_construction_permits_per_observed_year
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      high_discretion_permits,
      high_discretion_permits_per_observed_year,
      new_construction_permits,
      new_construction_permits_per_observed_year
    ),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure_label = case_when(
      measure == "high_discretion_permits" ~ "High-discretion permits",
      measure == "high_discretion_permits_per_observed_year" ~ "High-discretion permits per observed year",
      measure == "new_construction_permits" ~ "New-construction permits",
      measure == "new_construction_permits_per_observed_year" ~ "New-construction permits per observed year",
      TRUE ~ measure
    )
  ) %>%
  filter(is.finite(value), is.finite(union_share_total_receipts))

permit_count_plot_labels <- permit_count_plot_data %>%
  distinct(measure, measure_label) %>%
  left_join(correlation_labels, by = "measure", relationship = "many-to-one")

permit_count_plot <- ggplot(permit_count_plot_data, aes(x = union_share_total_receipts, y = value)) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = permit_count_plot_labels,
    aes(x = Inf, y = Inf, label = correlation_label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 2.8,
    color = "gray20"
  ) +
  facet_wrap(~ measure_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Union Donation Share and Permit Counts",
    subtitle = "Alderman-level audit; permit measures through 2026",
    x = "Union share of total receipts",
    y = NULL
  ) +
  plot_theme

ggsave("../output/union_share_permit_counts_scatter.png", permit_count_plot, width = 10, height = 7, dpi = 300, bg = "white")
ggsave("../output/union_share_permit_counts_scatter.pdf", permit_count_plot, width = 10, height = 7, bg = "white")

permit_count_plot_data_ytrimmed <- trim_y_by_measure(permit_count_plot_data)
permit_count_plot_ytrimmed_labels <- trimmed_union_correlation_labels(permit_count_plot_data_ytrimmed)

permit_count_plot_ytrimmed <- ggplot(permit_count_plot_data_ytrimmed, aes(x = union_share_total_receipts, y = value)) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = permit_count_plot_ytrimmed_labels,
    aes(x = Inf, y = Inf, label = correlation_label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 2.8,
    color = "gray20"
  ) +
  facet_wrap(~ measure_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Union Donation Share and Permit Counts",
    subtitle = paste0("Alderman-level audit; permit measures through 2026\n", y_trim_subtitle),
    x = "Union share of total receipts",
    y = NULL
  ) +
  plot_theme

ggsave("../output/union_share_permit_counts_scatter_ytrimmed.png", permit_count_plot_ytrimmed, width = 10, height = 7, dpi = 300, bg = "white")
ggsave("../output/union_share_permit_counts_scatter_ytrimmed.pdf", permit_count_plot_ytrimmed, width = 10, height = 7, bg = "white")

units_density_plot_data <- joined %>%
  select(
    full_name,
    union_share_total_receipts,
    new_buildings,
    new_units,
    median_building_far,
    aggregate_dupac
  ) %>%
  tidyr::pivot_longer(
    cols = c(new_buildings, new_units, median_building_far, aggregate_dupac),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure_label = case_when(
      measure == "new_buildings" ~ "New residential/commercial buildings",
      measure == "new_units" ~ "New residential units",
      measure == "median_building_far" ~ "Median building FAR",
      measure == "aggregate_dupac" ~ "Aggregate dwelling units per acre",
      TRUE ~ measure
    )
  ) %>%
  filter(is.finite(value), is.finite(union_share_total_receipts))

units_density_plot_labels <- units_density_plot_data %>%
  distinct(measure, measure_label) %>%
  left_join(correlation_labels, by = "measure", relationship = "many-to-one")

units_density_plot <- ggplot(units_density_plot_data, aes(x = union_share_total_receipts, y = value)) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = units_density_plot_labels,
    aes(x = Inf, y = Inf, label = correlation_label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 2.8,
    color = "gray20"
  ) +
  facet_wrap(~ measure_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Union Donation Share and Residential/Commercial New Construction",
    subtitle = "Combined residential plus commercial multifamily parcel source through 2026",
    x = "Union share of total receipts",
    y = NULL
  ) +
  plot_theme

ggsave("../output/union_share_units_density_scatter.png", units_density_plot, width = 10, height = 7, dpi = 300, bg = "white")
ggsave("../output/union_share_units_density_scatter.pdf", units_density_plot, width = 10, height = 7, bg = "white")

units_density_plot_data_ytrimmed <- trim_y_by_measure(units_density_plot_data)
units_density_plot_ytrimmed_labels <- trimmed_union_correlation_labels(units_density_plot_data_ytrimmed)

units_density_plot_ytrimmed <- ggplot(units_density_plot_data_ytrimmed, aes(x = union_share_total_receipts, y = value)) +
  geom_point(color = "#276f86", alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#9f3d3f", linewidth = 0.8) +
  geom_text(
    data = units_density_plot_ytrimmed_labels,
    aes(x = Inf, y = Inf, label = correlation_label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 2.8,
    color = "gray20"
  ) +
  facet_wrap(~ measure_label, scales = "free_y") +
  scale_x_continuous(labels = percent_label) +
  labs(
    title = "Union Donation Share and Residential/Commercial New Construction",
    subtitle = paste0("Combined residential plus commercial multifamily parcel source through 2026\n", y_trim_subtitle),
    x = "Union share of total receipts",
    y = NULL
  ) +
  plot_theme

ggsave("../output/union_share_units_density_scatter_ytrimmed.png", units_density_plot_ytrimmed, width = 10, height = 7, dpi = 300, bg = "white")
ggsave("../output/union_share_units_density_scatter_ytrimmed.pdf", units_density_plot_ytrimmed, width = 10, height = 7, bg = "white")

top_bottom <- bind_rows(
  joined %>%
    arrange(desc(union_share_total_receipts), full_name) %>%
    slice_head(n = 15) %>%
    mutate(group = "highest_union_share"),
  joined %>%
    arrange(union_share_total_receipts, full_name) %>%
    slice_head(n = 15) %>%
    mutate(group = "lowest_union_share")
) %>%
  select(
    group,
    full_name,
    union_share_total_receipts,
    total_receipts_amount,
    union_total_amount,
    high_discretion_permits,
    high_discretion_permits_per_observed_year,
    new_construction_permits,
    new_construction_mean_processing_time,
    new_buildings,
    new_units,
    aggregate_far,
    aggregate_dupac
  )

write_csv(joined, "../output/union_donations_permit_join.csv")
write_csv(summary_stats, "../output/union_donations_permit_summary_stats.csv")
write_csv(top_bottom, "../output/union_donations_permit_top_bottom.csv")
write_csv(unmatched, "../output/union_donations_permit_unmatched_aldermen.csv")
