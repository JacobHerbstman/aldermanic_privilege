# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/union_donations_permit_correlations/code")

source("../../../setup_environment/code/packages.R")

max_construction_year <- 2026
max_construction_month <- "2026-04"
start_analysis_year <- 2006

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
    construction_trades_share_total_receipts,
    teacher_education_share_total_receipts,
    public_sector_service_share_total_receipts,
    generic_labor_share_total_receipts
  )
validate_unique(union_donations, "alderman_key", "union donation aggregate")

permits <- data.table::fread(
  "../input/permits_for_uncertainty_index.csv",
  select = c("alderman", "month", "year", "permit_type_clean", "processing_time", "reported_cost"),
  showProgress = FALSE
) %>%
  as_tibble() %>%
  mutate(
    alderman_key = normalize_name(alderman),
    year = as.integer(year),
    processing_time = as.numeric(processing_time),
    reported_cost = as.numeric(reported_cost),
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
    yearmon_key = as.character(month_yearmon)
  ) %>%
  select(ward, yearmon_key, alderman)
validate_unique(alderman_panel, c("ward", "yearmon_key"), "alderman panel")

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
  left_join(parcel_measures, by = "alderman_key", relationship = "one-to-one") %>%
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
