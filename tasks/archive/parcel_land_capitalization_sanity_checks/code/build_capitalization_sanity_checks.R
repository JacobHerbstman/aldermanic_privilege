source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")

options(dplyr.summarise.inform = FALSE)
sf_use_s2(FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/parcel_land_capitalization_sanity_checks/code")
# panel_input <- "../input/parcel_land_redistricting_panel.parquet"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# output_dir <- "../output"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    panel_input,
    ward_panel_input,
    community_area_input,
    output_dir
  )
}

if (length(args) != 4) {
  stop(
    paste(
      "FATAL: Script requires 4 args:",
      "<panel_input> <ward_panel_input> <community_area_input> <output_dir>"
    ),
    call. = FALSE
  )
}

panel_input <- args[1]
ward_panel_input <- args[2]
community_area_input <- args[3]
output_dir <- args[4]

sample_view_labels <- c(
  current_land_only = "Current land-only",
  baseline_empty_last_pre_2012_2014 = "Baseline empty (last pre 2012-2014)"
)

bandwidth_labels <- c(
  `1000ft` = "1,000 ft",
  `500ft` = "500 ft"
)

treatment_sign_labels <- c(
  to_lenient = "To more lenient",
  no_change = "No change",
  to_stricter = "To stricter"
)

plot_years <- c(2012L, 2014L, 2016L)

safe_positive_quantile <- function(x, prob) {
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

safe_positive_median <- function(x) {
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  if (length(x) == 0) {
    return(NA_real_)
  }
  stats::median(x, na.rm = TRUE)
}

safe_quantile <- function(x, prob) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

make_bandwidth_views <- function(df) {
  bind_rows(
    df %>% mutate(bandwidth = "1000ft"),
    df %>% filter(in_500ft) %>% mutate(bandwidth = "500ft")
  )
}

save_plot_pair <- function(plot_obj, pdf_path, png_path, width, height) {
  ggsave(pdf_path, plot_obj, width = width, height = height, bg = "white")
  ggsave(png_path, plot_obj, width = width, height = height, dpi = 220, bg = "white")
}

build_level_map <- function(base_map, summary_df, metric_col, title_text, subtitle_text) {
  facet_grid_df <- summary_df %>%
    distinct(sample_view_label, bandwidth_label, year_label)

  plot_data <- tidyr::expand_grid(
    facet_grid_df,
    geography_id = unique(base_map$geography_id)
  ) %>%
    left_join(
      summary_df,
      by = c("sample_view_label", "bandwidth_label", "year_label", "geography_id")
    ) %>%
    left_join(base_map, by = "geography_id") %>%
    st_as_sf()

  ggplot(plot_data) +
    geom_sf(aes(fill = .data[[metric_col]]), color = "white", linewidth = 0.08) +
    facet_grid(sample_view_label ~ bandwidth_label + year_label) +
    scale_fill_viridis_c(
      option = "C",
      labels = scales::label_number(big.mark = ","),
      na.value = "grey92"
    ) +
    coord_sf(datum = NA) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      fill = NULL
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

build_change_map <- function(base_map, summary_df, metric_col, title_text, subtitle_text) {
  limit_value <- max(abs(summary_df[[metric_col]]), na.rm = TRUE)
  if (!is.finite(limit_value) || limit_value == 0) {
    limit_value <- 1
  }

  facet_grid_df <- summary_df %>%
    distinct(bandwidth_label)

  plot_data <- tidyr::expand_grid(
    facet_grid_df,
    geography_id = unique(base_map$geography_id)
  ) %>%
    left_join(summary_df, by = c("bandwidth_label", "geography_id")) %>%
    left_join(base_map, by = "geography_id") %>%
    st_as_sf()

  ggplot(plot_data) +
    geom_sf(aes(fill = .data[[metric_col]]), color = "white", linewidth = 0.08) +
    facet_wrap(~bandwidth_label) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-limit_value, limit_value),
      labels = scales::label_number(big.mark = ","),
      na.value = "grey92"
    ) +
    coord_sf(datum = NA) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      fill = NULL
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

panel <- arrow::read_parquet(panel_input) %>%
  mutate(
    pin10 = as.character(pin10),
    tax_year = as.integer(tax_year),
    ward_2014 = as.integer(ward_2014),
    in_500ft = as.logical(in_500ft),
    in_1000ft = as.logical(in_1000ft),
    switched_2015 = as.logical(switched_2015),
    valid_control_2015 = as.logical(valid_control_2015),
    land_only = as.logical(land_only),
    baseline_empty_last_pre_2012_2014 = as.logical(baseline_empty_last_pre_2012_2014)
  )

if (nrow(panel) == 0) {
  stop("Capitalization sanity-check panel is empty.", call. = FALSE)
}

if (anyDuplicated(panel[c("pin10", "tax_year")]) > 0) {
  stop("Capitalization sanity-check panel has duplicate pin10 x tax_year rows.", call. = FALSE)
}

coords_tbl <- panel %>%
  distinct(pin10, latitude, longitude) %>%
  mutate(coord_key = coord_key_from_latlon(latitude, longitude))

geo_lookup_raw <- build_coord_geography_lookup(coords_tbl, ward_panel_input, community_area_input)

geo_lookup_duplicates <- geo_lookup_raw %>%
  count(coord_key, name = "n_rows") %>%
  filter(n_rows > 1)

if (nrow(geo_lookup_duplicates) > 0) {
  warning(sprintf(
    "Aggregate geography lookup returned duplicate coord_key rows for %d coordinates; keeping the first match.",
    nrow(geo_lookup_duplicates)
  ))
}

geo_lookup <- geo_lookup_raw %>%
  distinct(coord_key, .keep_all = TRUE) %>%
  select(coord_key, community_area, community_name)

panel <- panel %>%
  left_join(
    coords_tbl %>% select(pin10, coord_key),
    by = "pin10",
    relationship = "many-to-one"
  ) %>%
  left_join(
    geo_lookup,
    by = "coord_key",
    relationship = "many-to-one"
  ) %>%
  mutate(
    treatment_sign = case_when(
      strictness_change < 0 ~ "to_lenient",
      strictness_change > 0 ~ "to_stricter",
      TRUE ~ "no_change"
    )
  )

panel_views <- bind_rows(
  panel %>%
    filter(land_only) %>%
    mutate(sample_view = "current_land_only"),
  panel %>%
    filter(baseline_empty_last_pre_2012_2014 %in% TRUE) %>%
    mutate(sample_view = "baseline_empty_last_pre_2012_2014")
) %>%
  filter(tax_year %in% plot_years) %>%
  make_bandwidth_views() %>%
  mutate(
    sample_view = factor(sample_view, levels = names(sample_view_labels)),
    sample_view_label = factor(sample_view_labels[as.character(sample_view)], levels = unname(sample_view_labels)),
    bandwidth = factor(bandwidth, levels = c("1000ft", "500ft")),
    bandwidth_label = factor(bandwidth_labels[as.character(bandwidth)], levels = unname(bandwidth_labels)),
    treatment_sign = factor(treatment_sign, levels = c("to_lenient", "no_change", "to_stricter")),
    treatment_sign_label = factor(treatment_sign_labels[as.character(treatment_sign)], levels = unname(treatment_sign_labels)),
    year_label = factor(tax_year)
  )

support_year <- panel_views %>%
  group_by(sample_view, bandwidth, tax_year, treatment_sign) %>%
  summarise(
    n_rows = n(),
    n_pin10 = n_distinct(pin10),
    n_switchers = n_distinct(pin10[switched_2015]),
    n_valid_controls = n_distinct(pin10[valid_control_2015]),
    .groups = "drop"
  ) %>%
  mutate(diagnostic_group = "year_support")

paired_support <- panel_views %>%
  distinct(sample_view, bandwidth, pin10, treatment_sign, switched_2015, valid_control_2015, tax_year) %>%
  group_by(sample_view, bandwidth, pin10) %>%
  summarise(
    treatment_sign = dplyr::first(treatment_sign),
    switched_2015 = dplyr::first(switched_2015),
    valid_control_2015 = dplyr::first(valid_control_2015),
    has_2014 = any(tax_year == 2014L),
    has_2016 = any(tax_year == 2016L),
    .groups = "drop"
  ) %>%
  group_by(sample_view, bandwidth, treatment_sign) %>%
  summarise(
    n_pin10 = n(),
    n_pin10_with_2014 = sum(has_2014),
    n_pin10_with_2016 = sum(has_2016),
    n_pin10_with_both_2014_2016 = sum(has_2014 & has_2016),
    n_switchers = sum(switched_2015),
    n_valid_controls = sum(valid_control_2015),
    .groups = "drop"
  ) %>%
  mutate(
    diagnostic_group = "paired_2014_2016_support",
    tax_year = NA_integer_,
    n_rows = NA_integer_
  )

support_summary <- bind_rows(
  support_year %>%
    mutate(
      n_pin10_with_2014 = NA_integer_,
      n_pin10_with_2016 = NA_integer_,
      n_pin10_with_both_2014_2016 = NA_integer_
    ),
  paired_support
) %>%
  arrange(diagnostic_group, sample_view, bandwidth, tax_year, treatment_sign)

diagnostics_year <- panel_views %>%
  group_by(sample_view, bandwidth, tax_year) %>%
  summarise(
    n_pin10 = n_distinct(pin10),
    share_land_sum_missing = mean(is.na(land_sum)),
    share_land_sum_zero = mean(tidyr::replace_na(land_sum, 0) <= 0),
    share_lot_sqft_missing = mean(is.na(lot_sqft_current) | lot_sqft_current <= 0),
    share_land_psf_missing = mean(is.na(land_psf)),
    land_psf_p01 = safe_positive_quantile(land_psf, 0.01),
    land_psf_p50 = safe_positive_quantile(land_psf, 0.50),
    land_psf_p99 = safe_positive_quantile(land_psf, 0.99),
    .groups = "drop"
  ) %>%
  mutate(
    diagnostic_group = "year_missingness",
    treatment_sign = NA_character_,
    n_pin10_with_both_2014_2016 = NA_integer_,
    share_positive_building_2016 = NA_real_,
    share_not_land_only_2016 = NA_real_
  )

baseline_transition <- panel %>%
  filter(baseline_empty_last_pre_2012_2014 %in% TRUE, tax_year %in% c(2014L, 2016L)) %>%
  distinct(pin10, tax_year, bldg_sum, land_only, strictness_change, switched_2015, valid_control_2015, in_500ft) %>%
  mutate(
    treatment_sign = case_when(
      strictness_change < 0 ~ "to_lenient",
      strictness_change > 0 ~ "to_stricter",
      TRUE ~ "no_change"
    )
  ) %>%
  make_bandwidth_views() %>%
  group_by(bandwidth, pin10) %>%
  arrange(tax_year, .by_group = TRUE) %>%
  summarise(
    treatment_sign = dplyr::first(treatment_sign),
    switched_2015 = dplyr::first(switched_2015),
    valid_control_2015 = dplyr::first(valid_control_2015),
    has_2014 = any(tax_year == 2014L),
    has_2016 = any(tax_year == 2016L),
    bldg_sum_2016 = if (any(tax_year == 2016L)) bldg_sum[tax_year == 2016L][1] else NA_real_,
    land_only_2016 = if (any(tax_year == 2016L)) land_only[tax_year == 2016L][1] else NA,
    .groups = "drop"
  ) %>%
  filter(has_2014 & has_2016) %>%
  group_by(bandwidth, treatment_sign) %>%
  summarise(
    n_pin10 = n(),
    n_switchers = sum(switched_2015),
    n_valid_controls = sum(valid_control_2015),
    n_pin10_with_both_2014_2016 = n(),
    share_positive_building_2016 = mean(bldg_sum_2016 > 0, na.rm = TRUE),
    share_not_land_only_2016 = mean(!land_only_2016, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    diagnostic_group = "baseline_transition_2014_2016",
    sample_view = "baseline_empty_last_pre_2012_2014",
    tax_year = NA_integer_,
    share_land_sum_missing = NA_real_,
    share_land_sum_zero = NA_real_,
    share_lot_sqft_missing = NA_real_,
    share_land_psf_missing = NA_real_,
    land_psf_p01 = NA_real_,
    land_psf_p50 = NA_real_,
    land_psf_p99 = NA_real_
  )

diagnostics_summary <- bind_rows(
  diagnostics_year,
  baseline_transition
) %>%
  arrange(diagnostic_group, sample_view, bandwidth, tax_year, treatment_sign)

distribution_summary <- bind_rows(
  panel_views %>%
    transmute(sample_view, bandwidth, tax_year, outcome = "land_sum", value = land_sum, positive_only = TRUE),
  panel_views %>%
    transmute(sample_view, bandwidth, tax_year, outcome = "land_psf", value = land_psf, positive_only = TRUE),
  panel_views %>%
    transmute(sample_view, bandwidth, tax_year, outcome = "log_land_sum", value = log_land_sum, positive_only = FALSE),
  panel_views %>%
    transmute(sample_view, bandwidth, tax_year, outcome = "log_land_psf", value = log_land_psf, positive_only = FALSE)
) %>%
  group_by(sample_view, bandwidth, tax_year, outcome, positive_only) %>%
  summarise(
    n_obs = sum(!is.na(value)),
    n_positive = sum(value > 0, na.rm = TRUE),
    p01 = ifelse(dplyr::first(positive_only), safe_positive_quantile(value, 0.01), safe_quantile(value, 0.01)),
    p05 = ifelse(dplyr::first(positive_only), safe_positive_quantile(value, 0.05), safe_quantile(value, 0.05)),
    p50 = ifelse(dplyr::first(positive_only), safe_positive_quantile(value, 0.50), safe_quantile(value, 0.50)),
    p95 = ifelse(dplyr::first(positive_only), safe_positive_quantile(value, 0.95), safe_quantile(value, 0.95)),
    p99 = ifelse(dplyr::first(positive_only), safe_positive_quantile(value, 0.99), safe_quantile(value, 0.99)),
    .groups = "drop"
  ) %>%
  select(-positive_only) %>%
  arrange(outcome, sample_view, bandwidth, tax_year)

distribution_levels <- bind_rows(
  panel_views %>%
    filter(!is.na(land_sum), land_sum > 0) %>%
    transmute(sample_view_label, bandwidth_label, year_label, outcome = "land_sum", value = land_sum),
  panel_views %>%
    filter(!is.na(land_psf), land_psf > 0) %>%
    transmute(sample_view_label, bandwidth_label, year_label, outcome = "land_psf", value = land_psf)
) %>%
  mutate(outcome = factor(outcome, levels = c("land_sum", "land_psf")))

distribution_levels_plot <- distribution_levels %>%
  ggplot(aes(x = value, color = year_label, fill = year_label)) +
  geom_density(alpha = 0.08, linewidth = 0.9) +
  facet_grid(outcome ~ sample_view_label + bandwidth_label, scales = "free") +
  scale_x_log10(labels = scales::label_number(big.mark = ",")) +
  scale_color_manual(values = c("2012" = "#1B4965", "2014" = "#2A9D8F", "2016" = "#C85C3D")) +
  scale_fill_manual(values = c("2012" = "#1B4965", "2014" = "#2A9D8F", "2016" = "#C85C3D")) +
  labs(
    title = "Positive land-value level distributions",
    subtitle = "Density plots on a log10 x-scale for 2012, 2014, and 2016",
    x = NULL,
    y = NULL,
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

distribution_logs <- bind_rows(
  panel_views %>%
    filter(!is.na(log_land_sum)) %>%
    transmute(sample_view_label, bandwidth_label, year_label, outcome = "log_land_sum", value = log_land_sum),
  panel_views %>%
    filter(!is.na(log_land_psf)) %>%
    transmute(sample_view_label, bandwidth_label, year_label, outcome = "log_land_psf", value = log_land_psf)
) %>%
  mutate(outcome = factor(outcome, levels = c("log_land_sum", "log_land_psf")))

distribution_logs_plot <- distribution_logs %>%
  ggplot(aes(x = value, color = year_label, fill = year_label)) +
  geom_density(alpha = 0.08, linewidth = 0.9) +
  facet_grid(outcome ~ sample_view_label + bandwidth_label, scales = "free") +
  scale_color_manual(values = c("2012" = "#1B4965", "2014" = "#2A9D8F", "2016" = "#C85C3D")) +
  scale_fill_manual(values = c("2012" = "#1B4965", "2014" = "#2A9D8F", "2016" = "#C85C3D")) +
  labs(
    title = "Logged land-value distributions",
    subtitle = "Positive-land observations only",
    x = NULL,
    y = NULL,
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

map_summary <- bind_rows(
  panel_views %>%
    filter(!is.na(ward_2014), tax_year %in% c(2014L, 2016L)) %>%
    group_by(sample_view, sample_view_label, bandwidth, bandwidth_label, tax_year, year_label, geography_level = "ward", geography_id = ward_2014) %>%
    summarise(
      n_pin10 = n_distinct(pin10),
      median_land_psf = safe_positive_median(land_psf),
      .groups = "drop"
    ) %>%
    mutate(geography_name = paste("Ward", geography_id)),
  panel_views %>%
    filter(!is.na(community_area), tax_year %in% c(2014L, 2016L)) %>%
    group_by(sample_view, sample_view_label, bandwidth, bandwidth_label, tax_year, year_label, geography_level = "community_area", geography_id = as.integer(community_area), geography_name = community_name) %>%
    summarise(
      n_pin10 = n_distinct(pin10),
      median_land_psf = safe_positive_median(land_psf),
      .groups = "drop"
    )
) %>%
  arrange(geography_level, sample_view, bandwidth, geography_id, tax_year)

change_summary <- map_summary %>%
  filter(sample_view == "baseline_empty_last_pre_2012_2014", tax_year %in% c(2014L, 2016L)) %>%
  select(geography_level, geography_id, geography_name, bandwidth, bandwidth_label, tax_year, n_pin10, median_land_psf) %>%
  tidyr::pivot_wider(
    names_from = tax_year,
    values_from = c(n_pin10, median_land_psf),
    names_sep = "_"
  ) %>%
  mutate(
    n_pin10_change_2016_minus_2014 = n_pin10_2016 - n_pin10_2014,
    median_land_psf_change_2016_minus_2014 = median_land_psf_2016 - median_land_psf_2014
  ) %>%
  arrange(geography_level, bandwidth, geography_id)

ward_base_map <- load_map_geography("ward", ward_panel_input, community_area_input, 2014) %>%
  st_make_valid()

community_base_map <- load_map_geography("community_area", ward_panel_input, community_area_input, 2014) %>%
  st_make_valid()

ward_summary <- map_summary %>%
  filter(geography_level == "ward")

community_summary <- map_summary %>%
  filter(geography_level == "community_area")

ward_count_plot <- build_level_map(
  ward_base_map,
  ward_summary,
  "n_pin10",
  "Ward counts on fixed pre-2015 ward geography",
  "2014 and 2016, current land-only view and baseline-empty cohort"
)

ward_land_psf_plot <- build_level_map(
  ward_base_map,
  ward_summary,
  "median_land_psf",
  "Ward median land value per square foot on fixed pre-2015 ward geography",
  "Positive land-psf observations only"
)

community_count_plot <- build_level_map(
  community_base_map,
  community_summary,
  "n_pin10",
  "Community-area counts",
  "2014 and 2016, current land-only view and baseline-empty cohort"
)

community_land_psf_plot <- build_level_map(
  community_base_map,
  community_summary,
  "median_land_psf",
  "Community-area median land value per square foot",
  "Positive land-psf observations only"
)

ward_change_plot <- build_change_map(
  ward_base_map,
  change_summary %>% filter(geography_level == "ward"),
  "n_pin10_change_2016_minus_2014",
  "Baseline-empty cohort ward count change, 2016 minus 2014",
  "Fixed pre-2015 ward geography"
)

ward_land_psf_change_plot <- build_change_map(
  ward_base_map,
  change_summary %>% filter(geography_level == "ward"),
  "median_land_psf_change_2016_minus_2014",
  "Baseline-empty cohort ward median land-psf change, 2016 minus 2014",
  "Fixed pre-2015 ward geography"
)

community_count_change_plot <- build_change_map(
  community_base_map,
  change_summary %>% filter(geography_level == "community_area"),
  "n_pin10_change_2016_minus_2014",
  "Baseline-empty cohort community-area count change, 2016 minus 2014",
  NULL
)

community_land_psf_change_plot <- build_change_map(
  community_base_map,
  change_summary %>% filter(geography_level == "community_area"),
  "median_land_psf_change_2016_minus_2014",
  "Baseline-empty cohort community-area median land-psf change, 2016 minus 2014",
  NULL
)

write_csv(support_summary, file.path(output_dir, "parcel_land_capitalization_support_summary.csv"))
write_csv(diagnostics_summary, file.path(output_dir, "parcel_land_capitalization_diagnostics.csv"))
write_csv(distribution_summary, file.path(output_dir, "parcel_land_capitalization_distribution_summary.csv"))
write_csv(map_summary, file.path(output_dir, "parcel_land_capitalization_map_summary.csv"))
write_csv(change_summary, file.path(output_dir, "parcel_land_capitalization_change_summary.csv"))

save_plot_pair(
  distribution_levels_plot,
  file.path(output_dir, "parcel_land_capitalization_distribution_levels.pdf"),
  file.path(output_dir, "parcel_land_capitalization_distribution_levels.png"),
  width = 13,
  height = 8.5
)

save_plot_pair(
  distribution_logs_plot,
  file.path(output_dir, "parcel_land_capitalization_distribution_logs.pdf"),
  file.path(output_dir, "parcel_land_capitalization_distribution_logs.png"),
  width = 13,
  height = 8.5
)

save_plot_pair(
  ward_count_plot,
  file.path(output_dir, "parcel_land_capitalization_ward_count_maps.pdf"),
  file.path(output_dir, "parcel_land_capitalization_ward_count_maps.png"),
  width = 13,
  height = 8.5
)

save_plot_pair(
  ward_land_psf_plot,
  file.path(output_dir, "parcel_land_capitalization_ward_land_psf_maps.pdf"),
  file.path(output_dir, "parcel_land_capitalization_ward_land_psf_maps.png"),
  width = 13,
  height = 8.5
)

save_plot_pair(
  community_count_plot,
  file.path(output_dir, "parcel_land_capitalization_community_count_maps.pdf"),
  file.path(output_dir, "parcel_land_capitalization_community_count_maps.png"),
  width = 13,
  height = 8.5
)

save_plot_pair(
  community_land_psf_plot,
  file.path(output_dir, "parcel_land_capitalization_community_land_psf_maps.pdf"),
  file.path(output_dir, "parcel_land_capitalization_community_land_psf_maps.png"),
  width = 13,
  height = 8.5
)

save_plot_pair(
  ward_change_plot,
  file.path(output_dir, "parcel_land_capitalization_ward_count_change_maps.pdf"),
  file.path(output_dir, "parcel_land_capitalization_ward_count_change_maps.png"),
  width = 11,
  height = 6
)

save_plot_pair(
  ward_land_psf_change_plot,
  file.path(output_dir, "parcel_land_capitalization_ward_land_psf_change_maps.pdf"),
  file.path(output_dir, "parcel_land_capitalization_ward_land_psf_change_maps.png"),
  width = 11,
  height = 6
)

save_plot_pair(
  community_count_change_plot,
  file.path(output_dir, "parcel_land_capitalization_community_count_change_maps.pdf"),
  file.path(output_dir, "parcel_land_capitalization_community_count_change_maps.png"),
  width = 11,
  height = 6
)

save_plot_pair(
  community_land_psf_change_plot,
  file.path(output_dir, "parcel_land_capitalization_community_land_psf_change_maps.pdf"),
  file.path(output_dir, "parcel_land_capitalization_community_land_psf_change_maps.png"),
  width = 11,
  height = 6
)

message(sprintf("Saved capitalization sanity-check outputs to %s", output_dir))
