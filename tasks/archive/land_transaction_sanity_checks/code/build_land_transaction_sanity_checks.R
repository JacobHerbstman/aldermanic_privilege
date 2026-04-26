source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")

options(dplyr.summarise.inform = FALSE)
sf_use_s2(FALSE)

safe_quantile <- function(x, prob) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = prob, names = FALSE, na.rm = TRUE))
}

safe_median <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  stats::median(x, na.rm = TRUE)
}

safe_positive_quantile <- function(x, prob) {
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = prob, names = FALSE, na.rm = TRUE))
}

safe_positive_median <- function(x) {
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  if (length(x) == 0) {
    return(NA_real_)
  }
  stats::median(x, na.rm = TRUE)
}

save_plot_pair <- function(plot_obj, pdf_path, png_path, width, height) {
  ggplot2::ggsave(pdf_path, plot_obj, width = width, height = height, bg = "white")
  ggplot2::ggsave(png_path, plot_obj, width = width, height = height, dpi = 220, bg = "white")
}

make_bandwidth_views <- function(df) {
  dplyr::bind_rows(
    df %>% dplyr::mutate(bandwidth = "1000ft"),
    df %>% dplyr::filter(in_500ft) %>% dplyr::mutate(bandwidth = "500ft")
  )
}

build_level_map <- function(base_map, summary_df, fill_col, title_text, subtitle_text) {
  facet_grid_df <- summary_df %>%
    dplyr::distinct(bandwidth_label, year_label)

  plot_data <- tidyr::expand_grid(
    facet_grid_df,
    geography_id = unique(base_map$geography_id)
  ) %>%
    dplyr::left_join(summary_df, by = c("bandwidth_label", "year_label", "geography_id")) %>%
    dplyr::left_join(base_map, by = "geography_id") %>%
    sf::st_as_sf()

  ggplot(plot_data) +
    geom_sf(aes(fill = .data[[fill_col]]), color = "white", linewidth = 0.08) +
    facet_grid(year_label ~ bandwidth_label) +
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

cohort_labels <- c(
  cohort_2012 = "2007-2017 window (2012 cohort)",
  cohort_2015 = "2010-2020 window (2015 cohort)"
)

bandwidth_labels <- c(
  `1000ft` = "1,000 ft",
  `500ft` = "500 ft"
)

sample_labels <- c(
  land_like = "Primary land-like sample",
  raw_land = "Strict raw LAND tag"
)

map_years <- c(2012L, 2014L, 2016L)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/land_transaction_sanity_checks/code")
# events_input <- "../input/land_transaction_sale_events.parquet"
# incidence_input <- "../input/land_transaction_incidence_panel.parquet"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# output_dir <- "../output"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(events_input, incidence_input, ward_panel_input, community_area_input, output_dir)
}

if (length(cli_args) != 5) {
  stop(
    "FATAL: Script requires 5 args: <events_input_parquet> <incidence_input_parquet> <ward_panel_input> <community_area_input> <output_dir>",
    call. = FALSE
  )
}

events_input <- cli_args[1]
incidence_input <- cli_args[2]
ward_panel_input <- cli_args[3]
community_area_input <- cli_args[4]
output_dir <- cli_args[5]

events_dt <- data.table::as.data.table(arrow::read_parquet(events_input))
events_dt[, `:=`(
  pin10 = as.character(pin10),
  sale_year = as.integer(sale_year),
  sale_price = as.numeric(sale_price),
  ward_2014 = as.integer(ward_2014),
  in_500ft = as.logical(in_500ft),
  in_1000ft = as.logical(in_1000ft),
  sale_time_land_like_flag = as.logical(sale_time_land_like_flag),
  raw_land_sale_flag = as.logical(raw_land_sale_flag)
)]

incidence_dt <- data.table::as.data.table(arrow::read_parquet(incidence_input))
incidence_dt[, `:=`(
  pin10 = as.character(pin10),
  calendar_year = as.integer(calendar_year),
  sold_any = as.integer(sold_any),
  sold_land_like = as.integer(sold_land_like),
  sold_land_tag = as.integer(sold_land_tag),
  n_sales_any = as.integer(n_sales_any),
  n_sales_land_like = as.integer(n_sales_land_like),
  n_sales_land_tag = as.integer(n_sales_land_tag),
  in_500ft = as.logical(in_500ft),
  in_1000ft = as.logical(in_1000ft),
  ward_2014 = as.integer(ward_2014),
  longitude = as.numeric(longitude),
  latitude = as.numeric(latitude)
)]

denominator_coords <- incidence_dt %>%
  dplyr::distinct(pin10, latitude, longitude) %>%
  dplyr::mutate(coord_key = coord_key_from_latlon(latitude, longitude))

geo_lookup <- build_coord_geography_lookup(denominator_coords, ward_panel_input, community_area_input) %>%
  dplyr::distinct(coord_key, .keep_all = TRUE) %>%
  dplyr::select(coord_key, community_area, community_name)

denominator_static <- incidence_dt %>%
  dplyr::distinct(
    pin10,
    longitude,
    latitude,
    ward_2014,
    in_500ft,
    in_1000ft
  ) %>%
  dplyr::mutate(coord_key = coord_key_from_latlon(latitude, longitude)) %>%
  dplyr::left_join(geo_lookup, by = "coord_key", relationship = "many-to-one")

events_dt <- events_dt %>%
  dplyr::left_join(
    denominator_static %>% dplyr::select(pin10, community_area, community_name) %>% dplyr::distinct(),
    by = "pin10",
    relationship = "many-to-one"
  )

pooled_years <- 2007L:2020L

annual_denominator <- tidyr::expand_grid(
  denominator_static %>% dplyr::select(-coord_key),
  calendar_year = pooled_years
)

annual_events <- events_dt %>%
  dplyr::group_by(pin10, calendar_year = sale_year) %>%
  dplyr::summarise(
    sold_any = 1L,
    sold_land_like = as.integer(any(sale_time_land_like_flag %in% TRUE, na.rm = TRUE)),
    sold_land_tag = as.integer(any(raw_land_sale_flag %in% TRUE, na.rm = TRUE)),
    n_sales_any = dplyr::n(),
    n_sales_land_like = sum(sale_time_land_like_flag %in% TRUE, na.rm = TRUE),
    n_sales_land_tag = sum(raw_land_sale_flag %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  )

annual_panel <- annual_denominator %>%
  dplyr::left_join(annual_events, by = c("pin10", "calendar_year")) %>%
  dplyr::mutate(
    sold_any = tidyr::replace_na(sold_any, 0L),
    sold_land_like = tidyr::replace_na(sold_land_like, 0L),
    sold_land_tag = tidyr::replace_na(sold_land_tag, 0L),
    n_sales_any = tidyr::replace_na(n_sales_any, 0L),
    n_sales_land_like = tidyr::replace_na(n_sales_land_like, 0L),
    n_sales_land_tag = tidyr::replace_na(n_sales_land_tag, 0L)
  ) %>%
  make_bandwidth_views() %>%
  dplyr::mutate(
    bandwidth = factor(bandwidth, levels = c("1000ft", "500ft")),
    bandwidth_label = factor(bandwidth_labels[as.character(bandwidth)], levels = unname(bandwidth_labels))
  )

citywide_incidence_summary <- annual_panel %>%
  dplyr::group_by(bandwidth, bandwidth_label, calendar_year) %>%
  dplyr::summarise(
    n_denominator_pin10 = dplyr::n(),
    share_sold_any = mean(sold_any),
    share_sold_land_like = mean(sold_land_like),
    share_sold_land_tag = mean(sold_land_tag),
    n_sales_any = sum(n_sales_any),
    n_sales_land_like = sum(n_sales_land_like),
    n_sales_land_tag = sum(n_sales_land_tag),
    .groups = "drop"
  ) %>%
  dplyr::arrange(bandwidth, calendar_year)

citywide_incidence_plot <- citywide_incidence_summary %>%
  tidyr::pivot_longer(
    cols = c(share_sold_any, share_sold_land_like, share_sold_land_tag),
    names_to = "series",
    values_to = "share_sold"
  ) %>%
  dplyr::mutate(
    series = dplyr::recode(
      series,
      share_sold_any = "Any sale",
      share_sold_land_like = "Land-like sale",
      share_sold_land_tag = "Raw LAND-tag sale"
    )
  ) %>%
  ggplot(aes(x = calendar_year, y = share_sold, color = series)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = c(2012, 2015), linetype = "dashed", color = "grey60") +
  facet_wrap(~bandwidth_label, ncol = 1) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  scale_color_manual(values = c(
    "Any sale" = "#2A9D8F",
    "Land-like sale" = "#1B4965",
    "Raw LAND-tag sale" = "#C85C3D"
  )) +
  labs(
    title = "Citywide annual sale incidence for the baseline-empty cohort",
    subtitle = "Shares use the fixed near-border denominator; dashed lines mark 2012 announcement and 2015 implementation",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

price_events <- dplyr::bind_rows(
  events_dt %>%
    dplyr::filter(sale_price > 0, sale_time_land_like_flag %in% TRUE) %>%
    dplyr::mutate(sample_definition = "land_like"),
  events_dt %>%
    dplyr::filter(sale_price > 0, raw_land_sale_flag %in% TRUE) %>%
    dplyr::mutate(sample_definition = "raw_land")
) %>%
  make_bandwidth_views() %>%
  dplyr::mutate(
    bandwidth = factor(bandwidth, levels = c("1000ft", "500ft")),
    bandwidth_label = factor(bandwidth_labels[as.character(bandwidth)], levels = unname(bandwidth_labels)),
    sample_definition = factor(sample_definition, levels = c("land_like", "raw_land")),
    sample_label = factor(sample_labels[as.character(sample_definition)], levels = unname(sample_labels))
  )

citywide_price_summary <- price_events %>%
  dplyr::group_by(bandwidth, bandwidth_label, calendar_year = sale_year, sample_definition, sample_label) %>%
  dplyr::summarise(
    n_sales = dplyr::n(),
    p10_sale_price = safe_positive_quantile(sale_price, 0.10),
    p25_sale_price = safe_quantile(sale_price, 0.25),
    median_sale_price = safe_positive_median(sale_price),
    p75_sale_price = safe_quantile(sale_price, 0.75),
    p90_sale_price = safe_positive_quantile(sale_price, 0.90),
    .groups = "drop"
  ) %>%
  dplyr::arrange(bandwidth, sample_definition, calendar_year)

citywide_price_plot <- citywide_price_summary %>%
  ggplot(aes(x = calendar_year, y = median_sale_price, color = sample_label, fill = sample_label)) +
  geom_ribbon(aes(ymin = p25_sale_price, ymax = p75_sale_price), alpha = 0.10, linewidth = 0) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = c(2012, 2015), linetype = "dashed", color = "grey60") +
  facet_wrap(~bandwidth_label, ncol = 1) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1, big.mark = ",")) +
  scale_color_manual(values = c(
    "Primary land-like sample" = "#1B4965",
    "Strict raw LAND tag" = "#C85C3D"
  )) +
  scale_fill_manual(values = c(
    "Primary land-like sample" = "#1B4965",
    "Strict raw LAND tag" = "#C85C3D"
  )) +
  labs(
    title = "Citywide annual land-transaction prices",
    subtitle = "Median with interquartile range for the primary and strict price samples",
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

distribution_summary <- dplyr::bind_rows(
  price_events %>%
    dplyr::filter(data.table::between(sale_year, 2007L, 2017L)) %>%
    dplyr::mutate(cohort = "cohort_2012"),
  price_events %>%
    dplyr::filter(data.table::between(sale_year, 2010L, 2020L)) %>%
    dplyr::mutate(cohort = "cohort_2015")
) %>%
  dplyr::group_by(cohort, bandwidth, bandwidth_label, sample_definition, sample_label) %>%
  dplyr::summarise(
    n_sales = dplyr::n(),
    p01_sale_price = safe_positive_quantile(sale_price, 0.01),
    p50_sale_price = safe_positive_median(sale_price),
    p99_sale_price = safe_positive_quantile(sale_price, 0.99),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    cohort_label = factor(cohort_labels[cohort], levels = unname(cohort_labels))
  ) %>%
  dplyr::arrange(cohort, bandwidth, sample_definition)

distribution_plot_df <- dplyr::bind_rows(
  price_events %>%
    dplyr::filter(data.table::between(sale_year, 2007L, 2017L)) %>%
    dplyr::mutate(cohort = "cohort_2012"),
  price_events %>%
    dplyr::filter(data.table::between(sale_year, 2010L, 2020L)) %>%
    dplyr::mutate(cohort = "cohort_2015")
) %>%
  dplyr::mutate(
    cohort_label = factor(cohort_labels[cohort], levels = unname(cohort_labels))
  )

price_distribution_plot <- distribution_plot_df %>%
  ggplot(aes(x = sale_price, color = cohort_label, fill = cohort_label)) +
  geom_density(alpha = 0.08, linewidth = 0.9) +
  facet_grid(sample_label ~ bandwidth_label, scales = "free") +
  scale_x_log10(labels = scales::label_dollar(scale = 1, big.mark = ",")) +
  scale_color_manual(values = c(
    "2007-2017 window (2012 cohort)" = "#1B4965",
    "2010-2020 window (2015 cohort)" = "#C85C3D"
  )) +
  scale_fill_manual(values = c(
    "2007-2017 window (2012 cohort)" = "#1B4965",
    "2010-2020 window (2015 cohort)" = "#C85C3D"
  )) +
  labs(
    title = "Land-transaction price distributions by cohort window",
    subtitle = "Positive-price events only, shown on a log10 x-scale",
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

incidence_map_summary <- dplyr::bind_rows(
  annual_panel %>%
    dplyr::filter(calendar_year %in% map_years, !is.na(ward_2014)) %>%
    dplyr::group_by(
      geography_level = "ward",
      geography_id = ward_2014,
      bandwidth,
      bandwidth_label,
      calendar_year
    ) %>%
    dplyr::summarise(
      geography_name = paste("Ward", dplyr::first(geography_id)),
      n_denominator_pin10 = dplyr::n(),
      share_sold_any = mean(sold_any),
      share_sold_land_like = mean(sold_land_like),
      share_sold_land_tag = mean(sold_land_tag),
      .groups = "drop"
    ),
  annual_panel %>%
    dplyr::filter(calendar_year %in% map_years, !is.na(community_area)) %>%
    dplyr::group_by(
      geography_level = "community_area",
      geography_id = as.integer(community_area),
      geography_name = community_name,
      bandwidth,
      bandwidth_label,
      calendar_year
    ) %>%
    dplyr::summarise(
      n_denominator_pin10 = dplyr::n(),
      share_sold_any = mean(sold_any),
      share_sold_land_like = mean(sold_land_like),
      share_sold_land_tag = mean(sold_land_tag),
      .groups = "drop"
    )
) %>%
  dplyr::mutate(year_label = factor(calendar_year))

primary_price_map_events <- price_events %>%
  dplyr::filter(
    sample_definition == "land_like",
    sale_year %in% map_years,
    sale_price > 0
  )

price_map_summary <- dplyr::bind_rows(
  primary_price_map_events %>%
    dplyr::filter(!is.na(ward_2014)) %>%
    dplyr::group_by(
      geography_level = "ward",
      geography_id = ward_2014,
      bandwidth,
      bandwidth_label,
      calendar_year = sale_year
    ) %>%
    dplyr::summarise(
      geography_name = paste("Ward", dplyr::first(geography_id)),
      n_land_like_sales = dplyr::n(),
      median_land_like_sale_price = safe_positive_median(sale_price),
      .groups = "drop"
    ),
  primary_price_map_events %>%
    dplyr::filter(!is.na(community_area)) %>%
    dplyr::group_by(
      geography_level = "community_area",
      geography_id = as.integer(community_area),
      geography_name = community_name,
      bandwidth,
      bandwidth_label,
      calendar_year = sale_year
    ) %>%
    dplyr::summarise(
      n_land_like_sales = dplyr::n(),
      median_land_like_sale_price = safe_positive_median(sale_price),
      .groups = "drop"
    )
) %>%
  dplyr::mutate(year_label = factor(calendar_year))

map_summary <- incidence_map_summary %>%
  dplyr::full_join(
    price_map_summary,
    by = c("geography_level", "geography_id", "geography_name", "bandwidth", "bandwidth_label", "calendar_year", "year_label")
  ) %>%
  dplyr::arrange(geography_level, bandwidth, geography_id, calendar_year)

ward_base_map <- load_map_geography("ward", ward_panel_input, community_area_input, 2014) %>%
  sf::st_make_valid()

community_base_map <- load_map_geography("community_area", ward_panel_input, community_area_input, 2014) %>%
  sf::st_make_valid()

ward_incidence_plot <- build_level_map(
  ward_base_map,
  incidence_map_summary %>% dplyr::filter(geography_level == "ward"),
  "share_sold_any",
  "Ward-level annual sale incidence",
  "Share of the baseline-empty denominator with any sale, on fixed pre-2015 ward geography"
)

community_incidence_plot <- build_level_map(
  community_base_map,
  incidence_map_summary %>% dplyr::filter(geography_level == "community_area"),
  "share_sold_any",
  "Community-area annual sale incidence",
  "Share of the baseline-empty denominator with any sale"
)

ward_price_plot <- build_level_map(
  ward_base_map,
  price_map_summary %>% dplyr::filter(geography_level == "ward"),
  "median_land_like_sale_price",
  "Ward-level median land-like sale price",
  "Median positive sale price among sale-time land-like events on fixed pre-2015 ward geography"
)

community_price_plot <- build_level_map(
  community_base_map,
  price_map_summary %>% dplyr::filter(geography_level == "community_area"),
  "median_land_like_sale_price",
  "Community-area median land-like sale price",
  "Median positive sale price among sale-time land-like events"
)

readr::write_csv(
  citywide_incidence_summary,
  file.path(output_dir, "land_transaction_citywide_incidence_summary.csv")
)
readr::write_csv(
  citywide_price_summary,
  file.path(output_dir, "land_transaction_citywide_price_summary.csv")
)
readr::write_csv(
  map_summary,
  file.path(output_dir, "land_transaction_map_summary.csv")
)
readr::write_csv(
  distribution_summary,
  file.path(output_dir, "land_transaction_price_distribution_summary.csv")
)

save_plot_pair(
  citywide_incidence_plot,
  file.path(output_dir, "land_transaction_citywide_incidence_series.pdf"),
  file.path(output_dir, "land_transaction_citywide_incidence_series.png"),
  width = 10,
  height = 7
)
save_plot_pair(
  citywide_price_plot,
  file.path(output_dir, "land_transaction_citywide_price_series.pdf"),
  file.path(output_dir, "land_transaction_citywide_price_series.png"),
  width = 10,
  height = 7
)
save_plot_pair(
  ward_incidence_plot,
  file.path(output_dir, "land_transaction_ward_incidence_maps.pdf"),
  file.path(output_dir, "land_transaction_ward_incidence_maps.png"),
  width = 11,
  height = 7
)
save_plot_pair(
  community_incidence_plot,
  file.path(output_dir, "land_transaction_community_incidence_maps.pdf"),
  file.path(output_dir, "land_transaction_community_incidence_maps.png"),
  width = 11,
  height = 7
)
save_plot_pair(
  ward_price_plot,
  file.path(output_dir, "land_transaction_ward_price_maps.pdf"),
  file.path(output_dir, "land_transaction_ward_price_maps.png"),
  width = 11,
  height = 7
)
save_plot_pair(
  community_price_plot,
  file.path(output_dir, "land_transaction_community_price_maps.pdf"),
  file.path(output_dir, "land_transaction_community_price_maps.png"),
  width = 11,
  height = 7
)
save_plot_pair(
  price_distribution_plot,
  file.path(output_dir, "land_transaction_price_distributions.pdf"),
  file.path(output_dir, "land_transaction_price_distributions.png"),
  width = 10,
  height = 7
)

message(sprintf("Saved land-transaction sanity outputs in %s", output_dir))
