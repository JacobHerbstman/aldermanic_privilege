source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")

options(dplyr.summarise.inform = FALSE)
sf_use_s2(FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/parcel_land_redistricting_sanity_checks/code")
# panel_input <- "../input/parcel_land_redistricting_panel.parquet"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# citywide_summary_output <- "../output/parcel_land_citywide_taxyear_summary.csv"
# geography_summary_output <- "../output/parcel_land_geography_taxyear_summary.csv"
# citywide_pdf_output <- "../output/parcel_land_citywide_series_panel.pdf"
# citywide_png_output <- "../output/parcel_land_citywide_series_panel.png"
# ward_pdf_output <- "../output/parcel_land_ward_heatmap_panel.pdf"
# ward_png_output <- "../output/parcel_land_ward_heatmap_panel.png"
# ca_pdf_output <- "../output/parcel_land_community_area_heatmap_panel.pdf"
# ca_png_output <- "../output/parcel_land_community_area_heatmap_panel.png"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    panel_input,
    ward_panel_input,
    community_area_input,
    citywide_summary_output,
    geography_summary_output,
    citywide_pdf_output,
    citywide_png_output,
    ward_pdf_output,
    ward_png_output,
    ca_pdf_output,
    ca_png_output
  )
}

if (length(args) != 11) {
  stop(
    paste(
      "FATAL: Script requires 11 args:",
      "<panel_input> <ward_panel_input> <community_area_input>",
      "<citywide_summary_output> <geography_summary_output>",
      "<citywide_pdf_output> <citywide_png_output>",
      "<ward_pdf_output> <ward_png_output>",
      "<ca_pdf_output> <ca_png_output>"
    ),
    call. = FALSE
  )
}

panel_input <- args[1]
ward_panel_input <- args[2]
community_area_input <- args[3]
citywide_summary_output <- args[4]
geography_summary_output <- args[5]
citywide_pdf_output <- args[6]
citywide_png_output <- args[7]
ward_pdf_output <- args[8]
ward_png_output <- args[9]
ca_pdf_output <- args[10]
ca_png_output <- args[11]

scope_from_flag <- function(in_500ft) {
  ifelse(in_500ft, "500ft", "1000ft")
}

format_big_dollars <- function(x) {
  scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)(x)
}

heat_fill_label <- function(x) {
  scales::label_number(scale = 1e-3, suffix = "k", accuracy = 0.1)(x)
}

panel <- arrow::read_parquet(panel_input) %>%
  mutate(
    pin10 = as.character(pin10),
    tax_year = as.integer(tax_year),
    ward_2014 = as.integer(ward_2014),
    in_500ft = as.logical(in_500ft),
    in_1000ft = as.logical(in_1000ft),
    switched_2015 = as.logical(switched_2015),
    valid_control_2015 = as.logical(valid_control_2015)
  )

if (nrow(panel) == 0) {
  stop("Sanity-check input panel is empty.", call. = FALSE)
}

if (anyDuplicated(panel[c("pin10", "tax_year")]) > 0) {
  stop("Sanity-check input panel has duplicate pin10 x tax_year rows.", call. = FALSE)
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
    "Aggregate geography lookup returned duplicate coord_key rows for %d coordinates; keeping the first match for sanity plots.",
    nrow(geo_lookup_duplicates)
  ))
}

geo_lookup <- geo_lookup_raw %>%
  distinct(coord_key, .keep_all = TRUE) %>%
  select(coord_key, community_area, community_name)

panel <- panel %>%
  left_join(coords_tbl %>% select(pin10, coord_key), by = "pin10", relationship = "many-to-one") %>%
  left_join(geo_lookup, by = "coord_key", relationship = "many-to-one")

if (any(is.na(panel$community_area))) {
  warning(sprintf(
    "Community-area assignment missing for %d rows in the near-border land panel.",
    sum(is.na(panel$community_area))
  ))
}

panel_1000 <- panel %>%
  mutate(scope = "1000ft")

panel_500 <- panel %>%
  filter(in_500ft) %>%
  mutate(scope = "500ft")

panel_plot <- bind_rows(panel_1000, panel_500)

citywide_summary <- panel_plot %>%
  group_by(scope, tax_year) %>%
  summarise(
    n_pin10 = n_distinct(pin10),
    n_switchers = n_distinct(pin10[switched_2015]),
    n_valid_controls = n_distinct(pin10[valid_control_2015]),
    total_land_sum = sum(land_sum, na.rm = TRUE),
    mean_land_sum = mean(land_sum, na.rm = TRUE),
    median_land_sum = median(land_sum, na.rm = TRUE),
    p25_land_sum = quantile(land_sum, 0.25, na.rm = TRUE),
    p75_land_sum = quantile(land_sum, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(scope, tax_year)

geography_summary <- bind_rows(
  panel_plot %>%
    filter(!is.na(ward_2014)) %>%
    group_by(scope, tax_year, geography_level = "ward_2014", geography_id = ward_2014) %>%
    summarise(
      geography_name = paste("Ward", first(ward_2014)),
      n_pin10 = n_distinct(pin10),
      total_land_sum = sum(land_sum, na.rm = TRUE),
      mean_land_sum = mean(land_sum, na.rm = TRUE),
      median_land_sum = median(land_sum, na.rm = TRUE),
      .groups = "drop"
    ),
  panel_plot %>%
    filter(!is.na(community_area)) %>%
    group_by(scope, tax_year, geography_level = "community_area", geography_id = as.integer(community_area)) %>%
    summarise(
      geography_name = first(community_name),
      n_pin10 = n_distinct(pin10),
      total_land_sum = sum(land_sum, na.rm = TRUE),
      mean_land_sum = mean(land_sum, na.rm = TRUE),
      median_land_sum = median(land_sum, na.rm = TRUE),
      .groups = "drop"
    )
) %>%
  arrange(geography_level, scope, geography_id, tax_year)

write_csv(citywide_summary, citywide_summary_output)
write_csv(geography_summary, geography_summary_output)

citywide_plot_data <- bind_rows(
  citywide_summary %>%
    transmute(scope, tax_year, metric = "Unique land-only parcels", value = n_pin10),
  citywide_summary %>%
    transmute(scope, tax_year, metric = "Total assessed land value", value = total_land_sum),
  citywide_summary %>%
    transmute(scope, tax_year, metric = "Median assessed land value", value = median_land_sum),
  citywide_summary %>%
    transmute(scope, tax_year, metric = "Switchers and valid controls", value = n_switchers, series = "Switchers"),
  citywide_summary %>%
    transmute(scope, tax_year, metric = "Switchers and valid controls", value = n_valid_controls, series = "Valid controls")
) %>%
  mutate(series = if_else(is.na(series), scope, series))

citywide_plot <- citywide_plot_data %>%
  ggplot(aes(x = tax_year, y = value, color = series)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c(
      "1000ft" = "#1B4965",
      "500ft" = "#C85C3D",
      "Switchers" = "#2A9D8F",
      "Valid controls" = "#6D597A"
    )
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = function(x) {
    if (max(x, na.rm = TRUE) > 1e6) {
      format_big_dollars(x)
    } else {
      scales::label_number(big.mark = ",")(x)
    }
  }) +
  labs(
    title = "Land-only near-border parcel sanity checks",
    subtitle = "Citywide annual series from the 2015 land-only parcel redistricting panel",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(citywide_pdf_output, citywide_plot, width = 10, height = 7.5, bg = "white")
ggsave(citywide_png_output, citywide_plot, width = 10, height = 7.5, dpi = 220, bg = "white")

ward_order <- geography_summary %>%
  filter(geography_level == "ward_2014", scope == "1000ft") %>%
  group_by(geography_id, geography_name) %>%
  summarise(total_count = sum(n_pin10, na.rm = TRUE), .groups = "drop") %>%
  arrange(total_count, geography_id) %>%
  pull(geography_name)

ward_heat_data <- bind_rows(
  geography_summary %>%
    filter(geography_level == "ward_2014") %>%
    transmute(scope, tax_year, geography_name, metric = "Parcel count", value = n_pin10),
  geography_summary %>%
    filter(geography_level == "ward_2014") %>%
    transmute(scope, tax_year, geography_name, metric = "Median land value", value = median_land_sum)
) %>%
  mutate(
    geography_name = factor(geography_name, levels = ward_order),
    metric = factor(metric, levels = c("Parcel count", "Median land value"))
  )

ward_heatmap <- ward_heat_data %>%
  ggplot(aes(x = tax_year, y = geography_name, fill = value)) +
  geom_tile(color = NA) +
  facet_grid(metric ~ scope, scales = "free_y") +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",
    labels = heat_fill_label
  ) +
  labs(
    title = "Pre-2015 ward sanity checks",
    subtitle = "Annual land-only near-border parcel counts and median land values by pre-2015 ward",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 9.5) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(ward_pdf_output, ward_heatmap, width = 11, height = 14, bg = "white")
ggsave(ward_png_output, ward_heatmap, width = 11, height = 14, dpi = 220, bg = "white")

ca_order <- geography_summary %>%
  filter(geography_level == "community_area", scope == "1000ft") %>%
  group_by(geography_id, geography_name) %>%
  summarise(total_count = sum(n_pin10, na.rm = TRUE), .groups = "drop") %>%
  arrange(total_count, geography_id) %>%
  pull(geography_name)

ca_heat_data <- bind_rows(
  geography_summary %>%
    filter(geography_level == "community_area") %>%
    transmute(scope, tax_year, geography_name, metric = "Parcel count", value = n_pin10),
  geography_summary %>%
    filter(geography_level == "community_area") %>%
    transmute(scope, tax_year, geography_name, metric = "Median land value", value = median_land_sum)
) %>%
  mutate(
    geography_name = factor(geography_name, levels = ca_order),
    metric = factor(metric, levels = c("Parcel count", "Median land value"))
  )

ca_heatmap <- ca_heat_data %>%
  ggplot(aes(x = tax_year, y = geography_name, fill = value)) +
  geom_tile(color = NA) +
  facet_grid(metric ~ scope, scales = "free_y") +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",
    labels = heat_fill_label
  ) +
  labs(
    title = "Community-area sanity checks",
    subtitle = "Annual land-only near-border parcel counts and median land values by community area",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 8.5) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(ca_pdf_output, ca_heatmap, width = 11, height = 18, bg = "white")
ggsave(ca_png_output, ca_heatmap, width = 11, height = 18, dpi = 220, bg = "white")
