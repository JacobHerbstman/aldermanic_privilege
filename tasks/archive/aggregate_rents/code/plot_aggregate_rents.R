source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/aggregate_rents/code")
# monthly_input <- "../output/rent_geography_monthly_summary.csv"
# period_input <- "../output/rent_map_period_summary.csv"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# citywide_yoy_output <- "../output/rent_citywide_yoy_monthly.csv"
# output_dir <- "../output"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    monthly_input,
    period_input,
    ward_panel_input,
    community_area_input,
    citywide_yoy_output,
    output_dir
  )
}

if (length(args) != 6) {
  stop(
    paste(
      "FATAL: Script requires 6 args:",
      "<monthly_input> <period_input> <ward_panel_input>",
      "<community_area_input> <citywide_yoy_output> <output_dir>"
    ),
    call. = FALSE
  )
}

monthly_input <- args[1]
period_input <- args[2]
ward_panel_input <- args[3]
community_area_input <- args[4]
citywide_yoy_output <- args[5]
output_dir <- args[6]

source_labels <- c(
  renthub_raw = "RentHub raw",
  renthub_clean = "RentHub clean",
  dwellsy = "Dwellsy"
)

source_colors <- c(
  renthub_raw = "#6D7A8A",
  renthub_clean = "#C8583D",
  dwellsy = "#1E6E61"
)

format_period_subtitle <- function(period_data) {
  period_type <- unique(period_data$period_type)
  current_end <- unique(period_data$current_end)
  prior_end <- unique(period_data$prior_end)

  if (period_type == "matched_ytd") {
    return(sprintf(
      "Matched-window YTD growth through %s versus the same months one year earlier",
      format(as.Date(current_end), "%b %Y")
    ))
  }

  sprintf(
    "Full-year growth in median real asking rent: %s versus %s",
    format(as.Date(current_end), "%Y"),
    format(as.Date(prior_end), "%Y")
  )
}

save_rent_map <- function(period_label, geography_level, map_period, ward_panel_input, community_area_input, output_dir) {
  period_data <- map_period %>%
    filter(period_label == !!period_label, geography_level == !!geography_level) %>%
    mutate(source_label = recode(source, !!!source_labels)) %>%
    filter(n_current >= 50, n_prior >= 50, is.finite(growth_pct))

  if (nrow(period_data) == 0) {
    return(invisible(NULL))
  }

  map_year <- max(lubridate::year(as.Date(period_data$current_end)))
  base_map <- load_map_geography(geography_level, ward_panel_input, community_area_input, map_year) %>%
    left_join(period_data, by = c("geography_id", "geography_name"))

  limit_value <- max(abs(base_map$growth_pct), na.rm = TRUE)
  if (!is.finite(limit_value) || limit_value == 0) {
    limit_value <- 1
  }

  plot_obj <- ggplot(base_map) +
    geom_sf(aes(fill = growth_pct), color = "white", linewidth = 0.08) +
    facet_wrap(~source_label) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-limit_value, limit_value),
      labels = function(x) sprintf("%.0f%%", x),
      na.value = "grey92"
    ) +
    coord_sf(datum = NA) +
    labs(
      title = sprintf("%s rent growth by %s", gsub("_", " ", period_label), gsub("_", " ", geography_level)),
      subtitle = format_period_subtitle(period_data),
      fill = "YoY growth"
    ) +
    theme_void(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )

  ggsave(
    filename = file.path(output_dir, sprintf("rent_map_%s_%s.pdf", geography_level, period_label)),
    plot = plot_obj,
    width = 11,
    height = 8
  )
}

message("Loading rent summaries...")
monthly_summary <- read_csv(monthly_input, show_col_types = FALSE) %>%
  mutate(month_start = as.Date(month_start))

map_period <- read_csv(period_input, show_col_types = FALSE) %>%
  mutate(
    current_start = as.Date(current_start),
    current_end = as.Date(current_end),
    prior_start = as.Date(prior_start),
    prior_end = as.Date(prior_end)
  )

citywide_yoy <- monthly_summary %>%
  filter(geography_level == "citywide") %>%
  arrange(source, month_start) %>%
  group_by(source) %>%
  mutate(
    yoy_rent_growth_pct = 100 * (median_rent_real_2024 / lag(median_rent_real_2024, 12) - 1),
    yoy_rent_per_sqft_growth_pct = 100 * (median_rent_per_sqft_real_2024 / lag(median_rent_per_sqft_real_2024, 12) - 1)
  ) %>%
  ungroup()

write_csv(citywide_yoy, citywide_yoy_output)

citywide_yoy_plot <- citywide_yoy %>%
  filter(!is.na(yoy_rent_growth_pct)) %>%
  mutate(source_label = recode(source, !!!source_labels))

longrun_plot <- ggplot(citywide_yoy_plot, aes(x = month_start, y = yoy_rent_growth_pct, color = source)) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = source_colors, labels = source_labels) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x)) +
  labs(
    title = "Citywide real rent growth by source",
    subtitle = "Monthly YoY growth in median real asking rent",
    x = NULL,
    y = "YoY growth",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(output_dir, "rent_citywide_yoy_longrun.pdf"),
  plot = longrun_plot,
  width = 10,
  height = 5.5
)

recent_plot <- citywide_yoy_plot %>%
  filter(month_start >= as.Date("2023-01-01")) %>%
  ggplot(aes(x = month_start, y = yoy_rent_growth_pct, color = source)) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = source_colors, labels = source_labels) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x)) +
  labs(
    title = "Citywide real rent growth, recent years",
    subtitle = "Recent monthly YoY growth in median real asking rent",
    x = NULL,
    y = "YoY growth",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(output_dir, "rent_citywide_yoy_recent.pdf"),
  plot = recent_plot,
  width = 10,
  height = 5.5
)

message("Saving rent maps...")
for (period_label in c(
  "2023_vs_2022_full_year",
  "2024_vs_2023_full_year",
  "2025_vs_2024_ytd",
  "2026_vs_2025_ytd"
)) {
  save_rent_map(period_label, "ward", map_period, ward_panel_input, community_area_input, output_dir)
  save_rent_map(period_label, "community_area", map_period, ward_panel_input, community_area_input, output_dir)
}

message("Saved citywide rent YoY CSV: ", citywide_yoy_output)
