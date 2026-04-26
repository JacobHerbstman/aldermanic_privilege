source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/aggregate_home_sales/code")
# monthly_input <- "../output/home_sales_geography_monthly_summary.csv"
# yearly_input <- "../output/home_sales_geography_yearly_summary.csv"
# period_input <- "../output/home_sales_map_period_summary.csv"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# citywide_monthly_yoy_output <- "../output/home_sales_citywide_yoy_monthly.csv"
# citywide_annual_yoy_output <- "../output/home_sales_citywide_yoy_annual.csv"
# output_dir <- "../output"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    monthly_input,
    yearly_input,
    period_input,
    ward_panel_input,
    community_area_input,
    citywide_monthly_yoy_output,
    citywide_annual_yoy_output,
    output_dir
  )
}

if (length(args) != 8) {
  stop(
    paste(
      "FATAL: Script requires 8 args:",
      "<monthly_input> <yearly_input> <period_input> <ward_panel_input>",
      "<community_area_input> <citywide_monthly_yoy_output>",
      "<citywide_annual_yoy_output> <output_dir>"
    ),
    call. = FALSE
  )
}

monthly_input <- args[1]
yearly_input <- args[2]
period_input <- args[3]
ward_panel_input <- args[4]
community_area_input <- args[5]
citywide_monthly_yoy_output <- args[6]
citywide_annual_yoy_output <- args[7]
output_dir <- args[8]

format_sales_period_subtitle <- function(period_data) {
  period_type <- unique(period_data$period_type)
  current_end <- unique(period_data$current_end)

  if (period_type == "matched_ytd") {
    return(sprintf(
      "Matched-window YTD growth through %s versus the same months one year earlier",
      format(as.Date(current_end), "%b %Y")
    ))
  }

  sprintf(
    "Full-year growth in median real sale price: %s versus %s",
    format(as.Date(unique(period_data$current_end)), "%Y"),
    format(as.Date(unique(period_data$prior_end)), "%Y")
  )
}

save_sales_map <- function(period_label, geography_level, period_summary, ward_panel_input, community_area_input, output_dir) {
  period_data <- period_summary %>%
    filter(period_label == !!period_label, geography_level == !!geography_level) %>%
    filter(n_current >= 10, n_prior >= 10, is.finite(growth_pct))

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
      title = sprintf("%s home-sales growth by %s", gsub("_", " ", period_label), gsub("_", " ", geography_level)),
      subtitle = format_sales_period_subtitle(period_data),
      fill = "YoY growth"
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )

  ggsave(
    filename = file.path(output_dir, sprintf("home_sales_map_%s_%s.pdf", geography_level, period_label)),
    plot = plot_obj,
    width = 8.5,
    height = 6.5
  )
}

message("Loading home sales summaries...")
monthly_summary <- read_csv(monthly_input, show_col_types = FALSE) %>%
  mutate(month_start = as.Date(month_start))

yearly_summary <- read_csv(yearly_input, show_col_types = FALSE)

period_summary <- read_csv(period_input, show_col_types = FALSE) %>%
  mutate(
    current_start = as.Date(current_start),
    current_end = as.Date(current_end),
    prior_start = as.Date(prior_start),
    prior_end = as.Date(prior_end)
  )

citywide_monthly_yoy <- monthly_summary %>%
  filter(geography_level == "citywide") %>%
  arrange(month_start) %>%
  mutate(
    yoy_sale_price_real_pct = 100 * (median_sale_price_real_2022 / lag(median_sale_price_real_2022, 12) - 1),
    yoy_transactions_pct = 100 * (n_transactions / lag(n_transactions, 12) - 1)
  )

citywide_month_coverage <- monthly_summary %>%
  filter(geography_level == "citywide") %>%
  group_by(year) %>%
  summarise(months_covered = n_distinct(month_start), .groups = "drop")

citywide_annual_yoy <- yearly_summary %>%
  filter(geography_level == "citywide") %>%
  arrange(year) %>%
  left_join(citywide_month_coverage, by = "year") %>%
  mutate(is_full_year = months_covered == 12) %>%
  mutate(
    yoy_sale_price_real_pct = if_else(
      is_full_year & lag(is_full_year, default = FALSE),
      100 * (median_sale_price_real_2022 / lag(median_sale_price_real_2022) - 1),
      NA_real_
    ),
    yoy_transactions_pct = if_else(
      is_full_year & lag(is_full_year, default = FALSE),
      100 * (n_transactions / lag(n_transactions) - 1),
      NA_real_
    )
  )

write_csv(citywide_monthly_yoy, citywide_monthly_yoy_output)
write_csv(citywide_annual_yoy, citywide_annual_yoy_output)

annual_plot <- citywide_annual_yoy %>%
  filter(is_full_year, !is.na(yoy_sale_price_real_pct)) %>%
  ggplot(aes(x = year, y = yoy_sale_price_real_pct)) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
  geom_line(color = "#1E6E61", linewidth = 0.9) +
  geom_point(color = "#1E6E61", size = 1.7) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "Citywide real home-sales growth",
    subtitle = "Annual YoY growth in median real sale price (2022 dollars)",
    x = NULL,
    y = "YoY growth"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = file.path(output_dir, "home_sales_citywide_yoy_annual_longrun.pdf"),
  plot = annual_plot,
  width = 9,
  height = 5.5
)

recent_monthly_plot <- citywide_monthly_yoy %>%
  filter(month_start >= as.Date("2021-01-01"), !is.na(yoy_sale_price_real_pct)) %>%
  ggplot(aes(x = month_start, y = yoy_sale_price_real_pct)) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
  geom_line(color = "#C8583D", linewidth = 0.9) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x)) +
  labs(
    title = "Citywide real home-sales growth, recent years",
    subtitle = "Monthly YoY growth in median real sale price (2022 dollars)",
    x = NULL,
    y = "YoY growth"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = file.path(output_dir, "home_sales_citywide_yoy_recent_monthly.pdf"),
  plot = recent_monthly_plot,
  width = 9,
  height = 5.5
)

transaction_plot <- citywide_monthly_yoy %>%
  filter(month_start >= as.Date("2021-01-01"), !is.na(yoy_transactions_pct)) %>%
  ggplot(aes(x = month_start, y = yoy_transactions_pct)) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
  geom_line(color = "#6D7A8A", linewidth = 0.9) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x)) +
  labs(
    title = "Citywide home-sales transaction growth, recent years",
    subtitle = "Monthly YoY growth in transaction counts",
    x = NULL,
    y = "YoY growth"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = file.path(output_dir, "home_sales_citywide_transaction_yoy_recent_monthly.pdf"),
  plot = transaction_plot,
  width = 9,
  height = 5.5
)

message("Saving home-sales maps...")
for (period_label in c(
  "2023_vs_2022_full_year",
  "2024_vs_2023_full_year",
  "2025_vs_2024_ytd"
)) {
  save_sales_map(period_label, "ward", period_summary, ward_panel_input, community_area_input, output_dir)
  save_sales_map(period_label, "community_area", period_summary, ward_panel_input, community_area_input, output_dir)
}

message("Saved citywide home-sales YoY CSVs.")
