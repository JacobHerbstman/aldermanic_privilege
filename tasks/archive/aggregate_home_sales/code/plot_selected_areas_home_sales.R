source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/aggregate_home_sales/code")
# monthly_input <- "../output/home_sales_geography_monthly_summary.csv"
# yearly_input <- "../output/home_sales_geography_yearly_summary.csv"
# period_input <- "../output/home_sales_map_period_summary.csv"
# yoy_output <- "../output/home_sales_selected_areas_recent_yoy.png"
# annual_output <- "../output/home_sales_selected_areas_annual_levels.png"
# summary_output <- "../output/home_sales_selected_areas_period_summary.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    monthly_input,
    yearly_input,
    period_input,
    yoy_output,
    annual_output,
    summary_output
  )
}

if (length(args) != 6) {
  stop(
    paste(
      "FATAL: Script requires 6 args:",
      "<monthly_input> <yearly_input> <period_input> <yoy_output> <annual_output> <summary_output>"
    ),
    call. = FALSE
  )
}

monthly_input <- args[1]
yearly_input <- args[2]
period_input <- args[3]
yoy_output <- args[4]
annual_output <- args[5]
summary_output <- args[6]

selected_areas <- c("LAKE VIEW", "LINCOLN PARK", "LOGAN SQUARE", "NEAR WEST SIDE")

area_labels <- c(
  "LAKE VIEW" = "Lake View",
  "LINCOLN PARK" = "Lincoln Park",
  "LOGAN SQUARE" = "Logan Square",
  "NEAR WEST SIDE" = "West Loop proxy\n(Near West Side)"
)

message("Loading selected-area home-sales summaries...")
monthly_summary <- read_csv(monthly_input, show_col_types = FALSE) %>%
  mutate(month_start = as.Date(month_start))

yearly_summary <- read_csv(yearly_input, show_col_types = FALSE)

period_summary <- read_csv(period_input, show_col_types = FALSE)

selected_monthly <- monthly_summary %>%
  filter(
    geography_level == "community_area",
    geography_name %in% selected_areas
  ) %>%
  arrange(geography_name, month_start) %>%
  group_by(geography_name) %>%
  mutate(
    lag_n_transactions = lag(n_transactions, 12),
    yoy_sale_price_real_pct = if_else(
      n_transactions >= 10 & lag_n_transactions >= 10,
      100 * (median_sale_price_real_2022 / lag(median_sale_price_real_2022, 12) - 1),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(area_label = recode(geography_name, !!!area_labels))

year_coverage <- monthly_summary %>%
  filter(
    geography_level == "community_area",
    geography_name %in% selected_areas
  ) %>%
  group_by(geography_name, year) %>%
  summarise(months_covered = n_distinct(month_start), .groups = "drop")

selected_yearly <- yearly_summary %>%
  filter(
    geography_level == "community_area",
    geography_name %in% selected_areas
  ) %>%
  left_join(year_coverage, by = c("geography_name", "year")) %>%
  mutate(
    is_full_year = months_covered == 12,
    area_label = recode(geography_name, !!!area_labels)
  )

selected_period_summary <- period_summary %>%
  filter(
    geography_level == "community_area",
    geography_name %in% selected_areas
  ) %>%
  mutate(area_label = recode(geography_name, !!!area_labels)) %>%
  arrange(area_label, period_label)

write_csv(selected_period_summary, summary_output)

recent_yoy <- selected_monthly %>%
  filter(month_start >= as.Date("2021-01-01"), !is.na(yoy_sale_price_real_pct)) %>%
  ggplot(aes(x = month_start, y = yoy_sale_price_real_pct)) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
  geom_line(color = "#C8583D", linewidth = 0.9) +
  facet_wrap(~area_label, scales = "free_y") +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x)) +
  labs(
    title = "Selected-area home-sales growth, recent YoY",
    subtitle = "Monthly YoY growth in median real sale price. West Loop is proxied by Near West Side.",
    x = NULL,
    y = "YoY growth"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = yoy_output,
  plot = recent_yoy,
  width = 11,
  height = 8
)

annual_levels <- selected_yearly %>%
  filter(year >= 2018) %>%
  ggplot(aes(x = year, y = median_sale_price_real_2022, group = 1)) +
  geom_line(color = "#1E6E61", linewidth = 0.85) +
  geom_point(aes(shape = is_full_year), color = "#1E6E61", size = 2) +
  facet_wrap(~area_label, scales = "free_y") +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), labels = c(`TRUE` = "Full year", `FALSE` = "Partial year")) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(breaks = seq(2018, 2025, 1)) +
  labs(
    title = "Selected-area home-sales levels",
    subtitle = "Annual median real sale price. 2025 is partial-year where hollow.",
    x = NULL,
    y = "Median real sale price (2022 dollars)",
    shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = annual_output,
  plot = annual_levels,
  width = 11,
  height = 8
)

message("Saved selected-area home-sales plots and summary.")
