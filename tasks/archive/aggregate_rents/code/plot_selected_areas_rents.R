source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/aggregate_rents/code")
# monthly_input <- "../output/rent_geography_monthly_summary.csv"
# period_input <- "../output/rent_map_period_summary.csv"
# level_output <- "../output/rent_selected_areas_recent_levels.png"
# yoy_output <- "../output/rent_selected_areas_recent_yoy.png"
# index_output <- "../output/rent_selected_areas_index_2021.png"
# summary_output <- "../output/rent_selected_areas_period_summary.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    monthly_input,
    period_input,
    level_output,
    yoy_output,
    index_output,
    summary_output
  )
}

if (length(args) != 6) {
  stop(
    paste(
      "FATAL: Script requires 6 args:",
      "<monthly_input> <period_input> <level_output> <yoy_output> <index_output> <summary_output>"
    ),
    call. = FALSE
  )
}

monthly_input <- args[1]
period_input <- args[2]
level_output <- args[3]
yoy_output <- args[4]
index_output <- args[5]
summary_output <- args[6]

selected_areas <- c("LAKE VIEW", "LINCOLN PARK", "LOGAN SQUARE", "NEAR WEST SIDE")

area_labels <- c(
  "LAKE VIEW" = "Lake View",
  "LINCOLN PARK" = "Lincoln Park",
  "LOGAN SQUARE" = "Logan Square",
  "NEAR WEST SIDE" = "West Loop proxy\n(Near West Side)"
)

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

message("Loading selected-area rent summaries...")
monthly_summary <- read_csv(monthly_input, show_col_types = FALSE) %>%
  mutate(month_start = as.Date(month_start))

period_summary <- read_csv(period_input, show_col_types = FALSE)

selected_monthly <- monthly_summary %>%
  filter(
    geography_level == "community_area",
    geography_name %in% selected_areas
  ) %>%
  arrange(source, geography_name, month_start) %>%
  group_by(source, geography_name) %>%
  mutate(
    lag_n_obs = lag(n_obs, 12),
    yoy_rent_growth_pct = if_else(
      n_obs >= 50 & lag_n_obs >= 50,
      100 * (median_rent_real_2024 / lag(median_rent_real_2024, 12) - 1),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    area_label = recode(geography_name, !!!area_labels),
    source_label = recode(source, !!!source_labels)
  )

selected_period_summary <- period_summary %>%
  filter(
    geography_level == "community_area",
    geography_name %in% selected_areas
  ) %>%
  mutate(
    area_label = recode(geography_name, !!!area_labels),
    source_label = recode(source, !!!source_labels)
  ) %>%
  arrange(area_label, source, period_label)

write_csv(selected_period_summary, summary_output)

selected_index <- selected_monthly %>%
  filter(month_start >= as.Date("2021-01-01"), n_obs >= 50) %>%
  group_by(source, geography_name) %>%
  mutate(
    baseline_2021 = median(median_rent_real_2024[lubridate::year(month_start) == 2021], na.rm = TRUE),
    index_2021 = 100 * median_rent_real_2024 / baseline_2021
  ) %>%
  ungroup()

recent_levels <- selected_monthly %>%
  filter(month_start >= as.Date("2021-01-01"), n_obs >= 50) %>%
  ggplot(aes(x = month_start, y = median_rent_real_2024, color = source)) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~area_label, scales = "free_y") +
  scale_color_manual(values = source_colors, labels = source_labels) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Selected-area asking rents, recent levels",
    subtitle = "Median real asking rent by month. West Loop is proxied by Near West Side.",
    x = NULL,
    y = "Median real rent (2024 dollars)",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = level_output,
  plot = recent_levels,
  width = 11,
  height = 8
)

recent_yoy <- selected_monthly %>%
  filter(month_start >= as.Date("2022-01-01"), !is.na(yoy_rent_growth_pct)) %>%
  ggplot(aes(x = month_start, y = yoy_rent_growth_pct, color = source)) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~area_label, scales = "free_y") +
  scale_color_manual(values = source_colors, labels = source_labels) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x)) +
  labs(
    title = "Selected-area asking rents, recent YoY growth",
    subtitle = "Monthly YoY growth in median real asking rent. West Loop is proxied by Near West Side.",
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
  filename = yoy_output,
  plot = recent_yoy,
  width = 11,
  height = 8
)

index_plot <- selected_index %>%
  ggplot(aes(x = month_start, y = index_2021, color = source)) +
  geom_hline(yintercept = 100, color = "grey75", linewidth = 0.3) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~area_label, scales = "free_y") +
  scale_color_manual(values = source_colors, labels = source_labels) +
  labs(
    title = "Selected-area asking rents since 2021",
    subtitle = "Index with each source-area normalized to its 2021 median. West Loop is proxied by Near West Side.",
    x = NULL,
    y = "2021 = 100",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = index_output,
  plot = index_plot,
  width = 11,
  height = 8
)

message("Saved selected-area rent plots and summary.")
