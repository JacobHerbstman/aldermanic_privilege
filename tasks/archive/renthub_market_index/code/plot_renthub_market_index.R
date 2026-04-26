source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")

library(data.table)
library(dplyr)
library(ggplot2)
library(sf)

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/renthub_market_index/code")
# benchmark_monthly_input <- "../output/renthub_market_benchmark_comparison.csv"
# neighborhood_period_input <- "../output/renthub_market_neighborhood_periods.csv"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# smooth_plot_output <- "../output/renthub_market_citywide_smooth_index.pdf"
# asking_plot_output <- "../output/renthub_market_citywide_asking_index.pdf"
# yoy_plot_output <- "../output/renthub_market_citywide_yoy_recent.pdf"
# map_ward_2023_output <- "../output/renthub_market_map_ward_2023_vs_2022.pdf"
# map_ca_2023_output <- "../output/renthub_market_map_community_area_2023_vs_2022.pdf"
# map_ward_2024_output <- "../output/renthub_market_map_ward_2024_vs_2023.pdf"
# map_ca_2024_output <- "../output/renthub_market_map_community_area_2024_vs_2023.pdf"
# map_ward_2025_output <- "../output/renthub_market_map_ward_2025_vs_2024_ytd.pdf"
# map_ca_2025_output <- "../output/renthub_market_map_community_area_2025_vs_2024_ytd.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    benchmark_monthly_input,
    neighborhood_period_input,
    ward_panel_input,
    community_area_input,
    smooth_plot_output,
    asking_plot_output,
    yoy_plot_output,
    map_ward_2023_output,
    map_ca_2023_output,
    map_ward_2024_output,
    map_ca_2024_output,
    map_ward_2025_output,
    map_ca_2025_output
  )
}

if (length(args) != 13) {
  stop(
    paste(
      "FATAL: Script requires 13 args:",
      "<benchmark_monthly_input> <neighborhood_period_input> <ward_panel_input>",
      "<community_area_input> <smooth_plot_output> <asking_plot_output>",
      "<yoy_plot_output> <map_ward_2023_output> <map_ca_2023_output>",
      "<map_ward_2024_output> <map_ca_2024_output> <map_ward_2025_output>",
      "<map_ca_2025_output>"
    ),
    call. = FALSE
  )
}

benchmark_monthly_input <- args[1]
neighborhood_period_input <- args[2]
ward_panel_input <- args[3]
community_area_input <- args[4]
smooth_plot_output <- args[5]
asking_plot_output <- args[6]
yoy_plot_output <- args[7]
map_ward_2023_output <- args[8]
map_ca_2023_output <- args[9]
map_ward_2024_output <- args[10]
map_ca_2024_output <- args[11]
map_ward_2025_output <- args[12]
map_ca_2025_output <- args[13]

plot_theme <- theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )

benchmark_monthly <- fread(benchmark_monthly_input)
benchmark_monthly[, month_start := as.Date(month_start)]
period_rows <- fread(neighborhood_period_input)
base_month <- as.Date("2019-01-01")

smooth_long <- rbindlist(
  list(
    benchmark_monthly[
      series_id %in% c("repeat_key", "repeat_unit_id", "hedonic_index"),
      .(
        month_start,
        series_label,
        index_value = index_2019m1,
        yoy_pct = yoy_pct
      )
    ],
    benchmark_monthly[
      ,
      .(
        month_start,
        series_label = "Zillow city ZORI",
        index_value = 100 * zillow_city_zori / zillow_city_zori[month_start == base_month][1],
        yoy_pct = zillow_city_zori_yoy_pct
      )
    ],
    benchmark_monthly[
      ,
      .(
        month_start,
        series_label = "Zillow metro ZORI",
        index_value = 100 * zillow_metro_zori / zillow_metro_zori[month_start == base_month][1],
        yoy_pct = zillow_metro_zori_yoy_pct
      )
    ],
    benchmark_monthly[
      ,
      .(
        month_start,
        series_label = "FRED Chicago rent CPI",
        index_value = 100 * fred_chi_rent_cpi / fred_chi_rent_cpi[month_start == base_month][1],
        yoy_pct = fred_chi_rent_cpi_yoy_pct
      )
    ]
  ),
  use.names = TRUE,
  fill = TRUE
)

asking_long <- rbindlist(
  list(
    benchmark_monthly[
      series_id %in% c("cycle_first", "active_month", "cycle_first_stable", "active_month_stable"),
      .(
        month_start,
        series_label,
        index_value = index_2019m1,
        yoy_pct = yoy_pct
      )
    ],
    benchmark_monthly[
      ,
      .(
        month_start,
        series_label = "Zillow city ZORI",
        index_value = 100 * zillow_city_zori / zillow_city_zori[month_start == base_month][1],
        yoy_pct = zillow_city_zori_yoy_pct
      )
    ]
  ),
  use.names = TRUE,
  fill = TRUE
)

if ("benchmark_label" %in% names(benchmark_monthly) && "benchmark_value" %in% names(benchmark_monthly)) {
  news_long <- benchmark_monthly[
    is.finite(benchmark_value),
    .(
      month_start,
      series_label = benchmark_label,
      index_value = 100 * benchmark_value / benchmark_value[month_start == base_month][1],
      yoy_pct = benchmark_yoy_pct
    )
  ]
  asking_long <- rbindlist(list(asking_long, news_long), use.names = TRUE, fill = TRUE)
}

recent_cutoff <- max(benchmark_monthly$month_start, na.rm = TRUE) - 365 * 3
yoy_long <- rbindlist(
  list(
    smooth_long[, .(month_start, series_label, yoy_pct, family = "Smooth index")],
    asking_long[, .(month_start, series_label, yoy_pct, family = "Asking-rent series")]
  ),
  use.names = TRUE,
  fill = TRUE
)[month_start >= recent_cutoff]

ggsave(
  smooth_plot_output,
  ggplot(smooth_long[is.finite(index_value)], aes(month_start, index_value, color = series_label)) +
    geom_line(linewidth = 0.8) +
    labs(
      title = "Citywide Smooth Rent Indexes (2019 = 100)",
      x = NULL,
      y = "Index"
    ) +
    plot_theme,
  width = 10,
  height = 6
)

ggsave(
  asking_plot_output,
  ggplot(asking_long[is.finite(index_value)], aes(month_start, index_value, color = series_label)) +
    geom_line(linewidth = 0.8) +
    labs(
      title = "Citywide Asking-Rent Series (2019 = 100)",
      x = NULL,
      y = "Index"
    ) +
    plot_theme,
  width = 10,
  height = 6
)

ggsave(
  yoy_plot_output,
  ggplot(yoy_long[is.finite(yoy_pct)], aes(month_start, yoy_pct, color = series_label)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_line(linewidth = 0.8) +
    facet_wrap(~family, ncol = 1, scales = "free_y") +
    labs(
      title = "Recent Citywide YoY Rent Growth",
      x = NULL,
      y = "YoY growth (%)"
    ) +
    plot_theme,
  width = 10,
  height = 7
)

make_map <- function(map_geography_level, map_period_label, output_file) {
  period_dt <- period_rows[
    period_label == map_period_label & geography_level == map_geography_level
  ]

  map_year <- unique(period_dt$map_year)
  map_sf <- load_map_geography(
    geography_level = map_geography_level,
    ward_panel_input = ward_panel_input,
    community_area_input = community_area_input,
    map_year = map_year
  ) %>%
    st_as_sf()

  data_variants <- rbindlist(
    list(
      period_dt[, .(geography_id, geography_name, growth_pct = raw_growth_nominal_pct, variant = "Raw")],
      period_dt[, .(geography_id, geography_name, growth_pct = display_growth_nominal_pct, variant = "Display")],
      period_dt[, .(geography_id, geography_name, growth_pct = shrunk_growth_nominal_pct, variant = "Shrunk")]
    ),
    use.names = TRUE,
    fill = TRUE
  )

  map_plot <- merge(
    map_sf,
    data_variants,
    by = "geography_id",
    all.x = TRUE,
    sort = FALSE
  ) %>%
    st_as_sf()

  map_limit <- max(abs(map_plot$growth_pct), na.rm = TRUE)
  if (!is.finite(map_limit) || map_limit <= 0) {
    map_limit <- 1
  }
  map_limit <- ceiling(map_limit / 5) * 5

  title_text <- switch(
    map_period_label,
    `2023_vs_2022_full_year` = "2023 vs 2022 Full-Year Asking-Rent Growth",
    `2024_vs_2023_full_year` = "2024 vs 2023 Full-Year Asking-Rent Growth",
    `2025_vs_2024_ytd` = "2025 vs 2024 YTD Asking-Rent Growth",
    map_period_label
  )

  ggsave(
    output_file,
    ggplot(map_plot) +
      geom_sf(aes(fill = growth_pct), color = "white", linewidth = 0.15) +
      scale_fill_gradient2(
        low = "#b83a2f",
        mid = "#f7f3eb",
        high = "#2a7f62",
        midpoint = 0,
        limits = c(-map_limit, map_limit),
        na.value = "grey90"
      ) +
      facet_wrap(~variant, nrow = 1) +
      labs(
        title = paste(title_text, ifelse(map_geography_level == "ward", "by Ward", "by Community Area")),
        fill = "Growth (%)"
      ) +
      theme_void(base_size = 11) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        plot.title.position = "plot"
      ),
    width = 12,
    height = 5
  )
}

make_map("ward", "2023_vs_2022_full_year", map_ward_2023_output)
make_map("community_area", "2023_vs_2022_full_year", map_ca_2023_output)
make_map("ward", "2024_vs_2023_full_year", map_ward_2024_output)
make_map("community_area", "2024_vs_2023_full_year", map_ca_2024_output)
make_map("ward", "2025_vs_2024_ytd", map_ward_2025_output)
make_map("community_area", "2025_vs_2024_ytd", map_ca_2025_output)
