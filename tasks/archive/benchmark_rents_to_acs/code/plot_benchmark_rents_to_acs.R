source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")

library(data.table)
library(scales)

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/benchmark_rents_to_acs/code")
# yearly_panel_input <- "../output/rent_acs_yearly_panel.csv"
# growth_input <- "../output/rent_acs_growth_comparison.csv"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# citywide_plot_output <- "../output/rent_acs_citywide_index_2014_2019.pdf"
# ward_scatter_output <- "../output/rent_acs_vs_renthub_clean_scatter_ward_2015_2019.pdf"
# community_scatter_output <- "../output/rent_acs_vs_renthub_clean_scatter_community_area_2014_2019.pdf"
# ward_map_output <- "../output/rent_acs_vs_renthub_clean_map_ward_2015_2019.pdf"
# community_map_output <- "../output/rent_acs_vs_renthub_clean_map_community_area_2014_2019.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    yearly_panel_input,
    growth_input,
    ward_panel_input,
    community_area_input,
    citywide_plot_output,
    ward_scatter_output,
    community_scatter_output,
    ward_map_output,
    community_map_output
  )
}

if (length(args) != 9) {
  stop(
    paste(
      "FATAL: Script requires 9 args:",
      "<yearly_panel_input> <growth_input> <ward_panel_input> <community_area_input>",
      "<citywide_plot_output> <ward_scatter_output> <community_scatter_output>",
      "<ward_map_output> <community_map_output>"
    ),
    call. = FALSE
  )
}

yearly_panel_input <- args[1]
growth_input <- args[2]
ward_panel_input <- args[3]
community_area_input <- args[4]
citywide_plot_output <- args[5]
ward_scatter_output <- args[6]
community_scatter_output <- args[7]
ward_map_output <- args[8]
community_map_output <- args[9]

yearly_panel <- fread(yearly_panel_input)
growth_comparison <- fread(growth_input)

series_colors <- c(
  "ACS gross rent" = "#1b1b1b",
  "RentHub clean" = "#1f78b4",
  "RentHub raw" = "#33a02c"
)

citywide_plot <- ggplot(
  yearly_panel[geography_level == "citywide"],
  aes(x = year, y = index_to_base_year, color = series_label)
) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = series_colors) +
  scale_x_continuous(breaks = 2014:2019) +
  labs(
    x = NULL,
    y = "Index (2014 = 100)",
    color = NULL,
    title = "Chicago Real Rent Growth, ACS vs RentHub",
    subtitle = "Citywide annual series, 2014-2019"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(citywide_plot_output, citywide_plot, width = 8, height = 5)

save_scatter <- function(target_geography_level, output_path, title_text) {
  scatter_dt <- growth_comparison[
    geography_level == target_geography_level &
      series_id == "renthub_clean"
  ]

  scatter_plot <- ggplot(
    scatter_dt,
    aes(x = acs_growth_pct, y = private_growth_pct)
  ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
    geom_point(color = "#1f78b4", alpha = 0.8, size = 2.2) +
    labs(
      x = "ACS growth (%)",
      y = "RentHub clean growth (%)",
      title = title_text
    ) +
    theme_minimal(base_size = 12)

  ggsave(output_path, scatter_plot, width = 6.5, height = 5.25)
}

save_scatter("ward", ward_scatter_output, "Ward Rent Growth: ACS vs RentHub Clean (2015-2019)")
save_scatter("community_area", community_scatter_output, "Community-Area Rent Growth: ACS vs RentHub Clean (2014-2019)")

save_map <- function(target_geography_level, map_year, output_path, title_text) {
  base_map <- load_map_geography(target_geography_level, ward_panel_input, community_area_input, map_year) %>%
    st_make_valid()

  map_dt <- growth_comparison[
    geography_level == target_geography_level &
      series_id == "renthub_clean",
    .(
      geography_id = as.integer(geography_id),
      geography_name,
      acs_growth_pct,
      private_growth_pct
    )
  ]
  map_long <- rbindlist(
    list(
      map_dt[, .(geography_id, geography_name, series_label = "ACS gross rent", growth_pct = acs_growth_pct)],
      map_dt[, .(geography_id, geography_name, series_label = "RentHub clean", growth_pct = private_growth_pct)]
    )
  )

  map_plot <- base_map %>%
    left_join(map_long, by = c("geography_id", "geography_name")) %>%
    ggplot() +
    geom_sf(aes(fill = growth_pct), color = "white", linewidth = 0.15) +
    facet_wrap(~ series_label) +
    scale_fill_gradient2(
      low = "#b2182b",
      mid = "white",
      high = "#2166ac",
      midpoint = 0,
      labels = label_number(suffix = "%")
    ) +
    labs(
      title = title_text,
      fill = "Growth"
    ) +
    theme_void(base_size = 12) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )

  ggsave(output_path, map_plot, width = 9, height = 5.75)
}

save_map("ward", 2015, ward_map_output, "Ward Rent Growth, ACS vs RentHub Clean (2015-2019)")
save_map("community_area", 2019, community_map_output, "Community-Area Rent Growth, ACS vs RentHub Clean (2014-2019)")
