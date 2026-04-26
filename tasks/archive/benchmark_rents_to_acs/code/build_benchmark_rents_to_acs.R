source("../../setup_environment/code/packages.R")
source("../../_lib/acs_benchmark_helpers.R")

library(data.table)

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/benchmark_rents_to_acs/code")
# private_yearly_input <- "../input/rent_geography_yearly_summary.csv"
# ward_controls_input <- "../input/ward_controls_2000_2023.csv"
# bg_controls_input <- "../input/block_group_controls_2000_2023.csv"
# bg_geometry_input <- "../input/block_group_geometry_2019.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# cpi_input <- "../input/fred_chi_cpi_all_items.csv"
# yearly_panel_output <- "../output/rent_acs_yearly_panel.csv"
# growth_output <- "../output/rent_acs_growth_comparison.csv"
# diagnostics_output <- "../output/rent_acs_diagnostics.csv"
# coverage_output <- "../output/rent_acs_coverage_summary.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    private_yearly_input,
    ward_controls_input,
    bg_controls_input,
    bg_geometry_input,
    community_area_input,
    cpi_input,
    yearly_panel_output,
    growth_output,
    diagnostics_output,
    coverage_output
  )
}

if (length(args) != 10) {
  stop(
    paste(
      "FATAL: Script requires 10 args:",
      "<private_yearly_input> <ward_controls_input> <bg_controls_input>",
      "<bg_geometry_input> <community_area_input> <cpi_input>",
      "<yearly_panel_output> <growth_output> <diagnostics_output> <coverage_output>"
    ),
    call. = FALSE
  )
}

private_yearly_input <- args[1]
ward_controls_input <- args[2]
bg_controls_input <- args[3]
bg_geometry_input <- args[4]
community_area_input <- args[5]
cpi_input <- args[6]
yearly_panel_output <- args[7]
growth_output <- args[8]
diagnostics_output <- args[9]
coverage_output <- args[10]

benchmark_start_year <- 2014L
benchmark_end_year <- 2019L

acs_bundle <- build_acs_benchmark_panel(
  ward_controls_input = ward_controls_input,
  bg_controls_input = bg_controls_input,
  bg_geometry_input = bg_geometry_input,
  community_area_input = community_area_input,
  cpi_input = cpi_input,
  start_year = benchmark_start_year,
  end_year = benchmark_end_year
)
acs_panel <- acs_bundle$panel[
  ,
  .(
    geography_level,
    geography_id,
    geography_name,
    year,
    value_real = acs_rent_real_2024
  )
]
acs_panel[, `:=`(
  series_id = "acs_rent",
  series_label = "ACS gross rent",
  base_year = fifelse(geography_level == "ward", 2015L, 2014L),
  n_obs = NA_integer_
)]

private_yearly <- fread(private_yearly_input)[
  source %in% c("renthub_clean", "renthub_raw") &
    geography_level %in% c("citywide", "ward", "community_area") &
    year >= benchmark_start_year &
    year <= benchmark_end_year,
  .(
    source,
    geography_level,
    geography_id = as.integer(geography_id),
    geography_name,
    year = as.integer(year),
    value_real = as.numeric(median_rent_real_2024),
    n_obs = as.integer(n_obs)
  )
]
private_yearly[, `:=`(
  series_id = source,
  series_label = fifelse(source == "renthub_clean", "RentHub clean", "RentHub raw"),
  base_year = fifelse(geography_level == "ward", 2015L, 2014L)
)]
private_yearly[, source := NULL]

yearly_panel <- rbindlist(
  list(
    acs_panel[, .(series_id, series_label, geography_level, geography_id, geography_name, year, value_real, n_obs, base_year)],
    private_yearly[, .(series_id, series_label, geography_level, geography_id, geography_name, year, value_real, n_obs, base_year)]
  ),
  use.names = TRUE,
  fill = TRUE
)
setorder(yearly_panel, geography_level, geography_id, series_id, year)
yearly_panel[, base_value := value_real[year == unique(base_year)], by = .(series_id, geography_level, geography_id)]
yearly_panel[, index_to_base_year := fifelse(
  is.finite(base_value) & base_value > 0,
  100 * value_real / base_value,
  NA_real_
)]
yearly_panel[, benchmark_window := fifelse(geography_level == "ward", "2015-2019", "2014-2019")]

acs_growth <- acs_panel[
  ,
  .(
    start_year = unique(base_year),
    end_year = benchmark_end_year,
    acs_start_real = value_real[year == unique(base_year)][1],
    acs_end_real = value_real[year == benchmark_end_year][1]
  ),
  by = .(geography_level, geography_id, geography_name)
]
acs_growth[, acs_growth_pct := fifelse(
  is.finite(acs_start_real) & acs_start_real > 0 & is.finite(acs_end_real),
  100 * (acs_end_real / acs_start_real - 1),
  NA_real_
)]

private_growth <- private_yearly[
  ,
  .(
    start_year = unique(base_year),
    end_year = benchmark_end_year,
    private_start_real = value_real[year == unique(base_year)][1],
    private_end_real = value_real[year == benchmark_end_year][1],
    private_n_start = n_obs[year == unique(base_year)][1],
    private_n_end = n_obs[year == benchmark_end_year][1]
  ),
  by = .(series_id, series_label, geography_level, geography_id, geography_name)
]
private_growth[, private_growth_pct := fifelse(
  is.finite(private_start_real) & private_start_real > 0 & is.finite(private_end_real),
  100 * (private_end_real / private_start_real - 1),
  NA_real_
)]

growth_comparison <- merge(
  private_growth,
  acs_growth,
  by = c("geography_level", "geography_id", "geography_name", "start_year", "end_year"),
  all = FALSE
)
growth_comparison <- growth_comparison[
  is.finite(private_start_real) &
    is.finite(private_end_real) &
    is.finite(acs_start_real) &
    is.finite(acs_end_real)
]
growth_comparison[, growth_gap_pct := private_growth_pct - acs_growth_pct]
growth_comparison <- growth_comparison[
  is.finite(private_growth_pct) &
    is.finite(acs_growth_pct) &
    is.finite(growth_gap_pct)
]
growth_comparison[, benchmark_window := fifelse(geography_level == "ward", "2015-2019", "2014-2019")]
setorder(growth_comparison, geography_level, series_id, geography_id)

slope_from_fit <- function(x, y) {
  if (length(x) < 2 || !is.finite(sd(x)) || sd(x) == 0) {
    return(NA_real_)
  }
  coef(lm(y ~ x))[2]
}

diagnostics <- growth_comparison[
  ,
  .(
    n_geographies = .N,
    correlation = if (.N >= 2) cor(private_growth_pct, acs_growth_pct) else NA_real_,
    slope = slope_from_fit(acs_growth_pct, private_growth_pct),
    mean_abs_gap_pct = mean(abs(growth_gap_pct)),
    median_abs_gap_pct = median(abs(growth_gap_pct)),
    mean_signed_gap_pct = mean(growth_gap_pct)
  ),
  by = .(geography_level, series_id, series_label, start_year, end_year, benchmark_window)
]

private_coverage <- private_yearly[
  ,
  .(n_geographies = uniqueN(geography_id[!is.na(geography_id)]) + as.integer(geography_level[1] == "citywide")),
  by = .(dataset = series_id, geography_level, year)
]
private_coverage[, expected_geographies := fifelse(
  geography_level == "citywide",
  1L,
  fifelse(geography_level == "ward", 50L, 77L)
)]
private_coverage[, share_covered := n_geographies / expected_geographies]

coverage_summary <- rbindlist(
  list(
    acs_bundle$coverage_summary,
    private_coverage
  ),
  use.names = TRUE,
  fill = TRUE
)[order(dataset, geography_level, year)]

fwrite(yearly_panel, yearly_panel_output)
fwrite(growth_comparison, growth_output)
fwrite(diagnostics, diagnostics_output)
fwrite(coverage_summary, coverage_output)
