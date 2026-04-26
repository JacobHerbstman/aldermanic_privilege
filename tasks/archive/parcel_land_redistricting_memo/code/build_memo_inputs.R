# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/parcel_land_redistricting_memo/code")
# panel_path <- "../input/parcel_land_redistricting_panel.parquet"
# support_path <- "../input/parcel_land_redistricting_support.csv"
# citywide_path <- "../input/parcel_land_citywide_taxyear_summary.csv"
# geography_path <- "../input/parcel_land_geography_taxyear_summary.csv"
# output_path <- "../output/memo_stats.tex"

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
})

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) {
  cli_args <- c(panel_path, support_path, citywide_path, geography_path, output_path)
}

stopifnot(length(cli_args) == 5)

panel_path <- cli_args[1]
support_path <- cli_args[2]
citywide_path <- cli_args[3]
geography_path <- cli_args[4]
output_path <- cli_args[5]

join_with_and <- function(values) {
  values <- as.character(values)

  if (length(values) == 0) {
    return("")
  }

  if (length(values) == 1) {
    return(values)
  }

  if (length(values) == 2) {
    return(paste(values, collapse = " and "))
  }

  paste0(
    paste(values[-length(values)], collapse = ", "),
    ", and ",
    values[length(values)]
  )
}

collapse_year_ranges <- function(values) {
  if (length(values) == 0) {
    return(character())
  }

  values <- sort(unique(as.integer(values)))
  starts <- c(values[1], values[which(diff(values) > 1) + 1])
  ends <- c(values[which(diff(values) > 1)], values[length(values)])

  vapply(
    seq_along(starts),
    function(i) {
      if (starts[i] == ends[i]) {
        as.character(starts[i])
      } else {
        paste0(starts[i], "--", ends[i])
      }
    },
    character(1)
  )
}

format_int <- function(value) {
  format(
    round(value),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

panel <- read_parquet(panel_path) %>%
  as_tibble()

pin_summary <- panel %>%
  arrange(pin10, tax_year) %>%
  group_by(pin10) %>%
  summarise(
    in_500ft = dplyr::first(in_500ft),
    in_1000ft = dplyr::first(in_1000ft),
    switched_2015 = dplyr::first(switched_2015),
    valid_control_2015 = dplyr::first(valid_control_2015),
    ward_2014 = dplyr::first(ward_2014),
    ward_pair_id = dplyr::first(ward_pair_id),
    segment_id = dplyr::first(segment_id),
    .groups = "drop"
  )

support <- read_csv(support_path, show_col_types = FALSE)

citywide <- read_csv(citywide_path, show_col_types = FALSE) %>%
  filter(scope == "1000ft") %>%
  arrange(tax_year)

geography <- read_csv(geography_path, show_col_types = FALSE)

interior_year_counts <- citywide %>%
  filter(tax_year > min(tax_year), tax_year < max(tax_year))

interior_median_n <- median(interior_year_counts$n_pin10)

thin_years <- citywide %>%
  filter(
    tax_year > min(tax_year),
    tax_year < max(tax_year),
    n_pin10 < 0.5 * interior_median_n
  ) %>%
  pull(tax_year)

fringe_years <- citywide %>%
  filter(tax_year %in% c(min(tax_year), max(tax_year))) %>%
  pull(tax_year)

pre_redistricting <- citywide %>%
  filter(tax_year == 2014)

post_redistricting <- citywide %>%
  filter(tax_year == 2016)

balanced_any <- support %>%
  transmute(
    balanced_any =
      balanced_raw_2010_2013_2016_2019 |
      balanced_raw_2011_2014_2017_2020 |
      balanced_raw_2012_2015_2018
  )

stats_lines <- c(
  paste0("\\newcommand{\\PanelRows}{", format_int(nrow(panel)), "}"),
  paste0("\\newcommand{\\PanelPins}{", format_int(nrow(pin_summary)), "}"),
  paste0("\\newcommand{\\SubsetRows}{", format_int(sum(panel$in_500ft)), "}"),
  paste0("\\newcommand{\\SubsetPins}{", format_int(sum(pin_summary$in_500ft)), "}"),
  paste0("\\newcommand{\\SwitcherPins}{", format_int(sum(pin_summary$switched_2015)), "}"),
  paste0("\\newcommand{\\ValidControlPins}{", format_int(sum(pin_summary$valid_control_2015)), "}"),
  paste0("\\newcommand{\\WardCount}{", format_int(n_distinct(pin_summary$ward_2014)), "}"),
  paste0(
    "\\newcommand{\\CommunityAreaCount}{",
    format_int(
      n_distinct(
        geography$geography_id[
          geography$scope == "1000ft" &
            geography$geography_level == "community_area"
        ]
      )
    ),
    "}"
  ),
  paste0("\\newcommand{\\WardPairCount}{", format_int(n_distinct(pin_summary$ward_pair_id)), "}"),
  paste0("\\newcommand{\\SegmentCount}{", format_int(n_distinct(pin_summary$segment_id)), "}"),
  paste0("\\newcommand{\\FirstYear}{", min(citywide$tax_year), "}"),
  paste0("\\newcommand{\\LastYear}{", max(citywide$tax_year), "}"),
  paste0("\\newcommand{\\MedianObservedYears}{", format_int(median(support$n_tax_years_observed)), "}"),
  paste0("\\newcommand{\\ObservedYearsPtf}{", format_int(quantile(support$n_tax_years_observed, 0.25)), "}"),
  paste0("\\newcommand{\\ObservedYearsPsf}{", format_int(quantile(support$n_tax_years_observed, 0.75)), "}"),
  paste0("\\newcommand{\\BalancedAnyPins}{", format_int(sum(balanced_any$balanced_any)), "}"),
  paste0("\\newcommand{\\BalancedPatternA}{", format_int(sum(support$balanced_raw_2010_2013_2016_2019)), "}"),
  paste0("\\newcommand{\\BalancedPatternB}{", format_int(sum(support$balanced_raw_2011_2014_2017_2020)), "}"),
  paste0("\\newcommand{\\BalancedPatternC}{", format_int(sum(support$balanced_raw_2012_2015_2018)), "}"),
  paste0("\\newcommand{\\PreRedistrictYear}{2014}"),
  paste0("\\newcommand{\\PostRedistrictYear}{2016}"),
  paste0("\\newcommand{\\PreRedistrictPins}{", format_int(pre_redistricting$n_pin10), "}"),
  paste0("\\newcommand{\\PostRedistrictPins}{", format_int(post_redistricting$n_pin10), "}"),
  paste0("\\newcommand{\\PreRedistrictMedianLand}{", format_int(pre_redistricting$median_land_sum), "}"),
  paste0("\\newcommand{\\PostRedistrictMedianLand}{", format_int(post_redistricting$median_land_sum), "}"),
  paste0(
    "\\newcommand{\\ThinYearList}{",
    join_with_and(collapse_year_ranges(thin_years)),
    "}"
  ),
  paste0(
    "\\newcommand{\\FringeYearList}{",
    join_with_and(collapse_year_ranges(fringe_years)),
    "}"
  )
)

writeLines(stats_lines, output_path)
