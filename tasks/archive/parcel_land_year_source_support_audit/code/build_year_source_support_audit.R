source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/parcel_land_year_source_support_audit/code")
# in_panel <- "../input/parcel_land_redistricting_panel.parquet"
# in_prepost <- "../input/parcel_land_capitalization_prepost_2014_2016.parquet"
# out_panel_support <- "../output/parcel_land_year_source_panel_support.csv"
# out_prepost_support <- "../output/parcel_land_year_source_prepost_support.csv"
# out_prepost_pairs <- "../output/parcel_land_year_source_prepost_source_pairs.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    in_panel,
    in_prepost,
    out_panel_support,
    out_prepost_support,
    out_prepost_pairs
  )
}

if (length(cli_args) != 5) {
  stop(
    paste(
      "FATAL: Script requires 5 args:",
      "<in_panel_parquet> <in_prepost_parquet> <out_panel_support_csv>",
      "<out_prepost_support_csv> <out_prepost_pairs_csv>"
    ),
    call. = FALSE
  )
}

in_panel <- cli_args[1]
in_prepost <- cli_args[2]
out_panel_support <- cli_args[3]
out_prepost_support <- cli_args[4]
out_prepost_pairs <- cli_args[5]

stopifnot(file.exists(in_panel))
stopifnot(file.exists(in_prepost))

admin_sources <- c("taxyear", "assr_year")
plot_years <- c(2012L, 2014L, 2016L)

panel <- arrow::read_parquet(in_panel) %>%
  mutate(
    pin10 = as.character(pin10),
    tax_year = as.integer(tax_year),
    in_500ft = as.logical(in_500ft),
    in_1000ft = as.logical(in_1000ft),
    switched_2015 = as.logical(switched_2015),
    valid_control_2015 = as.logical(valid_control_2015),
    baseline_empty_last_pre_2012_2014 = as.logical(baseline_empty_last_pre_2012_2014),
    land_only = as.logical(land_only)
  )

if (nrow(panel) == 0) {
  stop("Input panel parquet is empty.", call. = FALSE)
}

if (anyDuplicated(panel[c("pin10", "tax_year")]) > 0) {
  stop("Input panel parquet has duplicate pin10 x tax_year rows.", call. = FALSE)
}

panel_base <- bind_rows(
  panel %>%
    mutate(year_scope = "admin_plus_fallback"),
  panel %>%
    filter(tax_year_source %in% admin_sources) %>%
    mutate(year_scope = "admin_only")
) %>%
  filter(tax_year %in% plot_years) %>%
  mutate(
    treatment_sign = case_when(
      strictness_change < 0 ~ "to_lenient",
      strictness_change > 0 ~ "to_stricter",
      TRUE ~ "no_change"
    )
  )

panel_views <- bind_rows(
  panel_base %>%
    filter(land_only %in% TRUE) %>%
    mutate(sample_view = "current_land_only"),
  panel_base %>%
    filter(baseline_empty_last_pre_2012_2014 %in% TRUE) %>%
    mutate(sample_view = "baseline_empty_last_pre_2012_2014")
) %>%
  select(-land_only, -baseline_empty_last_pre_2012_2014)

panel_support <- bind_rows(
  panel_views %>%
    filter(in_1000ft %in% TRUE) %>%
    mutate(bandwidth = "1000ft"),
  panel_views %>%
    filter(in_500ft %in% TRUE) %>%
    mutate(bandwidth = "500ft")
) %>%
  group_by(year_scope, sample_view, bandwidth, tax_year, treatment_sign) %>%
  summarise(
    n_rows = n(),
    n_pin10 = n_distinct(pin10),
    n_switchers = n_distinct(pin10[switched_2015 %in% TRUE]),
    n_valid_controls = n_distinct(pin10[valid_control_2015 %in% TRUE]),
    n_positive_land_sum = n_distinct(pin10[land_sum > 0]),
    n_positive_land_psf = n_distinct(pin10[land_psf > 0]),
    .groups = "drop"
  ) %>%
  arrange(year_scope, sample_view, bandwidth, tax_year, treatment_sign)

prepost <- arrow::read_parquet(in_prepost) %>%
  mutate(
    pin10 = as.character(pin10),
    in_500ft = as.logical(in_500ft),
    in_1000ft = as.logical(in_1000ft),
    switched_2015 = as.logical(switched_2015),
    valid_control_2015 = as.logical(valid_control_2015)
  )

if (nrow(prepost) == 0) {
  stop("Input prepost parquet is empty.", call. = FALSE)
}

if (anyDuplicated(prepost["pin10"]) > 0) {
  stop("Input prepost parquet has duplicate pin10 rows.", call. = FALSE)
}

prepost_views <- bind_rows(
  prepost %>%
    filter(in_1000ft %in% TRUE) %>%
    mutate(bandwidth = "1000ft"),
  prepost %>%
    filter(in_500ft %in% TRUE) %>%
    mutate(bandwidth = "500ft")
)

prepost_pairs <- prepost_views %>%
  group_by(bandwidth, tax_year_source_2014, tax_year_source_2016, treatment_sign) %>%
  summarise(
    n_pin10 = n(),
    n_switchers = sum(switched_2015 %in% TRUE),
    n_valid_controls = sum(valid_control_2015 %in% TRUE),
    .groups = "drop"
  ) %>%
  arrange(bandwidth, desc(n_pin10), tax_year_source_2014, tax_year_source_2016, treatment_sign)

prepost_base <- bind_rows(
  prepost %>%
    mutate(year_scope = "admin_plus_fallback"),
  prepost %>%
    filter(
      tax_year_source_2014 %in% admin_sources,
      tax_year_source_2016 %in% admin_sources
    ) %>%
    mutate(year_scope = "admin_only")
)

prepost_support <- bind_rows(
  prepost_base %>%
    filter(in_1000ft %in% TRUE) %>%
    mutate(bandwidth = "1000ft"),
  prepost_base %>%
    filter(in_500ft %in% TRUE) %>%
    mutate(bandwidth = "500ft")
) %>%
  group_by(year_scope, bandwidth, treatment_sign) %>%
  summarise(
    n_pin10 = n(),
    n_switchers = sum(switched_2015 %in% TRUE),
    n_valid_controls = sum(valid_control_2015 %in% TRUE),
    n_positive_land_psf_both_years = sum(land_psf_2014 > 0 & land_psf_2016 > 0, na.rm = TRUE),
    n_positive_land_sum_both_years = sum(land_sum_2014 > 0 & land_sum_2016 > 0, na.rm = TRUE),
    share_missing_delta_log_land_psf = mean(is.na(delta_log_land_psf_2016_minus_2014)),
    share_missing_delta_log_land_sum = mean(is.na(delta_log_land_sum_2016_minus_2014)),
    share_building_positive_2016 = mean(building_positive_2016, na.rm = TRUE),
    share_gained_positive_building_2014_2016 = mean(gained_positive_building_2014_2016, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_scope, bandwidth, treatment_sign)

readr::write_csv(panel_support, out_panel_support)
readr::write_csv(prepost_support, out_prepost_support)
readr::write_csv(prepost_pairs, out_prepost_pairs)

message(sprintf("Saved %s", out_panel_support))
message(sprintf("Saved %s", out_prepost_support))
message(sprintf("Saved %s", out_prepost_pairs))
