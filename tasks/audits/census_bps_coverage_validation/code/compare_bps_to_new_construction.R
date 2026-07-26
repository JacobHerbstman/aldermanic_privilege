# Compare the reconstructed new-construction ledger against Census Building
# Permits Survey authorizations for the City of Chicago.
#
# BPS counts units AUTHORIZED by permit; the ledger counts units COMPLETED and
# recorded by the Assessor. Authorization precedes completion, and some
# authorized units are never built, so the ledger should account for somewhat
# less than the BPS total. A large shortfall would indicate missing completions.
#
# setwd("tasks/audits/census_bps_coverage_validation/code")
# first_completion_year <- "2006"
# last_completion_year <- "2022"
# max_lag <- "3"

source("../../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop("Usage: compare_bps_to_new_construction.R <first_completion_year> <last_completion_year> <max_lag>")
}
first_completion_year <- as.integer(args[1])
last_completion_year <- as.integer(args[2])
max_lag <- as.integer(args[3])

bps <- readr::read_csv(
  "../input/census_bps_place_midwest.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::filter(state_code == "17", county_code == "031", six_digit_id == "147700") |>
  dplyr::transmute(
    year = as.integer(survey_year),
    months_reported = as.integer(months_reported),
    bps_units_1 = as.numeric(units1_units),
    bps_units_2 = as.numeric(units2_units),
    bps_units_34 = as.numeric(units34_units),
    bps_units_5plus = as.numeric(units5plus_units),
    bps_bldgs_1 = as.numeric(units1_bldgs),
    bps_bldgs_2 = as.numeric(units2_bldgs),
    bps_bldgs_34 = as.numeric(units34_bldgs),
    bps_bldgs_5plus = as.numeric(units5plus_bldgs)
  ) |>
  dplyr::mutate(
    bps_units_total = bps_units_1 + bps_units_2 + bps_units_34 + bps_units_5plus,
    bps_bldgs_total = bps_bldgs_1 + bps_bldgs_2 + bps_bldgs_34 + bps_bldgs_5plus
  ) |>
  dplyr::arrange(year)

if (anyDuplicated(bps$year)) {
  stop("Building Permits Survey input is not uniquely keyed by year for Chicago.")
}
if (nrow(bps) == 0) {
  stop("No City of Chicago rows found in the Building Permits Survey input.")
}

ledger <- readr::read_csv(
  "../input/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_select = c(project_id, construction_year, dwelling_units, source_family, project_kind),
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_year = readr::col_double(),
    dwelling_units = readr::col_double(),
    source_family = readr::col_character(),
    project_kind = readr::col_character()
  )
) |>
  dplyr::filter(
    is.finite(construction_year),
    construction_year >= first_completion_year,
    construction_year <= last_completion_year,
    is.finite(dwelling_units),
    dwelling_units > 0
  ) |>
  dplyr::mutate(
    year = as.integer(construction_year),
    size_bucket = dplyr::case_when(
      dwelling_units == 1 ~ "1",
      dwelling_units == 2 ~ "2",
      dwelling_units <= 4 ~ "34",
      TRUE ~ "5plus"
    )
  )

ledger_annual <- ledger |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    ledger_projects = dplyr::n(),
    ledger_units_total = sum(dwelling_units),
    ledger_units_1 = sum(dwelling_units[size_bucket == "1"]),
    ledger_units_2 = sum(dwelling_units[size_bucket == "2"]),
    ledger_units_34 = sum(dwelling_units[size_bucket == "34"]),
    ledger_units_5plus = sum(dwelling_units[size_bucket == "5plus"]),
    .groups = "drop"
  )

annual <- ledger_annual |>
  dplyr::left_join(
    bps |> dplyr::select(year, months_reported, dplyr::starts_with("bps_")),
    by = "year"
  ) |>
  dplyr::mutate(
    contemporaneous_ratio = ledger_units_total / bps_units_total
  ) |>
  dplyr::arrange(year)

readr::write_csv(annual, "../output/bps_ledger_annual_comparison.csv")

# ---- cumulative coverage at each authorization-to-completion lag -------------
ledger_total <- sum(ledger$dwelling_units)
cumulative <- purrr::map_dfr(0:max_lag, function(lag) {
  window <- bps |>
    dplyr::filter(
      year >= first_completion_year - lag,
      year <= last_completion_year - lag
    )
  tibble::tibble(
    lag_years = lag,
    bps_first_year = min(window$year),
    bps_last_year = max(window$year),
    bps_units_authorized = sum(window$bps_units_total),
    ledger_units_completed = ledger_total,
    coverage_share = ledger_total / sum(window$bps_units_total)
  )
})
readr::write_csv(cumulative, "../output/bps_ledger_cumulative_coverage.csv")

# ---- composition by building size ------------------------------------------
bps_window <- bps |>
  dplyr::filter(year >= first_completion_year - 1, year <= last_completion_year - 1)
composition <- tibble::tibble(
  size_bucket = c("1", "2", "34", "5plus"),
  bps_units = c(
    sum(bps_window$bps_units_1), sum(bps_window$bps_units_2),
    sum(bps_window$bps_units_34), sum(bps_window$bps_units_5plus)
  ),
  ledger_units = c(
    sum(ledger$dwelling_units[ledger$size_bucket == "1"]),
    sum(ledger$dwelling_units[ledger$size_bucket == "2"]),
    sum(ledger$dwelling_units[ledger$size_bucket == "34"]),
    sum(ledger$dwelling_units[ledger$size_bucket == "5plus"])
  )
) |>
  dplyr::mutate(
    bps_share = bps_units / sum(bps_units),
    ledger_share = ledger_units / sum(ledger_units),
    coverage_share = ledger_units / bps_units
  )
readr::write_csv(composition, "../output/bps_ledger_size_composition.csv")

summary_table <- tibble::tibble(
  first_completion_year,
  last_completion_year,
  ledger_projects = nrow(ledger),
  ledger_units_completed = ledger_total,
  bps_units_authorized_lag1 = cumulative$bps_units_authorized[cumulative$lag_years == 1],
  coverage_share_lag1 = cumulative$coverage_share[cumulative$lag_years == 1],
  coverage_share_min = min(cumulative$coverage_share),
  coverage_share_max = max(cumulative$coverage_share),
  ledger_share_5plus = composition$ledger_share[composition$size_bucket == "5plus"],
  bps_share_5plus = composition$bps_share[composition$size_bucket == "5plus"]
)
readr::write_csv(summary_table, "../output/bps_ledger_coverage_summary.csv")

message("\n--- annual comparison ---")
print(as.data.frame(annual |> dplyr::select(
  year, ledger_projects, ledger_units_total, bps_units_total, contemporaneous_ratio
)), row.names = FALSE, digits = 3)
message("\n--- cumulative coverage by lag ---")
print(as.data.frame(cumulative), row.names = FALSE, digits = 3)
message("\n--- composition by building size (BPS lagged one year) ---")
print(as.data.frame(composition), row.names = FALSE, digits = 3)
