# Three-way benchmark for Chicago housing production in the 2010s.
#
#   BPS    - units AUTHORIZED by permit (Census Building Permits Survey)
#   ACS    - occupied units that EXIST and report being built 2010 or later
#   ledger - units COMPLETED and recorded by the Cook County Assessor
#
# The point of adding ACS is to separate two explanations for the small
# multifamily coverage gap found against BPS. If authorized 2-4 unit buildings
# were simply never built, or if BPS overcounts authorizations, then ACS should
# agree with the ledger and fall well below BPS. If ACS instead agrees with BPS,
# the units exist and the ledger is missing them.
#
# Two features of ACS make its counts a LOWER bound on units built:
#   - the universe is occupied units, so vacant new units are excluded; and
#   - a five-year 2015-2019 estimate averages over the period, so units built
#     late in the decade are under-represented.
# Both biases understate ACS relative to the truth, so a gap measured against
# ACS is a conservative gap.
#
# setwd("tasks/audits/acs_structure_age_validation/code")
# first_completion_year <- "2010"
# last_completion_year <- "2019"

source("../../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: compare_acs_to_new_construction.R <first_completion_year> <last_completion_year>")
}
first_completion_year <- as.integer(args[1])
last_completion_year <- as.integer(args[2])

acs <- readr::read_csv(
  "../output/acs_b25127_chicago_built_2010_or_later.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(structure_size != "mobile") |>
  dplyr::mutate(
    bucket = dplyr::case_when(
      structure_size == "units1" ~ "1",
      structure_size == "units2to4" ~ "2to4",
      TRUE ~ "5plus"
    )
  ) |>
  dplyr::group_by(bucket) |>
  dplyr::summarise(
    acs_units = sum(occupied_units),
    acs_moe = sqrt(sum(margin_of_error^2)),
    .groups = "drop"
  )

ledger <- readr::read_csv(
  "../input/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_select = c(project_id, construction_year, dwelling_units),
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_year = readr::col_double(),
    dwelling_units = readr::col_double()
  )
) |>
  dplyr::filter(
    is.finite(construction_year),
    construction_year >= first_completion_year,
    construction_year <= last_completion_year,
    is.finite(dwelling_units), dwelling_units > 0
  ) |>
  dplyr::mutate(
    bucket = dplyr::case_when(
      dwelling_units == 1 ~ "1",
      dwelling_units <= 4 ~ "2to4",
      TRUE ~ "5plus"
    )
  ) |>
  dplyr::group_by(bucket) |>
  dplyr::summarise(
    ledger_projects = dplyr::n(),
    ledger_units = sum(dwelling_units),
    .groups = "drop"
  )

# BPS is lagged one year so authorizations line up with the completion window.
bps <- readr::read_csv(
  "../input/census_bps_place_midwest.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::filter(state_code == "17", county_code == "031", six_digit_id == "147700") |>
  dplyr::transmute(
    year = as.integer(survey_year),
    `1` = as.numeric(units1_units),
    `2to4` = as.numeric(units2_units) + as.numeric(units34_units),
    `5plus` = as.numeric(units5plus_units)
  ) |>
  dplyr::filter(
    year >= first_completion_year - 1,
    year <= last_completion_year - 1
  ) |>
  tidyr::pivot_longer(-year, names_to = "bucket", values_to = "bps_units") |>
  dplyr::group_by(bucket) |>
  dplyr::summarise(bps_units = sum(bps_units), .groups = "drop")

comparison <- bps |>
  dplyr::full_join(acs, by = "bucket") |>
  dplyr::full_join(ledger, by = "bucket") |>
  dplyr::mutate(
    acs_share_of_bps = acs_units / bps_units,
    ledger_share_of_bps = ledger_units / bps_units,
    ledger_share_of_acs = ledger_units / acs_units
  ) |>
  dplyr::arrange(match(bucket, c("1", "2to4", "5plus")))

readr::write_csv(comparison, "../output/acs_ledger_bps_comparison.csv")

totals <- comparison |>
  dplyr::summarise(
    bucket = "all",
    bps_units = sum(bps_units), acs_units = sum(acs_units),
    ledger_units = sum(ledger_units),
    acs_share_of_bps = sum(acs_units) / sum(bps_units),
    ledger_share_of_bps = sum(ledger_units) / sum(bps_units),
    ledger_share_of_acs = sum(ledger_units) / sum(acs_units)
  )

summary_table <- dplyr::bind_rows(comparison |> dplyr::select(-ledger_projects), totals) |>
  dplyr::mutate(
    first_completion_year = first_completion_year,
    last_completion_year = last_completion_year,
    verdict = dplyr::case_when(
      bucket == "all" ~ NA_character_,
      acs_share_of_bps >= 0.75 & ledger_share_of_acs < 0.75 ~
        "units exist; ledger undercounts",
      acs_share_of_bps < 0.75 & ledger_share_of_acs >= 0.75 ~
        "authorized units not built or BPS overcounts; ledger agrees with ACS",
      TRUE ~ "mixed"
    )
  )
readr::write_csv(summary_table, "../output/acs_ledger_bps_summary.csv")

message(sprintf(
  "\nChicago, structures built %d-%d. BPS lagged one year.\n",
  first_completion_year, last_completion_year
))
print(as.data.frame(summary_table |> dplyr::select(
  bucket, bps_units, acs_units, ledger_units,
  acs_share_of_bps, ledger_share_of_bps, ledger_share_of_acs, verdict
)), row.names = FALSE, digits = 3)
