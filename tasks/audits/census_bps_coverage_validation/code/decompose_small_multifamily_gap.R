# Decompose the coverage gap in the 3-4 unit building bucket.
#
# The aggregate comparison shows 3-4 unit buildings covered at roughly 41% of
# Building Permits Survey authorizations while 5+ unit buildings are covered at
# 74%. This script attributes the shortfall to three sources:
#
#   1. project aggregation - a ledger project that combines several small
#      buildings is booked at its project-level unit count, so a site of three
#      3-unit buildings lands in the 5+ bucket while BPS counts three 3-unit
#      buildings;
#   2. single-dwelling bucketing - individually owned townhouse and rowhouse
#      records are one dwelling each in the ledger, while the authorizing permit
#      and BPS describe one multi-dwelling building; and
#   3. an unexplained residual.
#
# It also reports coverage by housing-cycle era, because a fixed one-year
# authorization-to-completion lag is wrong during the 2005-2008 boom when large
# projects took several years to finish.
#
# setwd("tasks/audits/census_bps_coverage_validation/code")
# first_completion_year <- "2006"
# last_completion_year <- "2022"

source("../../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: decompose_small_multifamily_gap.R <first_completion_year> <last_completion_year>")
}
first_completion_year <- as.integer(args[1])
last_completion_year <- as.integer(args[2])

bps <- readr::read_csv(
  "../input/census_bps_place_midwest.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::filter(state_code == "17", county_code == "031", six_digit_id == "147700") |>
  dplyr::transmute(
    year = as.integer(survey_year),
    bps_units_34 = as.numeric(units34_units),
    bps_units_5plus = as.numeric(units5plus_units)
  )

cross_section <- readr::read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_select = c(pin, class, pin_num_cards),
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    pin_num_cards = readr::col_double()
  )
)
if (anyDuplicated(cross_section$pin)) {
  stop("Residential cross-section input is not uniquely keyed by PIN.")
}
cards <- tibble::deframe(cross_section |> dplyr::select(pin, pin_num_cards))

ledger <- readr::read_csv(
  "../input/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_select = c(project_id, construction_year, dwelling_units, component_pins, project_kind),
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_year = readr::col_double(),
    dwelling_units = readr::col_double(),
    component_pins = readr::col_character(),
    project_kind = readr::col_character()
  )
) |>
  dplyr::filter(
    is.finite(construction_year),
    construction_year >= first_completion_year,
    construction_year <= last_completion_year,
    is.finite(dwelling_units), dwelling_units > 0
  ) |>
  dplyr::mutate(
    year = as.integer(construction_year),
    component_count = stringr::str_count(component_pins, "/") + 1L,
    card_count = purrr::map_dbl(
      stringr::str_split(component_pins, "/"),
      function(pins) {
        pins <- stringr::str_trim(pins)
        pins <- pins[pins != ""]
        n <- suppressWarnings(cards[pins])
        n[!is.finite(n)] <- 1
        if (length(n) == 0) 1 else sum(n)
      }
    ),
    implied_buildings = pmax(component_count, card_count, 1),
    units_per_building = dwelling_units / implied_buildings
  )

# ---- source 1: 5+ projects whose per-building unit count is really 3-4 -------
aggregation <- ledger |>
  dplyr::filter(
    dwelling_units >= 5,
    implied_buildings > 1,
    units_per_building >= 2, units_per_building <= 4.5
  )

# ---- source 2: single-dwelling records under a multi-dwelling permit --------
# The permit evidence is taken from the completed multifamily-classification
# decisions, which already carry exact-PIN new-construction permit text.
decisions <- readr::read_csv(
  "../input/multifamily_classification_decisions.csv",
  show_col_types = FALSE,
  col_select = c(project_id, exact_pin_positive_descriptions),
  col_types = readr::cols(
    project_id = readr::col_character(),
    exact_pin_positive_descriptions = readr::col_character()
  )
)
permit_dwellings <- function(text) {
  vapply(
    stringr::str_extract_all(
      toupper(dplyr::coalesce(text, "")),
      "(\\d+)\\s*[-–]?\\s*(?:DWELLING\\s*UNITS?|D\\.?\\s?U\\.?'?S?\\b)"
    ),
    function(hits) {
      if (length(hits) == 0) return(NA_real_)
      v <- suppressWarnings(as.numeric(stringr::str_extract(hits, "\\d+")))
      v <- v[is.finite(v) & v > 0 & v <= 100]
      if (length(v) == 0) NA_real_ else max(v)
    },
    numeric(1)
  )
}
single_dwelling_bucketing <- ledger |>
  dplyr::filter(dwelling_units == 1) |>
  dplyr::left_join(decisions, by = "project_id", relationship = "one-to-one") |>
  dplyr::mutate(permit_units = permit_dwellings(exact_pin_positive_descriptions)) |>
  dplyr::filter(is.finite(permit_units), permit_units >= 2)

bps_window <- bps |>
  dplyr::filter(year >= first_completion_year - 1, year <= last_completion_year - 1)
bps_34 <- sum(bps_window$bps_units_34)
ledger_34 <- sum(ledger$dwelling_units[ledger$dwelling_units >= 3 & ledger$dwelling_units <= 4])

decomposition <- tibble::tibble(
  component = c(
    "bps_units_authorized_34",
    "ledger_units_booked_34",
    "shortfall",
    "explained_project_aggregation_into_5plus",
    "explained_single_dwelling_townhouse_bucketing",
    "unexplained_residual"
  ),
  units = c(
    bps_34,
    ledger_34,
    bps_34 - ledger_34,
    sum(aggregation$dwelling_units),
    nrow(single_dwelling_bucketing),
    bps_34 - ledger_34 - sum(aggregation$dwelling_units) - nrow(single_dwelling_bucketing)
  )
) |>
  dplyr::mutate(share_of_bps_34 = units / bps_34)
readr::write_csv(decomposition, "../output/small_multifamily_gap_decomposition.csv")

# ---- coverage by housing-cycle era, 3-4 against 5+ as a control -------------
ledger_by_year <- ledger |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    ledger_units_34 = sum(dwelling_units[dwelling_units >= 3 & dwelling_units <= 4]),
    ledger_units_5plus = sum(dwelling_units[dwelling_units >= 5]),
    .groups = "drop"
  )
era <- bps |>
  dplyr::filter(year >= first_completion_year - 1, year <= last_completion_year - 1) |>
  dplyr::mutate(
    completion_year = year + 1L,
    era = dplyr::case_when(
      year <= 2008 ~ "boom authorizations 2005-2008",
      year <= 2012 ~ "trough authorizations 2009-2012",
      TRUE ~ "recovery authorizations 2013-2021"
    )
  ) |>
  dplyr::left_join(ledger_by_year, by = c("completion_year" = "year")) |>
  dplyr::group_by(era) |>
  dplyr::summarise(
    bps_34 = sum(bps_units_34), ledger_34 = sum(ledger_units_34, na.rm = TRUE),
    bps_5plus = sum(bps_units_5plus), ledger_5plus = sum(ledger_units_5plus, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(coverage_34 = ledger_34 / bps_34, coverage_5plus = ledger_5plus / bps_5plus)
readr::write_csv(era, "../output/small_multifamily_gap_by_era.csv")

message("\n--- 3-4 unit gap decomposition ---")
print(as.data.frame(decomposition), row.names = FALSE, digits = 3)
message("\n--- coverage by housing-cycle era (5+ is the control) ---")
print(as.data.frame(era), row.names = FALSE, digits = 3)
