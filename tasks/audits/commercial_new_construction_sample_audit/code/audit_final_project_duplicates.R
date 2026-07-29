# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_address <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_squish()
}

ledger <- readr::read_csv(
  "../output/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    permit_chain_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::mutate(normalized_address = normalize_address(source_addresses))

if (anyDuplicated(ledger$project_id)) {
  stop("Final ledger is not unique by project.", call. = FALSE)
}

additions <- ledger |>
  dplyr::filter(ledger_action == "add_recovered_project")
retained <- ledger |>
  dplyr::filter(ledger_action == "retain_existing")

pair_fields <- c(
  "project_id",
  "source_addresses",
  "normalized_address",
  "component_pins",
  "permit_chain_ids",
  "construction_year",
  "dwelling_units",
  "building_sqft",
  "land_sqft",
  "x_3435",
  "y_3435"
)

addition_retained <- dplyr::cross_join(
  additions |>
    dplyr::select(dplyr::all_of(pair_fields)) |>
    dplyr::rename_with(~ paste0(.x, "_1")),
  retained |>
    dplyr::select(dplyr::all_of(pair_fields)) |>
    dplyr::rename_with(~ paste0(.x, "_2"))
) |>
  dplyr::mutate(pair_scope = "recovered_vs_retained")

addition_pairs <- dplyr::cross_join(
  additions |>
    dplyr::select(dplyr::all_of(pair_fields)) |>
    dplyr::rename_with(~ paste0(.x, "_1")),
  additions |>
    dplyr::select(dplyr::all_of(pair_fields)) |>
    dplyr::rename_with(~ paste0(.x, "_2"))
) |>
  dplyr::filter(project_id_1 < project_id_2) |>
  dplyr::mutate(pair_scope = "recovered_vs_recovered")

candidates <- dplyr::bind_rows(
  addition_retained,
  addition_pairs
) |>
  dplyr::mutate(
    distance_ft = sqrt(
      (x_3435_1 - x_3435_2)^2 +
        (y_3435_1 - y_3435_2)^2
    ),
    year_gap = abs(construction_year_1 - construction_year_2),
    building_ratio = pmin(building_sqft_1, building_sqft_2) /
      pmax(building_sqft_1, building_sqft_2),
    land_ratio = pmin(land_sqft_1, land_sqft_2) /
      pmax(land_sqft_1, land_sqft_2),
    same_address =
      !is.na(normalized_address_1) &
        normalized_address_1 == normalized_address_2,
    candidate =
      same_address |
        (
          distance_ft <= 100 &
            year_gap <= 4 &
            (
              (
                dwelling_units_1 == dwelling_units_2 &
                  building_ratio >= 0.90
              ) |
                (
                  building_ratio >= 0.90 &
                    land_ratio >= 0.90
                )
            )
        )
  ) |>
  dplyr::filter(candidate) |>
  dplyr::select(-candidate) |>
  dplyr::arrange(pair_scope, project_id_1, project_id_2)

decisions <- readr::read_csv(
  "../adjudication/recovered_project_duplicate_pair_decisions.csv",
  show_col_types = FALSE
)

if (
  anyDuplicated(decisions[c("project_id_1", "project_id_2")]) ||
    any(decisions$project_id_1 >= decisions$project_id_2)
) {
  stop("Recovered duplicate decisions have invalid pair keys.", call. = FALSE)
}

candidates <- candidates |>
  dplyr::left_join(
    decisions,
    by = c("project_id_1", "project_id_2"),
    relationship = "one-to-one"
  )

unresolved <- candidates |>
  dplyr::filter(
    is.na(pair_disposition) |
      pair_disposition == ""
  )
unused_decisions <- decisions |>
  dplyr::anti_join(
    candidates,
    by = c("project_id_1", "project_id_2")
  )

if (
  any(candidates$pair_scope == "recovered_vs_retained") ||
    nrow(unresolved) > 0L ||
    nrow(unused_decisions) > 0L ||
    any(decisions$pair_disposition !=
      "retain_distinct_addressed_buildings")
) {
  stop("Final adversarial duplicate review is incomplete.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "recovered_vs_retained_candidate_pairs",
    "recovered_vs_recovered_candidate_pairs",
    "resolved_distinct_addressed_pairs",
    "unresolved_candidate_pairs",
    "unused_pair_decisions"
  ),
  value = c(
    sum(candidates$pair_scope == "recovered_vs_retained"),
    sum(candidates$pair_scope == "recovered_vs_recovered"),
    sum(candidates$pair_disposition ==
      "retain_distinct_addressed_buildings"),
    nrow(unresolved),
    nrow(unused_decisions)
  )
)

readr::write_csv(
  candidates,
  "../output/final_adversarial_duplicate_candidates.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/final_adversarial_duplicate_summary.csv",
  na = ""
)
