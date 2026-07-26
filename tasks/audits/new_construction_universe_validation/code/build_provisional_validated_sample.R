# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

current <- readr::read_csv(
  "../input/multicard_external_reviewed_model_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_family = readr::col_character(),
    ledger_action = readr::col_character(),
    project_kind = readr::col_character(),
    construction_date = readr::col_date(),
    ward_pair = readr::col_character(),
    segment_id = readr::col_character(),
    zone_group = readr::col_character(),
    alderman_own = readr::col_character(),
    alderman_neighbor = readr::col_character(),
    final_disposition = readr::col_character(),
    adjudication_reason = readr::col_character(),
    adjudication_confidence = readr::col_character(),
    original_dwelling_units = readr::col_double(),
    original_building_sqft = readr::col_double(),
    externally_reviewed = readr::col_logical(),
    external_multifamily = readr::col_logical(),
    external_value_used = readr::col_logical(),
    pin = readr::col_character(),
    review_priority = readr::col_double(),
    review_address = readr::col_character(),
    review_status = readr::col_character(),
    external_structure_class = readr::col_character(),
    multifamily_disposition = readr::col_character(),
    external_building_count = readr::col_double(),
    external_unit_count = readr::col_double(),
    external_building_sqft = readr::col_double(),
    source_1_url = readr::col_character(),
    source_2_url = readr::col_character(),
    supports_building_type = readr::col_logical(),
    supports_final_units = readr::col_logical(),
    reviewer_notes = readr::col_character(),
    review_date = readr::col_date(),
    .default = readr::col_guess()
  )
)

eligibility <- readr::read_csv(
  "../output/eligibility_rule_validation.csv",
  show_col_types = FALSE,
  col_select = c(
    project_id,
    eligibility_rule,
    proposed_action,
    rule_evidence
  ),
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::select(dplyr::everything())

classification <- readr::read_csv(
  "../output/multifamily_classification_decisions.csv",
  show_col_types = FALSE,
  col_select = c(
    project_id,
    resolved_dwelling_units,
    unit_count_rule,
    permit_unit_recovery_eligible,
    proposed_multifamily,
    classification_review_reason
  ),
  col_types = readr::cols(
    project_id = readr::col_character(),
    resolved_dwelling_units = readr::col_double(),
    unit_count_rule = readr::col_character(),
    permit_unit_recovery_eligible = readr::col_logical(),
    proposed_multifamily = readr::col_logical(),
    classification_review_reason = readr::col_character()
  )
) |>
  dplyr::select(dplyr::everything())

for (x in list(current, eligibility, classification)) {
  if (anyDuplicated(x$project_id)) {
    stop("A provisional-sample input is not uniquely keyed by project_id.")
  }
}

provisional <- current |>
  dplyr::left_join(
    eligibility,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    classification,
    by = "project_id",
    relationship = "one-to-one"
  )

if (
  any(is.na(provisional$proposed_action)) ||
    any(is.na(provisional$proposed_multifamily))
) {
  stop("Not every analysis project received an audit decision.")
}

adjudicated <- provisional |>
  dplyr::mutate(
    prior_dwelling_units = dwelling_units,
    prior_multifamily = dplyr::coalesce(external_multifamily, FALSE),
    dwelling_units = dplyr::if_else(
      permit_unit_recovery_eligible,
      resolved_dwelling_units,
      dwelling_units
    ),
    external_multifamily = proposed_multifamily,
    density_dupac = 43560 * dwelling_units / land_sqft,
    audit_sample_action = dplyr::case_when(
      proposed_action == "exclude" ~ "exclude_ineligible",
      proposed_action == "review" ~ "exclude_unresolved",
      permit_unit_recovery_eligible ~ "retain_recover_permit_units",
      external_multifamily & !prior_multifamily ~
        "retain_reclassify_as_multifamily",
      !external_multifamily & prior_multifamily ~
        "retain_reclassify_as_single_family",
      TRUE ~ "retain_unchanged"
    )
  )

provisional <- adjudicated |>
  dplyr::filter(proposed_action == "retain")

if (anyDuplicated(provisional$project_id)) {
  stop("The provisional validated sample contains duplicate project IDs.")
}
if (any(provisional$external_multifamily & provisional$dwelling_units <= 1)) {
  stop(
    paste(
      "Provisional multifamily projects without recovered unit counts:",
      paste(
        provisional$project_id[
          provisional$external_multifamily &
            provisional$dwelling_units <= 1
        ],
        collapse = ", "
      )
    )
  )
}
if (
  any(
    provisional$allow_dupac & !is.finite(provisional$density_dupac),
    na.rm = TRUE
  )
) {
  stop("A provisional project allowed in DUPAC has nonfinite DUPAC.")
}

changes <- dplyr::bind_rows(
  current |>
    dplyr::summarise(
      sample = "current",
      projects_1500ft = dplyr::n(),
      projects_500ft = sum(within_500ft),
      multifamily_1500ft = sum(external_multifamily, na.rm = TRUE),
      multifamily_500ft = sum(
        external_multifamily & within_500ft,
        na.rm = TRUE
      )
    ),
  provisional |>
    dplyr::summarise(
      sample = "provisional_validated",
      projects_1500ft = dplyr::n(),
      projects_500ft = sum(within_500ft),
      multifamily_1500ft = sum(external_multifamily),
      multifamily_500ft = sum(external_multifamily & within_500ft)
    ),
  adjudicated |>
    dplyr::group_by(audit_sample_action) |>
    dplyr::summarise(
      projects_1500ft = dplyr::n(),
      projects_500ft = sum(within_500ft),
      multifamily_1500ft = sum(external_multifamily),
      multifamily_500ft = sum(external_multifamily & within_500ft),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      sample = paste0("action:", audit_sample_action),
      projects_1500ft,
      projects_500ft,
      multifamily_1500ft,
      multifamily_500ft
    )
)

readr::write_csv(
  provisional,
  "../output/provisional_validated_density_input.csv",
  na = ""
)
readr::write_csv(
  changes,
  "../output/provisional_validated_sample_changes.csv",
  na = ""
)
