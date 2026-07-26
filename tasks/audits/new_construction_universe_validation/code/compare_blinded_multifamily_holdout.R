# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

sample <- readr::read_csv(
  "../output/multifamily_classification_mode_b_review_sample.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    class_first_multifamily = readr::col_logical(),
    .default = readr::col_guess()
  )
)

decisions <- readr::read_csv(
  "../adjudication/multifamily_holdout_blinded_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    sample_order = readr::col_integer(),
    project_id = readr::col_character(),
    decision = readr::col_character(),
    confidence = readr::col_character(),
    evidence_type = readr::col_character(),
    evidence_id = readr::col_character(),
    evidence_url = readr::col_character(),
    notes = readr::col_character()
  )
)

if (
  nrow(sample) != 50L ||
    nrow(decisions) != 50L ||
    anyDuplicated(sample$project_id) ||
    anyDuplicated(decisions$project_id) ||
    !setequal(sample$project_id, decisions$project_id)
) {
  stop("The blinded holdout sample and adjudication ledger do not match.")
}

if (
  any(!decisions$decision %in% c("multifamily", "not_multifamily", "ambiguous"))
) {
  stop("Unexpected blinded adjudication decision.")
}

comparison <- decisions |>
  dplyr::left_join(
    sample |>
      dplyr::select(
        project_id,
        source_family,
        component_pins,
        project_kind,
        construction_year,
        class_values,
        class_first_multifamily
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    programmatic_decision = dplyr::if_else(
      class_first_multifamily,
      "multifamily",
      "not_multifamily"
    ),
    independently_resolved = decision != "ambiguous",
    agreement = dplyr::if_else(
      independently_resolved,
      decision == programmatic_decision,
      NA
    )
  ) |>
  dplyr::arrange(sample_order)

summary <- tibble::tibble(
  statistic = c(
    "sampled_class_211_212_projects",
    "independently_resolved",
    "independently_confirmed_multifamily",
    "independently_rejected",
    "independently_ambiguous",
    "programmatic_disagreements",
    "agreement_among_resolved"
  ),
  value = c(
    nrow(comparison),
    sum(comparison$independently_resolved),
    sum(comparison$decision == "multifamily"),
    sum(comparison$decision == "not_multifamily"),
    sum(comparison$decision == "ambiguous"),
    sum(comparison$agreement == FALSE, na.rm = TRUE),
    mean(comparison$agreement, na.rm = TRUE)
  )
)

readr::write_csv(
  comparison,
  "../output/multifamily_holdout_blinded_comparison.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/multifamily_holdout_blinded_summary.csv",
  na = ""
)
