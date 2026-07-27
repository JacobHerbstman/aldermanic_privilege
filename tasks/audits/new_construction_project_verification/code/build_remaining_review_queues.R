# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/reviewed_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_year = readr::col_double(),
    audit_current_multifamily = readr::col_logical(),
    possible_contradictory_snapshot = readr::col_logical(),
    audit_decision = readr::col_character(),
    .default = readr::col_guess()
  )
)

pending_projects <- projects |>
  dplyr::filter(
    audit_decision == "retain_assessor_only_pending_review"
  )

multifamily_queue <- pending_projects |>
  dplyr::filter(audit_current_multifamily) |>
  dplyr::arrange(
    dplyr::desc(possible_contradictory_snapshot),
    construction_year,
    ward_pair,
    project_id
  )

if (anyDuplicated(multifamily_queue$project_id)) {
  stop("The remaining multifamily review queue contains duplicate project IDs.")
}

readr::write_csv(
  multifamily_queue,
  "../output/remaining_multifamily_review_queue.csv",
  na = ""
)

nonmultifamily_projects <- pending_projects |>
  dplyr::filter(!audit_current_multifamily) |>
  dplyr::mutate(
    review_cluster = stringr::str_to_upper(review_address),
    review_cluster = stringr::str_replace_all(
      review_cluster,
      "\\b(?:UNIT|APT|APARTMENT|BLDG|BUILDING)\\s*[A-Z0-9-]+\\b",
      ""
    ),
    review_cluster = stringr::str_replace_all(review_cluster, "[^A-Z0-9]+", " "),
    review_cluster = stringr::str_squish(review_cluster),
    review_cluster = dplyr::if_else(
      is.na(review_cluster) | review_cluster == "",
      project_id,
      review_cluster
    ),
    construction_period = cut(
      construction_year,
      breaks = c(2005, 2006, 2008, 2014, 2018, 2022),
      labels = c(
        "2006",
        "2007-2008",
        "2009-2014",
        "2015-2018",
        "2019-2022"
      )
    )
  )

set.seed(20260727)
nonmultifamily_validation_sample <- nonmultifamily_projects |>
  dplyr::group_by(review_cluster) |>
  dplyr::slice_sample(n = 1) |>
  dplyr::ungroup() |>
  dplyr::group_by(
    construction_period,
    possible_contradictory_snapshot
  ) |>
  dplyr::slice_sample(n = 5) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    construction_period,
    dplyr::desc(possible_contradictory_snapshot),
    construction_year,
    project_id
  )

if (nrow(nonmultifamily_validation_sample) == 0L ||
    anyDuplicated(nonmultifamily_validation_sample$project_id)) {
  stop("The non-multifamily validation sample is empty or duplicated.")
}

readr::write_csv(
  nonmultifamily_validation_sample,
  "../output/nonmultifamily_validation_sample.csv",
  na = ""
)
