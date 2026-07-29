# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

groups <- readr::read_csv(
  "../output/commercial_cross_family_duplicate_group_members.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    duplicate_review_group_id = readr::col_character(),
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

entity_versions <- readr::read_csv(
  "../output/commercial_entity_version_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    keypin = readr::col_character(),
    pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

permit_links <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "commercial")

if (anyDuplicated(groups$project_id) > 0) {
  stop("A commercial project appears in more than one duplicate-review group.", call. = FALSE)
}
if (anyDuplicated(entity_versions$raw_row) > 0) {
  stop("Commercial entity versions are not unique by raw source row.", call. = FALSE)
}
if (anyDuplicated(permit_links[c("project_id", "permit_chain_id", "permit_id")]) > 0) {
  stop("Commercial permit links repeat a project-chain-permit key.", call. = FALSE)
}

source_rows <- groups %>%
  select(duplicate_review_group_id, project_id) %>%
  left_join(
    entity_versions,
    by = c("project_id" = "project_family_id"),
    relationship = "one-to-many"
  ) %>%
  arrange(duplicate_review_group_id, project_id, valuation_year, raw_row)

permit_evidence <- groups %>%
  select(duplicate_review_group_id, project_id) %>%
  left_join(permit_links, by = "project_id", relationship = "one-to-many") %>%
  filter(!is.na(permit_chain_id)) %>%
  distinct(
    duplicate_review_group_id,
    project_id,
    permit_chain_id,
    permit_id,
    .keep_all = TRUE
  ) %>%
  arrange(
    duplicate_review_group_id,
    project_id,
    permit_chain_id,
    application_date,
    permit_number
  )

project_permits <- permit_evidence %>%
  group_by(duplicate_review_group_id, project_id) %>%
  summarise(
    permit_chain_ids = paste(sort(unique(permit_chain_id)), collapse = "/"),
    direct_permit_numbers = paste(
      sort(unique(permit_number[coalesce(directly_matched, FALSE)])),
      collapse = "/"
    ),
    all_linked_permit_numbers = paste(sort(unique(permit_number)), collapse = "/"),
    direct_permit_descriptions = paste(
      unique(work_description[coalesce(directly_matched, FALSE)]),
      collapse = " || "
    ),
    .groups = "drop"
  )

review_bundle <- groups %>%
  left_join(
    project_permits,
    by = c("duplicate_review_group_id", "project_id"),
    relationship = "one-to-one"
  ) %>%
  arrange(duplicate_review_group_id, project_id)

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(review_bundle), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Commercial duplicate review bundle contains a prohibited analysis field.", call. = FALSE)
}
if (nrow(review_bundle) != nrow(groups)) {
  stop("Commercial duplicate review bundle does not cover every grouped project.", call. = FALSE)
}
if (anyDuplicated(review_bundle$project_id) > 0) {
  stop("Commercial duplicate review bundle is not unique by project.", call. = FALSE)
}

summary <- tibble::tibble(
  section = "coverage",
  metric = c(
    "review_groups",
    "review_projects",
    "source_rows",
    "permit_links",
    "projects_with_direct_permits",
    "duplicate_review_project_ids",
    "duplicate_raw_source_rows",
    "duplicate_project_chain_permit_keys"
  ),
  value = c(
    n_distinct(groups$duplicate_review_group_id),
    nrow(groups),
    nrow(source_rows),
    nrow(permit_evidence),
    sum(!is.na(review_bundle$direct_permit_numbers) & review_bundle$direct_permit_numbers != ""),
    anyDuplicated(review_bundle$project_id),
    anyDuplicated(source_rows$raw_row),
    anyDuplicated(permit_evidence[c("project_id", "permit_chain_id", "permit_id")])
  )
)

readr::write_csv(source_rows, "../output/commercial_duplicate_review_source_rows.csv")
readr::write_csv(permit_evidence, "../output/commercial_duplicate_review_permit_evidence.csv")
readr::write_csv(review_bundle, "../output/commercial_duplicate_review_bundle.csv")
readr::write_csv(summary, "../output/commercial_duplicate_review_bundle_summary.csv")
