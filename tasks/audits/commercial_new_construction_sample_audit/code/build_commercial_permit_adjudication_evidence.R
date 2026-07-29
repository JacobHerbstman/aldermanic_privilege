# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

queue <- readr::read_csv(
  "../output/commercial_adjudication_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
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
  filter(source_family == "commercial", project_id %in% queue$project_id) %>%
  mutate(
    address_key = str_to_upper(coalesce(permit_address, "")) %>%
      str_replace_all("[^A-Z0-9 ]", " ") %>%
      str_replace_all(
        "\\b(STREET|ST|AVENUE|AVE|ROAD|RD|BOULEVARD|BLVD|COURT|CT|PLACE|PL|DRIVE|DR)\\b",
        ""
      ) %>%
      str_squish()
  )

unit_mentions <- readr::read_csv(
  "../output/project_permit_chain_unit_mentions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    permit_number = readr::col_character(),
    mention_context = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "commercial", project_id %in% queue$project_id) %>%
  select(-source_family)

chain_evidence <- permit_links %>%
  group_by(project_id, permit_chain_id) %>%
  summarise(
    permit_records = n_distinct(permit_number),
    directly_matched_permits = n_distinct(permit_number[directly_matched]),
    permit_numbers = paste(sort(unique(permit_number)), collapse = "/"),
    permit_addresses = paste(sort(unique(permit_address)), collapse = " / "),
    address_keys = paste(sort(unique(address_key)), collapse = "/"),
    earliest_application_date = min(application_date, na.rm = TRUE),
    earliest_issue_date = min(issue_date, na.rm = TRUE),
    latest_issue_date = max(issue_date, na.rm = TRUE),
    permit_statuses = paste(sort(unique(permit_status)), collapse = "/"),
    work_descriptions = paste(
      unique(paste0(permit_number, ": ", work_description)),
      collapse = " || "
    ),
    .groups = "drop"
  ) %>%
  left_join(
    unit_mentions %>%
      group_by(project_id, permit_chain_id) %>%
      summarise(
        distinct_unit_counts = n_distinct(unit_count),
        unit_counts = paste(sort(unique(unit_count)), collapse = "/"),
        unit_contexts = paste(
          unique(paste0(permit_number, ": ", unit_count, " [", mention_context, "]")),
          collapse = " || "
        ),
        .groups = "drop"
      ),
    by = c("project_id", "permit_chain_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    distinct_unit_counts = coalesce(distinct_unit_counts, 0L),
    unit_counts = coalesce(unit_counts, ""),
    resolved_chain_units = if_else(
      distinct_unit_counts == 1L,
      suppressWarnings(as.numeric(unit_counts)),
      NA_real_
    ),
    chain_resolution = case_when(
      distinct_unit_counts == 0L ~ "no_unit_count",
      distinct_unit_counts == 1L ~ "single_unit_count",
      TRUE ~ "conflicting_unit_counts"
    )
  ) %>%
  arrange(project_id, earliest_application_date, permit_chain_id)

address_evidence <- permit_links %>%
  filter(address_key != "") %>%
  distinct(project_id, permit_chain_id, permit_number, address_key) %>%
  left_join(
    unit_mentions,
    by = c("project_id", "permit_chain_id", "permit_number"),
    relationship = "one-to-many"
  )

if (anyDuplicated(address_evidence[c(
  "project_id", "permit_chain_id", "permit_number", "address_key", "mention_order"
)]) > 0) {
  stop("Permit-address unit evidence contains duplicate mention keys.", call. = FALSE)
}

address_evidence <- address_evidence %>%
  group_by(project_id, address_key) %>%
  summarise(
    permit_chains = n_distinct(permit_chain_id),
    permits_with_unit_mentions = n_distinct(permit_number[is.finite(unit_count)]),
    distinct_unit_counts = n_distinct(unit_count, na.rm = TRUE),
    unit_counts = paste(sort(unique(unit_count[is.finite(unit_count)])), collapse = "/"),
    unit_contexts = paste(
      unique(paste0(permit_number[is.finite(unit_count)], ": ", unit_count[is.finite(unit_count)],
                    " [", mention_context[is.finite(unit_count)], "]")),
      collapse = " || "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    resolved_address_units = if_else(
      distinct_unit_counts == 1L,
      suppressWarnings(as.numeric(unit_counts)),
      NA_real_
    ),
    address_resolution = case_when(
      distinct_unit_counts == 0L ~ "no_unit_count",
      distinct_unit_counts == 1L ~ "single_unit_count",
      TRUE ~ "conflicting_unit_counts"
    )
  ) %>%
  arrange(project_id, address_key)

address_project_summary <- address_evidence %>%
  group_by(project_id) %>%
  summarise(
    permit_addresses = n(),
    addresses_with_unit_counts = sum(distinct_unit_counts > 0),
    addresses_with_resolved_units = sum(is.finite(resolved_address_units)),
    resolved_address_unit_sum = if_else(
      addresses_with_unit_counts > 0 &
        addresses_with_resolved_units == addresses_with_unit_counts,
      sum(resolved_address_units, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

address_matches <- address_evidence %>%
  inner_join(
    queue %>% select(project_id, dwelling_units),
    by = "project_id",
    relationship = "many-to-one"
  ) %>%
  group_by(project_id) %>%
  summarise(
    assessor_units_match_one_address = any(resolved_address_units == dwelling_units),
    .groups = "drop"
  )

project_evidence <- queue %>%
  select(project_id, dwelling_units, decision_reason) %>%
  left_join(
    address_project_summary,
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  left_join(address_matches, by = "project_id", relationship = "one-to-one") %>%
  mutate(
    addresses_with_unit_counts = coalesce(addresses_with_unit_counts, 0L),
    addresses_with_resolved_units = coalesce(addresses_with_resolved_units, 0L),
    assessor_units_match_one_address = coalesce(assessor_units_match_one_address, FALSE),
    assessor_units_match_resolved_address_sum = coalesce(
      is.finite(resolved_address_unit_sum) & dwelling_units == resolved_address_unit_sum,
      FALSE
    )
  ) %>%
  arrange(project_id)

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(project_evidence), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Commercial permit evidence contains a prohibited analysis field.", call. = FALSE)
}

summary <- tibble::tribble(
  ~metric, ~value,
  "queued_commercial_projects", nrow(project_evidence),
  "permit_chains", nrow(chain_evidence),
  "permit_addresses", nrow(address_evidence),
  "projects_matching_one_permit_address", sum(project_evidence$assessor_units_match_one_address, na.rm = TRUE),
  "projects_matching_sum_of_resolved_addresses", sum(project_evidence$assessor_units_match_resolved_address_sum),
  "duplicate_project_ids", anyDuplicated(project_evidence$project_id)
)

readr::write_csv(chain_evidence, "../output/commercial_permit_chain_evidence.csv")
readr::write_csv(address_evidence, "../output/commercial_permit_address_evidence.csv")
readr::write_csv(project_evidence, "../output/commercial_permit_project_evidence.csv")
readr::write_csv(summary, "../output/commercial_permit_adjudication_summary.csv")
