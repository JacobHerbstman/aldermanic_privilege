# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin <- function(x) {
  digits <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
  dplyr::if_else(stringr::str_length(digits) == 14L, digits, NA_character_)
}

normalize_pin10 <- function(x) {
  digits <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
  dplyr::if_else(stringr::str_length(digits) >= 10L, stringr::str_sub(digits, 1, 10), NA_character_)
}

spatial_links <- readr::read_csv(
  "../output/permit_residential_assessor_candidate_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    candidate_row_id = readr::col_character(),
    candidate_pin = readr::col_character(),
    candidate_class = readr::col_character(),
    candidate_tieback_group = readr::col_character(),
    candidate_history_year_values = readr::col_character(),
    represented_project_ids = readr::col_character(),
    permit_pin10s = readr::col_character(),
    representative_permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
)
address_links <- readr::read_csv(
  "../output/permit_residential_assessor_address_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    candidate_row_id = readr::col_character(),
    candidate_pin = readr::col_character(),
    candidate_class = readr::col_character(),
    represented_project_ids = readr::col_character(),
    permit_pin10s = readr::col_character(),
    representative_permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
)
chains <- readr::read_csv(
  "../output/permit_first_unmatched_residential_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    permit_numbers = readr::col_character(),
    representative_permit_id = readr::col_character(),
    representative_permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
)
permits <- readr::read_csv(
  "../output/permit_first_permit_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
source_projects <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_row_ids = readr::col_character(),
    permit_chain_ids = readr::col_character(),
    permit_numbers = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(chains$permit_chain_id)) {
  stop("Residual permit chains must be unique.", call. = FALSE)
}

spatial_evidence <- spatial_links |>
  dplyr::transmute(
    permit_chain_id,
    candidate_pin = normalize_pin(candidate_pin),
    candidate_row_id,
    candidate_tax_year,
    candidate_card_num,
    candidate_class,
    candidate_year_built,
    candidate_building_sqft,
    candidate_land_sqft,
    candidate_units,
    candidate_family_type,
    candidate_residence_type,
    candidate_tieback_group,
    candidate_history_year_values,
    candidate_coordinate_source,
    candidate_review_category,
    candidate_mechanical_status,
    represented_project_ids,
    represented_in_preferred_ledger,
    exact_permit_pin10,
    evidence_source = "assessor_spatial",
    exact_normalized_address = FALSE,
    assessor_address = NA_character_,
    address_source = NA_character_,
    assessor_distance_ft = distance_ft
  )

address_evidence <- address_links |>
  dplyr::transmute(
    permit_chain_id,
    candidate_pin = normalize_pin(candidate_pin),
    candidate_row_id,
    candidate_tax_year,
    candidate_card_num,
    candidate_class,
    candidate_year_built,
    candidate_building_sqft,
    candidate_land_sqft,
    candidate_units,
    candidate_family_type = NA_character_,
    candidate_residence_type = NA_character_,
    candidate_tieback_group = NA_character_,
    candidate_history_year_values = NA_character_,
    candidate_coordinate_source = NA_character_,
    candidate_review_category,
    candidate_mechanical_status,
    represented_project_ids,
    represented_in_preferred_ledger,
    exact_permit_pin10,
    evidence_source = "exact_normalized_address",
    exact_normalized_address = TRUE,
    assessor_address,
    address_source,
    assessor_distance_ft = NA_real_
  )

evidence_rows <- dplyr::bind_rows(spatial_evidence, address_evidence) |>
  dplyr::filter(!is.na(candidate_pin)) |>
  dplyr::mutate(
    candidate_pin10 = normalize_pin10(candidate_pin),
    represented_in_preferred_ledger = dplyr::coalesce(
      represented_in_preferred_ledger,
      FALSE
    )
  )

candidate_evidence <- evidence_rows |>
  dplyr::group_by(permit_chain_id, candidate_pin) |>
  dplyr::summarise(
    evidence_sources = paste(sort(unique(evidence_source)), collapse = "/"),
    evidence_row_count = dplyr::n(),
    candidate_row_ids = paste(sort(unique(candidate_row_id)), collapse = "/"),
    candidate_tax_years = paste(sort(unique(candidate_tax_year)), collapse = "/"),
    candidate_card_nums = paste(sort(unique(candidate_card_num)), collapse = "/"),
    candidate_year_values = paste(sort(unique(candidate_year_built)), collapse = "/"),
    exact_permit_pin10 = any(exact_permit_pin10),
    exact_normalized_address = any(exact_normalized_address),
    nearest_assessor_distance_ft = suppressWarnings(min(
      assessor_distance_ft,
      na.rm = TRUE
    )),
    represented_in_preferred_ledger = any(represented_in_preferred_ledger),
    represented_project_ids = paste(
      sort(unique(stats::na.omit(represented_project_ids))),
      collapse = "/"
    ),
    assessor_addresses = paste(
      sort(unique(stats::na.omit(assessor_address))),
      collapse = "/"
    ),
    address_sources = paste(
      sort(unique(stats::na.omit(address_source))),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    nearest_assessor_distance_ft = dplyr::if_else(
      is.infinite(nearest_assessor_distance_ft),
      NA_real_,
      nearest_assessor_distance_ft
    )
  )

preferred_candidate_row <- evidence_rows |>
  dplyr::arrange(
    permit_chain_id,
    candidate_pin,
    dplyr::desc(evidence_source == "assessor_spatial"),
    dplyr::desc(exact_permit_pin10),
    dplyr::desc(exact_normalized_address),
    dplyr::coalesce(assessor_distance_ft, Inf),
    dplyr::desc(candidate_tax_year),
    candidate_row_id
  ) |>
  dplyr::group_by(permit_chain_id, candidate_pin) |>
  dplyr::slice(1L) |>
  dplyr::ungroup() |>
  dplyr::select(
    permit_chain_id,
    candidate_pin,
    candidate_row_id,
    candidate_tax_year,
    candidate_card_num,
    candidate_class,
    candidate_year_built,
    candidate_building_sqft,
    candidate_land_sqft,
    candidate_units,
    candidate_family_type,
    candidate_residence_type,
    candidate_tieback_group,
    candidate_history_year_values,
    candidate_coordinate_source,
    candidate_review_category,
    candidate_mechanical_status
  )

chain_candidate_counts <- candidate_evidence |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    represented_candidate_pins = sum(represented_in_preferred_ledger),
    unrepresented_candidate_pins = sum(!represented_in_preferred_ledger),
    unrepresented_exact_pin_candidates = sum(
      !represented_in_preferred_ledger & exact_permit_pin10
    ),
    unrepresented_exact_address_candidates = sum(
      !represented_in_preferred_ledger & exact_normalized_address
    ),
    unrepresented_within_50ft_candidates = sum(
      !represented_in_preferred_ledger &
        !is.na(nearest_assessor_distance_ft) &
        nearest_assessor_distance_ft <= 50
    ),
    .groups = "drop"
  )

source_project_map <- source_projects |>
  dplyr::select(
    source_project_id = project_id,
    component_pins,
    project_kind,
    source_project_construction_year = construction_year,
    source_project_units = dwelling_units,
    source_project_building_sqft = building_sqft,
    source_project_land_sqft = land_sqft,
    source_project_status = candidate_status,
    source_project_decision_reason = decision_reason
  ) |>
  tidyr::separate_rows(component_pins, sep = "/") |>
  dplyr::mutate(candidate_pin = normalize_pin(component_pins)) |>
  dplyr::filter(!is.na(candidate_pin)) |>
  dplyr::group_by(candidate_pin) |>
  dplyr::summarise(
    source_project_ids = paste(sort(unique(source_project_id)), collapse = "/"),
    source_project_kinds = paste(sort(unique(project_kind)), collapse = "/"),
    source_project_years = paste(
      sort(unique(source_project_construction_year)),
      collapse = "/"
    ),
    source_project_statuses = paste(sort(unique(source_project_status)), collapse = "/"),
    source_project_decision_reasons = paste(
      sort(unique(source_project_decision_reason)),
      collapse = " | "
    ),
    .groups = "drop"
  )

episode_evidence <- candidate_evidence |>
  dplyr::inner_join(
    preferred_candidate_row,
    by = c("permit_chain_id", "candidate_pin"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    chain_candidate_counts,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    chains,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    source_project_map,
    by = "candidate_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    strong_exact_pin = !represented_in_preferred_ledger &
      exact_permit_pin10 &
      unrepresented_exact_pin_candidates == 1L,
    strong_exact_address = !represented_in_preferred_ledger &
      exact_normalized_address &
      unrepresented_exact_address_candidates == 1L,
    strong_unique_within_50ft = !represented_in_preferred_ledger &
      !is.na(nearest_assessor_distance_ft) &
      nearest_assessor_distance_ft <= 50 &
      unrepresented_within_50ft_candidates == 1L &
      represented_candidate_pins == 0L,
    strong_completion_episode_evidence = strong_exact_pin |
      strong_exact_address |
      strong_unique_within_50ft,
    completion_year_start = as.Date(paste0(candidate_year_built, "-01-01")),
    completion_year_end = as.Date(paste0(candidate_year_built, "-12-31")),
    completion_date_proxy = as.Date(paste0(candidate_year_built, "-06-15")),
    completion_date_precision = "assessor_construction_year_interval",
    representative_application_to_issue_days = as.numeric(
      representative_issue_date - representative_application_date
    ),
    earliest_application_to_earliest_issue_days = as.numeric(
      earliest_issue_date - earliest_application_date
    ),
    earliest_application_to_latest_issue_days = as.numeric(
      latest_issue_date - earliest_application_date
    ),
    earliest_application_to_completion_min_days = as.numeric(
      completion_year_start - earliest_application_date
    ),
    earliest_application_to_completion_max_days = as.numeric(
      completion_year_end - earliest_application_date
    ),
    earliest_issue_to_completion_min_days = as.numeric(
      completion_year_start - earliest_issue_date
    ),
    earliest_issue_to_completion_max_days = as.numeric(
      completion_year_end - earliest_issue_date
    ),
    earliest_application_to_completion_proxy_days = as.numeric(
      completion_date_proxy - earliest_application_date
    ),
    timeline_order_status = dplyr::case_when(
      candidate_year_built < lubridate::year(earliest_application_date) ~
        "reported_construction_year_precedes_application",
      candidate_year_built < lubridate::year(latest_issue_date) ~
        "reported_construction_year_precedes_latest_issue",
      candidate_year_built == lubridate::year(earliest_application_date) ~
        "application_and_reported_construction_same_year",
      TRUE ~ "reported_construction_after_application"
    ),
    episode_review_priority = dplyr::case_when(
      represented_in_preferred_ledger ~ "already_represented",
      strong_completion_episode_evidence ~ "strong_unrepresented_episode",
      exact_permit_pin10 | exact_normalized_address ~
        "manual_multiple_exact_candidates",
      !is.na(nearest_assessor_distance_ft) &
        nearest_assessor_distance_ft <= 50 ~
        "manual_multiple_nearby_candidates",
      TRUE ~ "lower_confidence_unrepresented_candidate"
    )
  ) |>
  dplyr::arrange(
    factor(
      episode_review_priority,
      levels = c(
        "strong_unrepresented_episode",
        "manual_multiple_exact_candidates",
        "manual_multiple_nearby_candidates",
        "lower_confidence_unrepresented_candidate",
        "already_represented"
      )
    ),
    application_boundary_distance_ft,
    earliest_application_date,
    permit_chain_id,
    candidate_pin
  )

strong_episodes <- episode_evidence |>
  dplyr::filter(episode_review_priority == "strong_unrepresented_episode")

manual_episodes <- episode_evidence |>
  dplyr::filter(
    episode_review_priority %in% c(
      "manual_multiple_exact_candidates",
      "manual_multiple_nearby_candidates",
      "lower_confidence_unrepresented_candidate"
    )
  )

chain_permit_rows <- permits |>
  dplyr::filter(permit_chain_id %in% strong_episodes$permit_chain_id) |>
  dplyr::select(
    permit_chain_id,
    permit_id,
    permit_number,
    application_date,
    issue_date,
    permit_status,
    permit_issued,
    permit_address,
    pin,
    reported_cost,
    max_unit_mention,
    residential_signal,
    full_residential_signal,
    temporary_signal,
    revision_signal,
    foundation_signal,
    work_description
  ) |>
  dplyr::arrange(permit_chain_id, application_date, issue_date, permit_number) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    permit_rows = list(dplyr::pick(dplyr::everything())),
    .groups = "drop"
  )

strong_episode_permit_detail <- strong_episodes |>
  dplyr::select(
    permit_chain_id,
    candidate_pin,
    candidate_year_built,
    candidate_units,
    candidate_building_sqft,
    candidate_land_sqft,
    evidence_sources,
    exact_permit_pin10,
    exact_normalized_address,
    nearest_assessor_distance_ft,
    source_project_ids,
    source_project_statuses,
    application_ward,
    application_neighbor_ward,
    application_ward_pair,
    application_boundary_distance_ft
  ) |>
  dplyr::left_join(
    chain_permit_rows,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(permit_rows) |>
  dplyr::mutate(
    completion_year_start = as.Date(paste0(candidate_year_built, "-01-01")),
    completion_year_end = as.Date(paste0(candidate_year_built, "-12-31")),
    permit_application_to_issue_days = as.numeric(issue_date - application_date),
    permit_application_to_completion_min_days = as.numeric(
      completion_year_start - application_date
    ),
    permit_application_to_completion_max_days = as.numeric(
      completion_year_end - application_date
    ),
    permit_issue_to_completion_min_days = as.numeric(
      completion_year_start - issue_date
    ),
    permit_issue_to_completion_max_days = as.numeric(
      completion_year_end - issue_date
    ),
    completion_date_precision = "assessor_construction_year_interval",
    timeline_order_status = dplyr::case_when(
      candidate_year_built < lubridate::year(application_date) ~
        "reported_construction_year_precedes_application",
      candidate_year_built < lubridate::year(issue_date) ~
        "reported_construction_year_precedes_issue",
      candidate_year_built == lubridate::year(application_date) ~
        "application_and_reported_construction_same_year",
      TRUE ~ "reported_construction_after_application"
    )
  ) |>
  dplyr::arrange(
    permit_chain_id,
    candidate_pin,
    application_date,
    issue_date,
    permit_number
  )

summary <- dplyr::bind_rows(
  tibble::tibble(metric = "chain_candidate_episode_evidence_rows", value = nrow(episode_evidence)),
  tibble::tibble(metric = "strong_unrepresented_chain_pin_episodes", value = nrow(strong_episodes)),
  tibble::tibble(
    metric = "strong_unrepresented_unique_candidate_pins",
    value = dplyr::n_distinct(strong_episodes$candidate_pin)
  ),
  tibble::tibble(
    metric = "strong_unrepresented_permit_chains",
    value = dplyr::n_distinct(strong_episodes$permit_chain_id)
  ),
  tibble::tibble(
    metric = "strong_episode_permit_detail_rows",
    value = nrow(strong_episode_permit_detail)
  ),
  episode_evidence |>
    dplyr::count(episode_review_priority, name = "value") |>
    dplyr::transmute(metric = paste0("episode_priority:", episode_review_priority), value),
  strong_episodes |>
    dplyr::count(timeline_order_status, name = "value") |>
    dplyr::transmute(metric = paste0("strong_timeline_order:", timeline_order_status), value)
)

readr::write_csv(
  episode_evidence,
  "../output/permit_completion_episode_evidence.csv",
  na = ""
)
readr::write_csv(
  strong_episodes,
  "../output/permit_completion_episode_strong_queue.csv",
  na = ""
)
readr::write_csv(
  manual_episodes,
  "../output/permit_completion_episode_manual_queue.csv",
  na = ""
)
readr::write_csv(
  strong_episode_permit_detail,
  "../output/permit_completion_episode_permit_detail.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_completion_episode_summary.csv",
  na = ""
)
