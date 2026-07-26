# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin <- function(x) {
  digits <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
  dplyr::if_else(stringr::str_length(digits) == 14L, digits, NA_character_)
}

episode_evidence <- readr::read_csv(
  "../output/permit_completion_episode_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    candidate_pin = readr::col_character(),
    candidate_row_id = readr::col_character(),
    permit_numbers = readr::col_character(),
    source_project_ids = readr::col_character(),
    candidate_tieback_group = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    !represented_in_preferred_ledger,
    episode_review_priority %in% c(
      "strong_unrepresented_episode",
      "manual_multiple_exact_candidates",
      "manual_multiple_nearby_candidates"
    )
  )
candidate_inventory <- readr::read_csv(
  "../output/residential_project_candidate_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    row_id = readr::col_character(),
    class = readr::col_character(),
    tieback_group = readr::col_character(),
    tieback_lineage_id = readr::col_character(),
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
final_ledger <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_project_ids = readr::col_character(),
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
assessor_history <- readr::read_csv(
  "../input/residential_improvement_characteristics_full.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

if (anyDuplicated(candidate_inventory$row_id)) {
  stop("Candidate inventory row IDs must be unique.", call. = FALSE)
}
if (anyDuplicated(final_ledger$project_id)) {
  stop("Final project ledger IDs must be unique.", call. = FALSE)
}

chain_assignment <- episode_evidence |>
  dplyr::mutate(
    evidence_rank = dplyr::case_when(
      exact_normalized_address ~ 3L,
      exact_permit_pin10 ~ 2L,
      !is.na(nearest_assessor_distance_ft) &
        nearest_assessor_distance_ft <= 50 ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::mutate(
    best_evidence_rank = max(evidence_rank),
    candidates_at_best_evidence_rank = sum(evidence_rank == best_evidence_rank),
    deterministic_chain_candidate = evidence_rank == best_evidence_rank &
      candidates_at_best_evidence_rank == 1L &
      best_evidence_rank >= 2L,
    chain_assignment_status = dplyr::case_when(
      deterministic_chain_candidate & exact_normalized_address ~
        "unique_exact_address_candidate",
      deterministic_chain_candidate & exact_permit_pin10 ~
        "unique_exact_permit_pin10_candidate",
      evidence_rank < best_evidence_rank ~ "lower_rank_candidate",
      best_evidence_rank >= 2L ~ "multiple_candidates_with_equal_exact_evidence",
      best_evidence_rank == 1L ~ "nearby_candidate_requires_manual_review",
      TRUE ~ "no_deterministic_candidate"
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    permit_chain_id,
    dplyr::desc(evidence_rank),
    nearest_assessor_distance_ft,
    candidate_pin
  )

selected_chain_candidates <- chain_assignment |>
  dplyr::filter(deterministic_chain_candidate) |>
  dplyr::select(
    permit_chain_id,
    selected_candidate_pin = candidate_pin,
    chain_assignment_status
  )

chain_assignment_summary <- chain_assignment |>
  dplyr::filter(evidence_rank == best_evidence_rank) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    chain_assignment_status = dplyr::case_when(
      any(deterministic_chain_candidate & exact_normalized_address) ~
        "unique_exact_address_candidate",
      any(deterministic_chain_candidate & exact_permit_pin10) ~
        "unique_exact_permit_pin10_candidate",
      first(best_evidence_rank) >= 2L ~
        "multiple_candidates_with_equal_exact_evidence",
      first(best_evidence_rank) == 1L ~
        "nearby_candidate_requires_manual_review",
      TRUE ~ "no_deterministic_candidate"
    ),
    .groups = "drop"
  )

candidate_rows <- episode_evidence |>
  dplyr::select(candidate_pin, candidate_row_id) |>
  dplyr::distinct() |>
  dplyr::left_join(
    candidate_inventory |>
      dplyr::select(
        candidate_row_id = row_id,
        coordinate_x_3435,
        coordinate_y_3435,
        coordinate_source
      ),
    by = "candidate_row_id",
    relationship = "many-to-one"
  ) |>
  dplyr::arrange(
    candidate_pin,
    dplyr::desc(is.finite(coordinate_x_3435) & is.finite(coordinate_y_3435)),
    candidate_row_id
  ) |>
  dplyr::group_by(candidate_pin) |>
  dplyr::slice(1L) |>
  dplyr::ungroup()

if (anyDuplicated(candidate_rows$candidate_pin)) {
  stop("Candidate episode coordinates must be unique by PIN.", call. = FALSE)
}

candidate_points <- candidate_rows |>
  dplyr::filter(is.finite(coordinate_x_3435), is.finite(coordinate_y_3435)) |>
  sf::st_as_sf(
    coords = c("coordinate_x_3435", "coordinate_y_3435"),
    crs = 3435,
    remove = FALSE
  )
final_points <- sf::st_read(
  "../output/preferred_new_construction_project_centroids.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::select(final_project_id = project_id)

nearby_final_rows <- sf::st_is_within_distance(
  candidate_points,
  final_points,
  dist = 250
)
nearby_final_index <- tibble::tibble(
  candidate_row = rep(seq_along(nearby_final_rows), lengths(nearby_final_rows)),
  final_row = unlist(nearby_final_rows, use.names = FALSE)
)

nearby_final_projects <- nearby_final_index |>
  dplyr::mutate(
    candidate_pin = candidate_points$candidate_pin[candidate_row],
    nearby_final_project_id = final_points$final_project_id[final_row],
    nearby_final_project_distance_ft = as.numeric(sf::st_distance(
      candidate_points[candidate_row, ],
      final_points[final_row, ],
      by_element = TRUE
    ))
  ) |>
  dplyr::select(-candidate_row, -final_row) |>
  dplyr::left_join(
    final_ledger |>
      dplyr::select(
        nearby_final_project_id = project_id,
        nearby_final_source_family = source_family,
        nearby_final_project_kind = project_kind,
        nearby_final_construction_year = construction_year,
        nearby_final_dwelling_units = dwelling_units,
        nearby_final_building_sqft = building_sqft,
        nearby_final_land_sqft = land_sqft,
        nearby_final_source_addresses = source_addresses,
        nearby_final_component_pins = component_pins
      ),
    by = "nearby_final_project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::arrange(
    candidate_pin,
    nearby_final_project_distance_ft,
    nearby_final_project_id
  )

nearby_final_summary <- nearby_final_projects |>
  dplyr::group_by(candidate_pin) |>
  dplyr::summarise(
    final_projects_within_250ft = dplyr::n_distinct(nearby_final_project_id),
    nearest_final_project_id = first(nearby_final_project_id),
    nearest_final_project_distance_ft = first(nearby_final_project_distance_ft),
    nearest_final_project_family = first(nearby_final_source_family),
    nearest_final_project_kind = first(nearby_final_project_kind),
    nearest_final_project_construction_year = first(nearby_final_construction_year),
    nearest_final_project_units = first(nearby_final_dwelling_units),
    nearest_final_project_building_sqft = first(nearby_final_building_sqft),
    nearest_final_project_component_pins = first(nearby_final_component_pins),
    nearby_final_project_ids = paste(
      sort(unique(nearby_final_project_id)),
      collapse = "/"
    ),
    .groups = "drop"
  )

candidate_context <- episode_evidence |>
  dplyr::group_by(candidate_pin) |>
  dplyr::summarise(
    permit_chains = paste(sort(unique(permit_chain_id)), collapse = "/"),
    permit_chain_count = dplyr::n_distinct(permit_chain_id),
    deterministic_permit_chains = sum(
      permit_chain_id %in% selected_chain_candidates$permit_chain_id[
        selected_chain_candidates$selected_candidate_pin == first(candidate_pin)
      ]
    ),
    representative_addresses = paste(
      sort(unique(representative_address)),
      collapse = "/"
    ),
    permit_numbers = paste(sort(unique(permit_numbers)), collapse = "/"),
    candidate_row_ids = paste(sort(unique(candidate_row_id)), collapse = "/"),
    candidate_tax_years = paste(sort(unique(candidate_tax_years)), collapse = "/"),
    candidate_card_nums = paste(sort(unique(candidate_card_nums)), collapse = "/"),
    candidate_class = first(candidate_class),
    candidate_year_built = first(candidate_year_built),
    candidate_building_sqft = first(candidate_building_sqft),
    candidate_land_sqft = first(candidate_land_sqft),
    candidate_units = first(candidate_units),
    candidate_family_type = dplyr::first(
      candidate_family_type[!is.na(candidate_family_type)],
      default = NA_character_
    ),
    candidate_residence_type = dplyr::first(
      candidate_residence_type[!is.na(candidate_residence_type)],
      default = NA_character_
    ),
    candidate_tieback_group = dplyr::first(
      candidate_tieback_group[!is.na(candidate_tieback_group)],
      default = NA_character_
    ),
    evidence_sources = paste(sort(unique(evidence_sources)), collapse = "/"),
    any_exact_permit_pin10 = any(exact_permit_pin10),
    any_exact_normalized_address = any(exact_normalized_address),
    nearest_assessor_distance_ft = suppressWarnings(min(
      nearest_assessor_distance_ft,
      na.rm = TRUE
    )),
    source_project_ids = paste(sort(unique(source_project_ids)), collapse = "/"),
    source_project_kinds = paste(sort(unique(source_project_kinds)), collapse = "/"),
    source_project_years = paste(sort(unique(source_project_years)), collapse = "/"),
    source_project_statuses = paste(sort(unique(source_project_statuses)), collapse = "/"),
    source_project_decision_reasons = paste(
      sort(unique(source_project_decision_reasons)),
      collapse = " | "
    ),
    earliest_application_date = min(earliest_application_date),
    earliest_issue_date = min(earliest_issue_date),
    latest_issue_date = max(latest_issue_date),
    application_ward_pairs = paste(sort(unique(application_ward_pair)), collapse = "/"),
    minimum_application_boundary_distance_ft = min(application_boundary_distance_ft),
    timeline_order_statuses = paste(sort(unique(timeline_order_status)), collapse = "/"),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    nearest_assessor_distance_ft = dplyr::if_else(
      is.infinite(nearest_assessor_distance_ft),
      NA_real_,
      nearest_assessor_distance_ft
    ),
    completion_year_start = as.Date(paste0(candidate_year_built, "-01-01")),
    completion_year_end = as.Date(paste0(candidate_year_built, "-12-31")),
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
    completion_date_precision = "assessor_construction_year_interval"
  ) |>
  dplyr::left_join(
    candidate_rows |>
      dplyr::select(
        candidate_pin,
        coordinate_x_3435,
        coordinate_y_3435,
        coordinate_source
      ),
    by = "candidate_pin",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    nearby_final_summary,
    by = "candidate_pin",
    relationship = "one-to-one"
  ) |>
  dplyr::arrange(
    minimum_application_boundary_distance_ft,
    earliest_application_date,
    candidate_pin
  )

source_group_members <- source_projects |>
  dplyr::filter(project_id %in% unique(episode_evidence$source_project_ids)) |>
  dplyr::select(source_project_id = project_id, component_pins) |>
  tidyr::separate_rows(component_pins, sep = "/") |>
  dplyr::mutate(group_member_pin = normalize_pin(component_pins)) |>
  dplyr::filter(!is.na(group_member_pin)) |>
  dplyr::distinct(source_project_id, group_member_pin)

candidate_source_groups <- episode_evidence |>
  dplyr::select(candidate_pin, source_project_ids) |>
  dplyr::distinct() |>
  tidyr::separate_rows(source_project_ids, sep = "/") |>
  dplyr::rename(source_project_id = source_project_ids) |>
  dplyr::filter(!is.na(source_project_id), source_project_id != "")

candidate_group_members <- candidate_source_groups |>
  dplyr::left_join(
    source_group_members |>
      dplyr::group_by(source_project_id) |>
      dplyr::summarise(
        group_members = list(dplyr::pick(dplyr::everything())),
        .groups = "drop"
      ),
    by = "source_project_id",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(group_members) |>
  dplyr::distinct(candidate_pin, source_project_id, group_member_pin)

group_assessor_history <- candidate_group_members |>
  dplyr::left_join(
    assessor_history |>
      dplyr::mutate(group_member_pin = normalize_pin(pin)) |>
      dplyr::select(-pin) |>
      dplyr::group_by(group_member_pin) |>
      dplyr::summarise(
        history_rows = list(dplyr::pick(dplyr::everything())),
        .groups = "drop"
      ),
    by = "group_member_pin",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(history_rows) |>
  dplyr::arrange(
    candidate_pin,
    source_project_id,
    group_member_pin,
    year,
    card,
    row_id
  )

selected_episode_permits <- selected_chain_candidates |>
  dplyr::left_join(
    permits |>
      dplyr::filter(permit_chain_id %in% selected_chain_candidates$permit_chain_id) |>
      dplyr::arrange(permit_chain_id, application_date, issue_date, permit_number) |>
      dplyr::group_by(permit_chain_id) |>
      dplyr::summarise(
        permit_rows = list(dplyr::pick(dplyr::everything())),
        .groups = "drop"
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  tidyr::unnest(permit_rows) |>
  dplyr::left_join(
    candidate_context |>
      dplyr::select(
        selected_candidate_pin = candidate_pin,
        candidate_year_built,
        completion_year_start,
        completion_year_end,
        completion_date_precision
      ),
    by = "selected_candidate_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
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
    )
  ) |>
  dplyr::arrange(
    selected_candidate_pin,
    application_date,
    issue_date,
    permit_number
  )

summary <- dplyr::bind_rows(
  tibble::tibble(metric = "candidate_pins_under_episode_review", value = nrow(candidate_context)),
  tibble::tibble(
    metric = "permit_chains_with_unique_exact_assignment",
    value = nrow(selected_chain_candidates)
  ),
  tibble::tibble(
    metric = "candidate_pins_with_unique_exact_permit_assignment",
    value = dplyr::n_distinct(selected_chain_candidates$selected_candidate_pin)
  ),
  chain_assignment_summary |>
    dplyr::count(chain_assignment_status, name = "value") |>
    dplyr::transmute(metric = paste0("chain_assignment:", chain_assignment_status), value),
  tibble::tibble(
    metric = "candidate_pins_with_final_project_within_50ft",
    value = sum(candidate_context$nearest_final_project_distance_ft <= 50, na.rm = TRUE)
  ),
  tibble::tibble(
    metric = "candidate_pins_with_final_project_within_250ft",
    value = sum(candidate_context$nearest_final_project_distance_ft <= 250, na.rm = TRUE)
  )
)

readr::write_csv(
  chain_assignment,
  "../output/permit_completion_episode_chain_assignment.csv",
  na = ""
)
readr::write_csv(
  candidate_context,
  "../output/permit_completion_episode_candidate_context.csv",
  na = ""
)
readr::write_csv(
  nearby_final_projects,
  "../output/permit_completion_episode_nearby_final_projects.csv",
  na = ""
)
readr::write_csv(
  group_assessor_history,
  "../output/permit_completion_episode_assessor_history.csv",
  na = ""
)
readr::write_csv(
  selected_episode_permits,
  "../output/permit_completion_episode_selected_permits.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_completion_episode_context_summary.csv",
  na = ""
)
