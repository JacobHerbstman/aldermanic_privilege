# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin10 <- function(x) {
  digits <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
  dplyr::if_else(stringr::str_length(digits) >= 10, stringr::str_sub(digits, 1, 10), NA_character_)
}

chains <- readr::read_csv(
  "../output/permit_first_unmatched_residential_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    permit_numbers = readr::col_character(),
    .default = readr::col_guess()
  )
)
permits <- readr::read_csv(
  "../output/permit_first_permit_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    permit_chain_id = readr::col_character(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
candidates <- readr::read_csv(
  "../output/residential_project_candidate_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    row_id = readr::col_character(),
    class = readr::col_character(),
    .default = readr::col_guess()
  )
)
components <- readr::read_csv(
  "../output/preferred_new_construction_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
candidate_projects <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)
final_residential_ledger <- readr::read_csv(
  "../output/preferred_residential_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)
suppressions <- readr::read_csv(
  "../adjudication/residential_candidate_suppressions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_project_id = readr::col_character(),
    replacement_project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
additional_decisions <- readr::read_csv(
  "../adjudication/residential_additional_candidate_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_project_id = readr::col_character(),
    replacement_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)
review_dispositions <- readr::read_csv(
  "../output/residential_review_source_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    final_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)
unresolved_dispositions <- readr::read_csv(
  "../adjudication/residential_unresolved_source_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    final_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(chains$permit_chain_id)) {
  stop("Residual permit chains must be unique.", call. = FALSE)
}
if (anyDuplicated(candidates$row_id)) {
  stop("Residential assessor candidate row IDs must be unique.", call. = FALSE)
}

directly_represented_pins <- components |>
  dplyr::transmute(
    candidate_pin = stringr::str_replace_all(component_pin, "[^0-9]", ""),
    represented_project_id = project_id
  ) |>
  dplyr::filter(candidate_pin != "")

source_representation <- dplyr::bind_rows(
  final_residential_ledger |>
    dplyr::select(represented_project_id = project_id, source_project_ids) |>
    tidyr::separate_rows(source_project_ids, sep = "/") |>
    dplyr::transmute(
      source_project_id = source_project_ids,
      represented_project_id
    ),
  suppressions |>
    dplyr::transmute(
      source_project_id = candidate_project_id,
      represented_project_id = replacement_project_id
    ),
  additional_decisions |>
    dplyr::filter(
      decision %in% c("suppress_duplicate", "replace_by_commercial"),
      !is.na(replacement_project_ids),
      replacement_project_ids != ""
    ) |>
    dplyr::select(
      source_project_id = candidate_project_id,
      represented_project_ids = replacement_project_ids
    ) |>
    tidyr::separate_rows(represented_project_ids, sep = "/") |>
    dplyr::rename(represented_project_id = represented_project_ids),
  review_dispositions |>
    dplyr::filter(!is.na(final_project_ids), final_project_ids != "") |>
    dplyr::select(source_project_id, represented_project_ids = final_project_ids) |>
    tidyr::separate_rows(represented_project_ids, sep = "/") |>
    dplyr::rename(represented_project_id = represented_project_ids),
  unresolved_dispositions |>
    dplyr::filter(!is.na(final_project_ids), final_project_ids != "") |>
    dplyr::select(source_project_id, represented_project_ids = final_project_ids) |>
    tidyr::separate_rows(represented_project_ids, sep = "/") |>
    dplyr::rename(represented_project_id = represented_project_ids)
) |>
  dplyr::filter(
    !is.na(source_project_id),
    source_project_id != "",
    !is.na(represented_project_id),
    represented_project_id != ""
  ) |>
  dplyr::distinct(source_project_id, represented_project_id) |>
  dplyr::group_by(source_project_id) |>
  dplyr::summarise(
    represented_project_ids = list(sort(unique(represented_project_id))),
    .groups = "drop"
  )

candidate_pin_source_map <- candidate_projects |>
  dplyr::select(source_project_id = project_id, component_pins) |>
  tidyr::separate_rows(component_pins, sep = "/") |>
  dplyr::transmute(
    candidate_pin = stringr::str_replace_all(component_pins, "[^0-9]", ""),
    source_project_id
  ) |>
  dplyr::filter(candidate_pin != "") |>
  dplyr::distinct(candidate_pin, source_project_id) |>
  dplyr::inner_join(
    source_representation,
    by = "source_project_id",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest_longer(
    represented_project_ids,
    values_to = "represented_project_id"
  ) |>
  dplyr::select(candidate_pin, represented_project_id)

represented_pins <- dplyr::bind_rows(
  directly_represented_pins,
  candidate_pin_source_map
) |>
  dplyr::distinct(candidate_pin, represented_project_id) |>
  dplyr::group_by(candidate_pin) |>
  dplyr::summarise(
    represented_project_ids = paste(sort(unique(represented_project_id)), collapse = "/"),
    .groups = "drop"
  )

chain_pin10 <- permits |>
  dplyr::filter(permit_chain_id %in% chains$permit_chain_id) |>
  dplyr::select(permit_chain_id, pin) |>
  tidyr::separate_rows(pin, sep = "\\s*\\|\\s*") |>
  dplyr::mutate(permit_pin10 = normalize_pin10(pin)) |>
  dplyr::filter(!is.na(permit_pin10)) |>
  dplyr::distinct(permit_chain_id, permit_pin10) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    permit_pin10s = paste(sort(unique(permit_pin10)), collapse = "/"),
    .groups = "drop"
  )

chain_points <- sf::st_as_sf(
  chains,
  coords = c("representative_x_3435", "representative_y_3435"),
  crs = 3435,
  remove = FALSE
)
candidate_points <- candidates |>
  dplyr::filter(
    is.finite(coordinate_x_3435),
    is.finite(coordinate_y_3435),
    dplyr::between(year_built, 2006L, 2022L),
    is.finite(building_sqft),
    building_sqft > 0
  ) |>
  sf::st_as_sf(
    coords = c("coordinate_x_3435", "coordinate_y_3435"),
    crs = 3435,
    remove = FALSE
  )

nearby_candidate_rows <- sf::st_is_within_distance(
  chain_points,
  candidate_points,
  dist = 250
)
nearby_index <- tibble::tibble(
  chain_row = rep(seq_along(nearby_candidate_rows), lengths(nearby_candidate_rows)),
  candidate_row = unlist(nearby_candidate_rows, use.names = FALSE)
)

candidate_links <- nearby_index |>
  dplyr::mutate(
    permit_chain_id = chain_points$permit_chain_id[chain_row],
    candidate_row_id = candidate_points$row_id[candidate_row],
    distance_ft = as.numeric(sf::st_distance(
      chain_points[chain_row, ],
      candidate_points[candidate_row, ],
      by_element = TRUE
    ))
  ) |>
  dplyr::select(-chain_row, -candidate_row) |>
  dplyr::left_join(
    chains |>
      dplyr::select(
        permit_chain_id,
        representative_permit_number,
        representative_application_date,
        representative_issue_date,
        representative_address,
        representative_description,
        maximum_unit_mention,
        any_single_family_signal,
        application_ward_pair,
        application_boundary_distance_ft,
        review_priority
      ),
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    candidate_points |>
      sf::st_drop_geometry() |>
      dplyr::transmute(
        candidate_row_id = row_id,
        candidate_pin = stringr::str_replace_all(pin, "[^0-9]", ""),
        candidate_tax_year = tax_year,
        candidate_card_num = card_num,
        candidate_class = class,
        candidate_year_built = year_built,
        candidate_building_sqft = building_sqft,
        candidate_land_sqft = land_sqft,
        candidate_units = num_apartments,
        candidate_family_type = single_v_multi_family,
        candidate_residence_type = type_of_residence,
        candidate_tieback_group = tieback_group,
        candidate_history_year_values = history_year_values,
        candidate_coordinate_source = coordinate_source,
        candidate_review_category = review_category,
        candidate_mechanical_status = mechanical_status
      ),
    by = "candidate_row_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    represented_pins,
    by = "candidate_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    chain_pin10,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    application_year = lubridate::year(representative_application_date),
    construction_year_gap = candidate_year_built - application_year,
    plausible_construction_year = dplyr::between(construction_year_gap, -1L, 6L),
    candidate_pin10 = normalize_pin10(candidate_pin),
    exact_permit_pin10 = purrr::map2_lgl(
      candidate_pin10,
      dplyr::coalesce(permit_pin10s, ""),
      ~ !is.na(.x) && .y != "" && .x %in% stringr::str_split_1(.y, "/")
    ),
    represented_in_preferred_ledger = !is.na(represented_project_ids)
  ) |>
  dplyr::filter(plausible_construction_year) |>
  dplyr::arrange(
    permit_chain_id,
    dplyr::desc(exact_permit_pin10),
    distance_ft,
    abs(construction_year_gap),
    candidate_pin,
    candidate_card_num
  ) |>
  dplyr::group_by(permit_chain_id, candidate_pin) |>
  dplyr::mutate(candidate_pin_cards = dplyr::n_distinct(candidate_card_num)) |>
  dplyr::slice(1L) |>
  dplyr::ungroup()

chain_candidate_summary <- candidate_links |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    plausible_candidate_pins = dplyr::n_distinct(candidate_pin),
    represented_candidate_pins = dplyr::n_distinct(
      candidate_pin[represented_in_preferred_ledger]
    ),
    unrepresented_candidate_pins = dplyr::n_distinct(
      candidate_pin[!represented_in_preferred_ledger]
    ),
    exact_permit_pin_candidates = sum(exact_permit_pin10),
    nearest_candidate_distance_ft = min(distance_ft),
    nearest_unrepresented_distance_ft = suppressWarnings(min(
      distance_ft[!represented_in_preferred_ledger],
      na.rm = TRUE
    )),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    nearest_unrepresented_distance_ft = dplyr::if_else(
      is.infinite(nearest_unrepresented_distance_ft),
      NA_real_,
      nearest_unrepresented_distance_ft
    )
  )

chain_review <- chains |>
  dplyr::left_join(
    chain_candidate_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        plausible_candidate_pins,
        represented_candidate_pins,
        unrepresented_candidate_pins,
        exact_permit_pin_candidates
      ),
      ~ dplyr::coalesce(.x, 0L)
    ),
    assessor_review_status = dplyr::case_when(
      exact_permit_pin_candidates == 1L &
        unrepresented_candidate_pins == 1L ~
        "one_unrepresented_exact_pin10_completion_candidate",
      unrepresented_candidate_pins == 1L &
        represented_candidate_pins == 0L &
        nearest_unrepresented_distance_ft <= 50 ~
        "one_unrepresented_completion_candidate_within_50ft",
      unrepresented_candidate_pins > 0L ~
        "manual_review_unrepresented_completion_candidates",
      represented_candidate_pins > 0L ~
        "nearby_candidates_already_represented",
      TRUE ~ "no_nearby_assessor_completion_candidate"
    )
  ) |>
  dplyr::arrange(
    factor(
      assessor_review_status,
      levels = c(
        "one_unrepresented_exact_pin10_completion_candidate",
        "one_unrepresented_completion_candidate_within_50ft",
        "manual_review_unrepresented_completion_candidates",
        "nearby_candidates_already_represented",
        "no_nearby_assessor_completion_candidate"
      )
    ),
    application_boundary_distance_ft,
    permit_chain_id
  )

summary <- dplyr::bind_rows(
  tibble::tibble(metric = "residual_permit_chains", value = nrow(chains)),
  tibble::tibble(
    metric = "plausible_chain_assessor_pin_links",
    value = nrow(candidate_links)
  ),
  chain_review |>
    dplyr::count(assessor_review_status, name = "value") |>
    dplyr::transmute(metric = paste0("chain_status:", assessor_review_status), value)
)

readr::write_csv(
  candidate_links,
  "../output/permit_residential_assessor_candidate_links.csv",
  na = ""
)
readr::write_csv(
  chain_review,
  "../output/permit_residential_assessor_chain_review.csv",
  na = ""
)
readr::write_csv(
  represented_pins,
  "../output/residential_candidate_pin_representation.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_residential_assessor_summary.csv",
  na = ""
)
