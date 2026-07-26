# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

normalize_pin10 <- function(x) {
  digits <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
  dplyr::if_else(stringr::str_length(digits) >= 10, stringr::str_sub(digits, 1, 10), NA_character_)
}

normalize_address <- function(x) {
  out <- stringr::str_to_upper(dplyr::coalesce(as.character(x), ""))
  out <- stringr::str_replace_all(out, "\\bCHICAGO\\b", "")
  out <- stringr::str_replace_all(out, "\\bSTREET\\b", "ST")
  out <- stringr::str_replace_all(out, "\\bAVENUE\\b", "AVE")
  out <- stringr::str_replace_all(out, "\\bBOULEVARD\\b", "BLVD")
  out <- stringr::str_replace_all(out, "\\bROAD\\b", "RD")
  out <- stringr::str_replace_all(out, "\\bDRIVE\\b", "DR")
  out <- stringr::str_replace_all(out, "\\bPLACE\\b", "PL")
  out <- stringr::str_replace_all(out, "\\bCOURT\\b", "CT")
  out <- stringr::str_replace_all(out, "\\bPARKWAY\\b", "PKWY")
  out <- stringr::str_replace_all(out, "[^A-Z0-9 -]", " ")
  stringr::str_squish(out)
}

extract_max_units <- function(text) {
  matches <- stringr::str_match_all(
    stringr::str_to_upper(dplyr::coalesce(text, "")),
    "\\b([0-9]{1,4})\\s*(?:TOTAL\\s+)?(?:DWELLING\\s+|RESIDENTIAL\\s+|APARTMENT\\s+|EFFICIENCY\\s+)?(?:UNITS?|D\\.?U\\.?)\\b"
  )
  vapply(matches, function(m) {
    if (nrow(m) == 0) {
      return(NA_real_)
    }
    max(suppressWarnings(as.numeric(m[, 2])), na.rm = TRUE)
  }, numeric(1))
}

permits_sf <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) |>
  sf::st_transform(3435) |>
  dplyr::filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    !is.na(application_start_date),
    !is.na(issue_date)
  )

permit_coordinates <- sf::st_coordinates(permits_sf)
permits <- permits_sf |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    permit_id = as.character(id),
    permit_number = as.character(permit),
    pin,
    application_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    application_year = lubridate::year(application_date),
    issue_year = lubridate::year(issue_date),
    permit_status,
    permit_issued = as.logical(permit_issued),
    permit_address = stringr::str_squish(paste(street_number, street_direction, street_name)),
    normalized_address = normalize_address(permit_address),
    permit_x_3435 = permit_coordinates[, "X"],
    permit_y_3435 = permit_coordinates[, "Y"],
    reported_cost,
    work_description = dplyr::coalesce(work_description, ""),
    description_upper = stringr::str_to_upper(work_description),
    reference_numbers = purrr::map2(
      stringr::str_extract_all(
        description_upper,
        "(?<![0-9])10[0-9]{7}(?![0-9])"
      ),
      permit_number,
      ~ sort(setdiff(unique(.x), .y))
    )
  ) |>
  dplyr::mutate(
    max_unit_mention = extract_max_units(work_description),
    single_family_signal = stringr::str_detect(
      description_upper,
      "SINGLE[- ]?FAMILY|ONE[- ]?FAMILY|\\bSFR\\b|ONE[- ]?UNIT|1[- ]?UNIT"
    ),
    residential_signal = single_family_signal |
      is.finite(max_unit_mention) |
      stringr::str_detect(
        description_upper,
        "DWELLING|RESIDENTIAL|APARTMENT|EFFICIENCY|MULTI[- ]?FAMILY|CONDOMINIUM|TOWNHOME|TOWNHOUSE|TWO[- ]?FLAT|THREE[- ]?FLAT"
      ),
    temporary_signal = stringr::str_detect(
      description_upper,
      "PERMIT EXPIRES|ERECTION STARTS|HOIST|DERRICK CRANE|TOWER CRANE|SCAFFOLD|TENT|FESTIVAL|TEMPORARY STAGE|ANTENNA"
    ),
    revision_signal = stringr::str_detect(
      description_upper,
      "^\\s*(REVISION|REVISIONS|REVISE)|REVISION TO PERMIT|CHANGE TO PERMIT"
    ),
    foundation_signal = stringr::str_detect(
      description_upper,
      "FOUNDATION ONLY|FOUNDATION PERMIT|CAISSON ONLY|SHELL ONLY"
    ),
    existing_building_signal = stringr::str_detect(
      description_upper,
      "ADDITION TO (AN )?EXISTING|INTERIOR ALTER|INTERIOR REMODEL|BUILD[- ]?OUT|RENOVATION OF (AN )?EXISTING"
    ),
    full_building_signal = stringr::str_detect(
      description_upper,
      "ERECT.{0,35}(BUILDING|RESIDENCE|HOME|SFR)|NEW.{0,35}(BUILDING|RESIDENCE|HOME|SFR)|CONSTRUCT.{0,35}(BUILDING|RESIDENCE|HOME)"
    ),
    full_residential_signal = residential_signal &
      full_building_signal &
      !temporary_signal &
      !revision_signal &
      !foundation_signal &
      !existing_building_signal
  )

if (anyDuplicated(permits$permit_id) || anyDuplicated(permits$permit_number)) {
  stop("New-construction permit IDs and numbers must be unique.", call. = FALSE)
}

permit_edges <- permits |>
  dplyr::select(from = permit_number, reference_numbers) |>
  tidyr::unnest_longer(reference_numbers, values_to = "to") |>
  dplyr::filter(!is.na(to), to != "", to %in% permits$permit_number) |>
  dplyr::distinct(from, to)

permit_graph <- igraph::graph_from_data_frame(
  permit_edges,
  directed = FALSE,
  vertices = permits$permit_number
)
permit_components <- tibble::tibble(
  permit_number = names(igraph::components(permit_graph)$membership),
  graph_component = as.integer(igraph::components(permit_graph)$membership)
) |>
  dplyr::group_by(graph_component) |>
  dplyr::mutate(permit_chain_id = paste0("permit_chain_", min(permit_number))) |>
  dplyr::ungroup() |>
  dplyr::select(-graph_component)

permits <- permits |>
  dplyr::left_join(
    permit_components,
    by = "permit_number",
    relationship = "one-to-one"
  )

ward_maps <- load_canonical_ward_maps(
  sf::st_read("../input/ward_panel.gpkg", quiet = TRUE)
)
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

permit_points <- sf::st_as_sf(
  permits,
  coords = c("permit_x_3435", "permit_y_3435"),
  crs = 3435,
  remove = FALSE
)
permit_eras <- canonical_era_from_boundary_year(
  canonical_boundary_year_from_date(permits$application_date)
)
permit_boundary_assignments <- assign_points_to_boundaries(
  permit_points,
  permit_eras,
  ward_maps,
  boundary_lines,
  chunk_n = 5000L
)

alderman_terms <- readr::read_csv(
  "../input/chicago_alderman_terms.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    ward = readr::col_integer(),
    alderman = readr::col_character(),
    start_date = readr::col_date(),
    end_date = readr::col_date()
  )
) |>
  dplyr::arrange(ward, start_date)
if (anyDuplicated(alderman_terms[c("ward", "start_date")])) {
  stop("Alderman terms must be unique by ward and start date.", call. = FALSE)
}
overlapping_terms <- alderman_terms |>
  dplyr::group_by(ward) |>
  dplyr::mutate(next_start_date = dplyr::lead(start_date)) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(next_start_date), next_start_date <= end_date)
if (nrow(overlapping_terms) > 0L) {
  stop("Alderman terms contain overlapping ward-date intervals.", call. = FALSE)
}

permit_date_min <- min(permits$application_date, na.rm = TRUE)
permit_date_max <- max(permits$application_date, na.rm = TRUE)
alderman_daily <- alderman_terms |>
  dplyr::mutate(
    term_start = pmax(start_date, permit_date_min),
    term_end = pmin(end_date, permit_date_max)
  ) |>
  dplyr::filter(term_start <= term_end) |>
  dplyr::mutate(
    permit_application_date = purrr::map2(
      term_start,
      term_end,
      ~ seq(.x, .y, by = "day")
    )
  ) |>
  tidyr::unnest(permit_application_date) |>
  dplyr::select(
    ward,
    permit_application_date,
    permit_application_alderman = alderman
  )
if (anyDuplicated(alderman_daily[c("ward", "permit_application_date")])) {
  stop("Exact-date alderman lookup must be unique by ward-date.", call. = FALSE)
}

permits <- permits |>
  dplyr::bind_cols(
    permit_boundary_assignments |>
      dplyr::rename(
        permit_application_ward = ward,
        permit_application_neighbor_ward = neighbor_ward,
        permit_application_ward_pair = ward_pair_id,
        permit_application_boundary_distance_m = dist_m,
        permit_application_boundary_distance_ft = dist_ft
      )
  ) |>
  dplyr::mutate(permit_application_era = permit_eras) |>
  dplyr::left_join(
    alderman_daily,
    by = c(
      "permit_application_ward" = "ward",
      "application_date" = "permit_application_date"
    ),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    permit_application_alderman_status = dplyr::case_when(
      is.na(permit_application_ward) ~ "ward_unassigned",
      !is.na(permit_application_alderman) ~ "named_alderman",
      dplyr::between(
        application_date,
        min(alderman_terms$start_date),
        max(alderman_terms$end_date)
      ) ~ "vacant_or_term_gap",
      TRUE ~ "outside_alderman_term_panel"
    )
  )

issued_study_permits <- permits |>
  dplyr::filter(
    permit_issued,
    dplyr::between(application_year, 2006L, 2022L)
  )

candidate_chain_ids <- unique(issued_study_permits$permit_chain_id)
chain_catalog <- permits |>
  dplyr::filter(permit_chain_id %in% candidate_chain_ids)

chain_summary <- chain_catalog |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    permit_records = dplyr::n(),
    issued_study_permits = sum(permit_issued & dplyr::between(application_year, 2006L, 2022L)),
    issued_full_residential_permits = sum(
      permit_issued &
        dplyr::between(application_year, 2006L, 2022L) &
        full_residential_signal
    ),
    any_residential_signal = any(residential_signal),
    any_full_residential_signal = any(full_residential_signal),
    any_single_family_signal = any(single_family_signal),
    any_temporary_signal = any(temporary_signal),
    any_revision_signal = any(revision_signal),
    any_foundation_signal = any(foundation_signal),
    maximum_unit_mention = suppressWarnings(max(max_unit_mention, na.rm = TRUE)),
    earliest_application_date = min(application_date),
    earliest_issue_date = min(issue_date),
    latest_issue_date = max(issue_date),
    permit_numbers = paste(sort(unique(permit_number)), collapse = "/"),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    maximum_unit_mention = dplyr::if_else(
      is.infinite(maximum_unit_mention),
      NA_real_,
      maximum_unit_mention
    )
  )

representative_permits <- issued_study_permits |>
  dplyr::filter(permit_chain_id %in% candidate_chain_ids) |>
  dplyr::arrange(
    permit_chain_id,
    dplyr::desc(full_residential_signal),
    dplyr::desc(residential_signal),
    temporary_signal,
    revision_signal,
    foundation_signal,
    application_date,
    permit_number
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::slice(1L) |>
  dplyr::ungroup() |>
  dplyr::select(
    permit_chain_id,
    representative_permit_id = permit_id,
    representative_permit_number = permit_number,
    representative_application_date = application_date,
    representative_issue_date = issue_date,
    representative_status = permit_status,
    representative_address = permit_address,
    representative_normalized_address = normalized_address,
    representative_x_3435 = permit_x_3435,
    representative_y_3435 = permit_y_3435,
    representative_description = work_description,
    application_ward = permit_application_ward,
    application_alderman = permit_application_alderman,
    application_alderman_status = permit_application_alderman_status,
    application_neighbor_ward = permit_application_neighbor_ward,
    application_ward_pair = permit_application_ward_pair,
    application_boundary_distance_m = permit_application_boundary_distance_m,
    application_boundary_distance_ft = permit_application_boundary_distance_ft,
    application_era = permit_application_era
  )

chain_summary <- chain_summary |>
  dplyr::inner_join(
    representative_permits,
    by = "permit_chain_id",
    relationship = "one-to-one"
  )

ledger <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE
)
components <- readr::read_csv(
  "../output/preferred_new_construction_project_components.csv",
  show_col_types = FALSE
)

source_project_map <- ledger |>
  dplyr::select(final_project_id = project_id, source_project_ids) |>
  tidyr::separate_rows(source_project_ids, sep = "/") |>
  dplyr::filter(!is.na(source_project_ids), source_project_ids != "") |>
  dplyr::distinct(source_project_ids, final_project_id) |>
  dplyr::group_by(source_project_ids) |>
  dplyr::summarise(
    final_project_ids = list(sort(unique(final_project_id))),
    .groups = "drop"
  )

existing_chain_matches <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::distinct(source_project_ids = project_id, permit_chain_id) |>
  dplyr::inner_join(
    source_project_map,
    by = "source_project_ids",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest_longer(final_project_ids, values_to = "final_project_id") |>
  dplyr::distinct(permit_chain_id, final_project_id) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    existing_project_candidates = dplyr::n_distinct(final_project_id),
    existing_project_ids = paste(sort(unique(final_project_id)), collapse = "/"),
    .groups = "drop"
  )

existing_chain_project_links <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::distinct(source_project_ids = project_id, permit_chain_id) |>
  dplyr::inner_join(
    source_project_map,
    by = "source_project_ids",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest_longer(final_project_ids, values_to = "matched_project_id") |>
  dplyr::distinct(permit_chain_id, matched_project_id)

unique_pin_map <- components |>
  dplyr::transmute(
    final_project_id = project_id,
    pin10 = stringr::str_sub(component_pin, 1, 10)
  ) |>
  dplyr::filter(!is.na(pin10), pin10 != "") |>
  dplyr::distinct(pin10, final_project_id) |>
  dplyr::group_by(pin10) |>
  dplyr::summarise(
    pin_project_candidates = dplyr::n_distinct(final_project_id),
    pin_project_id = dplyr::if_else(
      pin_project_candidates == 1L,
      first(final_project_id),
      NA_character_
    ),
    .groups = "drop"
  ) |>
  dplyr::filter(pin_project_candidates == 1L)

chain_pin_matches <- chain_catalog |>
  dplyr::select(permit_chain_id, pin) |>
  tidyr::separate_rows(pin, sep = "\\s*\\|\\s*") |>
  dplyr::mutate(pin10 = normalize_pin10(pin)) |>
  dplyr::filter(!is.na(pin10)) |>
  dplyr::distinct(permit_chain_id, pin10) |>
  dplyr::inner_join(
    unique_pin_map,
    by = "pin10",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    pin_project_candidates = dplyr::n_distinct(pin_project_id),
    pin_project_id = dplyr::if_else(
      pin_project_candidates == 1L,
      first(pin_project_id),
      NA_character_
    ),
    .groups = "drop"
  )

unique_address_map <- ledger |>
  dplyr::select(final_project_id = project_id, source_addresses) |>
  tidyr::separate_rows(source_addresses, sep = "/") |>
  dplyr::mutate(normalized_address = normalize_address(source_addresses)) |>
  dplyr::filter(normalized_address != "") |>
  dplyr::distinct(normalized_address, final_project_id) |>
  dplyr::group_by(normalized_address) |>
  dplyr::summarise(
    address_project_candidates = dplyr::n_distinct(final_project_id),
    address_project_id = dplyr::if_else(
      address_project_candidates == 1L,
      first(final_project_id),
      NA_character_
    ),
    .groups = "drop"
  ) |>
  dplyr::filter(address_project_candidates == 1L)

chain_address_matches <- chain_catalog |>
  dplyr::filter(normalized_address != "") |>
  dplyr::distinct(permit_chain_id, normalized_address) |>
  dplyr::inner_join(
    unique_address_map,
    by = "normalized_address",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    address_project_candidates = dplyr::n_distinct(address_project_id),
    address_project_id = dplyr::if_else(
      address_project_candidates == 1L,
      first(address_project_id),
      NA_character_
    ),
    .groups = "drop"
  )

chain_points <- sf::st_as_sf(
  chain_summary,
  coords = c("representative_x_3435", "representative_y_3435"),
  crs = 3435,
  remove = FALSE
)
project_points <- sf::st_read(
  "../output/preferred_new_construction_project_centroids.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::select(final_project_id = project_id)

nearest_project_row <- sf::st_nearest_feature(chain_points, project_points)
nearest_project_distance <- as.numeric(sf::st_distance(
  chain_points,
  project_points[nearest_project_row, ],
  by_element = TRUE
))
nearest_project <- tibble::tibble(
  permit_chain_id = chain_summary$permit_chain_id,
  nearest_project_id = project_points$final_project_id[nearest_project_row],
  nearest_project_distance_ft = nearest_project_distance
)

ledger_fields <- ledger |>
  dplyr::select(
    final_project_id = project_id,
    source_family,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft
  )

chain_matches <- chain_summary |>
  dplyr::left_join(
    existing_chain_matches,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    chain_pin_matches,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    chain_address_matches,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    nearest_project,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    existing_project_candidates = dplyr::coalesce(existing_project_candidates, 0L),
    existing_project_ids = dplyr::coalesce(existing_project_ids, ""),
    candidate_project_ids = purrr::pmap_chr(
      list(pin_project_id, address_project_id),
      ~ paste(sort(unique(stats::na.omit(c(...)))), collapse = "/")
    ),
    strong_project_candidates = stringr::str_count(candidate_project_ids, "/") +
      as.integer(candidate_project_ids != ""),
    pin_conflicts_with_existing = purrr::map2_lgl(
      pin_project_id,
      existing_project_ids,
      ~ !is.na(.x) && .y != "" && !(.x %in% stringr::str_split_1(.y, "/"))
    ),
    address_conflicts_with_existing = purrr::map2_lgl(
      address_project_id,
      existing_project_ids,
      ~ !is.na(.x) && .y != "" && !(.x %in% stringr::str_split_1(.y, "/"))
    ),
    conflicting_strong_matches = pin_conflicts_with_existing |
      address_conflicts_with_existing |
      (existing_project_candidates == 0L & strong_project_candidates > 1L),
    matched_project_ids = dplyr::case_when(
      conflicting_strong_matches ~ "",
      existing_project_candidates > 0L ~ existing_project_ids,
      strong_project_candidates == 1L ~ candidate_project_ids,
      TRUE ~ ""
    ),
    matched_project_candidates = stringr::str_count(matched_project_ids, "/") +
      as.integer(matched_project_ids != ""),
    matched_project_id = dplyr::if_else(
      matched_project_candidates == 1L,
      matched_project_ids,
      NA_character_
    ),
    match_method = dplyr::case_when(
      conflicting_strong_matches ~ "conflicting_strong_matches",
      existing_project_candidates == 1L ~ "existing_permit_chain",
      existing_project_candidates > 1L ~ "existing_permit_chain_multiple_projects",
      !is.na(pin_project_id) ~ "unique_component_pin10",
      !is.na(address_project_id) ~ "unique_normalized_address",
      nearest_project_distance_ft <= 50 ~ "spatial_review_within_50ft",
      nearest_project_distance_ft <= 200 ~ "spatial_review_within_200ft",
      TRUE ~ "unmatched"
    )
  ) |>
  dplyr::left_join(
    ledger_fields,
    by = c("matched_project_id" = "final_project_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    application_year_gap = construction_year -
      lubridate::year(representative_application_date),
    plausible_matched_year = existing_project_candidates > 0L |
      is.na(construction_year) |
      dplyr::between(application_year_gap, -2L, 6L)
  ) |>
  dplyr::mutate(
    within_1500ft_at_application = application_boundary_distance_ft <= 1500,
    review_priority = dplyr::case_when(
      !any_residential_signal ~ "not_residential",
      matched_project_candidates > 0L & plausible_matched_year ~ "matched",
      match_method == "conflicting_strong_matches" ~ "high_conflict",
      any_full_residential_signal &
        within_1500ft_at_application ~ "high_unmatched_full_residential",
      any_residential_signal &
        within_1500ft_at_application &
        nearest_project_distance_ft <= 200 ~ "medium_near_existing_project",
      any_residential_signal &
        within_1500ft_at_application ~ "medium_unmatched_residential",
      any_full_residential_signal ~ "outside_1500ft_full_residential",
      TRUE ~ "outside_1500ft_other_residential"
    )
  )

unmatched_queue <- chain_matches |>
  dplyr::filter(
    review_priority %in% c(
      "high_conflict",
      "high_unmatched_full_residential",
      "medium_near_existing_project",
      "medium_unmatched_residential"
    )
  ) |>
  dplyr::arrange(
    factor(
      review_priority,
      levels = c(
        "high_conflict",
        "high_unmatched_full_residential",
        "medium_near_existing_project",
        "medium_unmatched_residential"
      )
    ),
    application_boundary_distance_ft,
    representative_application_date,
    permit_chain_id
  )

unique_chain_project_links <- chain_matches |>
  dplyr::filter(
    review_priority == "matched",
    existing_project_candidates == 0L,
    matched_project_candidates == 1L
  ) |>
  dplyr::select(permit_chain_id, matched_project_id)

matched_chain_project_links <- dplyr::bind_rows(
  existing_chain_project_links,
  unique_chain_project_links
) |>
  dplyr::distinct(permit_chain_id, matched_project_id)

matched_timelines <- matched_chain_project_links |>
  dplyr::inner_join(
    chain_matches |>
      dplyr::select(
        -matched_project_id,
        -source_family,
        -construction_year,
        -dwelling_units,
        -building_sqft,
        -land_sqft
      ),
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::inner_join(
    ledger_fields,
    by = c("matched_project_id" = "final_project_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::filter(
    review_priority == "matched",
    any_residential_signal
  ) |>
  dplyr::mutate(
    completion_year_start = as.Date(paste0(construction_year, "-01-01")),
    completion_year_end = as.Date(paste0(construction_year, "-12-31")),
    completion_date_proxy = as.Date(paste0(construction_year, "-06-15")),
    application_to_issue_days = as.numeric(
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
    application_to_completion_proxy_days = as.numeric(
      completion_date_proxy - representative_application_date
    ),
    issue_to_completion_proxy_days = as.numeric(
      completion_date_proxy - representative_issue_date
    ),
    completion_date_precision = "assessor_construction_year_interval",
    timeline_order_status = dplyr::case_when(
      construction_year < lubridate::year(earliest_application_date) ~
        "reported_construction_year_precedes_application",
      construction_year < lubridate::year(latest_issue_date) ~
        "reported_construction_year_precedes_latest_issue",
      construction_year == lubridate::year(earliest_application_date) ~
        "application_and_reported_construction_same_year",
      TRUE ~ "reported_construction_after_application"
    )
  ) |>
  dplyr::select(
    matched_project_id,
    source_family,
    permit_chain_id,
    permit_records,
    issued_study_permits,
    issued_full_residential_permits,
    representative_permit_id,
    representative_permit_number,
    earliest_application_date,
    earliest_issue_date,
    representative_application_date,
    representative_issue_date,
    latest_issue_date,
    representative_status,
    representative_address,
    representative_x_3435,
    representative_y_3435,
    application_ward,
    application_alderman,
    application_alderman_status,
    application_neighbor_ward,
    application_ward_pair,
    application_boundary_distance_ft,
    construction_year,
    completion_year_start,
    completion_year_end,
    completion_date_proxy,
    completion_date_precision,
    timeline_order_status,
    application_to_issue_days,
    earliest_application_to_earliest_issue_days,
    earliest_application_to_latest_issue_days,
    earliest_application_to_completion_min_days,
    earliest_application_to_completion_max_days,
    earliest_issue_to_completion_min_days,
    earliest_issue_to_completion_max_days,
    application_to_completion_proxy_days,
    issue_to_completion_proxy_days,
    dwelling_units,
    building_sqft,
    land_sqft,
    maximum_unit_mention,
    match_method,
    permit_numbers
  )

chain_permit_rows <- chain_catalog |>
  dplyr::select(
    permit_chain_id,
    permit_id,
    permit_number,
    application_date,
    issue_date,
    permit_status,
    permit_issued,
    permit_address,
    permit_x_3435,
    permit_y_3435,
    permit_application_ward,
    permit_application_alderman,
    permit_application_alderman_status,
    permit_application_neighbor_ward,
    permit_application_ward_pair,
    permit_application_boundary_distance_m,
    permit_application_boundary_distance_ft,
    permit_application_era,
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

permit_timelines <- matched_chain_project_links |>
  dplyr::add_count(permit_chain_id, name = "projects_linked_to_chain") |>
  dplyr::left_join(
    chain_permit_rows,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(permit_rows) |>
  dplyr::inner_join(
    ledger_fields,
    by = c("matched_project_id" = "final_project_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    completion_year_start = as.Date(paste0(construction_year, "-01-01")),
    completion_year_end = as.Date(paste0(construction_year, "-12-31")),
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
      construction_year < lubridate::year(application_date) ~
        "reported_construction_year_precedes_application",
      construction_year < lubridate::year(issue_date) ~
        "reported_construction_year_precedes_issue",
      construction_year == lubridate::year(application_date) ~
        "application_and_reported_construction_same_year",
      TRUE ~ "reported_construction_after_application"
    )
  ) |>
  dplyr::arrange(
    matched_project_id,
    permit_chain_id,
    application_date,
    issue_date,
    permit_number
  )

summary <- dplyr::bind_rows(
  tibble::tibble(metric = "new_construction_permit_catalog", value = nrow(permits)),
  tibble::tibble(metric = "issued_study_period_permits", value = nrow(issued_study_permits)),
  tibble::tibble(metric = "issued_study_period_chains", value = nrow(chain_matches)),
  issued_study_permits |>
    dplyr::count(permit_application_alderman_status, name = "value") |>
    dplyr::transmute(
      metric = paste0(
        "issued_study_alderman_status:",
        permit_application_alderman_status
      ),
      value
    ),
  chain_matches |>
    dplyr::count(review_priority, name = "value") |>
    dplyr::transmute(metric = paste0("chain_priority:", review_priority), value),
  tibble::tibble(metric = "unmatched_review_queue", value = nrow(unmatched_queue)),
  tibble::tibble(metric = "matched_timeline_rows", value = nrow(matched_timelines)),
  tibble::tibble(metric = "matched_permit_timeline_rows", value = nrow(permit_timelines)),
  matched_timelines |>
    dplyr::count(timeline_order_status, name = "value") |>
    dplyr::transmute(metric = paste0("timeline_order:", timeline_order_status), value)
)

readr::write_csv(
  permits |>
    dplyr::select(
      permit_id,
      permit_number,
      permit_chain_id,
      application_date,
      issue_date,
      permit_status,
      permit_issued,
      permit_address,
      normalized_address,
      pin,
      permit_x_3435,
      permit_y_3435,
      permit_application_ward,
      permit_application_alderman,
      permit_application_alderman_status,
      permit_application_neighbor_ward,
      permit_application_ward_pair,
      permit_application_boundary_distance_m,
      permit_application_boundary_distance_ft,
      permit_application_era,
      reported_cost,
      max_unit_mention,
      single_family_signal,
      residential_signal,
      temporary_signal,
      revision_signal,
      foundation_signal,
      existing_building_signal,
      full_building_signal,
      full_residential_signal,
      work_description
    ),
  "../output/permit_first_permit_inventory.csv",
  na = ""
)
readr::write_csv(
  chain_matches,
  "../output/permit_first_chain_inventory.csv",
  na = ""
)
readr::write_csv(
  unmatched_queue,
  "../output/permit_first_unmatched_residential_queue.csv",
  na = ""
)
readr::write_csv(
  matched_timelines,
  "../output/permit_project_timeline_candidates.csv",
  na = ""
)
readr::write_csv(
  permit_timelines,
  "../output/permit_project_timeline_detail.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_first_inventory_summary.csv",
  na = ""
)

sf::st_write(
  chain_points[match(unmatched_queue$permit_chain_id, chain_points$permit_chain_id), ] |>
    dplyr::select(permit_chain_id),
  "../output/permit_first_unmatched_residential_queue.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
