# setwd("tasks/construction_project_permits/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/permit_unit_patterns.R")


# Build spatial permit evidence

project_geometry <- sf::st_read(
  "../input/historical_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)

permits <- sf::st_read("../output/building_permits_for_verification.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435) %>%
  filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    !is.na(issue_date),
    !is.na(application_start_date)
  ) %>%
  mutate(
    permit_id = as.character(id),
    permit_number = as.character(permit),
    application_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    application_year = lubridate::year(application_date),
    issue_year = lubridate::year(issue_date),
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    referenced_permit_numbers = purrr::map2_chr(
      str_extract_all(
        str_to_upper(coalesce(work_description, "")),
        "(?<![0-9])10[0-9]{7}(?![0-9])"
      ),
      permit_number,
      ~ paste(sort(setdiff(unique(.x), .y)), collapse = "/")
    )
  ) %>%
  select(
    permit_id,
    permit_number,
    application_date,
    issue_date,
    application_year,
    issue_year,
    permit_status,
    permit_address,
    referenced_permit_numbers,
    work_description
  )

exact_links <- readr::read_csv(
  "../output/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_family = readr::col_character(),
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(plausible_application_window, plausible_issue_window) %>%
  distinct(source_family, project_id, permit_id) %>%
  mutate(exact_pin_match = TRUE)

candidate_index <- sf::st_is_within_distance(project_geometry, permits, dist = 200)

spatial_matches <- purrr::map2_dfr(
  seq_len(nrow(project_geometry)),
  candidate_index,
  function(project_row, permit_rows) {
    if (length(permit_rows) == 0) {
      return(tibble::tibble())
    }
    project <- project_geometry[project_row, ]
    permit_candidates <- permits[permit_rows, ]
    polygon_distance_ft <- as.numeric(sf::st_distance(
      project[rep(1, length(permit_rows)), ],
      permit_candidates,
      by_element = TRUE
    ))
    bind_cols(
      sf::st_drop_geometry(project[rep(1, length(permit_rows)), ]) %>%
        select(source_family, project_id, target_year),
      sf::st_drop_geometry(permit_candidates) %>%
        select(
          permit_id,
          permit_number,
          application_date,
          issue_date,
          application_year,
          issue_year,
          permit_status,
          permit_address,
          referenced_permit_numbers,
          work_description
        ),
      tibble::tibble(polygon_distance_ft)
    )
  }
) %>%
  mutate(
    application_year_gap = target_year - application_year,
    issue_year_gap = target_year - issue_year,
    plausible_application_window = between(application_year_gap, -2, 6),
    plausible_issue_window = is.na(issue_year_gap) | between(issue_year_gap, -2, 4)
  ) %>%
  filter(plausible_application_window, plausible_issue_window) %>%
  left_join(
    exact_links,
    by = c("source_family", "project_id", "permit_id"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    exact_pin_match = coalesce(exact_pin_match, FALSE),
    spatial_match_method = case_when(
      exact_pin_match ~ "exact_pin_and_project_polygon",
      polygon_distance_ft == 0 ~ "inside_project_polygon",
      TRUE ~ "within_200ft_of_project_polygon"
    )
  ) %>%
  distinct(source_family, project_id, target_year, permit_id, .keep_all = TRUE) %>%
  arrange(source_family, project_id, target_year, polygon_distance_ft, application_date, permit_id)

SaveData(spatial_matches, character(), "../output/new_construction_spatial_permit_matches.csv")

# Build permit revision evidence

extract_unit_mentions <- function(permit_number, work_description) {
  text <- str_to_upper(coalesce(work_description, ""))
  locations <- str_locate_all(
    text,
    paste0(
    "\\b[0-9]{1,4}\\)?\\s*(?:TOTAL\\s+)?(?:DWELLING\\s+|RESIDENTI?AL\\s+|APARTMENT\\s+|EFFICIENCY\\s+)*(?:UNITS?|D\\.?U\\.?)\\b",
      "|", attached_house_count_pattern
    )
  )[[1]]

  if (nrow(locations) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    permit_number,
    mention_order = seq_len(nrow(locations)),
    unit_mention = str_sub(text, locations[, "start"], locations[, "end"]),
    unit_count = suppressWarnings(as.numeric(str_extract(unit_mention, "[0-9]{1,4}"))),
    mention_context = purrr::map2_chr(
      locations[, "start"],
      locations[, "end"],
      ~ str_squish(str_sub(text, max(1, .x - 100), min(str_length(text), .y + 100)))
    )
  )
}

permits <- sf::st_read("../output/building_permits_for_verification.gpkg", quiet = TRUE) %>%
  sf::st_drop_geometry() %>%
  filter(
    !is.na(application_start_date),
    !is.na(issue_date)
  ) %>%
  transmute(
    permit_type,
    permit_id = as.character(id),
    permit_number = as.character(permit),
    application_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    permit_status,
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    work_description = coalesce(work_description, ""),
    reference_numbers = purrr::map2(
      str_extract_all(
        str_to_upper(coalesce(work_description, "")),
        "(?<![0-9])10[0-9]{7}(?![0-9])"
      ),
      as.character(permit),
      ~ sort(setdiff(unique(.x), .y))
    )
  )

if (anyDuplicated(permits$permit_id) > 0 || anyDuplicated(permits$permit_number) > 0) {
  stop("Issued permit IDs and numbers must each be unique.", call. = FALSE)
}

permit_edges <- permits %>%
  select(referencing_permit_number = permit_number, reference_numbers) %>%
  tidyr::unnest_longer(reference_numbers, values_to = "referenced_permit_number") %>%
  filter(!is.na(referenced_permit_number), referenced_permit_number != "") %>%
  left_join(
    permits %>% select(referencing_permit_number = permit_number, referencing_issue_date = issue_date),
    by = "referencing_permit_number",
    relationship = "many-to-one"
  ) %>%
  left_join(
    permits %>% select(referenced_permit_number = permit_number, referenced_issue_date = issue_date),
    by = "referenced_permit_number",
    relationship = "many-to-one"
  ) %>%
  mutate(
    referenced_permit_in_extract = !is.na(referenced_issue_date),
    chronological_relation = case_when(
      !referenced_permit_in_extract ~ "referenced_permit_not_in_extract",
      referencing_issue_date > referenced_issue_date ~ "later_permit_references_earlier_permit",
      referencing_issue_date < referenced_issue_date ~ "earlier_record_mentions_later_permit",
      TRUE ~ "same_issue_date"
    )
  ) %>%
  arrange(referencing_permit_number, referenced_permit_number)

resolved_edges <- permit_edges %>%
  filter(referenced_permit_in_extract) %>%
  transmute(from = referencing_permit_number, to = referenced_permit_number)

permit_graph <- igraph::graph_from_data_frame(
  resolved_edges,
  directed = FALSE,
  vertices = permits$permit_number
)

permit_components <- tibble::tibble(
  permit_number = names(igraph::components(permit_graph)$membership),
  graph_component = as.integer(igraph::components(permit_graph)$membership)
) %>%
  left_join(permits %>% select(permit_number, permit_type), by = "permit_number", relationship = "one-to-one") %>%
  group_by(graph_component) %>%
  mutate(permit_chain_id = paste0("permit_chain_", min(permit_number[permit_type == "PERMIT - NEW CONSTRUCTION" |
    !any(permit_type == "PERMIT - NEW CONSTRUCTION")]))) %>%
  ungroup() %>%
  select(-graph_component, -permit_type)

exact_links <- readr::read_csv(
  "../output/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_family = readr::col_character(),
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(plausible_application_window, plausible_issue_window) %>%
  transmute(source_family, project_id, permit_id, permit_number, direct_match_method = "exact_pin")

inside_links <- readr::read_csv(
  "../output/new_construction_spatial_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_family = readr::col_character(),
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    polygon_distance_ft = readr::col_double(),
    .default = readr::col_guess()
  )
) %>%
  filter(polygon_distance_ft == 0) %>%
  transmute(source_family, project_id, permit_id, permit_number, direct_match_method = "project_polygon")

direct_links <- bind_rows(exact_links, inside_links) %>%
  group_by(source_family, project_id, permit_id, permit_number) %>%
  summarise(
    direct_match_method = if_else(any(direct_match_method == "exact_pin"), "exact_pin", "project_polygon"),
    .groups = "drop"
  ) %>%
  left_join(
    permits %>% select(permit_id, catalog_permit_number = permit_number),
    by = "permit_id",
    relationship = "many-to-one"
  )

if (any(is.na(direct_links$catalog_permit_number))) {
  stop("Project evidence contains a permit ID absent from the permit catalog.", call. = FALSE)
}
if (any(direct_links$permit_number != direct_links$catalog_permit_number)) {
  stop("Project evidence and permit catalog disagree on permit number.", call. = FALSE)
}

direct_links <- direct_links %>%
  select(-catalog_permit_number) %>%
  left_join(permit_components, by = "permit_number", relationship = "many-to-one")

permits_by_chain <- split(permit_components, permit_components$permit_chain_id)
project_chains <- direct_links %>%
  distinct(source_family, project_id, permit_chain_id) %>%
  purrr::pmap_dfr(function(source_family, project_id, permit_chain_id) {
    chain <- permits_by_chain[[permit_chain_id]]
    bind_cols(
      tibble::tibble(
        source_family = rep(source_family, nrow(chain)),
        project_id = rep(project_id, nrow(chain))
      ),
      chain
    )
  }) %>%
  inner_join(permits, by = "permit_number", relationship = "many-to-one") %>%
  left_join(
    direct_links %>%
      select(source_family, project_id, permit_number, direct_match_method),
    by = c("source_family", "project_id", "permit_number"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    directly_matched = !is.na(direct_match_method),
    referenced_permit_numbers = purrr::map_chr(reference_numbers, ~ paste(.x, collapse = "/"))
  ) %>%
  select(
    source_family,
    project_id,
    permit_chain_id,
    permit_id,
    permit_number,
    permit_type,
    directly_matched,
    direct_match_method,
    application_date,
    issue_date,
    permit_status,
    permit_address,
    referenced_permit_numbers,
    work_description
  ) %>%
  arrange(source_family, project_id, permit_chain_id, issue_date, permit_number)

unit_mentions <- project_chains %>%
  distinct(permit_number, work_description) %>%
  purrr::pmap_dfr(extract_unit_mentions)

mentions_by_permit <- split(unit_mentions, unit_mentions$permit_number)
chain_unit_mentions <- project_chains %>%
  distinct(source_family, project_id, permit_chain_id, permit_number) %>%
  purrr::pmap_dfr(function(source_family, project_id, permit_chain_id, permit_number) {
    mentions <- mentions_by_permit[[permit_number]]
    if (is.null(mentions) || nrow(mentions) == 0) {
      return(tibble::tibble())
    }
    bind_cols(
      tibble::tibble(
        source_family = rep(source_family, nrow(mentions)),
        project_id = rep(project_id, nrow(mentions)),
        permit_chain_id = rep(permit_chain_id, nrow(mentions))
      ),
      mentions
    )
  }) %>%
  select(
    source_family,
    project_id,
    permit_chain_id,
    permit_number,
    mention_order,
    unit_mention,
    unit_count,
    mention_context
  ) %>%
  arrange(source_family, project_id, permit_chain_id, permit_number, mention_order)

# Separately numbered buildings can corroborate a whole-development count.
# Require a complete consecutive building list, one completed permit per building,
# and one residential count per permit. Do not add revisions or parking counts.
permit_counts <- unit_mentions %>% group_by(permit_number) %>%
  summarise(unit_count = if (n_distinct(unit_count) == 1) first(unit_count) else NA_real_, .groups = "drop")
numbered_buildings <- project_chains %>%
  left_join(permit_counts, by = "permit_number", relationship = "many-to-one") %>%
  filter(permit_type == "PERMIT - NEW CONSTRUCTION" | is.finite(unit_count)) %>%
  mutate(building_number = as.integer(str_match(str_to_upper(work_description),
    "\\bBUILDING\\s*#?\\s*([0-9]+)\\b")[, 2]),
    nonresidential_amenity = str_detect(str_to_upper(work_description), "COMMUNITY CENTER|AMENITY BUILDING") & is.na(unit_count)) %>%
  filter(!nonresidential_amenity) %>% group_by(source_family, project_id) %>%
  summarise(numbered_building_units = if (n() > 1 && all(!is.na(building_number)) &&
      n_distinct(building_number) == n() && setequal(building_number, seq_len(n())) &&
      all(!is.na(permit_status) & permit_status == "COMPLETE" & permit_type == "PERMIT - NEW CONSTRUCTION" &
        is.finite(unit_count) & unit_count > 0) &&
      !any(str_detect(str_to_upper(work_description), "REVISION|ALTERATION|CONVERT|FOUNDATION")))
      sum(unit_count) else NA_real_, .groups = "drop")

project_summary <- project_chains %>%
  filter(permit_type == "PERMIT - NEW CONSTRUCTION") %>%
  group_by(source_family, project_id) %>%
  summarise(
    permit_chains = n_distinct(permit_chain_id),
    directly_matched_permits = n_distinct(permit_number[directly_matched]),
    all_chain_permits = n_distinct(permit_number),
    earliest_application_date = min(application_date, na.rm = TRUE),
    earliest_issue_date = min(issue_date, na.rm = TRUE),
    latest_issue_date = max(issue_date, na.rm = TRUE),
    permit_numbers = paste(sort(unique(permit_number)), collapse = "/"),
    .groups = "drop"
  ) %>%
  left_join(
    chain_unit_mentions %>%
      group_by(source_family, project_id) %>%
      summarise(
        permits_with_unit_mentions = n_distinct(permit_number),
        distinct_unit_counts = n_distinct(unit_count),
        unit_counts = paste(sort(unique(unit_count)), collapse = "/"),
        .groups = "drop"
      ),
    by = c("source_family", "project_id"),
    relationship = "one-to-one"
  ) %>%
  left_join(numbered_buildings, by = c("source_family", "project_id"), relationship = "one-to-one") %>%
  mutate(
    permits_with_unit_mentions = coalesce(permits_with_unit_mentions, 0L),
    distinct_unit_counts = coalesce(distinct_unit_counts, 0L),
    unit_counts = coalesce(unit_counts, "")
  )

SaveData(project_chains, c("project_id", "permit_number"), "../output/project_permit_chain_links.csv")
SaveData(chain_unit_mentions, c("project_id", "permit_number", "mention_order"), "../output/project_permit_chain_unit_mentions.csv")
SaveData(project_summary, c("project_id"), "../output/project_permit_chain_summary.csv")
