# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

extract_unit_mentions <- function(permit_number, work_description) {
  text <- str_to_upper(coalesce(work_description, ""))
  locations <- str_locate_all(
    text,
    "\\b[0-9]{1,4}\\s*(?:TOTAL\\s+)?(?:DWELLING\\s+|RESIDENTIAL\\s+|APARTMENT\\s+|EFFICIENCY\\s+)?(?:UNITS?|D\\.?U\\.?)\\b"
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

permits <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  sf::st_drop_geometry() %>%
  filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    !is.na(application_start_date),
    !is.na(issue_date)
  ) %>%
  transmute(
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
  stop("Issued new-construction permit IDs and numbers must each be unique.", call. = FALSE)
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
  group_by(graph_component) %>%
  mutate(permit_chain_id = paste0("permit_chain_", min(permit_number))) %>%
  ungroup() %>%
  select(-graph_component)

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

project_summary <- project_chains %>%
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
  mutate(
    permits_with_unit_mentions = coalesce(permits_with_unit_mentions, 0L),
    distinct_unit_counts = coalesce(distinct_unit_counts, 0L),
    unit_counts = coalesce(unit_counts, "")
  )

summary <- tibble::tribble(
  ~metric, ~value,
  "issued_new_construction_permits", nrow(permits),
  "permits_referencing_another_number", n_distinct(permit_edges$referencing_permit_number),
  "distinct_referenced_permit_numbers", n_distinct(permit_edges$referenced_permit_number),
  "references_resolved_in_extract", sum(permit_edges$referenced_permit_in_extract),
  "references_unresolved_in_extract", sum(!permit_edges$referenced_permit_in_extract),
  "permit_revision_chains_with_multiple_records", sum(igraph::components(permit_graph)$csize > 1),
  "projects_with_direct_permit_evidence", n_distinct(paste(direct_links$source_family, direct_links$project_id)),
  "projects_with_multiple_permit_chains", sum(project_summary$permit_chains > 1),
  "projects_with_conflicting_unit_mentions", sum(project_summary$distinct_unit_counts > 1)
)

readr::write_csv(summary, "../output/permit_revision_summary.csv")
readr::write_csv(permit_edges, "../output/permit_revision_edges.csv")
readr::write_csv(project_chains, "../output/project_permit_chain_links.csv")
readr::write_csv(chain_unit_mentions, "../output/project_permit_chain_unit_mentions.csv")
readr::write_csv(project_summary, "../output/project_permit_chain_summary.csv")
