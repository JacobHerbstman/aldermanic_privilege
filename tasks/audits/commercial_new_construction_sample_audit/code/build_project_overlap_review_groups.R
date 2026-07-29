# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

pairs <- readr::read_csv(
  "../output/project_overlap_pairs.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    focus_project_id = readr::col_character(),
    focus_component_pins = readr::col_character(),
    neighbor_project_id = readr::col_character(),
    neighbor_component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

edges <- pairs %>%
  transmute(
    from = paste(focus_source, focus_project_id, sep = ":"),
    to = paste(neighbor_source, neighbor_project_id, sep = ":")
  ) %>%
  mutate(
    node_1 = pmin(from, to),
    node_2 = pmax(from, to)
  ) %>%
  distinct(node_1, node_2)

overlap_graph <- igraph::graph_from_data_frame(
  edges %>% select(from = node_1, to = node_2),
  directed = FALSE
)
membership <- igraph::components(overlap_graph)$membership
members <- tibble::tibble(
  project_key = names(membership),
  graph_component = as.integer(membership)
) %>%
  group_by(graph_component) %>%
  mutate(group_sort_key = min(project_key)) %>%
  ungroup() %>%
  mutate(
    overlap_group_id = paste0(
      "overlap_group_",
      stringr::str_pad(dense_rank(group_sort_key), width = 3, pad = "0")
    ),
    source_family = stringr::str_extract(project_key, "^[^:]+"),
    project_id = stringr::str_remove(project_key, "^[^:]+:")
  ) %>%
  select(overlap_group_id, source_family, project_id)

if (anyDuplicated(members[c("source_family", "project_id")]) > 0) {
  stop("An overlap project appears in more than one group.", call. = FALSE)
}

residential <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  transmute(
    source_family = "residential",
    project_id,
    project_kind,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    source_address = NA_character_,
    source_description = class_values,
    candidate_status,
    decision_reason,
    year_source,
    units_source,
    building_source,
    land_source
  )

commercial <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  transmute(
    source_family = "commercial",
    project_id,
    project_kind,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    source_address = selected_source_addresses,
    source_description = paste(
      selected_property_type_use,
      selected_property_description,
      sep = "; "
    ),
    candidate_status,
    decision_reason,
    year_source,
    units_source,
    building_source,
    land_source
  )

candidates <- bind_rows(residential, commercial)
if (anyDuplicated(candidates[c("source_family", "project_id")]) > 0) {
  stop("Overlap-review candidates are not unique by source and project.", call. = FALSE)
}

scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    source_family,
    project_id,
    distance_to_boundary_ft,
    within_500ft,
    review_scope
  )

permit_evidence <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    application_date = readr::col_date(),
    issue_date = readr::col_date(),
    .default = readr::col_guess()
  )
) %>%
  semi_join(members, by = c("source_family", "project_id")) %>%
  arrange(source_family, project_id, permit_chain_id, application_date, permit_number) %>%
  group_by(source_family, project_id) %>%
  summarise(
    permit_chain_evidence = paste0(
      permit_chain_id,
      ": permit ", permit_number,
      "; applied=", coalesce(as.character(application_date), "missing"),
      "; issued=", coalesce(as.character(issue_date), "missing"),
      "; status=", coalesce(permit_status, "missing"),
      "; address=", coalesce(permit_address, "missing"),
      "; direct=", directly_matched,
      "; method=", coalesce(direct_match_method, "missing"),
      "; description=", coalesce(work_description, "missing"),
      collapse = " || "
    ),
    .groups = "drop"
  )

neighbor_evidence <- pairs %>%
  transmute(
    source_family = focus_source,
    project_id = focus_project_id,
    neighbor = paste(neighbor_source, neighbor_project_id, sep = ":"),
    evidence = paste0(
      neighbor,
      " [", overlap_review_reason, "]",
      "; year_gap=", year_gap,
      "; polygon_distance_ft=", if_else(
        is.na(polygon_distance_ft),
        "missing",
        as.character(round(polygon_distance_ft, 2))
      ),
      "; centroid_distance_ft=", if_else(
        is.na(centroid_distance_ft),
        "missing",
        as.character(round(centroid_distance_ft, 2))
      ),
      "; smaller_polygon_overlap_share=",
      if_else(
        is.na(smaller_polygon_overlap_share),
        "missing",
        as.character(round(smaller_polygon_overlap_share, 3))
      )
    )
  ) %>%
  group_by(source_family, project_id) %>%
  summarise(
    overlap_edge_evidence = paste(evidence, collapse = " || "),
    .groups = "drop"
  )

review <- members %>%
  left_join(
    candidates,
    by = c("source_family", "project_id"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    scope,
    by = c("source_family", "project_id"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    permit_evidence,
    by = c("source_family", "project_id"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    neighbor_evidence,
    by = c("source_family", "project_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    in_review_scope = review_scope == "review_within_1500ft",
    within_500ft = coalesce(within_500ft, FALSE)
  ) %>%
  arrange(overlap_group_id, source_family, project_id)

if (any(is.na(review$project_kind))) {
  stop("An overlap-group member lacks candidate metadata.", call. = FALSE)
}

group_summary <- review %>%
  group_by(overlap_group_id) %>%
  summarise(
    projects = n(),
    residential_projects = sum(source_family == "residential"),
    commercial_projects = sum(source_family == "commercial"),
    scoped_projects = sum(in_review_scope),
    within_500ft_projects = sum(within_500ft),
    construction_years = paste(sort(unique(construction_year)), collapse = "/"),
    project_members = paste(
      paste(source_family, project_id, sep = ":"),
      collapse = " || "
    ),
    .groups = "drop"
  )

readr::write_csv(review, "../output/project_overlap_group_members.csv")
readr::write_csv(group_summary, "../output/project_overlap_group_summary.csv")
