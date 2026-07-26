# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_address <- function(x, keep_direction = TRUE) {
  value <- str_to_upper(coalesce(x, "")) %>%
    str_replace_all("[^A-Z0-9 ]", " ") %>%
    str_replace_all("\\bCHICAGO\\b", " ") %>%
    str_replace_all(
      "\\b(STREET|ST|AVENUE|AVE|ROAD|RD|BOULEVARD|BLVD|COURT|CT|PLACE|PL|DRIVE|DR)\\b",
      " "
    )
  if (!keep_direction) {
    value <- str_replace_all(value, "(?<=^[0-9]{1,6} )\\b(N|S|E|W)\\b", " ")
  }
  value <- str_squish(value)
  if_else(
    str_detect(value, "^[1-9][0-9]* [A-Z]") & !str_detect(value, fixed("UNKNOWN")),
    value,
    NA_character_
  )
}

make_pairs <- function(data, group_column, evidence_column) {
  split(data$project_id, data[[group_column]]) %>%
    purrr::imap_dfr(function(project_ids, evidence_value) {
      project_ids <- sort(unique(project_ids))
      if (length(project_ids) < 2) {
        return(tibble::tibble())
      }
      pairs <- t(utils::combn(project_ids, 2))
      tibble::tibble(
        project_id_1 = pairs[, 1],
        project_id_2 = pairs[, 2],
        evidence_value = evidence_value
      )
    }) %>%
    rename(!!evidence_column := evidence_value)
}

candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(between(construction_year, 2006L, 2022L)) %>%
  select(
    project_id,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    selected_source_addresses,
    component_pins
  )

entity_versions <- readr::read_csv(
  "../output/commercial_entity_version_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_family_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(project_family_id %in% candidates$project_id) %>%
  transmute(
    project_id = project_family_id,
    raw_row,
    address,
    address_key = normalize_address(address, keep_direction = TRUE),
    address_key_without_direction = normalize_address(address, keep_direction = FALSE)
  )

permit_links <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    source_family == "commercial",
    project_id %in% candidates$project_id,
    !is.na(permit_chain_id),
    permit_chain_id != ""
  ) %>%
  distinct(project_id, permit_chain_id)

geometry <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  filter(
    source_family == "commercial",
    project_id %in% candidates$project_id,
    project_polygon_valid
  ) %>%
  select(project_id)

if (anyDuplicated(candidates$project_id) > 0) {
  stop("Study-period commercial candidates are not unique by project.", call. = FALSE)
}
if (anyDuplicated(entity_versions$raw_row) > 0) {
  stop("Commercial source rows repeat in the duplicate screen.", call. = FALSE)
}
if (anyDuplicated(geometry$project_id) > 0) {
  stop("Commercial project geometry is not unique by project.", call. = FALSE)
}

exact_address_pairs <- entity_versions %>%
  filter(!is.na(address_key)) %>%
  distinct(project_id, address_key) %>%
  make_pairs("address_key", "exact_address_key")

weak_address_pairs <- entity_versions %>%
  filter(!is.na(address_key_without_direction)) %>%
  distinct(project_id, address_key_without_direction) %>%
  make_pairs("address_key_without_direction", "address_key_without_direction")

permit_pairs <- permit_links %>%
  make_pairs("permit_chain_id", "shared_permit_chain_id")

intersection_index <- sf::st_intersects(geometry, geometry)
spatial_pairs <- purrr::map2_dfr(
  seq_len(nrow(geometry)),
  intersection_index,
  function(row_number, neighbors) {
    neighbors <- neighbors[neighbors > row_number]
    if (length(neighbors) == 0) {
      return(tibble::tibble())
    }
    tibble::tibble(
      project_id_1 = geometry$project_id[row_number],
      project_id_2 = geometry$project_id[neighbors]
    )
  }
) %>%
  mutate(
    intersection_area_sqft = purrr::map2_dbl(
      project_id_1,
      project_id_2,
      function(project_id_1, project_id_2) {
        geometry_1 <- sf::st_geometry(geometry[geometry$project_id == project_id_1, ])
        geometry_2 <- sf::st_geometry(geometry[geometry$project_id == project_id_2, ])
        suppressWarnings(as.numeric(sf::st_area(sf::st_intersection(geometry_1, geometry_2))))
      }
    )
  ) %>%
  filter(intersection_area_sqft > 1)

pair_keys <- bind_rows(
  exact_address_pairs %>% select(project_id_1, project_id_2),
  weak_address_pairs %>% select(project_id_1, project_id_2),
  permit_pairs %>% select(project_id_1, project_id_2),
  spatial_pairs
) %>%
  distinct(project_id_1, project_id_2)

exact_address_evidence <- exact_address_pairs %>%
  group_by(project_id_1, project_id_2) %>%
  summarise(
    exact_address_keys = paste(sort(unique(exact_address_key)), collapse = "/"),
    .groups = "drop"
  )

weak_address_evidence <- weak_address_pairs %>%
  group_by(project_id_1, project_id_2) %>%
  summarise(
    address_keys_without_direction = paste(
      sort(unique(address_key_without_direction)),
      collapse = "/"
    ),
    .groups = "drop"
  )

permit_evidence <- permit_pairs %>%
  group_by(project_id_1, project_id_2) %>%
  summarise(
    shared_permit_chain_ids = paste(
      sort(unique(shared_permit_chain_id)),
      collapse = "/"
    ),
    .groups = "drop"
  )

spatial_evidence <- spatial_pairs %>%
  select(project_id_1, project_id_2, intersection_area_sqft)

project_1 <- candidates %>%
  rename_with(~ paste0(.x, "_1"), -project_id) %>%
  rename(project_id_1 = project_id)
project_2 <- candidates %>%
  rename_with(~ paste0(.x, "_2"), -project_id) %>%
  rename(project_id_2 = project_id)

pairs <- pair_keys %>%
  left_join(exact_address_evidence, by = c("project_id_1", "project_id_2"), relationship = "one-to-one") %>%
  left_join(weak_address_evidence, by = c("project_id_1", "project_id_2"), relationship = "one-to-one") %>%
  left_join(permit_evidence, by = c("project_id_1", "project_id_2"), relationship = "one-to-one") %>%
  left_join(spatial_evidence, by = c("project_id_1", "project_id_2"), relationship = "one-to-one") %>%
  left_join(project_1, by = "project_id_1", relationship = "many-to-one") %>%
  left_join(project_2, by = "project_id_2", relationship = "many-to-one") %>%
  mutate(
    shared_exact_address = !is.na(exact_address_keys),
    shared_address_without_direction = !is.na(address_keys_without_direction),
    shared_permit_chain = !is.na(shared_permit_chain_ids),
    polygons_overlap = is.finite(intersection_area_sqft) & intersection_area_sqft > 1,
    year_gap = abs(construction_year_1 - construction_year_2),
    same_units = is.finite(dwelling_units_1) & is.finite(dwelling_units_2) &
      dwelling_units_1 == dwelling_units_2,
    cross_family_review_required =
      shared_permit_chain |
      shared_exact_address |
      (
        shared_address_without_direction &
          same_units &
          is.finite(year_gap) & year_gap <= 2
      ) |
      (
        polygons_overlap &
          same_units &
          is.finite(year_gap) & year_gap <= 2
      ),
    duplicate_reason = paste(
      cbind(
        if_else(shared_permit_chain, "shared_permit_chain", ""),
        if_else(shared_exact_address, "same_normalized_address", ""),
        if_else(
          shared_address_without_direction & !shared_exact_address,
          "same_address_ignoring_direction",
          ""
        ),
        if_else(polygons_overlap, "construction_year_polygons_overlap", ""),
        if_else(same_units, "same_unit_count", "")
      ) %>%
        apply(1, function(values) paste(values[values != ""], collapse = ";"))
    )
  ) %>%
  arrange(desc(cross_family_review_required), project_id_1, project_id_2)

review_queue <- pairs %>%
  filter(cross_family_review_required)

project_evidence <- bind_rows(
  review_queue %>%
    transmute(
      project_id = project_id_1,
      possible_duplicate_project_id = project_id_2,
      duplicate_reason
    ),
  review_queue %>%
    transmute(
      project_id = project_id_2,
      possible_duplicate_project_id = project_id_1,
      duplicate_reason
    )
) %>%
  group_by(project_id) %>%
  summarise(
    possible_duplicate_projects = paste(
      sort(unique(possible_duplicate_project_id)),
      collapse = "/"
    ),
    duplicate_reasons = paste(sort(unique(duplicate_reason)), collapse = " / "),
    .groups = "drop"
  ) %>%
  right_join(candidates %>% select(project_id), by = "project_id", relationship = "one-to-one") %>%
  mutate(cross_family_review_required = !is.na(possible_duplicate_projects)) %>%
  arrange(project_id)

review_graph <- igraph::graph_from_data_frame(
  review_queue %>% select(project_id_1, project_id_2),
  directed = FALSE
)
review_membership <- igraph::components(review_graph)$membership
review_groups <- tibble::tibble(
  project_id = names(review_membership),
  graph_component = as.integer(review_membership)
) %>%
  group_by(graph_component) %>%
  mutate(group_key = min(project_id)) %>%
  ungroup() %>%
  mutate(
    duplicate_review_group_id = paste0(
      "commercial_duplicate_review_",
      str_pad(dense_rank(group_key), 3, pad = "0")
    )
  ) %>%
  select(duplicate_review_group_id, project_id) %>%
  left_join(candidates, by = "project_id", relationship = "many-to-one") %>%
  arrange(duplicate_review_group_id, project_id)

review_group_summary <- review_groups %>%
  group_by(duplicate_review_group_id) %>%
  summarise(
    projects = n(),
    project_ids = paste(project_id, collapse = "/"),
    construction_years = paste(sort(unique(construction_year)), collapse = "/"),
    dwelling_unit_values = paste(sort(unique(dwelling_units)), collapse = "/"),
    source_addresses = paste(
      sort(unique(na.omit(selected_source_addresses))),
      collapse = " / "
    ),
    .groups = "drop"
  )

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(review_queue), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Commercial duplicate review contains a prohibited analysis field.", call. = FALSE)
}
if (nrow(project_evidence) != nrow(candidates)) {
  stop("Cross-family duplicate evidence does not cover every commercial candidate.", call. = FALSE)
}
if (anyDuplicated(project_evidence$project_id) > 0) {
  stop("Cross-family duplicate evidence is not unique by project.", call. = FALSE)
}
if (any(review_queue$project_id_1 >= review_queue$project_id_2)) {
  stop("Cross-family duplicate pair ordering is not canonical.", call. = FALSE)
}

summary <- tibble::tibble(
  section = "coverage",
  metric = c(
    "study_period_candidates",
    "candidates_with_geometry",
    "candidate_pairs_with_any_evidence",
    "cross_family_review_pairs",
    "cross_family_review_groups",
    "projects_in_cross_family_review_pairs",
    "pairs_sharing_permit_chain",
    "pairs_sharing_exact_address",
    "pairs_sharing_address_without_direction",
    "pairs_with_overlapping_polygons",
    "duplicate_project_ids",
    "duplicate_geometry_project_ids"
  ),
  value = c(
    nrow(candidates),
    nrow(geometry),
    nrow(pairs),
    nrow(review_queue),
    nrow(review_group_summary),
    sum(project_evidence$cross_family_review_required),
    sum(review_queue$shared_permit_chain),
    sum(review_queue$shared_exact_address),
    sum(review_queue$shared_address_without_direction),
    sum(review_queue$polygons_overlap),
    anyDuplicated(candidates$project_id),
    anyDuplicated(geometry$project_id)
  )
)

readr::write_csv(pairs, "../output/commercial_cross_family_duplicate_pairs.csv")
readr::write_csv(review_queue, "../output/commercial_cross_family_duplicate_review_queue.csv")
readr::write_csv(project_evidence, "../output/commercial_cross_family_duplicate_evidence.csv")
readr::write_csv(review_groups, "../output/commercial_cross_family_duplicate_group_members.csv")
readr::write_csv(review_group_summary, "../output/commercial_cross_family_duplicate_groups.csv")
readr::write_csv(summary, "../output/commercial_cross_family_duplicate_summary.csv")
