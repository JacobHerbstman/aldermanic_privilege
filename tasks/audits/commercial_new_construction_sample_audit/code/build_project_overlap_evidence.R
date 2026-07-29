# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

residential <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    between(construction_year, 2006L, 2022L),
    candidate_status != "exclude_outside_period"
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
    candidate_status,
    decision_reason
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
  filter(
    between(construction_year, 2006L, 2022L),
    candidate_status != "exclude_outside_period"
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
    candidate_status,
    decision_reason
  )

candidates <- bind_rows(residential, commercial)
if (anyDuplicated(candidates[c("source_family", "project_id")]) > 0) {
  stop("Project overlap candidates are not unique by source and project.", call. = FALSE)
}
if (any(is.na(candidates$component_pins) | candidates$component_pins == "")) {
  stop("A project overlap candidate lacks component PINs.", call. = FALSE)
}

review_scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(review_scope == "review_within_1500ft") %>%
  select(source_family, project_id)

geometries <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  inner_join(
    candidates,
    by = c(
      "source_family",
      "project_id",
      "target_year" = "construction_year"
    ),
    relationship = "one-to-one"
  )

if (anyDuplicated(geometries[c("source_family", "project_id")]) > 0 ||
    nrow(anti_join(
      sf::st_drop_geometry(geometries),
      candidates,
      by = c("source_family", "project_id")
    )) > 0) {
  stop("Project overlap geometries are not unique or contain unknown projects.", call. = FALSE)
}
if (nrow(anti_join(
  review_scope,
  sf::st_drop_geometry(geometries),
  by = c("source_family", "project_id")
)) > 0) {
  stop("A project in the 1,500-foot review scope lacks geometry.", call. = FALSE)
}

focused <- geometries %>%
  semi_join(review_scope, by = c("source_family", "project_id"))
nearby_index <- sf::st_is_within_distance(focused, geometries, dist = 25)

spatial_pairs <- purrr::map2_dfr(seq_len(nrow(focused)), nearby_index, function(i, rows) {
  rows <- rows[
    focused$source_family[i] != geometries$source_family[rows] |
      focused$project_id[i] != geometries$project_id[rows]
  ]
  if (length(rows) == 0) return(tibble::tibble())

  focal <- focused[i, ]
  neighbors <- geometries[rows, ]
  focal_repeated <- focal[rep(1, length(rows)), ]
  polygon_distance_ft <- as.numeric(sf::st_distance(
    focal_repeated,
    neighbors,
    by_element = TRUE
  ))
  centroid_distance_ft <- as.numeric(sf::st_distance(
    suppressWarnings(sf::st_point_on_surface(focal_repeated)),
    suppressWarnings(sf::st_point_on_surface(neighbors)),
    by_element = TRUE
  ))
  intersection_area_sqft <- purrr::map2_dbl(
    seq_len(nrow(focal_repeated)),
    seq_len(nrow(neighbors)),
    function(j, k) {
      intersection <- suppressWarnings(sf::st_intersection(
        sf::st_geometry(focal_repeated[j, ]),
        sf::st_geometry(neighbors[k, ])
      ))
      if (length(intersection) == 0) 0 else sum(as.numeric(sf::st_area(intersection)))
    }
  )
  focal_area <- as.numeric(sf::st_area(focal_repeated))
  neighbor_area <- as.numeric(sf::st_area(neighbors))

  tibble::tibble(
    focus_source = focal$source_family,
    focus_project_id = focal$project_id,
    focus_project_kind = focal$project_kind,
    focus_component_pins = focal$component_pins,
    focus_year = focal$target_year,
    focus_units = focal$dwelling_units,
    focus_building_sqft = focal$building_sqft,
    focus_land_sqft = focal$land_sqft,
    focus_address = focal$source_address,
    neighbor_source = neighbors$source_family,
    neighbor_project_id = neighbors$project_id,
    neighbor_project_kind = neighbors$project_kind,
    neighbor_component_pins = neighbors$component_pins,
    neighbor_year = neighbors$target_year,
    neighbor_units = neighbors$dwelling_units,
    neighbor_building_sqft = neighbors$building_sqft,
    neighbor_land_sqft = neighbors$land_sqft,
    neighbor_address = neighbors$source_address,
    year_gap = abs(focal$target_year - neighbors$target_year),
    polygon_distance_ft,
    centroid_distance_ft,
    intersection_area_sqft,
    smaller_polygon_overlap_share = intersection_area_sqft / pmin(focal_area, neighbor_area)
  )
}) %>%
  mutate(
    shared_component_pin = map2_lgl(
      focus_component_pins,
      neighbor_component_pins,
      ~ length(intersect(str_split_1(.x, "/"), str_split_1(.y, "/"))) > 0
    ),
    same_positive_units = is.finite(focus_units) & focus_units > 0 &
      is.finite(neighbor_units) & neighbor_units == focus_units,
    same_positive_building_sqft = is.finite(focus_building_sqft) &
      focus_building_sqft > 0 &
      is.finite(neighbor_building_sqft) &
      neighbor_building_sqft == focus_building_sqft,
    overlap_review_reason = case_when(
      shared_component_pin ~ "shared_component_pin",
      focus_source != neighbor_source & smaller_polygon_overlap_share >= 0.50 & year_gap <= 8 ~
        "cross_source_same_site_similar_year",
      focus_source != neighbor_source & centroid_distance_ft <= 10 & year_gap <= 8 ~
        "cross_source_same_location_similar_year",
      focus_source == neighbor_source & smaller_polygon_overlap_share >= 0.50 &
        year_gap <= 3 & same_positive_building_sqft ~
        "within_source_same_site_same_building_fields",
      focus_source == neighbor_source & centroid_distance_ft <= 10 &
        year_gap <= 3 & same_positive_building_sqft ~
        "within_source_same_location_same_building_fields",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(overlap_review_reason)) %>%
  distinct(
    focus_source,
    focus_project_id,
    neighbor_source,
    neighbor_project_id,
    .keep_all = TRUE
  ) %>%
  arrange(focus_source, focus_project_id, neighbor_source, neighbor_project_id)

component_index <- candidates %>%
  transmute(
    source_family,
    project_id,
    component_pin = stringr::str_split(component_pins, "/")
  ) %>%
  tidyr::unnest_longer(component_pin) %>%
  distinct(source_family, project_id, component_pin)

if (anyDuplicated(component_index) > 0) {
  stop("The project-component index contains duplicate keys.", call. = FALSE)
}

component_pairs <- purrr::map_dfr(seq_len(nrow(review_scope)), function(i) {
  focal <- candidates %>%
    semi_join(review_scope[i, ], by = c("source_family", "project_id"))
  if (nrow(focal) != 1) {
    stop("A scoped project is missing from the candidate inventory.", call. = FALSE)
  }

  focal_pins <- component_index %>%
    filter(
      source_family == focal$source_family,
      project_id == focal$project_id
    ) %>%
    pull(component_pin)
  neighbor_keys <- component_index %>%
    filter(
      component_pin %in% focal_pins,
      source_family != focal$source_family | project_id != focal$project_id
    ) %>%
    distinct(source_family, project_id)
  neighbors <- candidates %>%
    semi_join(neighbor_keys, by = c("source_family", "project_id"))
  if (nrow(neighbors) == 0) return(tibble::tibble())

  tibble::tibble(
    focus_source = focal$source_family,
    focus_project_id = focal$project_id,
    focus_project_kind = focal$project_kind,
    focus_component_pins = focal$component_pins,
    focus_year = focal$construction_year,
    focus_units = focal$dwelling_units,
    focus_building_sqft = focal$building_sqft,
    focus_land_sqft = focal$land_sqft,
    focus_address = focal$source_address,
    neighbor_source = neighbors$source_family,
    neighbor_project_id = neighbors$project_id,
    neighbor_project_kind = neighbors$project_kind,
    neighbor_component_pins = neighbors$component_pins,
    neighbor_year = neighbors$construction_year,
    neighbor_units = neighbors$dwelling_units,
    neighbor_building_sqft = neighbors$building_sqft,
    neighbor_land_sqft = neighbors$land_sqft,
    neighbor_address = neighbors$source_address,
    year_gap = abs(focal$construction_year - neighbors$construction_year),
    polygon_distance_ft = NA_real_,
    centroid_distance_ft = NA_real_,
    intersection_area_sqft = NA_real_,
    smaller_polygon_overlap_share = NA_real_,
    shared_component_pin = TRUE,
    same_positive_units = is.finite(focal$dwelling_units) & focal$dwelling_units > 0 &
      is.finite(neighbors$dwelling_units) & neighbors$dwelling_units == focal$dwelling_units,
    same_positive_building_sqft = is.finite(focal$building_sqft) &
      focal$building_sqft > 0 &
      is.finite(neighbors$building_sqft) &
      neighbors$building_sqft == focal$building_sqft,
    overlap_review_reason = "shared_component_pin"
  )
})

pairs <- bind_rows(component_pairs, spatial_pairs) %>%
  arrange(
    focus_source,
    focus_project_id,
    neighbor_source,
    neighbor_project_id,
    desc(overlap_review_reason == "shared_component_pin")
  ) %>%
  distinct(
    focus_source,
    focus_project_id,
    neighbor_source,
    neighbor_project_id,
    .keep_all = TRUE
  )

if (anyDuplicated(pairs[c(
  "focus_source", "focus_project_id", "neighbor_source", "neighbor_project_id"
)]) > 0) {
  stop("Project overlap evidence contains duplicate directed pairs.", call. = FALSE)
}

project_evidence <- pairs %>%
  group_by(focus_source, focus_project_id) %>%
  summarise(
    overlap_candidates = n_distinct(paste(neighbor_source, neighbor_project_id)),
    project_overlap_evidence = paste0(
      neighbor_source, ":", neighbor_project_id,
      " [", overlap_review_reason, "]",
      "; year=", neighbor_year,
      "; units=", coalesce(as.character(neighbor_units), "missing"),
      "; building_sqft=", coalesce(as.character(neighbor_building_sqft), "missing"),
      "; land_sqft=", coalesce(as.character(neighbor_land_sqft), "missing"),
      "; address=", coalesce(neighbor_address, "missing"),
      "; polygon_distance_ft=", round(polygon_distance_ft, 2),
      "; overlap_share=", round(smaller_polygon_overlap_share, 3),
      collapse = " || "
    ),
    .groups = "drop"
  ) %>%
  rename(source_family = focus_source, project_id = focus_project_id)

project_evidence <- review_scope %>%
  left_join(
    project_evidence,
    by = c("source_family", "project_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(overlap_candidates = coalesce(overlap_candidates, 0L))

summary <- bind_rows(
  project_evidence %>%
    count(source_family, overlap_candidates, name = "value") %>%
    transmute(metric = paste(source_family, overlap_candidates, sep = ":"), value),
  tibble::tibble(
    metric = c(
      "candidate_projects",
      "candidate_projects_with_geometry",
      "candidate_projects_without_geometry",
      "review_projects",
      "directed_overlap_pairs"
    ),
    value = c(
      nrow(candidates),
      nrow(geometries),
      nrow(candidates) - nrow(geometries),
      nrow(review_scope),
      nrow(pairs)
    )
  )
)

readr::write_csv(pairs, "../output/project_overlap_pairs.csv")
readr::write_csv(project_evidence, "../output/project_overlap_evidence.csv")
readr::write_csv(summary, "../output/project_overlap_summary.csv")
