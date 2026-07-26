# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

split_pins <- function(x) {
  if (is.na(x) || x == "") character() else str_split(x, "/", simplify = FALSE)[[1]]
}

queue <- readr::read_csv(
  "../output/commercial_adjudication_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

vintages <- readr::read_csv(
  "../output/commercial_family_vintage_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    source_keypins = readr::col_character(),
    component_pin_list = readr::col_character(),
    source_yearbuilt = readr::col_character(),
    source_units = readr::col_character(),
    source_building_areas = readr::col_character(),
    source_land_areas = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(project_family_id %in% queue$project_id)

entity_rows <- readr::read_csv(
  "../output/commercial_entity_version_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    keypin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(project_family_id %in% queue$project_id)

permit_summary <- readr::read_csv(
  "../output/project_permit_chain_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_numbers = readr::col_character(),
    unit_counts = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "commercial", project_id %in% queue$project_id) %>%
  select(-source_family)

permit_mentions <- readr::read_csv(
  "../output/project_permit_chain_unit_mentions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    mention_context = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "commercial", project_id %in% queue$project_id) %>%
  group_by(project_id) %>%
  summarise(
    permit_unit_evidence = paste(
      unique(paste0(permit_number, ": ", unit_count, " [", mention_context, "]")),
      collapse = " || "
    ),
    .groups = "drop"
  )

geography <- readr::read_csv(
  "../output/preferred_project_boundary_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(source_family == "commercial", project_id %in% queue$project_id) %>%
  select(
    project_id,
    geography_status,
    requested_components,
    resolved_components,
    collapsed_components,
    project_land_area_sqft,
    distance_to_boundary_ft,
    within_1500ft
  )

component_geometry <- sf::st_read(
  "../output/preferred_project_component_geometry.gpkg",
  quiet = TRUE
) %>%
  filter(source_family == "commercial", project_id %in% queue$project_id)

review_scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(source_family == "commercial") %>%
  select(project_id, review_scope)

vintage_rows <- entity_rows %>%
  group_by(project_family_id, valuation_year) %>%
  summarise(
    source_row_ids = paste(sort(unique(raw_row)), collapse = "/"),
    source_addresses = paste(sort(unique(address)), collapse = " / "),
    property_descriptions = paste(
      sort(unique(na.omit(c(property_type_use, property_name_description)))),
      collapse = " / "
    ),
    .groups = "drop"
  )

vintages <- vintages %>%
  left_join(
    vintage_rows,
    by = c("project_family_id", "valuation_year"),
    relationship = "one-to-one"
  )

vintage_2021 <- vintages %>%
  filter(valuation_year == 2021) %>%
  select(-valuation_year) %>%
  rename_with(~ paste0(.x, "_2021"), -project_family_id)

vintage_2024 <- vintages %>%
  filter(valuation_year == 2024) %>%
  select(-valuation_year) %>%
  rename_with(~ paste0(.x, "_2024"), -project_family_id)

vintage_topology <- queue %>%
  select(project_id) %>%
  left_join(
    vintage_2021 %>% select(project_family_id, component_pin_list_2021),
    by = c("project_id" = "project_family_id"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    vintage_2024 %>% select(project_family_id, component_pin_list_2024),
    by = c("project_id" = "project_family_id"),
    relationship = "one-to-one"
  ) %>%
  pmap_dfr(function(project_id, component_pin_list_2021, component_pin_list_2024) {
    pins_2021 <- split_pins(component_pin_list_2021)
    pins_2024 <- split_pins(component_pin_list_2024)
    geometry_2021 <- component_geometry %>%
      filter(.data$project_id == project_id, component_pin %in% pins_2021)
    geometry_2024 <- component_geometry %>%
      filter(.data$project_id == project_id, component_pin %in% pins_2024)
    coverage_2021 <- n_distinct(geometry_2021$component_pin) == length(unique(pins_2021))
    coverage_2024 <- n_distinct(geometry_2024$component_pin) == length(unique(pins_2024))

    if (length(pins_2021) == 0 || length(pins_2024) == 0 ||
        !coverage_2021 || !coverage_2024) {
      return(tibble::tibble(
        project_id,
        vintage_geometry_complete_2021 = coverage_2021,
        vintage_geometry_complete_2024 = coverage_2024,
        vintage_land_area_sqft_2021 = NA_real_,
        vintage_land_area_sqft_2024 = NA_real_,
        vintage_geometry_intersection_over_union = NA_real_,
        vintage_geometry_equivalent = NA
      ))
    }

    union_2021 <- sf::st_union(sf::st_geometry(geometry_2021))
    union_2024 <- sf::st_union(sf::st_geometry(geometry_2024))
    area_2021 <- as.numeric(sf::st_area(union_2021))
    area_2024 <- as.numeric(sf::st_area(union_2024))
    intersection_area <- as.numeric(sf::st_area(sf::st_intersection(union_2021, union_2024)))
    union_area <- as.numeric(sf::st_area(sf::st_union(union_2021, union_2024)))
    intersection_over_union <- intersection_area / union_area

    tibble::tibble(
      project_id,
      vintage_geometry_complete_2021 = coverage_2021,
      vintage_geometry_complete_2024 = coverage_2024,
      vintage_land_area_sqft_2021 = area_2021,
      vintage_land_area_sqft_2024 = area_2024,
      vintage_geometry_intersection_over_union = intersection_over_union,
      vintage_geometry_equivalent =
        is.finite(intersection_over_union) && intersection_over_union >= 0.999999
    )
  })

evidence <- queue %>%
  select(
    project_id,
    decision_reason,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    documented_case_pins,
    documented_case_actions,
    documented_case_evidence,
    documented_case_confidence
  ) %>%
  left_join(
    vintage_2021,
    by = c("project_id" = "project_family_id"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    vintage_2024,
    by = c("project_id" = "project_family_id"),
    relationship = "one-to-one"
  ) %>%
  left_join(permit_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(permit_mentions, by = "project_id", relationship = "one-to-one") %>%
  left_join(geography, by = "project_id", relationship = "one-to-one") %>%
  left_join(review_scope, by = "project_id", relationship = "one-to-one") %>%
  left_join(vintage_topology, by = "project_id", relationship = "one-to-one") %>%
  rowwise() %>%
  mutate(
    component_relation = {
      pins_2021 <- split_pins(component_pin_list_2021)
      pins_2024 <- split_pins(component_pin_list_2024)
      case_when(
        length(pins_2021) == 0 ~ "2024_only",
        length(pins_2024) == 0 ~ "2021_only",
        setequal(pins_2021, pins_2024) ~ "same",
        all(pins_2021 %in% pins_2024) ~ "2021_subset_2024",
        all(pins_2024 %in% pins_2021) ~ "2024_subset_2021",
        length(intersect(pins_2021, pins_2024)) > 0 ~ "overlap_changed",
        TRUE ~ "disjoint"
      )
    },
    component_pins_added_2024 = paste(
      setdiff(split_pins(component_pin_list_2024), split_pins(component_pin_list_2021)),
      collapse = "/"
    ),
    component_pins_dropped_2024 = paste(
      setdiff(split_pins(component_pin_list_2021), split_pins(component_pin_list_2024)),
      collapse = "/"
    ),
    construction_year_land_ratio = if_else(
      is.finite(land_sqft) & land_sqft > 0 & is.finite(project_land_area_sqft),
      project_land_area_sqft / land_sqft,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  arrange(
    factor(
      review_scope,
      levels = c(
        "review_within_1500ft",
        "review_geography_unresolved",
        "mechanical_rule_outside_1500ft"
      )
    ),
    decision_reason,
    project_id
  )

if (anyDuplicated(evidence$project_id) > 0) {
  stop("Commercial adjudication evidence is not unique by project.", call. = FALSE)
}
if (nrow(evidence) != nrow(queue)) {
  stop("Commercial adjudication evidence does not contain every queued project.", call. = FALSE)
}

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(evidence), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Commercial adjudication evidence contains a prohibited analysis field.", call. = FALSE)
}

summary <- bind_rows(
  evidence %>%
    count(review_scope, decision_reason, name = "value") %>%
    transmute(
      section = "review_queue",
      metric = paste(review_scope, decision_reason, sep = ":"),
      value
    ),
  tibble::tibble(
    section = "validation",
    metric = c(
      "queued_projects",
      "projects_with_permit_chains",
      "projects_with_permit_unit_mentions",
      "projects_with_complete_construction_year_geography",
      "duplicate_project_ids"
    ),
    value = c(
      nrow(evidence),
      sum(!is.na(evidence$permit_chains)),
      sum(!is.na(evidence$permit_unit_evidence)),
      sum(evidence$geography_status == "complete_construction_year_geometry", na.rm = TRUE),
      anyDuplicated(evidence$project_id)
    )
  )
)

readr::write_csv(evidence, "../output/commercial_adjudication_evidence.csv")
readr::write_csv(summary, "../output/commercial_adjudication_evidence_summary.csv")
