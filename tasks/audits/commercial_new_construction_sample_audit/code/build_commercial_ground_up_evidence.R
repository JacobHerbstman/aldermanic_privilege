# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

projects <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  filter(
    source_family == "commercial",
    between(target_year, 2006L, 2022L)
  )

candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(between(construction_year, 2006L, 2022L))

footprints <- sf::st_read(
  "../output/commercial_city_building_footprints.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)

residential_construction_pattern <- regex(
  paste(
    "DWELLING|RESIDENTIAL|APARTMENT|MULTI[- ]?FAMILY|",
    "HOUSING|DORMITOR|SENIOR LIVING|",
    "\\b[0-9]{1,4}\\s*[- ]?\\s*(?:TOTAL\\s+)?(?:UNITS?|D\\.?U\\.?)\\b",
    sep = ""
  ),
  ignore_case = TRUE
)

normalize_address <- function(x) {
  str_to_upper(coalesce(as.character(x), "")) %>%
    str_replace_all("\\bCHICAGO\\b", "") %>%
    str_replace_all("[^A-Z0-9 ]", " ") %>%
    str_replace_all(
      "\\b(STREET|ST|AVENUE|AVE|ROAD|RD|BOULEVARD|BLVD|COURT|CT|PLACE|PL|DRIVE|DR)\\b",
      ""
    ) %>%
    str_squish()
}

parse_address_range <- function(x) {
  normalized <- normalize_address(x)
  parts <- str_match(normalized, "^([0-9]+)(?:\\s+([0-9]+))?\\s+(.+)$")
  first_number <- suppressWarnings(as.integer(parts[, 2]))
  second_number <- suppressWarnings(as.integer(parts[, 3]))
  tibble::tibble(
    address_number_min = pmin(first_number, coalesce(second_number, first_number)),
    address_number_max = pmax(first_number, coalesce(second_number, first_number)),
    street_key = str_squish(parts[, 4])
  )
}

address_permits <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  sf::st_drop_geometry() %>%
  filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    permit_status == "COMPLETE",
    !is.na(issue_date),
    !is.na(application_start_date),
    str_detect(coalesce(work_description, ""), residential_construction_pattern)
  ) %>%
  transmute(
    permit_id = as.character(id),
    permit_number = as.character(permit),
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    application_year = lubridate::year(as.Date(application_start_date)),
    issue_year = lubridate::year(as.Date(issue_date)),
    work_description
  ) %>%
  bind_cols(parse_address_range(.$permit_address)) %>%
  filter(is.finite(address_number_min), street_key != "") %>%
  distinct(permit_id, address_number_min, street_key, .keep_all = TRUE)

exact_permits <- readr::read_csv(
  "../output/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    source_family == "commercial",
    permit_status == "COMPLETE",
    plausible_application_window,
    plausible_issue_window,
    str_detect(coalesce(work_description, ""), residential_construction_pattern)
  ) %>%
  inner_join(
    candidates %>% select(project_id, construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) %>%
  filter(
    between(construction_year - application_year, -2L, 6L),
    is.na(issue_year) | between(construction_year - issue_year, -2L, 4L)
  ) %>%
  group_by(project_id) %>%
  summarise(
    exact_new_construction_permits = n_distinct(permit_id),
    exact_new_construction_permit_numbers = paste(
      sort(unique(permit_number)), collapse = "/"
    ),
    .groups = "drop"
  )

inside_permits <- readr::read_csv(
  "../output/new_construction_spatial_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    source_family == "commercial",
    permit_status == "COMPLETE",
    polygon_distance_ft == 0,
    str_detect(coalesce(work_description, ""), residential_construction_pattern)
  ) %>%
  inner_join(
    candidates %>% select(project_id, construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) %>%
  filter(target_year == construction_year) %>%
  group_by(project_id) %>%
  summarise(
    inside_new_construction_permits = n_distinct(permit_id),
    inside_new_construction_permit_numbers = paste(
      sort(unique(permit_number)), collapse = "/"
    ),
    .groups = "drop"
  )

if (anyDuplicated(projects[c("project_id", "target_year")]) > 0) {
  stop("Commercial project geometry is not unique by project and year.", call. = FALSE)
}
if (anyDuplicated(candidates$project_id) > 0) {
  stop("Commercial candidates are not unique by project.", call. = FALSE)
}
if (anyDuplicated(exact_permits$project_id) > 0 || anyDuplicated(inside_permits$project_id) > 0) {
  stop("Commercial permit summaries are not unique by project.", call. = FALSE)
}
intersection_index <- sf::st_intersects(projects, footprints)
footprint_links <- purrr::map2_dfr(
  seq_len(nrow(projects)),
  intersection_index,
  function(project_row, footprint_rows) {
    if (length(footprint_rows) == 0) {
      return(tibble::tibble())
    }
    project <- projects[project_row, ]
    candidate_footprints <- footprints[footprint_rows, ]
    footprint_centers <- suppressWarnings(sf::st_point_on_surface(candidate_footprints))
    center_within_project <- lengths(sf::st_within(
      footprint_centers,
      sf::st_geometry(project)
    )) > 0
    intersections <- suppressWarnings(sf::st_intersection(
      candidate_footprints,
      sf::st_geometry(project)
    ))
    if (nrow(intersections) == 0) {
      return(tibble::tibble())
    }
    sf::st_drop_geometry(intersections) %>%
      left_join(
        tibble::tibble(
          footprint_id = candidate_footprints$footprint_id,
          footprint_center_within_project = center_within_project
        ),
        by = "footprint_id",
        relationship = "many-to-one"
      ) %>%
      transmute(
        project_id = project$project_id,
        target_year = project$target_year,
        footprint_id,
        city_address,
        city_year_built,
        city_units,
        city_building_sqft,
        footprint_area_sqft = city_shape_area_sqft,
        overlap_area_sqft = as.numeric(sf::st_area(intersections)),
        footprint_overlap_share = overlap_area_sqft / footprint_area_sqft,
        footprint_center_within_project
      )
  }
) %>%
  filter(footprint_overlap_share >= 0.5 | footprint_center_within_project) %>%
  arrange(project_id, desc(overlap_area_sqft), footprint_id)

source_addresses <- candidates %>%
  select(project_id, construction_year, project_address = selected_source_addresses) %>%
  mutate(address_source = "commercial_assessor") %>%
  tidyr::separate_rows(project_address, sep = "\\s+/\\s+")

footprint_addresses <- footprint_links %>%
  distinct(project_id, target_year, city_address) %>%
  transmute(
    project_id,
    construction_year = target_year,
    project_address = city_address,
    address_source = "city_building_footprint"
  )

project_addresses <- bind_rows(source_addresses, footprint_addresses) %>%
  bind_cols(parse_address_range(.$project_address)) %>%
  filter(
    is.finite(address_number_min),
    is.finite(address_number_max),
    street_key != ""
  ) %>%
  distinct(
    project_id,
    construction_year,
    address_source,
    address_number_min,
    address_number_max,
    street_key
  )

permits_by_street <- split(address_permits, address_permits$street_key)
address_permit_matches <- purrr::map_dfr(seq_len(nrow(project_addresses)), function(i) {
  project_address <- project_addresses[i, ]
  matched <- permits_by_street[[project_address$street_key]]
  if (is.null(matched) || nrow(matched) == 0) {
    return(tibble::tibble())
  }
  matched <- matched %>%
    filter(
      address_number_min >= project_address$address_number_min,
      address_number_min <= project_address$address_number_max
    )
  if (nrow(matched) == 0) {
    return(tibble::tibble())
  }
  bind_cols(
    project_address[rep(1, nrow(matched)), ],
    matched %>% select(-address_number_min, -address_number_max, -street_key)
  )
}) %>%
  filter(
    between(construction_year - application_year, -2L, 6L),
    is.na(issue_year) | between(construction_year - issue_year, -2L, 4L)
  ) %>%
  distinct(project_id, permit_id, .keep_all = TRUE) %>%
  arrange(project_id, application_year, permit_number)

if (anyDuplicated(address_permit_matches[c("project_id", "permit_id")]) > 0) {
  stop("Commercial address-permit matches are not unique by project and permit.", call. = FALSE)
}

address_permit_summary <- address_permit_matches %>%
  group_by(project_id) %>%
  summarise(
    address_new_construction_permits = n_distinct(permit_id),
    address_new_construction_permit_numbers = paste(
      sort(unique(permit_number)), collapse = "/"
    ),
    address_new_construction_sources = paste(
      sort(unique(address_source)), collapse = "/"
    ),
    .groups = "drop"
  )

if (anyDuplicated(address_permit_summary$project_id) > 0) {
  stop("Commercial address-permit summaries are not unique by project.", call. = FALSE)
}

chain_permit_summary <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    source_family == "commercial",
    permit_status == "COMPLETE",
    str_detect(coalesce(work_description, ""), residential_construction_pattern)
  ) %>%
  inner_join(
    candidates %>% select(project_id, construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    application_year = lubridate::year(as.Date(application_date)),
    issue_year = lubridate::year(as.Date(issue_date))
  ) %>%
  filter(
    between(construction_year - application_year, -2L, 6L),
    between(construction_year - issue_year, -2L, 4L)
  ) %>%
  group_by(project_id) %>%
  summarise(
    chain_new_construction_permits = n_distinct(permit_number),
    chain_new_construction_permit_numbers = paste(
      sort(unique(permit_number)), collapse = "/"
    ),
    .groups = "drop"
  )

if (anyDuplicated(chain_permit_summary$project_id) > 0) {
  stop("Commercial permit-chain summaries are not unique by project.", call. = FALSE)
}

footprint_summary <- footprint_links %>%
  group_by(project_id, target_year) %>%
  summarise(
    matched_city_footprints = n_distinct(footprint_id),
    city_footprint_ids = paste(footprint_id, collapse = "/"),
    city_footprint_addresses = paste(city_address, collapse = " / "),
    city_year_built_values = paste(
      sort(unique(city_year_built[is.finite(city_year_built) & city_year_built > 0])),
      collapse = "/"
    ),
    city_footprint_area_sqft = sum(footprint_area_sqft),
    city_footprint_area_with_year_sqft = sum(
      footprint_area_sqft[is.finite(city_year_built) & city_year_built > 0]
    ),
    city_footprint_area_near_target_sqft = sum(
      footprint_area_sqft[
        is.finite(city_year_built) & abs(city_year_built - target_year) <= 2
      ]
    ),
    city_footprint_area_old_sqft = sum(
      footprint_area_sqft[
        is.finite(city_year_built) & city_year_built > 0 & city_year_built < target_year - 5
      ]
    ),
    .groups = "drop"
  ) %>%
  mutate(
    city_year_coverage_share = if_else(
      city_footprint_area_sqft > 0,
      city_footprint_area_with_year_sqft / city_footprint_area_sqft,
      NA_real_
    ),
    city_near_target_share = if_else(
      city_footprint_area_with_year_sqft > 0,
      city_footprint_area_near_target_sqft / city_footprint_area_with_year_sqft,
      NA_real_
    ),
    city_old_building_share = if_else(
      city_footprint_area_with_year_sqft > 0,
      city_footprint_area_old_sqft / city_footprint_area_with_year_sqft,
      NA_real_
    )
  )

ground_up_evidence <- candidates %>%
  left_join(
    sf::st_drop_geometry(projects) %>% select(project_id, target_year),
    by = c("project_id", "construction_year" = "target_year"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    footprint_summary,
    by = c("project_id", "construction_year" = "target_year"),
    relationship = "one-to-one"
  ) %>%
  left_join(exact_permits, by = "project_id", relationship = "one-to-one") %>%
  left_join(inside_permits, by = "project_id", relationship = "one-to-one") %>%
  left_join(address_permit_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(chain_permit_summary, by = "project_id", relationship = "one-to-one") %>%
  mutate(
    matched_city_footprints = coalesce(matched_city_footprints, 0L),
    exact_new_construction_permits = coalesce(exact_new_construction_permits, 0L),
    inside_new_construction_permits = coalesce(inside_new_construction_permits, 0L),
    address_new_construction_permits = coalesce(address_new_construction_permits, 0L),
    chain_new_construction_permits = coalesce(chain_new_construction_permits, 0L),
    has_issued_new_construction_permit =
      exact_new_construction_permits > 0 |
      inside_new_construction_permits > 0 |
      address_new_construction_permits > 0 |
      chain_new_construction_permits > 0,
    strong_city_ground_up_evidence =
      city_year_coverage_share >= 0.75 & city_near_target_share >= 0.75,
    strong_city_old_building_evidence =
      city_year_coverage_share >= 0.75 & city_old_building_share >= 0.75,
    city_year_conflicts_with_permit =
      has_issued_new_construction_permit & strong_city_old_building_evidence,
    ground_up_status = case_when(
      has_issued_new_construction_permit & strong_city_ground_up_evidence ~
        "confirmed_permit_and_city_building_year",
      has_issued_new_construction_permit ~
        "confirmed_issued_new_construction_permit",
      strong_city_ground_up_evidence ~
        "supported_by_city_building_year",
      strong_city_old_building_evidence ~
        "likely_adaptive_reuse_or_year_recode",
      matched_city_footprints == 0 ~
        "review_no_city_building_footprint",
      TRUE ~ "review_incomplete_or_mixed_city_building_year"
    ),
    ground_up_review_required = str_starts(ground_up_status, "review_") |
      ground_up_status == "likely_adaptive_reuse_or_year_recode"
  ) %>%
  arrange(desc(current_within_1500ft), desc(current_distance_m <= 152.4), project_id)

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
ground_up_review <- ground_up_evidence %>%
  filter(ground_up_review_required)
if (any(str_detect(names(ground_up_review), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Commercial ground-up review contains a prohibited analysis field.", call. = FALSE)
}

summary <- bind_rows(
  ground_up_evidence %>%
    count(ground_up_status, name = "value") %>%
    transmute(metric = paste0("all:", ground_up_status), value),
  ground_up_evidence %>%
    filter(current_within_1500ft) %>%
    count(ground_up_status, name = "value") %>%
    transmute(metric = paste0("within_1500ft:", ground_up_status), value),
  ground_up_evidence %>%
    filter(current_distance_m <= 152.4) %>%
    count(ground_up_status, name = "value") %>%
    transmute(metric = paste0("within_500ft:", ground_up_status), value)
)

readr::write_csv(
  footprint_links,
  "../output/commercial_city_building_footprint_links.csv"
)
readr::write_csv(
  address_permit_matches,
  "../output/commercial_address_permit_matches.csv"
)
readr::write_csv(
  ground_up_evidence,
  "../output/commercial_ground_up_evidence.csv"
)
readr::write_csv(
  ground_up_review,
  "../output/commercial_ground_up_review.csv"
)
readr::write_csv(
  summary,
  "../output/commercial_ground_up_summary.csv"
)
