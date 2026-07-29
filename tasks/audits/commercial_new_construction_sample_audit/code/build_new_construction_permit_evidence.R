# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin <- function(x) {
  digits <- str_replace_all(str_squish(as.character(x)), "[^0-9]", "")
  if_else(str_length(digits) == 14, digits, NA_character_)
}

finite_min <- function(x) {
  value <- suppressWarnings(min(x[is.finite(x)], na.rm = TRUE))
  if (is.infinite(value)) NA_real_ else value
}

finite_max <- function(x) {
  value <- suppressWarnings(max(x[is.finite(x)], na.rm = TRUE))
  if (is.infinite(value)) NA_real_ else value
}

extract_unit_mentions <- function(permit_id, permit_number, work_description) {
  text <- str_to_upper(coalesce(work_description, ""))
  locations <- str_locate_all(
    text,
    "\\b[0-9]{1,4}\\s*(?:TOTAL\\s+)?(?:DWELLING\\s+|RESIDENTIAL\\s+|APARTMENT\\s+|EFFICIENCY\\s+)?(?:UNITS?|D\\.?U\\.?)\\b"
  )[[1]]

  if (nrow(locations) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    permit_id = permit_id,
    permit_number = permit_number,
    mention_order = seq_len(nrow(locations)),
    unit_mention = str_sub(text, locations[, "start"], locations[, "end"]),
    unit_count = suppressWarnings(as.numeric(str_extract(unit_mention, "[0-9]{1,4}"))),
    mention_context = purrr::map2_chr(
      locations[, "start"],
      locations[, "end"],
      ~ str_squish(str_sub(text, max(1, .x - 90), min(str_length(text), .y + 90)))
    )
  )
}

residential <- readr::read_csv(
  "../output/residential_project_candidate_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    tieback_lineage_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

tieback_members <- readr::read_csv(
  "../output/residential_tieback_members_full.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    tieback_group = readr::col_character(),
    .default = readr::col_guess()
  )
)

tieback_groups <- readr::read_csv(
  "../output/residential_tieback_groups_full.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    tieback_group = readr::col_character(),
    tieback_lineage_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

residential_non_tieback_components <- residential %>%
  filter(review_category != "tieback") %>%
  transmute(
    source_family = "residential",
    project_id = paste0("residential_", pin),
    component_pin = pin,
    candidate_year_min = as.integer(year_built),
    candidate_year_max = as.integer(year_built),
    candidate_units_min = as.numeric(num_apartments),
    candidate_units_max = as.numeric(num_apartments),
    candidate_building_sqft_min = as.numeric(building_sqft),
    candidate_building_sqft_max = as.numeric(building_sqft),
    candidate_land_sqft_min = as.numeric(land_sqft),
    candidate_land_sqft_max = as.numeric(land_sqft),
    review_category,
    within_1500ft
  )

residential_tieback_components <- tieback_members %>%
  left_join(
    tieback_groups %>% select(tieback_group, tieback_lineage_id),
    by = "tieback_group",
    relationship = "many-to-one"
  ) %>%
  group_by(tieback_lineage_id, pin) %>%
  summarise(
    candidate_year_min = finite_min(year_built),
    candidate_year_max = finite_max(year_built),
    candidate_units_min = finite_min(num_apartments),
    candidate_units_max = finite_max(num_apartments),
    candidate_building_sqft_min = finite_min(building_sqft),
    candidate_building_sqft_max = finite_max(building_sqft),
    candidate_land_sqft_min = finite_min(land_sqft),
    candidate_land_sqft_max = finite_max(land_sqft),
    within_1500ft = any(within_1500ft %in% TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    source_family = "residential",
    project_id = tieback_lineage_id,
    component_pin = pin,
    candidate_year_min = as.integer(candidate_year_min),
    candidate_year_max = as.integer(candidate_year_max),
    candidate_units_min,
    candidate_units_max,
    candidate_building_sqft_min,
    candidate_building_sqft_max,
    candidate_land_sqft_min,
    candidate_land_sqft_max,
    review_category = "tieback",
    within_1500ft
  )

commercial_components <- readr::read_csv(
  "../output/commercial_entity_component_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  distinct(project_family_id, component_pin)

commercial_members <- readr::read_csv(
  "../output/commercial_production_family_members.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    keypin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  group_by(project_family_id) %>%
  summarise(
    candidate_year_min = finite_min(production_yearbuilt),
    candidate_year_max = finite_max(production_yearbuilt),
    candidate_units_min = finite_min(production_units),
    candidate_units_max = finite_max(production_units),
    candidate_building_sqft_min = finite_min(production_bldgsf),
    candidate_building_sqft_max = finite_max(production_bldgsf),
    candidate_land_sqft_min = finite_min(production_landsf),
    candidate_land_sqft_max = finite_max(production_landsf),
    within_1500ft = any(within_1500ft %in% TRUE),
    .groups = "drop"
  )

commercial_project_components <- commercial_components %>%
  inner_join(commercial_members, by = "project_family_id", relationship = "many-to-one") %>%
  transmute(
    source_family = "commercial",
    project_id = project_family_id,
    component_pin,
    candidate_year_min = as.integer(candidate_year_min),
    candidate_year_max = as.integer(candidate_year_max),
    candidate_units_min,
    candidate_units_max,
    candidate_building_sqft_min,
    candidate_building_sqft_max,
    candidate_land_sqft_min,
    candidate_land_sqft_max,
    review_category = "commercial_entity",
    within_1500ft
  )

project_components <- bind_rows(
  residential_non_tieback_components,
  residential_tieback_components,
  commercial_project_components
) %>%
  mutate(
    component_pin = normalize_pin(component_pin),
    pin10 = str_sub(component_pin, 1, 10)
  ) %>%
  filter(!is.na(component_pin)) %>%
  distinct(source_family, project_id, component_pin, .keep_all = TRUE) %>%
  arrange(source_family, project_id, component_pin)

if (anyDuplicated(project_components[c("source_family", "project_id", "component_pin")]) > 0) {
  stop("Project-component evidence keys are not unique.", call. = FALSE)
}

permits_sf <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435) %>%
  filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    !is.na(application_start_date),
    !is.na(issue_date)
  )

permit_coordinates <- sf::st_coordinates(permits_sf)
permits <- permits_sf %>%
  sf::st_drop_geometry() %>%
  mutate(
    permit_id = as.character(id),
    permit_number = as.character(permit),
    permit_x_3435 = permit_coordinates[, "X"],
    permit_y_3435 = permit_coordinates[, "Y"],
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
    pin,
    application_date,
    issue_date,
    application_year,
    issue_year,
    permit_status,
    permit_address,
    referenced_permit_numbers,
    permit_x_3435,
    permit_y_3435,
    work_description
  ) %>%
  separate_rows(pin, sep = "\\s*\\|\\s*") %>%
  mutate(pin10 = str_replace_all(pin, "[^0-9]", "")) %>%
  filter(str_detect(pin10, "^[0-9]{10}$")) %>%
  distinct(permit_id, pin10, .keep_all = TRUE)

permits_by_pin <- split(permits, permits$pin10)

exact_permit_matches <- purrr::map_dfr(
  seq_len(nrow(project_components)),
  function(i) {
    candidate <- project_components[i, ]
    matched <- permits_by_pin[[candidate$pin10]]
    if (is.null(matched) || nrow(matched) == 0) {
      return(tibble::tibble())
    }
    bind_cols(
      candidate[rep(1, nrow(matched)), ],
      matched %>% select(-pin10),
      tibble::tibble(match_method = "exact_component_pin10")
    )
  }
) %>%
  mutate(
    plausible_application_window =
      application_year >= candidate_year_min - 6 &
      application_year <= candidate_year_max + 2,
    plausible_issue_window =
      is.na(issue_year) |
      (issue_year >= candidate_year_min - 4 & issue_year <= candidate_year_max + 2)
  ) %>%
  distinct(source_family, project_id, component_pin, permit_id, .keep_all = TRUE) %>%
  arrange(source_family, project_id, application_date, permit_id)

permit_unit_mentions_base <- exact_permit_matches %>%
  distinct(permit_id, permit_number, work_description) %>%
  purrr::pmap_dfr(extract_unit_mentions)

unit_mentions_by_permit <- split(permit_unit_mentions_base, permit_unit_mentions_base$permit_id)
project_permit_links <- exact_permit_matches %>%
  select(source_family, project_id, component_pin, permit_id) %>%
  distinct()

permit_unit_mentions <- purrr::map_dfr(
  seq_len(nrow(project_permit_links)),
  function(i) {
    link <- project_permit_links[i, ]
    mentions <- unit_mentions_by_permit[[link$permit_id]]
    if (is.null(mentions) || nrow(mentions) == 0) {
      return(tibble::tibble())
    }
    bind_cols(
      link[rep(1, nrow(mentions)), ],
      mentions %>% select(-permit_id)
    )
  }
) %>%
  select(
    source_family,
    project_id,
    component_pin,
    permit_id,
    permit_number,
    mention_order,
    unit_mention,
    unit_count,
    mention_context
  ) %>%
  arrange(source_family, project_id, permit_id, mention_order)

project_permit_summary <- project_components %>%
  group_by(source_family, project_id) %>%
  summarise(
    review_category = first(review_category),
    component_pins = n_distinct(component_pin),
    candidate_year_min = finite_min(candidate_year_min),
    candidate_year_max = finite_max(candidate_year_max),
    within_1500ft = any(within_1500ft %in% TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    exact_permit_matches %>%
      group_by(source_family, project_id) %>%
      summarise(
        exact_permits = n_distinct(permit_id),
        plausible_exact_permits = n_distinct(
          permit_id[plausible_application_window & plausible_issue_window]
        ),
        exact_permit_ids = paste(sort(unique(permit_id)), collapse = "/"),
        exact_permit_numbers = paste(sort(unique(permit_number)), collapse = "/"),
        .groups = "drop"
      ),
    by = c("source_family", "project_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    exact_permits = coalesce(exact_permits, 0L),
    plausible_exact_permits = coalesce(plausible_exact_permits, 0L),
    exact_permit_ids = coalesce(exact_permit_ids, ""),
    exact_permit_numbers = coalesce(exact_permit_numbers, "")
  )

summary <- bind_rows(
  project_permit_summary %>%
    count(source_family, name = "value") %>%
    transmute(metric = paste0(source_family, "_project_candidates"), value),
  project_permit_summary %>%
    filter(within_1500ft) %>%
    count(source_family, name = "value") %>%
    transmute(metric = paste0(source_family, "_project_candidates_within_1500ft"), value),
  project_permit_summary %>%
    filter(within_1500ft, plausible_exact_permits > 0) %>%
    count(source_family, name = "value") %>%
    transmute(metric = paste0(source_family, "_within_1500ft_with_plausible_exact_permit"), value),
  tibble::tibble(metric = "exact_project_permit_links", value = nrow(exact_permit_matches)),
  tibble::tibble(metric = "unit_mentions_preserved", value = nrow(permit_unit_mentions))
)

readr::write_csv(summary, "../output/new_construction_permit_evidence_summary.csv")
readr::write_csv(project_components, "../output/new_construction_project_components.csv")
readr::write_csv(project_permit_summary, "../output/new_construction_project_permit_summary.csv")
readr::write_csv(exact_permit_matches, "../output/new_construction_exact_permit_matches.csv")
readr::write_csv(permit_unit_mentions, "../output/new_construction_permit_unit_mentions.csv")
