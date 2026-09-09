# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/assessor_classification.R")

single_finite_value <- function(x) {
  values <- sort(unique(x[is.finite(x)]))
  if (length(values) == 1) values else NA_real_
}

inventory <- readr::read_csv(
  "../output/residential_project_candidate_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    tieback_lineage_id = readr::col_character(),
    historical_tieback_groups = readr::col_character(),
    source_years = readr::col_character(),
    source_building_areas = readr::col_character(),
    source_land_areas = readr::col_character(),
    source_unit_counts = readr::col_character(),
    row_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

tieback_temporal <- readr::read_csv(
  "../output/residential_tieback_temporal_lineage_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    tieback_lineage_id = readr::col_character(),
    selected_component_pins = readr::col_character(),
    all_lineage_pins = readr::col_character(),
    selected_source_row_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

permit_links <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    application_date = readr::col_date(),
    issue_date = readr::col_date(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "residential")

permit_units <- readr::read_csv(
  "../output/project_permit_chain_unit_mentions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "residential")

if (anyDuplicated(inventory$pin) > 0) {
  stop("Residential candidate inventory is not unique by PIN.", call. = FALSE)
}
if (anyDuplicated(tieback_temporal$tieback_lineage_id) > 0) {
  stop("Tieback lineage input is not unique by lineage.", call. = FALSE)
}
permit_chain_evidence <- permit_links %>%
  group_by(project_id, permit_chain_id) %>%
  summarise(
    directly_matched_exact_pin = any(
      directly_matched & direct_match_method == "exact_pin",
      na.rm = TRUE
    ),
    earliest_application_date = min(application_date, na.rm = TRUE),
    permit_numbers = paste(sort(unique(permit_number)), collapse = "/"),
    .groups = "drop"
  ) %>%
  mutate(
    earliest_application_date = if_else(
      is.infinite(as.numeric(earliest_application_date)),
      as.Date(NA),
      earliest_application_date
    )
  )

permit_unit_evidence <- permit_units %>%
  group_by(project_id, permit_chain_id) %>%
  summarise(
    permit_unit_count = single_finite_value(unit_count),
    .groups = "drop"
  )

exact_permit_year <- permit_chain_evidence %>%
  filter(directly_matched_exact_pin) %>%
  group_by(project_id) %>%
  summarise(
    exact_permit_chains = n_distinct(permit_chain_id),
    exact_permit_chain_id = if_else(
      exact_permit_chains == 1,
      first(permit_chain_id),
      NA_character_
    ),
    exact_permit_application_date = if_else(
      exact_permit_chains == 1,
      first(earliest_application_date),
      as.Date(NA)
    ),
    exact_permit_numbers = if_else(
      exact_permit_chains == 1,
      first(permit_numbers),
      NA_character_
    ),
    .groups = "drop"
  )

inventory <- inventory %>%
  mutate(
    source_project_id = paste0("residential_", pin),
    assessor_single_family =
      str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE)) |
      type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      ),
    assessor_units = case_when(
      explicit_multifamily_one_unit_conflict ~ NA_real_,
      assessor_single_family & (is.na(num_apartments) | num_apartments == 0) ~ 1,
      TRUE ~ num_apartments
    )
  ) %>%
  left_join(
    exact_permit_year,
    by = c("source_project_id" = "project_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    exact_permit_chains = coalesce(exact_permit_chains, 0L),
    permit_year_correction =
      exact_permit_chains == 1 &
      !is.na(exact_permit_application_date) &
      year_built == lubridate::year(exact_permit_application_date) - 1L,
    preferred_year = if_else(
      permit_year_correction,
      lubridate::year(exact_permit_application_date),
      year_built
    ),
    year_source = if_else(
      permit_year_correction,
      paste0("issued_permit_chain:", exact_permit_chain_id),
      paste0("assessor_row:", row_id)
    )
  )

assessor_projects <- readr::read_csv(
  "../output/residential_assessor_project_candidates.csv",
  na = "NA",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(), component_pins = readr::col_character(),
    class_values = readr::col_character(), source_row_ids = readr::col_character(),
    construction_year = readr::col_integer(), component_count = readr::col_integer(),
    dwelling_units = readr::col_double(), building_sqft = readr::col_double(),
    land_sqft = readr::col_double(), current_distance_m = readr::col_double(),
    current_within_1500ft = readr::col_logical()
  )
)
stopifnot(nrow(readr::problems(assessor_projects)) == 0,
          !anyNA(assessor_projects$project_id), !anyDuplicated(assessor_projects$project_id))

tieback_pin_lineage <- tieback_temporal %>%
  select(tieback_lineage_id, pin = all_lineage_pins) %>%
  tidyr::separate_longer_delim(pin, delim = "/") %>%
  filter(!is.na(pin), pin != "") %>%
  distinct(pin, tieback_lineage_id)
stopifnot(!anyDuplicated(tieback_pin_lineage$pin))

ordinary_candidates <- assessor_projects %>%
  filter(project_kind == "single_pin_single_card") %>%
  left_join(
    inventory %>% select(source_project_id, preferred_year,
      corrected_year_source = year_source, exact_permit_chain_id,
      exact_permit_numbers, permit_year_correction),
    by = c("project_id" = "source_project_id"), relationship = "one-to-one"
  ) %>%
  mutate(
    construction_year = if_else(str_starts(year_source, "reviewed_construction_year:"),
      construction_year, preferred_year),
    year_source = if_else(str_starts(year_source, "reviewed_construction_year:"),
      year_source, corrected_year_source),
    permit_chain_ids = exact_permit_chain_id,
    permit_numbers = exact_permit_numbers,
    candidate_status = case_when(
      str_starts(candidate_status, "exclude_") ~ candidate_status,
      !between(construction_year, 2006L, 2022L) ~ "exclude_outside_period",
      !is.finite(dwelling_units) | dwelling_units <= 0 |
        !is.finite(building_sqft) | building_sqft <= 0 |
        !is.finite(land_sqft) | land_sqft <= 0 ~ "review_required",
      TRUE ~ "retain_mechanical"
    ),
    decision_reason = case_when(
      str_starts(candidate_status, "exclude_") ~ decision_reason,
      !between(construction_year, 2006L, 2022L) ~ "construction_year_outside_2006_2022",
      !is.finite(dwelling_units) | dwelling_units <= 0 ~ "missing_or_nonpositive_units",
      !is.finite(building_sqft) | building_sqft <= 0 ~ "missing_or_nonpositive_building_area",
      !is.finite(land_sqft) | land_sqft <= 0 ~ "missing_or_nonpositive_land_area",
      permit_year_correction ~ "single_exact_permit_chain_one_year_after_assessor_year",
      TRUE ~ "latest_single_card_assessor_report"
    )
  ) %>%
  select(project_id, source_family, project_kind, component_pins, component_count,
    construction_year, dwelling_units, building_sqft, land_sqft, class_values,
    source_row_ids, permit_chain_ids, permit_numbers, year_source, units_source,
    building_source, land_source, current_distance_m, current_within_1500ft,
    candidate_status, decision_reason)

tieback_candidates <- assessor_projects %>%
  filter(project_kind == "tieback_building") %>%
  mutate(permit_chain_ids = NA_character_, permit_numbers = NA_character_) %>%
  select(all_of(names(ordinary_candidates)))

multicard_candidates <- assessor_projects %>%
  filter(project_kind %in% c("same_pin_multiple_cards", "reviewed_multi_parcel_building")) %>%
  mutate(permit_chain_ids = NA_character_, permit_numbers = NA_character_) %>%
  select(all_of(names(ordinary_candidates)), replacement_project_ids, replacement_check)

class_297_rows <- inventory %>%
  filter(
    class == "297",
    !pin %in% tieback_pin_lineage$pin,
    !in_commercial_source
  ) %>%
  transmute(
    source_project_id,
    pin,
    row_id,
    year_built,
    building_sqft,
    land_sqft,
    num_apartments,
    dist_to_boundary_m,
    within_1500ft
  )

class_297_direct_chains <- permit_links %>%
  filter(
    project_id %in% class_297_rows$source_project_id,
    directly_matched
  ) %>%
  distinct(project_id, permit_chain_id)

if (nrow(class_297_direct_chains) > 0) {
  class_297_graph <- igraph::graph_from_data_frame(
    class_297_direct_chains %>%
      transmute(from = paste0("project:", project_id), to = paste0("chain:", permit_chain_id)),
    directed = FALSE
  )
  class_297_membership <- igraph::components(class_297_graph)$membership
  class_297_nodes <- tibble::tibble(
    node = names(class_297_membership),
    graph_component = as.integer(class_297_membership)
  )
  class_297_project_groups <- class_297_nodes %>%
    filter(str_starts(node, "project:")) %>%
    transmute(
      source_project_id = str_remove(node, "^project:"),
      graph_component
    ) %>%
    group_by(graph_component) %>%
    mutate(
      minimum_pin = min(str_remove(source_project_id, "^residential_")),
      class_297_group_id = paste0("residential_297_group_", minimum_pin)
    ) %>%
    ungroup() %>%
    select(-minimum_pin)
} else {
  class_297_project_groups <- tibble::tibble(
    source_project_id = character(),
    graph_component = integer(),
    class_297_group_id = character()
  )
}

class_297_rows <- class_297_rows %>%
  left_join(
    class_297_project_groups,
    by = "source_project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    class_297_group_id = coalesce(
      class_297_group_id,
      paste0("residential_297_group_", pin)
    )
  )

class_297_group_chains <- class_297_rows %>%
  select(class_297_group_id, source_project_id) %>%
  left_join(
    class_297_direct_chains,
    by = c("source_project_id" = "project_id"),
    relationship = "one-to-many"
  ) %>%
  filter(!is.na(permit_chain_id)) %>%
  group_by(class_297_group_id) %>%
  summarise(
    permit_chains = n_distinct(permit_chain_id),
    permit_chain_ids = paste(sort(unique(permit_chain_id)), collapse = "/"),
    .groups = "drop"
  )

class_297_group_units <- class_297_rows %>%
  select(class_297_group_id, source_project_id) %>%
  left_join(
    permit_unit_evidence,
    by = c("source_project_id" = "project_id"),
    relationship = "one-to-many"
  ) %>%
  filter(!is.na(permit_chain_id)) %>%
  group_by(class_297_group_id) %>%
  summarise(
    permit_unit_values = paste(
      sort(unique(permit_unit_count[is.finite(permit_unit_count)])),
      collapse = "/"
    ),
    distinct_permit_unit_values = n_distinct(permit_unit_count, na.rm = TRUE),
    permit_unit_count = single_finite_value(permit_unit_count),
    .groups = "drop"
  )

class_297_candidates <- class_297_rows %>%
  group_by(class_297_group_id) %>%
  summarise(
    project_id = first(class_297_group_id),
    source_family = "residential",
    project_kind = "class_297",
    component_pins = paste(sort(unique(pin)), collapse = "/"),
    component_count = n_distinct(pin),
    construction_year = single_finite_value(year_built),
    assessor_units = single_finite_value(num_apartments),
    building_sqft = if_else(
      component_count == 1,
      first(building_sqft),
      NA_real_
    ),
    land_sqft = if_else(
      component_count == 1,
      first(land_sqft),
      NA_real_
    ),
    class_values = "297",
    source_row_ids = paste(sort(unique(row_id)), collapse = "/"),
    current_distance_m = suppressWarnings(min(dist_to_boundary_m, na.rm = TRUE)),
    current_within_1500ft = any(within_1500ft %in% TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    current_distance_m = if_else(is.infinite(current_distance_m), NA_real_, current_distance_m)
  ) %>%
  left_join(
    class_297_group_chains,
    by = "class_297_group_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    class_297_group_units,
    by = "class_297_group_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    permit_chains = coalesce(permit_chains, 0L),
    distinct_permit_unit_values = coalesce(distinct_permit_unit_values, 0L),
    dwelling_units = case_when(
      component_count == 1 & is.finite(assessor_units) & assessor_units > 0 ~ assessor_units,
      component_count == 1 & permit_chains == 1 &
        distinct_permit_unit_values == 1 ~ permit_unit_count,
      TRUE ~ NA_real_
    ),
    permit_numbers = NA_character_,
    year_source = paste0("class_297_assessor_rows:", source_row_ids),
    units_source = case_when(
      component_count == 1 & is.finite(assessor_units) & assessor_units > 0 ~
        paste0("class_297_assessor_row:", source_row_ids),
      component_count == 1 & permit_chains == 1 & distinct_permit_unit_values == 1 ~
        paste0("issued_permit_chain:", permit_chain_ids),
      TRUE ~ NA_character_
    ),
    building_source = if_else(
      component_count == 1,
      paste0("class_297_assessor_row:", source_row_ids),
      NA_character_
    ),
    land_source = if_else(
      component_count == 1,
      paste0("class_297_assessor_row:", source_row_ids),
      NA_character_
    ),
    candidate_status = case_when(
      !between(construction_year, 2006L, 2022L) ~ "exclude_outside_period",
      component_count > 1 ~ "review_required",
      distinct_permit_unit_values > 1 ~ "review_required",
      !is.finite(dwelling_units) | dwelling_units <= 0 |
        !is.finite(building_sqft) | building_sqft <= 0 |
        !is.finite(land_sqft) | land_sqft <= 0 ~ "review_required",
      TRUE ~ "retain_mechanical"
    ),
    decision_reason = case_when(
      !between(construction_year, 2006L, 2022L) ~ "construction_year_outside_2006_2022",
      component_count > 1 ~ "class_297_pins_share_permit_chain",
      distinct_permit_unit_values > 1 ~ "conflicting_permit_unit_mentions",
      !is.finite(dwelling_units) | dwelling_units <= 0 ~ "class_297_units_unresolved",
      !is.finite(building_sqft) | building_sqft <= 0 ~ "missing_or_nonpositive_building_area",
      !is.finite(land_sqft) | land_sqft <= 0 ~ "missing_or_nonpositive_land_area",
      is.finite(assessor_units) & assessor_units > 0 ~ "class_297_assessor_units_available",
      TRUE ~ "class_297_single_permit_unit_count"
    )
  ) %>%
  select(all_of(names(ordinary_candidates)))

commercial_overlap_candidates <- inventory %>%
  filter(
    !pin %in% tieback_pin_lineage$pin,
    in_commercial_source
  ) %>%
  transmute(
    project_id = paste0("residential_overlap_", pin),
    source_family = "residential",
    project_kind = "residential_commercial_overlap",
    component_pins = pin,
    component_count = 1L,
    construction_year = preferred_year,
    dwelling_units = assessor_units,
    building_sqft,
    land_sqft,
    class_values = class,
    source_row_ids = row_id,
    permit_chain_ids = exact_permit_chain_id,
    permit_numbers = exact_permit_numbers,
    year_source,
    units_source = paste0("assessor_row:", row_id),
    building_source = paste0("assessor_row:", row_id),
    land_source = paste0("assessor_row:", row_id),
    current_distance_m = dist_to_boundary_m,
    current_within_1500ft = within_1500ft,
    candidate_status = "defer_to_commercial_reconciliation",
    decision_reason = "pin_also_appears_in_commercial_source"
  )

residential_candidates <- bind_rows(
  ordinary_candidates,
  tieback_candidates,
  multicard_candidates,
  class_297_candidates,
  commercial_overlap_candidates
) %>%
  arrange(project_kind, project_id)

# A one-square-foot area is a source placeholder, not a measured building or lot.
placeholder <- with(residential_candidates,
  candidate_status == "retain_mechanical" &
    ((!is.na(building_sqft) & building_sqft <= 1) |
     (!is.na(land_sqft) & land_sqft <= 1)))
residential_candidates$candidate_status[placeholder] <- "exclude_unusable_measurement"
residential_candidates$decision_reason[placeholder] <- "source_area_placeholder_not_usable_density_measurement"

assessor_match <- match(residential_candidates$project_id, assessor_projects$project_id)
residential_candidates$replacement_project_ids <- assessor_projects$replacement_project_ids[assessor_match]
residential_candidates$replacement_check <- assessor_projects$replacement_check[assessor_match]

if (anyDuplicated(residential_candidates$project_id) > 0) {
  stop("Preferred residential candidate IDs are not unique.", call. = FALSE)
}

# Superseded combined records remain in the candidate ledger, not in building membership.
component_rows <- residential_candidates %>%
  filter(decision_reason != "source_replaced_by_reviewed_assessor_buildings") %>%
  select(project_id, source_family, project_kind, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  rename(component_pin = component_pins) %>%
  distinct(project_id, component_pin, .keep_all = TRUE) %>%
  arrange(project_id, component_pin)

component_conflicts <- component_rows %>%
  group_by(component_pin) %>%
  summarise(projects = n_distinct(project_id), .groups = "drop") %>%
  filter(projects > 1)

if (nrow(component_conflicts) > 0) {
  stop(
    paste0(
      "Residential component PINs belong to multiple candidate projects: ",
      paste(head(component_conflicts$component_pin, 10), collapse = ", ")
    ),
    call. = FALSE
  )
}

adjudication_queue <- residential_candidates %>%
  filter(
    candidate_status %in% c(
      "review_required",
      "defer_to_commercial_reconciliation"
    ),
    is.na(construction_year) | between(construction_year, 2006L, 2022L)
  ) %>%
  select(
    project_id,
    project_kind,
    component_pins,
    component_count,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    class_values,
    source_row_ids,
    permit_chain_ids,
    permit_numbers,
    year_source,
    units_source,
    building_source,
    land_source,
    current_distance_m,
    current_within_1500ft,
    candidate_status,
    decision_reason
  )

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(adjudication_queue), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Residential adjudication queue contains a prohibited analysis field.", call. = FALSE)
}

readr::write_csv(
  residential_candidates,
  "../output/preferred_residential_project_candidates.csv"
)
readr::write_csv(
  component_rows,
  "../output/preferred_residential_project_components.csv"
)
readr::write_csv(
  adjudication_queue,
  "../output/residential_adjudication_queue.csv"
)
