# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(between(construction_year, 2006L, 2022L)) %>%
  select(
    project_id,
    construction_year,
    selected_vintage,
    selected_source_addresses
  )

entity_versions <- readr::read_csv(
  "../output/commercial_entity_version_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    keypin = readr::col_character(),
    pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(project_family_id %in% candidates$project_id)

raw <- readr::read_csv(
  "../input/commercial_value_raw.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) %>%
  janitor::clean_names() %>%
  mutate(raw_row = row_number()) %>%
  select(
    raw_row,
    idphlicense,
    cost_day_bed,
    revenuebed_day,
    subclass2,
    category,
    apt
  )

if (anyDuplicated(candidates$project_id) > 0) {
  stop("Study-period commercial candidates are not unique by project.", call. = FALSE)
}
if (anyDuplicated(entity_versions$raw_row) > 0) {
  stop("Commercial entity versions are not unique by raw source row.", call. = FALSE)
}
if (anyDuplicated(raw$raw_row) > 0) {
  stop("Raw commercial rows are not unique by row number.", call. = FALSE)
}

special_use_pattern <- regex(
  paste(
    "SENIOR|ASSISTED|SUPPORTIVE|STUDENT|DORMITOR|NURSING|SKILLED NURSING|",
    "SHELTER|GROUP HOME|GROUP RESIDENCE|SINGLE ROOM|\\bSRO\\b|",
    "ROOMING|BOARDING|HOTEL|MOTEL",
    sep = ""
  ),
  ignore_case = TRUE
)

row_evidence <- entity_versions %>%
  left_join(raw, by = "raw_row", relationship = "one-to-one") %>%
  mutate(
    source_text = str_squish(paste(
      coalesce(modelgroup, ""),
      coalesce(property_type_use, ""),
      coalesce(property_name_description, ""),
      coalesce(subclass2, "")
    )),
    idph_license_present = !is.na(idphlicense) &
      !str_trim(idphlicense) %in% c("", "0", "0.0"),
    bed_rate_present = suppressWarnings(as.numeric(cost_day_bed)) > 0 |
      suppressWarnings(as.numeric(revenuebed_day)) > 0,
    special_use_language = str_detect(source_text, special_use_pattern),
    special_model_group = str_detect(
      coalesce(modelgroup, ""),
      regex("Class9|Class3&9|Special", ignore_case = TRUE)
    ),
    apartment_breakdown_missing = apartment_unit_sum <= 0 &
      coalesce(source_tot_units > 0, FALSE),
    apartment_total_disagreement = apartment_unit_sum > 0 &
      coalesce(source_tot_units > 0, FALSE) &
      coalesce(apartment_unit_sum != source_tot_units, FALSE),
    property_use_missing = is.na(property_type_use) | str_trim(property_type_use) == "",
    row_semantic_review =
      special_use_language |
      special_model_group |
      idph_license_present |
      coalesce(bed_rate_present, FALSE) |
      apartment_breakdown_missing |
      apartment_total_disagreement |
      property_use_missing
  ) %>%
  select(
    project_id = project_family_id,
    raw_row,
    keypin,
    valuation_year,
    yearbuilt,
    address,
    modelgroup,
    property_type_use,
    property_name_description,
    subclass2,
    apartment_unit_sum,
    source_tot_units,
    reported_units,
    idph_license_present,
    bed_rate_present,
    special_use_language,
    special_model_group,
    apartment_breakdown_missing,
    apartment_total_disagreement,
    property_use_missing,
    row_semantic_review
  ) %>%
  arrange(project_id, valuation_year, raw_row)

if (any(is.na(row_evidence$project_id))) {
  stop("A raw commercial row failed to map to a project.", call. = FALSE)
}
if (anyDuplicated(row_evidence$raw_row) > 0) {
  stop("The semantic row evidence repeats a raw source row.", call. = FALSE)
}

screen <- row_evidence %>%
  group_by(project_id) %>%
  summarise(
    source_rows = n(),
    source_vintages = paste(sort(unique(valuation_year)), collapse = "/"),
    source_addresses = paste(sort(unique(na.omit(address))), collapse = " / "),
    model_groups = paste(sort(unique(na.omit(modelgroup))), collapse = "/"),
    property_uses = paste(sort(unique(na.omit(property_type_use))), collapse = "/"),
    apartment_unit_values = paste(
      sort(unique(apartment_unit_sum[apartment_unit_sum > 0])),
      collapse = "/"
    ),
    total_unit_values = paste(
      sort(unique(source_tot_units[source_tot_units > 0])),
      collapse = "/"
    ),
    any_special_use_language = any(special_use_language),
    any_special_model_group = any(special_model_group),
    any_idph_license = any(idph_license_present),
    any_bed_rate = any(coalesce(bed_rate_present, FALSE)),
    any_apartment_breakdown_missing = any(apartment_breakdown_missing),
    any_apartment_total_disagreement = any(apartment_total_disagreement),
    all_property_use_missing = all(property_use_missing),
    semantic_flag_present =
      any(special_use_language) |
      any(special_model_group) |
      any(idph_license_present) |
      any(coalesce(bed_rate_present, FALSE)) |
      any(apartment_breakdown_missing) |
      any(apartment_total_disagreement) |
      all(property_use_missing),
    semantic_review_required =
      any(special_use_language) |
      any(idph_license_present) |
      any(coalesce(bed_rate_present, FALSE)) |
      any(apartment_breakdown_missing) |
      any(apartment_total_disagreement) |
      all(property_use_missing),
    semantic_review_reason = paste(
      c(
        "special_use_language"[any(special_use_language)],
        "special_model_group"[any(special_model_group)],
        "idph_license"[any(idph_license_present)],
        "bed_rate_fields"[any(coalesce(bed_rate_present, FALSE))],
        "total_units_without_apartment_breakdown"[any(apartment_breakdown_missing)],
        "apartment_sum_differs_from_total_units"[any(apartment_total_disagreement)],
        "property_use_missing"[all(property_use_missing)]
      ),
      collapse = ";"
    ),
    .groups = "drop"
  ) %>%
  right_join(candidates, by = "project_id", relationship = "one-to-one") %>%
  arrange(project_id)

if (nrow(screen) != nrow(candidates)) {
  stop("The semantic screen does not cover every study-period commercial candidate.", call. = FALSE)
}
if (any(is.na(screen$source_rows))) {
  stop("A study-period commercial candidate has no source-row semantic evidence.", call. = FALSE)
}
if (anyDuplicated(screen$project_id) > 0) {
  stop("The semantic screen is not unique by project.", call. = FALSE)
}

review_queue <- screen %>%
  filter(semantic_review_required) %>%
  select(
    project_id,
    construction_year,
    selected_vintage,
    selected_source_addresses,
    source_rows,
    source_vintages,
    source_addresses,
    model_groups,
    property_uses,
    apartment_unit_values,
    total_unit_values,
    semantic_review_reason
  )

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(review_queue), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Commercial semantic review contains a prohibited analysis field.", call. = FALSE)
}

summary <- bind_rows(
  tibble::tibble(
    section = "coverage",
    metric = c(
      "study_period_candidates",
      "source_rows_screened",
      "projects_with_semantic_flags",
      "projects_requiring_semantic_review",
      "projects_with_special_use_language",
      "projects_with_special_model_group",
      "projects_with_idph_or_bed_fields",
      "projects_using_total_units_without_apartment_breakdown",
      "projects_with_apartment_total_disagreement",
      "projects_with_missing_property_use",
      "duplicate_project_ids",
      "duplicate_raw_rows"
    ),
    value = c(
      nrow(candidates),
      nrow(row_evidence),
      sum(screen$semantic_flag_present),
      nrow(review_queue),
      sum(screen$any_special_use_language),
      sum(screen$any_special_model_group),
      sum(screen$any_idph_license | screen$any_bed_rate),
      sum(screen$any_apartment_breakdown_missing),
      sum(screen$any_apartment_total_disagreement),
      sum(screen$all_property_use_missing),
      anyDuplicated(screen$project_id),
      anyDuplicated(row_evidence$raw_row)
    )
  ),
  screen %>%
    count(semantic_review_reason, name = "value") %>%
    transmute(
      section = "review_reasons",
      metric = if_else(semantic_review_reason == "", "none", semantic_review_reason),
      value
    )
)

readr::write_csv(row_evidence, "../output/commercial_semantic_row_evidence.csv")
readr::write_csv(screen, "../output/commercial_semantic_screen.csv")
readr::write_csv(review_queue, "../output/commercial_semantic_review_queue.csv")
readr::write_csv(summary, "../output/commercial_semantic_screen_summary.csv")
