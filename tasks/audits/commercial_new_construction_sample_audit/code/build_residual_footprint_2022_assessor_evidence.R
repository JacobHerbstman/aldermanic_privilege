# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

parse_apartments <- function(x) {
  value <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    is.na(value) | value == "" ~ NA_real_,
    value %in% c("none", "zero") ~ 0,
    value == "one" ~ 1,
    value == "two" ~ 2,
    value == "three" ~ 3,
    value == "four" ~ 4,
    value == "five" ~ 5,
    value == "six" ~ 6,
    TRUE ~ suppressWarnings(as.numeric(
      stringr::str_replace_all(value, "[^0-9.-]", "")
    ))
  )
}

footprint_parcels <- readr::read_csv(
  "../output/residual_permit_footprint_2022_parcel_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    footprint_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    footprint_parcel_match_status %in% c(
      "unique_containing_2022_parcel",
      "unique_nearest_2022_parcel_within_25ft"
    ),
    stringr::str_length(pin14_2022) == 14L
  ) |>
  dplyr::distinct(footprint_id, pin14_2022)
footprint_links <- readr::read_csv(
  "../output/permit_residual_city_building_footprint_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
candidate_chains <- readr::read_csv(
  "../output/residual_permit_historical_chain_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    historical_reconciliation_status ==
      "unrepresented_footprint_candidate"
  ) |>
  dplyr::select(
    permit_chain_id,
    semantic_building_class,
    representative_application_date,
    representative_issue_date,
    representative_address,
    representative_description,
    application_boundary_distance_ft
  )
components <- readr::read_csv(
  "../output/preferred_new_construction_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    represented_project_id = project_id,
    pin14_2022 = component_pin
  )

if (anyDuplicated(footprint_parcels$footprint_id) ||
    anyDuplicated(components$pin14_2022)) {
  stop("Footprint successor parcel keys are invalid.", call. = FALSE)
}

chain_footprints <- footprint_links |>
  dplyr::semi_join(
    candidate_chains,
    by = "permit_chain_id"
  ) |>
  dplyr::filter(
    strong_footprint_match,
    !represented_in_preferred_ledger
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::filter(
    dplyr::case_when(
      any(address_range_match) ~ address_range_match,
      any(exact_harris_pin) ~ exact_harris_pin,
      TRUE ~ permit_point_inside_footprint
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    permit_chain_id,
    footprint_id,
    city_year_built,
    city_units,
    no_stories,
    city_shape_area_sqft,
    city_address
  ) |>
  dplyr::distinct() |>
  dplyr::inner_join(
    footprint_parcels,
    by = "footprint_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    components,
    by = "pin14_2022",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    candidate_chains,
    by = "permit_chain_id",
    relationship = "many-to-one"
  )
target_pins <- chain_footprints |>
  dplyr::distinct(pin14_2022)

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbWriteTable(con, "target_pins", target_pins, overwrite = TRUE)
invisible(DBI::dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

residential_history <- DBI::dbGetQuery(con, "
SELECT
  regexp_replace(trim(r.pin), '[^0-9]', '', 'g') AS pin14_2022,
  try_cast(numeric_text(r.year) AS INTEGER) AS tax_year,
  try_cast(numeric_text(r.card) AS INTEGER) AS card_num,
  trim(r.class) AS assessor_class,
  try_cast(numeric_text(r.char_yrblt) AS INTEGER) AS year_built,
  try_cast(numeric_text(r.char_bldg_sf) AS DOUBLE) AS building_sqft,
  try_cast(numeric_text(r.char_land_sf) AS DOUBLE) AS land_sqft,
  trim(r.char_apts) AS apartments_text,
  trim(r.char_type_resd) AS type_of_residence,
  trim(r.char_use) AS residential_use,
  trim(r.row_id) AS row_id
FROM read_csv(
  '../input/residential_improvement_characteristics_full.csv',
  all_varchar = true,
  header = true,
  ignore_errors = false,
  max_line_size = 10000000
) AS r
INNER JOIN target_pins AS p
  ON regexp_replace(trim(r.pin), '[^0-9]', '', 'g') = p.pin14_2022
WHERE try_cast(numeric_text(r.township_code) AS INTEGER)
      IN (70, 71, 72, 73, 74, 75, 76, 77);
") |>
  dplyr::mutate(
    num_apartments = parse_apartments(apartments_text),
    assessor_single_family =
      stringr::str_detect(
        residential_use,
        stringr::regex("^single", ignore_case = TRUE)
      ) |
      type_of_residence %in% c(
        "1 Story",
        "1.5 Story",
        "2 Story",
        "3 Story +",
        "Split Level"
      ),
    dwelling_units = dplyr::case_when(
      assessor_single_family &
        (is.na(num_apartments) | num_apartments == 0) ~ 1,
      TRUE ~ num_apartments
    )
  ) |>
  dplyr::arrange(pin14_2022, card_num, tax_year, row_id) |>
  dplyr::group_by(pin14_2022, card_num, tax_year) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup()

latest_cards <- residential_history |>
  dplyr::filter(
    is.finite(tax_year),
    tax_year <= 2025L
  ) |>
  dplyr::group_by(pin14_2022, card_num) |>
  dplyr::arrange(
    dplyr::desc(tax_year),
    dplyr::desc(row_id),
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()
residential_pin_summary <- latest_cards |>
  dplyr::group_by(pin14_2022) |>
  dplyr::summarise(
    assessor_card_count = dplyr::n_distinct(card_num),
    assessor_class_values = paste(
      sort(unique(assessor_class)),
      collapse = "/"
    ),
    assessor_year_built_values = paste(
      sort(unique(year_built[is.finite(year_built)])),
      collapse = "/"
    ),
    assessor_completion_year = suppressWarnings(max(
      year_built[is.finite(year_built)],
      na.rm = TRUE
    )),
    assessor_dwelling_units = sum(dwelling_units, na.rm = TRUE),
    assessor_building_sqft = sum(building_sqft, na.rm = TRUE),
    assessor_land_sqft = suppressWarnings(max(
      land_sqft,
      na.rm = TRUE
    )),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    assessor_completion_year = dplyr::if_else(
      is.infinite(assessor_completion_year),
      NA_real_,
      assessor_completion_year
    ),
    assessor_land_sqft = dplyr::if_else(
      is.infinite(assessor_land_sqft),
      NA_real_,
      assessor_land_sqft
    )
  )

commercial_history <- DBI::dbGetQuery(con, "
WITH commercial_rows AS (
  SELECT
    row_number() OVER () AS commercial_row_id,
    regexp_replace(trim(keypin), '[^0-9]', '', 'g') AS keypin,
    trim(pins) AS source_pins,
    try_cast(numeric_text(year) AS INTEGER) AS assessor_report_year,
    try_cast(numeric_text(yearbuilt) AS INTEGER) AS year_built,
    try_cast(numeric_text(tot_units) AS DOUBLE) AS dwelling_units,
    try_cast(numeric_text(bldgsf) AS DOUBLE) AS building_sqft,
    try_cast(numeric_text(landsf) AS DOUBLE) AS land_sqft,
    trim(address) AS assessor_address,
    trim(property_type_use) AS property_type_use,
    trim(category) AS category,
    list_distinct(
      list_concat(
        regexp_extract_all(trim(pins), '[0-9]{14}'),
        CASE
          WHEN length(regexp_replace(trim(keypin), '[^0-9]', '', 'g')) = 14
          THEN [regexp_replace(trim(keypin), '[^0-9]', '', 'g')]
          ELSE []
        END
      )
    ) AS component_pins
  FROM read_csv(
    '../input/commercial_value_raw.csv',
    all_varchar = true,
    header = true,
    ignore_errors = false,
    max_line_size = 10000000
  )
),
expanded AS (
  SELECT
    commercial_row_id,
    keypin,
    source_pins,
    assessor_report_year,
    year_built,
    dwelling_units,
    building_sqft,
    land_sqft,
    assessor_address,
    property_type_use,
    category,
    unnest(component_pins) AS component_pin
  FROM commercial_rows
)
SELECT
  p.pin14_2022,
  e.*
FROM expanded AS e
INNER JOIN target_pins AS p
  ON e.component_pin = p.pin14_2022;
")
latest_commercial_rows <- commercial_history |>
  dplyr::filter(
    is.finite(assessor_report_year),
    assessor_report_year <= 2025L
  ) |>
  dplyr::group_by(pin14_2022, keypin) |>
  dplyr::arrange(
    dplyr::desc(assessor_report_year),
    commercial_row_id,
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()
latest_commercial_rows_nested <- latest_commercial_rows |>
  tidyr::nest(assessor_rows = -pin14_2022)
commercial_candidates <- chain_footprints |>
  dplyr::select(
    permit_chain_id,
    footprint_id,
    pin14_2022,
    representative_application_date
  ) |>
  dplyr::inner_join(
    latest_commercial_rows_nested,
    by = "pin14_2022",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(
    assessor_rows
  ) |>
  dplyr::mutate(
    application_year = lubridate::year(
      representative_application_date
    ),
    commercial_year_gap = year_built - application_year,
    plausible_commercial_year =
      dplyr::between(commercial_year_gap, -1L, 4L),
    direct_keypin = keypin == pin14_2022,
    complete_field_count =
      as.integer(is.finite(dwelling_units) & dwelling_units > 0) +
      as.integer(is.finite(building_sqft) & building_sqft > 0) +
      as.integer(is.finite(land_sqft) & land_sqft > 0)
  ) |>
  dplyr::group_by(
    permit_chain_id,
    footprint_id,
    pin14_2022
  ) |>
  dplyr::arrange(
    dplyr::desc(plausible_commercial_year),
    dplyr::desc(direct_keypin),
    dplyr::desc(complete_field_count),
    dplyr::desc(assessor_report_year),
    commercial_row_id,
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    permit_chain_id,
    footprint_id,
    pin14_2022,
    commercial_keypin = keypin,
    commercial_source_pins = source_pins,
    commercial_report_year = assessor_report_year,
    commercial_year_built = year_built,
    commercial_dwelling_units = dwelling_units,
    commercial_building_sqft = building_sqft,
    commercial_land_sqft = land_sqft,
    commercial_address = assessor_address,
    commercial_property_type_use = property_type_use,
    commercial_category = category,
    commercial_year_gap,
    plausible_commercial_year,
    commercial_direct_keypin = direct_keypin,
    commercial_complete_field_count = complete_field_count
  )

chain_evidence <- chain_footprints |>
  dplyr::left_join(
    residential_pin_summary,
    by = "pin14_2022",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    commercial_candidates,
    by = c(
      "permit_chain_id",
      "footprint_id",
      "pin14_2022"
    ),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    application_year = lubridate::year(
      representative_application_date
    ),
    city_year_gap = city_year_built - application_year,
    assessor_year_gap = assessor_completion_year - application_year,
    commercial_year_gap =
      commercial_year_built - application_year,
    completion_year = dplyr::case_when(
      dplyr::between(city_year_gap, -1L, 4L) ~ city_year_built,
      dplyr::between(assessor_year_gap, -1L, 4L) ~
        assessor_completion_year,
      dplyr::between(commercial_year_gap, -1L, 4L) ~
        commercial_year_built,
      TRUE ~ NA_real_
    ),
    candidate_dwelling_units = dplyr::case_when(
      is.finite(city_units) & city_units > 0 ~ city_units,
      is.finite(assessor_dwelling_units) &
        assessor_dwelling_units > 0 ~ assessor_dwelling_units,
      is.finite(commercial_dwelling_units) &
        commercial_dwelling_units > 0 ~
        commercial_dwelling_units,
      semantic_building_class == "full_residential_building" ~ 1,
      TRUE ~ NA_real_
    ),
    candidate_building_sqft = dplyr::case_when(
      is.finite(assessor_building_sqft) &
        assessor_building_sqft > 0 ~ assessor_building_sqft,
      is.finite(commercial_building_sqft) &
        commercial_building_sqft > 0 ~ commercial_building_sqft,
      TRUE ~ NA_real_
    ),
    candidate_land_sqft = dplyr::case_when(
      is.finite(assessor_land_sqft) &
        assessor_land_sqft > 0 ~ assessor_land_sqft,
      is.finite(commercial_land_sqft) &
        commercial_land_sqft > 0 ~ commercial_land_sqft,
      TRUE ~ NA_real_
    ),
    density_field_source = dplyr::case_when(
      is.finite(assessor_building_sqft) &
        assessor_building_sqft > 0 ~ "residential_assessor",
      is.finite(commercial_building_sqft) &
        commercial_building_sqft > 0 ~ "commercial_assessor",
      TRUE ~ NA_character_
    ),
    successor_assessor_status = dplyr::case_when(
      !is.na(represented_project_id) ~
        "already_represented_by_2022_successor_pin",
      stringr::str_detect(
        assessor_class_values,
        "(^|/)299($|/)"
      ) ~ "condominium_successor_excluded",
      is.finite(completion_year) &
        is.finite(candidate_building_sqft) &
        is.finite(candidate_land_sqft) &
        is.finite(candidate_dwelling_units) ~
        "complete_unrepresented_successor_assessor_candidate",
      is.finite(completion_year) ~
        "completion_supported_but_density_fields_incomplete",
      TRUE ~ "successor_assessor_does_not_confirm_completion"
    )
  )

summary <- dplyr::bind_rows(
  chain_evidence |>
    dplyr::distinct(
      permit_chain_id,
      footprint_id,
      pin14_2022,
      successor_assessor_status
    ) |>
    dplyr::count(successor_assessor_status, name = "value") |>
    dplyr::transmute(
      section = "candidate_status",
      metric = successor_assessor_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "footprint_candidate_chains",
      "footprint_successor_links",
      "unique_2022_pins",
      "residential_history_rows",
      "commercial_history_rows",
      "complete_unrepresented_candidates"
    ),
    value = c(
      dplyr::n_distinct(candidate_chains$permit_chain_id),
      nrow(chain_footprints),
      dplyr::n_distinct(chain_footprints$pin14_2022),
      nrow(residential_history),
      nrow(commercial_history),
      dplyr::n_distinct(chain_evidence$permit_chain_id[
        chain_evidence$successor_assessor_status ==
          "complete_unrepresented_successor_assessor_candidate"
      ])
    )
  )
)

readr::write_csv(
  residential_history,
  "../output/residual_permit_footprint_2022_residential_history.csv"
)
readr::write_csv(
  commercial_history,
  "../output/residual_permit_footprint_2022_commercial_history.csv"
)
readr::write_csv(
  chain_evidence,
  "../output/residual_permit_footprint_2022_assessor_evidence.csv"
)
readr::write_csv(
  summary,
  "../output/residual_permit_footprint_2022_assessor_summary.csv"
)
