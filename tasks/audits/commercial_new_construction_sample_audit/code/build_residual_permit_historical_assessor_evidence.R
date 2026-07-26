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

parcel_matches <- readr::read_csv(
  "../output/residual_permit_historical_parcel_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    historical_pin14 = readr::col_character(),
    .default = readr::col_guess()
  )
  ) |>
  dplyr::filter(stringr::str_length(historical_pin14) == 14L) |>
  dplyr::transmute(
    permit_chain_id,
    application_year,
    target_year,
    historical_pin14,
    historical_parcel_match_method = "containing_polygon",
    historical_parcel_distance_ft = 0
  )
nearest_parcel_matches <- readr::read_csv(
  "../output/residual_permit_validated_nearest_historical_parcel_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    historical_pin14 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(stringr::str_length(historical_pin14) == 14L) |>
  dplyr::transmute(
    permit_chain_id,
    application_year,
    target_year,
    historical_pin14,
    historical_parcel_match_method = nearest_validation_method,
    historical_parcel_distance_ft = nearest_distance_ft
  )
parcel_matches <- dplyr::bind_rows(
  parcel_matches,
  nearest_parcel_matches
) |>
  dplyr::distinct(
    permit_chain_id,
    application_year,
    target_year,
    historical_pin14,
    .keep_all = TRUE
  )
chains <- readr::read_csv(
  "../output/residual_permit_chain_semantics.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
existing_evidence <- readr::read_csv(
  "../output/permit_residual_evidence_matrix.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    permit_chain_id,
    representative_issue_date,
    completion_evidence_class,
    evidence_review_priority,
    represented_completion_evidence,
    nearby_represented_evidence,
    strong_unrepresented_assessor_episode,
    strong_unrepresented_footprint
  )
historical_parcel_project_evidence <- readr::read_csv(
  "../output/residual_permit_historical_parcel_project_chain_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
components <- readr::read_csv(
  "../output/preferred_new_construction_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
current_addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    .default = readr::col_character()
  )
) |>
  dplyr::transmute(
    historical_pin14 = pin,
    current_property_address = prop_address_full
  )

if (anyDuplicated(chains$permit_chain_id) ||
    anyDuplicated(existing_evidence$permit_chain_id) ||
    anyDuplicated(components$component_pin) ||
    anyDuplicated(current_addresses$historical_pin14) ||
    !setequal(chains$permit_chain_id, existing_evidence$permit_chain_id)) {
  stop("Residual permit evidence keys are invalid.", call. = FALSE)
}

chain_pins <- parcel_matches |>
  dplyr::distinct(
    permit_chain_id,
    application_year,
    historical_pin14,
    .keep_all = TRUE
  ) |>
  dplyr::left_join(
    components |>
      dplyr::select(
        represented_project_id = project_id,
        historical_pin14 = component_pin
      ),
    by = "historical_pin14",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    current_addresses,
    by = "historical_pin14",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    represented_in_preferred_ledger = !is.na(represented_project_id)
  )
target_pins <- chain_pins |>
  dplyr::distinct(historical_pin14)

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbWriteTable(
  con,
  "target_pins",
  target_pins,
  overwrite = TRUE
)
invisible(DBI::dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

residential_history <- DBI::dbGetQuery(con, "
SELECT
  regexp_replace(trim(r.pin), '[^0-9]', '', 'g') AS historical_pin14,
  try_cast(numeric_text(r.year) AS INTEGER) AS tax_year,
  try_cast(numeric_text(r.card) AS INTEGER) AS card_num,
  trim(r.class) AS assessor_class,
  regexp_replace(trim(r.tieback_key_pin), '[^0-9]', '', 'g')
    AS tieback_group,
  try_cast(numeric_text(r.tieback_proration_rate) AS DOUBLE)
    AS pin_proration_rate,
  try_cast(numeric_text(r.card_proration_rate) AS DOUBLE)
    AS card_proration_rate,
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
  ON regexp_replace(trim(r.pin), '[^0-9]', '', 'g') =
     p.historical_pin14
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
    ),
    tieback_group = dplyr::na_if(tieback_group, "")
  ) |>
  dplyr::arrange(historical_pin14, card_num, tax_year, row_id) |>
  dplyr::group_by(historical_pin14, card_num, tax_year) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup()

latest_residential_cards <- residential_history |>
  dplyr::filter(is.finite(tax_year), tax_year <= 2025L) |>
  dplyr::group_by(historical_pin14, card_num) |>
  dplyr::arrange(
    dplyr::desc(tax_year),
    dplyr::desc(row_id),
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()
latest_residential_cards_nested <- latest_residential_cards |>
  tidyr::nest(assessor_cards = -historical_pin14)

residential_episodes <- chain_pins |>
  dplyr::inner_join(
    latest_residential_cards_nested,
    by = "historical_pin14",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(assessor_cards) |>
  dplyr::mutate(
    application_year_gap = year_built - application_year,
    plausible_completion_episode =
      is.finite(year_built) &
      dplyr::between(year_built, 2006L, 2022L) &
      dplyr::between(application_year_gap, -1L, 4L),
    completion_date_start = as.Date(
      dplyr::if_else(
        is.finite(year_built),
        paste0(year_built, "-01-01"),
        NA_character_
      )
    ),
    completion_date_end = as.Date(
      dplyr::if_else(
        is.finite(year_built),
        paste0(year_built, "-12-31"),
        NA_character_
      )
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
    unnest(component_pins) AS historical_pin14
  FROM commercial_rows
)
SELECT e.*
FROM expanded AS e
INNER JOIN target_pins AS p
  ON e.historical_pin14 = p.historical_pin14;
")

latest_commercial_rows <- commercial_history |>
  dplyr::filter(
    is.finite(assessor_report_year),
    assessor_report_year <= 2025L
  ) |>
  dplyr::group_by(historical_pin14, keypin) |>
  dplyr::arrange(
    dplyr::desc(assessor_report_year),
    commercial_row_id,
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()
latest_commercial_rows_nested <- latest_commercial_rows |>
  tidyr::nest(assessor_rows = -historical_pin14)

commercial_episodes <- chain_pins |>
  dplyr::inner_join(
    latest_commercial_rows_nested,
    by = "historical_pin14",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(assessor_rows) |>
  dplyr::mutate(
    application_year_gap = year_built - application_year,
    plausible_completion_episode =
      is.finite(year_built) &
      dplyr::between(year_built, 2006L, 2022L) &
      dplyr::between(application_year_gap, -1L, 4L),
    completion_date_start = as.Date(
      dplyr::if_else(
        is.finite(year_built),
        paste0(year_built, "-01-01"),
        NA_character_
      )
    ),
    completion_date_end = as.Date(
      dplyr::if_else(
        is.finite(year_built),
        paste0(year_built, "-12-31"),
        NA_character_
      )
    )
  )

chain_pin_summary <- chain_pins |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    historical_pin_count = dplyr::n_distinct(historical_pin14),
    historical_pin_values = paste(
      sort(unique(historical_pin14)),
      collapse = "/"
    ),
    represented_historical_pin_count = dplyr::n_distinct(
      historical_pin14[represented_in_preferred_ledger]
    ),
    represented_project_ids = paste(
      sort(unique(represented_project_id[
        !is.na(represented_project_id)
      ])),
      collapse = "/"
    ),
    unrepresented_historical_pin_count = dplyr::n_distinct(
      historical_pin14[!represented_in_preferred_ledger]
    ),
    unrepresented_historical_pin_values = paste(
      sort(unique(historical_pin14[
        !represented_in_preferred_ledger
      ])),
      collapse = "/"
    ),
    .groups = "drop"
  )
residential_chain_summary <- residential_episodes |>
  dplyr::filter(
    plausible_completion_episode,
    !represented_in_preferred_ledger
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    plausible_unrepresented_residential_pins =
      dplyr::n_distinct(historical_pin14),
    plausible_unrepresented_residential_pin_values = paste(
      sort(unique(historical_pin14)),
      collapse = "/"
    ),
    plausible_residential_year_values = paste(
      sort(unique(year_built)),
      collapse = "/"
    ),
    .groups = "drop"
  )
commercial_chain_summary <- commercial_episodes |>
  dplyr::filter(
    plausible_completion_episode,
    !represented_in_preferred_ledger
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    plausible_unrepresented_commercial_pins =
      dplyr::n_distinct(historical_pin14),
    plausible_unrepresented_commercial_pin_values = paste(
      sort(unique(historical_pin14)),
      collapse = "/"
    ),
    plausible_commercial_year_values = paste(
      sort(unique(year_built)),
      collapse = "/"
    ),
    .groups = "drop"
  )

chain_evidence <- chains |>
  dplyr::left_join(
    existing_evidence,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    historical_parcel_project_evidence,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    chain_pin_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    residential_chain_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    commercial_chain_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        historical_pin_count,
        historical_parcel_project_candidates,
        plausible_historical_parcel_projects,
        represented_historical_pin_count,
        unrepresented_historical_pin_count,
        plausible_unrepresented_residential_pins,
        plausible_unrepresented_commercial_pins
      ),
      ~ dplyr::coalesce(.x, 0L)
    ),
    dplyr::across(
      c(
        represented_completion_evidence,
        nearby_represented_evidence,
        strong_unrepresented_footprint
      ),
      ~ dplyr::coalesce(.x, FALSE)
    ),
    historical_reconciliation_status = dplyr::case_when(
      represented_historical_pin_count > 0L &
        (
          plausible_unrepresented_residential_pins > 0L |
            plausible_unrepresented_commercial_pins > 0L
        ) ~ "represented_and_unrepresented_episode_conflict",
      represented_historical_pin_count > 0L |
        plausible_historical_parcel_projects > 0L |
        represented_completion_evidence |
        nearby_represented_evidence ~
        "already_represented",
      semantic_building_class %in% c(
        "accessory_structure_only",
        "addition_only",
        "foundation_or_phase_only",
        "no_residential_building_object"
      ) ~ "not_a_completed_residential_building_candidate",
      plausible_unrepresented_residential_pins > 0L |
        plausible_unrepresented_commercial_pins > 0L ~
        "unrepresented_assessor_completion_candidate",
      strong_unrepresented_footprint ~
        "unrepresented_footprint_candidate",
      semantic_building_class == "full_residential_building" ~
        "full_building_permit_without_completion_match",
      semantic_building_class ==
        "ambiguous_residential_new_construction" ~
        "ambiguous_permit_without_completion_match",
      TRUE ~ "other_unresolved_permit"
    )
  )

timeline_candidates <- dplyr::bind_rows(
  residential_episodes |>
    dplyr::filter(
      plausible_completion_episode,
      !represented_in_preferred_ledger
    ) |>
    dplyr::transmute(
      permit_chain_id,
      source_family = "residential",
      historical_pin14,
      year_built,
      completion_date_start,
      completion_date_end
    ),
  commercial_episodes |>
    dplyr::filter(
      plausible_completion_episode,
      !represented_in_preferred_ledger
    ) |>
    dplyr::transmute(
      permit_chain_id,
      source_family = "commercial",
      historical_pin14,
      year_built,
      completion_date_start,
      completion_date_end
    )
) |>
  dplyr::distinct() |>
  dplyr::left_join(
    chain_evidence |>
      dplyr::select(
        permit_chain_id,
        representative_application_date,
        representative_issue_date
      ),
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    application_to_completion_min_days = as.integer(
      completion_date_start - representative_application_date
    ),
    application_to_completion_max_days = as.integer(
      completion_date_end - representative_application_date
    ),
    issue_to_completion_min_days = as.integer(
      completion_date_start - representative_issue_date
    ),
    issue_to_completion_max_days = as.integer(
      completion_date_end - representative_issue_date
    )
  )

summary <- dplyr::bind_rows(
  chain_evidence |>
    dplyr::count(historical_reconciliation_status, name = "value") |>
    dplyr::transmute(
      section = "chain_status",
      metric = historical_reconciliation_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "residual_permit_chains",
      "chains_with_historical_pin",
      "unique_historical_pins",
      "residential_history_rows",
      "commercial_history_rows",
      "timeline_candidates"
    ),
    value = c(
      nrow(chains),
      sum(chain_evidence$historical_pin_count > 0L),
      dplyr::n_distinct(chain_pins$historical_pin14),
      nrow(residential_history),
      nrow(commercial_history),
      nrow(timeline_candidates)
    )
  )
)

readr::write_csv(
  chain_pins,
  "../output/residual_permit_historical_pin_reconciliation.csv"
)
readr::write_csv(
  residential_history,
  "../output/residual_permit_historical_residential_history.csv"
)
readr::write_csv(
  residential_episodes,
  "../output/residual_permit_historical_residential_episodes.csv"
)
readr::write_csv(
  commercial_history,
  "../output/residual_permit_historical_commercial_history.csv"
)
readr::write_csv(
  commercial_episodes,
  "../output/residual_permit_historical_commercial_episodes.csv"
)
readr::write_csv(
  timeline_candidates,
  "../output/residual_permit_completion_timeline_candidates.csv"
)
readr::write_csv(
  chain_evidence,
  "../output/residual_permit_historical_chain_evidence.csv"
)
readr::write_csv(
  summary,
  "../output/residual_permit_historical_assessor_summary.csv"
)
