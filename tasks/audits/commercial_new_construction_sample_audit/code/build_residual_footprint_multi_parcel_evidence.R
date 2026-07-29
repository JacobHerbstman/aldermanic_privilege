# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

address_key <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_replace_all(
      "\\b(STREET|ST|AVENUE|AVE|BOULEVARD|BLVD|ROAD|RD|DRIVE|DR|PLACE|PL|COURT|CT|PARKWAY|PKWY)\\b",
      " "
    ) |>
    stringr::str_squish()
}

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
    footprint_parcel_match_status ==
      "multiple_containing_2022_parcels"
  ) |>
  dplyr::distinct(footprint_id, pin14_2022)
candidate_chains <- readr::read_csv(
  "../output/residual_permit_historical_chain_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    plausible_unrepresented_residential_pin_values =
      readr::col_character(),
    plausible_residential_year_values =
      readr::col_character(),
    plausible_unrepresented_commercial_pin_values =
      readr::col_character(),
    plausible_commercial_year_values =
      readr::col_character(),
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
    maximum_parsed_unit_mention,
    application_boundary_distance_ft
  )
footprint_links <- readr::read_csv(
  "../output/permit_residual_city_building_footprint_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_id = readr::col_character(),
    harris_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
permit_points <- readr::read_csv(
  "../output/permit_first_chain_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    address_project_id = readr::col_character(),
    matched_project_id = readr::col_character(),
    matched_project_candidates = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    permit_chain_id,
    representative_x_3435,
    representative_y_3435
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

chain_footprints <- footprint_links |>
  dplyr::semi_join(candidate_chains, by = "permit_chain_id") |>
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
    relationship = "many-to-many"
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
  ) |>
  dplyr::left_join(
    permit_points,
    by = "permit_chain_id",
    relationship = "many-to-one"
  )

if (anyDuplicated(components$pin14_2022) ||
    anyDuplicated(
      chain_footprints[
        c("permit_chain_id", "footprint_id", "pin14_2022")
      ]
    )) {
  stop("Multi-parcel footprint evidence has invalid keys.", call. = FALSE)
}

target_pins <- chain_footprints |>
  dplyr::distinct(pin14_2022)
parcel_polygons <- sf::st_read(
  "../output/residual_permit_footprint_2022_parcels.gpkg",
  quiet = TRUE
) |>
  dplyr::semi_join(target_pins, by = "pin14_2022") |>
  dplyr::select(pin14_2022)
chain_points <- chain_footprints |>
  dplyr::distinct(
    permit_chain_id,
    representative_x_3435,
    representative_y_3435
  ) |>
  dplyr::filter(
    is.finite(representative_x_3435),
    is.finite(representative_y_3435)
  ) |>
  sf::st_as_sf(
    coords = c(
      "representative_x_3435",
      "representative_y_3435"
    ),
    crs = 3435
  )
point_parcel_evidence <- tidyr::crossing(
  permit_chain_id = chain_points$permit_chain_id,
  pin14_2022 = parcel_polygons$pin14_2022
) |>
  dplyr::inner_join(
    chain_footprints |>
      dplyr::distinct(
        permit_chain_id,
        footprint_id,
        pin14_2022
      ),
    by = c("permit_chain_id", "pin14_2022"),
    relationship = "many-to-many"
  )
point_rows <- chain_points[
  match(
    point_parcel_evidence$permit_chain_id,
    chain_points$permit_chain_id
  ),
]
parcel_rows <- parcel_polygons[
  match(
    point_parcel_evidence$pin14_2022,
    parcel_polygons$pin14_2022
  ),
]
point_parcel_evidence <- point_parcel_evidence |>
  dplyr::mutate(
    permit_point_to_parcel_ft = as.numeric(
      sf::st_distance(
        point_rows,
        parcel_rows,
        by_element = TRUE
      )
    ),
    permit_point_inside_parcel = lengths(
      sf::st_within(
        point_rows,
        parcel_rows,
        sparse = TRUE
      )
    ) > 0L
  )

parcel_addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::transmute(
    pin14_2022 = stringr::str_pad(pin, 14L, pad = "0"),
    parcel_address = prop_address_full,
    parcel_address_key = address_key(prop_address_full)
  ) |>
  dplyr::semi_join(target_pins, by = "pin14_2022") |>
  dplyr::filter(!is.na(parcel_address_key)) |>
  dplyr::group_by(pin14_2022) |>
  dplyr::summarise(
    parcel_addresses = paste(
      sort(unique(parcel_address)),
      collapse = "/"
    ),
    parcel_address_keys = paste(
      sort(unique(parcel_address_key)),
      collapse = "/"
    ),
    .groups = "drop"
  )

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
  )
latest_cards <- residential_history |>
  dplyr::filter(is.finite(tax_year), tax_year <= 2025L) |>
  dplyr::arrange(
    pin14_2022,
    card_num,
    dplyr::desc(tax_year),
    dplyr::desc(row_id)
  ) |>
  dplyr::group_by(pin14_2022, card_num) |>
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
    dplyr::across(
      c(assessor_completion_year, assessor_land_sqft),
      ~ dplyr::if_else(is.infinite(.x), NA_real_, .x)
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
") |>
  dplyr::filter(
    is.finite(assessor_report_year),
    assessor_report_year <= 2025L
  ) |>
  dplyr::arrange(
    pin14_2022,
    keypin,
    dplyr::desc(assessor_report_year),
    commercial_row_id
  ) |>
  dplyr::group_by(pin14_2022, keypin) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()
commercial_pin_summary <- commercial_history |>
  dplyr::group_by(pin14_2022) |>
  dplyr::summarise(
    commercial_row_count = dplyr::n(),
    commercial_keypins = paste(sort(unique(keypin)), collapse = "/"),
    commercial_year_built_values = paste(
      sort(unique(year_built[is.finite(year_built)])),
      collapse = "/"
    ),
    commercial_dwelling_unit_values = paste(
      sort(unique(dwelling_units[is.finite(dwelling_units)])),
      collapse = "/"
    ),
    commercial_building_sqft_values = paste(
      sort(unique(building_sqft[is.finite(building_sqft)])),
      collapse = "/"
    ),
    commercial_land_sqft_values = paste(
      sort(unique(land_sqft[is.finite(land_sqft)])),
      collapse = "/"
    ),
    commercial_addresses = paste(
      sort(unique(assessor_address[!is.na(assessor_address)])),
      collapse = "/"
    ),
    commercial_uses = paste(
      sort(unique(property_type_use[!is.na(property_type_use)])),
      collapse = "/"
    ),
    .groups = "drop"
  )

chain_parcel_evidence <- chain_footprints |>
  dplyr::left_join(
    point_parcel_evidence |>
      dplyr::select(
        permit_chain_id,
        footprint_id,
        pin14_2022,
        permit_point_to_parcel_ft,
        permit_point_inside_parcel
      ),
    by = c("permit_chain_id", "footprint_id", "pin14_2022"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    parcel_addresses,
    by = "pin14_2022",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    residential_pin_summary,
    by = "pin14_2022",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    commercial_pin_summary,
    by = "pin14_2022",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    permit_address_key = address_key(representative_address),
    exact_parcel_address =
      !is.na(permit_address_key) &
        stringr::str_detect(
          paste0("/", parcel_address_keys, "/"),
          stringr::fixed(paste0("/", permit_address_key, "/"))
        ),
    application_year = lubridate::year(
      representative_application_date
    ),
    assessor_year_gap = assessor_completion_year - application_year,
    residential_complete =
      !stringr::str_detect(
        assessor_class_values,
        "(^|/)299($|/)"
      ) &
        is.finite(assessor_completion_year) &
        dplyr::between(assessor_year_gap, -1L, 4L) &
        is.finite(assessor_dwelling_units) &
        assessor_dwelling_units > 0 &
        is.finite(assessor_building_sqft) &
        assessor_building_sqft >= 100 &
        is.finite(assessor_land_sqft) &
        assessor_land_sqft >= 100,
    parcel_evidence_priority = dplyr::case_when(
      residential_complete & exact_parcel_address ~ 1L,
      residential_complete & permit_point_inside_parcel ~ 2L,
      residential_complete ~ 3L,
      exact_parcel_address ~ 4L,
      permit_point_inside_parcel ~ 5L,
      TRUE ~ 6L
    )
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::mutate(
    minimum_parcel_evidence_priority = min(
      parcel_evidence_priority,
      na.rm = TRUE
    ),
    selected_parcel_candidate =
      parcel_evidence_priority == minimum_parcel_evidence_priority,
    selected_parcel_candidate_count =
      sum(selected_parcel_candidate)
  ) |>
  dplyr::ungroup()

summary <- dplyr::bind_rows(
  chain_parcel_evidence |>
    dplyr::distinct(permit_chain_id) |>
    dplyr::summarise(
      section = "coverage",
      metric = "multi_parcel_permit_chains",
      value = dplyr::n()
    ),
  chain_parcel_evidence |>
    dplyr::filter(selected_parcel_candidate) |>
    dplyr::distinct(
      permit_chain_id,
      selected_parcel_candidate_count,
      residential_complete
    ) |>
    dplyr::count(
      selected_parcel_candidate_count,
      residential_complete,
      name = "value"
    ) |>
    dplyr::transmute(
      section = "selected_candidate_status",
      metric = paste0(
        "candidate_count_",
        selected_parcel_candidate_count,
        "_residential_complete_",
        residential_complete
      ),
      value
    )
)

readr::write_csv(
  residential_history,
  "../output/residual_footprint_multi_parcel_residential_history.csv"
)
readr::write_csv(
  commercial_history,
  "../output/residual_footprint_multi_parcel_commercial_history.csv"
)
readr::write_csv(
  chain_parcel_evidence,
  "../output/residual_footprint_multi_parcel_evidence.csv"
)
readr::write_csv(
  summary,
  "../output/residual_footprint_multi_parcel_summary.csv"
)
