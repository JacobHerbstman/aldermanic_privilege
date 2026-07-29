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
  "../output/permit_candidate_historical_parcel_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_site_id = readr::col_character(),
    footprint_id = readr::col_character(),
    historical_pin14 = readr::col_character(),
    historical_pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(stringr::str_length(historical_pin14) == 14L) |>
  dplyr::distinct(
    candidate_site_id,
    target_year,
    object_id,
    historical_pin14,
    historical_pin10
  ) |>
  dplyr::mutate(
    site_pin_target_id = paste(
      candidate_site_id,
      target_year,
      object_id,
      historical_pin14,
      sep = ":"
    )
  )
site_ledger <- readr::read_csv(
  "../output/permit_candidate_site_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_site_id = readr::col_character(),
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
projects <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(site_ledger$candidate_site_id) ||
    anyDuplicated(parcel_matches$site_pin_target_id) ||
    anyDuplicated(components$component_pin) ||
    anyDuplicated(projects$project_id)) {
  stop("Candidate sites and retained project ledgers must be unique.", call. = FALSE)
}

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbWriteTable(
  con,
  "candidate_site_pins",
  parcel_matches,
  overwrite = TRUE
)
DBI::dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
")

site_history <- DBI::dbGetQuery(con, "
SELECT
  c.site_pin_target_id,
  c.candidate_site_id,
  c.target_year,
  c.object_id,
  c.historical_pin14,
  c.historical_pin10,
  regexp_replace(trim(r.pin), '[^0-9]', '', 'g') AS pin,
  try_cast(numeric_text(r.year) AS INTEGER) AS tax_year,
  try_cast(numeric_text(r.card) AS INTEGER) AS card_num,
  trim(r.class) AS class,
  regexp_replace(trim(r.tieback_key_pin), '[^0-9]', '', 'g') AS tieback_group,
  try_cast(numeric_text(r.tieback_proration_rate) AS DOUBLE) AS pin_proration_rate,
  try_cast(numeric_text(r.card_proration_rate) AS DOUBLE) AS card_proration_rate,
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
INNER JOIN candidate_site_pins AS c
  ON regexp_replace(trim(r.pin), '[^0-9]', '', 'g') = c.historical_pin14
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
  dplyr::arrange(pin, card_num, tax_year, row_id) |>
  dplyr::group_by(pin, card_num, tax_year) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup()

sites_without_history <- parcel_matches |>
  dplyr::anti_join(
    site_history |>
      dplyr::distinct(site_pin_target_id),
    by = "site_pin_target_id"
  )
site_history <- dplyr::bind_rows(
  site_history,
  sites_without_history
) |>
  dplyr::left_join(
    site_ledger |>
      dplyr::select(
        candidate_site_id,
        earliest_application_date,
        maximum_unit_mention,
        permit_addresses
      ),
    by = "candidate_site_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    application_year = lubridate::year(earliest_application_date),
    target_year_gap = abs(year_built - target_year),
    application_year_gap = year_built - application_year,
    plausible_construction_episode =
      is.finite(year_built) &
      dplyr::between(year_built, 2006L, 2022L) &
      (
        target_year_gap <= 2L |
          dplyr::between(application_year_gap, -1L, 4L)
      ),
    report_priority = dplyr::case_when(
      is.finite(tax_year) & tax_year <= 2022L ~ 1L,
      is.finite(tax_year) & tax_year <= 2025L ~ 2L,
      TRUE ~ 3L
    )
  )

selected_cards <- site_history |>
  dplyr::filter(plausible_construction_episode) |>
  dplyr::group_by(
    candidate_site_id,
    historical_pin14,
    target_year,
    card_num
  ) |>
  dplyr::arrange(
    target_year_gap,
    report_priority,
    dplyr::desc(tax_year),
    dplyr::desc(row_id),
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()

selected_pin_land <- selected_cards |>
  dplyr::group_by(candidate_site_id, historical_pin14, target_year) |>
  dplyr::summarise(
    selected_pin_land_sqft = dplyr::if_else(
      dplyr::n_distinct(land_sqft[is.finite(land_sqft)]) == 1L,
      min(land_sqft[is.finite(land_sqft)]),
      NA_real_
    ),
    selected_pin_land_values = paste(
      sort(unique(land_sqft[is.finite(land_sqft)])),
      collapse = "/"
    ),
    .groups = "drop"
  )

selected_site_evidence <- selected_cards |>
  dplyr::group_by(candidate_site_id) |>
  dplyr::summarise(
    historical_pins_with_episode = dplyr::n_distinct(historical_pin14),
    selected_cards = dplyr::n(),
    selected_year_values = paste(
      sort(unique(year_built)),
      collapse = "/"
    ),
    selected_tax_year_values = paste(
      sort(unique(tax_year)),
      collapse = "/"
    ),
    assessor_units = sum(dwelling_units, na.rm = TRUE),
    assessor_building_sqft = sum(building_sqft, na.rm = TRUE),
    assessor_unit_values = paste(
      sort(unique(dwelling_units[is.finite(dwelling_units)])),
      collapse = "/"
    ),
    assessor_building_sqft_values = paste(
      sort(unique(building_sqft[is.finite(building_sqft)])),
      collapse = "/"
    ),
    selected_row_ids = paste(sort(unique(row_id)), collapse = "/"),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    selected_pin_land |>
      dplyr::group_by(candidate_site_id) |>
      dplyr::summarise(
        assessor_land_sqft = sum(selected_pin_land_sqft, na.rm = TRUE),
        assessor_land_values = paste(
          selected_pin_land_values,
          collapse = " || "
        ),
        .groups = "drop"
      ),
    by = "candidate_site_id",
    relationship = "one-to-one"
  )

pin_representation <- parcel_matches |>
  dplyr::distinct(candidate_site_id, historical_pin14) |>
  dplyr::left_join(
    components |>
      dplyr::select(
        represented_project_id = project_id,
        historical_pin14 = component_pin
      ),
    by = "historical_pin14",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(candidate_site_id) |>
  dplyr::summarise(
    recovered_historical_pins = dplyr::n_distinct(historical_pin14),
    represented_historical_pins = dplyr::n_distinct(
      historical_pin14[!is.na(represented_project_id)]
    ),
    exact_pin_project_ids = paste(
      sort(unique(represented_project_id[!is.na(represented_project_id)])),
      collapse = "/"
    ),
    .groups = "drop"
  )

candidate_parcels <- sf::st_read(
  "../output/permit_candidate_historical_parcels.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::inner_join(
    parcel_matches,
    by = c(
      "target_year",
      "object_id",
      "historical_pin14",
      "historical_pin10"
    ),
    relationship = "one-to-many"
  )
project_geometry <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::semi_join(projects, by = "project_id")

intersection_rows <- sf::st_intersects(candidate_parcels, project_geometry)
spatial_links <- purrr::map2_dfr(
  seq_len(nrow(candidate_parcels)),
  intersection_rows,
  function(candidate_row, project_rows) {
    if (length(project_rows) == 0) {
      return(tibble::tibble())
    }
    candidate <- candidate_parcels[candidate_row, ]
    neighbors <- project_geometry[project_rows, ]
    intersection_area <- purrr::map_dbl(
      seq_len(nrow(neighbors)),
      function(i) {
        intersection <- suppressWarnings(sf::st_intersection(
          sf::st_geometry(candidate),
          sf::st_geometry(neighbors[i, ])
        ))
        if (length(intersection) == 0) {
          0
        } else {
          sum(as.numeric(sf::st_area(intersection)))
        }
      }
    )
    tibble::tibble(
      candidate_site_id = candidate$candidate_site_id,
      historical_pin14 = candidate$historical_pin14,
      target_year = candidate$target_year,
      project_id = neighbors$project_id,
      project_year = neighbors$target_year,
      candidate_parcel_area_sqft = as.numeric(sf::st_area(candidate)),
      intersection_area_sqft = intersection_area
    )
  }
) |>
  dplyr::mutate(
    candidate_parcel_overlap_share =
      intersection_area_sqft / candidate_parcel_area_sqft,
    project_year_gap = project_year - target_year
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::select(
        project_id,
        project_source_family = source_family,
        project_component_pins = component_pins,
        project_units = dwelling_units,
        project_building_sqft = building_sqft,
        project_land_sqft = land_sqft
      ),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::arrange(
    candidate_site_id,
    dplyr::desc(candidate_parcel_overlap_share),
    abs(project_year_gap),
    project_id
  )

spatial_site_evidence <- spatial_links |>
  dplyr::group_by(candidate_site_id) |>
  dplyr::summarise(
    intersecting_preferred_projects = dplyr::n_distinct(project_id),
    maximum_candidate_parcel_overlap_share = max(
      candidate_parcel_overlap_share
    ),
    intersecting_project_ids = paste(
      sort(unique(project_id)),
      collapse = "/"
    ),
    minimum_intersecting_project_year_gap = min(abs(project_year_gap)),
    .groups = "drop"
  )

site_evidence <- site_ledger |>
  dplyr::left_join(
    pin_representation,
    by = "candidate_site_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    selected_site_evidence,
    by = "candidate_site_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    spatial_site_evidence,
    by = "candidate_site_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    recovered_historical_pins = dplyr::coalesce(
      recovered_historical_pins,
      0L
    ),
    represented_historical_pins = dplyr::coalesce(
      represented_historical_pins,
      0L
    ),
    historical_pins_with_episode = dplyr::coalesce(
      historical_pins_with_episode,
      0L
    ),
    intersecting_preferred_projects = dplyr::coalesce(
      intersecting_preferred_projects,
      0L
    ),
    permit_assessor_units_agree =
      is.finite(maximum_unit_mention) &
      is.finite(assessor_units) &
      maximum_unit_mention == assessor_units,
    evidence_status = dplyr::case_when(
      represented_historical_pins > 0 ~
        "exact_historical_pin_already_represented",
      historical_pins_with_episode > 0 &
        intersecting_preferred_projects == 0 ~
        "assessor_episode_on_unrepresented_site",
      historical_pins_with_episode > 0 &
        intersecting_preferred_projects > 0 ~
        "assessor_episode_on_site_with_retained_project",
      recovered_historical_pins > 0 ~
        "historical_parcel_without_residential_assessor_episode",
      TRUE ~ "historical_parcel_unresolved"
    )
  ) |>
  dplyr::arrange(candidate_site_id)

summary <- dplyr::bind_rows(
  site_evidence |>
    dplyr::count(evidence_status, name = "value") |>
    dplyr::transmute(
      section = "site_status",
      metric = evidence_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "candidate_sites",
      "candidate_sites_with_assessor_history",
      "candidate_sites_with_plausible_construction_episode",
      "candidate_sites_with_exact_pin_in_preferred_ledger",
      "candidate_sites_intersecting_preferred_project_geometry",
      "candidate_sites_with_permit_assessor_unit_agreement"
    ),
    value = c(
      nrow(site_ledger),
      dplyr::n_distinct(site_history$candidate_site_id[
        !is.na(site_history$row_id)
      ]),
      sum(site_evidence$historical_pins_with_episode > 0),
      sum(site_evidence$represented_historical_pins > 0),
      sum(site_evidence$intersecting_preferred_projects > 0),
      sum(site_evidence$permit_assessor_units_agree, na.rm = TRUE)
    )
  )
)

readr::write_csv(
  site_history,
  "../output/permit_candidate_assessor_history.csv"
)
readr::write_csv(
  selected_cards,
  "../output/permit_candidate_assessor_selected_cards.csv"
)
readr::write_csv(
  spatial_links,
  "../output/permit_candidate_preferred_project_spatial_links.csv"
)
readr::write_csv(
  site_evidence,
  "../output/permit_candidate_assessor_site_evidence.csv"
)
readr::write_csv(
  summary,
  "../output/permit_candidate_assessor_summary.csv"
)
