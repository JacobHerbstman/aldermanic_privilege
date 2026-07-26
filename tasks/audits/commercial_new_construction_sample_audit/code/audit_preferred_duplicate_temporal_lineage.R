# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

pairs <- readr::read_csv(
  "../output/preferred_project_duplicate_review_pairs.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id_1 = readr::col_character(),
    project_id_2 = readr::col_character(),
    .default = readr::col_guess()
  )
)
components <- readr::read_csv(
  "../output/preferred_project_duplicate_review_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::distinct(project_id, component_pin)
target_pins <- components |>
  dplyr::distinct(component_pin)

if (anyDuplicated(pairs[c("project_id_1", "project_id_2")]) ||
    anyDuplicated(components[c("project_id", "component_pin")])) {
  stop("Duplicate review keys are invalid.", call. = FALSE)
}

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
SELECT DISTINCT
  regexp_replace(trim(r.pin), '[^0-9]', '', 'g') AS component_pin,
  try_cast(numeric_text(r.year) AS INTEGER) AS tax_year,
  try_cast(numeric_text(r.card) AS INTEGER) AS card_num,
  trim(r.class) AS assessor_class,
  regexp_replace(trim(r.tieback_key_pin), '[^0-9]', '', 'g')
    AS tieback_group,
  try_cast(numeric_text(r.char_yrblt) AS INTEGER) AS year_built,
  try_cast(numeric_text(r.char_bldg_sf) AS DOUBLE) AS building_sqft,
  try_cast(numeric_text(r.char_land_sf) AS DOUBLE) AS land_sqft
FROM read_csv(
  '../input/residential_improvement_characteristics_full.csv',
  all_varchar = true,
  header = true,
  ignore_errors = false,
  max_line_size = 10000000
) AS r
INNER JOIN target_pins AS p
  ON regexp_replace(trim(r.pin), '[^0-9]', '', 'g') = p.component_pin
WHERE try_cast(numeric_text(r.township_code) AS INTEGER)
      IN (70, 71, 72, 73, 74, 75, 76, 77)
  AND try_cast(numeric_text(r.year) AS INTEGER) BETWEEN 2000 AND 2026;
")

project_history <- components |>
  dplyr::left_join(
    residential_history,
    by = "component_pin",
    relationship = "one-to-many"
  )
project_summary <- project_history |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    first_assessor_year = suppressWarnings(min(tax_year, na.rm = TRUE)),
    last_assessor_year = suppressWarnings(max(tax_year, na.rm = TRUE)),
    assessor_year_values = paste(
      sort(unique(tax_year[is.finite(tax_year)])),
      collapse = "/"
    ),
    assessor_class_values = paste(
      sort(unique(assessor_class[
        !is.na(assessor_class) & assessor_class != ""
      ])),
      collapse = "/"
    ),
    tieback_group_values = paste(
      sort(unique(tieback_group[
        !is.na(tieback_group) & tieback_group != ""
      ])),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    first_assessor_year = dplyr::if_else(
      is.infinite(first_assessor_year),
      NA_real_,
      first_assessor_year
    ),
    last_assessor_year = dplyr::if_else(
      is.infinite(last_assessor_year),
      NA_real_,
      last_assessor_year
    )
  )
project_years <- split(
  project_history$tax_year[is.finite(project_history$tax_year)],
  project_history$project_id[is.finite(project_history$tax_year)]
)
project_tiebacks <- split(
  project_history$tieback_group[
    !is.na(project_history$tieback_group) &
      project_history$tieback_group != ""
  ],
  project_history$project_id[
    !is.na(project_history$tieback_group) &
      project_history$tieback_group != ""
  ]
)

temporal_pairs <- pairs |>
  dplyr::left_join(
    project_summary |>
      dplyr::rename_with(
        ~ paste0(.x, "_1"),
        -project_id
      ),
    by = c("project_id_1" = "project_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    project_summary |>
      dplyr::rename_with(
        ~ paste0(.x, "_2"),
        -project_id
      ),
    by = c("project_id_2" = "project_id"),
    relationship = "many-to-one"
  )

temporal_pairs$shared_assessor_year_count <- purrr::map2_int(
  temporal_pairs$project_id_1,
  temporal_pairs$project_id_2,
  function(project_1, project_2) {
    length(intersect(
      unique(project_years[[project_1]]),
      unique(project_years[[project_2]])
    ))
  }
)
temporal_pairs$shared_tieback_group_count <- purrr::map2_int(
  temporal_pairs$project_id_1,
  temporal_pairs$project_id_2,
  function(project_1, project_2) {
    length(intersect(
      unique(project_tiebacks[[project_1]]),
      unique(project_tiebacks[[project_2]])
    ))
  }
)
temporal_pairs <- temporal_pairs |>
  dplyr::mutate(
    coexisted_in_assessor = shared_assessor_year_count > 0L,
    assessor_span_gap = dplyr::case_when(
      !is.finite(first_assessor_year_1) |
        !is.finite(last_assessor_year_1) |
        !is.finite(first_assessor_year_2) |
        !is.finite(last_assessor_year_2) ~ NA_real_,
      last_assessor_year_1 < first_assessor_year_2 ~
        first_assessor_year_2 - last_assessor_year_1,
      last_assessor_year_2 < first_assessor_year_1 ~
        first_assessor_year_1 - last_assessor_year_2,
      TRUE ~ 0
    ),
    both_class_295_only =
      assessor_class_values_1 == "295" &
        assessor_class_values_2 == "295",
    temporal_lineage_status = dplyr::case_when(
      coexisted_in_assessor &
        both_class_295_only ~
        "contemporaneous_class_295_rowhouse_parcels",
      coexisted_in_assessor &
        distinct_current_addresses ~
        "contemporaneous_distinct_address_parcels",
      coexisted_in_assessor &
        shared_tieback_group_count == 0L &
        !exact_duplicate_fields ~
        "contemporaneous_distinct_assessor_parcels",
      !coexisted_in_assessor &
        (
          likely_lineage_repetition |
            nearby_possible_lineage_repetition
        ) &
        is.finite(assessor_span_gap) &
        assessor_span_gap <= 2L ~
        "likely_predecessor_successor_duplicate",
      coexisted_in_assessor &
        shared_tieback_group_count > 0L ~
        "contemporaneous_shared_tieback_requires_review",
      coexisted_in_assessor ~
        "contemporaneous_same_site_requires_review",
      TRUE ~ "insufficient_temporal_evidence"
    )
  )

summary <- dplyr::bind_rows(
  temporal_pairs |>
    dplyr::count(temporal_lineage_status, name = "value") |>
    dplyr::transmute(
      section = "pair_status",
      metric = temporal_lineage_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "review_pairs",
      "projects",
      "component_pins",
      "residential_history_rows",
      "pairs_with_assessor_coexistence",
      "likely_predecessor_successor_duplicates"
    ),
    value = c(
      nrow(pairs),
      dplyr::n_distinct(c(pairs$project_id_1, pairs$project_id_2)),
      dplyr::n_distinct(components$component_pin),
      nrow(residential_history),
      sum(temporal_pairs$coexisted_in_assessor),
      sum(
        temporal_pairs$temporal_lineage_status ==
          "likely_predecessor_successor_duplicate"
      )
    )
  )
)

readr::write_csv(
  project_history,
  "../output/preferred_project_duplicate_temporal_history.csv"
)
readr::write_csv(
  temporal_pairs,
  "../output/preferred_project_duplicate_temporal_pairs.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_project_duplicate_temporal_summary.csv"
)
