# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

final_projects <- readr::read_csv(
  "../output/final_density_model_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) |>
  dplyr::filter(project_kind == "same_pin_multiple_cards") |>
  dplyr::select(
    project_id,
    construction_year,
    within_500ft,
    within_1500ft,
    distance_to_boundary_ft,
    ward_pair,
    dwelling_units,
    building_sqft,
    land_sqft,
    density_far,
    density_dupac
  )

ledger <- readr::read_csv(
  "../output/preferred_residential_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(project_id, component_pins, source_project_ids)

projects <- final_projects |>
  dplyr::left_join(ledger, by = "project_id", relationship = "one-to-one") |>
  dplyr::mutate(
    pin = component_pins,
    permit_project_id = paste0("residential_", pin),
    geometry_project_id = source_project_ids
  )

if (
  nrow(projects) != 273L ||
    anyDuplicated(projects$project_id) ||
    anyDuplicated(projects$pin) ||
    any(is.na(projects$pin)) ||
    any(stringr::str_detect(projects$pin, "/"))
) {
  stop("Final multicard scope is not 273 unique one-PIN projects.", call. = FALSE)
}

cards <- readr::read_csv(
  "../output/residential_multicard_cards.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) |>
  dplyr::semi_join(projects, by = "pin") |>
  dplyr::mutate(
    target_card = year_built == projects$construction_year[
      match(pin, projects$pin)
    ],
    card_units = dplyr::case_when(
      class %in% c("211", "212") &
        is.finite(num_apartments) & num_apartments > 0 ~ num_apartments,
      target_card ~ 1,
      TRUE ~ NA_real_
    ),
    card_signature = paste(
      class,
      year_built,
      building_sqft,
      card_units,
      sep = "|"
    )
  ) |>
  dplyr::arrange(pin, card_num)

if (
  anyDuplicated(cards[c("pin", "card_num")]) ||
    !setequal(projects$pin, cards$pin)
) {
  stop("Multicard snapshots are not unique and complete by PIN-card.", call. = FALSE)
}

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

DBI::dbWriteTable(
  con,
  "multicard_pins",
  projects |>
    dplyr::select(pin),
  overwrite = TRUE
)

invisible(DBI::dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

history <- DBI::dbGetQuery(con, "
SELECT
  regexp_replace(trim(r.pin), '[^0-9]', '', 'g') AS pin,
  try_cast(numeric_text(r.year) AS INTEGER) AS tax_year,
  try_cast(numeric_text(r.card) AS INTEGER) AS card_num,
  trim(r.class) AS class,
  try_cast(numeric_text(r.char_yrblt) AS INTEGER) AS year_built,
  try_cast(numeric_text(r.char_bldg_sf) AS DOUBLE) AS building_sqft,
  try_cast(numeric_text(r.char_land_sf) AS DOUBLE) AS land_sqft,
  trim(r.char_apts) AS apartments_text,
  trim(r.char_use) AS single_v_multi_family,
  trim(r.char_type_resd) AS type_of_residence,
  try_cast(numeric_text(r.card_proration_rate) AS DOUBLE) AS card_proration_rate,
  trim(r.row_id) AS row_id
FROM read_csv(
  '../input/residential_improvement_characteristics_full.csv',
  all_varchar = true,
  header = true,
  ignore_errors = false,
  max_line_size = 10000000
) r
INNER JOIN multicard_pins p
  ON regexp_replace(trim(r.pin), '[^0-9]', '', 'g') = p.pin
WHERE trim(r.card) IS NOT NULL
  AND trim(r.card) != ''
ORDER BY pin, tax_year, card_num, row_id
") |>
  dplyr::mutate(
    num_apartments = dplyr::case_when(
      is.na(apartments_text) | stringr::str_squish(apartments_text) == "" ~ NA_real_,
      stringr::str_to_lower(stringr::str_squish(apartments_text)) %in%
        c("none", "zero") ~ 0,
      stringr::str_to_lower(stringr::str_squish(apartments_text)) == "one" ~ 1,
      stringr::str_to_lower(stringr::str_squish(apartments_text)) == "two" ~ 2,
      stringr::str_to_lower(stringr::str_squish(apartments_text)) == "three" ~ 3,
      stringr::str_to_lower(stringr::str_squish(apartments_text)) == "four" ~ 4,
      stringr::str_to_lower(stringr::str_squish(apartments_text)) == "five" ~ 5,
      stringr::str_to_lower(stringr::str_squish(apartments_text)) == "six" ~ 6,
      TRUE ~ suppressWarnings(as.numeric(
        stringr::str_replace_all(apartments_text, "[^0-9.-]", "")
      ))
    )
  ) |>
  dplyr::arrange(pin, card_num, tax_year, row_id) |>
  dplyr::group_by(pin, card_num, tax_year) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup()

if (anyDuplicated(history[c("pin", "card_num", "tax_year")])) {
  stop("Multicard history is not unique by PIN-card-year.", call. = FALSE)
}

card_history_summary <- history |>
  dplyr::group_by(pin) |>
  dplyr::summarise(
    history_first_year = min(tax_year, na.rm = TRUE),
    history_last_year = max(tax_year, na.rm = TRUE),
    history_card_numbers = dplyr::n_distinct(card_num),
    maximum_concurrent_cards = max(
      table(tax_year),
      na.rm = TRUE
    ),
    years_with_multiple_cards = sum(table(tax_year) > 1),
    card_numbers_with_multiple_building_values = sum(
      tapply(building_sqft, card_num, function(x) {
        dplyr::n_distinct(x[is.finite(x)]) > 1
      })
    ),
    card_numbers_with_multiple_year_built_values = sum(
      tapply(year_built, card_num, function(x) {
        dplyr::n_distinct(x[is.finite(x)]) > 1
      })
    ),
    .groups = "drop"
  )

card_summary <- cards |>
  dplyr::filter(target_card) |>
  dplyr::group_by(pin) |>
  dplyr::summarise(
    target_cards = dplyr::n(),
    target_card_numbers = paste(card_num, collapse = "/"),
    target_classes = paste(sort(unique(class)), collapse = "/"),
    target_card_signatures = dplyr::n_distinct(card_signature),
    identical_target_card_fields = target_card_signatures == 1,
    summed_card_units = sum(card_units, na.rm = TRUE),
    summed_card_building_sqft = sum(building_sqft, na.rm = TRUE),
    distinct_card_unit_values = paste(
      sort(unique(card_units[is.finite(card_units)])),
      collapse = "/"
    ),
    distinct_card_building_values = paste(
      sort(unique(building_sqft[is.finite(building_sqft)])),
      collapse = "/"
    ),
    all_target_card_units_observed = all(is.finite(card_units) & card_units > 0),
    all_target_card_building_sqft_observed =
      all(is.finite(building_sqft) & building_sqft > 0),
    .groups = "drop"
  )

permit_links <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    permit_number = readr::col_character(),
    application_date = readr::col_date(),
    issue_date = readr::col_date(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(source_family == "residential") |>
  dplyr::inner_join(
    projects |>
      dplyr::select(project_id, permit_project_id, construction_year),
    by = c("project_id" = "permit_project_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::rename(final_project_id = project_id.y) |>
  dplyr::filter(
    directly_matched,
    lubridate::year(application_date) >= construction_year - 4L,
    lubridate::year(application_date) <= construction_year + 2L
  )

unit_mentions <- readr::read_csv(
  "../output/project_permit_chain_unit_mentions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(source_family == "residential") |>
  dplyr::inner_join(
    projects |>
      dplyr::select(project_id, permit_project_id),
    by = c("project_id" = "permit_project_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::rename(final_project_id = project_id.y)

permit_chains <- permit_links |>
  dplyr::group_by(final_project_id, permit_chain_id) |>
  dplyr::summarise(
    permit_numbers = paste(sort(unique(permit_number)), collapse = "/"),
    application_date = min(application_date, na.rm = TRUE),
    issue_date = min(issue_date, na.rm = TRUE),
    permit_addresses = paste(
      sort(unique(permit_address[!is.na(permit_address) & permit_address != ""])),
      collapse = " / "
    ),
    work_descriptions = paste(
      unique(work_description[!is.na(work_description) & work_description != ""]),
      collapse = " || "
    ),
    exact_pin_match = any(direct_match_method == "exact_pin", na.rm = TRUE),
    polygon_match = any(direct_match_method == "project_polygon", na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    unit_mentions |>
      dplyr::group_by(final_project_id, permit_chain_id) |>
      dplyr::summarise(
        chain_unit_values = paste(sort(unique(unit_count)), collapse = "/"),
        chain_unit_value_count = dplyr::n_distinct(unit_count),
        chain_units = dplyr::if_else(
          chain_unit_value_count == 1L,
          dplyr::first(unit_count),
          NA_real_
        ),
        unit_evidence = paste(unique(mention_context), collapse = " || "),
        .groups = "drop"
      ),
    by = c("final_project_id", "permit_chain_id"),
    relationship = "one-to-one"
  )

permit_chain_assignments <- permit_chains |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    linked_projects = dplyr::n_distinct(final_project_id),
    exact_pin_projects = dplyr::n_distinct(final_project_id[exact_pin_match]),
    polygon_projects = dplyr::n_distinct(final_project_id[polygon_match]),
    project_ids = paste(sort(unique(final_project_id)), collapse = "/"),
    permit_addresses = paste(
      sort(unique(permit_addresses[permit_addresses != ""])),
      collapse = " / "
    ),
    .groups = "drop"
  )

permit_summary <- permit_chains |>
  dplyr::group_by(final_project_id) |>
  dplyr::summarise(
    permit_chains = dplyr::n(),
    exact_pin_permit_chains = sum(exact_pin_match),
    polygon_only_permit_chains = sum(polygon_match & !exact_pin_match),
    distinct_permit_addresses = dplyr::n_distinct(
      permit_addresses[permit_addresses != ""]
    ),
    exact_pin_distinct_permit_addresses = dplyr::n_distinct(
      permit_addresses[exact_pin_match & permit_addresses != ""]
    ),
    permit_addresses = paste(
      unique(permit_addresses[permit_addresses != ""]),
      collapse = " / "
    ),
    exact_pin_permit_addresses = paste(
      unique(permit_addresses[exact_pin_match & permit_addresses != ""]),
      collapse = " / "
    ),
    permit_unit_values = paste(
      sort(unique(chain_units[is.finite(chain_units)])),
      collapse = "/"
    ),
    exact_pin_permit_unit_values = paste(
      sort(unique(chain_units[exact_pin_match & is.finite(chain_units)])),
      collapse = "/"
    ),
    permit_chains_with_units = sum(is.finite(chain_units)),
    exact_pin_permit_chains_with_units =
      sum(exact_pin_match & is.finite(chain_units)),
    all_permit_chains_have_units = all(is.finite(chain_units)),
    all_exact_pin_permit_chains_have_units =
      any(exact_pin_match) & all(is.finite(chain_units[exact_pin_match])),
    summed_permit_chain_units = dplyr::if_else(
      all_permit_chains_have_units,
      sum(chain_units),
      NA_real_
    ),
    summed_exact_pin_permit_chain_units = dplyr::if_else(
      all_exact_pin_permit_chains_have_units,
      sum(chain_units[exact_pin_match]),
      NA_real_
    ),
    permit_evidence = paste(
      paste0(
        permit_chain_id,
        " [", dplyr::if_else(exact_pin_match, "exact PIN", "project polygon"), "]",
        " [", application_date, "] ",
        permit_addresses,
        " units=", dplyr::coalesce(as.character(chain_units), "missing"),
        " description=", work_descriptions
      ),
      collapse = " || "
    ),
    exact_pin_permit_evidence = paste(
      paste0(
        permit_chain_id[exact_pin_match],
        " [", application_date[exact_pin_match], "] ",
        permit_addresses[exact_pin_match],
        " units=",
        dplyr::coalesce(as.character(chain_units[exact_pin_match]), "missing"),
        " description=", work_descriptions[exact_pin_match]
      ),
      collapse = " || "
    ),
    .groups = "drop"
  ) |>
  dplyr::rename(project_id = final_project_id)

current_addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) |>
  dplyr::semi_join(projects, by = "pin") |>
  dplyr::group_by(pin) |>
  dplyr::summarise(
    current_addresses = paste(
      sort(unique(prop_address_full[
        !is.na(prop_address_full) & prop_address_full != ""
      ])),
      collapse = " / "
    ),
    current_address_count = dplyr::n_distinct(
      prop_address_full[!is.na(prop_address_full) & prop_address_full != ""]
    ),
    .groups = "drop"
  )

manual_review <- readr::read_csv(
  "../../density_multicard_manual_review/output/manual_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) |>
  dplyr::mutate(pin = sprintf("%014.0f", as.numeric(pin))) |>
  dplyr::select(
    pin,
    manual_address = address,
    manual_physical_buildings = physical_buildings,
    manual_verified_units = verified_units,
    manual_verified_sqft = verified_sqft,
    manual_classification = classification,
    manual_confidence = confidence,
    manual_evidence = evidence
  )

evidence <- projects |>
  dplyr::left_join(card_summary, by = "pin", relationship = "one-to-one") |>
  dplyr::left_join(card_history_summary, by = "pin", relationship = "one-to-one") |>
  dplyr::left_join(permit_summary, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(current_addresses, by = "pin", relationship = "one-to-one") |>
  dplyr::left_join(manual_review, by = "pin", relationship = "one-to-one") |>
  dplyr::mutate(
    permit_chains = dplyr::coalesce(permit_chains, 0L),
    exact_pin_permit_chains = dplyr::coalesce(exact_pin_permit_chains, 0L),
    polygon_only_permit_chains =
      dplyr::coalesce(polygon_only_permit_chains, 0L),
    permit_chains_with_units = dplyr::coalesce(permit_chains_with_units, 0L),
    exact_pin_permit_chains_with_units =
      dplyr::coalesce(exact_pin_permit_chains_with_units, 0L),
    selected_and_sum_differ =
      dwelling_units != summed_card_units |
      building_sqft != summed_card_building_sqft,
    summed_cards_change_multifamily =
      dwelling_units <= 1 & summed_card_units > 1,
    exact_pin_permit_units_match_selected =
      is.finite(summed_exact_pin_permit_chain_units) &
      summed_exact_pin_permit_chain_units == dwelling_units,
    exact_pin_permit_units_match_card_sum =
      is.finite(summed_exact_pin_permit_chain_units) &
      summed_exact_pin_permit_chain_units == summed_card_units,
    review_priority = dplyr::case_when(
      !is.na(manual_classification) ~ "previous_manual_review",
      within_500ft & summed_cards_change_multifamily ~ "1_multifamily_threshold",
      within_500ft & selected_and_sum_differ ~ "2_main_sample_outcome",
      summed_cards_change_multifamily ~ "3_placebo_scope_threshold",
      selected_and_sum_differ ~ "4_placebo_scope_outcome",
      TRUE ~ "5_confirmation"
    )
  ) |>
  dplyr::arrange(review_priority, distance_to_boundary_ft, project_id)

if (
  nrow(evidence) != nrow(projects) ||
    anyDuplicated(evidence$project_id) ||
    any(is.na(evidence$target_cards))
) {
  stop("Multicard evidence is not complete and unique by project.", call. = FALSE)
}

summary <- dplyr::bind_rows(
  tibble::tibble(metric = "final_multicard_projects_within_1500ft", value = nrow(evidence)),
  tibble::tibble(metric = "final_multicard_projects_within_500ft", value = sum(evidence$within_500ft)),
  tibble::tibble(
    metric = "within_500ft_selected_multifamily",
    value = sum(evidence$within_500ft & evidence$dwelling_units > 1)
  ),
  tibble::tibble(
    metric = "within_500ft_summed_multifamily",
    value = sum(evidence$within_500ft & evidence$summed_card_units > 1)
  ),
  tibble::tibble(
    metric = "projects_with_direct_permit_chains",
    value = sum(evidence$permit_chains > 0)
  ),
  tibble::tibble(
    metric = "projects_with_exact_pin_permit_chains",
    value = sum(evidence$exact_pin_permit_chains > 0)
  ),
  tibble::tibble(
    metric = "projects_with_exact_pin_permit_unit_evidence",
    value = sum(evidence$exact_pin_permit_chains_with_units > 0)
  ),
  tibble::tibble(
    metric = "permit_chains_linked_to_multiple_multicard_projects",
    value = sum(permit_chain_assignments$linked_projects > 1)
  ),
  evidence |>
    dplyr::count(review_priority, name = "value") |>
    dplyr::transmute(metric = paste0("review_priority:", review_priority), value)
)

readr::write_csv(history, "../output/multicard_card_history.csv")
readr::write_csv(cards, "../output/multicard_card_snapshot.csv")
readr::write_csv(permit_chains, "../output/multicard_permit_evidence.csv")
readr::write_csv(
  permit_chain_assignments,
  "../output/multicard_permit_chain_assignments.csv"
)
readr::write_csv(evidence, "../output/multicard_project_evidence_base.csv")
readr::write_csv(summary, "../output/multicard_project_evidence_summary.csv")
