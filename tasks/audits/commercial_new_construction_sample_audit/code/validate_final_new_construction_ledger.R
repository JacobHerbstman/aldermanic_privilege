# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

address_key <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_squish()
}

ledger <- readr::read_csv(
  "../output/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_family = readr::col_character(),
    source_addresses = readr::col_character(),
    project_kind = readr::col_character(),
    construction_year = readr::col_double(),
    dwelling_units = readr::col_double(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    geometry_source = readr::col_character(),
    x_3435 = readr::col_double(),
    y_3435 = readr::col_double(),
    ledger_action = readr::col_character(),
    .default = readr::col_character()
  )
)
project_dispositions <- readr::read_csv(
  "../output/preferred_project_duplicate_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    duplicate_disposition = readr::col_character(),
    .default = readr::col_skip()
  )
)
pair_dispositions <- readr::read_csv(
  "../output/preferred_project_duplicate_pair_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id_1 = readr::col_character(),
    project_id_2 = readr::col_character(),
    pair_disposition = readr::col_character(),
    .default = readr::col_skip()
  )
)
addition_screen <- readr::read_csv(
  "../output/final_recovered_missing_project_dedupe_screen.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    addition_project_id = readr::col_character(),
    component_pin_already_retained = readr::col_logical(),
    plausible_retained_duplicate_count = readr::col_double(),
    .default = readr::col_skip()
  )
)
addition_pairs <- readr::read_csv(
  "../output/final_recovered_missing_project_pair_screen.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id_1 = readr::col_character(),
    project_id_2 = readr::col_character(),
    plausible_duplicate = readr::col_logical(),
    .default = readr::col_skip()
  )
)
chain_dispositions <- readr::read_csv(
  "../output/final_residual_permit_chain_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    final_disposition = readr::col_character(),
    .default = readr::col_skip()
  )
)
adversarial_duplicate_summary <- readr::read_csv(
  "../output/final_adversarial_duplicate_summary.csv",
  show_col_types = FALSE
)

components <- ledger |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::filter(!is.na(component_pins), component_pins != "")

exact_record_duplicates <- ledger |>
  dplyr::mutate(normalized_address = address_key(source_addresses)) |>
  dplyr::filter(
    !is.na(normalized_address),
    normalized_address != ""
  ) |>
  dplyr::group_by(
    normalized_address,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft
  ) |>
  dplyr::filter(dplyr::n() > 1L) |>
  dplyr::ungroup()

checks <- tibble::tibble(
  check = c(
    "project_ids_unique",
    "component_pins_unique",
    "exact_address_year_physical_records_unique",
    "duplicate_project_dispositions_complete",
    "duplicate_pair_dispositions_complete",
    "recovered_additions_not_in_retained_ledger",
    "recovered_additions_distinct_from_one_another",
    "adversarial_duplicate_review_complete",
    "residual_permit_chains_unique",
    "residual_permit_chains_fully_classified"
  ),
  failures = c(
    anyDuplicated(ledger$project_id),
    anyDuplicated(components$component_pins),
    nrow(exact_record_duplicates),
    sum(!project_dispositions$duplicate_disposition %in%
      c("retain", "suppress_duplicate")),
    sum(is.na(pair_dispositions$pair_disposition) |
      pair_dispositions$pair_disposition == ""),
    sum(
      addition_screen$component_pin_already_retained |
        addition_screen$plausible_retained_duplicate_count > 0L
    ),
    sum(addition_pairs$plausible_duplicate),
    sum(
      adversarial_duplicate_summary$value[
        adversarial_duplicate_summary$metric %in%
          c(
            "recovered_vs_retained_candidate_pairs",
            "unresolved_candidate_pairs",
            "unused_pair_decisions"
          )
      ]
    ),
    anyDuplicated(chain_dispositions$permit_chain_id),
    sum(is.na(chain_dispositions$final_disposition) |
      chain_dispositions$final_disposition == "")
  )
) |>
  dplyr::mutate(status = dplyr::if_else(failures == 0L, "pass", "fail"))

if (any(checks$status == "fail")) {
  stop(
    paste(
      "Final new-construction validation failed:",
      paste(
        checks$check[checks$status == "fail"],
        checks$failures[checks$status == "fail"],
        sep = "=",
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

points <- ledger |>
  dplyr::mutate(
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  ) |>
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = FALSE
  )

ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE) |>
  sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers(
  "../input/ward_pair_boundaries.gpkg"
)

assignment <- assign_points_to_boundaries(
  points_sf = points,
  era_values = points$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 2000L
)

boundary_scope <- dplyr::bind_cols(
  sf::st_drop_geometry(points),
  assignment
) |>
  dplyr::transmute(
    project_id,
    source_family,
    ledger_action,
    construction_year,
    project_kind,
    component_pins,
    dwelling_units,
    building_sqft,
    land_sqft,
    geometry_source,
    x_3435,
    y_3435,
    ward,
    neighbor_ward,
    ward_pair = ward_pair_id,
    distance_to_boundary_ft = dist_ft,
    within_500ft = dist_ft <= 500,
    within_1500ft = dist_ft <= 1500
  ) |>
  dplyr::arrange(construction_year, source_family, project_id)

if (any(!is.finite(boundary_scope$distance_to_boundary_ft)) ||
    any(is.na(boundary_scope$ward)) ||
    any(is.na(boundary_scope$ward_pair))) {
  stop("A final project lacks a canonical boundary assignment.", call. = FALSE)
}

summary <- dplyr::bind_rows(
  tibble::tibble(
    metric = c(
      "final_projects",
      "retained_original_projects",
      "recovered_missing_projects",
      "unique_component_pins",
      "projects_within_500ft",
      "projects_within_1500ft",
      "recovered_projects_within_500ft",
      "recovered_projects_within_1500ft",
      "duplicate_projects_suppressed",
      "residual_permit_chains"
    ),
    value = c(
      nrow(ledger),
      sum(ledger$ledger_action == "retain_existing"),
      sum(ledger$ledger_action == "add_recovered_project"),
      nrow(components),
      sum(boundary_scope$within_500ft),
      sum(boundary_scope$within_1500ft),
      sum(
        boundary_scope$within_500ft &
          boundary_scope$ledger_action == "add_recovered_project"
      ),
      sum(
        boundary_scope$within_1500ft &
          boundary_scope$ledger_action == "add_recovered_project"
      ),
      sum(
        project_dispositions$duplicate_disposition ==
          "suppress_duplicate"
      ),
      nrow(chain_dispositions)
    )
  ),
  chain_dispositions |>
    dplyr::count(final_disposition, name = "value") |>
    dplyr::transmute(
      metric = paste0(
        "residual_permit_disposition:",
        final_disposition
      ),
      value
    )
)

readr::write_csv(
  checks,
  "../output/final_new_construction_duplicate_validation.csv"
)
readr::write_csv(
  boundary_scope,
  "../output/final_new_construction_boundary_scope.csv"
)
readr::write_csv(
  summary,
  "../output/final_new_construction_validation_summary.csv"
)
