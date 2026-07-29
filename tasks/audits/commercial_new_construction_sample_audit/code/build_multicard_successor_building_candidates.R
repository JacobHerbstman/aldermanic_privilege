# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

noncondo_candidates <- readr::read_csv(
  "../output/multicard_current_successor_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    project_pin = readr::col_character(),
    current_pin = readr::col_character(),
    current_pin10 = readr::col_character(),
    component_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    current_class != "299",
    stringr::str_detect(current_class, "^(2|396)"),
    latest_assessor_units > 0,
    latest_assessor_building_sqft > 0,
    latest_assessor_year_built != "",
    search_geometry_source != "centroid_150ft_candidate_search"
  ) |>
  dplyr::mutate(
    successor_year = suppressWarnings(as.integer(
      stringr::str_extract(latest_assessor_year_built, "[0-9]{4}")
    )),
    year_gap = successor_year - construction_year
  ) |>
  dplyr::transmute(
    project_id,
    project_pin,
    construction_year,
    within_500ft,
    successor_id = paste0("noncondo_", current_pin),
    successor_type = "current_noncondo_parcel",
    successor_pin = current_pin,
    successor_pin10 = current_pin10,
    successor_address = current_address,
    successor_year,
    year_gap,
    successor_class = current_class,
    successor_units = latest_assessor_units,
    successor_building_sqft = latest_assessor_building_sqft,
    successor_land_sqft = latest_assessor_land_sqft,
    successor_x_3435 = current_x_3435,
    successor_y_3435 = current_y_3435,
    represented_project_ids = component_project_ids,
    search_geometry_source
  ) |>
  dplyr::distinct(project_id, successor_id, .keep_all = TRUE)

condo_links <- readr::read_csv(
  "../output/multicard_successor_condo_requests.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id,
    construction_year,
    successor_pin10 = pin10,
    projects_per_condo_base
  ) |>
  dplyr::distinct()

project_fields <- readr::read_csv(
  "../output/multicard_project_evidence_base.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id,
    project_pin = pin,
    within_500ft
  )

condo_coordinates <- readr::read_csv(
  "../output/multicard_successor_condo_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    search_geometry_source != "centroid_150ft_candidate_search"
  ) |>
  dplyr::group_by(project_id, pin10) |>
  dplyr::summarise(
    successor_pin = paste(sort(unique(pin)), collapse = "/"),
    search_geometry_source = dplyr::first(search_geometry_source),
    .groups = "drop"
  ) |>
  dplyr::rename(successor_pin10 = pin10)

current_parcels <- data.table::fread(
  "../input/parcel_universe_2025_city.csv",
  select = c(
    "pin", "pin10", "class",
    "centroid_x_crs_3435", "centroid_y_crs_3435"
  ),
  colClasses = "character"
) |>
  tibble::as_tibble() |>
  dplyr::transmute(
    pin = stringr::str_pad(
      stringr::str_replace_all(pin, "[^0-9]", ""),
      14,
      pad = "0"
    ),
    successor_pin10 = stringr::str_pad(
      stringr::str_replace_all(pin10, "[^0-9]", ""),
      10,
      pad = "0"
    ),
    class = stringr::str_squish(class),
    x_3435 = as.numeric(centroid_x_crs_3435),
    y_3435 = as.numeric(centroid_y_crs_3435)
  ) |>
  dplyr::filter(
    class == "299",
    is.finite(x_3435),
    is.finite(y_3435)
  ) |>
  dplyr::group_by(successor_pin10) |>
  dplyr::summarise(
    successor_x_3435 = mean(x_3435),
    successor_y_3435 = mean(y_3435),
    .groups = "drop"
  )

condo_evidence <- readr::read_csv(
  "../output/multicard_successor_condo_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(year <= 2025) |>
  dplyr::group_by(pin10) |>
  dplyr::filter(year == max(year)) |>
  dplyr::summarise(
    successor_units = dplyr::n_distinct(
      pin[is_parking_space %in% FALSE & is_common_area %in% FALSE]
    ),
    successor_year = suppressWarnings(
      max(as.integer(char_yrblt), na.rm = TRUE)
    ),
    successor_building_sqft = suppressWarnings(
      max(as.numeric(char_building_sf), na.rm = TRUE)
    ),
    successor_land_sqft = suppressWarnings(
      max(as.numeric(char_land_sf), na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        successor_year,
        successor_building_sqft,
        successor_land_sqft
      ),
      ~ dplyr::if_else(is.infinite(.x), NA_real_, as.numeric(.x))
    )
  ) |>
  dplyr::rename(successor_pin10 = pin10)

condo_addresses <- data.table::fread(
  "../input/parcel_addresses_2025_chicago.csv",
  select = c("pin10", "prop_address_full"),
  colClasses = "character"
) |>
  tibble::as_tibble() |>
  dplyr::transmute(
    successor_pin10 = stringr::str_pad(
      stringr::str_replace_all(pin10, "[^0-9]", ""),
      10,
      pad = "0"
    ),
    successor_address = stringr::str_squish(prop_address_full)
  ) |>
  dplyr::filter(successor_address != "") |>
  dplyr::group_by(successor_pin10) |>
  dplyr::summarise(
    successor_address = paste(
      sort(unique(successor_address)),
      collapse = " / "
    ),
    .groups = "drop"
  )

condo_candidates <- condo_links |>
  dplyr::inner_join(
    project_fields,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::inner_join(
    condo_coordinates,
    by = c("project_id", "successor_pin10"),
    relationship = "one-to-one"
  ) |>
  dplyr::inner_join(
    condo_evidence,
    by = "successor_pin10",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    condo_addresses,
    by = "successor_pin10",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    current_parcels,
    by = "successor_pin10",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(year_gap = successor_year - construction_year) |>
  dplyr::transmute(
    project_id,
    project_pin,
    construction_year,
    within_500ft,
    successor_id = paste0("condo_", successor_pin10),
    successor_type = "current_condo_building",
    successor_pin,
    successor_pin10,
    successor_address,
    successor_year,
    year_gap,
    successor_class = "299",
    successor_units,
    successor_building_sqft,
    successor_land_sqft,
    successor_x_3435,
    successor_y_3435,
    represented_project_ids = NA_character_,
    search_geometry_source,
    projects_per_condo_base
  ) |>
  dplyr::distinct(project_id, successor_id, .keep_all = TRUE)

candidates <- dplyr::bind_rows(
  noncondo_candidates |>
    dplyr::mutate(projects_per_condo_base = NA_integer_),
  condo_candidates
) |>
  dplyr::mutate(
    within_one_year = abs(year_gap) <= 1,
    within_two_years = abs(year_gap) <= 2,
    automatic_candidate =
      within_two_years &
        dplyr::coalesce(projects_per_condo_base <= 1L, TRUE)
  ) |>
  dplyr::arrange(
    project_id,
    abs(year_gap),
    successor_type,
    successor_address,
    successor_id
  )

project_summary <- candidates |>
  dplyr::filter(automatic_candidate) |>
  dplyr::group_by(project_id, construction_year, within_500ft) |>
  dplyr::summarise(
    successor_buildings = dplyr::n_distinct(successor_id),
    successor_noncondo_buildings = dplyr::n_distinct(
      successor_id[successor_type == "current_noncondo_parcel"]
    ),
    successor_condo_buildings = dplyr::n_distinct(
      successor_id[successor_type == "current_condo_building"]
    ),
    successor_units_sum = sum(successor_units, na.rm = TRUE),
    successor_building_sqft_sum =
      sum(successor_building_sqft, na.rm = TRUE),
    successor_land_sqft_sum = sum(successor_land_sqft, na.rm = TRUE),
    successor_addresses = paste(
      sort(unique(stats::na.omit(successor_address))),
      collapse = " | "
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(project_id)

summary <- tibble::tibble(
  metric = c(
    "multicard_projects_with_successor_building_candidates",
    "successor_building_candidates",
    "automatic_successor_building_candidates",
    "noncondo_successor_candidates",
    "condo_successor_candidates"
  ),
  value = c(
    dplyr::n_distinct(candidates$project_id),
    nrow(candidates),
    sum(candidates$automatic_candidate),
    sum(candidates$successor_type == "current_noncondo_parcel"),
    sum(candidates$successor_type == "current_condo_building")
  )
)

readr::write_csv(
  candidates,
  "../output/multicard_successor_building_candidates.csv"
)
readr::write_csv(
  project_summary,
  "../output/multicard_successor_building_project_summary.csv"
)
readr::write_csv(
  summary,
  "../output/multicard_successor_building_summary.csv"
)
