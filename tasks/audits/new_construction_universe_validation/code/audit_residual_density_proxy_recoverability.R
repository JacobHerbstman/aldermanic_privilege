# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

parse_stories <- function(description) {
  description <- stringr::str_to_upper(dplyr::coalesce(description, ""))

  numeric_story <- suppressWarnings(as.numeric(
    stringr::str_match(
      description,
      "([0-9]{1,2})(?:ST|ND|RD|TH)?[[:space:]\\-\\)]*(?:STOR(?:Y|IES)|STOREY|STY)"
    )[, 2]
  ))

  word_story <- dplyr::case_when(
    stringr::str_detect(description, "\\bONE[ -](?:STOR(?:Y|IES)|STOREY)") ~ 1,
    stringr::str_detect(description, "\\bTWO[ -](?:STOR(?:Y|IES)|STOREY)") ~ 2,
    stringr::str_detect(description, "\\bTHREE[ -](?:STOR(?:Y|IES)|STOREY)") ~ 3,
    stringr::str_detect(description, "\\bFOUR[ -](?:STOR(?:Y|IES)|STOREY)") ~ 4,
    stringr::str_detect(description, "\\bFIVE[ -](?:STOR(?:Y|IES)|STOREY)") ~ 5,
    stringr::str_detect(description, "\\bSIX[ -](?:STOR(?:Y|IES)|STOREY)") ~ 6,
    stringr::str_detect(description, "\\bSEVEN[ -](?:STOR(?:Y|IES)|STOREY)") ~ 7,
    stringr::str_detect(description, "\\bEIGHT[ -](?:STOR(?:Y|IES)|STOREY)") ~ 8,
    stringr::str_detect(description, "\\bNINE[ -](?:STOR(?:Y|IES)|STOREY)") ~ 9,
    stringr::str_detect(description, "\\bTEN[ -](?:STOR(?:Y|IES)|STOREY)") ~ 10,
    stringr::str_detect(description, "\\bELEVEN[ -](?:STOR(?:Y|IES)|STOREY)") ~ 11,
    stringr::str_detect(description, "\\bTWELVE[ -](?:STOR(?:Y|IES)|STOREY)") ~ 12,
    stringr::str_detect(description, "\\bTHIRTEEN[ -](?:STOR(?:Y|IES)|STOREY)") ~ 13,
    stringr::str_detect(description, "\\bFOURTEEN[ -](?:STOR(?:Y|IES)|STOREY)") ~ 14,
    stringr::str_detect(description, "\\bFIFTEEN[ -](?:STOR(?:Y|IES)|STOREY)") ~ 15,
    stringr::str_detect(description, "\\bSIXTEEN[ -](?:STOR(?:Y|IES)|STOREY)") ~ 16,
    stringr::str_detect(description, "\\bSEVENTEEN[ -](?:STOR(?:Y|IES)|STOREY)") ~ 17,
    stringr::str_detect(description, "\\bEIGHTEEN[ -](?:STOR(?:Y|IES)|STOREY)") ~ 18,
    stringr::str_detect(description, "\\bNINETEEN[ -](?:STOR(?:Y|IES)|STOREY)") ~ 19,
    stringr::str_detect(description, "\\bTWENTY[ -](?:STOR(?:Y|IES)|STOREY)") ~ 20,
    TRUE ~ NA_real_
  )

  dplyr::coalesce(numeric_story, word_story)
}

attrition <- readr::read_csv(
  "../output/residual_completed_density_attrition.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

assessor_candidates <- readr::read_csv(
  "../input/residual_permit_footprint_2022_assessor_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    .default = readr::col_guess()
  )
)

field_transitions <- readr::read_csv(
  "../input/residual_footprint_field_transitions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    .default = readr::col_guess()
  )
)

footprint_links <- readr::read_csv(
  "../input/permit_residual_city_building_footprint_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_id = readr::col_character(),
    harris_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(strong_footprint_match) |>
  dplyr::distinct(
    permit_chain_id,
    footprint_id,
    city_shape_area_sqft,
    city_units,
    no_stories
  )

if (anyDuplicated(footprint_links[c("permit_chain_id", "footprint_id")])) {
  stop("A permit chain-footprint pair is not unique.")
}

main_footprints <- footprint_links |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::arrange(
    dplyr::desc(city_shape_area_sqft),
    footprint_id,
    .by_group = TRUE
  ) |>
  dplyr::mutate(strong_footprint_count = dplyr::n()) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup() |>
  dplyr::rename(
    main_footprint_id = footprint_id,
    main_footprint_sqft = city_shape_area_sqft,
    main_footprint_city_units = city_units,
    main_footprint_city_stories = no_stories
  )

parcel_matches <- readr::read_csv(
  "../input/residual_permit_footprint_2022_parcel_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    footprint_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::distinct(footprint_id, pin14_2022)

if (anyDuplicated(parcel_matches[c("footprint_id", "pin14_2022")])) {
  stop("A footprint-parcel pair is not unique.")
}

parcels <- sf::st_read(
  "../input/residual_permit_footprint_2022_parcels.gpkg",
  quiet = TRUE
)

if (
  sf::st_crs(parcels)$epsg != 3435 ||
    anyDuplicated(parcels$pin14_2022)
) {
  stop("The 2022 parcel file must contain one EPSG:3435 polygon per PIN.")
}

parcel_areas <- parcels |>
  dplyr::transmute(
    pin14_2022,
    parcel_area_sqft = as.numeric(sf::st_area(geom))
  ) |>
  sf::st_drop_geometry()

footprint_parcel_areas <- parcel_matches |>
  dplyr::left_join(
    parcel_areas,
    by = "pin14_2022",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(footprint_id) |>
  dplyr::summarise(
    containing_parcel_count = dplyr::n_distinct(
      pin14_2022[!is.na(pin14_2022)]
    ),
    proxy_land_sqft = sum(parcel_area_sqft, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    proxy_land_sqft = dplyr::if_else(
      containing_parcel_count > 0,
      proxy_land_sqft,
      NA_real_
    )
  )

main_footprint_parcels <- main_footprints |>
  dplyr::select(permit_chain_id, main_footprint_id) |>
  dplyr::left_join(
    footprint_parcel_areas,
    by = c("main_footprint_id" = "footprint_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::select(
    permit_chain_id,
    containing_parcel_count,
    proxy_land_sqft
  )

proxy_components <- dplyr::bind_rows(
  attrition |>
    dplyr::select(
      permit_chain_id,
      representative_description,
      maximum_parsed_unit_mention,
      city_unit_max
    ),
  assessor_candidates |>
    dplyr::anti_join(
      attrition |>
        dplyr::select(permit_chain_id),
      by = "permit_chain_id"
    ) |>
    dplyr::transmute(
      permit_chain_id,
      representative_description,
      maximum_parsed_unit_mention = NA_real_,
      city_unit_max = city_units
    )
) |>
  dplyr::distinct(permit_chain_id, .keep_all = TRUE) |>
  dplyr::mutate(
    permit_stories = parse_stories(representative_description),
    permit_units = dplyr::if_else(
      maximum_parsed_unit_mention > 0,
      maximum_parsed_unit_mention,
      NA_real_
    ),
    city_units = dplyr::if_else(city_unit_max > 0, city_unit_max, NA_real_),
    proxy_dwelling_units = dplyr::coalesce(permit_units, city_units),
    unit_source = dplyr::case_when(
      !is.na(permit_units) ~ "permit_text",
      !is.na(city_units) ~ "city_footprint",
      TRUE ~ NA_character_
    ),
    unit_conflict = !is.na(permit_units) &
      !is.na(city_units) &
      permit_units != city_units
  ) |>
  dplyr::left_join(
    main_footprints,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    main_footprint_parcels,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    proxy_building_sqft = main_footprint_sqft * permit_stories,
    proxy_far = proxy_building_sqft / proxy_land_sqft,
    proxy_dupac = proxy_dwelling_units / proxy_land_sqft * 43560,
    proxy_complete = !is.na(proxy_dwelling_units) &
      !is.na(proxy_building_sqft) &
      !is.na(proxy_land_sqft) &
      proxy_dwelling_units > 0 &
      proxy_building_sqft > 0 &
      proxy_land_sqft > 0,
    simple_single_footprint_proxy = proxy_complete &
      strong_footprint_count == 1 &
      containing_parcel_count == 1 &
      !unit_conflict
  )

recoverability <- attrition |>
  dplyr::left_join(
    proxy_components |>
      dplyr::select(
        permit_chain_id,
        permit_stories,
        permit_units,
        city_units,
        proxy_dwelling_units,
        unit_source,
        unit_conflict,
        main_footprint_id,
        main_footprint_sqft,
        strong_footprint_count,
        containing_parcel_count,
        proxy_building_sqft,
        proxy_land_sqft,
        proxy_far,
        proxy_dupac,
        proxy_complete,
        simple_single_footprint_proxy
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    proxy_complete = dplyr::coalesce(proxy_complete, FALSE),
    simple_single_footprint_proxy = dplyr::coalesce(
      simple_single_footprint_proxy,
      FALSE
    ),
    proxy_class = dplyr::case_when(
      strict_assessor_recovery ~ "comparable_assessor_fields",
      simple_single_footprint_proxy ~ "simple_proxy_only",
      proxy_complete ~ "complex_proxy_only",
      TRUE ~ "insufficient_proxy_fields"
    )
  )

calibration <- field_transitions |>
  dplyr::filter(final_decision == "include") |>
  dplyr::select(permit_chain_id) |>
  dplyr::inner_join(
    assessor_candidates |>
      dplyr::select(
        permit_chain_id,
        representative_address,
        candidate_dwelling_units,
        candidate_building_sqft,
        candidate_land_sqft
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    proxy_components |>
      dplyr::select(
        permit_chain_id,
        permit_stories,
        proxy_dwelling_units,
        main_footprint_sqft,
        strong_footprint_count,
        containing_parcel_count,
        proxy_building_sqft,
        proxy_land_sqft,
        proxy_far,
        proxy_dupac,
        proxy_complete,
        simple_single_footprint_proxy
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    assessor_far = candidate_building_sqft / candidate_land_sqft,
    assessor_dupac = candidate_dwelling_units / candidate_land_sqft * 43560,
    building_area_ratio = proxy_building_sqft / candidate_building_sqft,
    land_area_ratio = proxy_land_sqft / candidate_land_sqft,
    far_ratio = proxy_far / assessor_far,
    dupac_ratio = proxy_dupac / assessor_dupac,
    units_agree = proxy_dwelling_units == candidate_dwelling_units
  )

project_evidence <- readr::read_csv(
  "../output/project_evidence_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    construction_year >= 2006,
    construction_year <= 2015,
    !is.na(dwelling_units),
    dwelling_units > 0,
    !is.na(building_sqft),
    building_sqft > 0,
    !is.na(land_sqft),
    land_sqft > 0
  ) |>
  dplyr::mutate(
    permit_description = dplyr::coalesce(
      exact_pin_issued_new_building_descriptions,
      exact_permit_descriptions,
      strong_spatial_permit_descriptions
    ),
    permit_stories = parse_stories(permit_description)
  )

project_polygons <- sf::st_read(
  "../input/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  dplyr::semi_join(
    project_evidence |>
      dplyr::select(project_id),
    by = "project_id"
  ) |>
  dplyr::arrange(project_id)

if (
  sf::st_crs(project_polygons)$epsg != 3435 ||
    anyDuplicated(project_polygons$project_id)
) {
  stop("Project geometry must contain one EPSG:3435 polygon per project.")
}

archived_footprints <- sf::st_read(
  "/vsizip/../input/chicago_building_footprints_2015.zip/buildings.shp",
  query = paste(
    "SELECT BLDG_ID, BLDG_STATU, YEAR_BUILT FROM buildings",
    "WHERE YEAR_BUILT >= 2006 AND YEAR_BUILT <= 2015",
    "AND BLDG_STATU = 'ACTIVE'"
  ),
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::mutate(
    footprint_row = dplyr::row_number(),
    city_year_built = suppressWarnings(as.integer(YEAR_BUILT)),
    footprint_sqft = as.numeric(sf::st_area(geometry))
  )

footprint_points <- sf::st_point_on_surface(archived_footprints)
project_footprint_index <- sf::st_intersects(
  project_polygons,
  footprint_points
)

project_footprint_candidates <- tibble::tibble(
  project_row = rep(
    seq_along(project_footprint_index),
    lengths(project_footprint_index)
  ),
  footprint_row = unlist(project_footprint_index, use.names = FALSE)
) |>
  dplyr::left_join(
    project_polygons |>
      sf::st_drop_geometry() |>
      dplyr::mutate(project_row = dplyr::row_number()) |>
      dplyr::select(project_row, project_id),
    by = "project_row",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    archived_footprints |>
      sf::st_drop_geometry() |>
      dplyr::select(
        footprint_row,
        BLDG_ID,
        city_year_built,
        footprint_sqft
      ),
    by = "footprint_row",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    project_evidence |>
      dplyr::select(project_id, construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    footprint_year_gap = abs(city_year_built - construction_year)
  ) |>
  dplyr::filter(footprint_year_gap <= 1)

broad_footprint_matches <- project_footprint_candidates |>
  dplyr::group_by(project_id) |>
  dplyr::arrange(
    dplyr::desc(footprint_sqft),
    footprint_year_gap,
    BLDG_ID,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    matched_footprint_count = dplyr::n()
  ) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup() |>
  dplyr::select(
    project_id,
    matched_footprint_count,
    city_year_built,
    footprint_year_gap,
    footprint_sqft
  )

broad_calibration <- project_evidence |>
  dplyr::select(
    project_id,
    source_family,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    permit_unit_max,
    permit_stories
  ) |>
  dplyr::left_join(
    project_polygons |>
      dplyr::transmute(
        project_id,
        project_polygon_sqft = as.numeric(sf::st_area(geom))
      ) |>
      sf::st_drop_geometry(),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    broad_footprint_matches,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    proxy_dwelling_units = dplyr::if_else(
      permit_unit_max > 0,
      permit_unit_max,
      NA_real_
    ),
    proxy_building_sqft = footprint_sqft * permit_stories,
    proxy_land_sqft = project_polygon_sqft,
    assessor_far = building_sqft / land_sqft,
    assessor_dupac = dwelling_units / land_sqft * 43560,
    proxy_far = proxy_building_sqft / proxy_land_sqft,
    proxy_dupac = proxy_dwelling_units / proxy_land_sqft * 43560,
    far_proxy_complete = !is.na(proxy_building_sqft) &
      !is.na(proxy_land_sqft) &
      proxy_building_sqft > 0 &
      proxy_land_sqft > 0,
    dupac_proxy_complete = !is.na(proxy_dwelling_units) &
      !is.na(proxy_land_sqft) &
      proxy_dwelling_units > 0 &
      proxy_land_sqft > 0,
    proxy_complete = far_proxy_complete & dupac_proxy_complete,
    units_agree = proxy_dwelling_units == dwelling_units,
    building_area_ratio = proxy_building_sqft / building_sqft,
    land_area_ratio = proxy_land_sqft / land_sqft,
    far_ratio = proxy_far / assessor_far,
    dupac_ratio = proxy_dupac / assessor_dupac,
    multifamily = dwelling_units > 1
  )

calibration_metrics <- calibration |>
  dplyr::filter(proxy_complete) |>
  dplyr::summarise(
    calibration_projects = dplyr::n(),
    multifamily_calibration_projects = sum(candidate_dwelling_units > 1),
    unit_agreement_share = mean(units_agree),
    median_building_area_ratio = median(building_area_ratio),
    building_area_ratio_p25 = quantile(building_area_ratio, 0.25),
    building_area_ratio_p75 = quantile(building_area_ratio, 0.75),
    median_land_area_ratio = median(land_area_ratio),
    land_area_ratio_p25 = quantile(land_area_ratio, 0.25),
    land_area_ratio_p75 = quantile(land_area_ratio, 0.75),
    median_far_ratio = median(far_ratio),
    far_ratio_p25 = quantile(far_ratio, 0.25),
    far_ratio_p75 = quantile(far_ratio, 0.75),
    median_dupac_ratio = median(dupac_ratio),
    dupac_ratio_p25 = quantile(dupac_ratio, 0.25),
    dupac_ratio_p75 = quantile(dupac_ratio, 0.75),
    far_log_correlation = stats::cor(log(proxy_far), log(assessor_far)),
    dupac_log_correlation = stats::cor(log(proxy_dupac), log(assessor_dupac))
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "statistic",
    values_to = "value"
  )

recoverability_metrics <- dplyr::bind_rows(
  recoverability |>
    dplyr::summarise(
      statistic = "all_omitted_permit_chains",
      value = dplyr::n()
    ),
  recoverability |>
    dplyr::summarise(
      statistic = "all_proxy_complete",
      value = sum(proxy_complete)
    ),
  recoverability |>
    dplyr::summarise(
      statistic = "all_simple_single_footprint_proxy",
      value = sum(simple_single_footprint_proxy)
    ),
  recoverability |>
    dplyr::filter(inside_500ft) |>
    dplyr::summarise(
      statistic = "within_500ft_omitted_permit_chains",
      value = dplyr::n()
    ),
  recoverability |>
    dplyr::filter(inside_500ft) |>
    dplyr::summarise(
      statistic = "within_500ft_proxy_complete",
      value = sum(proxy_complete)
    ),
  recoverability |>
    dplyr::filter(inside_500ft) |>
    dplyr::summarise(
      statistic = "within_500ft_simple_single_footprint_proxy",
      value = sum(simple_single_footprint_proxy)
    ),
  recoverability |>
    dplyr::filter(inside_500ft, possible_multifamily) |>
    dplyr::summarise(
      statistic = "multifamily_within_500ft_omitted_permit_chains",
      value = dplyr::n()
    ),
  recoverability |>
    dplyr::filter(inside_500ft, possible_multifamily) |>
    dplyr::summarise(
      statistic = "multifamily_within_500ft_proxy_complete",
      value = sum(proxy_complete)
    ),
  recoverability |>
    dplyr::filter(inside_500ft, possible_multifamily) |>
    dplyr::summarise(
      statistic = "multifamily_within_500ft_simple_single_footprint_proxy",
      value = sum(simple_single_footprint_proxy)
    )
  )

broad_calibration_metrics <- dplyr::bind_rows(
  broad_calibration |>
    dplyr::filter(far_proxy_complete, matched_footprint_count <= 2) |>
    dplyr::summarise(
      statistic = "broad_far_calibration_projects",
      value = dplyr::n()
    ),
  broad_calibration |>
    dplyr::filter(
      far_proxy_complete,
      multifamily,
      matched_footprint_count <= 2
    ) |>
    dplyr::summarise(
      statistic = "broad_multifamily_far_calibration_projects",
      value = dplyr::n()
    ),
  broad_calibration |>
    dplyr::filter(dupac_proxy_complete) |>
    dplyr::summarise(
      statistic = "broad_dupac_calibration_projects",
      value = dplyr::n()
    ),
  broad_calibration |>
    dplyr::filter(dupac_proxy_complete, multifamily) |>
    dplyr::summarise(
      statistic = "broad_multifamily_dupac_calibration_projects",
      value = dplyr::n()
    ),
  broad_calibration |>
    dplyr::filter(dupac_proxy_complete) |>
    dplyr::summarise(
      statistic = "broad_unit_agreement_share",
      value = mean(units_agree)
    ),
  broad_calibration |>
    dplyr::filter(dupac_proxy_complete, multifamily) |>
    dplyr::summarise(
      statistic = "broad_multifamily_unit_agreement_share",
      value = mean(units_agree)
    ),
  broad_calibration |>
    dplyr::filter(far_proxy_complete, matched_footprint_count <= 2) |>
    dplyr::summarise(
      statistic = "broad_median_building_area_ratio",
      value = median(building_area_ratio)
    ),
  broad_calibration |>
    dplyr::filter(
      far_proxy_complete,
      multifamily,
      matched_footprint_count <= 2
    ) |>
    dplyr::summarise(
      statistic = "broad_multifamily_median_building_area_ratio",
      value = median(building_area_ratio)
    ),
  broad_calibration |>
    dplyr::filter(far_proxy_complete, matched_footprint_count <= 2) |>
    dplyr::summarise(
      statistic = "broad_building_area_ratio_p25",
      value = quantile(building_area_ratio, 0.25)
    ),
  broad_calibration |>
    dplyr::filter(far_proxy_complete, matched_footprint_count <= 2) |>
    dplyr::summarise(
      statistic = "broad_building_area_ratio_p75",
      value = quantile(building_area_ratio, 0.75)
    ),
  broad_calibration |>
    dplyr::filter(
      far_proxy_complete,
      multifamily,
      matched_footprint_count <= 2
    ) |>
    dplyr::summarise(
      statistic = "broad_multifamily_building_area_ratio_p25",
      value = quantile(building_area_ratio, 0.25)
    ),
  broad_calibration |>
    dplyr::filter(
      far_proxy_complete,
      multifamily,
      matched_footprint_count <= 2
    ) |>
    dplyr::summarise(
      statistic = "broad_multifamily_building_area_ratio_p75",
      value = quantile(building_area_ratio, 0.75)
    ),
  broad_calibration |>
    dplyr::filter(
      far_proxy_complete,
      multifamily,
      matched_footprint_count <= 2
    ) |>
    dplyr::summarise(
      statistic = "broad_multifamily_far_within_20_percent",
      value = mean(far_ratio >= 0.8 & far_ratio <= 1.2)
    ),
  broad_calibration |>
    dplyr::filter(far_proxy_complete, matched_footprint_count <= 2) |>
    dplyr::summarise(
      statistic = "broad_far_log_correlation",
      value = stats::cor(log(proxy_far), log(assessor_far))
    ),
  broad_calibration |>
    dplyr::filter(
      far_proxy_complete,
      multifamily,
      matched_footprint_count <= 2
    ) |>
    dplyr::summarise(
      statistic = "broad_multifamily_far_log_correlation",
      value = stats::cor(log(proxy_far), log(assessor_far))
    ),
  broad_calibration |>
    dplyr::filter(dupac_proxy_complete) |>
    dplyr::summarise(
      statistic = "broad_dupac_log_correlation",
      value = stats::cor(log(proxy_dupac), log(assessor_dupac))
    ),
  broad_calibration |>
    dplyr::filter(dupac_proxy_complete, multifamily) |>
    dplyr::summarise(
      statistic = "broad_multifamily_units_within_one",
      value = mean(abs(proxy_dwelling_units - dwelling_units) <= 1)
    ),
  broad_calibration |>
    dplyr::filter(dupac_proxy_complete, multifamily) |>
    dplyr::summarise(
      statistic = "broad_multifamily_dupac_log_correlation",
      value = stats::cor(log(proxy_dupac), log(assessor_dupac))
    )
)

readr::write_csv(
  recoverability,
  "../output/residual_density_proxy_recoverability.csv",
  na = ""
)
readr::write_csv(
  calibration,
  "../output/residual_density_proxy_calibration.csv",
  na = ""
)
readr::write_csv(
  broad_calibration,
  "../output/residual_density_proxy_broad_calibration.csv",
  na = ""
)
readr::write_csv(
  dplyr::bind_rows(
    recoverability_metrics,
    calibration_metrics,
    broad_calibration_metrics
  ),
  "../output/residual_density_proxy_summary.csv",
  na = ""
)
