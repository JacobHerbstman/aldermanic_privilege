# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

extract_unit_count <- function(description) {
  unit_matches <- stringr::str_match_all(
    stringr::str_to_upper(dplyr::coalesce(description, "")),
    "\\b([0-9]{1,4})\\s*(?:TOTAL\\s+)?(?:DWELLING\\s+|RESIDENTIAL\\s+|APARTMENT\\s+|EFFICIENCY\\s+)?UNITS?\\b"
  )[[1]]
  du_matches <- stringr::str_match_all(
    stringr::str_to_upper(dplyr::coalesce(description, "")),
    "\\b([0-9]{1,4})\\s*D\\.?U\\.?\\b"
  )[[1]]
  counts <- suppressWarnings(as.numeric(c(unit_matches[, 2], du_matches[, 2])))
  if (length(counts) == 0 || all(is.na(counts))) NA_real_ else max(counts, na.rm = TRUE)
}

class_297_data <- readr::read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(class == "297") %>%
  transmute(
    pin,
    assessor_yearbuilt = as.integer(year_built),
    assessor_units = as.numeric(num_apartments),
    assessor_building_sqft = as.numeric(building_sqft),
    assessor_land_sqft = as.numeric(land_sqft),
    assessor_building_use = single_v_multi_family,
    assessor_residence_type = type_of_residence
  ) %>%
  inner_join(
    readr::read_csv(
      "../input/parcels_with_ward_distances.csv",
      show_col_types = FALSE,
      col_types = readr::cols(pin = readr::col_character(), segment_id = readr::col_character(), .default = readr::col_guess())
    ) %>%
      filter(
        arealotsf > 1,
        areabuilding > 1,
        unitscount > 0,
        between(construction_year, 2006, 2022),
        dist_to_boundary_m <= 152.4,
        !is.na(ward_pair),
        is.finite(signed_distance_m),
        !is.na(construction_zone_group),
        !is.na(segment_id),
        segment_id != "",
        is.finite(strictness_own),
        is.finite(strictness_neighbor),
        if_all(
          c(
            share_white_own, share_black_own, median_hh_income_own,
            share_bach_plus_own, homeownership_rate_own
          ),
          is.finite
        )
      ) %>%
      select(pin, construction_year, production_units = unitscount),
    by = "pin",
    relationship = "one-to-one"
  )

class_297 <- sf::st_read("../input/geocoded_residential_data.gpkg", quiet = TRUE) %>%
  select(pin) %>%
  inner_join(class_297_data, by = "pin", relationship = "one-to-one") %>%
  sf::st_transform(3435)

permits <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    permit_issued == 1,
    permit_status == "COMPLETE",
    !is.na(application_start_date),
    !is.na(work_description)
  ) %>%
  mutate(
    permit_application_year = lubridate::year(as.Date(application_start_date)),
    permit_units = purrr::map_dbl(work_description, extract_unit_count)
  ) %>%
  filter(!is.na(permit_units)) %>%
  sf::st_transform(3435) %>%
  select(
    permit_id = id,
    permit_application_year,
    permit_units,
    street_number,
    street_direction,
    street_name,
    work_description
  )

candidate_index <- sf::st_is_within_distance(class_297, permits, dist = 200)

candidates <- purrr::map2_dfr(
  seq_len(nrow(class_297)),
  candidate_index,
  function(parcel_row, permit_rows) {
    if (length(permit_rows) == 0) return(tibble::tibble())
    distances <- as.numeric(sf::st_distance(
      class_297[rep(parcel_row, length(permit_rows)), ],
      permits[permit_rows, ],
      by_element = TRUE
    ))
    bind_cols(
      sf::st_drop_geometry(class_297[rep(parcel_row, length(permit_rows)), ]),
      sf::st_drop_geometry(permits[permit_rows, ]),
      tibble::tibble(distance_ft = distances)
    )
  }
) %>%
  filter(between(construction_year - permit_application_year, -1, 5)) %>%
  mutate(
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    year_gap = construction_year - permit_application_year
  ) %>%
  arrange(pin, distance_ft, abs(year_gap), permit_id)

nearest_candidates <- candidates %>%
  group_by(pin) %>%
  filter(distance_ft <= min(distance_ft) + 25) %>%
  ungroup()

permit_summary <- nearest_candidates %>%
  group_by(pin) %>%
  summarise(
    nearby_unit_permits = n_distinct(permit_id),
    nearest_permit_distance_ft = min(distance_ft),
    permit_unit_counts = paste(sort(unique(permit_units)), collapse = "/"),
    unambiguous_permit_units = if_else(n_distinct(permit_units) == 1, first(permit_units), NA_real_),
    permit_addresses = paste(sort(unique(permit_address)), collapse = " / "),
    permit_ids = paste(sort(unique(permit_id)), collapse = "/"),
    .groups = "drop"
  )

review <- class_297 %>%
  sf::st_drop_geometry() %>%
  left_join(permit_summary, by = "pin", relationship = "one-to-one") %>%
  mutate(
    permit_match = !is.na(unambiguous_permit_units),
    units_disagree = permit_match & production_units != unambiguous_permit_units
  )

readr::write_csv(
  tibble::tribble(
    ~metric, ~value,
    "class_297_rows_in_main_model", nrow(review),
    "class_297_rows_with_nearby_unambiguous_unit_permit", sum(review$permit_match),
    "class_297_rows_where_production_units_disagree_with_permit", sum(review$units_disagree)
  ),
  "../output/residential_class297_permit_summary.csv"
)
readr::write_csv(review %>% arrange(desc(units_disagree), pin), "../output/residential_class297_permit_review.csv")
readr::write_csv(candidates, "../output/residential_class297_permit_candidates.csv")
