# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_street <- function(x) {
  out <- stringr::str_to_upper(dplyr::coalesce(as.character(x), ""))
  out <- stringr::str_replace_all(out, "\\bSTREET\\b", "ST")
  out <- stringr::str_replace_all(out, "\\bAVENUE\\b", "AVE")
  out <- stringr::str_replace_all(out, "\\bBOULEVARD\\b", "BLVD")
  out <- stringr::str_replace_all(out, "\\bROAD\\b", "RD")
  out <- stringr::str_replace_all(out, "\\bDRIVE\\b", "DR")
  out <- stringr::str_replace_all(out, "\\bPLACE\\b", "PL")
  out <- stringr::str_replace_all(out, "\\bCOURT\\b", "CT")
  out <- stringr::str_replace_all(out, "\\bPARKWAY\\b", "PKWY")
  out <- stringr::str_replace_all(out, "[^A-Z0-9 ]", " ")
  stringr::str_squish(out)
}

chains <- readr::read_csv(
  "../output/permit_first_unmatched_residential_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
permits <- readr::read_csv(
  "../output/permit_first_permit_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
represented_pins <- readr::read_csv(
  "../output/residential_candidate_pin_representation.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_pin = readr::col_character(),
    represented_project_ids = readr::col_character()
  )
)
footprints <- sf::st_read(
  "../output/permit_residual_city_building_footprints.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)
project_sites <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::select(
    represented_project_id = project_id,
    represented_project_year = target_year
  )

if (anyDuplicated(chains$permit_chain_id) ||
    anyDuplicated(footprints$footprint_id) ||
    anyDuplicated(project_sites$represented_project_id)) {
  stop("Permit chains, footprints, and project sites must be unique.", call. = FALSE)
}

chain_pin14 <- permits |>
  dplyr::filter(permit_chain_id %in% chains$permit_chain_id) |>
  dplyr::select(permit_chain_id, pin) |>
  tidyr::separate_rows(pin, sep = "\\s*\\|\\s*") |>
  dplyr::mutate(
    permit_pin14 = stringr::str_replace_all(dplyr::coalesce(pin, ""), "[^0-9]", "")
  ) |>
  dplyr::filter(stringr::str_length(permit_pin14) == 14L) |>
  dplyr::distinct(permit_chain_id, permit_pin14) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    permit_pin14s = paste(sort(unique(permit_pin14)), collapse = "/"),
    .groups = "drop"
  )

footprint_project_rows <- sf::st_is_within_distance(
  footprints,
  project_sites,
  dist = 25
)
footprint_project_links <- tibble::tibble(
  footprint_row = rep(seq_along(footprint_project_rows), lengths(footprint_project_rows)),
  project_row = unlist(footprint_project_rows, use.names = FALSE)
) |>
  dplyr::transmute(
    footprint_id = footprints$footprint_id[footprint_row],
    represented_project_id = project_sites$represented_project_id[project_row],
    represented_project_year = project_sites$represented_project_year[project_row]
  ) |>
  dplyr::filter(
    abs(represented_project_year -
      footprints$city_year_built[match(footprint_id, footprints$footprint_id)]) <= 2L
  ) |>
  dplyr::distinct(footprint_id, represented_project_id) |>
  dplyr::group_by(footprint_id) |>
  dplyr::summarise(
    spatially_represented_project_ids = paste(
      sort(unique(represented_project_id)),
      collapse = "/"
    ),
    .groups = "drop"
  )

footprints <- footprints |>
  dplyr::left_join(
    represented_pins |>
      dplyr::rename(
        harris_pin = candidate_pin,
        pin_represented_project_ids = represented_project_ids
      ),
    by = "harris_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    footprint_project_links,
    by = "footprint_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    represented_project_ids = purrr::map2_chr(
      pin_represented_project_ids,
      spatially_represented_project_ids,
      ~ paste(sort(unique(setdiff(c(
        stringr::str_split_1(dplyr::coalesce(.x, ""), "/"),
        stringr::str_split_1(dplyr::coalesce(.y, ""), "/")
      ), ""))), collapse = "/")
    ),
    represented_project_ids = dplyr::na_if(represented_project_ids, "")
  )

chain_points <- sf::st_as_sf(
  chains,
  coords = c("representative_x_3435", "representative_y_3435"),
  crs = 3435,
  remove = FALSE
)
nearby_footprint_rows <- sf::st_is_within_distance(
  chain_points,
  footprints,
  dist = 250
)
nearby_index <- tibble::tibble(
  chain_row = rep(seq_along(nearby_footprint_rows), lengths(nearby_footprint_rows)),
  footprint_row = unlist(nearby_footprint_rows, use.names = FALSE)
)

links <- nearby_index |>
  dplyr::mutate(
    permit_chain_id = chain_points$permit_chain_id[chain_row],
    footprint_id = footprints$footprint_id[footprint_row],
    footprint_distance_ft = as.numeric(sf::st_distance(
      chain_points[chain_row, ],
      footprints[footprint_row, ],
      by_element = TRUE
    ))
  ) |>
  dplyr::select(-chain_row, -footprint_row) |>
  dplyr::left_join(
    chains |>
      dplyr::transmute(
        permit_chain_id,
        representative_permit_number,
        representative_application_date,
        representative_issue_date,
        representative_address,
        representative_description,
        application_ward_pair,
        application_boundary_distance_ft,
        review_priority,
        permit_address_number = suppressWarnings(as.integer(
          stringr::str_extract(representative_address, "^[0-9]+")
        )),
        permit_street = normalize_street(stringr::str_remove(
          representative_address,
          "^[0-9]+\\s+"
        ))
      ),
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    footprints |>
      sf::st_drop_geometry() |>
      dplyr::transmute(
        footprint_id,
        bldg_statu,
        address_from,
        address_to,
        city_street = normalize_street(paste(
          street_direction,
          street_name,
          street_type
        )),
        city_address,
        harris_pin,
        city_year_built,
        city_units,
        no_stories,
        city_building_sqft,
        city_shape_area_sqft,
        represented_project_ids
      ),
    by = "footprint_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    chain_pin14,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    application_year = lubridate::year(representative_application_date),
    construction_year_gap = city_year_built - application_year,
    plausible_construction_year = dplyr::between(construction_year_gap, -1L, 6L),
    same_street = permit_street == city_street,
    address_range_match = same_street &
      is.finite(permit_address_number) &
      permit_address_number >= pmin(address_from, address_to, na.rm = TRUE) &
      permit_address_number <= pmax(address_from, address_to, na.rm = TRUE),
    exact_harris_pin = purrr::map2_lgl(
      harris_pin,
      dplyr::coalesce(permit_pin14s, ""),
      ~ .x != "" && .y != "" && .x %in% stringr::str_split_1(.y, "/")
    ),
    permit_point_inside_footprint = footprint_distance_ft == 0,
    strong_footprint_match = address_range_match |
      exact_harris_pin |
      permit_point_inside_footprint,
    represented_in_preferred_ledger = !is.na(represented_project_ids)
  ) |>
  dplyr::filter(plausible_construction_year) |>
  dplyr::arrange(
    permit_chain_id,
    dplyr::desc(strong_footprint_match),
    footprint_distance_ft,
    footprint_id
  )

chain_summary <- links |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    plausible_footprints = dplyr::n_distinct(footprint_id),
    strong_footprint_matches = dplyr::n_distinct(
      footprint_id[strong_footprint_match]
    ),
    strong_represented_footprints = dplyr::n_distinct(
      footprint_id[strong_footprint_match & represented_in_preferred_ledger]
    ),
    strong_unrepresented_footprints = dplyr::n_distinct(
      footprint_id[strong_footprint_match & !represented_in_preferred_ledger]
    ),
    nearest_footprint_distance_ft = min(footprint_distance_ft),
    nearest_unrepresented_footprint_distance_ft = suppressWarnings(min(
      footprint_distance_ft[!represented_in_preferred_ledger],
      na.rm = TRUE
    )),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    nearest_unrepresented_footprint_distance_ft = dplyr::if_else(
      is.infinite(nearest_unrepresented_footprint_distance_ft),
      NA_real_,
      nearest_unrepresented_footprint_distance_ft
    )
  )

chain_review <- chains |>
  dplyr::left_join(
    chain_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        plausible_footprints,
        strong_footprint_matches,
        strong_represented_footprints,
        strong_unrepresented_footprints
      ),
      ~ dplyr::coalesce(.x, 0L)
    ),
    footprint_review_status = dplyr::case_when(
      strong_unrepresented_footprints == 1L &
        strong_represented_footprints == 0L ~
        "one_strong_unrepresented_city_footprint",
      strong_unrepresented_footprints > 0L ~
        "manual_review_unrepresented_city_footprints",
      strong_represented_footprints > 0L ~
        "strong_city_footprint_already_represented",
      plausible_footprints == 1L &
        nearest_unrepresented_footprint_distance_ft <= 50 ~
        "one_unrepresented_city_footprint_within_50ft",
      plausible_footprints > 0L ~ "nearby_city_footprints_without_strong_match",
      TRUE ~ "no_plausible_2006_2015_city_footprint"
    )
  ) |>
  dplyr::arrange(
    factor(
      footprint_review_status,
      levels = c(
        "one_strong_unrepresented_city_footprint",
        "one_unrepresented_city_footprint_within_50ft",
        "manual_review_unrepresented_city_footprints",
        "strong_city_footprint_already_represented",
        "nearby_city_footprints_without_strong_match",
        "no_plausible_2006_2015_city_footprint"
      )
    ),
    application_boundary_distance_ft,
    permit_chain_id
  )

summary <- dplyr::bind_rows(
  tibble::tibble(metric = "residual_permit_chains", value = nrow(chains)),
  tibble::tibble(metric = "plausible_chain_footprint_links", value = nrow(links)),
  chain_review |>
    dplyr::count(footprint_review_status, name = "value") |>
    dplyr::transmute(metric = paste0("chain_status:", footprint_review_status), value)
)

readr::write_csv(
  links,
  "../output/permit_residual_city_building_footprint_links.csv",
  na = ""
)
readr::write_csv(
  chain_review,
  "../output/permit_residual_city_building_footprint_chain_review.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_residual_city_building_footprint_link_summary.csv",
  na = ""
)
