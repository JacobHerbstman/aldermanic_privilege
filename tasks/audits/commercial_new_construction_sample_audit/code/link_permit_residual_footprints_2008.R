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
  "../output/permit_residual_evidence_matrix.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(evidence_review_priority %in% c(
    "high_completion_candidate",
    "medium_completion_candidate"
  ))
permits <- readr::read_csv(
  "../output/permit_first_permit_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
footprints <- sf::st_read(
  "../output/cook_building_footprints_2006_2008.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)

if (anyDuplicated(chains$permit_chain_id) ||
    anyDuplicated(footprints$footprint_2008_id)) {
  stop("Permit chains and 2008 footprints must be unique.", call. = FALSE)
}

chain_pin14 <- permits |>
  dplyr::filter(permit_chain_id %in% chains$permit_chain_id) |>
  dplyr::select(permit_chain_id, pin) |>
  tidyr::separate_rows(pin, sep = "\\s*\\|\\s*") |>
  dplyr::mutate(
    permit_pin14 = stringr::str_replace_all(
      dplyr::coalesce(as.character(pin), ""),
      "[^0-9]",
      ""
    )
  ) |>
  dplyr::filter(stringr::str_length(permit_pin14) == 14L) |>
  dplyr::distinct(permit_chain_id, permit_pin14) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    permit_pin14s = paste(sort(unique(permit_pin14)), collapse = "/"),
    .groups = "drop"
  )

chain_points <- sf::st_as_sf(
  chains,
  coords = c("representative_x_3435", "representative_y_3435"),
  crs = 3435,
  remove = FALSE
)
nearby_rows <- sf::st_is_within_distance(chain_points, footprints, dist = 250)
nearby_index <- tibble::tibble(
  chain_row = rep(seq_along(nearby_rows), lengths(nearby_rows)),
  footprint_row = unlist(nearby_rows, use.names = FALSE)
)

links <- nearby_index |>
  dplyr::mutate(
    permit_chain_id = chain_points$permit_chain_id[chain_row],
    footprint_2008_id = footprints$footprint_2008_id[footprint_row],
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
        footprint_2008_id,
        building_id,
        building_status,
        address_from,
        address_to,
        footprint_street = normalize_street(paste(
          street_direction,
          street_name,
          street_type
        )),
        harris_pin,
        footprint_year_built = year_built,
        footprint_units = units,
        footprint_stories = stories,
        footprint_building_sqft = building_sqft,
        footprint_area_sqft = geometry_area_sqft
      ),
    by = "footprint_2008_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    chain_pin14,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    application_year = lubridate::year(representative_application_date),
    construction_year_gap = footprint_year_built - application_year,
    plausible_construction_year = dplyr::between(
      construction_year_gap,
      -1L,
      3L
    ),
    same_street = permit_street == footprint_street,
    address_range_match = same_street &
      is.finite(permit_address_number) &
      permit_address_number >= pmin(address_from, address_to, na.rm = TRUE) &
      permit_address_number <= pmax(address_from, address_to, na.rm = TRUE),
    exact_harris_pin = purrr::map2_lgl(
      harris_pin,
      dplyr::coalesce(permit_pin14s, ""),
      ~ stringr::str_length(.x) == 14L &&
        .x %in% stringr::str_split_1(.y, "/")
    ),
    permit_point_inside_footprint = footprint_distance_ft == 0,
    strong_footprint_match = plausible_construction_year &
      (address_range_match | exact_harris_pin | permit_point_inside_footprint)
  ) |>
  dplyr::filter(plausible_construction_year) |>
  dplyr::arrange(
    permit_chain_id,
    dplyr::desc(strong_footprint_match),
    footprint_distance_ft,
    footprint_2008_id
  )

chain_review <- links |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    plausible_2008_footprints = dplyr::n_distinct(footprint_2008_id),
    strong_2008_footprints = dplyr::n_distinct(
      footprint_2008_id[strong_footprint_match]
    ),
    strong_2008_footprint_ids = paste(
      sort(unique(footprint_2008_id[strong_footprint_match])),
      collapse = "/"
    ),
    strong_2008_harris_pins = paste(
      sort(unique(harris_pin[
        strong_footprint_match & stringr::str_length(harris_pin) == 14L
      ])),
      collapse = "/"
    ),
    nearest_2008_footprint_distance_ft = min(footprint_distance_ft),
    .groups = "drop"
  ) |>
  dplyr::right_join(
    chains |>
      dplyr::select(
        permit_chain_id,
        representative_permit_number,
        representative_application_date,
        representative_address,
        representative_description,
        application_ward_pair,
        application_boundary_distance_ft
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    plausible_2008_footprints = dplyr::coalesce(plausible_2008_footprints, 0L),
    strong_2008_footprints = dplyr::coalesce(strong_2008_footprints, 0L),
    footprint_2008_review_status = dplyr::case_when(
      strong_2008_footprints == 1L ~ "one_strong_2008_footprint",
      strong_2008_footprints > 1L ~ "multiple_strong_2008_footprints",
      plausible_2008_footprints > 0L ~ "nearby_2008_footprints_only",
      TRUE ~ "no_plausible_2008_footprint"
    )
  ) |>
  dplyr::arrange(
    dplyr::desc(strong_2008_footprints),
    permit_chain_id
  )

summary <- dplyr::bind_rows(
  chain_review |>
    dplyr::count(footprint_2008_review_status, name = "value") |>
    dplyr::transmute(
      section = "chain_status",
      metric = footprint_2008_review_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "high_completion_candidate_chains",
      "candidate_chains_with_strong_2008_footprint",
      "strong_chain_footprint_links",
      "strong_footprints_with_harris_pin"
    ),
    value = c(
      nrow(chains),
      sum(chain_review$strong_2008_footprints > 0),
      sum(links$strong_footprint_match),
      sum(
        links$strong_footprint_match &
          stringr::str_length(links$harris_pin) == 14L
      )
    )
  )
)

readr::write_csv(
  links,
  "../output/permit_residual_2008_footprint_links.csv"
)
readr::write_csv(
  chain_review,
  "../output/permit_residual_2008_footprint_chain_review.csv"
)
readr::write_csv(
  summary,
  "../output/permit_residual_2008_footprint_summary.csv"
)
