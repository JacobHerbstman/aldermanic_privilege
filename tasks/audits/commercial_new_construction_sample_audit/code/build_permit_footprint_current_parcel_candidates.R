# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin <- function(x) {
  digits <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
  dplyr::if_else(stringr::str_length(digits) == 14L, digits, NA_character_)
}

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

high_queue <- readr::read_csv(
  "../output/permit_residual_high_completion_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
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
) |>
  dplyr::filter(
    permit_chain_id %in% high_queue$permit_chain_id,
    strong_footprint_match,
    !represented_in_preferred_ledger
  ) |>
  dplyr::select(-represented_project_ids)
parcel_addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    prop_address_full = readr::col_character()
  ),
  col_select = c(pin, prop_address_full)
)
parcel_points <- readr::read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    centroid_x_crs_3435 = readr::col_double(),
    centroid_y_crs_3435 = readr::col_double()
  ),
  col_select = c(
    pin,
    class,
    centroid_x_crs_3435,
    centroid_y_crs_3435
  )
) |>
  dplyr::transmute(
    candidate_pin = normalize_pin(pin),
    current_parcel_class = class,
    current_x_3435 = centroid_x_crs_3435,
    current_y_3435 = centroid_y_crs_3435
  ) |>
  dplyr::filter(!is.na(candidate_pin)) |>
  dplyr::distinct(candidate_pin, .keep_all = TRUE)
represented_pins <- readr::read_csv(
  "../output/residential_candidate_pin_representation.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_pin = readr::col_character(),
    represented_project_ids = readr::col_character()
  )
) |>
  dplyr::transmute(
    candidate_pin = normalize_pin(candidate_pin),
    represented_project_ids
  )
footprints <- sf::st_read(
  "../output/permit_residual_city_building_footprints.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::select(footprint_id)

if (anyDuplicated(parcel_points$candidate_pin) ||
    anyDuplicated(represented_pins$candidate_pin) ||
    anyDuplicated(footprints$footprint_id)) {
  stop("Current parcel and footprint keys must be unique.", call. = FALSE)
}

parcel_address_rows <- parcel_addresses |>
  dplyr::transmute(
    candidate_pin = normalize_pin(pin),
    current_property_address = stringr::str_squish(prop_address_full),
    current_address_number = suppressWarnings(as.integer(
      stringr::str_extract(prop_address_full, "^[0-9]+")
    )),
    current_street = normalize_street(stringr::str_remove(
      prop_address_full,
      "^[0-9]+\\s+"
    ))
  ) |>
  dplyr::filter(
    !is.na(candidate_pin),
    is.finite(current_address_number),
    current_street != ""
  ) |>
  dplyr::distinct(
    candidate_pin,
    current_property_address,
    current_address_number,
    current_street
  ) |>
  dplyr::arrange(current_street, current_address_number, candidate_pin) |>
  dplyr::group_by(current_street) |>
  dplyr::summarise(
    street_addresses = list(dplyr::pick(dplyr::everything())),
    .groups = "drop"
  )

footprint_address_candidates <- footprint_links |>
  dplyr::filter(city_street != "") |>
  dplyr::left_join(
    parcel_address_rows,
    by = c("city_street" = "current_street"),
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(street_addresses, keep_empty = TRUE) |>
  dplyr::filter(
    !is.na(candidate_pin),
    current_address_number >= pmin(address_from, address_to, na.rm = TRUE),
    current_address_number <= pmax(address_from, address_to, na.rm = TRUE)
  ) |>
  dplyr::left_join(
    parcel_points,
    by = "candidate_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::filter(is.finite(current_x_3435), is.finite(current_y_3435))

candidate_points <- sf::st_as_sf(
  footprint_address_candidates,
  coords = c("current_x_3435", "current_y_3435"),
  crs = 3435,
  remove = FALSE
)
footprint_rows <- match(candidate_points$footprint_id, footprints$footprint_id)
if (anyNA(footprint_rows)) {
  stop("A footprint address candidate lacks source geometry.", call. = FALSE)
}
candidate_points$current_centroid_to_footprint_ft <- as.numeric(sf::st_distance(
  candidate_points,
  footprints[footprint_rows, ],
  by_element = TRUE
))

current_parcel_candidates <- candidate_points |>
  sf::st_drop_geometry() |>
  dplyr::filter(current_centroid_to_footprint_ft <= 250) |>
  dplyr::left_join(
    represented_pins,
    by = "candidate_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    represented_in_preferred_ledger = !is.na(represented_project_ids),
    current_condo_class = stringr::str_starts(current_parcel_class, "299")
  ) |>
  dplyr::arrange(
    permit_chain_id,
    footprint_id,
    current_centroid_to_footprint_ft,
    candidate_pin
  ) |>
  dplyr::distinct(permit_chain_id, footprint_id, candidate_pin, .keep_all = TRUE)

chain_summary <- current_parcel_candidates |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    strong_unrepresented_footprints = dplyr::n_distinct(footprint_id),
    footprint_address_current_pins = dplyr::n_distinct(candidate_pin),
    footprint_address_current_pin10s = dplyr::n_distinct(
      stringr::str_sub(candidate_pin, 1, 10)
    ),
    footprint_address_noncondo_pins = dplyr::n_distinct(
      candidate_pin[!current_condo_class]
    ),
    footprint_address_condo_pins = dplyr::n_distinct(
      candidate_pin[current_condo_class]
    ),
    footprint_address_represented_pins = dplyr::n_distinct(
      candidate_pin[represented_in_preferred_ledger]
    ),
    footprint_address_unrepresented_pins = dplyr::n_distinct(
      candidate_pin[!represented_in_preferred_ledger]
    ),
    nearest_current_parcel_centroid_ft = min(current_centroid_to_footprint_ft),
    current_candidate_pins = paste(sort(unique(candidate_pin)), collapse = "/"),
    .groups = "drop"
  )

chain_review <- high_queue |>
  dplyr::left_join(
    chain_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        footprint_address_current_pins,
        footprint_address_current_pin10s,
        footprint_address_noncondo_pins,
        footprint_address_condo_pins,
        footprint_address_represented_pins,
        footprint_address_unrepresented_pins
      ),
      ~ dplyr::coalesce(.x, 0L)
    ),
    footprint_current_parcel_status = dplyr::case_when(
      footprint_address_represented_pins > 0L ~
        "footprint_range_contains_represented_current_pin",
      footprint_address_noncondo_pins == 1L &
        footprint_address_condo_pins == 0L ~
        "one_unrepresented_noncondo_current_pin",
      footprint_address_noncondo_pins > 0L ~
        "multiple_unrepresented_noncondo_current_pins",
      footprint_address_condo_pins > 0L ~
        "current_condo_pins_only",
      TRUE ~ "no_current_pin_in_footprint_address_range"
    )
  ) |>
  dplyr::arrange(
    factor(
      footprint_current_parcel_status,
      levels = c(
        "one_unrepresented_noncondo_current_pin",
        "multiple_unrepresented_noncondo_current_pins",
        "current_condo_pins_only",
        "footprint_range_contains_represented_current_pin",
        "no_current_pin_in_footprint_address_range"
      )
    ),
    application_boundary_distance_ft,
    representative_application_date,
    permit_chain_id
  )

summary <- dplyr::bind_rows(
  tibble::tibble(
    metric = "high_completion_candidate_chains",
    value = nrow(high_queue)
  ),
  tibble::tibble(
    metric = "strong_footprint_chain_links",
    value = dplyr::n_distinct(footprint_links$permit_chain_id)
  ),
  tibble::tibble(
    metric = "footprint_range_current_parcel_links",
    value = nrow(current_parcel_candidates)
  ),
  chain_review |>
    dplyr::count(footprint_current_parcel_status, name = "value") |>
    dplyr::transmute(
      metric = paste0("chain_status:", footprint_current_parcel_status),
      value
    )
)

readr::write_csv(
  current_parcel_candidates,
  "../output/permit_footprint_current_parcel_candidates.csv",
  na = ""
)
readr::write_csv(
  chain_review,
  "../output/permit_footprint_current_parcel_chain_review.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_footprint_current_parcel_summary.csv",
  na = ""
)
