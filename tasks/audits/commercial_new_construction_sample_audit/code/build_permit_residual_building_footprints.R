# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

chains <- readr::read_csv(
  "../output/permit_first_unmatched_residential_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(chains$permit_chain_id)) {
  stop("Residual permit chains must be unique.", call. = FALSE)
}

chain_points <- sf::st_as_sf(
  chains,
  coords = c("representative_x_3435", "representative_y_3435"),
  crs = 3435,
  remove = FALSE
)

footprints <- sf::st_read(
  "../temp/chicago_building_footprints_2015/buildings.shp",
  query = paste(
    "SELECT * FROM buildings",
    "WHERE YEAR_BUILT >= 2006 AND YEAR_BUILT <= 2015",
    "AND BLDG_STATU = 'ACTIVE'"
  ),
  quiet = TRUE
) |>
  sf::st_transform(3435)
names(footprints)[
  names(footprints) == attr(footprints, "sf_column")
] <- "geometry"
sf::st_geometry(footprints) <- "geometry"

nearby_rows <- sf::st_is_within_distance(
  footprints,
  chain_points,
  dist = 250
)
footprints <- footprints[lengths(nearby_rows) > 0, ] |>
  dplyr::mutate(
    bldg_id = as.character(BLDG_ID),
    footprint_id = dplyr::if_else(
      !is.na(bldg_id) & bldg_id != "",
      paste0("city_building_", bldg_id),
      paste0(
        "city_geometry_",
        vapply(
          sf::st_as_binary(geometry),
          digest::digest,
          character(1),
          algo = "sha256",
          serialize = FALSE
        )
      )
    ),
    address_from = suppressWarnings(as.integer(F_ADD1)),
    address_to = suppressWarnings(as.integer(T_ADD1)),
    street_direction = stringr::str_to_upper(dplyr::coalesce(PRE_DIR1, "")),
    street_name = stringr::str_to_upper(dplyr::coalesce(ST_NAME1, "")),
    street_type = stringr::str_to_upper(dplyr::coalesce(ST_TYPE1, "")),
    city_address = stringr::str_squish(paste(
      F_ADD1,
      T_ADD1,
      street_direction,
      street_name,
      street_type
    )),
    harris_pin = stringr::str_replace_all(
      dplyr::coalesce(as.character(HARRIS_STR), ""),
      "[^0-9]",
      ""
    ),
    city_year_built = suppressWarnings(as.integer(YEAR_BUILT)),
    city_units = suppressWarnings(as.numeric(NO_OF_UNIT)),
    city_building_sqft = suppressWarnings(as.numeric(BLDG_SQ_FO)),
    city_shape_area_sqft = as.numeric(sf::st_area(geometry))
  ) |>
  dplyr::arrange(footprint_id) |>
  dplyr::distinct(footprint_id, .keep_all = TRUE) |>
  dplyr::select(
    footprint_id,
    bldg_id,
    orig_bldg_ = ORIG_BLDG_,
    bldg_statu = BLDG_STATU,
    address_from,
    address_to,
    street_direction,
    street_name,
    street_type,
    city_address,
    bldg_name1 = BLDG_NAME1,
    comments = COMMENTS,
    footprint_ = FOOTPRINT_,
    bldg_creat = BLDG_CREAT,
    bldg_activ = BLDG_ACTIV,
    harris_pin,
    city_year_built,
    city_units,
    no_stories = NO_STORIES,
    city_building_sqft,
    city_shape_area_sqft
  )
sf::st_geometry(footprints) <- "geometry"

if (anyDuplicated(footprints$footprint_id)) {
  stop("City building footprints are not uniquely identified.", call. = FALSE)
}
if (any(!sf::st_is_valid(footprints))) {
  stop("City building footprints contain invalid geometry.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "residual_permit_chains",
    "citywide_active_footprints_built_2006_2015",
    "nearby_footprints_built_2006_2015",
    "source_snapshot_year"
  ),
  value = c(
    nrow(chains),
    nrow(sf::st_read(
      "../temp/chicago_building_footprints_2015/buildings.shp",
      query = paste(
        "SELECT BLDG_ID FROM buildings",
        "WHERE YEAR_BUILT >= 2006 AND YEAR_BUILT <= 2015",
        "AND BLDG_STATU = 'ACTIVE'"
      ),
      quiet = TRUE
    )),
    nrow(footprints),
    2015
  )
)

sf::st_write(
  footprints,
  "../output/permit_residual_city_building_footprints.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  tibble::tibble(
    source_url = paste0(
      "https://data.cityofchicago.org/api/views/syp8-uezg/files/",
      "Wir2BTHPb7-BTOWMkr8XcKCfKCt8U6y8wK20cV4Tjhw",
      "?download=true&filename=buildings.zip"
    ),
    source_vintage = "Current as of August 2015",
    zip_sha256 = digest::digest(
      file = "../temp/chicago_building_footprints_2015.zip",
      algo = "sha256"
    )
  ),
  "../output/permit_residual_city_building_footprint_manifest.csv"
)
readr::write_csv(
  summary,
  "../output/permit_residual_city_building_footprint_summary.csv"
)
