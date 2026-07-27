# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../input/eligibility_uncorroborated_retained.csv",
  show_col_types = FALSE,
  col_select = c(
    project_id,
    within_500ft,
    x_3435,
    y_3435
  ),
  col_types = readr::cols(
    project_id = readr::col_character(),
    within_500ft = readr::col_logical(),
    x_3435 = readr::col_double(),
    y_3435 = readr::col_double()
  )
) |>
  dplyr::filter(within_500ft) |>
  dplyr::select(project_id, x_3435, y_3435)

if (nrow(projects) != 795L || anyDuplicated(projects$project_id)) {
  stop("The verification scope is not the expected 795 unique projects.")
}

project_polygons <- sf::st_read(
  "../input/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::inner_join(
    projects |>
      dplyr::select(project_id),
    by = "project_id",
    relationship = "one-to-one"
  )

project_points <- projects |>
  dplyr::anti_join(
    sf::st_drop_geometry(project_polygons) |>
      dplyr::select(project_id),
    by = "project_id"
  )
if (
  any(!is.finite(project_points$x_3435)) ||
    any(!is.finite(project_points$y_3435))
) {
  stop("Projects without polygons also lack audited coordinates.")
}

project_search_sites <- dplyr::bind_rows(
  project_polygons |>
    dplyr::select(project_id),
  project_points |>
    sf::st_as_sf(
      coords = c("x_3435", "y_3435"),
      crs = 3435,
      remove = TRUE
    ) |>
    sf::st_buffer(100) |>
    dplyr::select(project_id)
)

if (
  nrow(project_search_sites) != nrow(projects) ||
    anyDuplicated(project_search_sites$project_id)
) {
  stop("Project search sites are incomplete or duplicated.")
}

footprints <- sf::st_read(
  paste0(
    "/vsizip/",
    getwd(),
    "/../input/chicago_building_footprints_2015.zip/buildings.shp"
  ),
  query = "SELECT BLDG_ID, YEAR_BUILT FROM buildings",
  quiet = TRUE
) |>
  sf::st_transform(3435)

near_project_sites <- lengths(
  sf::st_intersects(
    footprints,
    sf::st_union(sf::st_buffer(project_search_sites, 100))
  )
) > 0L
footprints <- footprints[near_project_sites, ] |>
  sf::st_make_valid()
footprints <- footprints[!sf::st_is_empty(footprints), ]

if (
  nrow(footprints) == 0L ||
    anyDuplicated(footprints$BLDG_ID) ||
    any(!sf::st_is_valid(footprints))
) {
  stop("The local 2015 footprint extract failed validation.")
}

sf::st_write(
  footprints,
  "../output/official_building_footprints_2015.gpkg",
  layer = "official_building_footprints_2015",
  delete_dsn = TRUE,
  quiet = TRUE
)

readr::write_csv(
  tibble::tibble(
    snapshot_year = 2015L,
    source_url = paste0(
      "https://data.cityofchicago.org/Buildings/",
      "Building-Footprints/syp8-uezg"
    ),
    source_file = "chicago_building_footprints_2015.zip",
    retained_unique_features = nrow(footprints),
    project_scope = nrow(projects)
  ),
  "../output/official_building_footprints_2015_manifest.csv"
)
