# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

review_pairs <- readr::read_csv(
  "../output/preferred_project_duplicate_temporal_pairs.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id_1 = readr::col_character(),
    project_id_2 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    temporal_lineage_status %in% c(
      "insufficient_temporal_evidence",
      "contemporaneous_same_site_requires_review"
    )
  ) |>
  dplyr::distinct(duplicate_review_group_id)

projects <- readr::read_csv(
  "../output/preferred_project_duplicate_review_members.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::semi_join(
    review_pairs,
    by = "duplicate_review_group_id"
  ) |>
  dplyr::distinct(
    duplicate_review_group_id,
    project_id,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    x_3435,
    y_3435,
    within_1500ft,
    within_500ft
  )

if (anyDuplicated(projects$project_id) ||
    any(!is.finite(projects$x_3435)) ||
    any(!is.finite(projects$y_3435))) {
  stop("Duplicate-review project locations are invalid.", call. = FALSE)
}

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  "../input/building_permits_clean.gpkg"
)
on.exit(DBI::dbDisconnect(con), add = TRUE)

permits <- DBI::dbGetQuery(
  con,
  "
SELECT
  cast(id AS TEXT) AS permit_id,
  cast(permit AS TEXT) AS permit_number,
  application_start_date,
  issue_date,
  street_number,
  street_direction,
  street_name,
  work_description,
  cast(xcoordinate AS REAL) AS x_3435,
  cast(ycoordinate AS REAL) AS y_3435
FROM building_permits_clean
WHERE permit_type = 'PERMIT - NEW CONSTRUCTION'
  AND application_start_date IS NOT NULL
  AND issue_date IS NOT NULL
  AND cast(substr(application_start_date, 1, 4) AS INTEGER)
      BETWEEN 2004 AND 2024
  AND cast(xcoordinate AS REAL) IS NOT NULL
  AND cast(ycoordinate AS REAL) IS NOT NULL
"
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    application_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    application_year = lubridate::year(application_date),
    permit_address = stringr::str_squish(
      paste(street_number, street_direction, street_name)
    )
  ) |>
  dplyr::select(
    permit_id,
    permit_number,
    application_date,
    issue_date,
    application_year,
    permit_address,
    work_description,
    x_3435,
    y_3435
  ) |>
  dplyr::distinct(permit_id, .keep_all = TRUE)

project_points <- sf::st_as_sf(
  projects,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)
permit_points <- sf::st_as_sf(
  permits,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)
nearby_permits <- sf::st_is_within_distance(
  project_points,
  permit_points,
  dist = units::set_units(125, "ft")
)

matches <- purrr::map2_dfr(
  seq_len(nrow(projects)),
  nearby_permits,
  function(project_row, permit_rows) {
    if (length(permit_rows) == 0L) {
      return(tibble::tibble())
    }
    distances <- units::drop_units(
      sf::st_distance(
        project_points[project_row, ],
        permit_points[permit_rows, ],
        by_element = FALSE
      )
    )
    dplyr::bind_cols(
      projects[rep(project_row, length(permit_rows)), ],
      permits[permit_rows, ] |>
        dplyr::rename(
          permit_x_3435 = x_3435,
          permit_y_3435 = y_3435
        ),
      tibble::tibble(project_to_permit_ft = as.numeric(distances))
    )
  }
) |>
  dplyr::filter(
    application_year >= construction_year - 2L,
    application_year <= construction_year + 2L
  ) |>
  dplyr::arrange(
    duplicate_review_group_id,
    project_id,
    project_to_permit_ft,
    application_date,
    permit_id
  )

project_summary <- projects |>
  dplyr::left_join(
    matches |>
      dplyr::group_by(duplicate_review_group_id, project_id) |>
      dplyr::summarise(
        nearby_permit_count = dplyr::n_distinct(permit_id),
        nearby_permit_address_count =
          dplyr::n_distinct(permit_address),
        nearest_permit_distance_ft = min(project_to_permit_ft),
        nearby_permit_addresses = paste(
          sort(unique(permit_address)),
          collapse = "/"
        ),
        .groups = "drop"
      ),
    by = c("duplicate_review_group_id", "project_id"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    nearby_permit_count = dplyr::coalesce(
      nearby_permit_count,
      0L
    ),
    nearby_permit_address_count = dplyr::coalesce(
      nearby_permit_address_count,
      0L
    )
  )

group_summary <- projects |>
  dplyr::group_by(duplicate_review_group_id) |>
  dplyr::summarise(
    project_count = dplyr::n_distinct(project_id),
    any_within_1500ft = any(within_1500ft),
    any_within_500ft = any(within_500ft),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    matches |>
      dplyr::group_by(duplicate_review_group_id) |>
      dplyr::summarise(
        nearby_permit_count = dplyr::n_distinct(permit_id),
        nearby_permit_address_count =
          dplyr::n_distinct(permit_address),
        nearby_permit_addresses = paste(
          sort(unique(permit_address)),
          collapse = "/"
        ),
        .groups = "drop"
      ),
    by = "duplicate_review_group_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    nearby_permit_count = dplyr::coalesce(
      nearby_permit_count,
      0L
    ),
    nearby_permit_address_count = dplyr::coalesce(
      nearby_permit_address_count,
      0L
    )
  )

readr::write_csv(
  matches,
  "../output/preferred_project_duplicate_nearby_permits.csv"
)
readr::write_csv(
  project_summary,
  "../output/preferred_project_duplicate_nearby_permit_projects.csv"
)
readr::write_csv(
  group_summary,
  "../output/preferred_project_duplicate_nearby_permit_summary.csv"
)
