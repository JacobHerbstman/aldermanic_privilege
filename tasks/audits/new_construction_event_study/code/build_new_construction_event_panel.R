# setwd("tasks/audits/new_construction_event_study/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE
)
coordinates <- readr::read_csv(
  "../input/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE
) |>
  dplyr::select(project_id, x_3435, y_3435)

if (
  anyDuplicated(projects$project_id) ||
    anyDuplicated(coordinates$project_id) ||
    any(is.na(projects$project_id)) ||
    any(is.na(coordinates$project_id))
) {
  stop("Project identifiers must be complete and unique.", call. = FALSE)
}

projects <- projects |>
  dplyr::left_join(
    coordinates,
    by = "project_id",
    relationship = "one-to-one"
  )

if (
  any(is.na(projects$x_3435)) ||
    any(is.na(projects$y_3435))
) {
  stop("Every analysis project must have saved coordinates.", call. = FALSE)
}

block_rows <- readr::read_csv(
  "../input/census_blocks_2010.csv",
  col_types = readr::cols(
    .default = readr::col_character()
  )
) |>
  dplyr::select(block_id = GEOID10, the_geom)

conflicting_blocks <- block_rows |>
  dplyr::group_by(block_id) |>
  dplyr::summarise(
    geometry_count = dplyr::n_distinct(the_geom),
    .groups = "drop"
  ) |>
  dplyr::filter(geometry_count != 1L)

if (
  nrow(conflicting_blocks) > 0L ||
    any(is.na(block_rows$block_id))
) {
  stop("Census block geometries failed validation.", call. = FALSE)
}

blocks <- block_rows |>
  dplyr::distinct(block_id, the_geom) |>
  sf::st_as_sf(wkt = "the_geom", crs = 4269) |>
  sf::st_transform(3435)

if (
  anyDuplicated(blocks$block_id) ||
    any(is.na(blocks$block_id))
) {
  stop("Census block identifiers must be complete and unique.", call. = FALSE)
}

project_points <- sf::st_as_sf(
  projects,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)

project_blocks <- sf::st_join(
  project_points,
  blocks,
  join = sf::st_within,
  left = TRUE
) |>
  sf::st_drop_geometry()

if (anyDuplicated(project_blocks$project_id)) {
  stop("A project was assigned to more than one census block.", call. = FALSE)
}

permit_panel <- arrow::read_parquet(
  "../input/permit_block_year_panel_2015.parquet"
) |>
  dplyr::mutate(block_id = as.character(block_id)) |>
  dplyr::filter(
    dist_m <= 152.4,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) |>
  dplyr::select(
    block_id,
    year,
    relative_year,
    ward_pair_id,
    dist_m,
    alderman_origin_2014,
    alderman_dest_2014,
    n_high_discretion_application
  )

if (
  anyDuplicated(permit_panel[c("block_id", "year")]) ||
    any(is.na(permit_panel$block_id))
) {
  stop("The permit event-study panel must be unique by block and year.", call. = FALSE)
}

scores <- readr::read_csv(
  "../input/current_income_scores.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    cutoff == 2014L,
    variant == "income_added_back"
  ) |>
  dplyr::select(alderman, score)

if (
  nrow(scores) == 0L ||
    anyDuplicated(scores$alderman) ||
    any(is.na(scores$alderman)) ||
    any(is.na(scores$score))
) {
  stop("The selected 2006-2014 scores failed validation.", call. = FALSE)
}

permit_panel <- permit_panel |>
  dplyr::left_join(
    scores |>
      dplyr::rename(
        alderman_origin_2014 = alderman,
        score_origin = score
      ),
    by = "alderman_origin_2014",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    scores |>
      dplyr::rename(
        alderman_dest_2014 = alderman,
        score_dest = score
      ),
    by = "alderman_dest_2014",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    score_change = score_dest - score_origin,
    stricter = as.integer(score_change > 0),
    lenient = as.integer(score_change < 0),
    signed_direction = stricter - lenient
  )

if (
  any(is.na(permit_panel$score_origin)) ||
    any(is.na(permit_panel$score_dest)) ||
    any(!permit_panel$signed_direction %in% c(-1L, 0L, 1L))
) {
  stop("The binary treatment assignment failed validation.", call. = FALSE)
}

eligible_blocks <- permit_panel |>
  dplyr::distinct(block_id)

project_blocks <- project_blocks |>
  dplyr::mutate(
    block_id = as.character(block_id),
    event_period = construction_year >= 2010L &
      construction_year <= 2020L,
    eligible_event_block = !is.na(block_id) &
      block_id %in% eligible_blocks$block_id,
    valid_far = allow_far &
      !is.na(density_far) &
      density_far > 0,
    valid_dupac = allow_dupac &
      !is.na(density_dupac) &
      density_dupac > 0
  )

event_projects <- project_blocks |>
  dplyr::filter(event_period, eligible_event_block)

construction_by_block_year <- event_projects |>
  dplyr::group_by(
    block_id,
    year = construction_year
  ) |>
  dplyr::summarise(
    n_construction = dplyr::n(),
    n_far_projects = sum(valid_far),
    mean_log_far = ifelse(
      n_far_projects > 0L,
      mean(log(density_far[valid_far])),
      NA_real_
    ),
    n_dupac_projects = sum(valid_dupac),
    mean_log_dupac = ifelse(
      n_dupac_projects > 0L,
      mean(log(density_dupac[valid_dupac])),
      NA_real_
    ),
    .groups = "drop"
  )

if (anyDuplicated(construction_by_block_year[c("block_id", "year")])) {
  stop("Construction outcomes must be unique by block and year.", call. = FALSE)
}

event_panel <- permit_panel |>
  dplyr::left_join(
    construction_by_block_year,
    by = c("block_id", "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    n_construction = dplyr::coalesce(n_construction, 0L),
    n_far_projects = dplyr::coalesce(n_far_projects, 0L),
    n_dupac_projects = dplyr::coalesce(n_dupac_projects, 0L)
  )

pre_period_controls <- event_panel |>
  dplyr::filter(relative_year < 0L) |>
  dplyr::group_by(block_id) |>
  dplyr::summarise(
    pre_period_construction = sum(n_construction),
    pre_period_permit_volume = sum(
      n_high_discretion_application
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    no_pre_period_construction = as.integer(
      pre_period_construction == 0L
    ),
    no_pre_period_permits = as.integer(
      pre_period_permit_volume == 0L
    )
  )

if (anyDuplicated(pre_period_controls$block_id)) {
  stop("Pre-period controls must be unique by block.", call. = FALSE)
}

event_panel <- event_panel |>
  dplyr::left_join(
    pre_period_controls,
    by = "block_id",
    relationship = "many-to-one"
  )

coverage <- tibble::tribble(
  ~measure, ~count,
  "final_projects_all_years", nrow(project_blocks),
  "projects_assigned_to_2010_block", sum(!is.na(project_blocks$block_id)),
  "projects_2010_2020", sum(project_blocks$event_period),
  "projects_2010_2020_in_event_blocks", nrow(event_projects),
  "projects_2010_2020_within_500ft_by_project", sum(
    project_blocks$event_period & project_blocks$within_500ft
  ),
  "projects_2010_2020_in_event_blocks_and_within_500ft_by_project", sum(
    project_blocks$event_period &
      project_blocks$eligible_event_block &
      project_blocks$within_500ft
  ),
  "projects_2010_2020_in_event_blocks_beyond_500ft_by_project", sum(
    project_blocks$event_period &
      project_blocks$eligible_event_block &
      !project_blocks$within_500ft
  ),
  "event_blocks", dplyr::n_distinct(event_panel$block_id),
  "event_block_years", nrow(event_panel),
  "positive_construction_block_years", sum(
    event_panel$n_construction > 0L
  ),
  "far_block_years", sum(!is.na(event_panel$mean_log_far)),
  "dupac_block_years", sum(!is.na(event_panel$mean_log_dupac))
)

readr::write_csv(
  project_blocks |>
    dplyr::select(
      project_id,
      construction_year,
      block_id,
      event_period,
      eligible_event_block,
      within_500ft,
      distance_to_boundary_ft,
      valid_far,
      valid_dupac
    ),
  "../output/new_construction_project_block_assignments.csv"
)
readr::write_csv(
  coverage,
  "../output/new_construction_project_coverage.csv"
)
arrow::write_parquet(
  event_panel,
  "../output/new_construction_block_year_panel.parquet"
)
