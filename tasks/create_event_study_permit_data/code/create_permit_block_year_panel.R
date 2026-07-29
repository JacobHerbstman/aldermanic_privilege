# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_event_study_permit_data/code")
# panel_max_distance_m <- 800
# permit_start_year <- 2010
# permit_end_year <- 2020

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(panel_max_distance_m, permit_start_year, permit_end_year)
}
if (length(cli_args) != 3) {
  stop("Script requires maximum distance, first permit year, and last permit year.", call. = FALSE)
}

panel_max_distance_m <- as.numeric(cli_args[1])
permit_start_year <- as.integer(cli_args[2])
permit_end_year <- as.integer(cli_args[3])
if (!is.finite(panel_max_distance_m) || panel_max_distance_m <= 0) {
  stop("panel_max_distance_m must be positive.", call. = FALSE)
}
if (!is.finite(permit_start_year) || !is.finite(permit_end_year) || permit_start_year > permit_end_year) {
  stop("Permit years must define an increasing window.", call. = FALSE)
}

sf_use_s2(FALSE)

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- build_canonical_boundary_list(ward_panel)

blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_make_valid() %>%
  st_transform(st_crs(ward_panel)) %>%
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id))

if (any(is.na(blocks$block_id) | blocks$block_id == "")) {
  stop("Census blocks contain missing block IDs.", call. = FALSE)
}
duplicate_blocks <- blocks %>%
  mutate(geometry_wkt = st_as_text(geometry)) %>%
  st_drop_geometry() %>%
  distinct(block_id, geometry_wkt) %>%
  count(block_id) %>%
  filter(n > 1)
if (nrow(duplicate_blocks) > 0) {
  stop("Census block IDs have conflicting geometries.", call. = FALSE)
}
blocks <- blocks %>% distinct(block_id, .keep_all = TRUE)

manual_assignments <- read_csv(
  "../input/manual_permit_block_assignments.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  transmute(
    id = trimws(id),
    block_vintage = trimws(block_vintage),
    reviewed_block_id = na_if(trimws(reviewed_block_id), "")
  ) %>%
  filter(block_vintage == "2010", id != "")
if (anyDuplicated(manual_assignments$id) > 0) {
  stop("Manual 2010 block assignments must be unique by permit ID.", call. = FALSE)
}

permits <- st_read(
  "../input/building_permits_clean.gpkg",
  query = paste(
    "SELECT id, permit_type, high_discretion, application_start_date_ym, geom",
    "FROM building_permits_clean",
    "WHERE permit_status NOT IN ('CANCELLED', 'REVOKED', 'SUSPENDED') OR permit_status IS NULL"
  ),
  quiet = TRUE
) %>%
  mutate(
    id = as.character(id),
    application_year = as.integer(format(as.Date(application_start_date_ym), "%Y"))
  ) %>%
  filter(application_year >= permit_start_year, application_year <= permit_end_year)

if (nrow(permits) == 0) {
  stop("No permits remain in the requested application-year window.", call. = FALSE)
}
if (st_crs(permits) != st_crs(blocks)) {
  permits <- st_transform(permits, st_crs(blocks))
}

permit_blocks <- st_join(permits, blocks %>% select(block_id), join = st_within) %>%
  st_drop_geometry()
if (any(permit_blocks %>% count(id) %>% pull(n) > 1L)) {
  stop("At least one permit falls within multiple census blocks.", call. = FALSE)
}

unmatched_ids <- permit_blocks %>%
  filter(is.na(block_id)) %>%
  distinct(id)
unreviewed_ids <- unmatched_ids %>%
  anti_join(manual_assignments, by = "id")
if (nrow(unreviewed_ids) > 0) {
  stop(
    sprintf("%d unmatched permits lack a manual 2010 block decision.", nrow(unreviewed_ids)),
    call. = FALSE
  )
}

invalid_manual_ids <- unmatched_ids %>%
  left_join(manual_assignments, by = "id", relationship = "one-to-one") %>%
  filter(!is.na(reviewed_block_id), !reviewed_block_id %in% blocks$block_id)
if (nrow(invalid_manual_ids) > 0) {
  stop("Manual permit assignments contain invalid 2010 census block IDs.", call. = FALSE)
}

permit_blocks <- permit_blocks %>%
  left_join(
    manual_assignments %>% select(id, reviewed_block_id),
    by = "id",
    relationship = "many-to-one"
  ) %>%
  mutate(block_id = coalesce(block_id, reviewed_block_id)) %>%
  filter(!is.na(block_id)) %>%
  select(-reviewed_block_id)

if (anyDuplicated(permit_blocks$id) > 0) {
  stop("Permit-to-block assignment must be unique by permit ID.", call. = FALSE)
}

permit_counts <- permit_blocks %>%
  group_by(block_id, year = application_year) %>%
  summarise(
    n_high_discretion_application = sum(high_discretion == 1, na.rm = TRUE),
    n_low_discretion_nosigns_application = sum(
      high_discretion == 0 & permit_type != "PERMIT - SIGNS",
      na.rm = TRUE
    ),
    n_new_construction_application = sum(
      permit_type == "PERMIT - NEW CONSTRUCTION",
      na.rm = TRUE
    ),
    .groups = "drop"
  )
if (anyDuplicated(permit_counts[c("block_id", "year")]) > 0) {
  stop("Permit counts must be unique by block and year.", call. = FALSE)
}

treatment <- read_csv("../input/block_treatment_panel_frozen2014.csv", show_col_types = FALSE) %>%
  mutate(block_id = as.character(block_id), cohort = as.character(cohort))
if (anyDuplicated(treatment$block_id) > 0) {
  stop("The 2015 treatment input must be unique by block.", call. = FALSE)
}

block_centroids <- st_centroid(blocks)
control_assignment <- assign_points_to_boundaries(
  points_sf = block_centroids,
  era_values = rep("2003_2014", nrow(block_centroids)),
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 5000L
) %>%
  transmute(
    block_id = blocks$block_id,
    control_own_ward = ward,
    control_neighbor_ward = neighbor_ward,
    control_pair_id = normalize_pair_dash(ward_pair_id),
    control_dist_m = dist_m
  )
if (anyDuplicated(control_assignment$block_id) > 0) {
  stop("Control boundary assignments must be unique by block.", call. = FALSE)
}

base <- blocks %>%
  st_drop_geometry() %>%
  select(block_id) %>%
  left_join(treatment, by = "block_id", relationship = "one-to-one") %>%
  left_join(control_assignment, by = "block_id", relationship = "one-to-one")

treated_pair_id <- normalize_pair_id(base$ward_origin, base$ward_dest, sep = "-")
treated_dist_m <- distance_to_boundary_pair_m(
  points_sf = block_centroids,
  pair_values = treated_pair_id,
  boundary_sf = boundary_lines[["2003_2014"]],
  chunk_n = 5000L
)

base <- base %>%
  mutate(
    switched = coalesce(switched, FALSE),
    treat = as.integer(switched),
    point_origin_mismatch = !is.na(control_own_ward) &
      !is.na(ward_origin) &
      ward_origin != control_own_ward,
    event_neighbor_ward = if_else(switched, ward_dest, control_neighbor_ward),
    ward_pair_id = if_else(switched, treated_pair_id, control_pair_id),
    dist_m = if_else(switched, treated_dist_m, control_dist_m),
    ward_pair_id = normalize_pair_dash(ward_pair_id),
    ward_pair_side = if_else(
      !is.na(ward_pair_id) & !is.na(ward_origin),
      paste(ward_pair_id, ward_origin, sep = "_"),
      NA_character_
    )
  ) %>%
  filter(
    valid,
    !is.na(ward_origin),
    !is.na(ward_dest),
    !is.na(ward_pair_id),
    !is.na(dist_m),
    !point_origin_mismatch,
    dist_m <= panel_max_distance_m
  )

if (anyNA(base$stable_both) || anyNA(base$strictness_change_frozen)) {
  stop("Frozen 2015 treatment mapping is incomplete.", call. = FALSE)
}
if (anyDuplicated(base$block_id) > 0) {
  stop("The event-study block sample must be unique by block.", call. = FALSE)
}

panel <- base %>%
  select(
    block_id, block_vintage, cohort,
    ward_origin, ward_dest, switched, treat,
    strictness_origin_frozen, strictness_dest_frozen, strictness_change_frozen,
    alderman_origin_2014, alderman_origin_2015,
    alderman_dest_2014, alderman_dest_2015,
    stable_origin, stable_dest, stable_both,
    valid, ward_pair_id, ward_pair_side, event_neighbor_ward, dist_m,
    point_origin_mismatch
  ) %>%
  tidyr::crossing(year = seq(permit_start_year, permit_end_year)) %>%
  mutate(relative_year = year - 2015L) %>%
  left_join(permit_counts, by = c("block_id", "year"), relationship = "many-to-one") %>%
  mutate(
    across(
      c(
        n_high_discretion_application,
        n_low_discretion_nosigns_application,
        n_new_construction_application
      ),
      ~ replace_na(as.integer(.x), 0L)
    )
  ) %>%
  arrange(block_id, year)

if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("The permit event-study panel must be unique by block and year.", call. = FALSE)
}

write_parquet(panel, "../output/permit_block_year_panel_2015.parquet")
