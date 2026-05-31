# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_event_study_permit_data/code")
# segment_buffer_m <- 304.8
# panel_max_distance_m <- 800
# permit_start_year <- 2010
# permit_end_month <- "2020-12"

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(segment_buffer_m, panel_max_distance_m, permit_start_year, permit_end_month)
}

if (length(cli_args) != 4) {
  stop("FATAL: Script requires 4 args: <segment_buffer_m> <panel_max_distance_m> <permit_start_year> <permit_end_month>", call. = FALSE)
}
segment_buffer_m <- as.numeric(cli_args[1])
panel_max_distance_m <- as.numeric(cli_args[2])
permit_start_year <- as.integer(cli_args[3])
permit_end_month <- cli_args[4]
if (!is.finite(segment_buffer_m) || segment_buffer_m <= 0) {
  stop("segment_buffer_m must be positive.", call. = FALSE)
}
if (!is.finite(panel_max_distance_m) || panel_max_distance_m <= 0) {
  stop("panel_max_distance_m must be positive.", call. = FALSE)
}
if (!is.finite(permit_start_year)) {
  stop("permit_start_year must be a valid year.", call. = FALSE)
}
if (!grepl("^\\d{4}-\\d{2}$", permit_end_month)) {
  stop("permit_end_month must use YYYY-MM format.", call. = FALSE)
}
permit_end_yearmon <- as.yearmon(as.Date(paste0(permit_end_month, "-01")))
permit_end_year <- as.integer(format(as.Date(permit_end_yearmon), "%Y"))
if (permit_start_year > permit_end_year) {
  stop("permit_start_year must be no later than permit_end_month.", call. = FALSE)
}

sf_use_s2(FALSE)

signs_permit_type <- "PERMIT - SIGNS"

manual_block_assignments <- read_csv(
  "../input/manual_permit_block_assignments.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)
if (!all(c("id", "block_vintage", "reviewed_block_id") %in% names(manual_block_assignments))) {
  stop("manual_permit_block_assignments.csv is missing required columns.", call. = FALSE)
}
manual_block_assignments <- manual_block_assignments %>%
  transmute(
    id = trimws(id),
    block_vintage = trimws(block_vintage),
    reviewed_block_id = na_if(trimws(reviewed_block_id), "")
  ) %>%
  filter(id != "", block_vintage != "")
if (anyDuplicated(manual_block_assignments[, c("id", "block_vintage")]) > 0) {
  stop("manual_permit_block_assignments.csv must be unique by id-block_vintage.", call. = FALSE)
}

read_blocks <- function(path, block_col, target_crs) {
  blocks_sf <- read_csv(path, show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_make_valid() %>%
    st_transform(target_crs) %>%
    rename(block_id = all_of(block_col)) %>%
    mutate(block_id = as.character(block_id))

  if (any(is.na(blocks_sf$block_id) | blocks_sf$block_id == "")) {
    stop(sprintf("Block input %s contains missing block_id values.", path), call. = FALSE)
  }

  duplicate_block_ids <- blocks_sf %>%
    mutate(geometry_wkt = st_as_text(st_geometry(.))) %>%
    st_drop_geometry() %>%
    count(block_id, geometry_wkt, name = "n_rows") %>%
    count(block_id, name = "n_unique_geometries") %>%
    filter(n_unique_geometries > 1)

  if (nrow(duplicate_block_ids) > 0) {
    stop(
      sprintf(
        "Block input %s contains %d block IDs with conflicting geometries.",
        path,
        nrow(duplicate_block_ids)
      ),
      call. = FALSE
    )
  }

  blocks_sf %>%
    distinct(block_id, .keep_all = TRUE)
}

assign_permits_to_blocks <- function(permits_sf, blocks_sf, block_vintage_label, manual_block_assignments) {
  joined <- st_join(permits_sf, blocks_sf %>% select(block_id), join = st_within)
  joined_df <- joined %>% st_drop_geometry()

  duplicate_matches <- joined_df %>%
    count(id, name = "n_matches") %>%
    filter(n_matches > 1)

  if (nrow(duplicate_matches) > 0) {
    stop(
      sprintf(
        "Permit-to-block assignment produced %d permits with multiple block matches.",
        nrow(duplicate_matches)
      ),
      call. = FALSE
    )
  }

  missing_matches <- joined_df %>%
    filter(is.na(block_id)) %>%
    distinct(id)

  if (nrow(missing_matches) > 0) {
    missing_review <- missing_matches %>%
      left_join(
        manual_block_assignments %>% filter(block_vintage == block_vintage_label),
        by = "id",
        relationship = "many-to-one"
      ) %>%
      mutate(
        reviewed_block_id = na_if(reviewed_block_id, "NA"),
        review_decision = if_else(is.na(reviewed_block_id), "drop", "assign"),
        final_block_id = if_else(review_decision == "assign", reviewed_block_id, NA_character_)
      )

    invalid_manual_assignments <- missing_review %>%
      filter(review_decision == "assign") %>%
      filter(is.na(final_block_id) | !final_block_id %in% blocks_sf$block_id)

    if (nrow(invalid_manual_assignments) > 0) {
      stop(
        sprintf(
          "Manual block review contains %d invalid %s-vintage block assignments.",
          nrow(invalid_manual_assignments),
          block_vintage_label
        ),
        call. = FALSE
      )
    }

    joined_df <- joined_df %>%
      left_join(
        missing_review %>% select(id, final_block_id),
        by = "id",
        relationship = "many-to-one"
      ) %>%
      mutate(block_id = coalesce(block_id, final_block_id)) %>%
      select(-final_block_id)
  }

  as.data.table(
    joined_df %>%
      filter(!is.na(block_id)) %>%
      arrange(id, block_id)
  )
}

permit_outcome_meta <- tibble(
  count_var = c(
    "n_new_construction_issue",
    "n_new_construction_application",
    "n_new_construction_demolition_issue",
    "n_new_construction_demolition_application",
    "n_demolition_issue",
    "n_demolition_application",
    "n_renovation_issue",
    "n_renovation_application",
    "n_low_discretion_nosigns_issue",
    "n_low_discretion_nosigns_application",
    "n_high_discretion_issue",
    "n_high_discretion_application",
    "n_unit_increase_issue",
    "n_unit_increase_application"
  ),
  indicator_col = c(
    "is_new_construction_issued",
    "is_new_construction_issued",
    "is_new_construction_demolition_issued",
    "is_new_construction_demolition_issued",
    "is_demolition_issued",
    "is_demolition_issued",
    "is_renovation_issued",
    "is_renovation_issued",
    "is_low_discretion_nosigns_issued",
    "is_low_discretion_nosigns_issued",
    "is_high_discretion_issued",
    "is_high_discretion_issued",
    "is_unit_increase_issued",
    "is_unit_increase_issued"
  ),
  date_col = c(
    "issue_year",
    "application_year",
    "issue_year",
    "application_year",
    "issue_year",
    "application_year",
    "issue_year",
    "application_year",
    "issue_year",
    "application_year",
    "issue_year",
    "application_year",
    "issue_year",
    "application_year"
  )
)

aggregate_outcome_counts <- function(permits_dt) {
  pieces <- list()
  for (i in seq_len(nrow(permit_outcome_meta))) {
    spec_i <- permit_outcome_meta[i, ]
    part_i <- permits_dt[
      get(spec_i$indicator_col) == 1L & !is.na(get(spec_i$date_col)),
      .(count = .N),
      by = .(block_id, year = get(spec_i$date_col))
    ]
    setnames(part_i, "count", spec_i$count_var)
    pieces[[i]] <- part_i
  }

  if (length(pieces) == 0) {
    return(data.table(block_id = character(), year = integer()))
  }

  out <- Reduce(function(x, y) merge(x, y, by = c("block_id", "year"), all = TRUE), pieces)
  for (count_var in permit_outcome_meta$count_var) {
    if (!count_var %in% names(out)) {
      out[, (count_var) := 0L]
    }
    out[is.na(get(count_var)), (count_var) := 0L]
  }

  setorder(out, block_id, year)
  out
}

build_panel_with_counts <- function(base_df, start_year, end_year, cohort_year, counts_dt) {
  if (anyDuplicated(paste(counts_dt$block_id, counts_dt$year, sep = "\r")) > 0) {
    stop("Permit outcome counts must be unique by block-year.", call. = FALSE)
  }

  panel <- base_df %>%
    tidyr::crossing(year = seq(start_year, end_year)) %>%
    mutate(
      relative_year = year - cohort_year,
      relative_year_capped = relative_year,
      cohort_block_id = paste(cohort, block_id, sep = "_"),
      cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_"),
      cohort_ward_pair_side = paste(cohort, ward_pair_side, sep = "_"),
      cohort_segment = if_else(!is.na(segment_id_cohort), paste(cohort, segment_id_cohort, sep = "_"), NA_character_),
      cohort_segment_side = if_else(!is.na(segment_side), paste(cohort, segment_side, sep = "_"), NA_character_)
    ) %>%
    left_join(as_tibble(counts_dt), by = c("block_id", "year"), relationship = "many-to-one")

  count_cols <- permit_outcome_meta$count_var

  panel %>%
    mutate(across(all_of(count_cols), ~ replace_na(as.integer(.x), 0L))) %>%
    mutate(
      has_new_construction_issue = as.integer(n_new_construction_issue > 0L),
      has_new_construction_application = as.integer(n_new_construction_application > 0L),
      has_new_construction_demolition_issue = as.integer(n_new_construction_demolition_issue > 0L),
      has_new_construction_demolition_application = as.integer(n_new_construction_demolition_application > 0L),
      has_demolition_issue = as.integer(n_demolition_issue > 0L),
      has_demolition_application = as.integer(n_demolition_application > 0L),
      has_renovation_issue = as.integer(n_renovation_issue > 0L),
      has_renovation_application = as.integer(n_renovation_application > 0L),
      has_low_discretion_nosigns_issue = as.integer(n_low_discretion_nosigns_issue > 0L),
      has_low_discretion_nosigns_application = as.integer(n_low_discretion_nosigns_application > 0L),
      has_high_discretion_issue = as.integer(n_high_discretion_issue > 0L),
      has_high_discretion_application = as.integer(n_high_discretion_application > 0L),
      has_unit_increase_issue = as.integer(n_unit_increase_issue > 0L),
      has_unit_increase_application = as.integer(n_unit_increase_application > 0L)
    )
}

prepare_cohort_base <- function(blocks_sf, treatment_df, cohort_label, era_label) {
  if (anyDuplicated(treatment_df$block_id) > 0) {
    stop(sprintf("Treatment input for cohort %s must be unique by block_id.", cohort_label), call. = FALSE)
  }

  block_centroids <- st_centroid(blocks_sf)
  control_assign <- assign_points_to_boundaries(
    points_sf = block_centroids,
    era_values = rep(era_label, nrow(block_centroids)),
    ward_maps = ward_maps,
    boundary_lines = boundary_lines,
    chunk_n = 5000L
  ) %>%
    transmute(
      block_id = blocks_sf$block_id,
      control_own_ward = ward,
      control_neighbor_ward = neighbor_ward,
      control_pair_id = normalize_pair_dash(ward_pair_id),
      control_dist_m = dist_m
    )
  if (anyDuplicated(control_assign$block_id) > 0) {
    stop(sprintf("Boundary assignment for cohort %s must be unique by block_id.", cohort_label), call. = FALSE)
  }

  base <- st_drop_geometry(blocks_sf) %>%
    select(block_id) %>%
    left_join(treatment_df, by = "block_id", relationship = "one-to-one") %>%
    left_join(control_assign, by = "block_id", relationship = "one-to-one")

  treated_pair_id <- normalize_pair_id(base$ward_origin, base$ward_dest, sep = "-")
  treated_dist_m <- distance_to_boundary_pair_m(
    points_sf = block_centroids,
    pair_values = treated_pair_id,
    boundary_sf = boundary_lines[[era_label]],
    chunk_n = 5000L
  )

  base <- base %>%
    mutate(
      cohort = cohort_label,
      switched = coalesce(switched, FALSE),
      treat = as.integer(switched),
      point_origin_mismatch = !is.na(control_own_ward) &
        !is.na(ward_origin) &
        ward_origin != control_own_ward,
      event_neighbor_ward = if_else(switched, ward_dest, control_neighbor_ward),
      ward_pair_id = if_else(switched, treated_pair_id, control_pair_id),
      dist_m = if_else(switched, treated_dist_m, control_dist_m),
      control_origin_mismatch = !switched & point_origin_mismatch
    ) %>%
    mutate(
      ward_pair_id = normalize_pair_dash(ward_pair_id),
      ward_pair_side = if_else(
        !is.na(ward_pair_id) & !is.na(ward_origin),
        paste(ward_pair_id, ward_origin, sep = "_"),
        NA_character_
      )
    )

  segment_id <- assign_points_to_nearest_segments(
    points_sf = block_centroids,
    era_values = rep(era_label, nrow(block_centroids)),
    pair_values = base$ward_pair_id,
    segment_layers = segment_layers,
    max_distance = units::set_units(segment_buffer_m, "m"),
    chunk_n = 50000L
  )

  assert_event_segment_contract(
    points_sf = block_centroids,
    era_values = rep(era_label, nrow(block_centroids)),
    pair_values = base$ward_pair_id,
    segment_layers = segment_layers,
    segment_id = segment_id,
    boundary_dist_m = base$dist_m,
    max_distance_m = segment_buffer_m,
    context = sprintf("permit %s", cohort_label),
    chunk_n = 50000L
  )

  segment_match <- match(
    paste(era_label, segment_id, sep = "\r"),
    paste(segment_meta$era, segment_meta$segment_id, sep = "\r")
  )

  base <- base %>%
    mutate(
      segment_id_cohort = segment_id,
      segment_length_ft_cohort = segment_meta$segment_length_ft[segment_match],
      segment_lt500ft_cohort = segment_meta$segment_lt500ft[segment_match],
      segment_lt1000ft_cohort = segment_meta$segment_lt1000ft[segment_match],
      segment_side = if_else(
        !is.na(segment_id_cohort) & !is.na(ward_origin),
        paste(segment_id_cohort, ward_origin, sep = "_"),
        NA_character_
      )
    ) %>%
    filter(
      !is.na(switched),
      valid,
      !is.na(strictness_change),
      !is.na(ward_pair_id),
      !is.na(ward_origin),
      !is.na(dist_m),
      point_origin_mismatch == FALSE,
      dist_m <= panel_max_distance_m
    ) %>%
    select(
      block_id, block_vintage, cohort,
      ward_origin, ward_dest, switched, treat,
      strictness_origin, strictness_dest, strictness_change, switch_type,
      valid, ward_pair_id, ward_pair_side, event_neighbor_ward, dist_m,
      segment_id_cohort, segment_side,
      segment_length_ft_cohort, segment_lt500ft_cohort, segment_lt1000ft_cohort,
      control_origin_mismatch, point_origin_mismatch
    )

  base
}

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- build_canonical_boundary_list(ward_panel)
segment_layers <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg")
segment_meta <- segment_metadata_from_layers(segment_layers)
segment_pair_lookup <- bind_rows(lapply(names(segment_layers), function(era_i) {
  st_drop_geometry(segment_layers[[era_i]]) %>%
    transmute(
      segment_era = era_i,
      segment_id_cohort = as.character(segment_id),
      segment_pair_id = normalize_pair_dash(ward_pair_id)
    )
}))

treatment_panel <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE) %>%
  mutate(block_id = as.character(block_id))
if (anyDuplicated(paste(treatment_panel$cohort, treatment_panel$block_id, sep = "\r")) > 0) {
  stop("Block treatment panel must be unique by cohort-block.", call. = FALSE)
}

blocks_2010 <- read_blocks("../input/census_blocks_2010.csv", "GEOID10", st_crs(ward_panel))
blocks_2020 <- read_blocks("../input/census_blocks_2020.csv", "GEOID20", st_crs(ward_panel))

curated_unit_types <- c(
  "PERMIT - NEW CONSTRUCTION",
  "PERMIT - RENOVATION/ALTERATION"
)

unit_increase_flags <- read_csv(
  "../input/building_permits_text_features.csv.gz",
  show_col_types = FALSE,
  col_select = c(
    "id", "permit_type", "permit_issued", "unit_change_text",
    "unit_change_direction", "unit_change_confidence", "unit_increase_signal",
    "unit_change_signal", "flag_revision", "flag_revision_contractor",
    "flag_new_construction_phrase", "flag_adu", "flag_increase"
  )
) %>%
  mutate(
    permit_issued = as.integer(permit_issued),
    unit_change_text = suppressWarnings(as.numeric(unit_change_text)),
    unit_increase_signal = coalesce(as.integer(unit_increase_signal), 0L),
    unit_change_signal = coalesce(as.integer(unit_change_signal), 0L),
    flag_revision = coalesce(as.integer(flag_revision), 0L),
    flag_revision_contractor = coalesce(as.integer(flag_revision_contractor), 0L),
    flag_new_construction_phrase = coalesce(as.integer(flag_new_construction_phrase), 0L),
    flag_adu = coalesce(as.integer(flag_adu), 0L),
    flag_increase = coalesce(as.integer(flag_increase), 0L),
    curated_type = permit_type %in% curated_unit_types,
    positive_unit_signal = unit_increase_signal == 1L |
      (!is.na(unit_change_text) & unit_change_text > 0) |
      unit_change_direction == "increase",
    revision_only = flag_revision == 1L | flag_revision_contractor == 1L,
    unit_increase_candidate = (permit_issued == 1L | is.na(permit_issued)) & (curated_type | positive_unit_signal),
    unit_increase_reason = case_when(
      !unit_increase_candidate ~ "outside_unit_increase_candidate_pool",
      !curated_type ~ "excluded_outside_curated_type",
      !positive_unit_signal ~ "excluded_no_positive_unit_signal",
      revision_only ~ "excluded_revision_signal",
      unit_change_confidence == "high" ~ "included_high_confidence",
      unit_change_confidence == "medium" ~ "included_medium_confidence",
      unit_change_confidence == "low" ~ "included_low_confidence",
      TRUE ~ "included_unclassified_confidence"
    ),
    unit_increase_included = as.integer(grepl("^included_", unit_increase_reason))
  ) %>%
  arrange(desc(unit_increase_included), desc(positive_unit_signal), desc(curated_type), id)

permits_clean <- st_read(
  "../input/building_permits_clean.gpkg",
  query = paste(
    "SELECT id, pin, permit_type, high_discretion, permit_issued,",
    "application_start_date_ym, issue_date_ym, latitude, longitude, processing_time, geom",
    "FROM building_permits_clean",
    "WHERE permit_issued = 1 OR permit_issued IS NULL"
  ),
  quiet = TRUE
) %>%
  mutate(
    id = as.character(id),
    application_year = as.integer(format(as.Date(application_start_date_ym), "%Y")),
    issue_year = as.integer(format(as.Date(issue_date_ym), "%Y")),
    application_month = as.yearmon(as.Date(application_start_date_ym)),
    issue_month = as.yearmon(as.Date(issue_date_ym))
  )

permits_clean <- permits_clean %>%
  filter(
    (!is.na(application_month) &
      application_year >= permit_start_year &
      application_month <= permit_end_yearmon) |
      (!is.na(issue_month) &
        issue_year >= permit_start_year &
        issue_month <= permit_end_yearmon)
  )
if (nrow(permits_clean) == 0) {
  stop("No issued permits remain after applying the permit analysis year window.", call. = FALSE)
}

unit_increase_flags <- unit_increase_flags %>%
  semi_join(
    permits_clean %>%
      st_drop_geometry() %>%
      select(id),
    by = "id"
  )

unit_increase_included_ids <- unit_increase_flags %>%
  filter(unit_increase_included == 1L) %>%
  select(id, unit_increase_included, unit_increase_reason)
if (anyDuplicated(unit_increase_included_ids$id) > 0) {
  stop("Unit-increase inclusion IDs must be unique by permit id.", call. = FALSE)
}

permit_year_min <- permit_start_year
permit_year_max <- permit_end_year

if (st_crs(permits_clean) != st_crs(blocks_2010)) {
  permits_clean <- st_transform(permits_clean, st_crs(blocks_2010))
}

permits_2010 <- assign_permits_to_blocks(
  permits_sf = permits_clean,
  blocks_sf = blocks_2010,
  block_vintage_label = "2010",
  manual_block_assignments = manual_block_assignments
) %>%
  left_join(unit_increase_included_ids, by = "id", relationship = "many-to-one") %>%
  mutate(
    unit_increase_included = coalesce(as.integer(unit_increase_included), 0L)
  )
setDT(permits_2010)
permits_2010[, `:=`(
  is_new_construction_issued = as.integer(permit_type == "PERMIT - NEW CONSTRUCTION"),
  is_demolition_issued = as.integer(grepl("WRECKING|DEMOLITION", permit_type)),
  is_renovation_issued = as.integer(permit_type == "PERMIT - RENOVATION/ALTERATION"),
  is_new_construction_demolition_issued = as.integer(
    permit_type == "PERMIT - NEW CONSTRUCTION" | grepl("WRECKING|DEMOLITION", permit_type)
  ),
  is_low_discretion_nosigns_issued = as.integer(high_discretion == 0 & permit_type != signs_permit_type),
  is_high_discretion_issued = as.integer(high_discretion == 1),
  is_unit_increase_issued = as.integer(unit_increase_included == 1)
)]

permits_2020 <- assign_permits_to_blocks(
  permits_sf = permits_clean,
  blocks_sf = blocks_2020,
  block_vintage_label = "2020",
  manual_block_assignments = manual_block_assignments
) %>%
  left_join(unit_increase_included_ids, by = "id", relationship = "many-to-one") %>%
  mutate(
    unit_increase_included = coalesce(as.integer(unit_increase_included), 0L)
  )
setDT(permits_2020)
permits_2020[, `:=`(
  is_new_construction_issued = as.integer(permit_type == "PERMIT - NEW CONSTRUCTION"),
  is_demolition_issued = as.integer(grepl("WRECKING|DEMOLITION", permit_type)),
  is_renovation_issued = as.integer(permit_type == "PERMIT - RENOVATION/ALTERATION"),
  is_new_construction_demolition_issued = as.integer(
    permit_type == "PERMIT - NEW CONSTRUCTION" | grepl("WRECKING|DEMOLITION", permit_type)
  ),
  is_low_discretion_nosigns_issued = as.integer(high_discretion == 0 & permit_type != signs_permit_type),
  is_high_discretion_issued = as.integer(high_discretion == 1),
  is_unit_increase_issued = as.integer(unit_increase_included == 1)
)]

counts_2010 <- aggregate_outcome_counts(permits_2010)
counts_2020 <- aggregate_outcome_counts(permits_2020)

base_2015 <- prepare_cohort_base(
  blocks_sf = blocks_2010,
  treatment_df = treatment_panel %>% filter(cohort == "2015"),
  cohort_label = "2015",
  era_label = "2003_2014"
)

base_2023 <- prepare_cohort_base(
  blocks_sf = blocks_2020,
  treatment_df = treatment_panel %>% filter(cohort == "2023"),
  cohort_label = "2023",
  era_label = "2015_2023"
)

cohort_2015 <- build_panel_with_counts(
  base_df = base_2015,
  start_year = permit_year_min,
  end_year = permit_year_max,
  cohort_year = 2015L,
  counts_dt = counts_2010
)

cohort_2023 <- build_panel_with_counts(
  base_df = base_2023,
  start_year = permit_year_min,
  end_year = permit_year_max,
  cohort_year = 2023L,
  counts_dt = counts_2020
)

permit_panel <- bind_rows(cohort_2015, cohort_2023)

event_rows_1000ft <- permit_panel %>%
  mutate(
    segment_era = case_when(
      cohort == "2015" ~ "2003_2014",
      cohort == "2023" ~ "2015_2023",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    segment_pair_lookup,
    by = c("segment_era", "segment_id_cohort"),
    relationship = "many-to-one"
  ) %>%
  filter(dist_m <= 304.8, relative_year >= -5L, relative_year <= 5L) %>%
  mutate(
    expected_cohort_block_id = paste(cohort, block_id, sep = "_"),
    expected_cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_"),
    expected_cohort_segment = if_else(
      !is.na(segment_id_cohort),
      paste(cohort, segment_id_cohort, sep = "_"),
      NA_character_
    ),
    ward_pair_id = normalize_pair_dash(ward_pair_id),
    ward_origin_text = as.character(suppressWarnings(as.integer(ward_origin))),
    event_pair_contains_origin = !is.na(ward_origin_text) & (
      str_starts(ward_pair_id, paste0(ward_origin_text, "-")) |
        str_ends(ward_pair_id, paste0("-", ward_origin_text))
    ),
    treated_pair_id = normalize_pair_id(ward_origin, ward_dest, sep = "-"),
    treated_pair_matches_origin_dest = treat != 1L | ward_pair_id == treated_pair_id,
    segment_pair_matches_event_pair = is.na(segment_id_cohort) |
      (!is.na(segment_pair_id) & normalize_pair_dash(segment_pair_id) == ward_pair_id)
  )

if (any(is.na(event_rows_1000ft$strictness_change))) {
  stop("Permit event panel has missing strictness changes inside the 1000ft regression window.", call. = FALSE)
}
if (any(event_rows_1000ft$cohort_block_id != event_rows_1000ft$expected_cohort_block_id, na.rm = TRUE) ||
    any(event_rows_1000ft$cohort_ward_pair != event_rows_1000ft$expected_cohort_ward_pair, na.rm = TRUE) ||
    any(
      !is.na(event_rows_1000ft$segment_id_cohort) &
        (is.na(event_rows_1000ft$cohort_segment) |
          event_rows_1000ft$cohort_segment != event_rows_1000ft$expected_cohort_segment),
      na.rm = TRUE
    )) {
  stop("Permit event panel has invalid cohort-prefixed FE identifiers.", call. = FALSE)
}
if (any(!event_rows_1000ft$event_pair_contains_origin, na.rm = TRUE) ||
    any(!event_rows_1000ft$treated_pair_matches_origin_dest, na.rm = TRUE) ||
    any(is.na(event_rows_1000ft$segment_id_cohort) | event_rows_1000ft$segment_id_cohort == "") ||
    any(!event_rows_1000ft$segment_pair_matches_event_pair, na.rm = TRUE)) {
  stop("Permit event panel failed the 1000ft event-pair/segment contract.", call. = FALSE)
}

write_parquet(permit_panel, "../output/permit_block_year_panel.parquet")
write_parquet(cohort_2015, "../output/permit_block_year_panel_2015.parquet")
write_parquet(cohort_2023, "../output/permit_block_year_panel_2023.parquet")
