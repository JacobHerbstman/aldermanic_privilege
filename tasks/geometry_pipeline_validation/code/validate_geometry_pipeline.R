# validate_geometry_pipeline.R
# Builds geometry validation outputs comparing canonical geometry outputs
# against pre-fix snapshots for the active analysis branch.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/geometry_pipeline_validation/code")

source("../../setup_environment/code/packages.R")

normalize_pair_id_local <- function(x) {
  x <- as.character(x)
  x <- gsub("-", "_", x, fixed = TRUE)
  x <- trimws(x)
  out <- rep(NA_character_, length(x))
  ok <- grepl("^[0-9]+_[0-9]+$", x)
  if (!any(ok)) {
    return(out)
  }
  parts <- strsplit(x[ok], "_", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) {
      return(NA_character_)
    }
    paste(min(a, b), max(a, b), sep = "_")
  }, character(1))
  out
}

extract_density_table_row <- function(path_tex) {
  lines <- readLines(path_tex, warn = FALSE)
  row_idx <- grep("^\\s*Stringency Index", lines)
  if (length(row_idx) == 0) {
    stop(sprintf("Could not find Stringency Index row in %s", path_tex), call. = FALSE)
  }
  obs_idx <- grep("^\\s*Observations", lines)
  pairs_idx <- grep("^\\s*Ward Pairs", lines)
  if (length(obs_idx) == 0 || length(pairs_idx) == 0) {
    stop(sprintf("Could not find observation rows in %s", path_tex), call. = FALSE)
  }

  parse_cells <- function(line) {
    str_split(line, "&", simplify = TRUE) |>
      as.character() |>
      trimws() |>
      gsub("\\\\\\\\.*$", "", x = _, perl = TRUE)
  }

  est_cells <- parse_cells(lines[row_idx[1]])
  se_cells <- parse_cells(lines[row_idx[1] + 1])
  obs_cells <- parse_cells(lines[obs_idx[1]])
  pair_cells <- parse_cells(lines[pairs_idx[1]])

  tibble(
    outcome = c("far", "dupac", "units"),
    estimate_display = est_cells[2:4],
    se_display = se_cells[2:4],
    n_obs = as.numeric(gsub("[^0-9.\\-]", "", obs_cells[2:4])),
    n_pairs = as.numeric(gsub("[^0-9.\\-]", "", pair_cells[2:4]))
  )
}

extract_density_meta <- function(path_csv, label) {
  d <- read_csv(path_csv, show_col_types = FALSE)
  d |>
    transmute(
      source = label,
      yvar,
      n_obs = n_obs,
      n_pairs = n_pairs,
      rd_jump_estimate,
      rd_jump_se,
      rd_jump_p
    )
}

read_tabular_geometry_input <- function(path) {
  if (grepl("\\.parquet$", path, ignore.case = TRUE)) {
    return(read_parquet(path))
  }
  read_csv(path, show_col_types = FALSE, progress = FALSE)
}

pick_col <- function(df, candidates, required = TRUE) {
  hit <- intersect(candidates, names(df))
  if (length(hit) > 0) {
    return(hit[[1]])
  }
  if (!required) {
    return(NA_character_)
  }
  stop(
    sprintf(
      "Could not find any of the required columns: %s",
      paste(candidates, collapse = ", ")
    ),
    call. = FALSE
  )
}

pair_universe_from_pre_scores <- function(path, dataset_name, era_col, pair_col) {
  if (!file.exists(path)) {
    return(tibble(dataset = character(), era = character(), ward_pair_id = character(), n_obs = integer()))
  }
  d <- read_tabular_geometry_input(path)
  if (!(era_col %in% names(d)) || !(pair_col %in% names(d))) {
    return(tibble(dataset = character(), era = character(), ward_pair_id = character(), n_obs = integer()))
  }
  d |>
    mutate(
      era = case_when(
        era_col == "boundary_year" ~ case_when(
          .data[[era_col]] == 1998 ~ "1998_2002",
          .data[[era_col]] == 2003 ~ "2003_2014",
          .data[[era_col]] == 2015 ~ "2015_2023",
          .data[[era_col]] == 2024 ~ "post_2023",
          TRUE ~ NA_character_
        ),
        TRUE ~ as.character(.data[[era_col]])
      ),
      ward_pair_id = normalize_pair_id_local(.data[[pair_col]])
    ) |>
    filter(!is.na(era), !is.na(ward_pair_id)) |>
    count(era, ward_pair_id, name = "n_obs") |>
    mutate(dataset = dataset_name) |>
    select(dataset, era, ward_pair_id, n_obs)
}

current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

dir.create("../output", showWarnings = FALSE, recursive = TRUE)

boundary_layers <- st_layers("../input/ward_pair_boundaries.gpkg")$name
segment_layers <- st_layers("../input/boundary_segments_1320ft.gpkg")$name

canonical_pair_universe <- bind_rows(lapply(boundary_layers, function(layer_name) {
  d <- st_read("../input/ward_pair_boundaries.gpkg", layer = layer_name, quiet = TRUE)
  tibble(
    source = "boundary_lines",
    era = layer_name,
    ward_pair_id = normalize_pair_id_local(d$ward_pair_id),
    shared_length_ft = as.numeric(d$shared_length_ft)
  )
})) |>
  filter(!is.na(ward_pair_id)) |>
  arrange(era, ward_pair_id)

segment_pair_universe <- bind_rows(lapply(segment_layers, function(layer_name) {
  d <- st_read("../input/boundary_segments_1320ft.gpkg", layer = layer_name, quiet = TRUE)
  tibble(
    source = "segments",
    era = sub("_bw.*$", "", layer_name),
    layer = layer_name,
    ward_pair_id = normalize_pair_id_local(d$ward_pair_id),
    segment_id = as.character(d$segment_id)
  )
})) |>
  filter(!is.na(ward_pair_id))

write_csv(canonical_pair_universe, "../output/canonical_pair_universe.csv")

parcel_pairs <- pair_universe_from_pre_scores("../input/parcels_pre_scores.csv", "parcels", "boundary_year", "ward_pair_id")
sales_pairs <- pair_universe_from_pre_scores("../input/sales_pre_scores.csv", "sales", "boundary_year", "ward_pair_id")
rent_pairs <- pair_universe_from_pre_scores("../input/rent_pre_scores_full.parquet", "rental", "boundary_year", "ward_pair_id")

canonical_pairs_only <- canonical_pair_universe |>
  distinct(era, ward_pair_id)
segment_pairs_only <- segment_pair_universe |>
  distinct(era, ward_pair_id)
dataset_pairs <- bind_rows(parcel_pairs, sales_pairs, rent_pairs)

geometry_pair_mismatch_report <- bind_rows(
  anti_join(dataset_pairs, canonical_pairs_only, by = c("era", "ward_pair_id")) |>
    mutate(reason = "dataset_pair_missing_from_canonical"),
  anti_join(canonical_pairs_only, segment_pairs_only, by = c("era", "ward_pair_id")) |>
    mutate(dataset = "segments", n_obs = NA_integer_, reason = "canonical_pair_missing_from_segments"),
  anti_join(segment_pairs_only, canonical_pairs_only, by = c("era", "ward_pair_id")) |>
    mutate(dataset = "segments", n_obs = NA_integer_, reason = "segment_pair_missing_from_canonical")
) |>
  arrange(reason, dataset, era, ward_pair_id)

write_csv(geometry_pair_mismatch_report, "../output/geometry_pair_mismatch_report.csv")

parcel_coverage_current <- read_csv("../input/parcel_segment_ids_coverage.csv", show_col_types = FALSE) |>
  mutate(dataset = "parcel_current")
parcel_coverage_before <- read_csv("../input/before_fix/parcel_segment_ids_coverage.csv", show_col_types = FALSE) |>
  mutate(dataset = "parcel_before_fix")

segment_coverage_current <- if (file.exists("../input/segment_assignment_coverage_summary.csv")) {
  read_csv("../input/segment_assignment_coverage_summary.csv", show_col_types = FALSE) |>
    mutate(dataset = paste0(dataset, "_current"))
} else {
  tibble()
}

segment_coverage_before <- read_csv("../input/before_fix/segment_assignment_coverage_summary.csv", show_col_types = FALSE) |>
  mutate(dataset = paste0(dataset, "_before_fix"))

geometry_coverage_summary <- bind_rows(
  parcel_coverage_current,
  parcel_coverage_before,
  segment_coverage_current,
  segment_coverage_before
)

write_csv(geometry_coverage_summary, "../output/geometry_coverage_summary.csv")

current_density_table <- extract_density_table_row("../input/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex") |>
  mutate(source = "current")
before_density_table <- extract_density_table_row("../input/before_fix/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex") |>
  mutate(source = "before_fix")

density_result_change <- full_join(
  before_density_table,
  current_density_table,
  by = "outcome",
  suffix = c("_before", "_current")
) |>
  transmute(
    result_family = "density_fe_main_500_multifamily",
    outcome,
    estimate_before = estimate_display_before,
    estimate_current = estimate_display_current,
    n_obs_before = n_obs_before,
    n_obs_current = n_obs_current,
    n_pairs_before = n_pairs_before,
    n_pairs_current = n_pairs_current
  )

if (file.exists("../input/fe_table_rental_bw500_pre_2023.csv")) {
  rental_before <- read_csv("../input/before_fix/fe_table_rental_bw500_pre_2023.csv", show_col_types = FALSE)
  rental_current <- read_csv("../input/fe_table_rental_bw500_pre_2023.csv", show_col_types = FALSE)
  rental_change <- full_join(
    rental_before,
    rental_current,
    by = "specification",
    suffix = c("_before", "_current")
  ) |>
    transmute(
      result_family = "rental_fe_main_500",
      outcome = specification,
      estimate_before = as.character(round(estimate_before, 4)),
      estimate_current = as.character(round(estimate_current, 4)),
      n_obs_before = n_obs_before,
      n_obs_current = n_obs_current,
      n_pairs_before = ward_pairs_before,
      n_pairs_current = ward_pairs_current
    )
} else {
  rental_change <- tibble()
}

if (file.exists("../input/fe_table_sales_bw500_year_quarter.csv")) {
  sales_before <- read_csv("../input/before_fix/fe_table_sales_bw500_year_quarter.csv", show_col_types = FALSE)
  sales_current <- read_csv("../input/fe_table_sales_bw500_year_quarter.csv", show_col_types = FALSE)
  sales_change <- full_join(
    sales_before,
    sales_current,
    by = "specification",
    suffix = c("_before", "_current")
  ) |>
    transmute(
      result_family = "sales_fe_main_500",
      outcome = specification,
      estimate_before = as.character(round(estimate_before, 4)),
      estimate_current = as.character(round(estimate_current, 4)),
      n_obs_before = n_obs_before,
      n_obs_current = n_obs_current,
      n_pairs_before = ward_pairs_before,
      n_pairs_current = ward_pairs_current
    )
} else {
  sales_change <- tibble()
}

geometry_result_change_summary <- bind_rows(
  density_result_change,
  rental_change,
  sales_change
)

write_csv(geometry_result_change_summary, "../output/geometry_result_change_summary.csv")

geometry_sample_impact_summary <- geometry_result_change_summary |>
  mutate(
    delta_n_obs = n_obs_current - n_obs_before,
    delta_n_pairs = n_pairs_current - n_pairs_before
  )

write_csv(geometry_sample_impact_summary, "../output/geometry_sample_impact_summary.csv")

if (file.exists("../input/parcels_with_ward_distances.csv")) {
  parcel_df <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE)
  signed_col <- pick_col(parcel_df, c("signed_distance", "signed_dist"))
  dist_col <- pick_col(parcel_df, c("dist_to_boundary", "dist_ft"))
  pair_col <- pick_col(parcel_df, c("ward_pair", "ward_pair_id"))
  neighbor_col <- pick_col(parcel_df, c("other_ward", "neighbor_ward"))
  score_col <- pick_col(parcel_df, c("uncertainty_index", "strictness_own"), required = FALSE)
  geometry_spotcheck_queue <- parcel_df |>
    filter(abs(.data[[signed_col]]) <= 1000) |>
    mutate(segment_missing = is.na(segment_id)) |>
    arrange(desc(segment_missing), abs(.data[[signed_col]])) |>
    transmute(
      pin = pin,
      ward = ward,
      neighbor_ward = .data[[neighbor_col]],
      ward_pair_id = .data[[pair_col]],
      boundary_year = boundary_year,
      signed_dist = .data[[signed_col]],
      dist_ft = .data[[dist_col]],
      segment_id = segment_id,
      uncertainty_index = if (!is.na(score_col)) .data[[score_col]] else NA_real_,
      segment_missing = segment_missing
    ) |>
    slice_head(n = 200)
} else {
  geometry_spotcheck_queue <- tibble()
}

write_csv(geometry_spotcheck_queue, "../output/geometry_spotcheck_queue.csv")

density_meta_current <- bind_rows(
  extract_density_meta("../output/validation_density_far_meta.csv", "current"),
  extract_density_meta("../output/validation_density_dupac_meta.csv", "current")
)

report_lines <- c(
  "# Geometry Validation Report",
  "",
  paste0("Generated: ", current_time),
  "",
  "## Canonical Pair Universe",
  paste0("- Canonical boundary pair-eras: ", nrow(canonical_pairs_only)),
  paste0("- Canonical segment pair-eras: ", nrow(segment_pairs_only)),
  paste0("- Pair-era mismatches still present: ", nrow(geometry_pair_mismatch_report)),
  "",
  "## Parcel Coverage",
  paste0(
    "- Current parcel segment coverage at 500 ft: ",
    scales::percent(
      geometry_coverage_summary |>
        filter(dataset == 'parcel_current', scope == 'regression_bw500', era == 'all') |>
        pull(coverage_rate) |>
        first(),
      accuracy = 0.1
    )
  ),
  paste0(
    "- Current parcel segment coverage at 1000 ft: ",
    scales::percent(
      geometry_coverage_summary |>
        filter(dataset == 'parcel_current', scope == 'regression_bw1000', era == 'all') |>
        pull(coverage_rate) |>
        first(),
      accuracy = 0.1
    )
  ),
  "",
  "## Headline Density Change",
  paste0(
    "- FE table FAR changed from ",
    density_result_change |> filter(outcome == "far") |> pull(estimate_before) |> first(),
    " to ",
    density_result_change |> filter(outcome == "far") |> pull(estimate_current) |> first()
  ),
  paste0(
    "- FE table DUPAC changed from ",
    density_result_change |> filter(outcome == "dupac") |> pull(estimate_before) |> first(),
    " to ",
    density_result_change |> filter(outcome == "dupac") |> pull(estimate_current) |> first()
  ),
  paste0(
    "- FE table Units changed from ",
    density_result_change |> filter(outcome == "units") |> pull(estimate_before) |> first(),
    " to ",
    density_result_change |> filter(outcome == "units") |> pull(estimate_current) |> first()
  ),
  paste0(
    "- RD FAR jump (current meta): ",
    round(density_meta_current |> filter(yvar == "density_far") |> pull(rd_jump_estimate) |> first(), 3)
  ),
  paste0(
    "- RD DUPAC jump (current meta): ",
    round(density_meta_current |> filter(yvar == "density_dupac") |> pull(rd_jump_estimate) |> first(), 3)
  ),
  "",
  "## Remaining Risk",
  if (nrow(geometry_pair_mismatch_report) == 0) {
    "- No pair-era mismatches remain across canonical lines, segments, and current pre-score files."
  } else {
    paste0("- Remaining pair-era mismatches require follow-up; see geometry_pair_mismatch_report.csv (", nrow(geometry_pair_mismatch_report), " rows).")
  },
  if (file.exists("../input/segment_assignment_coverage_summary.csv")) {
    paste0(
      "- Sales/rental segment coverage summary is available in geometry_coverage_summary.csv."
    )
  } else {
    "- Sales/rental segment coverage summary is not yet available; rerun validation after assign_segment_ids_sales_rental completes."
  }
)

writeLines(report_lines, "../output/geometry_validation_report.md")
