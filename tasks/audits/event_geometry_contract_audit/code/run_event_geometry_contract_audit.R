# Event-study geometry contract audit.
# Checks that permit and sales event-study panels use cohort-specific boundary
# geometry consistently.
#
# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/event_geometry_contract_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

panel_window_m <- 800
analysis_window_m <- 304.8

segment_layers <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg")
segment_lookup <- bind_rows(lapply(names(segment_layers), function(era_i) {
  segment_layers[[era_i]] %>%
    st_drop_geometry() %>%
    transmute(
      era = era_i,
      segment_id = as.character(segment_id),
      segment_pair_id = normalize_pair_dash(ward_pair_id)
    )
}))

add_flag <- function(flags, dataset, cohort, check, value, status, detail) {
  bind_rows(
    flags,
    tibble(
      dataset = dataset,
      cohort = cohort,
      check = check,
      value = as.numeric(value),
      status = status,
      detail = detail
    )
  )
}

normal_pair_from_wards <- function(ward_origin, ward_dest) {
  normalize_pair_id(ward_origin, ward_dest, sep = "-")
}

pair_contains_ward <- function(pair_id, ward) {
  pair_id <- normalize_pair_dash(pair_id)
  ward <- suppressWarnings(as.integer(ward))
  pair_a <- suppressWarnings(as.integer(sub("-.*$", "", pair_id)))
  pair_b <- suppressWarnings(as.integer(sub("^.*-", "", pair_id)))
  !is.na(pair_id) & !is.na(ward) & (ward == pair_a | ward == pair_b)
}

cohort_segment_era <- function(cohort) {
  case_when(
    cohort %in% c("2012", "2015") ~ "2003_2014",
    cohort %in% c("2022", "2023") ~ "2015_2023",
    TRUE ~ NA_character_
  )
}

summarize_panel <- function(path, dataset, cohort, treat_col) {
  df <- arrow::read_parquet(path) %>% as_tibble()
  required_cols <- c(
    "ward_pair_id", "ward_origin", "ward_dest", "event_neighbor_ward", "dist_m",
    treat_col
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "%s %s panel is missing required geometry columns: %s",
      dataset, cohort, paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }

  has_segment_assignment <- "segment_id_cohort" %in% names(df)

  df <- df %>%
    mutate(
      ward_pair_id_norm = normalize_pair_dash(ward_pair_id),
      switched = .data[[treat_col]] == TRUE,
      expected_switched_pair = normal_pair_from_wards(ward_origin, ward_dest),
      expected_event_pair = normal_pair_from_wards(ward_origin, event_neighbor_ward),
      segment_era = cohort_segment_era(cohort)
    )

  if (has_segment_assignment) {
    df <- df %>%
      left_join(
        segment_lookup,
        by = c("segment_era" = "era", "segment_id_cohort" = "segment_id"),
        relationship = "many-to-one"
      )
  } else {
    df <- df %>%
      mutate(segment_id_cohort = NA_character_, segment_pair_id = NA_character_)
  }

  switched_rows <- df %>% filter(switched)
  control_rows <- df %>% filter(!switched)
  in_analysis_window <- df %>% filter(dist_m <= analysis_window_m)

  tibble(
    dataset = dataset,
    cohort = cohort,
    rows = nrow(df),
    rows_le_1000ft = nrow(in_analysis_window),
    max_dist_m = max(df$dist_m, na.rm = TRUE),
    missing_dist = sum(is.na(df$dist_m)),
    negative_dist = sum(!is.na(df$dist_m) & df$dist_m < 0),
    missing_pair = sum(is.na(df$ward_pair_id_norm)),
    missing_origin = sum(is.na(df$ward_origin)),
    missing_dest = sum(is.na(df$ward_dest)),
    missing_event_neighbor = sum(is.na(df$event_neighbor_ward)),
    control_dest_not_origin = sum(
      control_rows$ward_dest != control_rows$ward_origin,
      na.rm = TRUE
    ),
    ward_pair_missing_origin = sum(!pair_contains_ward(df$ward_pair_id_norm, df$ward_origin)),
    event_neighbor_pair_mismatch = sum(
      df$ward_pair_id_norm != df$expected_event_pair,
      na.rm = TRUE
    ),
    switched_pair_mismatch = sum(
      switched_rows$ward_pair_id_norm != switched_rows$expected_switched_pair,
      na.rm = TRUE
    ),
    missing_segment_le_1000ft = if (has_segment_assignment) {
      sum(is.na(in_analysis_window$segment_id_cohort) | in_analysis_window$segment_id_cohort == "")
    } else {
      NA_integer_
    },
    segment_pair_mismatch_le_1000ft = if (has_segment_assignment) {
      sum(
        !is.na(in_analysis_window$segment_id_cohort) &
          in_analysis_window$segment_id_cohort != "" &
          in_analysis_window$segment_pair_id != in_analysis_window$ward_pair_id_norm,
        na.rm = TRUE
      )
    } else {
      NA_integer_
    },
    n_switched = nrow(switched_rows),
    n_pairs = n_distinct(df$ward_pair_id_norm),
    n_segments_le_1000ft = n_distinct(in_analysis_window$segment_id_cohort)
  )
}

panel_summary <- bind_rows(
  summarize_panel("../input/permit_block_year_panel_2015.parquet", "permits", "2015", "switched"),
  summarize_panel("../input/sales_transaction_panel_2015.parquet", "sales", "2015", "treat"),
  summarize_panel("../input/sales_transaction_panel_2023.parquet", "sales", "2023", "treat")
)

flags <- tibble(
  dataset = character(),
  cohort = character(),
  check = character(),
  value = numeric(),
  status = character(),
  detail = character()
)

for (i in seq_len(nrow(panel_summary))) {
  row_i <- panel_summary[i, ]
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "row_count_positive", row_i$rows,
    if_else(row_i$rows > 0, "pass", "fail"),
    "Panel should not be empty."
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "max_dist_within_panel_window", row_i$max_dist_m,
    if_else(row_i$max_dist_m <= panel_window_m + 1e-6, "pass", "fail"),
    sprintf("Saved panel should be no wider than %.1fm.", panel_window_m)
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "missing_dist", row_i$missing_dist,
    if_else(row_i$missing_dist == 0, "pass", "fail"),
    "Event-specific distance should be present for every saved row."
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "negative_dist", row_i$negative_dist,
    if_else(row_i$negative_dist == 0, "pass", "fail"),
    "Unsigned distance should never be negative."
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "missing_pair", row_i$missing_pair,
    if_else(row_i$missing_pair == 0, "pass", "fail"),
    "Event-specific ward-pair id should be valid for every saved row."
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "missing_origin", row_i$missing_origin,
    if_else(row_i$missing_origin == 0, "pass", "fail"),
    "Event-side origin ward should be present for every saved row."
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "missing_dest", row_i$missing_dest,
    if_else(row_i$missing_dest == 0, "pass", "fail"),
    "Treatment destination ward should be present for every saved row."
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "missing_event_neighbor", row_i$missing_event_neighbor,
    if_else(row_i$missing_event_neighbor == 0, "pass", "fail"),
    "Event boundary neighbor ward should be present for every saved row."
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "control_dest_not_origin", row_i$control_dest_not_origin,
    if_else(row_i$control_dest_not_origin == 0, "pass", "fail"),
    "Non-switched controls should have treatment destination equal to origin; event_neighbor_ward stores the opposite boundary side."
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "ward_pair_missing_origin", row_i$ward_pair_missing_origin,
    if_else(row_i$ward_pair_missing_origin == 0, "pass", "fail"),
    "Every event ward-pair should contain the row's origin ward."
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "event_neighbor_pair_mismatch", row_i$event_neighbor_pair_mismatch,
    if_else(row_i$event_neighbor_pair_mismatch == 0, "pass", "fail"),
    "ward_pair_id should equal the normalized pair of ward_origin and event_neighbor_ward."
  )
  flags <- add_flag(flags, row_i$dataset, row_i$cohort, "switched_pair_mismatch", row_i$switched_pair_mismatch,
    if_else(row_i$switched_pair_mismatch == 0, "pass", "fail"),
    "Treated/switched rows must use the origin-destination ward pair, not a transaction-date nearest pair."
  )
  if (!is.na(row_i$missing_segment_le_1000ft)) {
    flags <- add_flag(flags, row_i$dataset, row_i$cohort, "missing_segment_le_1000ft", row_i$missing_segment_le_1000ft,
      if_else(row_i$missing_segment_le_1000ft == 0, "pass", "fail"),
      "Rows inside the 1000ft event-study window should have cohort segment assignment for diagnostics/robustness."
    )
    flags <- add_flag(flags, row_i$dataset, row_i$cohort, "segment_pair_mismatch_le_1000ft", row_i$segment_pair_mismatch_le_1000ft,
      if_else(row_i$segment_pair_mismatch_le_1000ft == 0, "pass", "fail"),
      "Assigned segments inside 1000ft should belong to the same event ward-pair as the row."
    )
  }
}

sales_geometry_diagnostics <- read_csv(
  "../input/sales_transaction_panel_event_geometry_diagnostics.csv",
  show_col_types = FALSE
)

sales_post_filter <- sales_geometry_diagnostics %>%
  filter(stage == "post_event_distance_filter")

diagnostic_summary <- sales_geometry_diagnostics %>%
  summarise(
    rows = sum(rows, na.rm = TRUE),
    missing_event_dist = sum(missing_event_dist, na.rm = TRUE),
    point_origin_mismatch = sum(point_origin_mismatch, na.rm = TRUE),
    old_event_1000ft_disagree = sum(old_event_1000ft_disagree, na.rm = TRUE),
    .by = c(panel_mode, stage, treat)
  )

for (i in seq_len(nrow(sales_post_filter))) {
  row_i <- sales_post_filter[i, ]
  cohort_i <- sub("^cohort_", "", row_i$panel_mode)
  treat_label <- if_else(row_i$treat == 1, "treated", "control")
  flags <- add_flag(flags, "sales", cohort_i, paste0(treat_label, "_missing_event_dist_post_filter"), row_i$missing_event_dist,
    if_else(row_i$missing_event_dist == 0, "pass", "fail"),
    "No row should remain after the event-distance filter with missing event-specific distance."
  )
  flags <- add_flag(flags, "sales", cohort_i, paste0(treat_label, "_point_origin_mismatch_post_filter"), row_i$point_origin_mismatch,
    if_else(row_i$point_origin_mismatch == 0, "pass", "fail"),
    "No retained transaction should sit in a different pre-redistricting ward than its treated/control origin ward."
  )
}

write_csv(panel_summary, "../output/event_geometry_contract_summary.csv")
write_csv(flags, "../output/event_geometry_contract_flags.csv")

failures <- flags %>% filter(status == "fail")

memo_lines <- c(
  "# Event Geometry Contract Audit",
  "",
  sprintf("Panel window checked: %.1fm. Analysis window checked: %.1fm (1000ft).", panel_window_m, analysis_window_m),
  "",
  "## Panel Summary",
  "",
  paste(capture.output(print(panel_summary, n = Inf)), collapse = "\n"),
  "",
  "## Sales Event-Geometry Diagnostics",
  "",
  paste(capture.output(print(diagnostic_summary, n = Inf)), collapse = "\n"),
  "",
  "## Flags",
  "",
  paste(capture.output(print(flags, n = Inf)), collapse = "\n")
)

if (nrow(failures) > 0) {
  memo_lines <- c(
    memo_lines,
    "",
    "## Failures",
    "",
    paste(capture.output(print(failures, n = Inf)), collapse = "\n")
  )
}

writeLines(memo_lines, "../output/event_geometry_contract_memo.md")

if (nrow(failures) > 0) {
  stop(sprintf("Event geometry contract audit failed with %d failed checks.", nrow(failures)), call. = FALSE)
}

message("Event geometry contract audit passed.")
