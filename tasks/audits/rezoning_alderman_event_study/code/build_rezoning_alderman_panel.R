# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_alderman_event_study/code")
# score_window <- "through2022"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(score_window)
}
if (length(cli_args) != 1) {
  stop("Script requires a score window.", call. = FALSE)
}
score_window <- cli_args[1]
if (!score_window %in% c("through2022", "pre2015")) {
  stop("Score window must be through2022 or pre2015.", call. = FALSE)
}

boundary_source <- read_parquet("../input/permit_block_year_panel_2015.parquet")
pre_permit_history <- boundary_source %>%
  filter(year >= 2011L, year <= 2013L) %>%
  group_by(block_id) %>%
  summarise(
    pre_new_construction_count = sum(n_new_construction_issue),
    pre_high_discretion_count = sum(n_high_discretion_issue),
    .groups = "drop"
  )
boundary_panel <- boundary_source %>%
  select(block_id, year, ward_pair_id, dist_m)
if (anyDuplicated(boundary_panel[, c("block_id", "year")]) > 0) {
  stop("The 2015 event-study block panel must be unique by block-year.", call. = FALSE)
}

if (score_window == "pre2015") {
  block_treatment <- read_parquet("../input/corrected_permit_block_year_panel.parquet") %>%
    filter(as.character(cohort) == "2015", valid, year == 2014L) %>%
    distinct(
      block_id, ward_origin, ward_dest, switched,
      strictness_origin_2014, strictness_dest_2014,
      assigned_change_2014, switch_type
    ) %>%
    transmute(
      block_id = as.character(block_id),
      ward_origin, ward_dest, switched,
      treat = as.integer(switched),
      strictness_origin = strictness_origin_2014,
      strictness_dest = strictness_dest_2014,
      strictness_change = assigned_change_2014,
      switch_type
    )
} else {
  block_treatment <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE) %>%
    filter(as.character(cohort) == "2015", valid, has_complete_ward_assignment) %>%
    transmute(
      block_id = as.character(block_id),
      ward_origin, ward_dest, switched,
      treat = as.integer(switched),
      strictness_origin, strictness_dest, strictness_change, switch_type
    )
}
if (anyDuplicated(block_treatment$block_id) > 0) {
  stop("The 2015 treatment panel must be unique by block.", call. = FALSE)
}

permit_panel <- block_treatment %>%
  tidyr::crossing(year = seq(min(boundary_panel$year), max(boundary_panel$year))) %>%
  mutate(relative_year = year - 2015L) %>%
  left_join(boundary_panel, by = c("block_id", "year"), relationship = "one-to-one") %>%
  left_join(pre_permit_history, by = "block_id", relationship = "many-to-one") %>%
  mutate(
    pre_new_construction_count = if_else(
      !is.na(dist_m), replace_na(pre_new_construction_count, 0), NA_real_
    ),
    pre_high_discretion_count = if_else(
      !is.na(dist_m), replace_na(pre_high_discretion_count, 0), NA_real_
    )
  )
if (anyDuplicated(permit_panel[, c("block_id", "year")]) > 0) {
  stop("The citywide 2015 event-study panel must be unique by block-year.", call. = FALSE)
}

rezonings <- read_csv(
  "../input/rezoning_census_blocks_20101101_20201231.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(
    matter_id,
    year = as.integer(format(as.Date(matter_passed_date), "%Y")),
    far_pair_status,
    far_change = as.numeric(far_change),
    is_upzone = tolower(is_upzone) == "true"
  )
if (anyDuplicated(rezonings$matter_id) > 0) {
  stop("Rezoning matters must be unique by matter_id.", call. = FALSE)
}

bridge <- read_csv(
  "../input/rezoning_matter_block_bridge_20101101_20201231.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(matter_id, block_id = census_block_id)
if (anyDuplicated(bridge[, c("matter_id", "block_id")]) > 0) {
  stop("Rezoning matter-block bridge contains duplicate pairs.", call. = FALSE)
}

rezoning_block_year <- bridge %>%
  left_join(rezonings, by = "matter_id", relationship = "many-to-one") %>%
  filter(!is.na(year)) %>%
  group_by(block_id, year) %>%
  summarise(
    n_rezoning = n_distinct(matter_id),
    n_classified = n_distinct(matter_id[far_pair_status == "resolved_both"]),
    all_rezonings_classified = all(far_pair_status == "resolved_both"),
    n_upzone = n_distinct(matter_id[far_pair_status == "resolved_both" & is_upzone]),
    far_change_total_observed = sum(far_change[far_pair_status == "resolved_both"], na.rm = TRUE),
    mean_far_change_observed = mean(far_change[far_pair_status == "resolved_both"], na.rm = TRUE),
    upzone_share_observed = mean(is_upzone[far_pair_status == "resolved_both"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_far_change_observed = if_else(is.nan(mean_far_change_observed), NA_real_, mean_far_change_observed),
    upzone_share_observed = if_else(is.nan(upzone_share_observed), NA_real_, upzone_share_observed)
  )
if (anyDuplicated(rezoning_block_year[, c("block_id", "year")]) > 0) {
  stop("Rezoning outcomes must be unique by block-year.", call. = FALSE)
}

pre_rezoning_history <- rezoning_block_year %>%
  filter(year >= 2011L, year <= 2013L) %>%
  group_by(block_id) %>%
  summarise(
    pre_rezoning_count = sum(n_rezoning),
    pre_upzone_count = sum(n_upzone),
    pre_far_change_total = sum(far_change_total_observed),
    pre_unclassified_count = sum(n_rezoning - n_classified),
    .groups = "drop"
  )

panel <- permit_panel %>%
  left_join(rezoning_block_year, by = c("block_id", "year"), relationship = "one-to-one") %>%
  left_join(pre_rezoning_history, by = "block_id", relationship = "many-to-one") %>%
  mutate(
    across(
      c(pre_rezoning_count, pre_upzone_count, pre_far_change_total, pre_unclassified_count),
      ~replace_na(.x, 0)
    ),
    any_rezoning = as.integer(coalesce(n_rezoning, 0L) > 0L),
    any_upzone = case_when(
      is.na(n_rezoning) ~ 0,
      n_upzone > 0L ~ 1,
      all_rezonings_classified ~ 0,
      TRUE ~ NA_real_
    ),
    far_change_total = case_when(
      is.na(n_rezoning) ~ 0,
      all_rezonings_classified ~ far_change_total_observed,
      TRUE ~ NA_real_
    ),
    mean_far_change = if_else(all_rezonings_classified, mean_far_change_observed, NA_real_),
    upzone_share = if_else(all_rezonings_classified, upzone_share_observed, NA_real_)
  ) %>%
  select(
    block_id, year, relative_year, ward_origin, ward_dest, switched, treat,
    strictness_origin, strictness_dest, strictness_change, switch_type,
    ward_pair_id, dist_m, n_rezoning, all_rezonings_classified,
    pre_rezoning_count, pre_upzone_count, pre_far_change_total, pre_unclassified_count,
    pre_new_construction_count, pre_high_discretion_count,
    any_rezoning, any_upzone, far_change_total, mean_far_change, upzone_share
  ) %>%
  arrange(block_id, year)

if (nrow(panel) != nrow(permit_panel)) {
  stop("Joining rezoning outcomes changed the 2015 event-study panel row count.", call. = FALSE)
}

if (score_window == "pre2015") {
  write_parquet(panel, "../output/rezoning_alderman_block_year_panel_pre2015.parquet")
} else {
  write_parquet(panel, "../output/rezoning_alderman_block_year_panel.parquet")
}
