library(arrow)
library(data.table)
library(dplyr)
library(fixest)
library(fs)
library(purrr)
library(readr)
library(sf)
library(stringr)
library(tibble)
library(tidyr)
library(withr)

source("../../../_lib/canonical_geometry_helpers.R")
source("../../../_lib/border_pair_helpers.R")

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/manuscript_verification_audit/code")
# source("run_hostile_replication_audit.R")

repo_root <- normalizePath("../../../..", mustWork = TRUE)
task_root <- normalizePath("..", mustWork = TRUE)
output_dir <- file.path(task_root, "output")
dir_create(output_dir)

base_findings_path <- file.path(output_dir, "findings_ledger.csv")
base_artifact_map_path <- file.path(output_dir, "artifact_map.csv")
base_coverage_path <- file.path(output_dir, "coverage_matrix.csv")

hostile_claim_ledger_path <- file.path(output_dir, "hostile_claim_ledger.csv")
hostile_artifact_map_path <- file.path(output_dir, "claim_artifact_map_hostile.csv")
package_integrity_path <- file.path(output_dir, "package_integrity_findings.csv")
release_checklist_path <- file.path(output_dir, "replication_package_release_checklist.md")
result_scoreboard_path <- file.path(output_dir, "result_stability_scoreboard.csv")
challenge_results_path <- file.path(output_dir, "challenge_results.csv")
density_challenge_path <- file.path(output_dir, "density_challenge_results.csv")
permit_challenge_path <- file.path(output_dir, "permit_challenge_results.csv")
sales_challenge_path <- file.path(output_dir, "sales_challenge_results.csv")
density_balance_path <- file.path(output_dir, "density_balance_tests.csv")
density_sample_ladder_path <- file.path(output_dir, "density_sample_ladder.csv")
permit_sample_ladder_path <- file.path(output_dir, "permit_sample_ladder.csv")
sales_sample_ladder_path <- file.path(output_dir, "sales_sample_ladder.csv")
density_sidecar_inventory_path <- file.path(output_dir, "density_sidecar_inventory.csv")
data_integrity_checks_path <- file.path(output_dir, "data_integrity_checks.csv")
density_influence_pair_path <- file.path(output_dir, "density_influence_ward_pair.csv")
density_influence_segment_path <- file.path(output_dir, "density_influence_segment.csv")
sales_influence_pair_path <- file.path(output_dir, "sales_influence_ward_pair.csv")
sales_influence_block_path <- file.path(output_dir, "sales_influence_block.csv")
red_team_memo_path <- file.path(output_dir, "hostile_red_team_memo.md")
upstream_spatial_verdicts_path <- file.path(repo_root, "tasks/audits/upstream_spatial_red_team_audit/output/upstream_branch_verdicts.csv")

critical_tasks <- c(
  "summary_stats_new_construction",
  "permit_summary_stats",
  "create_alderman_uncertainty_index",
  "strictness_score_map",
  "within_ward_strictness",
  "uncertainty_validation_checks",
  "border_pair_FE_regressions",
  "nonparametric_rd_density_linear_display",
  "nonparametric_rd_density_donut",
  "nonparametric_rd_density_placebo",
  "nonparametric_rd_density_gap_split",
  "pruned_boundary_maps",
  "event_study_treatment_maps",
  "create_event_study_permit_data",
  "run_event_study_permit",
  "create_event_study_sales_data_disaggregate",
  "run_event_study_sales_disaggregate",
  "merge_event_study_scores",
  "create_block_treatment_panel",
  "merge_in_scores",
  "border_segment_creation",
  "calculate_sale_distances",
  "calculate_ward_boundary_distances",
  "assign_segment_ids",
  "create_block_group_controls",
  "clean_building_permits",
  "create_alderman_data",
  "ward_panel_create",
  "census_block_2020_cleaning",
  "geocode_ccao_data"
)

makefile_targets <- c(
  "summary_stats_new_construction" = "tasks/summary_stats_new_construction/code/Makefile",
  "permit_summary_stats" = "tasks/permit_summary_stats/code/Makefile",
  "create_alderman_uncertainty_index" = "tasks/create_alderman_uncertainty_index/code/Makefile",
  "strictness_score_map" = "tasks/strictness_score_map/code/Makefile",
  "within_ward_strictness" = "tasks/within_ward_strictness/code/Makefile",
  "uncertainty_validation_checks" = "tasks/uncertainty_validation_checks/code/Makefile",
  "border_pair_FE_regressions" = "tasks/border_pair_FE_regressions/code/Makefile",
  "nonparametric_rd_density_linear_display" = "tasks/nonparametric_rd_density_linear_display/code/Makefile",
  "nonparametric_rd_density_donut" = "tasks/nonparametric_rd_density_donut/code/Makefile",
  "nonparametric_rd_density_placebo" = "tasks/nonparametric_rd_density_placebo/code/Makefile",
  "nonparametric_rd_density_gap_split" = "tasks/nonparametric_rd_density_gap_split/code/Makefile",
  "pruned_boundary_maps" = "tasks/pruned_boundary_maps/code/Makefile",
  "event_study_treatment_maps" = "tasks/event_study_treatment_maps/code/Makefile",
  "create_event_study_permit_data" = "tasks/create_event_study_permit_data/code/Makefile",
  "run_event_study_permit" = "tasks/run_event_study_permit/code/Makefile",
  "create_event_study_sales_data_disaggregate" = "tasks/create_event_study_sales_data_disaggregate/code/Makefile",
  "run_event_study_sales_disaggregate" = "tasks/run_event_study_sales_disaggregate/code/Makefile",
  "merge_event_study_scores" = "tasks/merge_event_study_scores/code/Makefile",
  "create_block_treatment_panel" = "tasks/create_block_treatment_panel/code/Makefile",
  "merge_in_scores" = "tasks/merge_in_scores/code/Makefile",
  "border_segment_creation" = "tasks/border_segment_creation/code/Makefile",
  "calculate_sale_distances" = "tasks/calculate_sale_distances/code/Makefile",
  "calculate_ward_boundary_distances" = "tasks/calculate_ward_boundary_distances/code/Makefile",
  "assign_segment_ids" = "tasks/assign_segment_ids/code/Makefile",
  "create_block_group_controls" = "tasks/create_block_group_controls/code/Makefile",
  "clean_building_permits" = "tasks/clean_building_permits/code/Makefile",
  "create_alderman_data" = "tasks/create_alderman_data/code/Makefile",
  "ward_panel_create" = "tasks/ward_panel_create/code/Makefile",
  "census_block_2020_cleaning" = "tasks/census_block_2020_cleaning/code/Makefile",
  "geocode_ccao_data" = "tasks/geocode_ccao_data/code/Makefile"
)

collapse_make_lines <- function(lines) {
  out <- character()
  buffer <- ""

  for (line in lines) {
    stripped <- sub("\\s*#.*$", "", line)
    if (!nzchar(trimws(stripped))) {
      next
    }

    if (grepl("\\\\$", stripped)) {
      buffer <- paste0(buffer, sub("\\\\$", "", stripped), " ")
    } else {
      out <- c(out, paste0(buffer, stripped))
      buffer <- ""
    }
  }

  out
}

parse_prereqs_from_makefile <- function(makefile_path) {
  lines <- collapse_make_lines(readLines(makefile_path, warn = FALSE))
  task_code_dir <- dirname(makefile_path)

  out <- list()
  for (line in lines) {
    if (str_detect(line, "^[A-Za-z0-9_.-]+\\s*(?::=|\\+=|\\?=|=)")) {
      next
    }
    if (grepl("^\\s", line) || !str_detect(line, ":")) {
      next
    }
    if (str_detect(line, "^include\\b") || str_detect(line, "^\\.PHONY\\b")) {
      next
    }

    lhs_rhs <- strsplit(line, ":", fixed = TRUE)[[1]]
    if (length(lhs_rhs) < 2) {
      next
    }

    target <- trimws(lhs_rhs[1])
    rhs <- trimws(strsplit(lhs_rhs[2], "\\|", perl = TRUE)[[1]][1])
    if (!nzchar(rhs)) {
      next
    }

    prereqs <- strsplit(rhs, "\\s+")[[1]]
    prereqs <- prereqs[nzchar(prereqs)]
    prereqs <- prereqs[!str_detect(prereqs, "\\$\\(|^@|^\\|$|^all$|^link-inputs$")]

    if (length(prereqs) == 0) {
      next
    }

    out[[length(out) + 1]] <- tibble(
      target = target,
      prereq_rel = prereqs,
      makefile = path_rel(makefile_path, repo_root),
      abs_path = path_abs(prereq_rel, start = task_code_dir)
    )
  }

  if (length(out) == 0) {
    return(tibble())
  }

  bind_rows(out)
}

null_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

git_state <- function(abs_path) {
  rel_path <- path_rel(abs_path, repo_root)

  tracked_status <- with_dir(
    repo_root,
    system2("git", c("ls-files", "--error-unmatch", "--", rel_path), stdout = TRUE, stderr = TRUE)
  )
  tracked_code <- null_coalesce(attr(tracked_status, "status"), 0L)
  if (tracked_code == 0L) {
    return("tracked")
  }

  ignored_status <- with_dir(
    repo_root,
    system2("git", c("check-ignore", "-q", "--", rel_path), stdout = FALSE, stderr = FALSE)
  )
  ignored_code <- ignored_status
  if (ignored_code == 0L) {
    return("ignored")
  }

  "untracked"
}

extract_claim_text <- function(paper_ref) {
  if (is.na(paper_ref) || !str_detect(paper_ref, ":")) {
    return(NA_character_)
  }

  parts <- strsplit(paper_ref, ":", fixed = TRUE)[[1]]
  file_rel <- parts[1]
  line_ref <- parts[2]
  file_abs <- file.path(repo_root, file_rel)

  if (!file.exists(file_abs)) {
    return(NA_character_)
  }

  lines <- readLines(file_abs, warn = FALSE)
  if (str_detect(line_ref, "-")) {
    bounds <- as.integer(strsplit(line_ref, "-", fixed = TRUE)[[1]])
    start_line <- bounds[1]
    end_line <- bounds[2]
  } else {
    start_line <- as.integer(line_ref)
    end_line <- start_line
  }

  start_line <- max(1L, start_line)
  end_line <- min(length(lines), end_line)
  text <- paste(trimws(lines[start_line:end_line]), collapse = " ")
  str_squish(text)
}

parse_density_figure_spec <- function(artifact_path) {
  figure_name <- basename(artifact_path) %>% str_remove("\\.pdf$")
  placebo <- str_starts(figure_name, "placebo_")
  core_name <- str_remove(figure_name, "^placebo_") %>% str_remove("^rd_fe_plot_")

  gap_split <- "all"
  if (str_detect(core_name, "_gap_below_median$")) {
    gap_split <- "below_median"
    core_name <- str_remove(core_name, "_gap_below_median$")
  } else if (str_detect(core_name, "_gap_above_median$")) {
    gap_split <- "above_median"
    core_name <- str_remove(core_name, "_gap_above_median$")
  }

  donut_ft <- 0
  if (str_detect(core_name, "_donut\\d+$")) {
    donut_ft <- as.numeric(str_match(core_name, "_donut(\\d+)$")[, 2])
    core_name <- str_remove(core_name, "_donut\\d+$")
  }

  placebo_shift_ft <- 0
  if (placebo) {
    placebo_shift_ft <- as.numeric(str_match(core_name, "_shift(-?\\d+)$")[, 2])
    core_name <- str_remove(core_name, "_shift-?\\d+$")
  }

  core_name <- str_remove(core_name, "_clust_ward_pair$")
  parsed <- str_match(core_name, "^(log_)?(density_[A-Za-z0-9]+)_bw(\\d+)_(all|multifamily)_(.+)$")

  if (any(is.na(parsed))) {
    stop(sprintf("Could not parse spatial RD artifact name: %s", basename(artifact_path)), call. = FALSE)
  }

  tibble(
    artifact_path = artifact_path,
    figure_name = basename(artifact_path),
    yvar = parsed[, 3],
    use_log = !is.na(parsed[, 2]) && nzchar(parsed[, 2]),
    bw_ft = as.numeric(parsed[, 4]),
    sample_filter = parsed[, 5],
    fe_spec = parsed[, 6],
    plot_style = "slope",
    gap_split = gap_split,
    donut_ft = donut_ft,
    placebo_shift_ft = placebo_shift_ft
  )
}

build_density_sidecars <- function(spatial_artifact_paths, density_sidecar_dir) {
  invisible(spatial_artifact_paths)
  invisible(density_sidecar_dir)
  tibble(
    artifact_path = character(),
    figure_name = character(),
    yvar = character(),
    use_log = logical(),
    bw_ft = numeric(),
    sample_filter = character(),
    fe_spec = character(),
    plot_style = character(),
    gap_split = character(),
    donut_ft = numeric(),
    placebo_shift_ft = numeric(),
    audit_pdf_path = character(),
    audit_bins_path = character(),
    audit_meta_path = character(),
    bins_exists = logical(),
    meta_exists = logical()
  )
}

event_sidecars_exist <- function(artifact_path, density_sidecar_dir = NULL) {
  if (!file.exists(artifact_path)) {
    return(FALSE)
  }

  artifact_dir <- dirname(artifact_path)
  artifact_name <- basename(artifact_path)

  if (str_detect(artifact_path, "tasks/run_event_study_permit/output/")) {
    stub <- artifact_name %>%
      str_remove("\\.pdf$") %>%
      str_remove("^event_study_yearly_") %>%
      str_remove("^event_study_combined_")

    live_expected <- c(
      file.path(artifact_dir, paste0("event_study_coefficients_", stub, ".csv")),
      file.path(artifact_dir, paste0("event_study_support_", stub, ".csv")),
      file.path(artifact_dir, paste0("event_study_pretrend_", stub, ".csv")),
      file.path(artifact_dir, paste0("event_study_metadata_", stub, ".csv"))
    )
    audit_dir <- str_replace(
      artifact_dir,
      "tasks/run_event_study_permit/output$",
      "tasks/audits/permit_event_study_audit/output"
    )
    audit_expected <- file.path(
      audit_dir,
      paste0(
        c(
          "event_study_coefficients_",
          "event_study_support_",
          "event_study_pretrend_",
          "event_study_metadata_"
        ),
        stub,
        ".csv"
      )
    )
    return(all(file.exists(live_expected)) || all(file.exists(audit_expected)))
  }

  if (str_detect(artifact_path, "tasks/run_event_study_sales_disaggregate/output/")) {
    stub <- artifact_name %>%
      str_remove("\\.pdf$") %>%
      str_remove("^event_study_disaggregate_") %>%
      str_remove("^event_study_combined_disaggregate_")

    expected <- c(
      file.path(artifact_dir, paste0("event_study_coefficients_disaggregate_", stub, ".csv")),
      file.path(artifact_dir, paste0("event_study_support_disaggregate_", stub, ".csv")),
      file.path(artifact_dir, paste0("event_study_pretrend_disaggregate_", stub, ".csv")),
      file.path(artifact_dir, paste0("event_study_metadata_disaggregate_", stub, ".csv"))
    )
    return(all(file.exists(expected)))
  }

  NA
}

tidy_p_value <- function(model, term) {
  coef_table <- coeftable(model)
  if (!term %in% rownames(coef_table)) {
    return(NA_real_)
  }
  p_col <- grep("^Pr\\(", colnames(coef_table), value = TRUE)[1]
  coef_table[term, p_col]
}

effect_pct <- function(beta) {
  ifelse(is.finite(beta), 100 * (exp(beta) - 1), NA_real_)
}

fe_formulas <- list(
  zonegroup_segment_year_additive = "zone_group + segment_id + construction_year",
  zonegroup_pair_year_additive = "zone_group + ward_pair + construction_year"
)

apply_density_prune <- function(df, confound_flags_path) {
  conf_flags <- read_csv(
    confound_flags_path,
    show_col_types = FALSE,
    col_select = c("ward_pair_id_dash", "era", "drop_confound")
  ) %>%
    transmute(
      pair_dash = normalize_pair_dash(ward_pair_id_dash),
      era = as.character(era),
      keep_pair_era = !as.logical(drop_confound)
    ) %>%
    distinct()

  df %>%
    mutate(
      pair_dash = normalize_pair_dash(ward_pair),
      era = era_from_year(construction_year)
    ) %>%
    left_join(conf_flags, by = c("pair_dash", "era")) %>%
    mutate(keep_pair_era = replace_na(keep_pair_era, FALSE)) %>%
    filter(keep_pair_era)
}

run_density_model <- function(df, outcome_var, bw_ft = 500, fe_spec = "zonegroup_segment_year_additive", prune_sample = FALSE, units_cap = Inf, donut_ft = 0) {
  controls <- c(
    "strictness_own",
    "share_white_own",
    "share_black_own",
    "median_hh_income_own",
    "share_bach_plus_own",
    "homeownership_rate_own"
  )

  work <- df %>%
    filter(unitscount > 1)

  if (is.finite(units_cap)) {
    work <- work %>% filter(unitscount <= units_cap)
  }

  if (prune_sample) {
    work <- apply_density_prune(work, file.path(repo_root, "tasks/border_confound_pruning/output/confounded_pair_era_flags.csv"))
  }

  work <- work %>%
    filter(
      !is.na(ward_pair), ward_pair != "",
      !is.na(segment_id), segment_id != "",
      !is.na(zone_group),
      dist_to_boundary >= donut_ft,
      dist_to_boundary <= bw_ft,
      is.finite(.data[[outcome_var]]),
      .data[[outcome_var]] > 0
    )

  model <- feols(
    as.formula(sprintf(
      "log(%s) ~ %s | %s",
      outcome_var,
      paste(controls, collapse = " + "),
      fe_formulas[[fe_spec]]
    )),
    data = work,
    cluster = ~ward_pair,
    notes = FALSE
  )

  tibble(
    estimate = coef(model)[["strictness_own"]],
    se = se(model)[["strictness_own"]],
    p_value = tidy_p_value(model, "strictness_own"),
    effect_pct = effect_pct(coef(model)[["strictness_own"]]),
    n_obs = nobs(model),
    n_ward_pairs = n_distinct(work$ward_pair),
    n_segments = n_distinct(work$segment_id),
    model = list(model),
    sample = list(work)
  )
}

run_density_balance_model <- function(df, outcome_var) {
  work <- df %>%
    filter(
      unitscount > 1,
      !is.na(ward_pair), ward_pair != "",
      !is.na(segment_id), segment_id != "",
      !is.na(zone_group),
      dist_to_boundary <= 500,
      is.finite(.data[[outcome_var]])
    )

  work <- work %>%
    mutate(outcome_std = as.numeric(scale(.data[[outcome_var]])))

  model <- feols(
    outcome_std ~ strictness_own | zone_group + segment_id + construction_year,
    data = work,
    cluster = ~ward_pair,
    notes = FALSE
  )

  tibble(
    covariate = outcome_var,
    estimate_sd = coef(model)[["strictness_own"]],
    se = se(model)[["strictness_own"]],
    p_value = tidy_p_value(model, "strictness_own"),
    n_obs = nobs(model),
    n_ward_pairs = n_distinct(work$ward_pair)
  )
}

run_permit_did <- function(df, rel_min = -5, rel_max = 5, cluster_level = "block") {
  work <- df %>%
    filter(
      !is.na(ward_pair_id), ward_pair_id != "",
      !is.na(block_id), block_id != "",
      !is.na(strictness_change),
      !is.na(n_high_discretion_issue),
      dist_ft <= 1000,
      relative_year >= rel_min,
      relative_year <= rel_max
    ) %>%
    mutate(
      weight = 1,
      post_treat = as.integer(relative_year >= 0) * strictness_change
    )

  model <- fepois(
    n_high_discretion_issue ~ post_treat | block_id + ward_pair_id^year,
    data = work,
    weights = ~weight,
    cluster = if (cluster_level == "block") ~block_id else ~ward_pair_id,
    notes = FALSE
  )

  tibble(
    estimate = coef(model)[["post_treat"]],
    se = se(model)[["post_treat"]],
    p_value = tidy_p_value(model, "post_treat"),
    effect_pct = effect_pct(coef(model)[["post_treat"]]),
    n_obs = nobs(model),
    n_ward_pairs = n_distinct(work$ward_pair_id)
  )
}

run_sales_did <- function(df, bandwidth = 1000, cluster_level = "block", price_trim = 0, include_hedonics = TRUE) {
  hedonic_vars <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")

  work <- df %>%
    mutate(ward_pair = sub("_[0-9]+$", "", ward_pair_side)) %>%
    filter(
      dist_ft <= bandwidth,
      relative_year >= -5,
      relative_year <= 5,
      !is.na(ward_pair), ward_pair != "",
      !is.na(ward_pair_side), ward_pair_side != "",
      !is.na(block_id), block_id != "",
      sale_price > 0,
      if_all(all_of(hedonic_vars), ~ is.finite(.x))
    )

  if (price_trim > 0) {
    cutoffs <- quantile(work$sale_price, probs = c(price_trim, 1 - price_trim), na.rm = TRUE)
    work <- work %>% filter(sale_price >= cutoffs[[1]], sale_price <= cutoffs[[2]])
  }

  work <- work %>%
    mutate(
      weight = 1,
      post_treat = as.integer(relative_year >= 0) * strictness_change
    )

  rhs <- if (include_hedonics) {
    paste(c("post_treat", hedonic_vars), collapse = " + ")
  } else {
    "post_treat"
  }

  model <- feols(
    as.formula(sprintf("log(sale_price) ~ %s | ward_pair_side + ward_pair^sale_year", rhs)),
    data = work,
    weights = ~weight,
    cluster = if (cluster_level == "block") ~block_id else ~ward_pair,
    notes = FALSE
  )

  tibble(
    estimate = coef(model)[["post_treat"]],
    se = se(model)[["post_treat"]],
    p_value = tidy_p_value(model, "post_treat"),
    effect_pct = effect_pct(coef(model)[["post_treat"]]),
    n_obs = nobs(model),
    n_ward_pairs = n_distinct(work$ward_pair),
    n_blocks = n_distinct(work$block_id)
  )
}

with_dir(repo_root, {
  base_findings <- if (file.exists(base_findings_path)) {
    read_csv(base_findings_path, show_col_types = FALSE)
  } else {
    tibble(
      id = character(),
      severity = character(),
      section = character(),
      paper_ref = character(),
      claim_summary = character(),
      status = character(),
      recommended_fix = character(),
      fix_type = character()
    )
  }
  artifact_map <- if (file.exists(base_artifact_map_path)) {
    read_csv(base_artifact_map_path, show_col_types = FALSE)
  } else {
    tibble(
      section = character(),
      paper_ref = character(),
      artifact_type = character(),
      artifact_path = character(),
      producer_task = character(),
      producer_script = character()
    )
  }
  coverage_matrix <- if (file.exists(base_coverage_path)) {
    read_csv(base_coverage_path, show_col_types = FALSE)
  } else {
    tibble()
  }
  density_sidecar_dir <- file.path(output_dir, "density_sidecars")
  dir_create(density_sidecar_dir)

  density_sidecar_inventory <- artifact_map %>%
    filter(FALSE) %>%
    pull(artifact_path) %>%
    build_density_sidecars(density_sidecar_dir = density_sidecar_dir)
  write_csv(density_sidecar_inventory, density_sidecar_inventory_path)

  hostile_artifact_map <- artifact_map %>%
    mutate(
      artifact_exists_now = file.exists(artifact_path),
      producer_script_exists = file.exists(file.path(repo_root, producer_script)),
      producer_script_git_state = map_chr(file.path(repo_root, producer_script), git_state),
      live_sidecars_exist_now = if_else(
        FALSE,
        map_lgl(artifact_path, ~ event_sidecars_exist(.x, density_sidecar_dir = NULL)),
        NA
      ),
      audit_sidecars_exist_now = if_else(
        FALSE,
        map_lgl(artifact_path, function(x) {
          audit_pdf <- file.path(density_sidecar_dir, basename(x))
          file.exists(sub("\\.pdf$", "_bins.csv", audit_pdf)) &&
            file.exists(sub("\\.pdf$", "_meta.csv", audit_pdf))
        }),
        NA
      ),
      figure_sidecars_exist_now = if_else(
        artifact_type == "figure",
        map_lgl(artifact_path, ~ event_sidecars_exist(.x, density_sidecar_dir = density_sidecar_dir)),
        NA
      ),
      artifact_auditability = case_when(
        artifact_type == "table" & artifact_exists_now ~ "direct_numeric_artifact",
        FALSE &
          dplyr::coalesce(audit_sidecars_exist_now, FALSE) & !dplyr::coalesce(live_sidecars_exist_now, FALSE) ~ "figure_with_audit_sidecars",
        artifact_type == "figure" & dplyr::coalesce(figure_sidecars_exist_now, FALSE) ~ "figure_with_sidecars",
        artifact_type == "figure" & artifact_exists_now ~ "figure_without_sidecars",
        TRUE ~ "missing_artifact"
      )
    )

  make_prereq_audit <- map_dfr(names(makefile_targets), function(task_name) {
    makefile_rel <- makefile_targets[[task_name]]
    makefile_abs <- file.path(repo_root, makefile_rel)
    if (!file.exists(makefile_abs)) {
      return(tibble())
    }

    parse_prereqs_from_makefile(makefile_abs) %>%
      mutate(
        task = task_name,
        prereq_exists = file.exists(abs_path),
        git_state = map_chr(abs_path, ~ if (file.exists(.x)) git_state(.x) else "missing"),
        prereq_kind = case_when(
          str_detect(abs_path, paste0("^", fixed(file.path(repo_root, "data_raw")))) ~ "raw_data",
          str_detect(abs_path, "/output/") ~ "task_output",
          str_detect(abs_path, "/input/") ~ "task_input",
          str_detect(abs_path, paste0("^", fixed(file.path(repo_root, "tasks")), ".*/code/")) ~ "task_code",
          str_detect(abs_path, paste0("^", fixed(repo_root))) ~ "repo_file",
          TRUE ~ "external"
        ),
        risk_level = case_when(
          !prereq_exists ~ "P1_missing_prereq",
          prereq_kind %in% c("task_code", "repo_file") & git_state %in% c("ignored", "untracked") ~ "P1_hidden_dependency",
          prereq_kind == "raw_data" ~ "expected_external_raw_data",
          TRUE ~ "none"
        ),
        prereq_rel_repo = path_rel(abs_path, repo_root)
      )
  })

  package_findings <- make_prereq_audit %>%
    filter(risk_level != "none") %>%
    select(task, makefile, target, prereq_rel_repo, prereq_kind, prereq_exists, git_state, risk_level) %>%
    arrange(desc(risk_level), task, prereq_rel_repo)

  write_csv(package_findings, package_integrity_path)

  hostile_claim_ledger <- base_findings %>%
    left_join(
      hostile_artifact_map %>%
        select(section, paper_ref, artifact_type, artifact_path, artifact_exists_now, figure_sidecars_exist_now, artifact_auditability, producer_task, producer_script, producer_script_git_state),
      by = c("section", "paper_ref")
    ) %>%
    mutate(
      claim_text = map_chr(paper_ref, extract_claim_text),
      hostile_evidence_class = case_when(
        status == "verified" & artifact_auditability %in% c("direct_numeric_artifact", "figure_with_sidecars", "figure_with_audit_sidecars") ~ "reproduced_and_auditable",
        status == "verified" & artifact_auditability == "figure_without_sidecars" ~ "reproduced_but_only_visually_supported",
        status == "verified" & is.na(artifact_auditability) ~ "reproduced_and_auditable",
        status == "partially_verified" & artifact_auditability %in% c("figure_with_sidecars", "figure_with_audit_sidecars") ~ "reproduced_after_sidecar_check",
        status == "stale" ~ "stale_relative_to_current_code_or_outputs",
        status == "contradicted" ~ "contradicted_by_current_code_or_outputs",
        status == "unverified" ~ "unsupported_or_missing_artifact",
        TRUE ~ status
      ),
      hostile_stoplight = case_when(
        hostile_evidence_class %in% c("reproduced_and_auditable", "reproduced_after_sidecar_check") ~ "green",
        hostile_evidence_class %in% c("reproduced_but_only_visually_supported", "stale_relative_to_current_code_or_outputs") ~ "yellow",
        hostile_evidence_class %in% c("contradicted_by_current_code_or_outputs", "unsupported_or_missing_artifact") ~ "red",
        TRUE ~ "yellow"
      )
    )
  density_input <- read_csv(file.path(repo_root, "tasks/merge_in_scores/output/parcels_with_ward_distances.csv"), show_col_types = FALSE) %>%
    mutate(
      zone_group = zone_group_from_code(zone_code)
    )

  density_sample_ladder <- bind_rows(
    tibble(stage = "raw_scored_parcels", n_rows = nrow(density_input), n_unique_pin = n_distinct(density_input$pin)),
    density_input %>%
      filter(arealotsf > 1, areabuilding > 1, construction_year >= 2006, construction_year <= 2022) %>%
      summarise(stage = "quality_screen", n_rows = n(), n_unique_pin = n_distinct(pin)),
    density_input %>%
      filter(arealotsf > 1, areabuilding > 1, construction_year >= 2006, construction_year <= 2022, unitscount > 1) %>%
      summarise(stage = "multifamily_sample", n_rows = n(), n_unique_pin = n_distinct(pin)),
    density_input %>%
      filter(arealotsf > 1, areabuilding > 1, construction_year >= 2006, construction_year <= 2022, unitscount > 1, !is.na(ward_pair), !is.na(segment_id), !is.na(zone_group)) %>%
      summarise(stage = "main_fe_complete_case", n_rows = n(), n_unique_pin = n_distinct(pin)),
    density_input %>%
      filter(arealotsf > 1, areabuilding > 1, construction_year >= 2006, construction_year <= 2022, unitscount > 1, !is.na(ward_pair), !is.na(segment_id), !is.na(zone_group), dist_to_boundary <= 500) %>%
      summarise(stage = "main_fe_bw500", n_rows = n(), n_unique_pin = n_distinct(pin)),
    density_input %>%
      filter(arealotsf > 1, areabuilding > 1, construction_year >= 2006, construction_year <= 2022, unitscount > 1, !is.na(ward_pair), !is.na(segment_id), !is.na(zone_group), dist_to_boundary <= 500, density_far > 0) %>%
      summarise(stage = "main_fe_bw500_log_far", n_rows = n(), n_unique_pin = n_distinct(pin)),
    density_input %>%
      filter(arealotsf > 1, areabuilding > 1, construction_year >= 2006, construction_year <= 2022, unitscount > 1, !is.na(ward_pair), !is.na(segment_id), !is.na(zone_group), dist_to_boundary <= 500, density_dupac > 0) %>%
      summarise(stage = "main_fe_bw500_log_dupac", n_rows = n(), n_unique_pin = n_distinct(pin))
  )
  write_csv(density_sample_ladder, density_sample_ladder_path)

  geocoded_parcels <- st_read(file.path(repo_root, "tasks/geocode_ccao_data/output/geocoded_residential_data.gpkg"), quiet = TRUE)
  chicago_boundary <- st_read(file.path(repo_root, "data_raw/Boundaries_-_City_20250920.geojson"), quiet = TRUE) %>%
    st_transform(st_crs(geocoded_parcels))
  density_geocode_checks <- tibble(
    branch = "density",
    check = c("duplicate_pin", "outside_city_boundary", "invalid_lat_lon_bbox"),
    value = as.character(c(
      sum(duplicated(geocoded_parcels$pin)),
      sum(!(lengths(st_within(geocoded_parcels, chicago_boundary)) > 0)),
      sum(st_coordinates(st_transform(geocoded_parcels, 4326))[, "Y"] < 41 | st_coordinates(st_transform(geocoded_parcels, 4326))[, "Y"] > 43 |
            st_coordinates(st_transform(geocoded_parcels, 4326))[, "X"] < -89 | st_coordinates(st_transform(geocoded_parcels, 4326))[, "X"] > -87)
    )),
    detail = c("Unique parcel PINs should be one-to-one in the geocoded parcel layer.",
               "Geocoded parcel points should fall within the current Chicago city boundary.",
               "Parcel lat/lon should stay inside the broad Chicago bounding box [41,43] x [-89,-87].")
  )

  density_challenge_results <- bind_rows(
    run_density_model(density_input, "density_far") %>% mutate(branch = "density", outcome = "FAR", challenge = "baseline_segment_fe_bw500"),
    run_density_model(density_input, "density_dupac") %>% mutate(branch = "density", outcome = "DUPAC", challenge = "baseline_segment_fe_bw500"),
    run_density_model(density_input, "density_far", bw_ft = 250) %>% mutate(branch = "density", outcome = "FAR", challenge = "bandwidth_250"),
    run_density_model(density_input, "density_dupac", bw_ft = 250) %>% mutate(branch = "density", outcome = "DUPAC", challenge = "bandwidth_250"),
    run_density_model(density_input, "density_far", bw_ft = 1000) %>% mutate(branch = "density", outcome = "FAR", challenge = "bandwidth_1000"),
    run_density_model(density_input, "density_dupac", bw_ft = 1000) %>% mutate(branch = "density", outcome = "DUPAC", challenge = "bandwidth_1000"),
    run_density_model(density_input, "density_far", fe_spec = "zonegroup_pair_year_additive") %>% mutate(branch = "density", outcome = "FAR", challenge = "pair_fe"),
    run_density_model(density_input, "density_dupac", fe_spec = "zonegroup_pair_year_additive") %>% mutate(branch = "density", outcome = "DUPAC", challenge = "pair_fe"),
    run_density_model(density_input, "density_far", prune_sample = TRUE) %>% mutate(branch = "density", outcome = "FAR", challenge = "pruned_pairs"),
    run_density_model(density_input, "density_dupac", prune_sample = TRUE) %>% mutate(branch = "density", outcome = "DUPAC", challenge = "pruned_pairs"),
    run_density_model(density_input, "density_far", units_cap = 100) %>% mutate(branch = "density", outcome = "FAR", challenge = "units_2_to_100"),
    run_density_model(density_input, "density_dupac", units_cap = 100) %>% mutate(branch = "density", outcome = "DUPAC", challenge = "units_2_to_100")
  ) %>%
    select(branch, outcome, challenge, estimate, effect_pct, se, p_value, n_obs, n_ward_pairs, n_segments)

  density_balance_tests <- bind_rows(
    run_density_balance_model(density_input, "share_white_own"),
    run_density_balance_model(density_input, "share_black_own"),
    run_density_balance_model(density_input, "median_hh_income_own"),
    run_density_balance_model(density_input, "share_bach_plus_own"),
    run_density_balance_model(density_input, "homeownership_rate_own"),
    run_density_balance_model(density_input, "avg_rent_own"),
    run_density_balance_model(density_input, "avg_home_value_own"),
    run_density_balance_model(density_input, "population_density_bg")
  )
  write_csv(density_balance_tests, density_balance_path)

  density_far_baseline <- run_density_model(density_input, "density_far")
  density_far_sample <- density_far_baseline$sample[[1]]
  density_far_estimate <- density_far_baseline$estimate[[1]]

  density_pair_influence <- density_far_sample %>%
    count(ward_pair, sort = TRUE) %>%
    mutate(
      estimate_drop = map_dbl(ward_pair, function(pair_id) {
        run_density_model(density_input %>% filter(ward_pair != pair_id), "density_far")$estimate[[1]]
      }),
      abs_change = abs(estimate_drop - density_far_estimate)
    )
  write_csv(density_pair_influence, density_influence_pair_path)

  segment_screen_n <- min(150L, n_distinct(density_far_sample$segment_id))
  density_segment_influence <- density_far_sample %>%
    count(segment_id, sort = TRUE) %>%
    slice_head(n = segment_screen_n) %>%
    mutate(
      estimate_drop = map_dbl(segment_id, function(seg_id) {
        run_density_model(density_input %>% filter(segment_id != seg_id), "density_far")$estimate[[1]]
      }),
      abs_change = abs(estimate_drop - density_far_estimate),
      screen_scope = sprintf("top_%d_segments_by_observation_count", segment_screen_n)
    )
  write_csv(density_segment_influence, density_influence_segment_path)

  permit_review_log <- read_csv(file.path(repo_root, "tasks/create_event_study_permit_data/output/permit_block_assignment_review_log.csv"), show_col_types = FALSE)
  permit_panel_2015 <- read_parquet(file.path(repo_root, "tasks/create_event_study_permit_data/output/permit_block_year_panel_2015.parquet"))
  permit_panel_full <- read_parquet(file.path(repo_root, "tasks/create_event_study_permit_data/output/permit_block_year_panel.parquet"))
  permit_missing_2010 <- read_csv(file.path(repo_root, "tasks/create_event_study_permit_data/output/permit_block_assignment_missing_2010.csv"), show_col_types = FALSE)
  permit_missing_2020 <- read_csv(file.path(repo_root, "tasks/create_event_study_permit_data/output/permit_block_assignment_missing_2020.csv"), show_col_types = FALSE)
  permits_clean_min <- st_read(
    file.path(repo_root, "tasks/clean_building_permits/output/building_permits_clean.gpkg"),
    query = paste(
      "SELECT id, pin, permit_type, high_discretion, permit_issued,",
      "application_start_date_ym, issue_date_ym, latitude, longitude",
      "FROM building_permits_clean"
    ),
    quiet = TRUE
  ) %>%
    st_drop_geometry() %>%
    mutate(
      id = as.character(id),
      issue_year = as.integer(format(as.Date(issue_date_ym), "%Y")),
      application_year = as.integer(format(as.Date(application_start_date_ym), "%Y"))
    )

  permit_integrity_checks <- tibble(
    branch = "permits",
    check = c(
      "duplicate_clean_permit_id",
      "issued_missing_lat_lon",
      "issued_outside_bbox",
      "review_log_rows",
      "review_manual_assign_rows",
      "residual_unmatched_2010",
      "residual_unmatched_2020",
      "stacked_panel_duplicate_cohort_block_year",
      "manual_review_file_git_state"
    ),
    value = as.character(c(
      sum(duplicated(permits_clean_min$id)),
      permits_clean_min %>% filter(permit_issued == 1) %>% summarise(n = sum(is.na(latitude) | is.na(longitude))) %>% pull(n),
      permits_clean_min %>% filter(permit_issued == 1) %>% summarise(n = sum(latitude < 41 | latitude > 43 | longitude < -89 | longitude > -87, na.rm = TRUE)) %>% pull(n),
      nrow(permit_review_log),
      permit_review_log %>% filter(review_decision == "assign") %>% nrow(),
      nrow(permit_missing_2010),
      nrow(permit_missing_2020),
      permit_panel_full %>% summarise(n = n() - n_distinct(cohort_block_id, year)) %>% pull(n),
      git_state(file.path(repo_root, "tasks/create_event_study_permit_data/code/manual_permit_block_review.csv"))
    )),
    detail = c(
      "Clean permit IDs should be unique.",
      "Issued permits should not lose coordinate fields unexpectedly.",
      "Issued permits should stay inside the broad Chicago bounding box after cleaning.",
      "Reviewed unmatched permit cases should match the explicit review log.",
      "Exactly reviewed manually assigned permit cases used by the permit panel.",
      "Residual 2010 block-assignment misses after review.",
      "Residual 2020 block-assignment misses after review.",
      "Stacked permit panel should be unique at cohort_block_id x year.",
      "Manual permit review should not remain an ignored local dependency in a public package."
    )
  )

  permit_sample_ladder <- bind_rows(
    permits_clean_min %>%
      summarise(stage = "clean_permits_total", n_rows = n(), n_unique_ids = n_distinct(id)),
    permits_clean_min %>%
      filter(permit_issued == 1) %>%
      summarise(stage = "issued_permits", n_rows = n(), n_unique_ids = n_distinct(id)),
    permits_clean_min %>%
      filter(permit_issued == 1, high_discretion == 1) %>%
      summarise(stage = "issued_high_discretion", n_rows = n(), n_unique_ids = n_distinct(id)),
    permit_review_log %>%
      summarise(stage = "reviewed_unmatched_cases", n_rows = n(), n_unique_ids = n_distinct(id)),
    permit_review_log %>%
      filter(review_decision == "assign") %>%
      summarise(stage = "manually_assigned_cases", n_rows = n(), n_unique_ids = n_distinct(id)),
    permit_review_log %>%
      filter(review_decision == "drop") %>%
      summarise(stage = "reviewed_dropped_cases", n_rows = n(), n_unique_ids = n_distinct(id)),
    permit_panel_2015 %>%
      summarise(stage = "permit_panel_2015_rows", n_rows = n(), n_unique_ids = n_distinct(block_id)),
    permit_panel_full %>%
      summarise(stage = "permit_panel_stacked_rows", n_rows = n(), n_unique_ids = n_distinct(cohort_block_id))
  )
  write_csv(permit_sample_ladder, permit_sample_ladder_path)

  permit_assigned_case <- permit_review_log %>%
    filter(review_decision == "assign", permit_issued == 1) %>%
    mutate(
      id = as.character(id),
      final_block_id = as.character(final_block_id)
    ) %>%
    distinct(id, final_block_id, issue_year, high_discretion)

  permit_panel_no_manual <- permit_panel_2015
  if (nrow(permit_assigned_case) > 0) {
    for (i in seq_len(nrow(permit_assigned_case))) {
      row_i <- permit_assigned_case[i, ]
      idx <- permit_panel_no_manual$block_id == row_i$final_block_id & permit_panel_no_manual$year == row_i$issue_year
      if (row_i$high_discretion == 1 && any(idx)) {
        permit_panel_no_manual$n_high_discretion_issue[idx] <- pmax(0, permit_panel_no_manual$n_high_discretion_issue[idx] - 1L)
        permit_panel_no_manual$has_high_discretion_issue[idx] <- as.integer(permit_panel_no_manual$n_high_discretion_issue[idx] > 0)
      }
    }
  }

  permit_panel_drop_assigned_block <- if (nrow(permit_assigned_case) > 0) {
    permit_panel_2015 %>% filter(!block_id %in% permit_assigned_case$final_block_id)
  } else {
    permit_panel_2015
  }

  permit_challenge_results <- bind_rows(
    run_permit_did(permit_panel_2015) %>% mutate(branch = "permits", outcome = "high_discretion_permits", challenge = "baseline"),
    run_permit_did(permit_panel_2015, cluster_level = "ward_pair") %>% mutate(branch = "permits", outcome = "high_discretion_permits", challenge = "cluster_ward_pair"),
    run_permit_did(permit_panel_2015, rel_min = -4, rel_max = 4) %>% mutate(branch = "permits", outcome = "high_discretion_permits", challenge = "window_neg4_pos4"),
    run_permit_did(permit_panel_no_manual) %>% mutate(branch = "permits", outcome = "high_discretion_permits", challenge = "drop_manually_assigned_permit"),
    run_permit_did(permit_panel_drop_assigned_block) %>% mutate(branch = "permits", outcome = "high_discretion_permits", challenge = "drop_assigned_block_entirely")
  ) %>%
    select(branch, outcome, challenge, estimate, effect_pct, se, p_value, n_obs, n_ward_pairs)

  sales_input <- read_csv(file.path(repo_root, "tasks/calculate_sale_distances/output/sales_with_ward_distances.csv"), show_col_types = FALSE)
  sales_panel_2015 <- read_parquet(file.path(repo_root, "tasks/create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet"))
  sales_points <- st_as_sf(
    sales_input %>% filter(!is.na(longitude), !is.na(latitude)),
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(st_crs(chicago_boundary))

  sales_integrity_checks <- tibble(
    branch = "home_sales",
    check = c(
      "duplicate_scored_sale_pin_date",
      "missing_sale_coordinates",
      "sale_outside_bbox",
      "sale_outside_city_boundary",
      "analysis_panel_duplicate_pin_sale_date",
      "analysis_panel_missing_block_id",
      "analysis_panel_missing_ward_pair_side"
    ),
    value = as.character(c(
      sales_input %>% summarise(n = n() - n_distinct(pin, sale_date)) %>% pull(n),
      sales_input %>% summarise(n = sum(is.na(latitude) | is.na(longitude))) %>% pull(n),
      sales_input %>% summarise(n = sum(latitude < 41 | latitude > 43 | longitude < -89 | longitude > -87, na.rm = TRUE)) %>% pull(n),
      sum(!(lengths(st_within(sales_points, chicago_boundary)) > 0)),
      sales_panel_2015 %>% summarise(n = n() - n_distinct(pin, sale_date)) %>% pull(n),
      sales_panel_2015 %>% summarise(n = sum(is.na(block_id) | block_id == "")) %>% pull(n),
      sales_panel_2015 %>% summarise(n = sum(is.na(ward_pair_side) | ward_pair_side == "")) %>% pull(n)
    )),
    detail = c(
      "Scored sale records should be unique at PIN x sale_date.",
      "Scored sale records should not be missing coordinates.",
      "Sale coordinates should stay inside the broad Chicago bounding box [41,43] x [-89,-87].",
      "Sale coordinates should fall within the Chicago city boundary.",
      "2015 sales analysis panel should be unique at PIN x sale_date.",
      "2015 sales analysis panel should have block IDs for all included rows.",
      "2015 sales analysis panel should have ward-pair-side assignments for all included rows."
    )
  )

  sales_sample_ladder <- bind_rows(
    sales_input %>%
      summarise(stage = "scored_sales_total", n_rows = n(), n_unique_ids = n_distinct(pin, sale_date)),
    sales_input %>%
      filter(!is.na(latitude), !is.na(longitude)) %>%
      summarise(stage = "sales_with_coordinates", n_rows = n(), n_unique_ids = n_distinct(pin, sale_date)),
    sales_panel_2015 %>%
      summarise(stage = "sales_panel_2015_rows", n_rows = n(), n_unique_ids = n_distinct(pin, sale_date)),
    sales_panel_2015 %>%
      mutate(ward_pair = sub("_[0-9]+$", "", ward_pair_side)) %>%
      filter(
        dist_ft <= 1000,
        relative_year >= -5,
        relative_year <= 5,
        !is.na(ward_pair), ward_pair != "",
        !is.na(ward_pair_side), ward_pair_side != "",
        !is.na(block_id), block_id != "",
        sale_price > 0,
        if_all(c(log_sqft, log_land_sqft, log_building_age, log_bedrooms, log_baths, has_garage), ~ is.finite(.x))
      ) %>%
      summarise(stage = "sales_did_analysis_sample", n_rows = n(), n_unique_ids = n_distinct(pin, sale_date))
  )
  write_csv(sales_sample_ladder, sales_sample_ladder_path)

  sales_challenge_results <- bind_rows(
    run_sales_did(sales_panel_2015) %>% mutate(branch = "home_sales", outcome = "log_sale_price", challenge = "baseline"),
    run_sales_did(sales_panel_2015, cluster_level = "ward_pair") %>% mutate(branch = "home_sales", outcome = "log_sale_price", challenge = "cluster_ward_pair"),
    run_sales_did(sales_panel_2015, bandwidth = 500) %>% mutate(branch = "home_sales", outcome = "log_sale_price", challenge = "bandwidth_500"),
    run_sales_did(sales_panel_2015, price_trim = 0.01) %>% mutate(branch = "home_sales", outcome = "log_sale_price", challenge = "trim_price_1pct"),
    run_sales_did(sales_panel_2015, include_hedonics = FALSE) %>% mutate(branch = "home_sales", outcome = "log_sale_price", challenge = "no_hedonics_same_sample")
  ) %>%
    select(branch, outcome, challenge, estimate, effect_pct, se, p_value, n_obs, n_ward_pairs, n_blocks)

  sales_baseline <- run_sales_did(sales_panel_2015)
  sales_baseline_estimate <- sales_baseline$estimate[[1]]
  sales_baseline_sample <- sales_panel_2015 %>%
    mutate(ward_pair = sub("_[0-9]+$", "", ward_pair_side)) %>%
    filter(
      dist_ft <= 1000,
      relative_year >= -5,
      relative_year <= 5,
      !is.na(ward_pair), ward_pair != "",
      !is.na(ward_pair_side), ward_pair_side != "",
      !is.na(block_id), block_id != "",
      sale_price > 0,
      if_all(c(log_sqft, log_land_sqft, log_building_age, log_bedrooms, log_baths, has_garage), ~ is.finite(.x))
    )

  sales_pair_influence <- sales_baseline_sample %>%
    count(ward_pair, sort = TRUE) %>%
    mutate(
      estimate_drop = map_dbl(ward_pair, ~ run_sales_did(sales_panel_2015 %>% filter(sub("_[0-9]+$", "", ward_pair_side) != .x))$estimate[[1]]),
      abs_change = abs(estimate_drop - sales_baseline_estimate)
    )
  write_csv(sales_pair_influence, sales_influence_pair_path)

  block_screen_n <- min(150L, n_distinct(sales_baseline_sample$block_id))
  sales_block_influence <- sales_baseline_sample %>%
    count(block_id, sort = TRUE) %>%
    slice_head(n = block_screen_n) %>%
    mutate(
      estimate_drop = map_dbl(block_id, ~ run_sales_did(sales_panel_2015 %>% filter(block_id != .x))$estimate[[1]]),
      abs_change = abs(estimate_drop - sales_baseline_estimate),
      screen_scope = sprintf("top_%d_blocks_by_observation_count", block_screen_n)
    )
  write_csv(sales_block_influence, sales_influence_block_path)

  challenge_results <- bind_rows(density_challenge_results, permit_challenge_results, sales_challenge_results)
  write_csv(challenge_results, challenge_results_path)

  write_csv(density_challenge_results, density_challenge_path)
  write_csv(permit_challenge_results, permit_challenge_path)
  write_csv(sales_challenge_results, sales_challenge_path)

  data_integrity_checks <- bind_rows(density_geocode_checks, permit_integrity_checks, sales_integrity_checks)
  write_csv(data_integrity_checks, data_integrity_checks_path)

  release_findings <- package_findings %>%
    mutate(summary = case_when(
      risk_level == "P1_hidden_dependency" ~ paste0(task, " requires non-tracked local file ", prereq_rel_repo),
      risk_level == "P1_missing_prereq" ~ paste0(task, " has missing prerequisite ", prereq_rel_repo),
      risk_level == "expected_external_raw_data" ~ paste0(task, " depends on external raw input ", prereq_rel_repo),
      TRUE ~ prereq_rel_repo
    ))

  upstream_spatial_verdicts <- if (file.exists(upstream_spatial_verdicts_path)) {
    read_csv(upstream_spatial_verdicts_path, show_col_types = FALSE)
  } else {
    tibble(branch = character(), stoplight = character(), verdict = character(), summary = character())
  }

  merge_stoplight <- function(current_stoplight, upstream_stoplight) {
    current_rank <- c(green = 1L, yellow = 2L, red = 3L)[dplyr::coalesce(current_stoplight, "green")]
    upstream_rank <- c(green = 1L, yellow = 2L, red = 3L)[dplyr::coalesce(upstream_stoplight, "green")]
    c("green", "yellow", "red")[pmax(current_rank, upstream_rank)]
  }

  density_balance_red_flag <- any(density_balance_tests$p_value < 0.10, na.rm = TRUE)
  permit_hidden_dependency <- any(package_findings$prereq_rel_repo == "tasks/create_event_study_permit_data/code/manual_permit_block_review.csv")

  density_scoreboard <- density_challenge_results %>%
    group_by(outcome) %>%
    summarise(
      baseline_effect_pct = effect_pct[challenge == "baseline_segment_fe_bw500"][1],
      all_effects_negative = all(effect_pct < 0, na.rm = TRUE),
      max_abs_effect_pct = max(abs(effect_pct), na.rm = TRUE),
      min_abs_effect_pct = min(abs(effect_pct), na.rm = TRUE),
      any_p_above_0_10 = any(p_value >= 0.10, na.rm = TRUE),
      stoplight = case_when(
        !all_effects_negative ~ "red",
        density_balance_red_flag ~ "yellow",
        any_p_above_0_10 ~ "yellow",
        TRUE ~ "green"
      ),
      verdict = case_when(
        stoplight == "green" ~ "survives",
        stoplight == "yellow" ~ "survives_but_fragile",
        TRUE ~ "fails_as_written"
      ),
      summary = case_when(
        stoplight == "green" ~ "Sign and significance survive the predefined density challenge grid.",
        stoplight == "yellow" ~ "Main sign survives, but at least one challenge or balance check raises fragility.",
        TRUE ~ "At least one predefined challenge breaks the sign."
      )
    ) %>%
    mutate(branch = "density")

  permit_scoreboard <- permit_challenge_results %>%
    summarise(
      baseline_effect_pct = effect_pct[challenge == "baseline"][1],
      all_effects_negative = all(effect_pct < 0, na.rm = TRUE),
      any_p_above_0_10 = any(p_value >= 0.10, na.rm = TRUE),
      stoplight = case_when(
        !all_effects_negative ~ "red",
        permit_hidden_dependency ~ "yellow",
        any_p_above_0_10 ~ "yellow",
        TRUE ~ "green"
      ),
      verdict = case_when(
        stoplight == "green" ~ "survives",
        stoplight == "yellow" ~ "survives_but_fragile",
        TRUE ~ "fails_as_written"
      ),
      summary = case_when(
        stoplight == "green" ~ "High-discretion permit effect survives the reasonable challenge set.",
        stoplight == "yellow" ~ "Estimate survives, but package integrity or sensitivity checks keep it from green status.",
        TRUE ~ "At least one reasonable challenge breaks the sign."
      )
    ) %>%
    mutate(branch = "permits", outcome = "high_discretion_permits")

  sales_scoreboard <- sales_challenge_results %>%
    summarise(
      baseline_effect_pct = effect_pct[challenge == "baseline"][1],
      all_effects_positive = all(effect_pct > 0, na.rm = TRUE),
      any_p_above_0_10 = any(p_value >= 0.10, na.rm = TRUE),
      stoplight = case_when(
        !all_effects_positive ~ "red",
        any_p_above_0_10 ~ "yellow",
        TRUE ~ "green"
      ),
      verdict = case_when(
        stoplight == "green" ~ "survives",
        stoplight == "yellow" ~ "survives_but_fragile",
        TRUE ~ "fails_as_written"
      ),
      summary = case_when(
        stoplight == "green" ~ "Home-price estimate keeps the same sign through the predefined challenge grid.",
        stoplight == "yellow" ~ "Home-price estimate keeps the sign but is fragile to at least one reasonable challenge.",
        TRUE ~ "At least one reasonable challenge breaks the sign."
      )
    ) %>%
    mutate(branch = "home_sales", outcome = "log_sale_price")

  result_scoreboard <- bind_rows(density_scoreboard, permit_scoreboard, sales_scoreboard) %>%
    left_join(
      upstream_spatial_verdicts %>%
        transmute(branch, upstream_stoplight = stoplight, upstream_summary = summary),
      by = "branch"
    ) %>%
    rowwise() %>%
    mutate(
      stoplight = merge_stoplight(stoplight, upstream_stoplight),
      verdict = case_when(
        stoplight == "green" ~ "survives",
        stoplight == "yellow" ~ "survives_but_fragile",
        TRUE ~ "fails_as_written"
      ),
      summary = case_when(
        !is.na(upstream_stoplight) & upstream_stoplight != "green" ~ paste(summary, upstream_summary),
        TRUE ~ summary
      )
    ) %>%
    ungroup() %>%
    select(branch, outcome, baseline_effect_pct, verdict, stoplight, summary)
  write_csv(result_scoreboard, result_scoreboard_path)

  upstream_spatial_findings <- upstream_spatial_verdicts %>%
    filter(stoplight != "green", branch %in% c("density", "permits", "home_sales")) %>%
    mutate(
      id = case_when(
        branch == "density" ~ "H004",
        branch == "permits" ~ "H005",
        branch == "home_sales" ~ "H006",
        TRUE ~ "H099"
      ),
      severity = case_when(
        stoplight == "red" ~ "P1",
        TRUE ~ "P2"
      ),
      section = case_when(
        branch == "density" ~ "Density",
        branch == "permits" ~ "Permits",
        TRUE ~ "Home Sales"
      ),
      paper_ref = case_when(
        branch == "density" ~ "paper/sections/empirics_density.tex:22-113",
        branch == "permits" ~ "paper/sections/empirics_permits.tex:1-999",
        TRUE ~ "paper/sections/empirics_home_sales.tex:1-999"
      ),
      claim_summary = paste0("The manuscript branch relies on an upstream spatial/data-construction pipeline that the separate upstream red-team audit does not rate green."),
      status = if_else(stoplight == "red", "contradicted", "release_fragility"),
      evidence_output = upstream_spatial_verdicts_path,
      evidence_code = "tasks/audits/upstream_spatial_red_team_audit/code/run_upstream_spatial_red_team_audit.R",
      recommended_fix = "Resolve the upstream spatial audit findings or downgrade the manuscript claims that rely on this branch.",
      fix_type = "upstream_audit_followup",
      claim_text = NA_character_,
      hostile_stoplight = stoplight,
      finding_class = case_when(
        stoplight == "red" ~ "code_bug",
        TRUE ~ "robustness_fragility"
      )
    ) %>%
    select(
      id, severity, section, paper_ref, claim_summary, status, finding_class,
      evidence_output, evidence_code, recommended_fix, fix_type, claim_text, hostile_stoplight
    )

  density_balance_finding <- if (density_balance_red_flag) {
    tibble(
      id = "H001",
      severity = "P2",
      section = "Density",
      paper_ref = NA_character_,
      claim_summary = "Hostile balance audit flags detectable relationships between stringency and at least one audited covariate in the main density boundary sample.",
      status = "balance_fragility",
      evidence_output = density_balance_path,
      evidence_code = "tasks/audits/manuscript_verification_audit/code/run_hostile_replication_audit.R",
      recommended_fix = "If the paper discusses covariate balance, describe it as a diagnostic with mixed results or add the audit table to the appendix.",
      fix_type = "robustness_note"
    )
  } else {
    tibble(
      id = "H001",
      severity = "none",
      section = "Density",
      paper_ref = NA_character_,
      claim_summary = "The hostile balance audit does not find statistically detectable relationships across the audited covariates in the main density boundary sample.",
      status = "verified",
      evidence_output = density_balance_path,
      evidence_code = "tasks/audits/manuscript_verification_audit/code/run_hostile_replication_audit.R",
      recommended_fix = "No change needed.",
      fix_type = "table_or_appendix_artifact"
    )
  }

  package_hidden_dependency_finding <- if (permit_hidden_dependency) {
    tibble(
      id = "H002",
      severity = "P1",
      section = "Permits",
      paper_ref = "paper/sections/empirics_permits.tex:40-94",
      claim_summary = "Paper-facing permit results depend on the ignored local file manual_permit_block_review.csv in create_event_study_permit_data.",
      status = "package_failure",
      evidence_output = package_integrity_path,
      evidence_code = "tasks/create_event_study_permit_data/code/Makefile; tasks/create_event_study_permit_data/code/create_permit_block_year_panel.R",
      recommended_fix = "Track the manual review file or replace it with deterministic code before public release.",
      fix_type = "package_integrity"
    )
  } else {
    tibble()
  }

  density_sidecar_finding <- tibble()

  updated_claim_ledger <- bind_rows(
    hostile_claim_ledger %>%
      transmute(
        id,
        severity,
        section,
        paper_ref,
        claim_summary,
        status = hostile_evidence_class,
        finding_class = case_when(
          status == "package_failure" ~ "packaging_failure",
          status == "release_fragility" ~ "packaging_failure",
          status %in% c("contradicted_by_current_code_or_outputs", "contradicted") ~ "manuscript_overclaim",
          status %in% c("stale_relative_to_current_code_or_outputs", "unsupported_or_missing_artifact") ~ "manuscript_overclaim",
          status == "reproduced_but_only_visually_supported" ~ "manuscript_overclaim",
          status %in% c("reproduced_and_auditable", "reproduced_after_sidecar_check", "verified") ~ "no_issue",
          TRUE ~ "robustness_fragility"
        ),
        evidence_output = artifact_path,
        evidence_code = producer_script,
        recommended_fix,
        fix_type,
        claim_text,
        hostile_stoplight
      ),
    density_balance_finding %>% mutate(claim_text = NA_character_, hostile_stoplight = ifelse(severity == "none", "green", "yellow")),
    package_hidden_dependency_finding %>% mutate(claim_text = NA_character_, hostile_stoplight = "yellow"),
    density_sidecar_finding %>% mutate(claim_text = NA_character_, hostile_stoplight = "yellow"),
    upstream_spatial_findings
  ) %>%
    mutate(
      finding_class = dplyr::coalesce(
        finding_class,
        case_when(
          status == "package_failure" ~ "packaging_failure",
          status == "release_fragility" ~ "packaging_failure",
          status %in% c("contradicted_by_current_code_or_outputs", "contradicted") ~ "manuscript_overclaim",
          status %in% c("stale_relative_to_current_code_or_outputs", "unsupported_or_missing_artifact") ~ "manuscript_overclaim",
          status %in% c("reproduced_and_auditable", "reproduced_after_sidecar_check", "verified") ~ "no_issue",
          TRUE ~ "robustness_fragility"
        )
      )
    ) %>%
    arrange(section, id)
  write_csv(updated_claim_ledger, hostile_claim_ledger_path)

  write_csv(hostile_artifact_map, hostile_artifact_map_path)

  release_lines <- c(
    "# Replication Package Release Checklist",
    "",
    "## Release blockers",
    if (nrow(package_findings %>% filter(risk_level == "P1_hidden_dependency")) == 0) {
      "- No hidden local code-file dependencies detected in the audited paper-facing task set."
    } else {
      paste0("- Hidden local dependency: `", package_findings %>% filter(risk_level == "P1_hidden_dependency") %>% pull(prereq_rel_repo) %>% unique() %>% paste(collapse = "`, `"), "`")
    },
    "",
    "## External raw-data dependencies",
    paste0("- `", make_prereq_audit %>% filter(prereq_kind == "raw_data") %>% pull(prereq_rel_repo) %>% unique() %>% sort() %>% paste(collapse = "`\n- `"), "`"),
    "",
    "## Build-order spine for the compiled paper",
    "- Density: `geocode_ccao_data` -> `calculate_ward_boundary_distances` -> `assign_segment_ids` -> `merge_in_scores` -> `border_pair_FE_regressions` plus the active nonparametric density figure tasks -> `pruned_boundary_maps`",
    "- Permits/stringency: `clean_building_permits` -> `create_alderman_data` -> `create_block_treatment_panel` -> `merge_event_study_scores` -> `create_event_study_permit_data` -> `run_event_study_permit` ; stringency appendix uses `create_alderman_uncertainty_index`, `strictness_score_map`, `within_ward_strictness`, `uncertainty_validation_checks`, and `permit_summary_stats`",
    "- Event-study treatment maps: `create_event_study_permit_data` -> `merge_event_study_scores` -> `event_study_treatment_maps`",
    "",
    "## Artifact-preservation issues",
    "- Current paper-linked density figures should remain tied to saved estimate CSVs or tables when making quantitative claims.",
    "",
    "## Default release recommendation",
    "- Do not release the compiled-paper replication package until every P1 hidden dependency is tracked or replaced by deterministic code, and until every paper-facing prose claim with quantitative content can be audited from saved sidecars or saved tables."
  )
  writeLines(release_lines, release_checklist_path)

  density_headline <- result_scoreboard %>% filter(branch == "density")
  permit_headline <- result_scoreboard %>% filter(branch == "permits")
  sales_headline <- result_scoreboard %>% filter(branch == "home_sales")

  memo_lines <- c(
    "# Hostile Replication Audit for the Compiled Paper",
    "",
    "Scope reviewed:",
    "- Active compiled manuscript only: `paper/paper.tex`",
    "- Main text plus appendices A-D",
    "- Parked rent/home-value sections outside the build excluded",
    "",
    "Bottom line:",
    sprintf("- Density headline: %s.", paste0(density_headline$outcome, " = ", density_headline$verdict, collapse = "; ")),
    sprintf("- Permit headline: %s.", permit_headline$verdict[[1]]),
    sprintf("- Home-sales headline: %s.", sales_headline$verdict[[1]]),
    if (permit_hidden_dependency) {
      "- The biggest replication-package blocker is that the permit branch still depends on the ignored local file `tasks/create_event_study_permit_data/code/manual_permit_block_review.csv`."
    } else {
      "- No hidden local file dependencies were detected in the audited paper-facing tasks."
    },
    if (density_balance_red_flag) {
      "- The hostile balance audit flags at least one detectable relationship between stringency and the audited covariates in the main density boundary sample."
    } else {
      "- The hostile balance audit does not detect statistically significant relationships across the audited covariates in the main density boundary sample."
    },
    if (nrow(upstream_spatial_verdicts %>% filter(stoplight != "green", branch %in% c("density", "permits", "home_sales"))) > 0) {
      paste0(
        "- The separate upstream spatial red-team audit is not green for: ",
        paste(upstream_spatial_verdicts %>% filter(stoplight != "green", branch %in% c("density", "permits", "home_sales")) %>% pull(branch), collapse = ", "),
        "."
      )
    } else {
      "- The separate upstream spatial red-team audit does not add any new non-green branch verdicts."
    },
    "",
    "## Headline stability scoreboard",
    sprintf("- Density FAR: %.1f%% baseline, %s.", density_headline$baseline_effect_pct[density_headline$outcome == "FAR"], density_headline$verdict[density_headline$outcome == "FAR"]),
    sprintf("- Density DUPAC: %.1f%% baseline, %s.", density_headline$baseline_effect_pct[density_headline$outcome == "DUPAC"], density_headline$verdict[density_headline$outcome == "DUPAC"]),
    sprintf("- Permits: %.1f%% baseline, %s.", permit_headline$baseline_effect_pct[[1]], permit_headline$verdict[[1]]),
    sprintf("- Home sales: %.1f%% baseline, %s.", sales_headline$baseline_effect_pct[[1]], sales_headline$verdict[[1]]),
    "",
    "## Highest-priority red-team findings",
    if (nrow(package_findings %>% filter(risk_level == "P1_hidden_dependency")) > 0) {
      paste0("- Hidden dependency: `", package_findings %>% filter(risk_level == "P1_hidden_dependency") %>% pull(prereq_rel_repo) %>% unique() %>% paste(collapse = "`, `"), "`.")
    } else {
      "- No hidden local code-file prerequisites detected."
    },
    "- Current paper-linked density figures are audited visually unless linked to saved estimate CSVs, so quantitative prose should stay tied to tables and saved event-study CSVs.",
    sprintf("- Density balance max p-value concern flag: %s.", ifelse(density_balance_red_flag, "triggered", "not triggered")),
    "",
    "## Challenge-grid notes",
    "- Density challenge grid covers bandwidth 250/500/1000, pair FE vs segment FE, pruned vs full sample, 2-100 units sensitivity, and concentration / leave-one-out screens.",
    "- Permit challenge grid covers ward-pair clustering, shorter event window, and the exact role of the single manually assigned reviewed permit.",
    "- Home-sales challenge grid covers ward-pair clustering, 500-foot bandwidth, price trimming, no-hedonics same-sample estimation, and leave-one-out screens at ward-pair and high-weight block level.",
    "",
    "## Default hostile recommendation",
    "- The headline empirical signs currently survive the reasonable challenge set, but the compiled paper is not release-ready as a replication package until the permit manual-review dependency and density sidecar preservation issue are resolved.",
    "- Any claim that remains only visually supported should be downgraded in prose before public release."
  )
  writeLines(memo_lines, red_team_memo_path)
})
