source("../../../setup_environment/code/packages.R")

library(data.table)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/project_cleanup_audit/code")

style_dt <- fread("../output/task_style_conformance_after.csv")
tex_usage <- fread("../output/active_tex_task_usage.csv")
task_summary <- fread("../output/task_cleanup_summary.csv")
paper_queue <- fread("../output/paper_pipeline_cleanup_queue.csv")

if (!("tex_group" %in% names(tex_usage)) && "tex_file" %in% names(tex_usage)) {
  tex_usage[, tex_group := fifelse(startsWith(tex_file, "paper/"), "paper", "slides")]
}

tex_summary <- tex_usage[, .(
  n_tex_refs = sum(n_refs, na.rm = TRUE),
  n_paper_refs = sum(n_refs[tex_group == "paper"], na.rm = TRUE),
  n_slide_refs = sum(n_refs[tex_group == "slides"], na.rm = TRUE)
), by = task]

paper_queue <- paper_queue[, .(
  task,
  paper_pipeline_rank = cleanup_rank,
  branch_bucket,
  min_steps_to_paper,
  paper_root_tasks
)]

queue_dt <- merge(style_dt, tex_summary, by = "task", all.x = TRUE)
queue_dt <- merge(queue_dt, task_summary, by = "task", all.x = TRUE)
queue_dt <- merge(queue_dt, paper_queue, by = "task", all.x = TRUE)

for (col in c(
  "n_tex_refs", "n_paper_refs", "n_slide_refs",
  "output_files", "keep_tex", "keep_downstream", "keep_canonical",
  "prune_stale", "prune_size_gb", "paper_pipeline_rank", "min_steps_to_paper",
  "script_count", "total_script_lines", "max_script_lines", "scripts_ge_500_lines",
  "scripts_ge_1000_lines", "diagnostic_keyword_lines", "scripts_with_diagnostic_keywords",
  "output_write_calls", "makefile_line_count", "all_output_family_count"
)) {
  queue_dt[is.na(get(col)), (col) := 0L]
}

queue_dt[is.na(branch_bucket), branch_bucket := "remaining_active"]
queue_dt[is.na(paper_root_tasks), paper_root_tasks := ""]
queue_dt[is.na(production_split_flag), production_split_flag := FALSE]
queue_dt[is.na(audit_named_task_outside_audits), audit_named_task_outside_audits := FALSE]
queue_dt[is.na(all_output_families), all_output_families := ""]

queue_dt[, cleanup_stage := fifelse(
  n_paper_refs > 0L,
  "paper_facing",
  fifelse(paper_pipeline_rank > 0L, "paper_ancestor", "remaining_active")
)]

queue_dt[, recommended_batch := fcase(
  task %chin% c(
    "create_event_study_permit_data",
    "create_event_study_rental_data_disaggregate",
    "create_event_study_sales_data_disaggregate",
    "run_event_study_permit",
    "run_event_study_rental_disaggregate",
    "run_event_study_sales_disaggregate"
  ),
  "Batch 1: Event-study producers",
  grepl("rental|rent|sales|sale|price_rd", task),
  "Batch 2: Rental/sales/price RD",
  task %chin% c(
    "border_pair_FE_regressions",
    "nonparametric_rd_density_linear_display",
    "nonparametric_rd_density_donut",
    "nonparametric_rd_density_placebo",
    "nonparametric_rd_density_gap_split",
    "density_amenity_balance",
    "density_corner_clean_table",
    "new_construction_density_robustness"
  ),
  "Batch 3: Density/spatial RD",
  task %chin% c(
    "data_for_alderman_uncertainty_index",
    "create_alderman_uncertainty_index",
    "new_construction_score_variants",
    "new_construction_score_comparison",
    "permit_stringency_table",
    "within_ward_strictness",
    "uncertainty_score_density_robustness"
  ),
  "Batch 4: Stringency/validation",
  task %chin% c(
    "calculate_rent_distances",
    "calculate_sale_distances",
    "assign_segment_ids",
    "assign_segment_ids_sales_rental",
    "border_segment_creation",
    "calculate_ward_boundary_distances",
    "process_rent_data",
    "commercial_value_data_cleaning"
  ),
  "Batch 5: Raw/geography/distance",
  default = "Batch 6: Remaining active"
)]

queue_dt[, cleanup_score :=
  100000L * as.integer(cleanup_stage == "paper_facing") +
  50000L * as.integer(cleanup_stage == "paper_ancestor") +
  20000L * as.integer(production_split_flag == TRUE) +
  5000L * default_sidecar_output_count +
  4000L * fixed_path_cli_arg_cmds +
  2500L * scripts_ge_1000_lines +
  1500L * scripts_ge_500_lines +
  1000L * cli_scripts_gt2_local_helpers +
  500L * as.integer(audit_named_task_outside_audits == TRUE) +
  250L * pmin(diagnostic_keyword_lines, 50L) +
  250L * pmin(output_write_calls, 20L) +
  100L * pmax(all_output_family_count - 1L, 0L) +
  500L * scripts_with_file_exists +
  100L * all_named_local_functions +
  50L * file_exists_refs +
  25L * non_lib_source_calls +
  10L * single_use_task_helper_files +
  5L * prune_stale
]

queue_dt[, cleanup_reason := vapply(seq_len(.N), function(i) {
  reasons <- character()
  if (queue_dt$n_paper_refs[i] > 0L) {
    reasons <- c(reasons, sprintf("%d paper refs", queue_dt$n_paper_refs[i]))
  } else if (queue_dt$paper_pipeline_rank[i] > 0L) {
    reasons <- c(reasons, "paper pipeline ancestor")
  }
  if (queue_dt$default_sidecar_output_count[i] > 0L) {
    reasons <- c(reasons, sprintf("%d sidecar/QC outputs in default all", queue_dt$default_sidecar_output_count[i]))
  }
  if (queue_dt$production_split_flag[i] == TRUE) {
    split_reasons <- character()
    if (queue_dt$max_script_lines[i] > 500L) {
      split_reasons <- c(split_reasons, sprintf("max script %d lines", queue_dt$max_script_lines[i]))
    }
    if (queue_dt$makefile_line_count[i] > 50L) {
      split_reasons <- c(split_reasons, sprintf("Makefile %d lines", queue_dt$makefile_line_count[i]))
    }
    if (queue_dt$all_output_family_count[i] > 1L) {
      split_reasons <- c(split_reasons, sprintf("%d output families", queue_dt$all_output_family_count[i]))
    }
    reasons <- c(reasons, sprintf("split candidate (%s)", paste(split_reasons, collapse = ", ")))
  }
  if (queue_dt$audit_named_task_outside_audits[i] == TRUE) {
    reasons <- c(reasons, "audit/diagnostic-like task outside tasks/audits")
  }
  if (queue_dt$fixed_path_cli_arg_cmds[i] > 0L) {
    reasons <- c(reasons, sprintf("%d recipe commands pass fixed paths", queue_dt$fixed_path_cli_arg_cmds[i]))
  }
  if (queue_dt$diagnostic_keyword_lines[i] > 0L) {
    reasons <- c(reasons, sprintf("%d diagnostic/QC keyword lines", queue_dt$diagnostic_keyword_lines[i]))
  }
  if (queue_dt$output_write_calls[i] > 0L) {
    reasons <- c(reasons, sprintf("%d write calls", queue_dt$output_write_calls[i]))
  }
  if (queue_dt$file_exists_refs[i] > 0L) {
    reasons <- c(reasons, sprintf("%d file.exists refs", queue_dt$file_exists_refs[i]))
  }
  if (queue_dt$cli_scripts_gt2_local_helpers[i] > 0L) {
    reasons <- c(reasons, sprintf("%d helper-heavy CLI scripts", queue_dt$cli_scripts_gt2_local_helpers[i]))
  }
  if (queue_dt$all_named_local_functions[i] > 0L) {
    reasons <- c(reasons, sprintf("%d local helper defs", queue_dt$all_named_local_functions[i]))
  }
  if (queue_dt$prune_stale[i] > 0L) {
    reasons <- c(reasons, sprintf("%d stale outputs flagged", queue_dt$prune_stale[i]))
  }
  if (length(reasons) == 0L) {
    "already clean under current audit"
  } else {
    paste(reasons, collapse = "; ")
  }
}, character(1))]

stage_rank <- c(paper_facing = 1L, paper_ancestor = 2L, remaining_active = 3L)
queue_dt[, cleanup_stage_rank := stage_rank[cleanup_stage]]
setorder(queue_dt, cleanup_stage_rank, -cleanup_score, recommended_batch, task)
queue_dt[, cleanup_rank := seq_len(.N)]
queue_dt[, cleanup_stage_rank := NULL]

setcolorder(queue_dt, c(
  "cleanup_rank", "recommended_batch", "cleanup_stage", "task", "cleanup_score",
  "cleanup_reason", "n_paper_refs", "n_slide_refs", "paper_pipeline_rank",
  "branch_bucket", "min_steps_to_paper", "paper_root_tasks",
  "output_files", "keep_tex", "keep_downstream", "keep_canonical", "prune_stale",
  "makefile_line_count", "script_count", "total_script_lines", "max_script_lines",
  "scripts_ge_500_lines", "scripts_ge_1000_lines", "diagnostic_keyword_lines",
  "scripts_with_diagnostic_keywords", "output_write_calls", "all_output_family_count",
  "all_output_families", "production_split_flag", "audit_named_task_outside_audits",
  "default_sidecar_output_count", "fixed_path_cli_arg_cmds",
  "scripts_with_file_exists", "file_exists_refs",
  "all_named_local_functions", "named_local_functions", "cli_scripts_gt2_local_helpers",
  "non_lib_source_calls", "single_use_task_helper_files"
))

fwrite(queue_dt, "../output/repo_cleanup_queue.csv")

lines <- c(
  "# Repo Cleanup Queue",
  "",
  sprintf("- Tasks ranked: %d", nrow(queue_dt)),
  sprintf("- Paper-facing tasks: %d", queue_dt[cleanup_stage == "paper_facing", .N]),
  sprintf("- Paper-ancestor tasks: %d", queue_dt[cleanup_stage == "paper_ancestor", .N]),
  sprintf("- Remaining active tasks: %d", queue_dt[cleanup_stage == "remaining_active", .N]),
  sprintf("- Split candidates: %d", queue_dt[production_split_flag == TRUE, .N]),
  sprintf("- Scripts >=500 lines: %d", sum(queue_dt$scripts_ge_500_lines, na.rm = TRUE)),
  sprintf("- Scripts >=1,000 lines: %d", sum(queue_dt$scripts_ge_1000_lines, na.rm = TRUE)),
  sprintf("- Diagnostic/QC keyword lines: %d", sum(queue_dt$diagnostic_keyword_lines, na.rm = TRUE)),
  "",
  "| Rank | Batch | Stage | Task | Paper Refs | Max Script Lines | Makefile Lines | Product Families | Diagnostic Lines | Write Calls | Sidecar `all` Outputs | Fixed Path CLI Cmds | File Exists Refs | Local Functions | Stale Outputs | Reason |",
  "|---:|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|"
)

for (i in seq_len(min(nrow(queue_dt), 120L))) {
  rr <- queue_dt[i]
  lines <- c(lines, sprintf(
    "| %d | %s | %s | %s | %d | %d | %d | %d | %d | %d | %d | %d | %d | %d | %d | %s |",
    rr$cleanup_rank,
    rr$recommended_batch,
    rr$cleanup_stage,
    rr$task,
    rr$n_paper_refs,
    rr$max_script_lines,
    rr$makefile_line_count,
    rr$all_output_family_count,
    rr$diagnostic_keyword_lines,
    rr$output_write_calls,
    rr$default_sidecar_output_count,
    rr$fixed_path_cli_arg_cmds,
    rr$file_exists_refs,
    rr$all_named_local_functions,
    rr$prune_stale,
    rr$cleanup_reason
  ))
}

writeLines(lines, "../output/repo_cleanup_queue.md")
cat("Saved: ../output/repo_cleanup_queue.csv\n")
cat("Saved: ../output/repo_cleanup_queue.md\n")
