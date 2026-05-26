source("../../../setup_environment/code/packages.R")

library(data.table)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/project_cleanup_audit/code")
# before_csv <- "../output/task_style_conformance_before.csv"
# after_csv <- "../output/task_style_conformance_after.csv"
# out_md <- "../output/task_style_diff.md"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(before_csv, after_csv, out_md)
}
if (length(args) < 3) {
  stop("Usage: Rscript build_task_style_diff.R <before_csv> <after_csv> <out_md>", call. = FALSE)
}

before <- fread(args[1])
after <- fread(args[2])
out_md <- args[3]

key_cols <- c("task")

merged <- merge(before, after, by = key_cols, suffixes = c("_before", "_after"), all = TRUE)

num_cols <- c(
  "makefile_line_count", "target_count", "comment_count", "recursive_make_refs", "path_alias_count",
  "all_output_count", "default_sidecar_output_count", "recipe_cli_command_count",
  "all_output_family_count", "fixed_path_cli_arg_cmds",
  "script_count", "total_script_lines", "max_script_lines", "scripts_ge_500_lines",
  "scripts_ge_1000_lines", "diagnostic_keyword_lines", "scripts_with_diagnostic_keywords",
  "output_write_calls",
  "cli_scripts", "cli_scripts_compliant", "cli_scripts_missing_named_assignments", "cli_scripts_placeholder_setwd",
  "cli_scripts_with_exists_fallback", "scripts_with_file_exists", "file_exists_refs",
  "all_named_local_functions", "named_local_functions", "cli_scripts_gt2_local_helpers", "non_lib_source_calls",
  "task_local_helper_files", "single_use_task_helper_files"
)

for (col in num_cols) {
  b <- paste0(col, "_before")
  a <- paste0(col, "_after")
  if (!b %in% names(merged)) merged[[b]] <- NA_real_
  if (!a %in% names(merged)) merged[[a]] <- NA_real_
  merged[[paste0(col, "_delta")]] <- as.numeric(merged[[a]]) - as.numeric(merged[[b]])
}

changed <- merged[
  makefile_line_count_delta != 0 |
    target_count_delta != 0 |
    comment_count_delta != 0 |
    recursive_make_refs_delta != 0 |
    path_alias_count_delta != 0 |
    all_output_family_count_delta != 0 |
    default_sidecar_output_count_delta != 0 |
    fixed_path_cli_arg_cmds_delta != 0 |
    total_script_lines_delta != 0 |
    max_script_lines_delta != 0 |
    scripts_ge_500_lines_delta != 0 |
    scripts_ge_1000_lines_delta != 0 |
    diagnostic_keyword_lines_delta != 0 |
    scripts_with_diagnostic_keywords_delta != 0 |
    output_write_calls_delta != 0 |
    scripts_with_file_exists_delta != 0 |
    file_exists_refs_delta != 0 |
    cli_scripts_missing_named_assignments_delta != 0 |
    cli_scripts_placeholder_setwd_delta != 0 |
    cli_scripts_with_exists_fallback_delta != 0 |
    cli_scripts_compliant_delta != 0 |
    all_named_local_functions_delta != 0 |
    named_local_functions_delta != 0 |
    cli_scripts_gt2_local_helpers_delta != 0 |
    non_lib_source_calls_delta != 0 |
    task_local_helper_files_delta != 0 |
    single_use_task_helper_files_delta != 0,
]

lines <- c(
  "# Task Style Conformance Diff",
  "",
  sprintf("- Before file: `%s`", args[1]),
  sprintf("- After file: `%s`", args[2]),
  sprintf("- Tasks compared: %d", nrow(merged)),
  sprintf("- Tasks changed: %d", nrow(changed)),
  "",
  "## Aggregate deltas",
  "",
  sprintf("- makefile_line_count delta (sum): %.0f", sum(merged$makefile_line_count_delta, na.rm = TRUE)),
  sprintf("- target_count delta (sum): %.0f", sum(merged$target_count_delta, na.rm = TRUE)),
  sprintf("- comment_count delta (sum): %.0f", sum(merged$comment_count_delta, na.rm = TRUE)),
  sprintf("- recursive_make_refs delta (sum): %.0f", sum(merged$recursive_make_refs_delta, na.rm = TRUE)),
  sprintf("- path_alias_count delta (sum): %.0f", sum(merged$path_alias_count_delta, na.rm = TRUE)),
  sprintf("- all_output_family_count delta (sum): %.0f", sum(merged$all_output_family_count_delta, na.rm = TRUE)),
  sprintf("- default_sidecar_output_count delta (sum): %.0f", sum(merged$default_sidecar_output_count_delta, na.rm = TRUE)),
  sprintf("- fixed_path_cli_arg_cmds delta (sum): %.0f", sum(merged$fixed_path_cli_arg_cmds_delta, na.rm = TRUE)),
  sprintf("- total_script_lines delta (sum): %.0f", sum(merged$total_script_lines_delta, na.rm = TRUE)),
  sprintf("- scripts_ge_500_lines delta (sum): %.0f", sum(merged$scripts_ge_500_lines_delta, na.rm = TRUE)),
  sprintf("- scripts_ge_1000_lines delta (sum): %.0f", sum(merged$scripts_ge_1000_lines_delta, na.rm = TRUE)),
  sprintf("- diagnostic_keyword_lines delta (sum): %.0f", sum(merged$diagnostic_keyword_lines_delta, na.rm = TRUE)),
  sprintf("- scripts_with_diagnostic_keywords delta (sum): %.0f", sum(merged$scripts_with_diagnostic_keywords_delta, na.rm = TRUE)),
  sprintf("- output_write_calls delta (sum): %.0f", sum(merged$output_write_calls_delta, na.rm = TRUE)),
  sprintf("- scripts_with_file_exists delta (sum): %.0f", sum(merged$scripts_with_file_exists_delta, na.rm = TRUE)),
  sprintf("- file_exists_refs delta (sum): %.0f", sum(merged$file_exists_refs_delta, na.rm = TRUE)),
  sprintf("- cli_scripts_compliant delta (sum): %.0f", sum(merged$cli_scripts_compliant_delta, na.rm = TRUE)),
  sprintf("- cli_scripts_missing_named_assignments delta (sum): %.0f", sum(merged$cli_scripts_missing_named_assignments_delta, na.rm = TRUE)),
  sprintf("- cli_scripts_placeholder_setwd delta (sum): %.0f", sum(merged$cli_scripts_placeholder_setwd_delta, na.rm = TRUE)),
  sprintf("- cli_scripts_with_exists_fallback delta (sum): %.0f", sum(merged$cli_scripts_with_exists_fallback_delta, na.rm = TRUE)),
  sprintf("- all_named_local_functions delta (sum): %.0f", sum(merged$all_named_local_functions_delta, na.rm = TRUE)),
  sprintf("- named_local_functions delta (sum): %.0f", sum(merged$named_local_functions_delta, na.rm = TRUE)),
  sprintf("- cli_scripts_gt2_local_helpers delta (sum): %.0f", sum(merged$cli_scripts_gt2_local_helpers_delta, na.rm = TRUE)),
  sprintf("- non_lib_source_calls delta (sum): %.0f", sum(merged$non_lib_source_calls_delta, na.rm = TRUE)),
  sprintf("- task_local_helper_files delta (sum): %.0f", sum(merged$task_local_helper_files_delta, na.rm = TRUE)),
  sprintf("- single_use_task_helper_files delta (sum): %.0f", sum(merged$single_use_task_helper_files_delta, na.rm = TRUE)),
  "",
  "## Per-task changes",
  "",
  "| Task | Makefile lines | Script lines | Scripts >=500 | Diagnostic lines | Write calls | Targets | Product families | Sidecar `all` outputs | Fixed path CLI cmds | File exists refs | CLI compliant | All local funcs | CLI local funcs | >2 helpers | Non-_lib sources | Single-use helpers | Missing named args | Placeholder setwd | `exists()` fallbacks |",
  "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
)

if (nrow(changed) > 0) {
  for (i in seq_len(nrow(changed))) {
    rr <- changed[i]
    lines <- c(lines, sprintf(
      "| %s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s |",
      rr$task,
      rr$makefile_line_count_before, rr$makefile_line_count_after,
      rr$total_script_lines_before, rr$total_script_lines_after,
      rr$scripts_ge_500_lines_before, rr$scripts_ge_500_lines_after,
      rr$diagnostic_keyword_lines_before, rr$diagnostic_keyword_lines_after,
      rr$output_write_calls_before, rr$output_write_calls_after,
      rr$target_count_before, rr$target_count_after,
      rr$all_output_family_count_before, rr$all_output_family_count_after,
      rr$default_sidecar_output_count_before, rr$default_sidecar_output_count_after,
      rr$fixed_path_cli_arg_cmds_before, rr$fixed_path_cli_arg_cmds_after,
      rr$file_exists_refs_before, rr$file_exists_refs_after,
      rr$cli_scripts_compliant_before, rr$cli_scripts_compliant_after,
      rr$all_named_local_functions_before, rr$all_named_local_functions_after,
      rr$named_local_functions_before, rr$named_local_functions_after,
      rr$cli_scripts_gt2_local_helpers_before, rr$cli_scripts_gt2_local_helpers_after,
      rr$non_lib_source_calls_before, rr$non_lib_source_calls_after,
      rr$single_use_task_helper_files_before, rr$single_use_task_helper_files_after,
      rr$cli_scripts_missing_named_assignments_before, rr$cli_scripts_missing_named_assignments_after,
      rr$cli_scripts_placeholder_setwd_before, rr$cli_scripts_placeholder_setwd_after,
      rr$cli_scripts_with_exists_fallback_before, rr$cli_scripts_with_exists_fallback_after
    ))
  }
}

writeLines(lines, out_md)
cat(sprintf("Saved: %s\n", out_md))
