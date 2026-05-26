source("../../../setup_environment/code/packages.R")

library(data.table)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/project_cleanup_audit/code")
# tex_usage <- "../output/active_tex_task_usage.csv"
# style_dt <- "../output/task_style_conformance_after.csv"
# out_csv <- "../output/task_linearization_priority.csv"
# out_md <- "../output/task_linearization_priority.md"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(tex_usage, style_dt, out_csv, out_md)
}

if (length(args) < 4) {
  stop(
    "Usage: Rscript build_task_linearization_priority.R <active_tex_csv> <style_csv> <out_csv> <out_md>",
    call. = FALSE
  )
}

tex_usage <- fread(args[1])
style_dt <- fread(args[2])
out_csv <- args[3]
out_md <- args[4]

if (!("tex_group" %in% names(tex_usage)) && "tex_file" %in% names(tex_usage)) {
  tex_usage[, tex_group := fifelse(startsWith(tex_file, "paper/"), "paper", "slides")]
}

tex_summary <- if (nrow(tex_usage) == 0) {
  data.table(
    task = character(),
    n_tex_refs = integer(),
    n_tex_files = integer(),
    n_paper_refs = integer(),
    n_slide_refs = integer()
  )
} else {
  tex_usage[, .(
    n_tex_refs = sum(n_refs, na.rm = TRUE),
    n_tex_files = uniqueN(tex_file),
    n_paper_refs = sum(n_refs[tex_group == "paper"], na.rm = TRUE),
    n_slide_refs = sum(n_refs[tex_group == "slides"], na.rm = TRUE)
  ), by = task]
}

priority_dt <- merge(
  style_dt[, .(
    task,
    cli_scripts,
    default_sidecar_output_count,
    fixed_path_cli_arg_cmds,
    scripts_with_file_exists,
    file_exists_refs,
    named_local_functions,
    cli_scripts_gt2_local_helpers,
    non_lib_source_calls,
    task_local_helper_files,
    single_use_task_helper_files
  )],
  tex_summary,
  by = "task",
  all = TRUE
)

for (col in c(
  "cli_scripts", "named_local_functions", "cli_scripts_gt2_local_helpers",
  "default_sidecar_output_count", "fixed_path_cli_arg_cmds",
  "scripts_with_file_exists", "file_exists_refs",
  "non_lib_source_calls", "task_local_helper_files", "single_use_task_helper_files",
  "n_tex_refs", "n_tex_files", "n_paper_refs", "n_slide_refs"
)) {
  priority_dt[is.na(get(col)), (col) := 0L]
}

priority_dt[, priority_stage := fifelse(n_tex_refs > 0L, "paper_slides_first", "remaining_active")]
priority_dt[, priority_score :=
  100000L * as.integer(n_tex_refs > 0L) +
  5000L * default_sidecar_output_count +
  4000L * fixed_path_cli_arg_cmds +
  1000L * cli_scripts_gt2_local_helpers +
  100L * named_local_functions +
  50L * file_exists_refs +
  25L * non_lib_source_calls +
  10L * single_use_task_helper_files +
  5L * n_tex_refs +
  cli_scripts
]

priority_dt[, priority_reason := vapply(seq_len(.N), function(i) {
  reasons <- character()
  if (priority_dt$n_tex_refs[i] > 0L) {
    reasons <- c(
      reasons,
      sprintf(
        "%d paper/slides refs (%d paper, %d slides)",
        priority_dt$n_tex_refs[i],
        priority_dt$n_paper_refs[i],
        priority_dt$n_slide_refs[i]
      )
    )
  }
  if (priority_dt$cli_scripts_gt2_local_helpers[i] > 0L) {
    reasons <- c(
      reasons,
      sprintf("%d CLI scripts with >2 local helpers", priority_dt$cli_scripts_gt2_local_helpers[i])
    )
  }
  if (priority_dt$default_sidecar_output_count[i] > 0L) {
    reasons <- c(reasons, sprintf("%d sidecar/QC outputs in default all", priority_dt$default_sidecar_output_count[i]))
  }
  if (priority_dt$fixed_path_cli_arg_cmds[i] > 0L) {
    reasons <- c(reasons, sprintf("%d recipe commands pass fixed paths", priority_dt$fixed_path_cli_arg_cmds[i]))
  }
  if (priority_dt$file_exists_refs[i] > 0L) {
    reasons <- c(reasons, sprintf("%d file.exists refs", priority_dt$file_exists_refs[i]))
  }
  if (priority_dt$named_local_functions[i] > 0L) {
    reasons <- c(reasons, sprintf("%d local helper defs in CLI scripts", priority_dt$named_local_functions[i]))
  }
  if (priority_dt$non_lib_source_calls[i] > 0L) {
    reasons <- c(reasons, sprintf("%d non-_lib source() calls", priority_dt$non_lib_source_calls[i]))
  }
  if (priority_dt$single_use_task_helper_files[i] > 0L) {
    reasons <- c(reasons, sprintf("%d single-use task helper files", priority_dt$single_use_task_helper_files[i]))
  }
  if (length(reasons) == 0L) {
    "Already fairly linear under current heuristic"
  } else {
    paste(reasons, collapse = "; ")
  }
}, character(1))]

priority_dt[, tex_priority := as.integer(n_tex_refs > 0L)]
setorder(priority_dt, -tex_priority, -priority_score, task)
priority_dt[, priority_rank := seq_len(.N)]
setcolorder(priority_dt, c(
  "priority_rank", "task", "priority_stage", "priority_score", "priority_reason",
  "n_tex_refs", "n_paper_refs", "n_slide_refs", "n_tex_files",
  "cli_scripts", "default_sidecar_output_count", "fixed_path_cli_arg_cmds",
  "scripts_with_file_exists", "file_exists_refs",
  "named_local_functions", "cli_scripts_gt2_local_helpers",
  "non_lib_source_calls", "task_local_helper_files", "single_use_task_helper_files",
  "tex_priority"
))
priority_dt[, tex_priority := NULL]

fwrite(priority_dt, out_csv)

lines <- c(
  "# Task Linearization Priority",
  "",
  sprintf("- Tex usage file: `%s`", args[1]),
  sprintf("- Style file: `%s`", args[2]),
  sprintf("- Tasks ranked: %d", nrow(priority_dt)),
  "",
  "| Rank | Task | Stage | Tex Refs | Sidecar `all` Outputs | Fixed Path CLI Cmds | File Exists Refs | Local Functions | CLI Scripts >2 Helpers | Reason |",
  "|---:|---|---|---:|---:|---:|---:|---:|---:|---|"
)

for (i in seq_len(nrow(priority_dt))) {
  rr <- priority_dt[i]
  lines <- c(
    lines,
    sprintf(
      "| %d | %s | %s | %d | %d | %d | %d | %d | %d | %s |",
      rr$priority_rank,
      rr$task,
      rr$priority_stage,
      rr$n_tex_refs,
      rr$default_sidecar_output_count,
      rr$fixed_path_cli_arg_cmds,
      rr$file_exists_refs,
      rr$named_local_functions,
      rr$cli_scripts_gt2_local_helpers,
      rr$priority_reason
    )
  )
}

writeLines(lines, out_md)
cat(sprintf("Saved: %s\n", out_csv))
cat(sprintf("Saved: %s\n", out_md))
