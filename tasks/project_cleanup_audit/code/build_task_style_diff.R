source("../../setup_environment/code/packages.R")

library(data.table)

# Interactive Test Block
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/project_cleanup_audit/code")
# Rscript build_task_style_diff.R ../output/task_style_conformance_before.csv ../output/task_style_conformance_after.csv ../output/task_style_diff.md

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript build_task_style_diff.R <before_csv> <after_csv> <out_md>", call. = FALSE)
}

before <- fread(args[1])
after <- fread(args[2])
out_md <- args[3]

key_cols <- c("task")

merged <- merge(before, after, by = key_cols, suffixes = c("_before", "_after"), all = TRUE)

num_cols <- c(
  "target_count", "comment_count", "recursive_make_refs", "path_alias_count",
  "cli_scripts", "cli_scripts_compliant", "cli_scripts_missing_rscript", "cli_scripts_placeholder_setwd"
)

for (col in num_cols) {
  b <- paste0(col, "_before")
  a <- paste0(col, "_after")
  if (!b %in% names(merged)) merged[[b]] <- NA_real_
  if (!a %in% names(merged)) merged[[a]] <- NA_real_
  merged[[paste0(col, "_delta")]] <- as.numeric(merged[[a]]) - as.numeric(merged[[b]])
}

changed <- merged[
  target_count_delta != 0 |
    comment_count_delta != 0 |
    recursive_make_refs_delta != 0 |
    path_alias_count_delta != 0 |
    cli_scripts_missing_rscript_delta != 0 |
    cli_scripts_placeholder_setwd_delta != 0 |
    cli_scripts_compliant_delta != 0,
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
  sprintf("- target_count delta (sum): %.0f", sum(merged$target_count_delta, na.rm = TRUE)),
  sprintf("- comment_count delta (sum): %.0f", sum(merged$comment_count_delta, na.rm = TRUE)),
  sprintf("- recursive_make_refs delta (sum): %.0f", sum(merged$recursive_make_refs_delta, na.rm = TRUE)),
  sprintf("- path_alias_count delta (sum): %.0f", sum(merged$path_alias_count_delta, na.rm = TRUE)),
  sprintf("- cli_scripts_compliant delta (sum): %.0f", sum(merged$cli_scripts_compliant_delta, na.rm = TRUE)),
  sprintf("- cli_scripts_missing_rscript delta (sum): %.0f", sum(merged$cli_scripts_missing_rscript_delta, na.rm = TRUE)),
  sprintf("- cli_scripts_placeholder_setwd delta (sum): %.0f", sum(merged$cli_scripts_placeholder_setwd_delta, na.rm = TRUE)),
  "",
  "## Per-task changes",
  "",
  "| Task | Targets (before->after) | Comments (before->after) | Path aliases (before->after) | CLI compliant (before->after) | Missing Rscript hdr (before->after) | Placeholder setwd (before->after) |",
  "|---|---:|---:|---:|---:|---:|---:|"
)

if (nrow(changed) > 0) {
  for (i in seq_len(nrow(changed))) {
    rr <- changed[i]
    lines <- c(lines, sprintf(
      "| %s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s | %s->%s |",
      rr$task,
      rr$target_count_before, rr$target_count_after,
      rr$comment_count_before, rr$comment_count_after,
      rr$path_alias_count_before, rr$path_alias_count_after,
      rr$cli_scripts_compliant_before, rr$cli_scripts_compliant_after,
      rr$cli_scripts_missing_rscript_before, rr$cli_scripts_missing_rscript_after,
      rr$cli_scripts_placeholder_setwd_before, rr$cli_scripts_placeholder_setwd_after
    ))
  }
}

writeLines(lines, out_md)
cat(sprintf("Saved: %s\n", out_md))
