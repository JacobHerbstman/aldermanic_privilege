
source("../../setup_environment/code/packages.R")

library(data.table)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/project_cleanup_audit/code")
# before <- "../output/pre_cleanup_snapshot.csv"
# after <- "../output/post_cleanup_snapshot.csv"
# summary_dt <- "../output/task_cleanup_summary.csv"
# out_md <- "../output/cleanup_delta.md"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(before, after, summary_dt, out_md)
}

stopifnot(length(args) == 4)

before <- fread(args[1])
after <- fread(args[2])
summary_dt <- fread(args[3])
out_md <- args[4]

delta <- merge(before, after, by = "task", all = TRUE, suffixes = c("_before", "_after"))
for (col in c("output_files_before", "output_files_after", "code_files_before", "code_files_after", "make_lines_before", "make_lines_after")) {
  delta[is.na(get(col)), (col) := 0L]
}
delta[, output_delta := output_files_after - output_files_before]
delta[, code_delta := code_files_after - code_files_before]
delta[, make_line_delta := make_lines_after - make_lines_before]

setorder(summary_dt, -prune_stale, task)
setorder(delta, output_delta, task)

lines <- c(
  "# Cleanup Delta",
  "",
  "| Task | Outputs Before | Outputs After | Delta | Code Before | Code After | Delta | Make Lines Before | Make Lines After | Delta | Pruned |",
  "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
)

delta <- merge(delta, summary_dt[, .(task, prune_stale)], by = "task", all.x = TRUE)
delta[is.na(prune_stale), prune_stale := 0L]

for (i in seq_len(nrow(delta))) {
  rr <- delta[i]
  lines <- c(
    lines,
    sprintf(
      "| %s | %d | %d | %d | %d | %d | %d | %d | %d | %d | %d |",
      rr$task,
      rr$output_files_before,
      rr$output_files_after,
      rr$output_delta,
      rr$code_files_before,
      rr$code_files_after,
      rr$code_delta,
      rr$make_lines_before,
      rr$make_lines_after,
      rr$make_line_delta,
      rr$prune_stale
    )
  )
}

writeLines(lines, out_md)

cat("Saved:\n")
cat(sprintf(" - %s\n", out_md))
