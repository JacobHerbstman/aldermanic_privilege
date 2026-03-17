# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/project_cleanup_audit/code")
# Rscript capture_task_snapshot.R ../output/post_cleanup_snapshot.csv

source("../../setup_environment/code/packages.R")

library(data.table)

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1)

root_dir <- normalizePath(file.path("..", "..", ".."))
tasks_dir <- file.path(root_dir, "tasks")

excluded_tasks <- c("archive", "_deprecated", "_archived_outputs")

task_dirs <- list.dirs(tasks_dir, recursive = FALSE, full.names = TRUE)
task_dirs <- task_dirs[basename(task_dirs) %chin% excluded_tasks == FALSE]
task_dirs <- task_dirs[!startsWith(basename(task_dirs), "_")]

snapshot <- rbindlist(lapply(task_dirs, function(task_dir) {
  out_dir <- file.path(task_dir, "output")
  code_dir <- file.path(task_dir, "code")
  makefile <- file.path(code_dir, "Makefile")

  out_files <- if (dir.exists(out_dir)) {
    files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
    sum(file.info(files)$isdir %in% FALSE)
  } else {
    0L
  }

  code_files <- if (dir.exists(code_dir)) {
    files <- list.files(code_dir, recursive = TRUE, full.names = TRUE)
    sum(file.info(files)$isdir %in% FALSE)
  } else {
    0L
  }

  data.table(
    task = basename(task_dir),
    output_files = out_files,
    code_files = code_files,
    make_lines = if (file.exists(makefile)) length(readLines(makefile, warn = FALSE)) else NA_integer_
  )
}), fill = TRUE)

setorder(snapshot, task)
fwrite(snapshot, args[1])

cat("Saved:\n")
cat(sprintf(" - %s\n", args[1]))
