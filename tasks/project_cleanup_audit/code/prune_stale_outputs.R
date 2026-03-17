# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/project_cleanup_audit/code")
# Rscript prune_stale_outputs.R ../output/pruned_outputs.csv

source("../../setup_environment/code/packages.R")

library(data.table)

args <- commandArgs(trailingOnly = TRUE)
output_csv <- if (length(args) >= 1) args[1] else "../output/pruned_outputs.csv"
root_dir <- normalizePath(file.path("..", "..", ".."))

manifest <- fread("../output/output_keep_manifest.csv")
stale <- manifest[prune_stale == TRUE]

if (nrow(stale) == 0) {
  fwrite(data.table(file = character(), size_bytes = numeric(), status = character()), output_csv)
  quit(save = "no")
}

stale[, abs_path := normalizePath(file.path(root_dir, file), winslash = "/", mustWork = FALSE)]
stale <- stale[grepl("/tasks/[^/]+/output/", abs_path)]

results <- rbindlist(lapply(seq_len(nrow(stale)), function(i) {
  exists_now <- file.exists(stale$abs_path[i])
  if (exists_now) unlink(stale$abs_path[i], force = TRUE)
  deleted <- exists_now && !file.exists(stale$abs_path[i])

  data.table(
    file = stale$file[i],
    size_bytes = stale$size_bytes[i],
    status = if (exists_now && deleted) "deleted" else if (exists_now) "failed" else "missing"
  )
}))

fwrite(results, output_csv)

cat("Saved:\n")
cat(sprintf(" - %s\n", output_csv))
