source("../../../setup_environment/code/packages.R")

library(data.table)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/project_cleanup_audit/code")
# out_csv <- "../output/legacy_task_map.csv"
# out_md <- "../output/legacy_task_map.md"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(out_csv, out_md)
}

if (length(args) < 2) {
  stop("Usage: Rscript build_legacy_task_map.R <out_csv> <out_md>", call. = FALSE)
}

root_dir <- normalizePath(file.path("..", "..", "..", ".."))

legacy_dt <- data.table(
  legacy_item = c(
    "data_for_alderman_strictness_scores",
    "create_alderman_strictness_scores",
    "restrictiveness/strictness score labels in active code",
    "same-zone-only spatial RD variants"
  ),
	current_item = c(
	  "data_for_alderman_uncertainty_index",
	  "create_alderman_uncertainty_index",
	  "stringency/uncertainty labels in active code",
	  "nonparametric_rd_density_linear_display"
	),
  action = c("archive task", "archive task", "rename active labels", "deprecate legacy variants"),
  note = c(
    "Old permit-panel builder replaced by uncertainty-index input builder.",
    "Old score-construction task replaced by the uncertainty/stringency index task.",
    "Active paper-facing nomenclature should no longer mix strictness/restrictiveness with stringency/uncertainty.",
	  "Current active nonparametric RD display task uses the paper-facing density figure workflow; same-zone-only variants are legacy."
	)
)

legacy_dt[, active_path_exists := vapply(legacy_item, function(item) {
  dir.exists(file.path(root_dir, "tasks", item))
}, logical(1))]
legacy_dt[, replacement_path_exists := vapply(current_item, function(item) {
  dir.exists(file.path(root_dir, "tasks", item))
}, logical(1))]

fwrite(legacy_dt, args[1])

lines <- c(
  "# Legacy Task Map",
  "",
  "| Legacy Item | Current Item | Action | Legacy Path Exists | Replacement Path Exists | Note |",
  "|---|---|---|---|---|---|"
)

for (i in seq_len(nrow(legacy_dt))) {
  rr <- legacy_dt[i]
  lines <- c(
    lines,
    sprintf(
      "| %s | %s | %s | %s | %s | %s |",
      rr$legacy_item,
      rr$current_item,
      rr$action,
      ifelse(rr$active_path_exists, "yes", "no"),
      ifelse(rr$replacement_path_exists, "yes", "no"),
      rr$note
    )
  )
}

writeLines(lines, args[2])
cat(sprintf("Saved: %s\n", args[1]))
cat(sprintf("Saved: %s\n", args[2]))
