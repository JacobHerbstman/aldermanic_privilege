source("../../setup_environment/code/packages.R")

library(data.table)

# Interactive Test Block
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/project_cleanup_audit/code")
# Rscript build_task_style_conformance.R after ../output/task_style_conformance_after.csv

root_dir <- normalizePath(file.path("..", "..", ".."))
tasks_dir <- file.path(root_dir, "tasks")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript build_task_style_conformance.R <snapshot_label> <output_csv>", call. = FALSE)
}

snapshot_label <- args[1]
out_csv <- args[2]

list_active_tasks <- function() {
  dirs <- list.dirs(tasks_dir, recursive = FALSE, full.names = TRUE)
  dirs <- dirs[dir.exists(file.path(dirs, "code"))]
  dirs <- dirs[!grepl("/(archive|_deprecated|_archived_outputs)$", dirs)]
  basename(dirs)
}

parse_makefile_targets <- function(path) {
  txt <- readLines(path, warn = FALSE)
  targets <- character()
  comment_count <- 0L
  recursive_make_refs <- 0L
  path_alias_count <- 0L

  for (ln in txt) {
    s <- trimws(ln)
    if (startsWith(s, "#")) {
      comment_count <- comment_count + 1L
    }
    if (grepl("\\$\\(MAKE\\)\\s+-C\\s+\\.\\./\\.\\.", ln) || grepl("make\\s+-C\\s+\\.\\./\\.\\.", ln)) {
      recursive_make_refs <- recursive_make_refs + 1L
    }
    if (grepl("^[A-Z][A-Z0-9_]+\\s*[:?+]?=\\s*(\\.\\./|\\$\\(wildcard\\s+\\.\\./)", ln)) {
      path_alias_count <- path_alias_count + 1L
    }
    if (grepl("^[A-Za-z0-9_./%$(){}\\-]+\\s*:(?!=)", ln, perl = TRUE)) {
      target <- trimws(strsplit(ln, ":", fixed = TRUE)[[1]][1])
      if (nchar(target) > 0 && !startsWith(target, ".") && !startsWith(target, "define")) {
        targets <- c(targets, target)
      }
    }
  }

  targets <- unique(targets)
  explicit_non_file_targets <- targets[!grepl("^(all|link-inputs)$", targets) & !grepl("/|\\$\\(", targets)]

  list(
    target_count = length(targets),
    comment_count = comment_count,
    recursive_make_refs = recursive_make_refs,
    path_alias_count = path_alias_count,
    explicit_non_file_targets = paste(explicit_non_file_targets, collapse = ";")
  )
}

script_rows <- rbindlist(lapply(list_active_tasks(), function(task) {
  files <- list.files(file.path(tasks_dir, task, "code"), pattern = "\\.R$", full.names = TRUE)
  if (length(files) == 0) {
    return(NULL)
  }

  rbindlist(lapply(files, function(f) {
    txt <- readLines(f, warn = FALSE)
    body <- paste(txt, collapse = "\n")
    uses_cli <- grepl("commandArgs\\(trailingOnly\\s*=\\s*TRUE\\)", body)
    has_block <- grepl("Interactive Test Block", body)
    has_setwd <- any(grepl("^#\\s*setwd\\(", txt))
    has_rscript <- any(grepl("^#\\s*Rscript\\s+.+\\.R", txt))
    has_placeholder_setwd <- any(grepl('^#\\s*setwd\\([^)]*tasks/"task"/code', txt))

    data.table(
      snapshot_label = snapshot_label,
      task = task,
      file = gsub(paste0("^", root_dir, "/"), "", f),
      uses_commandArgs = uses_cli,
      has_interactive_block = has_block,
      has_comment_setwd = has_setwd,
      has_comment_rscript = has_rscript,
      has_placeholder_setwd = has_placeholder_setwd,
      cli_header_compliant = !uses_cli || (has_block && has_setwd && has_rscript && !has_placeholder_setwd)
    )
  }), fill = TRUE)
}), fill = TRUE)

make_rows <- rbindlist(lapply(list_active_tasks(), function(task) {
  mk <- file.path(tasks_dir, task, "code", "Makefile")
  if (!file.exists(mk)) {
    return(data.table(
      snapshot_label = snapshot_label,
      task = task,
      makefile = NA_character_,
      makefile_exists = FALSE,
      target_count = NA_integer_,
      comment_count = NA_integer_,
      recursive_make_refs = NA_integer_,
      path_alias_count = NA_integer_,
      explicit_non_file_targets = NA_character_
    ))
  }

  parsed <- parse_makefile_targets(mk)
  data.table(
    snapshot_label = snapshot_label,
    task = task,
    makefile = gsub(paste0("^", root_dir, "/"), "", mk),
    makefile_exists = TRUE,
    target_count = as.integer(parsed$target_count),
    comment_count = as.integer(parsed$comment_count),
    recursive_make_refs = as.integer(parsed$recursive_make_refs),
    path_alias_count = as.integer(parsed$path_alias_count),
    explicit_non_file_targets = parsed$explicit_non_file_targets
  )
}), fill = TRUE)

summary_rows <- script_rows[, .(
  cli_scripts = sum(uses_commandArgs, na.rm = TRUE),
  cli_scripts_compliant = sum(uses_commandArgs & cli_header_compliant, na.rm = TRUE),
  cli_scripts_missing_rscript = sum(uses_commandArgs & !has_comment_rscript, na.rm = TRUE),
  cli_scripts_placeholder_setwd = sum(uses_commandArgs & has_placeholder_setwd, na.rm = TRUE)
), by = task]

out <- merge(make_rows, summary_rows, by = "task", all.x = TRUE)
out[is.na(cli_scripts), `:=`(
  cli_scripts = 0L,
  cli_scripts_compliant = 0L,
  cli_scripts_missing_rscript = 0L,
  cli_scripts_placeholder_setwd = 0L
)]

setorder(out, task)
fwrite(out, out_csv)
cat(sprintf("Saved: %s\n", out_csv))
