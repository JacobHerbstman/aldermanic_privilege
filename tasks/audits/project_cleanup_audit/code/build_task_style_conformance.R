source("../../../setup_environment/code/packages.R")

library(data.table)
library(stringr)


root_dir <- normalizePath(file.path("..", "..", "..", ".."))
tasks_dir <- file.path(root_dir, "tasks")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/project_cleanup_audit/code")
# snapshot_label <- "after"
# out_csv <- "../output/task_style_conformance_after.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(snapshot_label, out_csv)
}

if (length(args) < 2) {
  stop("Usage: Rscript build_task_style_conformance.R <snapshot_label> <output_csv>", call. = FALSE)
}

snapshot_label <- args[1]
out_csv <- args[2]
packages_path <- normalizePath(
  file.path(tasks_dir, "setup_environment", "code", "packages.R"),
  winslash = "/",
  mustWork = FALSE
)

sidecar_output_pattern <- paste(c(
  "audit",
  "coverage",
  "diagnostic",
  "diagnostics",
  "metadata",
  "support",
  "pretrend",
  "coefficient",
  "coefficients",
  "rank_change",
  "rank_changes",
  "stability",
  "review",
  "manifest",
  "qc",
  "validation",
  "unmatched",
  "contract",
  "assignment",
  "multiplicity",
  "sensitivity",
  "robustness",
  "placebo",
  "permutation",
  "comparison",
  "year_diagnostics",
  "sample_summary"
), collapse = "|")

diagnostic_keyword_pattern <- paste(c(
  "audit",
  "coverage",
  "diagnostic",
  "diagnostics",
  "metadata",
  "support",
  "pretrend",
  "coefficient",
  "coefficients",
  "rank_change",
  "rank_changes",
  "stability",
  "review",
  "manifest",
  "qc",
  "validation",
  "validate",
  "unmatched",
  "contract",
  "assignment",
  "multiplicity",
  "sensitivity",
  "robustness",
  "placebo",
  "permutation",
  "comparison",
  "sample_summary"
), collapse = "|")

output_write_call_pattern <- paste(c(
  "write_csv\\s*\\(",
  "write_tsv\\s*\\(",
  "write_parquet\\s*\\(",
  "write_rds\\s*\\(",
  "write_feather\\s*\\(",
  "write\\.csv\\s*\\(",
  "write\\.dta\\s*\\(",
  "fwrite\\s*\\(",
  "st_write\\s*\\(",
  "write_sf\\s*\\(",
  "ggsave\\s*\\(",
  "pdf\\s*\\(",
  "png\\s*\\(",
  "jpeg\\s*\\(",
  "tiff\\s*\\(",
  "saveRDS\\s*\\(",
  "writeLines\\s*\\("
), collapse = "|")

audit_task_name_pattern <- "(^|_)(audit|diagnostic|diagnostics|validation|validate|qc)($|_)"

classify_output_family <- function(paths) {
  if (length(paths) == 0) {
    return(character())
  }

  base <- basename(paths)
  ext <- tolower(tools::file_ext(base))

  fifelse(
    grepl(sidecar_output_pattern, base, ignore.case = TRUE),
    "audit_qc",
    fifelse(
      ext %chin% c("pdf", "png", "jpg", "jpeg", "svg"),
      "figure",
      fifelse(
        ext %chin% c("tex"),
        "table",
        fifelse(
          ext %chin% c("parquet", "gpkg", "rds", "rda", "csv", "tsv", "feather", "shp", "geojson"),
          "data",
          "other"
        )
      )
    )
  )
}

list_active_tasks <- function() {
  dirs <- list.dirs(tasks_dir, recursive = FALSE, full.names = TRUE)
  dirs <- dirs[dir.exists(file.path(dirs, "code"))]
  dirs <- dirs[!grepl("/(archive|_deprecated|_archived_outputs|audits)$", dirs)]
  basename(dirs)
}

parse_task <- function(path) {
  parts <- strsplit(path, .Platform$file.sep, fixed = TRUE)[[1]]
  idx <- match("tasks", parts)
  if (is.na(idx) || idx + 1 > length(parts)) return(NA_character_)
  parts[idx + 1]
}

parse_makefile_targets <- function(path) {
  txt <- readLines(path, warn = FALSE)
  targets <- character()
  comment_count <- 0L
  recursive_make_refs <- 0L
  path_alias_count <- 0L
  all_outputs <- character()
  recipe_cli_lines <- character()
  fixed_path_cli_lines <- character()

  all_idx <- grep("^all\\s*:", txt)
  if (length(all_idx) > 0) {
    all_lines <- character()
    j <- all_idx[1]
    repeat {
      all_lines <- c(all_lines, txt[j])
      if (!grepl("\\\\\\s*$", txt[j]) || j >= length(txt)) {
        break
      }
      j <- j + 1L
    }
    all_text <- paste(all_lines, collapse = " ")
    all_outputs <- unique(str_extract_all(all_text, "\\.\\./output/[^[:space:]\\\\]+")[[1]])
  }

  for (ln in txt) {
    s <- trimws(ln)
    if (startsWith(s, "#")) {
      comment_count <- comment_count + 1L
    }
    if (startsWith(ln, "\t") && grepl("\\b(Rscript|python3|python|julia)\\b", ln)) {
      recipe_cli_lines <- c(recipe_cli_lines, s)
      if (grepl(
        "(\\.\\./(input|output|temp)/|\\$@|\\$\\$@|\\b[A-Z0-9_]*(INPUT|OUTPUT|PATH|SUMMARY|DIAGNOSTIC|METADATA|SUPPORT|COVERAGE)[A-Z0-9_]*=)",
        s,
        perl = TRUE
      )) {
        fixed_path_cli_lines <- c(fixed_path_cli_lines, s)
      }
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
  sidecar_outputs <- all_outputs[grepl(sidecar_output_pattern, basename(all_outputs), ignore.case = TRUE)]
  all_output_families <- unique(classify_output_family(all_outputs))

  list(
    makefile_line_count = length(txt),
    target_count = length(targets),
    comment_count = comment_count,
    recursive_make_refs = recursive_make_refs,
    path_alias_count = path_alias_count,
    explicit_non_file_targets = paste(explicit_non_file_targets, collapse = ";"),
    all_output_count = length(all_outputs),
    all_output_family_count = length(all_output_families),
    all_output_families = paste(all_output_families, collapse = ";"),
    default_sidecar_output_count = length(sidecar_outputs),
    default_sidecar_outputs = paste(sidecar_outputs, collapse = ";"),
    recipe_cli_command_count = length(recipe_cli_lines),
    fixed_path_cli_arg_cmds = length(fixed_path_cli_lines),
    fixed_path_cli_arg_examples = paste(head(fixed_path_cli_lines, 3), collapse = " || ")
  )
}

parse_cli_var_name <- function(body) {
  cli_match <- str_match(
    body,
    "(?m)^\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*<-\\s*commandArgs\\(trailingOnly\\s*=\\s*TRUE\\)"
  )
  if (nrow(cli_match) == 0 || is.na(cli_match[1, 2])) {
    return(NA_character_)
  }
  cli_match[1, 2]
}

parse_expected_cli_vars <- function(body, cli_var_name) {
  if (is.na(cli_var_name) || !nzchar(cli_var_name)) {
    return(character())
  }

  fallback_match <- str_match(
    body,
    sprintf(
      "(?s)if\\s*\\(\\s*length\\(%s\\)\\s*==\\s*0\\s*\\)\\s*\\{\\s*%s\\s*<-\\s*c\\((.*?)\\)\\s*\\}",
      cli_var_name,
      cli_var_name
    )
  )

  if (nrow(fallback_match) > 0 && !is.na(fallback_match[1, 2]) && nzchar(trimws(fallback_match[1, 2]))) {
    raw_vars <- strsplit(fallback_match[1, 2], ",", fixed = TRUE)[[1]]
    raw_vars <- trimws(gsub("\\s+", " ", raw_vars))
    raw_vars <- raw_vars[nzchar(raw_vars)]
    return(raw_vars)
  }

  assign_matches <- str_match_all(
    body,
    sprintf(
      "(?m)^\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*<-\\s*.*\\b%s\\[(\\d+)\\].*$",
      cli_var_name
    )
  )[[1]]

  if (nrow(assign_matches) == 0) {
    return(character())
  }

  assign_dt <- data.table(
    var_name = assign_matches[, 2],
    arg_index = as.integer(assign_matches[, 3])
  )
  setorder(assign_dt, arg_index)
  unique(assign_dt$var_name)
}

script_rows <- rbindlist(lapply(list_active_tasks(), function(task) {
  files <- list.files(file.path(tasks_dir, task, "code"), pattern = "\\.(R|py|do)$", full.names = TRUE)
  if (length(files) == 0) {
    return(NULL)
  }

  rbindlist(lapply(files, function(f) {
    txt <- readLines(f, warn = FALSE)
    body <- paste(txt, collapse = "\n")
    uses_cli <- grepl("commandArgs\\(trailingOnly\\s*=\\s*TRUE\\)", body)
    cli_var_name <- parse_cli_var_name(body)
    expected_cli_vars <- parse_expected_cli_vars(body, cli_var_name)
    cmdargs_line <- which(grepl("commandArgs\\(trailingOnly\\s*=\\s*TRUE\\)", txt))[1]
    header_lines <- if (!is.na(cmdargs_line) && cmdargs_line > 1) txt[seq_len(cmdargs_line - 1)] else character()
    comment_assignment_matches <- str_match(header_lines, "^#\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*<-\\s*.+$")
    comment_assignment_vars <- unique(comment_assignment_matches[, 2])
    comment_assignment_vars <- comment_assignment_vars[!is.na(comment_assignment_vars)]
    missing_comment_cli_vars <- setdiff(expected_cli_vars, comment_assignment_vars)
    has_block <- grepl("Interactive Test Block", body)
    has_setwd <- any(grepl("^#\\s*setwd\\(", txt))
    has_placeholder_setwd <- any(grepl('^#\\s*setwd\\([^)]*tasks/"task"/code', txt))
    has_exists_fallback <- grepl("(?m)^\\s*if\\s*\\(.*exists\\s*\\(", body, perl = TRUE)
    file_exists_refs <- sum(grepl("file\\.exists\\s*\\(", txt))
    local_function_defs <- sum(grepl("^\\s*[A-Za-z.][A-Za-z0-9._]*\\s*<-\\s*function\\s*\\(", txt))
    diagnostic_keyword_lines <- sum(grepl(diagnostic_keyword_pattern, txt, ignore.case = TRUE))
    output_write_calls <- sum(grepl(output_write_call_pattern, txt))

    source_matches <- str_match_all(
      body,
      "source\\(\\s*(?:file\\s*=\\s*)?[\"']([^\"']+)[\"']"
    )[[1]]
    source_refs <- if (nrow(source_matches) == 0) character() else unique(source_matches[, 2])
    source_refs <- source_refs[nzchar(source_refs)]
    source_abs <- if (length(source_refs) == 0) {
      character()
    } else {
      normalizePath(file.path(dirname(f), source_refs), winslash = "/", mustWork = FALSE)
    }

    is_packages_source <- source_abs == packages_path
    is_lib_source <- grepl("/tasks/_lib/", source_abs)

    data.table(
      snapshot_label = snapshot_label,
      task = task,
      file = gsub(paste0("^", root_dir, "/"), "", f),
      file_abs = normalizePath(f, winslash = "/", mustWork = FALSE),
      script_line_count = length(txt),
      diagnostic_keyword_lines = diagnostic_keyword_lines,
      output_write_calls = output_write_calls,
      uses_commandArgs = uses_cli,
      cli_var_name = cli_var_name,
      interactive_named_var_count = length(expected_cli_vars),
      comment_named_assignment_count = sum(expected_cli_vars %chin% comment_assignment_vars),
      missing_comment_named_assignments = length(missing_comment_cli_vars),
      has_interactive_block = has_block,
      has_comment_setwd = has_setwd,
      has_placeholder_setwd = has_placeholder_setwd,
      has_exists_fallback = has_exists_fallback,
      file_exists_refs = file_exists_refs,
      cli_header_compliant = !uses_cli || (
        has_block &&
          has_setwd &&
          !has_placeholder_setwd &&
          !has_exists_fallback &&
          length(expected_cli_vars) > 0 &&
          length(missing_comment_cli_vars) == 0
      ),
      named_local_functions = local_function_defs,
      non_lib_source_calls = sum(!is_packages_source & !is_lib_source),
      expected_cli_vars = list(expected_cli_vars),
      comment_assignment_vars = list(comment_assignment_vars),
      source_refs = list(source_refs),
      source_abs = list(source_abs)
    )
  }), fill = TRUE)
}), fill = TRUE)

source_rows <- rbindlist(lapply(seq_len(nrow(script_rows)), function(i) {
  refs <- script_rows$source_refs[[i]]
  abs_paths <- script_rows$source_abs[[i]]
  if (length(refs) == 0) {
    return(NULL)
  }

  data.table(
    caller_task = script_rows$task[[i]],
    caller_file = script_rows$file[[i]],
    caller_file_abs = script_rows$file_abs[[i]],
    caller_uses_commandArgs = script_rows$uses_commandArgs[[i]],
    source_ref = refs,
    source_abs = abs_paths
  )
}), fill = TRUE)

if (nrow(source_rows) == 0) {
  helper_usage <- data.table(source_abs = character(), helper_task = character(), n_cli_callers = integer())
} else {
  source_rows[, helper_task := vapply(source_abs, parse_task, character(1))]
  source_rows[, is_packages_source := source_abs == packages_path]
  source_rows[, is_lib_source := grepl("/tasks/_lib/", source_abs)]
  source_rows[, is_task_code_source := grepl("/tasks/[A-Za-z0-9_\\-]+/code/.+\\.R$", source_abs)]

  helper_usage <- unique(
    source_rows[
      caller_uses_commandArgs == TRUE &
        is_task_code_source == TRUE &
        is_packages_source == FALSE &
        is_lib_source == FALSE &
        !is.na(helper_task),
      .(source_abs, helper_task, caller_file_abs)
    ]
  )[, .(n_cli_callers = uniqueN(caller_file_abs)), by = .(source_abs, helper_task)]
}

helper_summary <- helper_usage[, .(
  task_local_helper_files = .N,
  single_use_task_helper_files = sum(n_cli_callers == 1L)
), by = .(task = helper_task)]

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
      explicit_non_file_targets = NA_character_,
      all_output_count = NA_integer_,
      default_sidecar_output_count = NA_integer_,
      default_sidecar_outputs = NA_character_,
      recipe_cli_command_count = NA_integer_,
      fixed_path_cli_arg_cmds = NA_integer_,
      fixed_path_cli_arg_examples = NA_character_,
      all_output_family_count = NA_integer_,
      all_output_families = NA_character_,
      makefile_line_count = NA_integer_
    ))
  }

  parsed <- parse_makefile_targets(mk)
  data.table(
    snapshot_label = snapshot_label,
    task = task,
    makefile = gsub(paste0("^", root_dir, "/"), "", mk),
    makefile_exists = TRUE,
    makefile_line_count = as.integer(parsed$makefile_line_count),
    target_count = as.integer(parsed$target_count),
    comment_count = as.integer(parsed$comment_count),
    recursive_make_refs = as.integer(parsed$recursive_make_refs),
    path_alias_count = as.integer(parsed$path_alias_count),
    explicit_non_file_targets = parsed$explicit_non_file_targets,
    all_output_count = as.integer(parsed$all_output_count),
    all_output_family_count = as.integer(parsed$all_output_family_count),
    all_output_families = parsed$all_output_families,
    default_sidecar_output_count = as.integer(parsed$default_sidecar_output_count),
    default_sidecar_outputs = parsed$default_sidecar_outputs,
    recipe_cli_command_count = as.integer(parsed$recipe_cli_command_count),
    fixed_path_cli_arg_cmds = as.integer(parsed$fixed_path_cli_arg_cmds),
    fixed_path_cli_arg_examples = parsed$fixed_path_cli_arg_examples
  )
}), fill = TRUE)

summary_rows <- script_rows[, .(
  script_count = .N,
  total_script_lines = sum(script_line_count, na.rm = TRUE),
  max_script_lines = max(script_line_count, na.rm = TRUE),
  scripts_ge_500_lines = sum(script_line_count >= 500L, na.rm = TRUE),
  scripts_ge_1000_lines = sum(script_line_count >= 1000L, na.rm = TRUE),
  diagnostic_keyword_lines = sum(diagnostic_keyword_lines, na.rm = TRUE),
  scripts_with_diagnostic_keywords = sum(diagnostic_keyword_lines > 0L, na.rm = TRUE),
  output_write_calls = sum(output_write_calls, na.rm = TRUE),
  cli_scripts = sum(uses_commandArgs, na.rm = TRUE),
  cli_scripts_compliant = sum(uses_commandArgs & cli_header_compliant, na.rm = TRUE),
  cli_scripts_missing_named_assignments = sum(
    uses_commandArgs & (interactive_named_var_count == 0L | missing_comment_named_assignments > 0L),
    na.rm = TRUE
  ),
  cli_scripts_placeholder_setwd = sum(uses_commandArgs & has_placeholder_setwd, na.rm = TRUE),
  cli_scripts_with_exists_fallback = sum(uses_commandArgs & has_exists_fallback, na.rm = TRUE),
  scripts_with_file_exists = sum(file_exists_refs > 0L, na.rm = TRUE),
  file_exists_refs = sum(file_exists_refs, na.rm = TRUE),
  all_named_local_functions = sum(named_local_functions, na.rm = TRUE),
  named_local_functions = sum(fifelse(uses_commandArgs, named_local_functions, 0L), na.rm = TRUE),
  cli_scripts_gt2_local_helpers = sum(uses_commandArgs & named_local_functions > 2L, na.rm = TRUE),
  non_lib_source_calls = sum(fifelse(uses_commandArgs, non_lib_source_calls, 0L), na.rm = TRUE)
), by = task]

out <- merge(make_rows, summary_rows, by = "task", all.x = TRUE)
out <- merge(out, helper_summary, by = "task", all.x = TRUE)
out[is.na(cli_scripts), `:=`(
  script_count = 0L,
  total_script_lines = 0L,
  max_script_lines = 0L,
  scripts_ge_500_lines = 0L,
  scripts_ge_1000_lines = 0L,
  diagnostic_keyword_lines = 0L,
  scripts_with_diagnostic_keywords = 0L,
  output_write_calls = 0L,
  cli_scripts = 0L,
  cli_scripts_compliant = 0L,
  cli_scripts_missing_named_assignments = 0L,
  cli_scripts_placeholder_setwd = 0L,
  cli_scripts_with_exists_fallback = 0L,
  scripts_with_file_exists = 0L,
  file_exists_refs = 0L,
  all_named_local_functions = 0L,
  named_local_functions = 0L,
  cli_scripts_gt2_local_helpers = 0L,
  non_lib_source_calls = 0L
)]
out[is.na(task_local_helper_files), task_local_helper_files := 0L]
out[is.na(single_use_task_helper_files), single_use_task_helper_files := 0L]
out[, audit_named_task_outside_audits := grepl(audit_task_name_pattern, task)]
out[, makefile_gt50_lines := fifelse(is.na(makefile_line_count), FALSE, makefile_line_count > 50L)]
out[, production_split_flag := (
  max_script_lines > 500L |
    makefile_gt50_lines |
    fifelse(is.na(all_output_family_count), FALSE, all_output_family_count > 1L)
)]

setorder(out, task)
fwrite(out, out_csv)
cat(sprintf("Saved: %s\n", out_csv))
