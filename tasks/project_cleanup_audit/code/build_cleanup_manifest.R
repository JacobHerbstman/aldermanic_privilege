source("../../setup_environment/code/packages.R")

library(data.table)

root_dir <- normalizePath(file.path("..", "..", ".."))
tasks_dir <- file.path(root_dir, "tasks")

excluded_tasks <- c("archive", "_deprecated", "_archived_outputs")
full_keep_task_patterns <- c("^download_", "^building_permits_scraping$", "^project_cleanup_audit$")

task_dirs <- list.dirs(tasks_dir, recursive = FALSE, full.names = TRUE)
task_dirs <- task_dirs[basename(task_dirs) %chin% excluded_tasks == FALSE]
task_dirs <- task_dirs[!startsWith(basename(task_dirs), "_")]

makefiles <- file.path(task_dirs, "code", "Makefile")
makefiles <- makefiles[file.exists(makefiles)]

parse_task <- function(path) {
  parts <- strsplit(path, .Platform$file.sep, fixed = TRUE)[[1]]
  idx <- match("tasks", parts)
  if (is.na(idx) || idx + 1 > length(parts)) return(NA_character_)
  parts[idx + 1]
}

parse_output_refs <- function(text, prefix) {
  hits <- unique(unlist(regmatches(text, gregexpr(prefix, text, perl = TRUE))))
  hits[nzchar(hits)]
}

normalize_refs <- function(base_dir, refs) {
  if (length(refs) == 0) return(character())
  normalizePath(file.path(base_dir, refs), winslash = "/", mustWork = FALSE)
}

extract_all_outputs <- function(mk) {
  code_dir <- dirname(mk)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(code_dir)

  db <- tryCatch(
    suppressWarnings(system2("make", c("-pn", "all"), stdout = TRUE, stderr = TRUE)),
    error = function(e) character()
  )
  if (length(db) == 0) return(character())

  all_line <- grep("^all:", db, value = TRUE)
  if (length(all_line) == 0) return(character())
  all_line <- all_line[1]

  lhs <- trimws(sub("^all:\\s*", "", all_line))
  refs <- trimws(strsplit(lhs, "\\s+")[[1]])
  refs <- refs[startsWith(refs, "../output/")]
  refs <- refs[!grepl("[%$(){}]", refs)]

  normalize_refs(code_dir, refs)
}

edge_rows <- rbindlist(lapply(makefiles, function(mk) {
  txt <- readLines(mk, warn = FALSE)
  down <- parse_task(mk)
  refs <- parse_output_refs(
    txt,
    "\\.\\./\\.\\./[A-Za-z0-9_\\-]+/output/[^\\s\"']+"
  )
  if (length(refs) == 0) return(NULL)

  data.table(
    downstream_task = down,
    upstream_task = sub("^\\.\\./\\.\\./([A-Za-z0-9_\\-]+)/output/.*$", "\\1", refs, perl = TRUE),
    upstream_output_ref = refs,
    upstream_output_abs = normalize_refs(dirname(mk), refs),
    makefile = gsub(paste0("^", root_dir, "/"), "", mk)
  )
}), fill = TRUE)

if (nrow(edge_rows) == 0) {
  edge_rows <- data.table(
    downstream_task = character(),
    upstream_task = character(),
    upstream_output_ref = character(),
    upstream_output_abs = character(),
    makefile = character()
  )
}
setorder(edge_rows, downstream_task, upstream_task, upstream_output_ref)
fwrite(edge_rows, "../output/task_dependency_edges.csv")

tex_files <- c(
  list.files(file.path(root_dir, "paper"), pattern = "\\.tex$", recursive = TRUE, full.names = TRUE),
  list.files(file.path(root_dir, "slides"), pattern = "\\.tex$", recursive = TRUE, full.names = TRUE)
)

tex_refs <- rbindlist(lapply(tex_files, function(tf) {
  txt <- readLines(tf, warn = FALSE)
  hits <- parse_output_refs(
    txt,
    "\\.\\./tasks/[A-Za-z0-9_\\-]+/output/[^}\\s]+"
  )
  if (length(hits) == 0) return(NULL)

  rel_tf <- gsub(paste0("^", root_dir, "/"), "", tf)
  tex_group <- if (startsWith(rel_tf, "paper/")) "paper" else "slides"
  tex_base_dir <- file.path(root_dir, tex_group)

  data.table(
    tex_group = tex_group,
    tex_file = rel_tf,
    output_ref = hits,
    task = sub("^\\.\\./tasks/([A-Za-z0-9_\\-]+)/output/.*$", "\\1", hits, perl = TRUE),
    abs_path = normalizePath(file.path(tex_base_dir, hits), winslash = "/", mustWork = FALSE)
  )
}), fill = TRUE)

if (nrow(tex_refs) == 0) {
  tex_usage <- data.table(tex_group = character(), tex_file = character(), task = character(), n_refs = integer())
} else {
  tex_usage <- tex_refs[, .(n_refs = .N), by = .(tex_group, tex_file, task)]
  setorder(tex_usage, tex_group, tex_file, task)
}
fwrite(tex_usage, "../output/active_tex_task_usage.csv")

all_output_refs <- unique(unlist(lapply(makefiles, extract_all_outputs)))

output_files <- rbindlist(lapply(task_dirs, function(task_dir) {
  out_dir <- file.path(task_dir, "output")
  if (!dir.exists(out_dir)) return(NULL)
  files <- list.files(out_dir, full.names = TRUE, recursive = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  if (length(files) == 0) return(NULL)

  data.table(
    task = basename(task_dir),
    file = gsub(paste0("^", root_dir, "/"), "", files),
    abs_path = normalizePath(files, winslash = "/", mustWork = FALSE),
    size_bytes = as.numeric(file.info(files)$size)
  )
}), fill = TRUE)

if (nrow(output_files) == 0) {
  keep_manifest <- data.table(
    task = character(),
    file = character(),
    size_bytes = numeric(),
    keep_class = character(),
    referenced_by_tex = logical(),
    referenced_downstream = logical(),
    targeted_by_all = logical(),
    prune_stale = logical()
  )
} else {
  tex_abs <- unique(tex_refs$abs_path)
  downstream_abs <- unique(edge_rows$upstream_output_abs)

  output_files[, referenced_by_tex := abs_path %in% tex_abs]
  output_files[, referenced_downstream := abs_path %in% downstream_abs]
  output_files[, targeted_by_all := abs_path %in% all_output_refs]
  output_files[, full_keep_task := Reduce(`|`, lapply(full_keep_task_patterns, grepl, x = task))]

  output_files[, keep_class := fifelse(
    referenced_by_tex, "keep_tex",
    fifelse(referenced_downstream, "keep_downstream",
      fifelse(targeted_by_all | full_keep_task, "keep_canonical", "prune_stale")
    )
  )]
  output_files[, prune_stale := keep_class == "prune_stale"]

  keep_manifest <- output_files[, .(
    task,
    file,
    size_bytes,
    keep_class,
    referenced_by_tex,
    referenced_downstream,
    targeted_by_all,
    prune_stale
  )]
  setorder(keep_manifest, task, keep_class, file)
}

fwrite(keep_manifest, "../output/output_keep_manifest.csv")
fwrite(keep_manifest[prune_stale == TRUE], "../output/stale_output_manifest.csv")

task_summary <- keep_manifest[, .(
  output_files = .N,
  keep_tex = sum(keep_class == "keep_tex"),
  keep_downstream = sum(keep_class == "keep_downstream"),
  keep_canonical = sum(keep_class == "keep_canonical"),
  prune_stale = sum(keep_class == "prune_stale"),
  prune_size_gb = sum(size_bytes[keep_class == "prune_stale"], na.rm = TRUE) / 1024^3
), by = task]
setorder(task_summary, -prune_size_gb, -prune_stale, task)
fwrite(task_summary, "../output/task_cleanup_summary.csv")

lines <- c(
  "# Cleanup Manifest",
  "",
  sprintf("- Generated at: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("- Active tasks scanned: %d", length(task_dirs)),
  sprintf("- Makefiles scanned: %d", length(makefiles)),
  sprintf("- Paper/slides-linked task refs: %d", nrow(tex_refs)),
  sprintf("- Output files scanned: %d", nrow(keep_manifest)),
  sprintf("- Keep tex: %d", sum(keep_manifest$keep_class == "keep_tex")),
  sprintf("- Keep downstream: %d", sum(keep_manifest$keep_class == "keep_downstream")),
  sprintf("- Keep canonical: %d", sum(keep_manifest$keep_class == "keep_canonical")),
  sprintf("- Prune stale: %d", sum(keep_manifest$keep_class == "prune_stale")),
  "",
  "## Task summary",
  "",
  "| Task | Outputs | Tex | Downstream | Canonical | Prune | Prune Size (GB) |",
  "|---|---:|---:|---:|---:|---:|---:|"
)

if (nrow(task_summary) > 0) {
  for (i in seq_len(nrow(task_summary))) {
    rr <- task_summary[i]
    lines <- c(
      lines,
      sprintf(
        "| %s | %d | %d | %d | %d | %d | %.3f |",
        rr$task,
        rr$output_files,
        rr$keep_tex,
        rr$keep_downstream,
        rr$keep_canonical,
        rr$prune_stale,
        rr$prune_size_gb
      )
    )
  }
}

writeLines(lines, "../output/cleanup_manifest.md")

cat("Saved:\n")
cat(" - ../output/task_dependency_edges.csv\n")
cat(" - ../output/active_tex_task_usage.csv\n")
cat(" - ../output/output_keep_manifest.csv\n")
cat(" - ../output/stale_output_manifest.csv\n")
cat(" - ../output/task_cleanup_summary.csv\n")
cat(" - ../output/cleanup_manifest.md\n")
