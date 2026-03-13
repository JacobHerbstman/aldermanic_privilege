source("../../setup_environment/code/packages.R")

library(data.table)

root_dir <- normalizePath(file.path("..", "..", ".."))
tasks_dir <- file.path(root_dir, "tasks")

makefiles <- list.files(tasks_dir, pattern = "Makefile$", recursive = TRUE, full.names = TRUE)
makefiles <- makefiles[grepl("/code/Makefile$", makefiles)]

parse_task <- function(path) {
  parts <- strsplit(path, .Platform$file.sep, fixed = TRUE)[[1]]
  idx <- match("tasks", parts)
  if (is.na(idx) || idx + 1 > length(parts)) return(NA_character_)
  parts[idx + 1]
}

# -----------------------------------------------------------------------------
# 1) Dependency edges from symlink-like upstream references
# -----------------------------------------------------------------------------
edge_rows <- rbindlist(lapply(makefiles, function(mk) {
  txt <- readLines(mk, warn = FALSE)
  down <- parse_task(mk)
  if (!is.finite(nchar(down))) return(NULL)

  refs <- unique(unlist(regmatches(txt, gregexpr("\\.\\./\\.\\./([A-Za-z0-9_\\-]+)/output/", txt, perl = TRUE))))
  if (length(refs) == 0) return(NULL)

  ups <- unique(sub("^\\.\\./\\.\\./([A-Za-z0-9_\\-]+)/output/.*$", "\\1", refs, perl = TRUE))
  data.table(
    downstream_task = down,
    upstream_task = ups,
    makefile = gsub(paste0("^", root_dir, "/"), "", mk)
  )
}), fill = TRUE)

if (nrow(edge_rows) == 0) {
  edge_rows <- data.table(downstream_task = character(), upstream_task = character(), makefile = character())
}
setorder(edge_rows, downstream_task, upstream_task)
fwrite(edge_rows, "../output/task_dependency_edges.csv")

# -----------------------------------------------------------------------------
# 2) Active usage map from TeX refs
# -----------------------------------------------------------------------------
tex_files <- c(
  list.files(file.path(root_dir, "paper"), pattern = "\\.tex$", recursive = TRUE, full.names = TRUE),
  list.files(file.path(root_dir, "slides"), pattern = "\\.tex$", recursive = TRUE, full.names = TRUE)
)

tex_refs <- rbindlist(lapply(tex_files, function(tf) {
  txt <- readLines(tf, warn = FALSE)
  hits <- unique(unlist(regmatches(txt, gregexpr("\\.\\./tasks/[A-Za-z0-9_\\-]+/output/[^}\\s]+", txt, perl = TRUE))))
  if (length(hits) == 0) return(NULL)

  src <- fifelse(grepl("/slides/", tf), "slides", "paper")

  data.table(
    source_group = src,
    tex_file = gsub(paste0("^", root_dir, "/"), "", tf),
    output_ref = hits,
    task = sub("^\\.\\./tasks/([A-Za-z0-9_\\-]+)/output/.*$", "\\1", hits, perl = TRUE)
  )
}), fill = TRUE)

if (nrow(tex_refs) == 0) {
  tex_usage <- data.table(source_group = character(), task = character(), n_refs = integer(), n_files = integer())
} else {
  tex_usage <- tex_refs[, .(
    n_refs = .N,
    n_files = uniqueN(tex_file)
  ), by = .(source_group, task)]
  setorder(tex_usage, source_group, -n_refs, task)
}

fwrite(tex_usage, "../output/active_tex_task_usage.csv")

# -----------------------------------------------------------------------------
# 3) Stale output manifest
# -----------------------------------------------------------------------------
# References from TeX
tex_ref_abs <- character()
if (nrow(tex_refs) > 0) {
  tex_ref_abs <- normalizePath(file.path(dirname(file.path(root_dir, tex_refs$tex_file)), tex_refs$output_ref), winslash = "/", mustWork = FALSE)
}

# References from make targets in each task's expanded make database (`make -pn all`)
extract_make_output_refs <- function(mk) {
  code_dir <- dirname(mk)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(code_dir)

  db <- tryCatch(
    suppressWarnings(system2("make", c("-pn", "all"), stdout = TRUE, stderr = TRUE)),
    error = function(e) character()
  )
  if (length(db) == 0) return(character())

  target_lines <- grep("^\\.\\./output/[^:]+\\s*:", db, value = TRUE)
  if (length(target_lines) == 0) return(character())

  targets <- unique(unlist(lapply(target_lines, function(ln) {
    lhs <- trimws(strsplit(ln, ":", fixed = TRUE)[[1]][1])
    trimws(strsplit(lhs, "\\s+")[[1]])
  })))
  targets <- targets[startsWith(targets, "../output/")]
  targets <- targets[!grepl("[%$(){}]", targets)]
  if (length(targets) == 0) return(character())

  normalizePath(file.path(code_dir, targets), winslash = "/", mustWork = FALSE)
}

mk_ref_abs <- unique(unlist(lapply(makefiles, extract_make_output_refs)))

all_refs <- unique(c(tex_ref_abs, mk_ref_abs))

output_files <- rbindlist(lapply(list.dirs(tasks_dir, recursive = FALSE, full.names = TRUE), function(task_dir) {
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
  stale_manifest <- data.table(task = character(), file = character(), size_bytes = numeric(), referenced_by_tex = logical(), referenced_by_makefile = logical(), stale_candidate = logical())
} else {
  output_files[, referenced_by_tex := abs_path %in% tex_ref_abs]
  output_files[, referenced_by_makefile := abs_path %in% mk_ref_abs]
  output_files[, stale_candidate := !(referenced_by_tex | referenced_by_makefile)]
  stale_manifest <- output_files[, .(task, file, size_bytes, referenced_by_tex, referenced_by_makefile, stale_candidate)]
  setorder(stale_manifest, -stale_candidate, task, file)
}

fwrite(stale_manifest, "../output/stale_output_manifest.csv")

# -----------------------------------------------------------------------------
# 4) Markdown summary
# -----------------------------------------------------------------------------
stale_summary <- stale_manifest[, .(
  n_files = .N,
  n_stale = sum(stale_candidate),
  stale_gb = sum(size_bytes[stale_candidate], na.rm = TRUE) / 1024^3
), by = task][order(-stale_gb, -n_stale, task)]

lines <- c(
  "# Cleanup Manifest",
  "",
  sprintf("- Generated at: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("- Tasks scanned: %d", length(list.dirs(tasks_dir, recursive = FALSE, full.names = TRUE))),
  sprintf("- Makefiles scanned: %d", length(makefiles)),
  sprintf("- Dependency edges: %d", nrow(edge_rows)),
  sprintf("- TeX task refs: %d", ifelse(nrow(tex_usage) > 0, sum(tex_usage$n_refs), 0L)),
  sprintf("- Output files scanned: %d", nrow(stale_manifest)),
  sprintf("- Stale output candidates: %d", sum(stale_manifest$stale_candidate, na.rm = TRUE)),
  "",
  "## Top stale-output tasks by size",
  "",
  "| Task | Files | Stale Files | Stale Size (GB) |",
  "|---|---:|---:|---:|"
)

if (nrow(stale_summary) > 0) {
  top_rows <- head(stale_summary, 25)
  for (i in seq_len(nrow(top_rows))) {
    rr <- top_rows[i]
    lines <- c(lines, sprintf("| %s | %d | %d | %.3f |", rr$task, rr$n_files, rr$n_stale, rr$stale_gb))
  }
}

writeLines(lines, "../output/cleanup_manifest.md")

cat("Saved:\n")
cat(" - ../output/task_dependency_edges.csv\n")
cat(" - ../output/active_tex_task_usage.csv\n")
cat(" - ../output/stale_output_manifest.csv\n")
cat(" - ../output/cleanup_manifest.md\n")
