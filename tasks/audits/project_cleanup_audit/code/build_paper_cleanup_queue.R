source("../../../setup_environment/code/packages.R")

library(data.table)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/project_cleanup_audit/code")
# edge_csv <- "../output/task_dependency_edges.csv"
# tex_csv <- "../output/active_tex_task_usage.csv"
# style_csv <- "../output/task_style_conformance_after.csv"
# out_csv <- "../output/paper_pipeline_cleanup_queue.csv"
# out_md <- "../output/paper_pipeline_cleanup_queue.md"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(edge_csv, tex_csv, style_csv, out_csv, out_md)
}

if (length(args) < 5) {
  stop(
    "Usage: Rscript build_paper_cleanup_queue.R <edge_csv> <tex_csv> <style_csv> <out_csv> <out_md>",
    call. = FALSE
  )
}

edge_csv <- args[1]
tex_csv <- args[2]
style_csv <- args[3]
out_csv <- args[4]
out_md <- args[5]

deprecated_task_map <- data.table(
  legacy_task = c(
    "data_for_alderman_strictness_scores",
    "create_alderman_strictness_scores"
  ),
  replacement_task = c(
    "data_for_alderman_uncertainty_index",
    "create_alderman_uncertainty_index"
  )
)

edges <- fread(edge_csv)
tex_usage <- fread(tex_csv)
style_dt <- fread(style_csv)

if (!("tex_group" %in% names(tex_usage)) && "tex_file" %in% names(tex_usage)) {
  tex_usage[, tex_group := fifelse(startsWith(tex_file, "paper/"), "paper", "slides")]
}

paper_roots <- sort(unique(tex_usage[tex_group == "paper", task]))
excluded_tasks <- deprecated_task_map$legacy_task

active_tasks <- sort(unique(style_dt$task))

edges <- unique(edges[
  upstream_task %chin% active_tasks &
    downstream_task %chin% active_tasks &
    !upstream_task %chin% excluded_tasks &
    !downstream_task %chin% excluded_tasks,
  .(upstream_task, downstream_task)
])

pipeline_tasks <- unique(paper_roots)
frontier <- unique(paper_roots)

while (length(frontier) > 0) {
  upstream_hits <- unique(edges[downstream_task %chin% frontier, upstream_task])
  upstream_hits <- setdiff(upstream_hits, pipeline_tasks)
  pipeline_tasks <- c(pipeline_tasks, upstream_hits)
  frontier <- upstream_hits
}

pipeline_tasks <- sort(unique(pipeline_tasks))

edges_sub <- edges[
  upstream_task %chin% pipeline_tasks &
    downstream_task %chin% pipeline_tasks
]

branch_bucket <- function(task) {
  if (task %chin% c(
    "building_permits_scraping",
    "clean_building_permits",
    "create_alderman_data",
    "ward_panel_create",
    "zoning_data_cleaning",
    "census_block_2020_cleaning",
    "border_segment_creation",
    "create_ward_controls",
    "create_block_group_controls",
    "create_block_treatment_panel",
    "geocode_ccao_data",
    "calculate_ward_boundary_distances",
    "calculate_sale_distances",
    "calculate_rent_distances",
    "assign_segment_ids",
    "assign_segment_ids_sales_rental",
    "merge_event_study_scores",
    "merge_in_scores"
  )) {
    return("common_raw")
  }
  if (task %chin% c(
    "data_for_alderman_uncertainty_index",
    "create_alderman_uncertainty_index",
    "strictness_score_map",
    "within_ward_strictness",
    "permit_stringency_table"
  )) {
    return("stringency")
  }
  if (task %chin% c(
    "new_construction_density_robustness",
    "new_construction_score_comparison",
    "new_construction_score_variants",
    "uncertainty_score_density_robustness",
    "border_pair_FE_regressions",
    "nonparametric_rd_density_linear_display"
  )) {
    return("density_spatial_rd")
  }
  if (task %chin% c(
    "create_event_study_permit_data",
    "run_event_study_permit",
    "run_event_study_permit_dcdh_robustness",
    "permit_summary_stats"
  )) {
    return("permit")
  }
  if (
    grepl("rental|rent|sales|sale|repeat_sales", task) &&
      !task %chin% c("building_permits_scraping")
  ) {
    return("sales_rents")
  }
  "paper_other"
}

task_bucket_dt <- data.table(
  task = pipeline_tasks,
  branch_bucket = vapply(pipeline_tasks, branch_bucket, character(1))
)

bucket_rank_map <- c(
  common_raw = 1L,
  stringency = 2L,
  density_spatial_rd = 3L,
  permit = 4L,
  sales_rents = 5L,
  paper_other = 6L
)

in_degree <- setNames(integer(length(pipeline_tasks)), pipeline_tasks)
if (nrow(edges_sub) > 0) {
  indeg_dt <- edges_sub[, .N, by = downstream_task]
  in_degree[indeg_dt$downstream_task] <- indeg_dt$N
}

available <- sort(names(in_degree)[in_degree == 0L])
topo_order <- character()

while (length(available) > 0) {
  next_task <- available[1]
  available <- available[-1]
  topo_order <- c(topo_order, next_task)

  down_hits <- edges_sub[upstream_task == next_task, downstream_task]
  if (length(down_hits) == 0) {
    next
  }

  for (task_i in down_hits) {
    in_degree[[task_i]] <- in_degree[[task_i]] - 1L
    if (in_degree[[task_i]] == 0L) {
      available <- sort(unique(c(available, task_i)))
    }
  }
}

if (length(topo_order) != length(pipeline_tasks)) {
  topo_order <- sort(unique(c(topo_order, pipeline_tasks)))
}

distance_dt <- data.table(task = pipeline_tasks, min_steps_to_paper = NA_integer_)
distance_dt[task %chin% paper_roots, min_steps_to_paper := 0L]

for (task_i in rev(topo_order)) {
  if (task_i %chin% paper_roots) {
    next
  }
  downstream_steps <- distance_dt[
    task %chin% edges_sub[upstream_task == task_i, downstream_task],
    min_steps_to_paper
  ]
  downstream_steps <- downstream_steps[!is.na(downstream_steps)]
  if (length(downstream_steps) > 0) {
    distance_dt[task == task_i, min_steps_to_paper := min(downstream_steps) + 1L]
  }
}

paper_root_map <- lapply(pipeline_tasks, function(task_i) {
  hits <- character()
  frontier_i <- task_i
  visited_i <- character()

  while (length(frontier_i) > 0) {
    hits <- unique(c(hits, intersect(frontier_i, paper_roots)))
    visited_i <- unique(c(visited_i, frontier_i))
    frontier_i <- unique(edges_sub[upstream_task %chin% frontier_i, downstream_task])
    frontier_i <- setdiff(frontier_i, visited_i)
  }

  paste(sort(hits), collapse = ";")
})

paper_ref_count <- tex_usage[tex_group == "paper", .(n_paper_refs = sum(n_refs, na.rm = TRUE)), by = task]

queue_dt <- merge(
  data.table(task = topo_order, topo_position = seq_along(topo_order)),
  task_bucket_dt,
  by = "task",
  all.x = TRUE
)
queue_dt <- merge(queue_dt, distance_dt, by = "task", all.x = TRUE)
queue_dt <- merge(
  queue_dt,
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
    single_use_task_helper_files
  )],
  by = "task",
  all.x = TRUE
)
queue_dt <- merge(queue_dt, paper_ref_count, by = "task", all.x = TRUE)
queue_dt[, paper_root_tasks := unlist(paper_root_map)]

for (col in c(
  "cli_scripts", "named_local_functions", "cli_scripts_gt2_local_helpers",
  "default_sidecar_output_count", "fixed_path_cli_arg_cmds",
  "scripts_with_file_exists", "file_exists_refs",
  "non_lib_source_calls", "single_use_task_helper_files", "n_paper_refs"
)) {
  queue_dt[is.na(get(col)), (col) := 0L]
}

queue_dt[, branch_bucket_rank := bucket_rank_map[branch_bucket]]
queue_dt[is.na(branch_bucket_rank), branch_bucket_rank := max(bucket_rank_map) + 1L]
setorder(queue_dt, branch_bucket_rank, -min_steps_to_paper, topo_position, task)
queue_dt[, cleanup_rank := seq_len(.N)]
queue_dt[, branch_bucket_rank := NULL]
setcolorder(queue_dt, c(
  "cleanup_rank", "task", "branch_bucket", "min_steps_to_paper", "paper_root_tasks",
  "n_paper_refs", "cli_scripts", "default_sidecar_output_count",
  "fixed_path_cli_arg_cmds", "scripts_with_file_exists", "file_exists_refs",
  "named_local_functions",
  "cli_scripts_gt2_local_helpers", "non_lib_source_calls",
  "single_use_task_helper_files", "topo_position"
))

fwrite(queue_dt, out_csv)

lines <- c(
  "# Paper Pipeline Cleanup Queue",
  "",
  sprintf("- Paper root tasks: %d", length(paper_roots)),
  sprintf("- Tasks in paper pipeline ancestry: %d", nrow(queue_dt)),
  sprintf("- Deprecated active tasks excluded: %s", paste(excluded_tasks, collapse = ", ")),
  "",
  "| Rank | Task | Branch | Steps To Paper | Paper Roots | Paper Refs | Sidecar `all` Outputs | Fixed Path CLI Cmds | File Exists Refs | Local Functions | CLI Scripts >2 Helpers |",
  "|---:|---|---|---:|---|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(queue_dt))) {
  rr <- queue_dt[i]
  lines <- c(
    lines,
    sprintf(
      "| %d | %s | %s | %d | %s | %d | %d | %d | %d | %d | %d |",
      rr$cleanup_rank,
      rr$task,
      rr$branch_bucket,
      rr$min_steps_to_paper,
      rr$paper_root_tasks,
      rr$n_paper_refs,
      rr$default_sidecar_output_count,
      rr$fixed_path_cli_arg_cmds,
      rr$file_exists_refs,
      rr$named_local_functions,
      rr$cli_scripts_gt2_local_helpers
    )
  )
}

writeLines(lines, out_md)
cat(sprintf("Saved: %s\n", out_csv))
cat(sprintf("Saved: %s\n", out_md))
