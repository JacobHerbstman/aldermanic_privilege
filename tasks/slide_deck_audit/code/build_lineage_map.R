source("../../setup_environment/code/packages.R")

## Build task lineage and object verification index
## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/slide_deck_audit/code")
# Rscript build_lineage_map.R ../output/slide_asset_inventory.csv ../output/slide_claim_manifest.csv ../output/slide_task_lineage.csv ../output/object_verification_index.csv

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 4) {
  asset_inventory_input <- args[1]
  claim_manifest_input <- args[2]
  lineage_output <- args[3]
  verification_output <- args[4]
} else {
  stop(
    "FATAL: Script requires 4 args: <asset_inventory_input> <claim_manifest_input> <lineage_output> <verification_output>",
    call. = FALSE
  )
}

root_dir <- normalizePath(file.path("..", "..", ".."), winslash = "/", mustWork = TRUE)
tasks_dir <- file.path(root_dir, "tasks")

repo_rel <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  sub(paste0("^", root_dir, "/"), "", path)
}

parse_task_from_output <- function(path) {
  out <- str_match(path, "^tasks/([^/]+)/output/")[, 2]
  ifelse(is.na(out), "", out)
}

parse_output_rel <- function(path) {
  sub("^tasks/[^/]+/output/", "../output/", path)
}

read_logical_make_lines <- function(path) {
  lines <- readLines(path, warn = FALSE)
  out <- character()
  buffer <- ""

  for (line in lines) {
    if (str_detect(line, "\\\\$") && !str_detect(line, "^\\t")) {
      buffer <- paste0(buffer, str_remove(line, "\\\\$"), " ")
    } else {
      out <- c(out, paste0(buffer, line))
      buffer <- ""
    }
  }

  if (nzchar(buffer)) {
    out <- c(out, buffer)
  }

  out
}

extract_make_metadata <- function(task_name) {
  makefile <- file.path(tasks_dir, task_name, "code", "Makefile")
  if (!file.exists(makefile)) {
    return(NULL)
  }

  logical_lines <- read_logical_make_lines(makefile)

  input_rules <- list()
  output_rules <- list()
  task_scripts <- unique(unlist(str_extract_all(logical_lines, "[A-Za-z0-9_./-]+\\.(R|r|py|sh)\\b")))
  task_scripts <- task_scripts[task_scripts != ""]

  for (line in logical_lines) {
    if (!str_detect(line, ":")) {
      next
    }

    lhs <- str_trim(str_split_fixed(line, ":", 2)[, 1])
    rhs <- str_trim(str_split_fixed(line, ":", 2)[, 2])
    rhs_main <- str_trim(str_split_fixed(rhs, "\\|", 2)[, 1])
    prereqs <- str_split(rhs_main, "\\s+")[[1]]
    prereqs <- prereqs[prereqs != ""]
    targets <- str_split(lhs, "\\s+")[[1]]
    targets <- targets[targets != ""]

    if (length(targets) == 0) {
      next
    }

    if (all(str_starts(targets, "../input/"))) {
      for (target in targets) {
        if (length(prereqs) > 0) {
          input_rules[[target]] <- prereqs[1]
        }
      }
    }

    if (any(str_starts(targets, "../output/"))) {
      for (target in targets[str_starts(targets, "../output/")]) {
        output_rules[[target]] <- list(
          prereqs = prereqs,
          scripts = prereqs[str_detect(prereqs, "\\.(R|r|py|sh)$")]
        )
      }
    }
  }

  list(
    task = task_name,
    makefile = makefile,
    input_rules = input_rules,
    output_rules = output_rules,
    task_scripts = task_scripts
  )
}

all_tasks <- list.dirs(tasks_dir, recursive = FALSE, full.names = FALSE)
make_meta <- setNames(lapply(all_tasks, extract_make_metadata), all_tasks)
make_meta <- make_meta[!vapply(make_meta, is.null, logical(1))]

resolve_path_from_prereq <- function(task_name, prereq) {
  code_dir <- file.path(tasks_dir, task_name, "code")
  repo_rel(file.path(code_dir, prereq))
}

choose_task_script <- function(scripts) {
  if (length(scripts) == 0) {
    return("")
  }
  scripts <- scripts[!str_detect(scripts, "^\\.\\./input/|^\\.\\./output/")]
  non_helpers <- scripts[!str_detect(basename(scripts), "helper|packages")]
  if (length(non_helpers) > 0) {
    return(non_helpers[1])
  }
  scripts[1]
}

resolve_input_paths <- function(task_name, input_rules) {
  if (length(input_rules) == 0) {
    return(character())
  }
  map_chr(unname(input_rules), ~ resolve_path_from_prereq(task_name, .x))
}

resolve_fallback_lineage <- function(task_name, target_rel, visited = character()) {
  meta <- make_meta[[task_name]]
  if (is.null(meta)) {
    return(list(
      direct_producer = "",
      direct_inputs = character(),
      upstream_tasks = character(),
      raw_inputs = character(),
      inspection_stages = file.path("tasks", task_name, "output", sub("^../output/", "", target_rel))
    ))
  }

  direct_producer <- choose_task_script(meta$task_scripts)
  if (direct_producer != "") {
    direct_producer <- resolve_path_from_prereq(task_name, direct_producer)
  }

  direct_inputs <- resolve_input_paths(task_name, meta$input_rules)
  upstream_tasks <- character()
  raw_inputs <- character()
  inspection_stages <- file.path("tasks", task_name, "output", sub("^../output/", "", target_rel))

  for (input_path in direct_inputs) {
    if (str_detect(input_path, "^tasks/.+/output/")) {
      upstream_task <- parse_task_from_output(input_path)
      upstream_tasks <- c(upstream_tasks, upstream_task)
      recurse <- resolve_output_lineage(
        upstream_task,
        parse_output_rel(input_path),
        visited = c(visited, paste(task_name, target_rel, sep = "::"))
      )
      upstream_tasks <- c(upstream_tasks, recurse$upstream_tasks)
      raw_inputs <- c(raw_inputs, recurse$raw_inputs)
      inspection_stages <- c(inspection_stages, recurse$inspection_stages)
    } else {
      raw_inputs <- c(raw_inputs, input_path)
    }
  }

  list(
    direct_producer = direct_producer,
    direct_inputs = unique(direct_inputs[direct_inputs != ""]),
    upstream_tasks = unique(upstream_tasks[upstream_tasks != ""]),
    raw_inputs = unique(raw_inputs[raw_inputs != ""]),
    inspection_stages = unique(inspection_stages[inspection_stages != ""])
  )
}

resolve_output_lineage <- function(task_name, target_rel, visited = character()) {
  visit_key <- paste(task_name, target_rel, sep = "::")
  if (visit_key %in% visited) {
    return(list(
      direct_producer = "",
      direct_inputs = character(),
      upstream_tasks = character(),
      raw_inputs = character(),
      inspection_stages = character()
    ))
  }

  meta <- make_meta[[task_name]]
  if (is.null(meta)) {
    return(list(
      direct_producer = "",
      direct_inputs = character(),
      upstream_tasks = character(),
      raw_inputs = character(),
      inspection_stages = character()
    ))
  }

  rule <- meta$output_rules[[target_rel]]
  if (is.null(rule)) {
    return(resolve_fallback_lineage(task_name, target_rel, visited = visited))
  }

  direct_producer <- if (length(rule$scripts) > 0) {
    resolve_path_from_prereq(task_name, rule$scripts[1])
  } else {
    ""
  }

  direct_inputs <- character()
  upstream_tasks <- character()
  raw_inputs <- character()
  inspection_stages <- c(file.path("tasks", task_name, "output", sub("^../output/", "", target_rel)))

  for (prereq in rule$prereqs) {
    if (str_starts(prereq, "../input/")) {
      mapped <- meta$input_rules[[prereq]]
      if (is.null(mapped)) {
        direct_inputs <- c(direct_inputs, file.path("tasks", task_name, "input", sub("^../input/", "", prereq)))
        next
      }

      mapped_repo <- resolve_path_from_prereq(task_name, mapped)
      direct_inputs <- c(direct_inputs, mapped_repo)

      if (str_detect(mapped_repo, "^tasks/.+/output/")) {
        upstream_task <- parse_task_from_output(mapped_repo)
        upstream_tasks <- c(upstream_tasks, upstream_task)
        recurse <- resolve_output_lineage(
          upstream_task,
          parse_output_rel(mapped_repo),
          visited = c(visited, visit_key)
        )
        upstream_tasks <- c(upstream_tasks, recurse$upstream_tasks)
        raw_inputs <- c(raw_inputs, recurse$raw_inputs)
        inspection_stages <- c(inspection_stages, recurse$inspection_stages)
        if (direct_producer == "" && recurse$direct_producer != "") {
          direct_producer <- recurse$direct_producer
        }
      } else {
        raw_inputs <- c(raw_inputs, mapped_repo)
      }
      next
    }

    if (str_starts(prereq, "../output/")) {
      recurse <- resolve_output_lineage(task_name, prereq, visited = c(visited, visit_key))
      direct_inputs <- c(direct_inputs, file.path("tasks", task_name, "output", sub("^../output/", "", prereq)))
      upstream_tasks <- c(upstream_tasks, recurse$upstream_tasks)
      raw_inputs <- c(raw_inputs, recurse$raw_inputs)
      inspection_stages <- c(inspection_stages, recurse$inspection_stages)
      if (direct_producer == "" && recurse$direct_producer != "") {
        direct_producer <- recurse$direct_producer
      }
      next
    }

    if (str_starts(prereq, "../../") || str_starts(prereq, "../../../")) {
      prereq_repo <- resolve_path_from_prereq(task_name, prereq)
      direct_inputs <- c(direct_inputs, prereq_repo)

      if (str_detect(prereq_repo, "^tasks/.+/output/")) {
        upstream_task <- parse_task_from_output(prereq_repo)
        upstream_tasks <- c(upstream_tasks, upstream_task)
        recurse <- resolve_output_lineage(
          upstream_task,
          parse_output_rel(prereq_repo),
          visited = c(visited, visit_key)
        )
        upstream_tasks <- c(upstream_tasks, recurse$upstream_tasks)
        raw_inputs <- c(raw_inputs, recurse$raw_inputs)
        inspection_stages <- c(inspection_stages, recurse$inspection_stages)
      } else {
        raw_inputs <- c(raw_inputs, prereq_repo)
      }
      next
    }

    if (str_detect(prereq, "\\.(R|r|py|sh)$")) {
      if (direct_producer == "") {
        direct_producer <- resolve_path_from_prereq(task_name, prereq)
      }
      next
    }

    if (str_detect(prereq, "data_raw|slides/|paper/")) {
      raw_inputs <- c(raw_inputs, prereq)
    }
  }

  list(
    direct_producer = direct_producer,
    direct_inputs = unique(direct_inputs[direct_inputs != ""]),
    upstream_tasks = unique(upstream_tasks[upstream_tasks != ""]),
    raw_inputs = unique(raw_inputs[raw_inputs != ""]),
    inspection_stages = unique(inspection_stages[inspection_stages != ""])
  )
}

extract_spec_fields <- function(path_text, expected_spec) {
  text <- paste(c(path_text, expected_spec), collapse = " ")

  bandwidth <- str_match(text, "bw(250|500|1000|all)")[, 2]
  cluster <- str_match(text, "clust_(segment|ward_pair)")[, 2]

  fe_structure <- case_when(
    str_detect(text, "zonegroup_segment_year_additive") ~ "zone group + segment + year",
    str_detect(text, "zone_segment_year_additive") ~ "zone + segment + year",
    str_detect(text, "segment_year") ~ "segment + year",
    str_detect(text, "zonegroup_pair_year_additive") ~ "zone group + ward pair + year",
    str_detect(text, "pair_x_year") ~ "ward pair × year",
    TRUE ~ ""
  )

  grouping_level <- case_when(
    str_detect(text, "alderman_summary|alderman_mean|uncertainty_index|stage1_regression") ~ "alderman",
    str_detect(text, "ward_summary|ward_map") ~ "ward",
    str_detect(text, "fe_table|rd_fe_plot") ~ "parcel / border segment",
    str_detect(text, "rent|sales|listing_units|permutation") ~ "listing / transaction panel",
    TRUE ~ ""
  )

  time_window <- case_when(
    str_detect(text, "pre_2023") ~ "pre_2023",
    str_detect(text, "2014_2022") ~ "2014-2022",
    str_detect(text, "2025-01") ~ "2025-01",
    str_detect(text, "2006-2025") ~ "2006-2025",
    TRUE ~ ""
  )

  sample_restrictions <- case_when(
    str_detect(text, "multifamily") ~ "multifamily",
    str_detect(text, "all") ~ "all",
    TRUE ~ ""
  )

  sign_conventions <- case_when(
    str_detect(text, "shift-500") ~ "placebo shift -500 ft",
    str_detect(text, "shift500") ~ "placebo shift +500 ft",
    str_detect(text, "gap_below_median") ~ "below-median within-pair gap",
    str_detect(text, "gap_above_median") ~ "above-median within-pair gap",
    TRUE ~ ""
  )

  tibble(
    bandwidth = coalesce(bandwidth, ""),
    clustering = coalesce(cluster, ""),
    fe_structure = fe_structure,
    grouping_level = grouping_level,
    time_window = time_window,
    sample_restrictions = sample_restrictions,
    side_sign_conventions = sign_conventions
  )
}

asset_inventory <- read_csv(asset_inventory_input, show_col_types = FALSE)
claim_manifest <- read_csv(claim_manifest_input, show_col_types = FALSE)

lineage_objects <- unique(c(
  asset_inventory$asset_path[asset_inventory$asset_class == "task_output"],
  claim_manifest$source_path[str_detect(claim_manifest$source_path, "^tasks/.+/output/")],
  claim_manifest$inspection_object[str_detect(claim_manifest$inspection_object, "^tasks/.+/output/")]
))
lineage_objects <- lineage_objects[lineage_objects != "" & !is.na(lineage_objects)]

lineage_rows <- map_dfr(lineage_objects, function(object_path) {
  task_name <- parse_task_from_output(object_path)
  if (task_name == "") {
    return(tibble())
  }

  resolved <- resolve_output_lineage(task_name, parse_output_rel(object_path))

  tibble(
    object_path = object_path,
    object_task = task_name,
    direct_producer = resolved$direct_producer,
    direct_input_objects = paste(resolved$direct_inputs, collapse = " || "),
    upstream_task_chain = paste(unique(c(task_name, resolved$upstream_tasks)), collapse = " -> "),
    first_raw_boundary = ifelse(length(resolved$raw_inputs) > 0, resolved$raw_inputs[1], ""),
    raw_input_paths = paste(resolved$raw_inputs, collapse = " || "),
    inspection_stage_paths = paste(resolved$inspection_stages, collapse = " || "),
    file_exists = file.exists(file.path(root_dir, object_path))
  )
})

verification_index <- claim_manifest %>%
  left_join(
    lineage_rows %>%
      rename(
        source_path = object_path,
        source_task = object_task,
        source_direct_producer = direct_producer,
        source_direct_inputs = direct_input_objects,
        source_upstream_chain = upstream_task_chain,
        source_first_raw_boundary = first_raw_boundary,
        source_raw_inputs = raw_input_paths,
        source_inspection_stages = inspection_stage_paths
      ),
    by = "source_path"
  ) %>%
  left_join(
    lineage_rows %>%
      rename(
        inspection_object = object_path,
        inspection_task = object_task,
        inspection_direct_producer = direct_producer,
        inspection_direct_inputs = direct_input_objects,
        inspection_upstream_chain = upstream_task_chain,
        inspection_first_raw_boundary = first_raw_boundary,
        inspection_raw_inputs = raw_input_paths,
        inspection_stages = inspection_stage_paths
      ),
    by = "inspection_object"
  ) %>%
  mutate(
    producer_path = coalesce(inspection_direct_producer, source_direct_producer, ""),
    output_file = coalesce(inspection_object, source_path, ""),
    upstream_task_chain = coalesce(inspection_upstream_chain, source_upstream_chain, ""),
    first_raw_boundary = coalesce(inspection_first_raw_boundary, source_first_raw_boundary, ""),
    reviewer_inspection_path = coalesce(inspection_stages, source_inspection_stages, inspection_object, source_path),
    key_variable_definitions = expected_spec
  )

spec_fields <- pmap_dfr(
  list(verification_index$output_file, verification_index$expected_spec),
  extract_spec_fields
)

verification_index <- bind_cols(
  verification_index %>% select(
    section_file, frame_label, claim_id, claim_type, display_object,
    source_kind, source_path, inspection_object, producer_path, output_file,
    key_variable_definitions, upstream_task_chain, first_raw_boundary,
    reviewer_inspection_path, priority
  ),
  spec_fields
) %>%
  mutate(
    clustering = coalesce(clustering, ""),
    bandwidth = coalesce(bandwidth, ""),
    fe_structure = coalesce(fe_structure, ""),
    grouping_level = coalesce(grouping_level, ""),
    time_window = coalesce(time_window, ""),
    side_sign_conventions = coalesce(side_sign_conventions, "")
  )

write_csv(lineage_rows, lineage_output)
write_csv(verification_index, verification_output)

message("Saved: ", lineage_output)
message("Saved: ", verification_output)
