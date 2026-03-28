source("../../setup_environment/code/packages.R")

## Run slide audit

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/slide_deck_audit/code")
# asset_inventory_input <- "../output/slide_asset_inventory.csv"
# claim_manifest_input <- "../output/slide_claim_manifest.csv"
# lineage_input <- "../output/slide_task_lineage.csv"
# verification_index_input <- "../output/object_verification_index.csv"
# claim_status_output <- "../output/slide_claim_status.csv"
# geometry_checks_output <- "../output/geometry_geocoding_checks.csv"
# rerun_log_output <- "../output/rerun_reproducibility_log.csv"
# findings_output <- "../output/slides_audit_findings.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(asset_inventory_input, claim_manifest_input, lineage_input, verification_index_input, claim_status_output, geometry_checks_output, rerun_log_output, findings_output)
}

if (length(args) >= 8) {
  asset_inventory_input <- args[1]
  claim_manifest_input <- args[2]
  lineage_input <- args[3]
  verification_index_input <- args[4]
  claim_status_output <- args[5]
  geometry_checks_output <- args[6]
  rerun_log_output <- args[7]
  findings_output <- args[8]
} else {
  stop(
    "FATAL: Script requires 8 args: <asset_inventory_input> <claim_manifest_input> <lineage_input> <verification_index_input> <claim_status_output> <geometry_checks_output> <rerun_log_output> <findings_output>",
    call. = FALSE
  )
}

root_dir <- normalizePath(file.path("..", "..", ".."), winslash = "/", mustWork = TRUE)

repo_abs <- function(path) {
  path <- as.character(path)
  out <- rep(NA_character_, length(path))
  keep <- !is.na(path) & path != ""
  out[keep] <- normalizePath(file.path(root_dir, path[keep]), winslash = "/", mustWork = FALSE)
  out
}

parse_task_from_output <- function(path) {
  out <- str_match(path, "^tasks/([^/]+)/output/")[, 2]
  ifelse(is.na(out), "", out)
}

parse_output_rel <- function(path) {
  sub("^tasks/[^/]+/output/", "../output/", path)
}

normalize_claim_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9%]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

load_table_object <- function(path) {
  abs_path <- repo_abs(path)
  if (!file.exists(abs_path)) {
    return(NULL)
  }

  if (str_detect(path, "\\.csv$")) {
    return(read_csv(abs_path, show_col_types = FALSE))
  }

  if (str_detect(path, "\\.parquet$")) {
    return(arrow::read_parquet(abs_path))
  }

  NULL
}

read_text_object <- function(path) {
  abs_path <- repo_abs(path)
  if (!file.exists(abs_path)) {
    return(NA_character_)
  }
  paste(readLines(abs_path, warn = FALSE), collapse = "\n")
}

run_make_cmd <- function(code_dir, targets, dry_run = FALSE) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(code_dir)

  cmd_args <- c(if (dry_run) "-n", targets)
  out <- tryCatch(
    system2("make", cmd_args, stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )

  list(
    status = as.integer(attr(out, "status") %||% 0L),
    log = paste(out, collapse = "\n")
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

asset_inventory <- read_csv(asset_inventory_input, show_col_types = FALSE)
claim_manifest <- read_csv(claim_manifest_input, show_col_types = FALSE) %>%
  mutate(across(everything(), ~ replace_na(as.character(.x), "")))
lineage <- read_csv(lineage_input, show_col_types = FALSE)
verification_index <- read_csv(verification_index_input, show_col_types = FALSE)

task_output_paths <- unique(c(
  asset_inventory$asset_path[asset_inventory$asset_class == "task_output"],
  claim_manifest$source_path[str_detect(claim_manifest$source_path, "^tasks/.+/output/")],
  claim_manifest$inspection_object[str_detect(claim_manifest$inspection_object, "^tasks/.+/output/")]
))
task_output_paths <- task_output_paths[task_output_paths != ""]

rerun_targets <- tibble(
  output_path = task_output_paths,
  task = parse_task_from_output(task_output_paths),
  target_rel = parse_output_rel(task_output_paths)
) %>%
  filter(task != "") %>%
  distinct()

rerun_log <- map_dfr(split(rerun_targets, rerun_targets$task), function(task_df) {
  code_dir <- file.path(root_dir, "tasks", task_df$task[1], "code")
  targets <- task_df$target_rel
  file_exists_before <- file.exists(repo_abs(task_df$output_path))

  md5_before <- map_chr(task_df$output_path, function(path) {
    abs_path <- repo_abs(path)
    if (!file.exists(abs_path)) return("")
    unname(tools::md5sum(abs_path))
  })

  dry <- run_make_cmd(code_dir, targets, dry_run = TRUE)
  build <- run_make_cmd(code_dir, targets, dry_run = FALSE)

  tibble(
    task = task_df$task,
    output_path = task_df$output_path,
    target_rel = targets,
    dry_run_status = dry$status,
    build_status = build$status,
    file_exists_before = file_exists_before,
    md5_before = md5_before,
    file_exists_after = file.exists(repo_abs(task_df$output_path)),
    md5_after = map_chr(task_df$output_path, function(path) {
      abs_path <- repo_abs(path)
      if (!file.exists(abs_path)) return("")
      unname(tools::md5sum(abs_path))
    }),
    changed = md5_before != md5_after & md5_before != "" & md5_after != "",
    log_excerpt = str_sub(paste(dry$log, build$log, sep = "\n"), 1, 1000)
  )
})

status_for_task_path <- function(path) {
  row <- rerun_log %>% filter(output_path == path) %>% slice(1)
  if (nrow(row) == 0) {
    return("verified")
  }
  if (row$build_status != 0 || !row$file_exists_after) {
    return("build_failure")
  }
  "verified"
}

evaluate_claim <- function(row) {
  source_path <- row$source_path
  inspection_object <- ifelse(row$inspection_object != "", row$inspection_object, source_path)
  method <- row$verification_method
  slide_text <- normalize_claim_text(row$slide_text_excerpt)
  pattern_terms <- str_split(row$comparison_pattern, "\\|\\|")[[1]]
  pattern_terms <- normalize_claim_text(pattern_terms)
  pattern_terms <- pattern_terms[pattern_terms != ""]

  out <- list(
    verification_status = "verified",
    observed_value = "",
    evidence_path = ifelse(inspection_object != "", inspection_object, source_path),
    why_matters = "",
    reviewer_inspection_path = inspection_object
  )

  source_status <- if (str_detect(source_path, "^tasks/.+/output/")) status_for_task_path(source_path) else "verified"
  inspect_status <- if (str_detect(inspection_object, "^tasks/.+/output/")) status_for_task_path(inspection_object) else "verified"
  if (source_status == "build_failure" || inspect_status == "build_failure") {
    out$verification_status <- "build_failure"
    out$why_matters <- "The linked task output did not rebuild successfully during the audit."
    return(out)
  }

  if (method == "task_output_exists") {
    if (!file.exists(repo_abs(source_path))) {
      out$verification_status <- "build_failure"
      out$why_matters <- "The slide-linked task output is missing."
    }
    return(out)
  }

  if (method == "copy_checksum_match") {
    source_abs <- repo_abs(source_path)
    inspect_abs <- repo_abs(inspection_object)
    if (!file.exists(source_abs) || !file.exists(inspect_abs)) {
      out$verification_status <- "build_failure"
      out$why_matters <- "The local figure copy or its source output is missing."
      return(out)
    }
    src_md5 <- unname(tools::md5sum(source_abs))
    insp_md5 <- unname(tools::md5sum(inspect_abs))
    out$observed_value <- paste0("local=", src_md5, "; source=", insp_md5)
    if (!identical(src_md5, insp_md5)) {
      out$verification_status <- "stale_output"
      out$why_matters <- "The local slide copy does not match the generated task output."
    }
    return(out)
  }

  if (method == "slide_contains_all") {
    if (length(pattern_terms) == 0) {
      out$verification_status <- "unverifiable_without_fix"
      out$why_matters <- "No expected terms were provided for the definition/claim check."
      return(out)
    }
    if (!all(pattern_terms %in% slide_text)) {
      out$verification_status <- "mismatch"
      out$why_matters <- "The slide wording omits or changes part of the expected code/data definition."
    }
    return(out)
  }

  if (method == "source_text_contains_all") {
    text_obj <- normalize_claim_text(read_text_object(inspection_object))
    if (is.na(text_obj) || text_obj == "") {
      out$verification_status <- "build_failure"
      out$why_matters <- "The referenced source file could not be read."
      return(out)
    }
    out$observed_value <- str_sub(text_obj, 1, 200)
    if (!all(pattern_terms %in% text_obj)) {
      out$verification_status <- "mismatch"
      out$why_matters <- "The referenced source file does not contain the expected terms."
    }
    return(out)
  }

  if (method == "csv_value_match") {
    tbl <- load_table_object(inspection_object)
    if (is.null(tbl)) {
      out$verification_status <- "build_failure"
      out$why_matters <- "The referenced machine-readable source object is missing or unreadable."
      return(out)
    }

    if (row$source_filter_column != "" && row$source_filter_column %in% names(tbl)) {
      tbl <- tbl %>% filter(as.character(.data[[row$source_filter_column]]) == row$source_filter_value)
    }

    if (nrow(tbl) == 0 || !(row$source_column %in% names(tbl))) {
      out$verification_status <- "build_failure"
      out$why_matters <- "The referenced source column or filter did not resolve to an auditable value."
      return(out)
    }

    observed <- if (row$source_aggregate == "min") {
      min(tbl[[row$source_column]], na.rm = TRUE)
    } else if (row$source_aggregate == "max") {
      max(tbl[[row$source_column]], na.rm = TRUE)
    } else if (row$source_aggregate == "sum") {
      sum(tbl[[row$source_column]], na.rm = TRUE)
    } else if (row$source_aggregate == "n_distinct") {
      dplyr::n_distinct(tbl[[row$source_column]])
    } else {
      tbl[[row$source_column]][1]
    }

    observed_num <- suppressWarnings(as.numeric(observed))
    target_num <- suppressWarnings(as.numeric(row$comparison_value))
    tolerance_num <- suppressWarnings(as.numeric(ifelse(row$comparison_tolerance == "", 0, row$comparison_tolerance)))

    out$observed_value <- as.character(observed)

    if (!is.finite(observed_num) || !is.finite(target_num)) {
      out$verification_status <- "unverifiable_without_fix"
      out$why_matters <- "The numeric comparison could not be evaluated cleanly."
      return(out)
    }

    if (abs(observed_num - target_num) > tolerance_num) {
      out$verification_status <- "mismatch"
      out$why_matters <- "The slide number does not match the machine-readable source output."
    }
    return(out)
  }

  if (method == "manual_source_required") {
    if (source_path == "" || is.na(source_path)) {
      out$verification_status <- "unverifiable_without_fix"
      out$why_matters <- "This visible slide claim does not currently have an inspectable linked source."
    } else if (!file.exists(repo_abs(source_path))) {
      out$verification_status <- "build_failure"
      out$why_matters <- "The intended source for this claim is missing."
    }
    return(out)
  }

  out$verification_status <- "unverifiable_without_fix"
  out$why_matters <- paste("Unsupported verification method:", method)
  out
}

claim_status <- claim_manifest %>%
  left_join(
    verification_index %>%
      select(claim_id, producer_path, upstream_task_chain, first_raw_boundary, reviewer_inspection_path, bandwidth, clustering, fe_structure, grouping_level, time_window, side_sign_conventions),
    by = "claim_id"
  ) %>%
  mutate(
    subsystem = case_when(
      str_detect(section_file, "aldermen_FE|appendix_summary_stats|appendix_alderman_fe") ~ "Strictness and permits",
      str_detect(section_file, "density_analysis|appendix_density") ~ "Density and parcel geography",
      str_detect(section_file, "rental_market|appendix_rents") ~ "Rentals and sales",
      TRUE ~ "Manual dates and context claims"
    )
  ) %>%
  rowwise() %>%
  mutate(result = list(evaluate_claim(pick(everything())))) %>%
  mutate(
    verification_status = result$verification_status,
    observed_value = result$observed_value,
    evidence_path = result$evidence_path,
    why_matters = result$why_matters,
    reviewer_inspection_path = coalesce(result$reviewer_inspection_path, reviewer_inspection_path, inspection_object, source_path)
  ) %>%
  ungroup() %>%
  select(-result) %>%
  mutate(
    across(
      c(
        priority, producer_path, upstream_task_chain, first_raw_boundary,
        reviewer_inspection_path, bandwidth, clustering, fe_structure,
        grouping_level, time_window, side_sign_conventions, observed_value,
        evidence_path, why_matters
      ),
      as.character
    )
  )

strictness_assets <- asset_inventory %>%
  filter(str_detect(section_file, "aldermen_FE|appendix_alderman_fe"), asset_class == "task_output")

baseline_spec <- strictness_assets %>%
  filter(str_detect(asset_path, "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH")) %>%
  slice(1) %>%
  pull(asset_path)

spec_drift_findings <- strictness_assets %>%
  filter(str_detect(asset_path, "ptfe"), !str_detect(asset_path, "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH")) %>%
  transmute(
    section_file,
    frame_label,
    claim_id = paste0(frame_label, "__spec_drift__", row_number()),
    claim_type = "identification_claim",
    slide_text_excerpt = asset_path,
    display_object = basename(asset_path),
    source_kind = "task_output",
    source_path = asset_path,
    inspection_object = asset_path,
    priority = ifelse(str_detect(section_file, "appendix_"), "appendix", "main"),
    subsystem = "Strictness and permits",
    verification_status = "code_risk",
    observed_value = asset_path,
    evidence_path = asset_path,
    why_matters = paste0("This strictness slide uses a score spec that differs from the baseline score asset ", basename(baseline_spec), "."),
    reviewer_inspection_path = asset_path,
    producer_path = "",
    upstream_task_chain = "",
    first_raw_boundary = "",
    bandwidth = "",
    clustering = "",
    fe_structure = "",
    grouping_level = "alderman",
    time_window = "",
    side_sign_conventions = ""
  ) %>%
  mutate(across(everything(), as.character))

run_geometry_checks <- function() {
  boundary_gpkg <- file.path(root_dir, "tasks/border_segment_creation/output/boundary_segments_1320ft.gpkg")
  boundary_layers <- st_layers(boundary_gpkg)
  era_layers <- boundary_layers$name[str_detect(boundary_layers$name, "^(\\d{4}_\\d{4}|post_2023)$")]
  boundary_segments <- map_dfr(era_layers, ~ st_read(boundary_gpkg, layer = .x, quiet = TRUE) %>% mutate(layer_name = .x))
  parcel_segments <- read_csv(file.path(root_dir, "tasks/assign_segment_ids/output/parcel_segment_ids.csv"), show_col_types = FALSE)
  parcels_pre_scores <- read_csv(file.path(root_dir, "tasks/calculate_ward_boundary_distances/output/parcels_pre_scores.csv"), show_col_types = FALSE)
  merged_parcels <- read_csv(file.path(root_dir, "tasks/merge_in_scores/output/parcels_with_ward_distances.csv"), show_col_types = FALSE)
  sales_panel <- read_csv(file.path(root_dir, "tasks/merge_event_study_scores/output/sales_with_ward_distances.csv"), show_col_types = FALSE)
  rent_panel <- arrow::read_parquet(file.path(root_dir, "tasks/merge_event_study_scores/output/rent_with_ward_distances_full.parquet"))
  parcels_geom <- st_read(file.path(root_dir, "tasks/calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg"), quiet = TRUE)

  parcel_segment_joined <- parcels_pre_scores %>%
    transmute(pin = as.character(pin), dist_to_boundary) %>%
    left_join(parcel_segments %>% mutate(pin = as.character(pin)), by = "pin")

  checks <- tribble(
    ~check_id, ~metric, ~value, ~threshold, ~status, ~source_path, ~notes,
    "segments_unique_ids", nrow(boundary_segments) - n_distinct(boundary_segments$segment_id), 0, "0 duplicate segment ids", "verified", "tasks/border_segment_creation/output/boundary_segments_1320ft.gpkg", "Boundary segments should have unique segment_id values",
    "segments_expected_eras", length(era_layers), 4, "4 era line layers", ifelse(length(era_layers) == 4, "verified", "mismatch"), "tasks/border_segment_creation/output/boundary_segments_1320ft.gpkg", "Boundary segment file should span all map eras used by the deck",
    "parcel_segment_duplicate_pins", sum(duplicated(parcel_segments$pin)), 0, "0 duplicate pins", ifelse(sum(duplicated(parcel_segments$pin)) == 0, "verified", "mismatch"), "tasks/assign_segment_ids/output/parcel_segment_ids.csv", "Each pin should map to at most one boundary segment",
    "parcel_segment_missing_share", mean(is.na(parcel_segment_joined$segment_id[parcel_segment_joined$dist_to_boundary <= 1320])), 0.05, "<= 0.05 within 1320 ft", ifelse(mean(is.na(parcel_segment_joined$segment_id[parcel_segment_joined$dist_to_boundary <= 1320])) <= 0.05, "verified", "code_risk"), "tasks/assign_segment_ids/output/parcel_segment_ids.csv", "Segment coverage should be high for parcels inside the 1320 ft corridor",
    "distance_nonnegative_share", mean(parcels_pre_scores$dist_to_boundary < 0, na.rm = TRUE), 0, "0 negative distances", ifelse(mean(parcels_pre_scores$dist_to_boundary < 0, na.rm = TRUE) == 0, "verified", "mismatch"), "tasks/calculate_ward_boundary_distances/output/parcels_pre_scores.csv", "Unsigned parcel distance should never be negative",
    "merged_signed_distance_consistency", mean(sign(merged_parcels$signed_distance) == merged_parcels$sign, na.rm = TRUE), 0.99, ">= 0.99", ifelse(mean(sign(merged_parcels$signed_distance) == merged_parcels$sign, na.rm = TRUE) >= 0.99, "verified", "mismatch"), "tasks/merge_in_scores/output/parcels_with_ward_distances.csv", "Signed distance should agree with sign variable",
    "merged_segment_missing_share", mean(is.na(merged_parcels$segment_id[merged_parcels$dist_to_boundary <= 1000])), 0.10, "<= 0.10 within 1000 ft", ifelse(mean(is.na(merged_parcels$segment_id[merged_parcels$dist_to_boundary <= 1000])) <= 0.10, "verified", "code_risk"), "tasks/merge_in_scores/output/parcels_with_ward_distances.csv", "Regression-ready parcel file should have strong segment coverage inside the 1000 ft border sample",
    "sales_signed_distance_consistency", mean(sign(sales_panel$signed_dist) == sales_panel$sign, na.rm = TRUE), 0.99, ">= 0.99", ifelse(mean(sign(sales_panel$signed_dist) == sales_panel$sign, na.rm = TRUE) >= 0.99, "verified", "mismatch"), "tasks/merge_event_study_scores/output/sales_with_ward_distances.csv", "Sales signed distance should agree with sign variable",
    "rent_signed_distance_consistency", mean(sign(rent_panel$signed_dist) == rent_panel$sign, na.rm = TRUE), 0.99, ">= 0.99", ifelse(mean(sign(rent_panel$signed_dist) == rent_panel$sign, na.rm = TRUE) >= 0.99, "verified", "mismatch"), "tasks/merge_event_study_scores/output/rent_with_ward_distances_full.parquet", "Rental signed distance should agree with sign variable",
    "parcel_geometry_crs", st_crs(parcels_geom)$epsg, 3435, "EPSG 3435", ifelse(identical(st_crs(parcels_geom)$epsg, 3435L), "verified", "code_risk"), "tasks/calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg", "Parcel geometry CRS should remain in Illinois State Plane East feet"
  )

  checks %>%
    mutate(subsystem = "Density and parcel geography")
}

geometry_checks <- run_geometry_checks()

severity_from_status <- function(status, claim_type, priority) {
  case_when(
    status == "mismatch" & claim_type %in% c("manual_stat", "definition", "identification_claim") & priority == "main" ~ "P0",
    status %in% c("build_failure", "stale_output") & priority == "main" ~ "P1",
    status %in% c("mismatch", "code_risk", "stale_output") ~ "P2",
    status == "unverifiable_without_fix" & claim_type == "context_fact" ~ "P3",
    status == "unverifiable_without_fix" ~ "P2",
    TRUE ~ "P3"
  )
}

claim_findings <- bind_rows(
  claim_status,
  spec_drift_findings
) %>%
  filter(verification_status != "verified") %>%
  mutate(
    severity = severity_from_status(verification_status, claim_type, priority),
    affects_scope = ifelse(priority == "appendix", "appendix", "main deck"),
    recommended_fix_target = case_when(
      str_detect(source_path, "^tasks/") ~ source_path,
      str_detect(inspection_object, "^tasks/") ~ inspection_object,
      TRUE ~ section_file
    )
  ) %>%
  select(
    severity, subsystem, section_file, frame_label, claim_id, claim_type,
    display_object, verification_status, why_matters, evidence_path,
    upstream_task_chain, reviewer_inspection_path, recommended_fix_target,
    affects_scope, observed_value
  )

geometry_findings <- geometry_checks %>%
  filter(status != "verified") %>%
  transmute(
    severity = case_when(
      status == "mismatch" ~ "P1",
      TRUE ~ "P2"
    ),
    subsystem,
    section_file = "slides/sections/density_analysis.tex",
    frame_label = "identification-strategy",
    claim_id = check_id,
    claim_type = "identification_claim",
    display_object = check_id,
    verification_status = status,
    why_matters = notes,
    evidence_path = source_path,
    upstream_task_chain = "",
    reviewer_inspection_path = source_path,
    recommended_fix_target = source_path,
    affects_scope = "main deck",
    observed_value = as.character(metric)
  )

rerun_findings <- rerun_log %>%
  filter(build_status != 0 | !file_exists_after) %>%
  transmute(
    severity = "P1",
    subsystem = "Build reproducibility",
    section_file = "",
    frame_label = "",
    claim_id = paste0("build__", task, "__", row_number()),
    claim_type = "task_output",
    display_object = basename(output_path),
    verification_status = "build_failure",
    why_matters = "A slide-linked task output failed to rebuild during the audit.",
    evidence_path = output_path,
    upstream_task_chain = task,
    reviewer_inspection_path = output_path,
    recommended_fix_target = file.path("tasks", task, "code"),
    affects_scope = "main deck",
    observed_value = log_excerpt
  )

findings <- bind_rows(claim_findings, geometry_findings, rerun_findings) %>%
  arrange(factor(severity, levels = c("P0", "P1", "P2", "P3")), subsystem, section_file, frame_label, claim_id)

write_csv(claim_status, claim_status_output)
write_csv(geometry_checks, geometry_checks_output)
write_csv(rerun_log, rerun_log_output)
write_csv(findings, findings_output)

message("Saved: ", claim_status_output)
message("Saved: ", geometry_checks_output)
message("Saved: ", rerun_log_output)
message("Saved: ", findings_output)
