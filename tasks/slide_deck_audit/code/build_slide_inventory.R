source("../../setup_environment/code/packages.R")

## Build slide inventory and claim manifest
## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/slide_deck_audit/code")
# Rscript build_slide_inventory.R ../input/slides ../input/manual_slide_claims.csv ../output/slide_asset_inventory.csv ../output/slide_claim_manifest.csv

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 4) {
  slides_root_input <- args[1]
  manual_claims_input <- args[2]
  asset_inventory_output <- args[3]
  claim_manifest_output <- args[4]
} else {
  stop(
    "FATAL: Script requires 4 args: <slides_root_input> <manual_claims_input> <asset_inventory_output> <claim_manifest_output>",
    call. = FALSE
  )
}

root_dir <- normalizePath(file.path("..", "..", ".."), winslash = "/", mustWork = TRUE)
slides_root <- normalizePath(slides_root_input, winslash = "/", mustWork = TRUE)

repo_rel <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  sub(paste0("^", root_dir, "/"), "", path)
}

normalize_text <- function(x) {
  x %>%
    str_replace_all("\\\\textbf\\{([^}]*)\\}", "\\1") %>%
    str_replace_all("\\\\textit\\{([^}]*)\\}", "\\1") %>%
    str_replace_all("\\\\sim", "~") %>%
    str_replace_all("\\\\Rightarrow", "=>") %>%
    str_replace_all("\\$+", "") %>%
    str_replace_all("\\\\%", "%") %>%
    str_replace_all("\\\\_", "_") %>%
    str_replace_all("\\\\uparrow", "up") %>%
    str_replace_all("\\\\downarrow", "down") %>%
    str_replace_all("\\\\[A-Za-z]+", " ") %>%
    str_replace_all("\\{", " ") %>%
    str_replace_all("\\}", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

extract_refs <- function(text, pattern) {
  hit_mat <- str_match_all(text, pattern)[[1]]
  if (nrow(hit_mat) == 0) {
    return(character())
  }
  unique(hit_mat[, 2])
}

resolve_ref <- function(section_abs, ref) {
  normalizePath(file.path(slides_root, ref), winslash = "/", mustWork = FALSE)
}

classify_asset <- function(asset_abs, ref) {
  asset_rel <- repo_rel(asset_abs)
  case_when(
    str_detect(asset_rel, "^tasks/.+/output/") ~ "task_output",
    str_detect(asset_rel, "^slides/figures/.+\\.(pdf|png|jpg|jpeg)$") ~ "local_slide_figure",
    str_detect(asset_rel, "^slides/figures/.+\\.tex$") ~ "local_slide_tex",
    str_detect(asset_rel, "^slides/images/") ~ "local_slide_image",
    str_detect(asset_rel, "^slides/sections/") ~ "local_section_input",
    TRUE ~ "other"
  )
}

parse_frame_blocks <- function(section_abs, section_rel) {
  lines <- readLines(section_abs, warn = FALSE)
  begin_idx <- grep("^\\\\begin\\{frame", lines)
  end_idx <- grep("^\\\\end\\{frame\\}", lines)

  if (length(begin_idx) == 0 || length(end_idx) == 0) {
    return(tibble())
  }

  blocks <- map2(begin_idx, end_idx[seq_along(begin_idx)], function(start_i, end_i) {
    frame_lines <- lines[start_i:end_i]
    raw_frame_text <- paste(frame_lines, collapse = "\n")
    begin_line <- frame_lines[1]
    frame_label <- str_match(begin_line, "label=([^,\\]]+)")[, 2]
    frame_title <- str_match(raw_frame_text, "\\\\frametitle\\{([^}]*)\\}")[, 2]
    if (is.na(frame_label) || frame_label == "") {
      frame_label <- ifelse(is.na(frame_title), paste0("frame_", start_i), frame_title %>%
        str_to_lower() %>%
        str_replace_all("[^a-z0-9]+", "-") %>%
        str_replace_all("(^-|-$)", ""))
    }

    tibble(
      section_file = section_rel,
      frame_label = frame_label,
      frame_title = coalesce(frame_title, ""),
      frame_text = normalize_text(raw_frame_text),
      raw_frame_text = raw_frame_text,
      input_refs = list(extract_refs(raw_frame_text, "\\\\input\\{([^}]*)\\}")),
      graphic_refs = list(extract_refs(raw_frame_text, "\\\\includegraphics(?:\\[[^]]*\\])?\\{([^}]*)\\}")),
      section_abs = section_abs
    )
  })

  bind_rows(blocks)
}

slides_tex <- file.path(slides_root, "slides.tex")
slides_lines <- readLines(slides_tex, warn = FALSE)
section_refs <- extract_refs(paste(slides_lines, collapse = "\n"), "\\\\input\\{sections/([^}]*)\\}")
section_paths <- file.path(slides_root, "sections", section_refs)
section_paths <- section_paths[file.exists(section_paths)]

frame_inventory <- map2_dfr(section_paths, file.path("slides", "sections", basename(section_paths)), parse_frame_blocks)

asset_inventory <- bind_rows(
  frame_inventory %>%
    select(section_file, frame_label, frame_title, section_abs, input_refs) %>%
    unnest_longer(input_refs, values_to = "asset_ref") %>%
    mutate(asset_type = "input"),
  frame_inventory %>%
    select(section_file, frame_label, frame_title, section_abs, graphic_refs) %>%
    unnest_longer(graphic_refs, values_to = "asset_ref") %>%
    mutate(asset_type = "graphic")
) %>%
  filter(!is.na(asset_ref), asset_ref != "") %>%
  mutate(
    asset_abs_path = map2_chr(section_abs, asset_ref, resolve_ref),
    asset_path = repo_rel(asset_abs_path),
    asset_class = map_chr(seq_len(n()), ~ classify_asset(asset_abs_path[.x], asset_ref[.x])),
    task = case_when(
      str_detect(asset_path, "^tasks/.+/output/") ~ str_match(asset_path, "^tasks/([^/]+)/output/")[, 2],
      TRUE ~ ""
    ),
    is_appendix = str_detect(section_file, "appendix_"),
    file_exists = file.exists(asset_abs_path)
  ) %>%
  select(section_file, frame_label, frame_title, asset_type, asset_ref, asset_path, asset_class, task, is_appendix, file_exists) %>%
  arrange(section_file, frame_label, asset_type, asset_path)

manual_claims <- read_csv(manual_claims_input, show_col_types = FALSE) %>%
  mutate(across(everything(), ~ replace_na(as.character(.x), "")))

task_output_claims <- asset_inventory %>%
  filter(asset_class == "task_output") %>%
  group_by(section_file, frame_label) %>%
  mutate(asset_idx = row_number()) %>%
  ungroup() %>%
  transmute(
    section_file,
    frame_label,
    claim_id = paste0(frame_label, "__asset__", asset_idx),
    claim_type = "task_output",
    slide_text_excerpt = asset_ref,
    display_object = basename(asset_path),
    source_kind = "task_output",
    source_path = asset_path,
    upstream_task = replace(as.character(task), is.na(as.character(task)), ""),
    verification_method = "task_output_exists",
    expected_spec = asset_ref,
    inspection_object = asset_path,
    priority = ifelse(is_appendix, "appendix", "main"),
    source_column = "",
    source_filter_column = "",
    source_filter_value = "",
    source_aggregate = "",
    comparison_value = "",
    comparison_tolerance = "",
    comparison_pattern = "",
    notes = "Auto-generated from slide-linked task output"
  ) %>%
  mutate(across(everything(), as.character))

claim_manifest <- bind_rows(task_output_claims, manual_claims) %>%
  mutate(
    priority = ifelse(priority == "", ifelse(str_detect(section_file, "appendix_"), "appendix", "main"), priority),
    section_file = ifelse(str_starts(section_file, "slides/"), section_file, file.path("slides", "sections", basename(section_file)))
  ) %>%
  distinct(claim_id, .keep_all = TRUE) %>%
  arrange(priority, section_file, frame_label, claim_id)

write_csv(asset_inventory, asset_inventory_output)
write_csv(claim_manifest, claim_manifest_output)

message("Saved: ", asset_inventory_output)
message("Saved: ", claim_manifest_output)
