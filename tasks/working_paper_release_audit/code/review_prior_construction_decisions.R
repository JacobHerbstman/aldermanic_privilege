# setwd("tasks/working_paper_release_audit/code")
library(dplyr)
library(readr)
library(tidyr)

review <- read_csv("../reference/prior_construction_cases.csv", show_col_types = FALSE)
candidates <- read_csv("../input/preferred_residential_project_candidates.csv", show_col_types = FALSE)
stopifnot(nrow(review) == 10L, !anyDuplicated(review$project_id),
  !anyDuplicated(candidates$project_id), all(review$project_id %in% candidates$project_id))
review <- review |> left_join(candidates |> select(project_id, candidate_status,
  decision_reason, replacement_project_ids), by = "project_id", relationship = "one-to-one")
# A written resolution is closed only if the producer actually applied it.
stopifnot(all(review$candidate_status[review$review_result == "resolved"] ==
  "exclude_source_duplicate_keep_successors"))
replacements <- review |> filter(review_result == "resolved") |>
  select(project_id, replacement_project_id = replacement_project_ids) |>
  separate_longer_delim(replacement_project_id, delim = "/") |>
  left_join(candidates |> select(replacement_project_id = project_id,
    construction_year, dwelling_units, building_sqft, land_sqft,
    replacement_status = candidate_status), by = "replacement_project_id", relationship = "many-to-one")
stopifnot(all(replacements$replacement_status == "retain_mechanical"),
  !anyNA(replacements$construction_year), !anyNA(replacements$building_sqft), !anyNA(replacements$land_sqft))
replacement_summary <- replacements |> group_by(project_id) |> summarise(
  retained_projects = n(), retained_units = sum(dwelling_units),
  retained_measurements = paste(paste0(replacement_project_id, ": year ", construction_year,
    "; units ", dwelling_units, "; building sqft ", building_sqft, "; land sqft ", land_sqft), collapse = " | "),
  .groups = "drop")
review <- review |> left_join(replacement_summary, by = "project_id", relationship = "one-to-one")
write_csv(review, "../output/prior_construction_decision_review.csv")
