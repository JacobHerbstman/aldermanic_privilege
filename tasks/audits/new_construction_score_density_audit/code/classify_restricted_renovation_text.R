## Classify renovation permit text for the restricted-renovation score

source("../../../setup_environment/code/packages.R")

options(audit_source_wd = getwd())
setwd("../../../new_construction_score_variants/code")
source("../../_lib/alderman_uncertainty_helpers.R")
setwd(getOption("audit_source_wd"))
options(audit_source_wd = NULL)
source("../../../_lib/restricted_renovation_classification.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/new_construction_score_density_audit/code")
# permits_input <- "../input/permits_for_uncertainty_index.csv"
# building_permits_input <- "../input/building_permits_clean.gpkg"
# classification_output <- "../output/renovation_text_classification_restricted_renovation.csv"
# summary_output <- "../output/renovation_text_classification_summary_restricted_renovation.csv"
# porch_summary_output <- "../output/renovation_text_classification_porch_keyword_summary_restricted_renovation.csv"
# mixed_summary_output <- "../output/renovation_text_classification_mixed_job_summary_restricted_renovation.csv"
# review_output <- "../output/renovation_text_classification_dropped_description_review_restricted_renovation.csv"
# filtered_permits_output <- "../output/permits_for_uncertainty_index_restricted_renovation.csv"
# max_permit_year <- 2022

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    permits_input,
    building_permits_input,
    classification_output,
    summary_output,
    porch_summary_output,
    mixed_summary_output,
    review_output,
    filtered_permits_output,
    max_permit_year
  )
}

if (length(args) >= 9) {
  permits_input <- args[1]
  building_permits_input <- args[2]
  classification_output <- args[3]
  summary_output <- args[4]
  porch_summary_output <- args[5]
  mixed_summary_output <- args[6]
  review_output <- args[7]
  filtered_permits_output <- args[8]
  max_permit_year <- as.integer(args[9])
} else {
  stop(
    "FATAL: Script requires 9 args: <permits_input> <building_permits_input> <classification_output> <summary_output> <porch_summary_output> <mixed_summary_output> <review_output> <filtered_permits_output> <max_permit_year>",
    call. = FALSE
  )
}

if (!is.finite(max_permit_year)) {
  stop("FATAL: max_permit_year must be a valid integer.", call. = FALSE)
}

permits <- load_uncertainty_permits(permits_input) %>%
  filter(year <= max_permit_year)

classification_result <- classify_restricted_renovation_permits(permits, building_permits_input)

dropped_buckets <- c(
  "telecom_infrastructure",
  "site_other_specialized",
  "porch_deck_stair_only",
  "generic_other",
  "missing_description"
)
renovation_classification <- classification_result$classification
filtered_permits <- classification_result$filtered_permits
summary_order <- classification_result$summary_order

classification_summary <- renovation_classification %>%
  count(renovation_text_bucket, keep_restricted_renovation, name = "n") %>%
  mutate(
    total_renovation = nrow(renovation_classification),
    total_nonmissing_description = sum(renovation_classification$has_description),
    share_of_all = n / total_renovation
  ) %>%
  mutate(renovation_text_bucket = factor(renovation_text_bucket, levels = summary_order)) %>%
  arrange(renovation_text_bucket) %>%
  mutate(renovation_text_bucket = as.character(renovation_text_bucket))

porch_keyword_summary <- renovation_classification %>%
  filter(has_porch_deck_stair_keyword) %>%
  count(renovation_text_bucket, keep_restricted_renovation, name = "n") %>%
  mutate(
    total_porch_keyword_rows = sum(n),
    share_of_porch_keyword_rows = n / total_porch_keyword_rows
  ) %>%
  mutate(renovation_text_bucket = factor(renovation_text_bucket, levels = summary_order)) %>%
  arrange(renovation_text_bucket) %>%
  mutate(renovation_text_bucket = as.character(renovation_text_bucket))

mixed_job_summary <- tibble(
  total_renovation_rows = nrow(renovation_classification),
  total_nonmissing_description_rows = sum(renovation_classification$has_description),
  missing_description_rows = sum(!renovation_classification$has_description),
  total_porch_keyword_rows = sum(renovation_classification$has_porch_deck_stair_keyword),
  porch_keyword_rows_porch_only = sum(renovation_classification$renovation_text_bucket == "porch_deck_stair_only"),
  porch_keyword_rows_mixed_substantive = sum(renovation_classification$naive_regex_wrong_drop),
  naive_regex_drop_rows = sum(renovation_classification$naive_regex_drop),
  naive_regex_wrong_drop_rows = sum(renovation_classification$naive_regex_wrong_drop),
  naive_regex_wrong_drop_share_of_porch_keyword_rows = mean(
    renovation_classification$naive_regex_wrong_drop[renovation_classification$has_porch_deck_stair_keyword]
  ),
  naive_regex_wrong_drop_share_of_naive_drop_rows = sum(renovation_classification$naive_regex_wrong_drop) /
    sum(renovation_classification$naive_regex_drop)
)

dropped_description_review <- renovation_classification %>%
  filter(renovation_text_bucket %in% dropped_buckets, has_description) %>%
  count(renovation_text_bucket, work_description, sort = TRUE, name = "n") %>%
  group_by(renovation_text_bucket) %>%
  mutate(
    bucket_total = sum(n),
    share_within_bucket = n / bucket_total,
    bucket_rank = row_number()
  ) %>%
  ungroup() %>%
  filter(bucket_rank <= 40)

write_csv(renovation_classification, classification_output)
write_csv(classification_summary, summary_output)
write_csv(porch_keyword_summary, porch_summary_output)
write_csv(mixed_job_summary, mixed_summary_output)
write_csv(dropped_description_review, review_output)
write_csv(filtered_permits, filtered_permits_output)

message("Saved restricted-renovation classification outputs:")
message("  Classification CSV: ", classification_output)
message("  Summary CSV: ", summary_output)
message("  Porch-keyword summary CSV: ", porch_summary_output)
message("  Mixed-job summary CSV: ", mixed_summary_output)
message("  Dropped-description review CSV: ", review_output)
message("  Filtered permits CSV: ", filtered_permits_output)
