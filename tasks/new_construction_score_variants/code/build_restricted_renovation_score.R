## Build restricted-renovation alderman stringency score

source("../../_lib/alderman_uncertainty_helpers.R")
source("../../_lib/restricted_renovation_classification.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_variants/code")
# permits_input <- "../input/permits_for_uncertainty_index.csv"
# building_permits_input <- "../input/building_permits_clean.gpkg"
# score_output <- "../output/alderman_uncertainty_index_restricted_renovation.csv"
# variant_id <- "restricted_renovation"
# variant_label <- "Restricted renovation"
# max_permit_year <- 2022

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    permits_input,
    building_permits_input,
    score_output,
    variant_id,
    variant_label,
    max_permit_year
  )
}

if (length(args) == 6) {
  permits_input <- args[1]
  building_permits_input <- args[2]
  score_output <- args[3]
  variant_id <- args[4]
  variant_label <- args[5]
  max_permit_year <- as.integer(args[6])
} else {
  stop(
    "FATAL: Script requires 6 args: <permits_input> <building_permits_input> <score_output> <variant_id> <variant_label> <max_permit_year>",
    call. = FALSE
  )
}
if (!is.finite(max_permit_year)) {
  stop("FATAL: max_permit_year must be a valid integer.", call. = FALSE)
}

permits <- load_uncertainty_permits(permits_input) %>%
  filter(year <= max_permit_year)
classification_result <- classify_restricted_renovation_permits(permits, building_permits_input)
permits <- classification_result$filtered_permits
config <- default_uncertainty_config()

result <- build_residualized_uncertainty_index(
  permits = permits,
  config = config,
  variant_id = variant_id,
  stage1_outcome = "log_processing_time",
  drop_covariates = c("share_bach_plus"),
  construction_rule = paste(
    paste0("Through-", max_permit_year, " baseline residualized score keeping all non-renovation high-discretion permits"),
    "and only broad substantive renovation text buckets:",
    "addition_expansion, unit_reconfiguration, tenant_commercial_buildout,",
    "rehab_interior_remodel, facade_masonry_envelope, systems_mep."
  )
)

write_csv(result$alderman_index, score_output)

message("Saved restricted-renovation score:")
message("  Variant: ", variant_label)
message("  Score: ", score_output)
