# --- Interactive Test Block ---
# setwd("tasks/new_construction_score_variants/code")
# max_permit_year <- 2022

source("../../_lib/alderman_uncertainty_helpers.R")
source("../../_lib/restricted_renovation_classification.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(max_permit_year)
}

if (length(args) != 1) {
  stop(
    "FATAL: Script requires 1 arg: <max_permit_year>",
    call. = FALSE
  )
}

max_permit_year <- as.integer(args[1])
variant_id <- "restricted_renovation"

if (!is.finite(max_permit_year)) {
  stop("FATAL: max_permit_year must be a valid integer.", call. = FALSE)
}

permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv") %>%
  filter(year <= max_permit_year)
classification_result <- classify_restricted_renovation_permits(
  permits,
  "../input/building_permits_clean.gpkg"
)
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

write_csv(
  result$alderman_index,
  sprintf("../output/alderman_uncertainty_index_%s.csv", variant_id)
)
