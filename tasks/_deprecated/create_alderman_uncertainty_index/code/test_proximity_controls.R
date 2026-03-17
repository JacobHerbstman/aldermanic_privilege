## Test adding proximity controls incrementally
## Compare minimal controls vs adding CTA + lake distance

source("../../setup_environment/code/packages.R")
library(fixest)

message("=== Testing Proximity Controls Variations ===")

# Load data
permits <- read_csv("../input/permits_for_uncertainty_index.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))

# Load OLD scores for comparison
old_scores <- read_csv("../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv", 
                       show_col_types = FALSE) %>%
  rename(old_score = strictness_index)

# Filter aldermen with >3 months
coverage <- permits %>%
  group_by(alderman) %>%
  summarise(n_months = n_distinct(month), .groups = "drop")

keep_aldermen <- coverage %>%
  filter(n_months > 3) %>%
  pull(alderman)

permits <- permits %>% filter(alderman %in% keep_aldermen)

message("Total permits: ", nrow(permits))

# Key aldermen to track
key_aldermen <- c("Brian Hopkins", "Desmon Yancy", "Ed Smith", "Michael Chandler", 
                  "Isaac Carothers", "Emma Mitts", "Walter Burnett, Jr.", 
                  "Jeanette Taylor", "Howard Brookins", "Latasha Thomas",
                  "Patrick Daley Thompson", "Byron Sigcho-Lopez")

# Helper function to run a spec and extract key aldermen
run_spec <- function(covariates, spec_name, data) {
  fe_terms <- c("month", "review_type_clean")
  covar_part <- paste(covariates, collapse = " + ")
  fe_part <- paste(fe_terms, collapse = " + ")
  formula_str <- paste0("log_processing_time ~ ", covar_part, " | ", fe_part)
  
  # Filter to complete cases for ALL covariates needed
  all_vars <- c("log_processing_time", "review_type_clean", "month", "alderman", "ward", covariates)
  permits_for_reg <- data %>%
    filter(!is.na(log_processing_time) & is.finite(log_processing_time)) %>%
    filter(!is.na(review_type_clean) & !is.na(month)) %>%
    filter(if_all(all_of(covariates), ~!is.na(.)))
  
  message(sprintf("  %s: %d obs", spec_name, nrow(permits_for_reg)))
  
  if (nrow(permits_for_reg) < 1000) {
    message("    WARNING: Too few observations")
    return(NULL)
  }
  
  model <- tryCatch(
    feols(as.formula(formula_str), data = permits_for_reg, vcov = ~ward),
    error = function(e) { message("    ERROR: ", e$message); NULL }
  )
  
  if (is.null(model)) return(NULL)
  
  # Extract residuals
  permits_for_reg$resid <- residuals(model)
  
  # Aggregate
  alderman_moments <- permits_for_reg %>%
    filter(!is.na(resid)) %>%
    group_by(alderman) %>%
    summarise(mean_resid = mean(resid, na.rm = TRUE), .groups = "drop")
  
  # Standardize
  alderman_moments <- alderman_moments %>%
    mutate(index = (mean_resid - mean(mean_resid)) / sd(mean_resid))
  
  # Return key aldermen
  result <- alderman_moments %>%
    filter(alderman %in% key_aldermen) %>%
    select(alderman, index) %>%
    rename(!!spec_name := index)
  
  return(result)
}

# Base covariates (minimal)
base_covs <- c("median_hh_income", "share_black", "share_hisp", "share_white",
               "homeownership_rate", "share_bach_plus", "pop_total")

message("\n--- Running multiple specifications ---")

# Run specs
specs <- list()
specs[["minimal"]] <- run_spec(base_covs, "minimal", permits)
specs[["plus_cta"]] <- run_spec(c(base_covs, "nearest_cta_stop_dist_ft"), "plus_cta", permits)
specs[["plus_lake"]] <- run_spec(c(base_covs, "lake_michigan_dist_ft"), "plus_lake", permits)
specs[["plus_cta_lake"]] <- run_spec(c(base_covs, "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"), "plus_cta_lake", permits)
specs[["plus_metra"]] <- run_spec(c(base_covs, "nearest_metra_stop_dist_ft"), "plus_metra", permits)
specs[["plus_transit"]] <- run_spec(c(base_covs, "nearest_cta_stop_dist_ft", "nearest_metra_stop_dist_ft"), "plus_transit", permits)
specs[["plus_transit_lake"]] <- run_spec(c(base_covs, "nearest_cta_stop_dist_ft", "nearest_metra_stop_dist_ft", "lake_michigan_dist_ft"), "plus_transit_lake", permits)
specs[["plus_park"]] <- run_spec(c(base_covs, "nearest_park_dist_ft"), "plus_park", permits)
specs[["plus_geo"]] <- run_spec(c(base_covs, "lake_michigan_dist_ft", "nearest_park_dist_ft"), "plus_geo", permits)
specs[["plus_all"]] <- run_spec(c(base_covs, "nearest_cta_stop_dist_ft", "nearest_metra_stop_dist_ft", 
                                   "lake_michigan_dist_ft", "nearest_park_dist_ft"), "plus_all", permits)

# Remove NULL specs
specs <- specs[!sapply(specs, is.null)]

# Combine all results
if (length(specs) > 0) {
  comparison <- specs[[1]]
  if (length(specs) > 1) {
    for (i in 2:length(specs)) {
      comparison <- comparison %>% left_join(specs[[i]], by = "alderman")
    }
  }
  
  # Add old scores
  comparison <- comparison %>%
    left_join(old_scores, by = "alderman") %>%
    arrange(old_score)
  
  message("\n=== COMPARISON OF KEY ALDERMEN ACROSS SPECS ===")
  message("(sorted by OLD score)")
  print(comparison, n = 15, width = 300)
  
  # Calculate correlations with old scores
  spec_names <- names(specs)
  cors <- sapply(spec_names, function(s) {
    if (s %in% names(comparison)) {
      cor(comparison$old_score, comparison[[s]], use = "complete.obs")
    } else {
      NA
    }
  })
  cors_df <- data.frame(spec = names(cors), correlation = round(cors, 3)) %>% 
    filter(!is.na(correlation)) %>%
    arrange(desc(correlation))
  
  message("\n=== CORRELATIONS WITH OLD SCORES ===")
  print(cors_df, n = 20)
  
  # Save
  write_csv(comparison, "../output/proximity_controls_comparison.csv")
  write_csv(cors_df, "../output/proximity_controls_correlations.csv")
  message("\nSaved: ../output/proximity_controls_comparison.csv")
  message("Saved: ../output/proximity_controls_correlations.csv")
} else {
  message("ERROR: No specs completed successfully")
}
