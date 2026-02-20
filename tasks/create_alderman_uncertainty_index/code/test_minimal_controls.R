## Test minimal controls specification
## This is a one-off test to compare with old strictness scores

source("../../setup_environment/code/packages.R")
library(fixest)

message("=== Testing MINIMAL CONTROLS specification ===")

# Load data
permits <- read_csv("../input/permits_for_uncertainty_index.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))

# Filter aldermen with >3 months
coverage <- permits %>%
  group_by(alderman) %>%
  summarise(n_months = n_distinct(month), .groups = "drop")

keep_aldermen <- coverage %>%
  filter(n_months > 3) %>%
  pull(alderman)

permits <- permits %>% filter(alderman %in% keep_aldermen)

message("Permits: ", nrow(permits))
message("Aldermen: ", n_distinct(permits$alderman))

# MINIMAL CONTROLS - just ward demographics like old method
covariates <- c(
  "median_hh_income", "share_black", "share_hisp", "share_white",
  "homeownership_rate", "share_bach_plus", "pop_total"
)

# Month + review type FE only (no CA FE)
fe_terms <- c("month", "review_type_clean")

# Build formula
covar_part <- paste(covariates, collapse = " + ")
fe_part <- paste(fe_terms, collapse = " + ")
formula_str <- paste0("log_processing_time ~ ", covar_part, " | ", fe_part)
message("Formula: ", formula_str)

# Simple filter - just need log_processing_time to be valid
permits_for_reg <- permits %>%
  filter(!is.na(log_processing_time) & is.finite(log_processing_time)) %>%
  filter(!is.na(review_type_clean)) %>%
  filter(!is.na(month))

message("Permits for regression: ", nrow(permits_for_reg))

# Run regression
model <- feols(as.formula(formula_str), data = permits_for_reg, vcov = ~ward)

message("Model obs: ", nobs(model))
message("R-squared: ", round(r2(model), 4))

# Extract residuals - handle dropped obs
if (model$nobs < nrow(permits_for_reg)) {
  resid_vec <- rep(NA_real_, nrow(permits_for_reg))
  kept_obs <- model$obs_selection$obsRemoved
  if (!is.null(kept_obs)) {
    resid_vec[!kept_obs] <- residuals(model)
  }
  permits_for_reg$resid <- resid_vec
} else {
  permits_for_reg$resid <- residuals(model)
}

# Aggregate to alderman level (simple mean residual)
alderman_moments <- permits_for_reg %>%
  filter(!is.na(resid)) %>%
  group_by(alderman) %>%
  summarise(
    n_permits = n(),
    mean_resid = mean(resid, na.rm = TRUE),
    .groups = "drop"
  )

# Standardize
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

alderman_index <- alderman_moments %>%
  mutate(uncertainty_index = standardize(mean_resid))

# Save output
write_csv(alderman_index, "../output/alderman_uncertainty_index_MINIMAL_CONTROLS.csv")

# Print key aldermen comparison
message("\n=== KEY ALDERMEN COMPARISON ===")
key_aldermen <- c("Brian Hopkins", "Desmon Yancy", "Ed Smith", "Michael Chandler", 
                  "Isaac Carothers", "Emma Mitts", "Walter Burnett, Jr.", 
                  "Jeanette Taylor", "Howard Brookins", "Latasha Thomas",
                  "Patrick Daley Thompson", "Byron Sigcho-Lopez")

comparison <- alderman_index %>%
  filter(alderman %in% key_aldermen) %>%
  arrange(uncertainty_index) %>%
  select(alderman, uncertainty_index)

print(comparison, n = 20)

message("\nSaved: ../output/alderman_uncertainty_index_MINIMAL_CONTROLS.csv")
