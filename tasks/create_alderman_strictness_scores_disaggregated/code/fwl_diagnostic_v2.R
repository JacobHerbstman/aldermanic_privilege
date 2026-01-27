################################################################################
# fwl_diagnostic_v2.R
# 
# Proper FWL implementation - must partial out controls from BOTH sides
################################################################################

source("../../setup_environment/code/packages.R")

# Load data
permits <- read_csv("../input/alderman_restrictiveness_scores_data.csv", show_col_types = FALSE)

# Create variables
permits <- permits %>%
  mutate(
    ca_id = as.factor(community_area),
    month_factor = as.factor(month),
    pop_10k = pop_total / 10000,
    income_10k = median_hh_income / 10000
  ) %>%
  filter(
    !is.na(log_processing_time),
    !is.na(alderman),
    !is.na(month),
    !is.na(ca_id),
    is.finite(log_processing_time)
  )

# Filter to aldermen with >= 50 permits
alderman_counts <- permits %>% group_by(alderman) %>% summarise(n = n(), .groups = "drop")
keep_aldermen <- alderman_counts %>% filter(n >= 50) %>% pull(alderman)
permits <- permits %>% filter(alderman %in% keep_aldermen)

# Reference alderman
ref_alderman <- "Rossana Rodriguez-Sanchez"

message("\n========== FWL DIAGNOSTIC V2 ==========\n")
message("FWL Theorem: To get equivalent coefficients in two steps,")
message("you must partial out the controls from BOTH Y and X.\n")

# =============================================================================
# APPROACH 1: ONE-STEP (benchmark)
# =============================================================================
message("--- Approach 1: One-Step (Benchmark) ---")

model_onestep <- feols(
  log_processing_time ~ i(alderman, ref = ref_alderman) + 
    homeownership_rate + pop_10k + income_10k + share_black + share_hisp | 
    month_factor + ca_id,
  data = permits
)

coefs_onestep <- enframe(coef(model_onestep), name = "term", value = "fe_onestep") %>%
  filter(str_detect(term, "alderman::")) %>%
  mutate(alderman = str_remove(term, "alderman::")) %>%
  select(alderman, fe_onestep)
coefs_onestep <- bind_rows(coefs_onestep, tibble(alderman = ref_alderman, fe_onestep = 0))

# =============================================================================
# APPROACH 2: INCORRECT Two-Step (only residualize Y)
# =============================================================================
message("\n--- Approach 2: INCORRECT Two-Step (only residualize Y) ---")
message("This is what we were doing - it's WRONG for FWL!")

# Only residualize Y
model_resid_y <- feols(
  log_processing_time ~ homeownership_rate + pop_10k + income_10k + share_black + share_hisp | 
    month_factor + ca_id,
  data = permits
)
permits$resid_y <- resid(model_resid_y)

# Run on alderman (but alderman also has month/CA structure not partialed out!)
model_incorrect <- feols(resid_y ~ i(alderman, ref = ref_alderman), data = permits)

coefs_incorrect <- enframe(coef(model_incorrect), name = "term", value = "fe_incorrect") %>%
  filter(str_detect(term, "alderman::")) %>%
  mutate(alderman = str_remove(term, "alderman::")) %>%
  select(alderman, fe_incorrect)
coefs_incorrect <- bind_rows(coefs_incorrect, tibble(alderman = ref_alderman, fe_incorrect = 0))

# =============================================================================
# APPROACH 3: CORRECT Two-Step (residualize both Y and X)
# For alderman indicators, this means including the FEs in stage 2
# =============================================================================
message("\n--- Approach 3: CORRECT Two-Step ---")
message("Option A: Just include the FEs in stage 2!")

# Stage 1: residualize Y on demographics only (FEs will be in stage 2)
model_resid_y_partial <- feols(
  log_processing_time ~ homeownership_rate + pop_10k + income_10k + share_black + share_hisp,
  data = permits
)
permits$resid_y_partial <- resid(model_resid_y_partial)

# Stage 2: Include FEs with alderman
model_correct <- feols(
  resid_y_partial ~ i(alderman, ref = ref_alderman) | month_factor + ca_id,
  data = permits
)

coefs_correct <- enframe(coef(model_correct), name = "term", value = "fe_correct") %>%
  filter(str_detect(term, "alderman::")) %>%
  mutate(alderman = str_remove(term, "alderman::")) %>%
  select(alderman, fe_correct)
coefs_correct <- bind_rows(coefs_correct, tibble(alderman = ref_alderman, fe_correct = 0))

# =============================================================================
# COMPARE ALL APPROACHES
# =============================================================================
message("\n\n========== RESULTS ==========\n")

comparison <- coefs_onestep %>%
  inner_join(coefs_incorrect, by = "alderman") %>%
  inner_join(coefs_correct, by = "alderman")

message("Correlation: One-step vs Incorrect Two-step: ", 
        round(cor(comparison$fe_onestep, comparison$fe_incorrect), 4))

message("Correlation: One-step vs Correct Two-step:   ", 
        round(cor(comparison$fe_onestep, comparison$fe_correct), 4))

message("\nMax difference (One-step vs Correct): ", 
        round(max(abs(comparison$fe_onestep - comparison$fe_correct)), 8))

if (max(abs(comparison$fe_onestep - comparison$fe_correct)) < 1e-6) {
  message("\n✓ CORRECT two-step matches one-step perfectly!")
} else {
  message("\n✗ Still some difference...")
}

message("\n\nKEY INSIGHT:")
message("The 'incorrect' two-step (residualize Y, then regress on alderman without FEs)")
message("gives DIFFERENT results because alderman is correlated with month/CA.")
message("If an alderman serves only in certain months or CAs, their effect gets confounded.")
