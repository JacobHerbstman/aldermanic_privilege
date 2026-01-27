################################################################################
# fwl_diagnostic.R
# 
# Compare one-step vs two-step approaches to verify FWL theorem equivalence
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

message("\n========== FWL DIAGNOSTIC ==========\n")

# =============================================================================
# APPROACH 1: ONE-STEP (alderman + demographics in same model)
# =============================================================================
message("--- Approach 1: One-Step (Combined) ---")
message("log_processing_time ~ alderman + demographics | month + ca_id")

model_onestep <- feols(
  log_processing_time ~ i(alderman, ref = ref_alderman) + 
    homeownership_rate + pop_10k + income_10k + share_black + share_hisp | 
    month_factor + ca_id,
  data = permits,
  cluster = ~alderman
)

# Extract alderman coefficients
coefs_onestep <- enframe(coef(model_onestep), name = "term", value = "fe_onestep") %>%
  filter(str_detect(term, "alderman::")) %>%
  mutate(alderman = str_remove(term, "alderman::")) %>%
  select(alderman, fe_onestep)

# Add reference
coefs_onestep <- bind_rows(coefs_onestep, tibble(alderman = ref_alderman, fe_onestep = 0))

message(paste("One-step N coefficients:", nrow(coefs_onestep)))

# =============================================================================
# APPROACH 2: TWO-STEP (residualize first, then alderman)
# =============================================================================
message("\n--- Approach 2: Two-Step (Residualize) ---")
message("Step 1: log_processing_time ~ demographics | month + ca_id")
message("Step 2: residuals ~ alderman")

# Stage 1: residualize
model_stage1 <- feols(
  log_processing_time ~ homeownership_rate + pop_10k + income_10k + share_black + share_hisp | 
    month_factor + ca_id,
  data = permits
)

permits$resid <- resid(model_stage1)

# Stage 2: alderman FEs on residuals
model_stage2 <- feols(
  resid ~ i(alderman, ref = ref_alderman),
  data = permits,
  cluster = ~alderman
)

# Extract alderman coefficients
coefs_twostep <- enframe(coef(model_stage2), name = "term", value = "fe_twostep") %>%
  filter(str_detect(term, "alderman::")) %>%
  mutate(alderman = str_remove(term, "alderman::")) %>%
  select(alderman, fe_twostep)

# Add reference
coefs_twostep <- bind_rows(coefs_twostep, tibble(alderman = ref_alderman, fe_twostep = 0))

message(paste("Two-step N coefficients:", nrow(coefs_twostep)))

# =============================================================================
# COMPARE THE TWO APPROACHES
# =============================================================================
message("\n--- Comparing Coefficients ---")

comparison <- coefs_onestep %>%
  inner_join(coefs_twostep, by = "alderman") %>%
  mutate(diff = fe_onestep - fe_twostep)

message(paste("Correlation between one-step and two-step coefficients:", 
              round(cor(comparison$fe_onestep, comparison$fe_twostep), 6)))

message("\nDifference statistics (one-step - two-step):")
message(paste("  Min:", round(min(comparison$diff), 6)))
message(paste("  Max:", round(max(comparison$diff), 6)))
message(paste("  Mean:", round(mean(comparison$diff), 6)))
message(paste("  SD:", round(sd(comparison$diff), 6)))

# Check if differences are essentially zero (numerical precision)
if (max(abs(comparison$diff)) < 1e-6) {
  message("\n✓ PASS: Coefficients are identical (within numerical precision)")
  message("  FWL theorem confirmed!")
} else {
  message("\n✗ FAIL: Coefficients differ by more than numerical precision")
  message("  Something is different between the approaches...")
  
  # Show biggest differences
  message("\nLargest differences:")
  print(comparison %>% arrange(desc(abs(diff))) %>% head(5))
}

# =============================================================================
# NOW CHECK: What was the ORIGINAL one-step I ran earlier?
# =============================================================================
message("\n\n--- Checking Original (NO demographics) ---")
message("Original was: log_processing_time ~ alderman | month + ca_id")
message("This is DIFFERENT from the two-step because it lacked demographic controls!")

model_original <- feols(
  log_processing_time ~ i(alderman, ref = ref_alderman) | month_factor + ca_id,
  data = permits,
  cluster = ~alderman
)

coefs_original <- enframe(coef(model_original), name = "term", value = "fe_original") %>%
  filter(str_detect(term, "alderman::")) %>%
  mutate(alderman = str_remove(term, "alderman::")) %>%
  select(alderman, fe_original)

coefs_original <- bind_rows(coefs_original, tibble(alderman = ref_alderman, fe_original = 0))

# Compare original (no demos) vs two-step (with demos)
comparison2 <- coefs_original %>%
  inner_join(coefs_twostep, by = "alderman")

message(paste("\nCorrelation between original (no demos) and two-step (with demos):", 
              round(cor(comparison2$fe_original, comparison2$fe_twostep), 4)))

message("\nThis explains the difference - the original one-step was missing demographics!")
