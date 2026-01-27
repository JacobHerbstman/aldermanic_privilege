################################################################################
# fwl_diagnostic_v3.R
# 
# Definitive FWL test - with and without FEs
################################################################################

source("../../setup_environment/code/packages.R")

# Load data
permits <- read_csv("../input/alderman_restrictiveness_scores_data.csv", show_col_types = FALSE)

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

alderman_counts <- permits %>% group_by(alderman) %>% summarise(n = n(), .groups = "drop")
keep_aldermen <- alderman_counts %>% filter(n >= 50) %>% pull(alderman)
permits <- permits %>% filter(alderman %in% keep_aldermen)

ref_alderman <- "Rossana Rodriguez-Sanchez"

message("\n========== DEFINITIVE FWL TEST ==========\n")

# =============================================================================
# TEST 1: Simple case - demographics only, NO fixed effects
# =============================================================================
message("=== TEST 1: Demographics only (NO FEs) ===\n")

# One-step
m1_onestep <- feols(
  log_processing_time ~ i(alderman, ref = ref_alderman) + 
    homeownership_rate + pop_10k + income_10k + share_black + share_hisp,
  data = permits
)

# Two-step FWL (residualize Y, then regress on alderman)
# Note: For FWL with OLS controls (not FEs), we just residualize Y
m1_resid <- lm(log_processing_time ~ homeownership_rate + pop_10k + income_10k + share_black + share_hisp,
               data = permits)
permits$resid1 <- resid(m1_resid)

m1_twostep <- feols(resid1 ~ i(alderman, ref = ref_alderman), data = permits)

# Compare
c1_one <- coef(m1_onestep)[str_detect(names(coef(m1_onestep)), "alderman")]
c1_two <- coef(m1_twostep)[str_detect(names(coef(m1_twostep)), "alderman")]

message("Correlation (should be ~1): ", round(cor(c1_one, c1_two), 6))
message("Max difference (should be ~0): ", round(max(abs(c1_one - c1_two)), 8))

# =============================================================================
# TEST 2: With FEs - must include FEs in BOTH stages
# =============================================================================
message("\n=== TEST 2: With FEs (month + CA) ===\n")

# One-step with FEs
m2_onestep <- feols(
  log_processing_time ~ i(alderman, ref = ref_alderman) + 
    homeownership_rate + pop_10k + income_10k + share_black + share_hisp |
    month_factor + ca_id,
  data = permits
)

# Correct two-step: must absorb FEs in BOTH stages
# Stage 1: residualize Y on demos + absorb FEs
m2_resid <- feols(
  log_processing_time ~ homeownership_rate + pop_10k + income_10k + share_black + share_hisp |
    month_factor + ca_id,
  data = permits
)
permits$resid2 <- resid(m2_resid)

# Stage 2: regress residualized Y on alderman, ALSO absorbing FEs
m2_twostep <- feols(resid2 ~ i(alderman, ref = ref_alderman) | month_factor + ca_id, data = permits)

# Compare
c2_one <- coef(m2_onestep)[str_detect(names(coef(m2_onestep)), "alderman")]
c2_two <- coef(m2_twostep)[str_detect(names(coef(m2_twostep)), "alderman")]

message("Correlation (should be ~1): ", round(cor(c2_one, c2_two), 6))
message("Max difference (should be ~0): ", round(max(abs(c2_one - c2_two)), 8))

# =============================================================================
# TEST 3: What we ACTUALLY did (incorrect approach)
# =============================================================================
message("\n=== TEST 3: What we were doing (INCORRECT) ===\n")
message("Stage 1: residualize Y on demos + FEs")
message("Stage 2: regress residuals on alderman WITHOUT FEs")

# This is WRONG because alderman is correlated with the FEs!
m3_twostep <- feols(resid2 ~ i(alderman, ref = ref_alderman), data = permits)

c3 <- coef(m3_twostep)[str_detect(names(coef(m3_twostep)), "alderman")]

message("\nCorrelation with one-step: ", round(cor(c2_one, c3), 4))
message("This is LOW because alderman effects get confounded with month/CA structure!")

# =============================================================================
# SUMMARY
# =============================================================================
message("\n\n========== SUMMARY ==========\n")
message("The original 'two-step' approach was theoretically incorrect.")
message("By not including FEs in stage 2, we allowed alderman effects to pick up")
message("month and community area variation that should have been absorbed.")
message("")
message("For proper FWL equivalence with FEs:")
message("  - Stage 1: residualize Y on controls | FEs")
message("  - Stage 2: regress residuals on alderman | FEs (SAME FEs!)")
