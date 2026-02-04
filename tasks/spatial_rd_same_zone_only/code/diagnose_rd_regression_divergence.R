# diagnose_rd_regression_divergence.R
# Identify which ward pairs/borders are driving the divergence between 
# regression results (negative) and visual RD (null/positive)

source("../../setup_environment/code/packages.R")

message("=== Diagnosing RD vs Regression Divergence ===")

# Load the data
dat <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  filter(arealotsf > 1) %>%
  filter(areabuilding > 1) %>%
  filter(unitscount > 1) %>%
  # filter(unitscount > 1 & unitscount <= 100) %>%
  filter(construction_year >= 2006) %>%
  mutate(
    log_far = log(density_far),
    side = as.integer(signed_distance > 0)  # 1 = stricter side, 0 = lenient side
  )

bw <- 500  # bandwidth in feet
dat_bw <- dat %>% filter(abs(signed_distance) <= bw)

message(sprintf("Parcels within %d ft bandwidth: %d", bw, nrow(dat_bw)))

# =============================================================================
# 1. WARD PAIR LEVEL ANALYSIS
# For each ward pair, compute mean density on each side of boundary
# =============================================================================
message("\n=== WARD PAIR LEVEL ANALYSIS ===")

ward_pair_stats <- dat_bw %>%
  filter(!is.na(strictness_own) & !is.na(strictness_neighbor)) %>%
  group_by(ward_pair, alderman_own, alderman_neighbor) %>%
  summarise(
    n_parcels = n(),
    n_strict_side = sum(side == 1, na.rm = TRUE),
    n_lenient_side = sum(side == 0, na.rm = TRUE),
    mean_far_strict = mean(density_far[side == 1], na.rm = TRUE),
    mean_far_lenient = mean(density_far[side == 0], na.rm = TRUE),
    strictness_own = mean(strictness_own, na.rm = TRUE),
    strictness_neighbor = mean(strictness_neighbor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_strict_side >= 1 & n_lenient_side >= 1) %>%  # Need obs on both sides
  mutate(
    # Difference: strict - lenient (should be NEGATIVE if theory is correct)
    far_diff = mean_far_strict - mean_far_lenient,
    strictness_diff = abs(strictness_own - strictness_neighbor),
    # Weight by sample size
    contribution = n_parcels * abs(far_diff),
    # Does this pair support or contradict the theory?
    supports_theory = far_diff < 0  # strict side should have LOWER density
  )

# Summary stats
n_support <- sum(ward_pair_stats$supports_theory, na.rm = TRUE)
n_contradict <- sum(!ward_pair_stats$supports_theory, na.rm = TRUE)
n_support_obs <- sum(ward_pair_stats$n_parcels[ward_pair_stats$supports_theory], na.rm = TRUE)
n_contradict_obs <- sum(ward_pair_stats$n_parcels[!ward_pair_stats$supports_theory], na.rm = TRUE)

message(sprintf("\nWard pairs supporting theory (strict < lenient): %d (%d parcels)", n_support, n_support_obs))
message(sprintf("Ward pairs contradicting theory (strict > lenient): %d (%d parcels)", n_contradict, n_contradict_obs))

# Top ward pairs CONTRADICTING theory (driving positive visual RD)
message("\n=== TOP 15 WARD PAIRS CONTRADICTING THEORY ===")
message("(These make the visual RD look positive)")

top_contradict <- ward_pair_stats %>%
  filter(!supports_theory) %>%
  arrange(desc(contribution)) %>%
  head(15) %>%
  select(ward_pair, alderman_own, alderman_neighbor, n_parcels, 
         mean_far_strict, mean_far_lenient, far_diff, strictness_diff)

print(top_contradict, n = 15, width = 200)

# Top ward pairs SUPPORTING theory
message("\n=== TOP 15 WARD PAIRS SUPPORTING THEORY ===")
top_support <- ward_pair_stats %>%
  filter(supports_theory) %>%
  arrange(desc(contribution)) %>%
  head(15) %>%
  select(ward_pair, alderman_own, alderman_neighbor, n_parcels,
         mean_far_strict, mean_far_lenient, far_diff, strictness_diff)

print(top_support, n = 15, width = 200)

# =============================================================================
# 2. BY ALDERMAN: Which aldermen have parcels that contradict theory?
# =============================================================================
message("\n=== ALDERMEN WITH MOST CONTRADICTING PARCELS ===")

alderman_contradict <- dat_bw %>%
  filter(!is.na(signed_distance) & !is.na(strictness_own)) %>%
  # For each parcel, does it support theory?
  # If side=1 (strict), it should have LOWER density than neighbors on side=0
  group_by(ward_pair) %>%
  mutate(
    ward_mean_strict = mean(density_far[side == 1], na.rm = TRUE),
    ward_mean_lenient = mean(density_far[side == 0], na.rm = TRUE),
    pair_contradicts = ward_mean_strict > ward_mean_lenient
  ) %>%
  ungroup() %>%
  filter(pair_contradicts) %>%  # Only contradicting pairs
  group_by(alderman_own) %>%
  summarise(
    n_contradict_parcels = n(),
    mean_strictness = mean(strictness_own, na.rm = TRUE),
    mean_far = mean(density_far, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_contradict_parcels))

print(head(alderman_contradict, 15), n = 15)

# =============================================================================
# 3. ZONE-SPECIFIC ANALYSIS
# Are certain zones driving the contradiction?
# =============================================================================
message("\n=== ZONES WITH MOST CONTRADICTION ===")

zone_analysis <- dat_bw %>%
  filter(!is.na(signed_distance) & !is.na(zone_code)) %>%
  group_by(zone_code, ward_pair) %>%
  summarise(
    n = n(),
    n_strict = sum(side == 1, na.rm = TRUE),
    n_lenient = sum(side == 0, na.rm = TRUE),
    mean_strict = mean(density_far[side == 1], na.rm = TRUE),
    mean_lenient = mean(density_far[side == 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_strict >= 1 & n_lenient >= 1) %>%
  mutate(
    diff = mean_strict - mean_lenient,
    contradicts = diff > 0
  ) %>%
  group_by(zone_code) %>%
  summarise(
    n_pairs = n(),
    n_contradict_pairs = sum(contradicts, na.rm = TRUE),
    pct_contradict = 100 * n_contradict_pairs / n_pairs,
    total_parcels = sum(n),
    .groups = "drop"
  ) %>%
  filter(total_parcels >= 10) %>%
  arrange(desc(pct_contradict))

print(head(zone_analysis, 15), n = 15)

# =============================================================================
# 4. SAVE OUTPUTS
# =============================================================================
write_csv(ward_pair_stats, "../output/diagnostic_rd_ward_pairs.csv")
write_csv(alderman_contradict, "../output/diagnostic_rd_aldermen_contradict.csv")
write_csv(zone_analysis, "../output/diagnostic_rd_zones.csv")

message("\n=== SAVED DIAGNOSTICS ===")
message("  - ../output/diagnostic_rd_ward_pairs.csv")
message("  - ../output/diagnostic_rd_aldermen_contradict.csv")
message("  - ../output/diagnostic_rd_zones.csv")
