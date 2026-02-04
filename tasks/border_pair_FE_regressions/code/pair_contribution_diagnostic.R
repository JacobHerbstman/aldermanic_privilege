# pair_contribution_diagnostic.R
# Identify which alderman pairs and ward pairs drive within-boundary variation
# This helps understand how sensitive results are to specific score changes

source("../../setup_environment/code/packages.R")

# ── DATA ──────────────────────────────────────────────────────────────────────
message("Loading data...")
parcels <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  filter(arealotsf > 1) %>%
  filter(areabuilding > 1) %>%
  filter(unitscount > 1 & unitscount <= 100) %>%
  filter(construction_year >= 2006) %>%
  # Create density_far if not present
  mutate(density_far = areabuilding / arealotsf)

message(sprintf("Loaded %d parcels", nrow(parcels)))

# ── PARAMETERS ────────────────────────────────────────────────────────────────
bw_ft <- 500  # bandwidth in feet

df <- parcels %>% filter(dist_to_boundary <= bw_ft)
message(sprintf("\nParcels within %d ft bandwidth: %d", bw_ft, nrow(df)))

# =============================================================================
# 1. ALDERMAN PAIR SUMMARY
# Which alderman pairs (own-neighbor) have the most observations?
# =============================================================================
message("\n=== ALDERMAN PAIR CONTRIBUTION ===\n")

alderman_pair_summary <- df %>%
  filter(!is.na(alderman_own) & !is.na(alderman_neighbor)) %>%
  mutate(
    alderman_pair = paste0(
      pmin(alderman_own, alderman_neighbor), " / ",
      pmax(alderman_own, alderman_neighbor)
    ),
    strictness_diff = abs(strictness_own - strictness_neighbor)
  ) %>%
  group_by(alderman_pair, alderman_own, alderman_neighbor) %>%
  summarise(
    n_parcels = n(),
    mean_strictness_own = mean(strictness_own, na.rm = TRUE),
    mean_strictness_neighbor = mean(strictness_neighbor, na.rm = TRUE),
    strictness_diff = mean(strictness_diff, na.rm = TRUE),
    mean_density_far = mean(density_far, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Contribution metric: more parcels * larger diff = more leverage
  mutate(
    contribution_score = n_parcels * strictness_diff
  ) %>%
  arrange(desc(contribution_score))

message("Top 20 alderman pairs by contribution (n_parcels × strictness_diff):\n")
print(head(alderman_pair_summary %>% select(alderman_pair, n_parcels, strictness_diff, contribution_score), 20), n = 20)

# =============================================================================
# 2. WARD PAIR SUMMARY (BY ZONE)
# Which ward pair × zone combinations provide identifying variation?
# =============================================================================
message("\n=== WARD PAIR × ZONE VARIATION ===\n")

ward_zone_summary <- df %>%
  filter(!is.na(strictness_own)) %>%
  group_by(ward_pair, zone_code) %>%
  summarise(
    n_obs = n(),
    n_strict_side = sum(sign > 0, na.rm = TRUE),
    n_lenient_side = sum(sign < 0, na.rm = TRUE),
    sd_strictness = sd(strictness_own, na.rm = TRUE),
    mean_strictness_diff = mean(abs(strictness_own - strictness_neighbor), na.rm = TRUE),
    mean_density_far = mean(density_far, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_obs > 1 & !is.na(sd_strictness) & sd_strictness > 0) %>%
  # Has variation on BOTH sides?
  mutate(
    has_both_sides = n_strict_side >= 1 & n_lenient_side >= 1,
    contribution_score = n_obs * sd_strictness
  ) %>%
  arrange(desc(contribution_score))

message(sprintf("Ward-pair × zone groups with variation: %d", nrow(ward_zone_summary)))
message(sprintf("Groups with observations on BOTH sides: %d", sum(ward_zone_summary$has_both_sides)))
message(sprintf("Total obs in groups with both sides: %d", 
  sum(ward_zone_summary$n_obs[ward_zone_summary$has_both_sides])))

message("\nTop 20 ward-pair × zone combinations by contribution:\n")
print(head(ward_zone_summary %>% 
  filter(has_both_sides) %>%
  select(ward_pair, zone_code, n_obs, n_strict_side, n_lenient_side, sd_strictness, contribution_score), 20), n = 20)

# =============================================================================
# 3. INDIVIDUAL ALDERMAN SUMMARY
# How many parcels does each alderman have vs their neighbor?
# =============================================================================
message("\n=== INDIVIDUAL ALDERMAN CONTRIBUTION ===\n")

alderman_stats <- df %>%
  filter(!is.na(alderman_own) & !is.na(strictness_own)) %>%
  group_by(alderman_own) %>%
  summarise(
    n_parcels = n(),
    strictness = mean(strictness_own, na.rm = TRUE),
    mean_neighbor_strictness = mean(strictness_neighbor, na.rm = TRUE),
    mean_strictness_diff = mean(abs(strictness_own - strictness_neighbor), na.rm = TRUE),
    n_unique_neighbors = n_distinct(alderman_neighbor),
    .groups = "drop"
  ) %>%
  arrange(desc(n_parcels))

message("Top 20 aldermen by number of boundary parcels:\n")
print(head(alderman_stats, 20), n = 20)

# =============================================================================
# 4. WHICH PAIRS WOULD CHANGE RESULTS IF SCORES CHANGED?
# =============================================================================
message("\n=== SENSITIVITY ANALYSIS: HIGH LEVERAGE PAIRS ===\n")

# High leverage = large contribution AND alderman with uncertain estimates
high_leverage <- alderman_pair_summary %>%
  filter(contribution_score > quantile(contribution_score, 0.8, na.rm = TRUE)) %>%
  mutate(
    leverage_reason = case_when(
      n_parcels > 50 & strictness_diff > 0.5 ~ "High volume + large diff",
      n_parcels > 100 ~ "Very high volume",
      strictness_diff > 1.0 ~ "Very large strictness gap",
      TRUE ~ "Moderate leverage"
    )
  )

message(sprintf("High-leverage pairs (top 20%%): %d", nrow(high_leverage)))
message("\nThese pairs would most affect results if alderman scores changed:\n")
print(high_leverage %>% 
  select(alderman_own, alderman_neighbor, n_parcels, strictness_diff, contribution_score, leverage_reason) %>%
  head(15), n = 15)

# =============================================================================
# 5. SAVE OUTPUTS
# =============================================================================
message("\n=== SAVING OUTPUTS ===\n")

write_csv(alderman_pair_summary, "../output/diagnostic_alderman_pairs.csv")
write_csv(ward_zone_summary, "../output/diagnostic_ward_zone_groups.csv")
write_csv(alderman_stats, "../output/diagnostic_alderman_stats.csv")
write_csv(high_leverage, "../output/diagnostic_high_leverage_pairs.csv")

message("Saved:")
message("  - ../output/diagnostic_alderman_pairs.csv")
message("  - ../output/diagnostic_ward_zone_groups.csv")
message("  - ../output/diagnostic_alderman_stats.csv")
message("  - ../output/diagnostic_high_leverage_pairs.csv")

message("\n=== DIAGNOSTIC COMPLETE ===")
