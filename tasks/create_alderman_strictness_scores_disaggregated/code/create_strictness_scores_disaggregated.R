################################################################################
# create_strictness_scores_disaggregated.R
#
# Creates alderman strictness scores using PERMIT-LEVEL (disaggregated) data
# instead of the ward-month aggregated data. This is methodologically cleaner
# because it:
#   1. Uses natural weighting (each permit counts equally)
#   2. Absorbs permit-level variation via month, community area, and permit type FEs
#   3. Avoids issues with variable ward-month sample sizes
#
# Outputs:
#   - alderman_strictness_scores_disaggregated.csv
#   - strictness_index_plot.pdf
#   - stage1_regression_table.tex
################################################################################

# Load packages
source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD PERMIT-LEVEL DATA
# =============================================================================
message("Loading permit-level data...")
permits <- read_csv("../input/alderman_restrictiveness_scores_data.csv",
  show_col_types = FALSE
)

message(paste("Loaded", nrow(permits), "permits"))

# =============================================================================
# 2. CREATE NECESSARY VARIABLES
# =============================================================================
message("Creating variables...")

# Ensure log_processing_time exists (it already does in the data)
permits <- permits %>%
  mutate(
    # Create ca_id from community_area (already in data)
    ca_id = as.factor(community_area),

    # Use permit_type directly as factor
    # Data already filtered to high-discretion permits upstream:
    # - PERMIT - NEW CONSTRUCTION
    # - PERMIT - RENOVATION/ALTERATION
    # - PERMIT - WRECKING/DEMOLITION
    # - PERMIT - PORCH CONSTRUCTION
    # - PERMIT - REINSTATE REVOKED PMT
    permit_type_clean = as.factor(permit_type),

    # Month as factor for FE
    month_factor = as.factor(month)
  )

# Show distribution of permit types
message("Permit type distribution:")
print(table(permits$permit_type_clean))

# Filter out rows with missing key variables
permits <- permits %>%
  filter(
    !is.na(log_processing_time),
    !is.na(alderman),
    !is.na(month),
    !is.na(ca_id),
    !is.na(permit_type_clean),
    is.finite(log_processing_time)
  )

message(paste("After filtering:", nrow(permits), "permits"))

# =============================================================================
# 3. FILTER TO ALDERMEN WITH SUFFICIENT OBSERVATIONS
# =============================================================================
MIN_PERMITS <- 50 # Minimum permits per alderman

alderman_counts <- permits %>%
  group_by(alderman) %>%
  summarise(n_permits = n(), .groups = "drop")

keep_aldermen <- alderman_counts %>%
  filter(n_permits >= MIN_PERMITS) %>%
  pull(alderman)

message(paste("Keeping", length(keep_aldermen), "aldermen with >=", MIN_PERMITS, "permits"))

permits <- permits %>%
  filter(alderman %in% keep_aldermen)

message(paste("Final sample:", nrow(permits), "permits"))

# Display alderman count distribution
message("\nAlderman permit counts:")
print(summary(alderman_counts$n_permits))

# =============================================================================
# 4. SET REFERENCE ALDERMAN
# =============================================================================
# Use the median alderman as reference (based on permit count)
ref_alderman <- alderman_counts %>%
  filter(alderman %in% keep_aldermen) %>%
  arrange(n_permits) %>%
  slice(ceiling(n() / 2)) %>%
  pull(alderman)

message(paste("Reference alderman:", ref_alderman))

# =============================================================================
# 5. ONE-STEP REGRESSION: ALDERMAN + DEMOGRAPHICS | FEs
# =============================================================================
message("\n--- Running one-step regression with alderman + demographics ---")

# Scale demographic variables for interpretable coefficients
permits <- permits %>%
  mutate(
    pop_10k = pop_total / 10000,
    income_10k = median_hh_income / 10000
  )

# Demographic controls
demo_vars <- c("homeownership_rate", "pop_10k", "income_10k", "share_black", "share_hisp")

# One-step model: alderman + demographics | month + ca_id
# This is the theoretically correct specification (FWL-consistent)
message("Formula: log_processing_time ~ alderman + demographics | month + ca_id")

model_main <- feols(
  log_processing_time ~ i(alderman, ref = ref_alderman) + 
    homeownership_rate + pop_10k + income_10k + share_black + share_hisp | 
    month_factor + ca_id,
  data = permits,
  cluster = ~alderman
)

message("Model summary:")
print(summary(model_main))

# =============================================================================
# 6. EXTRACT ALDERMAN FIXED EFFECTS
# =============================================================================
message("\n--- Extracting alderman fixed effects ---")

coefs <- enframe(coef(model_main), name = "term", value = "alderman_fe")
ses <- enframe(se(model_main), name = "term", value = "alderman_se")

alderman_effects <- coefs %>%
  filter(str_detect(term, "alderman::")) %>%
  mutate(alderman = str_remove(term, "alderman::")) %>%
  left_join(
    ses %>%
      filter(str_detect(term, "alderman::")) %>%
      mutate(alderman = str_remove(term, "alderman::")) %>%
      select(alderman, alderman_se),
    by = "alderman"
  ) %>%
  select(alderman, alderman_fe, alderman_se)

# Add reference alderman with 0
ref_row <- tibble(
  alderman = ref_alderman,
  alderman_fe = 0,
  alderman_se = 0
)
alderman_effects <- bind_rows(alderman_effects, ref_row)

message(paste("Extracted effects for", nrow(alderman_effects), "aldermen"))

# =============================================================================
# 7. APPLY EMPIRICAL BAYES SHRINKAGE
# =============================================================================
message("\n--- Applying Empirical Bayes shrinkage ---")

# Estimate tau^2 (true variance) = observed variance - mean sampling variance
observed_var <- var(alderman_effects$alderman_fe, na.rm = TRUE)
mean_sampling_var <- mean(alderman_effects$alderman_se^2, na.rm = TRUE)
tau2 <- pmax(0, observed_var - mean_sampling_var)

message(paste("Observed variance:", round(observed_var, 4)))
message(paste("Mean sampling variance:", round(mean_sampling_var, 4)))
message(paste("Estimated tau^2:", round(tau2, 4)))

# If tau^2 is 0 (sampling variance >= observed variance), skip shrinkage
# This can happen when standard errors are large relative to point estimates
if (tau2 < 1e-6) {
  message("NOTE: tau^2 ≈ 0, skipping shrinkage (using raw FEs)")
  alderman_effects <- alderman_effects %>%
    mutate(
      tau2 = 0,
      shrinkage_factor = 1,  # No shrinkage
      alderman_fe_shrunk = alderman_fe  # Use raw estimates
    )
} else {
  alderman_effects <- alderman_effects %>%
    mutate(
      tau2 = tau2,
      shrinkage_factor = tau2 / (tau2 + alderman_se^2),
      alderman_fe_shrunk = alderman_fe * shrinkage_factor
    )
}

message(paste("Mean shrinkage factor:", round(mean(alderman_effects$shrinkage_factor, na.rm = TRUE), 3)))

# =============================================================================
# 8. STANDARDIZE TO MEAN 0, SD 1
# =============================================================================
message("\n--- Standardizing scores ---")

alderman_effects <- alderman_effects %>%
  mutate(
    strictness_index = (alderman_fe_shrunk - mean(alderman_fe_shrunk, na.rm = TRUE)) /
      sd(alderman_fe_shrunk, na.rm = TRUE)
  )

message("Strictness index summary:")
print(summary(alderman_effects$strictness_index))

# =============================================================================
# 9. SAVE FINAL SCORES
# =============================================================================
message("\n--- Saving outputs ---")

final_scores <- alderman_effects %>%
  select(alderman, strictness_index, alderman_fe, alderman_fe_shrunk, alderman_se, shrinkage_factor) %>%
  arrange(desc(strictness_index))

write_csv(final_scores, "../output/alderman_strictness_scores_disaggregated.csv")
message("Saved: ../output/alderman_strictness_scores_disaggregated.csv")

# =============================================================================
# 10. CREATE STRICTNESS INDEX PLOT
# =============================================================================
message("\n--- Creating strictness index plot ---")

plot_data <- final_scores %>%
  arrange(strictness_index) %>%
  mutate(
    alderman = factor(alderman, levels = alderman),
    fill_color = strictness_index
  )

p_strictness <- ggplot(plot_data, aes(x = strictness_index, y = alderman, fill = strictness_index)) +
  geom_col() +
  scale_fill_distiller(
    palette = "RdYlBu",
    direction = -1,
    name = "Strictness Index"
  ) +
  labs(
    title = "Alderman Strictness Index (Disaggregated)",
    subtitle = "Based on permit-level regression with month, community area, and permit type FEs",
    x = "Strictness Index (SD units)",
    y = NULL
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray50")
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7)

ggsave("../output/strictness_index_plot.pdf",
  plot = p_strictness,
  width = 8, height = 12, device = "pdf", bg = "white"
)
message("Saved: ../output/strictness_index_plot.pdf")

# Also save PNG version
ggsave("../output/strictness_index_plot.png",
  plot = p_strictness,
  width = 8, height = 12, dpi = 300, bg = "white"
)
message("Saved: ../output/strictness_index_plot.png")

# =============================================================================
# 11. CREATE REGRESSION TABLE FOR APPENDIX
# =============================================================================
message("\n--- Creating regression table ---")

# One-step model table showing demographic controls (dropping alderman FEs for display)
etable(
  model_main,
  digits = 3, se.below = TRUE,
  depvar = FALSE,
  headers = c("Log Processing Time"),
  drop = "alderman",
  dict = c(
    homeownership_rate = "Homeownership rate",
    pop_10k = "Population (10,000s)",
    income_10k = "Median HH income (\\$10,000s)",
    share_black = "Share Black",
    share_hisp = "Share Hispanic"
  ),
  fixef.group = list(
    "Month FE" = "month_factor",
    "Community Area FE" = "ca_id"
  ),
  fitstat = ~ n + r2,
  file = "../output/stage1_regression_table.tex",
  replace = TRUE,
  style.tex = style.tex(
    main = "aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  )
)
message("Saved: ../output/stage1_regression_table.tex")

# =============================================================================
# 12. CREATE WARD MAPS (2014 and 2025)
# =============================================================================
message("\n--- Creating ward maps ---")

# Helper function to create a map for a given date
create_ward_map <- function(date_str, scores_df) {
  month_dt <- as.Date(paste0(date_str, "-01"))
  use_year <- as.integer(format(month_dt, "%Y"))

  # Load ward shapes for the chosen year
  wards <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
    filter(year == use_year)

  # Prepare scores with lowercase matching
  scores_clean <- scores_df %>%
    transmute(
      alderman = str_squish(str_to_lower(alderman)),
      score = strictness_index
    )

  # Alderman → Ward mapping for the chosen year
  panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
    filter(month == as.yearmon(month_dt)) %>%
    transmute(ward, alderman = str_squish(str_to_lower(alderman)))

  # Join: scores → panel (get ward) → polygons
  ward_scores <- panel %>%
    left_join(scores_clean, by = "alderman") %>%
    distinct(ward, .keep_all = TRUE)

  ward_map <- wards %>%
    left_join(ward_scores, by = "ward")

  # Create the map
  p <- ggplot(ward_map) +
    geom_sf(aes(fill = score), color = "grey20", linewidth = 0.2) +
    scale_fill_distiller(
      palette = "RdYlBu",
      direction = -1,
      name = "Strictness index",
      na.value = "grey90"
    ) +
    labs(
      title = paste0("Alderman Strictness Index by Ward (", as.yearmon(date_str), ")"),
      subtitle = "Disaggregated permit-level estimates"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray50")
    )

  return(p)
}

# Create maps for 2014 and 2025
p_map_2014 <- create_ward_map("2014-01", final_scores)
ggsave("../output/strictness_score_map_2014-01.pdf", plot = p_map_2014, width = 8, height = 10, dpi = 300)
message("Saved: ../output/strictness_score_map_2014-01.pdf")

p_map_2025 <- create_ward_map("2025-01", final_scores)
ggsave("../output/strictness_score_map_2025-01.pdf", plot = p_map_2025, width = 8, height = 10, dpi = 300)
message("Saved: ../output/strictness_score_map_2025-01.pdf")

# =============================================================================
# 13. CORRELATION WITH OLD AGGREGATED SCORES
# =============================================================================
message("\n--- Comparing with old aggregated scores ---")

# Load old aggregated scores
old_scores <- read_csv("../input/alderman_restrictiveness_scores_month_FEs.csv", show_col_types = FALSE) %>%
  transmute(
    alderman = str_squish(str_to_lower(alderman)),
    old_strictness_index = strictness_index
  )

# Prepare new scores
new_scores <- final_scores %>%
  transmute(
    alderman = str_squish(str_to_lower(alderman)),
    new_strictness_index = strictness_index
  )

# Merge and compute correlation
merged_scores <- new_scores %>%
  inner_join(old_scores, by = "alderman")

n_matched <- nrow(merged_scores)
correlation <- cor(merged_scores$new_strictness_index, merged_scores$old_strictness_index, use = "complete.obs")

message(paste("N aldermen matched:", n_matched))
message(paste("Correlation between old and new scores:", round(correlation, 3)))

# Create scatter plot
p_correlation <- ggplot(merged_scores, aes(x = old_strictness_index, y = new_strictness_index)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = "Comparison: Disaggregated vs Aggregated Strictness Scores",
    subtitle = paste0("Correlation = ", round(correlation, 3), " (N = ", n_matched, " aldermen)"),
    x = "Old Scores (Ward-Month Aggregated)",
    y = "New Scores (Permit-Level)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray50")
  ) +
  coord_equal()

ggsave("../output/correlation_comparison.png", plot = p_correlation, width = 8, height = 8, dpi = 300)
message("Saved: ../output/correlation_comparison.png")

# =============================================================================
# 14. SUMMARY STATISTICS
# =============================================================================
message("\n======== SUMMARY ========")
message(paste("N permits:", nrow(permits)))
message(paste("N aldermen:", nrow(final_scores)))
message(paste("R-squared (within):", round(r2(model_main, "wr2"), 3)))
message(paste("Reference alderman:", ref_alderman))

message("\nTop 5 strictest aldermen:")
print(head(final_scores %>% select(alderman, strictness_index), 5))

message("\nTop 5 most lenient aldermen:")
print(tail(final_scores %>% select(alderman, strictness_index), 5))

message(paste("\nCorrelation with old aggregated scores:", round(correlation, 3)))

message("\n✓ Disaggregated strictness scores complete!")

