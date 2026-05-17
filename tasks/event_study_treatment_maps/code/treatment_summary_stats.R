# ==============================================================================
# TREATMENT SUMMARY STATISTICS
# Calculates key statistics for presentation:
# - Blocks by treatment direction (stricter/lenient/control) for 2015 and 2023
# - Average strictness score change by cohort
# - Average control blocks per treated block
# ==============================================================================

source("../../setup_environment/code/packages.R")

message("\n=== Treatment Summary Statistics ===\n")

# Load block treatment panel
treatment_panel <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE)

# ==============================================================================
# 1. BLOCKS BY TREATMENT DIRECTION (BY COHORT)
# ==============================================================================
message("=== Block Counts by Treatment Direction ===\n")

block_summary <- treatment_panel %>%
  filter(valid == TRUE) %>%
  mutate(
    treatment_group = case_when(
      switched & strictness_change > 0 ~ "To Stricter",
      switched & strictness_change < 0 ~ "To Lenient",
      !switched ~ "Control",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(cohort, treatment_group) %>%
  summarise(n_blocks = n(), .groups = "drop") %>%
  pivot_wider(names_from = treatment_group, values_from = n_blocks, values_fill = 0)

cat("\nBlock counts by cohort and treatment direction:\n")
print(block_summary)

# ==============================================================================
# 2. AVERAGE STRICTNESS CHANGE BY COHORT (AMONG TREATED BLOCKS)
# ==============================================================================
message("\n=== Strictness Change Statistics ===\n")

strictness_stats <- treatment_panel %>%
  filter(valid == TRUE, switched == TRUE) %>%
  group_by(cohort) %>%
  summarise(
    n_treated = n(),
    mean_strictness_change = mean(strictness_change, na.rm = TRUE),
    median_strictness_change = median(strictness_change, na.rm = TRUE),
    sd_strictness_change = sd(strictness_change, na.rm = TRUE),
    mean_abs_change = mean(abs(strictness_change), na.rm = TRUE),
    min_change = min(strictness_change, na.rm = TRUE),
    max_change = max(strictness_change, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nStrictness change statistics for treated blocks:\n")
print(strictness_stats)

# By direction
strictness_by_direction <- treatment_panel %>%
  filter(valid == TRUE, switched == TRUE) %>%
  mutate(direction = if_else(strictness_change > 0, "To Stricter", "To Lenient")) %>%
  group_by(cohort, direction) %>%
  summarise(
    n_blocks = n(),
    mean_strictness_change = mean(strictness_change, na.rm = TRUE),
    median_strictness_change = median(strictness_change, na.rm = TRUE),
    mean_abs_change = mean(abs(strictness_change), na.rm = TRUE),
    .groups = "drop"
  )

cat("\nStrictness change by direction:\n")
print(strictness_by_direction)

# ==============================================================================
# 3. CONTROL BLOCKS PER TREATED BLOCK
# ==============================================================================
message("\n=== Control to Treated Ratio ===\n")

control_ratio <- treatment_panel %>%
  filter(valid == TRUE) %>%
  mutate(is_treated = switched) %>%
  group_by(cohort) %>%
  summarise(
    n_treated = sum(is_treated),
    n_control = sum(!is_treated),
    controls_per_treated = n_control / n_treated,
    .groups = "drop"
  )

cat("\nControl blocks per treated block:\n")
print(control_ratio)

# ==============================================================================
# 4. PRINT PRESENTATION-READY SUMMARY
# ==============================================================================
message("\n")
cat("================================================================================\n")
cat("              PRESENTATION SUMMARY\n")
cat("================================================================================\n\n")

for (yr in c("2015", "2023")) {
  cat(sprintf("--- %s COHORT ---\n", yr))
  
  # Block counts
  yr_blocks <- block_summary %>% filter(cohort == yr)
  stricter <- yr_blocks$`To Stricter`
  lenient <- yr_blocks$`To Lenient`
  control <- yr_blocks$Control
  total_treated <- stricter + lenient
  
  cat(sprintf("  Blocks switching to STRICTER alderman: %d\n", stricter))
  cat(sprintf("  Blocks switching to LENIENT alderman:  %d\n", lenient))
  cat(sprintf("  Total TREATED blocks:                  %d\n", total_treated))
  cat(sprintf("  CONTROL blocks (no switch):            %d\n", control))
  
  # Ratio
  ratio <- control / total_treated
  cat(sprintf("  Control blocks per treated block:      %.2f\n", ratio))
  
  # Strictness changes
  yr_strictness <- strictness_stats %>% filter(cohort == yr)
  cat(sprintf("\n  Mean strictness change (all treated):  %.3f\n", yr_strictness$mean_strictness_change))
  cat(sprintf("  Mean |strictness change| (magnitude):  %.3f\n", yr_strictness$mean_abs_change))
  
  # By direction
  yr_dir <- strictness_by_direction %>% filter(cohort == yr)
  stricter_change <- yr_dir %>% filter(direction == "To Stricter") %>% pull(mean_strictness_change)
  lenient_change <- yr_dir %>% filter(direction == "To Lenient") %>% pull(mean_strictness_change)
  
  cat(sprintf("  Mean change (to stricter):             +%.3f\n", stricter_change))
  cat(sprintf("  Mean change (to lenient):              %.3f\n", lenient_change))
  cat("\n")
}

cat("================================================================================\n")

# ==============================================================================
# 5. SAVE OUTPUT
# ==============================================================================

# Combine into a single summary table
summary_table <- bind_rows(
  block_summary %>% 
    pivot_longer(cols = -cohort, names_to = "metric", values_to = "value") %>%
    mutate(metric = paste0("n_blocks_", metric)),
  strictness_stats %>%
    pivot_longer(cols = -cohort, names_to = "metric", values_to = "value"),
  control_ratio %>%
    pivot_longer(cols = -cohort, names_to = "metric", values_to = "value")
)

write_csv(summary_table, "../output/treatment_summary_stats.csv")
message("\nSaved: ../output/treatment_summary_stats.csv")

# Also save the presentation-ready version
sink("../output/treatment_summary_presentation.txt")
cat("================================================================================\n")
cat("              PRESENTATION SUMMARY\n")
cat("================================================================================\n\n")

for (yr in c("2015", "2023")) {
  cat(sprintf("--- %s COHORT ---\n", yr))
  
  yr_blocks <- block_summary %>% filter(cohort == yr)
  stricter <- yr_blocks$`To Stricter`
  lenient <- yr_blocks$`To Lenient`
  control <- yr_blocks$Control
  total_treated <- stricter + lenient
  
  cat(sprintf("  Blocks switching to STRICTER alderman: %d\n", stricter))
  cat(sprintf("  Blocks switching to LENIENT alderman:  %d\n", lenient))
  cat(sprintf("  Total TREATED blocks:                  %d\n", total_treated))
  cat(sprintf("  CONTROL blocks (no switch):            %d\n", control))
  
  ratio <- control / total_treated
  cat(sprintf("  Control blocks per treated block:      %.2f\n", ratio))
  
  yr_strictness <- strictness_stats %>% filter(cohort == yr)
  cat(sprintf("\n  Mean strictness change (all treated):  %.3f\n", yr_strictness$mean_strictness_change))
  cat(sprintf("  Mean |strictness change| (magnitude):  %.3f\n", yr_strictness$mean_abs_change))
  
  yr_dir <- strictness_by_direction %>% filter(cohort == yr)
  stricter_change <- yr_dir %>% filter(direction == "To Stricter") %>% pull(mean_strictness_change)
  lenient_change <- yr_dir %>% filter(direction == "To Lenient") %>% pull(mean_strictness_change)
  
  cat(sprintf("  Mean change (to stricter):             +%.3f\n", stricter_change))
  cat(sprintf("  Mean change (to lenient):              %.3f\n", lenient_change))
  cat("\n")
}

cat("================================================================================\n")
sink()

message("Saved: ../output/treatment_summary_presentation.txt")
message("\n=== Done! ===")
