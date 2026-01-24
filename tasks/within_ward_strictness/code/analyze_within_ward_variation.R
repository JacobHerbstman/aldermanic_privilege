source("../../setup_environment/code/packages.R")
library(ggrepel)

# =============================================================================
# WITHIN-WARD STRICTNESS SCORE VALIDATION
# Validates whether strictness scores capture alderman-specific behavior
# versus ward characteristics
# =============================================================================

message("\n=== Within-Ward Strictness Score Validation ===")

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================
message("\n=== Loading Data ===")

# Load strictness scores
scores <- read_csv("../input/alderman_restrictiveness_scores_month_FEs.csv", show_col_types = FALSE)
message(sprintf("Loaded %d aldermen with strictness scores", nrow(scores)))

# Load alderman-ward panel
panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
    mutate(month = as.yearmon(month))

# Filter to post-2006 (when permit data starts)
panel_post2006 <- panel %>%
    filter(month >= as.yearmon("2006-01"))

message(sprintf("Panel filtered to post-2006: %d ward-month observations", nrow(panel_post2006)))

# Determine primary ward for each alderman (ward with most months served)
alderman_primary_ward <- panel_post2006 %>%
    count(alderman, ward, name = "months_served") %>%
    group_by(alderman) %>%
    slice_max(months_served, n = 1, with_ties = FALSE) %>%
    ungroup()

message(sprintf("Identified primary ward for %d aldermen", nrow(alderman_primary_ward)))

# Merge with scores (inner join - only keep aldermen with scores)
alderman_ward_scores <- alderman_primary_ward %>%
    inner_join(scores, by = "alderman")

message(sprintf("Matched %d aldermen with both ward assignments and strictness scores", 
                nrow(alderman_ward_scores)))

# Count aldermen per ward
ward_counts <- alderman_ward_scores %>%
    count(ward, name = "n_aldermen")

wards_with_multiple <- ward_counts %>%
    filter(n_aldermen >= 2) %>%
    pull(ward)

# Create analysis flags
alderman_ward_scores <- alderman_ward_scores %>%
    left_join(ward_counts, by = "ward") %>%
    mutate(in_analysis_sample = ward %in% wards_with_multiple)

analysis_sample <- alderman_ward_scores %>%
    filter(in_analysis_sample)

message(sprintf("Analysis sample: %d aldermen in %d wards with 2+ aldermen", 
                nrow(analysis_sample), length(wards_with_multiple)))

# =============================================================================
# 2. DESCRIPTIVE STATISTICS
# =============================================================================
message("\n=== Descriptive Statistics ===")

# Coverage stats
total_scored_aldermen <- nrow(scores)
aldermen_with_wards <- nrow(alderman_ward_scores)
unique_wards <- n_distinct(alderman_ward_scores$ward)

# Within-ward variation
wards_with_1 <- sum(ward_counts$n_aldermen == 1)
wards_with_2 <- sum(ward_counts$n_aldermen == 2)
wards_with_3plus <- sum(ward_counts$n_aldermen >= 3)
wards_in_sample <- length(wards_with_multiple)
aldermen_in_sample <- nrow(analysis_sample)

# Score distribution
overall_mean <- mean(alderman_ward_scores$strictness_index)
overall_sd <- sd(alderman_ward_scores$strictness_index)

# Mean within-ward SD (for wards with 2+ aldermen)
within_ward_sds <- analysis_sample %>%
    group_by(ward) %>%
    summarise(within_sd = sd(strictness_index), .groups = "drop")
mean_within_ward_sd <- mean(within_ward_sds$within_sd)
ratio_within_to_overall <- mean_within_ward_sd / overall_sd

# Print summary
cat("\n=== COVERAGE ===\n")
cat(sprintf("Total aldermen with strictness scores: %d\n", total_scored_aldermen))
cat(sprintf("Aldermen with post-2006 ward assignments: %d\n", aldermen_with_wards))
cat(sprintf("Unique wards represented: %d\n", unique_wards))

cat("\n=== WITHIN-WARD VARIATION ===\n")
cat(sprintf("Wards with exactly 1 alderman (scored): %d\n", wards_with_1))
cat(sprintf("Wards with exactly 2 aldermen (scored): %d\n", wards_with_2))
cat(sprintf("Wards with 3+ aldermen (scored): %d\n", wards_with_3plus))
cat(sprintf("Total wards with 2+ aldermen (analysis sample): %d\n", wards_in_sample))
cat(sprintf("Total aldermen in analysis sample: %d\n", aldermen_in_sample))

cat("\n=== SCORE DISTRIBUTION ===\n")
cat(sprintf("Overall mean strictness score: %.3f\n", overall_mean))
cat(sprintf("Overall SD of strictness scores: %.3f\n", overall_sd))
cat(sprintf("Mean within-ward SD (for wards with 2+ aldermen): %.3f\n", mean_within_ward_sd))
cat(sprintf("Ratio: within-ward SD / overall SD: %.3f\n", ratio_within_to_overall))

# Save summary
summary_df <- data.frame(
    metric = c("total_scored_aldermen", "aldermen_with_wards", "unique_wards",
               "wards_with_1_alderman", "wards_with_2_aldermen", "wards_with_3plus_aldermen",
               "wards_in_analysis_sample", "aldermen_in_analysis_sample",
               "overall_mean", "overall_sd", "mean_within_ward_sd", "ratio_within_to_overall"),
    value = c(total_scored_aldermen, aldermen_with_wards, unique_wards,
              wards_with_1, wards_with_2, wards_with_3plus,
              wards_in_sample, aldermen_in_sample,
              overall_mean, overall_sd, mean_within_ward_sd, ratio_within_to_overall)
)
write_csv(summary_df, "../output/ward_alderman_summary.csv")
message("\nSaved: ../output/ward_alderman_summary.csv")

# =============================================================================
# 3. WITHIN-WARD DOT PLOT
# =============================================================================
message("\n=== Creating Within-Ward Dot Plot ===")

# Calculate ward-level stats
ward_stats <- alderman_ward_scores %>%
    group_by(ward) %>%
    summarise(
        ward_mean = mean(strictness_index),
        ward_min = min(strictness_index),
        ward_max = max(strictness_index),
        ward_n = n(),
        .groups = "drop"
    )

# Merge back
plot_data <- alderman_ward_scores %>%
    left_join(ward_stats, by = "ward")

# Find notable examples for annotation
notable_wards <- ward_stats %>%
    filter(ward_n >= 2) %>%
    mutate(range = ward_max - ward_min) %>%
    slice_max(range, n = 3)

notable_aldermen <- alderman_ward_scores %>%
    filter(ward %in% notable_wards$ward)

# Create plot
p_dotplot <- ggplot(plot_data, aes(x = strictness_index, y = reorder(factor(ward), ward_mean))) +
    # Horizontal segments showing within-ward range (for wards with 2+ aldermen)
    geom_segment(
        data = plot_data %>% filter(n_aldermen >= 2) %>% distinct(ward, .keep_all = TRUE),
        aes(x = ward_min, xend = ward_max, yend = reorder(factor(ward), ward_mean)),
        color = "gray70", linewidth = 0.5
    ) +
    # Points for each alderman
    geom_point(aes(color = n_aldermen >= 2), size = 2.5, alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_color_manual(
        values = c("TRUE" = "#E41A1C", "FALSE" = "#999999"),
        labels = c("TRUE" = "2+ aldermen", "FALSE" = "1 alderman"),
        name = "Ward has:"
    ) +
    labs(
        x = "Strictness Score (higher = stricter)",
        y = "Ward (ordered by mean strictness)",
        title = "Alderman Strictness Scores by Ward",
        subtitle = "Each dot is one alderman; horizontal segments show within-ward range"
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        axis.text.y = element_text(size = 7),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40")
    )

ggsave("../output/within_ward_dotplot.pdf", p_dotplot, width = 8, height = 11)
message("Saved: ../output/within_ward_dotplot.pdf")

# =============================================================================
# 4. PREDECESSOR-SUCCESSOR ANALYSIS
# =============================================================================
message("\n=== Predecessor-Successor Analysis ===")

# Determine alderman sequence within each ward
# Get first month each alderman served in each ward
alderman_sequence <- panel_post2006 %>%
    group_by(ward, alderman) %>%
    summarise(first_month = min(month), .groups = "drop") %>%
    arrange(ward, first_month)

# For each ward, create predecessor-successor pairs
turnover_pairs <- alderman_sequence %>%
    group_by(ward) %>%
    arrange(first_month) %>%
    mutate(
        successor = lead(alderman),
        predecessor = alderman
    ) %>%
    filter(!is.na(successor)) %>%
    select(ward, predecessor, successor) %>%
    ungroup()

# Merge with scores
turnover_pairs <- turnover_pairs %>%
    inner_join(scores %>% select(alderman, strictness_index) %>% rename(predecessor_score = strictness_index),
               by = c("predecessor" = "alderman")) %>%
    inner_join(scores %>% select(alderman, strictness_index) %>% rename(successor_score = strictness_index),
               by = c("successor" = "alderman"))

message(sprintf("Created %d predecessor-successor pairs with both scores", nrow(turnover_pairs)))

# Calculate correlation
if (nrow(turnover_pairs) >= 3) {
    pred_succ_cor <- cor(turnover_pairs$predecessor_score, turnover_pairs$successor_score)
    message(sprintf("Predecessor-successor correlation: %.3f", pred_succ_cor))
    
    # Find notable outliers (largest absolute difference)
    turnover_pairs <- turnover_pairs %>%
        mutate(score_diff = abs(successor_score - predecessor_score))
    
    top_outliers <- turnover_pairs %>%
        slice_max(score_diff, n = 3)
    
    # Create scatter plot
    p_scatter <- ggplot(turnover_pairs, aes(x = predecessor_score, y = successor_score)) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
        geom_smooth(method = "lm", se = TRUE, color = "#2171B5", alpha = 0.2) +
        geom_point(size = 3, alpha = 0.7, color = "#E41A1C") +
        labs(
            x = "Predecessor Strictness Score",
            y = "Successor Strictness Score",
            title = "Strictness Score Persistence Across Alderman Turnovers",
            subtitle = "If scores were ward-driven, points would cluster on the 45° line"
        ) +
        coord_fixed() +
        theme_minimal() +
        theme(
            plot.title = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(size = 10, color = "gray40")
        )
    
    ggsave("../output/predecessor_successor_scatter.pdf", p_scatter, width = 6, height = 6)
    message("Saved: ../output/predecessor_successor_scatter.pdf")
} else {
    pred_succ_cor <- NA
    message("Not enough turnover pairs to calculate correlation")
}

# =============================================================================
# 5. VARIANCE DECOMPOSITION
# =============================================================================
message("\n=== Variance Decomposition ===")

# Calculate using analysis sample (wards with 2+ aldermen)
total_var <- var(analysis_sample$strictness_index)

# Between-ward variance
ward_means <- analysis_sample %>%
    group_by(ward) %>%
    summarise(mean_score = mean(strictness_index), n = n(), .groups = "drop")

grand_mean <- mean(analysis_sample$strictness_index)
between_var <- weighted.mean((ward_means$mean_score - grand_mean)^2, ward_means$n)

# Within-ward variance
within_vars <- analysis_sample %>%
    group_by(ward) %>%
    summarise(within_var = var(strictness_index), n = n(), .groups = "drop")
within_var <- weighted.mean(within_vars$within_var, within_vars$n, na.rm = TRUE)

# Share between-ward
share_between <- between_var / total_var

# Create summary
variance_summary <- data.frame(
    metric = c("total_variance", "between_ward_variance", "within_ward_variance",
               "share_between_ward", "predecessor_successor_correlation",
               "n_wards_in_sample", "n_aldermen_in_sample", "n_turnover_events",
               "overall_sd", "mean_within_ward_sd", "ratio_within_to_overall_sd"),
    value = c(total_var, between_var, within_var,
              share_between, ifelse(is.na(pred_succ_cor), NA, pred_succ_cor),
              wards_in_sample, aldermen_in_sample, nrow(turnover_pairs),
              overall_sd, mean_within_ward_sd, ratio_within_to_overall)
)

write_csv(variance_summary, "../output/variance_decomposition.csv")
message("\nSaved: ../output/variance_decomposition.csv")

# Print interpretive summary
cat("\n")
cat("=============================================================\n")
cat("           VARIANCE DECOMPOSITION SUMMARY\n")
cat("=============================================================\n")
cat(sprintf("\nShare of variance explained by ward: %.1f%%\n", share_between * 100))
cat(sprintf("(i.e., %.1f%% is BETWEEN-ward, %.1f%% is WITHIN-ward)\n", 
            share_between * 100, (1 - share_between) * 100))

if (share_between < 0.3) {
    cat("\n✓ INTERPRETATION: Ward identity explains only a small fraction\n")
    cat("  of strictness score variation. The majority of variation is\n")
    cat("  WITHIN wards — different aldermen from the same ward have\n")
    cat("  meaningfully different scores.\n")
} else if (share_between < 0.5) {
    cat("\n• INTERPRETATION: Ward identity explains a moderate fraction\n")
    cat("  of strictness score variation. There is still substantial\n")
    cat("  within-ward variation.\n")
} else {
    cat("\n⚠ INTERPRETATION: Ward identity explains most of strictness\n")
    cat("  score variation. Scores may be proxying for ward characteristics.\n")
}

if (!is.na(pred_succ_cor)) {
    cat(sprintf("\nPredecessor-successor correlation: %.2f\n", pred_succ_cor))
    if (pred_succ_cor < 0.3) {
        cat("✓ When a new alderman takes over a ward, their strictness score\n")
        cat("  is WEAKLY correlated with their predecessor — consistent with\n")
        cat("  scores reflecting alderman-specific rather than ward-driven behavior.\n")
    } else if (pred_succ_cor < 0.6) {
        cat("• When a new alderman takes over a ward, their strictness score\n")
        cat("  is MODERATELY correlated with their predecessor.\n")
    } else {
        cat("⚠ When a new alderman takes over a ward, their strictness score\n")
        cat("  is STRONGLY correlated with their predecessor.\n")
    }
}

cat("\n=============================================================\n")

message("\n=== Done! ===")
