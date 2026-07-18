# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")

scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE)
specifications <- read_csv("../input/score_specifications.csv", show_col_types = FALSE)
models <- read_csv("../input/density_score_sensitivity_models.csv", show_col_types = FALSE)
assignments <- read_csv("../input/density_score_side_assignments.csv", show_col_types = FALSE)

baseline <- scores %>%
  filter(spec_id == "baseline_log_all_workload") %>%
  select(cutoff, alderman, baseline_score = score, baseline_rank = strictness_rank)

score_comparisons <- scores %>%
  left_join(baseline, by = c("cutoff", "alderman"), relationship = "many-to-one") %>%
  group_by(cutoff, spec_id) %>%
  summarise(
    aldermen = sum(complete.cases(score, baseline_score)),
    pearson = cor(score, baseline_score, use = "complete.obs"),
    spearman = cor(score, baseline_score, method = "spearman", use = "complete.obs"),
    mean_absolute_score_change = mean(abs(score - baseline_score), na.rm = TRUE),
    maximum_rank_change = max(abs(strictness_rank - baseline_rank), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    specifications %>% select(spec_id, family, label, spec_order),
    by = "spec_id",
    relationship = "many-to-one"
  ) %>%
  arrange(cutoff, spec_order)

baseline_models <- models %>%
  filter(spec_id == "baseline_log_all_workload") %>%
  select(
    construction_sample, outcome, treatment,
    baseline_estimate = estimate,
    baseline_se = se,
    baseline_p_value = p_value
  )

model_comparisons <- models %>%
  left_join(
    baseline_models,
    by = c("construction_sample", "outcome", "treatment"),
    relationship = "many-to-one"
  ) %>%
  left_join(assignments, by = "spec_id", relationship = "many-to-one") %>%
  mutate(
    estimate_change = estimate - baseline_estimate,
    significant_5pct = p_value < 0.05,
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  ) %>%
  arrange(construction_sample, outcome, treatment, match(spec_id, specifications$spec_id))

score_rank_changes <- scores %>%
  left_join(baseline, by = c("cutoff", "alderman"), relationship = "many-to-one") %>%
  mutate(
    score_change = score - baseline_score,
    rank_change = strictness_rank - baseline_rank,
    absolute_rank_change = abs(rank_change)
  ) %>%
  left_join(
    specifications %>% select(spec_id, family, label, spec_order),
    by = "spec_id",
    relationship = "many-to-one"
  ) %>%
  arrange(cutoff, spec_order, desc(absolute_rank_change), alderman)

summary_table <- model_comparisons %>%
  group_by(construction_sample, outcome, treatment) %>%
  summarise(
    specifications = n(),
    minimum_estimate = min(estimate),
    median_estimate = median(estimate),
    maximum_estimate = max(estimate),
    negative_share = mean(estimate < 0),
    significant_negative_share = mean(estimate < 0 & p_value < 0.05),
    significant_positive_share = mean(estimate > 0 & p_value < 0.05),
    .groups = "drop"
  )

plot_data <- model_comparisons %>%
  mutate(
    outcome_label = recode(
      outcome,
      density_far = "Log FAR",
      density_dupac = "Log DUPAC",
      unitscount = "Log units"
    ),
    treatment_label = recode(
      treatment,
      continuous = "Continuous score",
      binary = "More-stringent side"
    ),
    label = factor(label, levels = rev(specifications$label))
  )

for (sample_i in c("all", "multifamily")) {
  sample_plot <- plot_data %>%
    filter(construction_sample == sample_i) %>%
    ggplot(aes(x = estimate, y = label)) +
    geom_vline(xintercept = 0, color = "gray55", linewidth = 0.35) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, linewidth = 0.35) +
    geom_point(aes(fill = significant_5pct), shape = 21, size = 2.2) +
    facet_grid(outcome_label ~ treatment_label, scales = "free_x") +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black"), guide = "none") +
    labs(
      title = paste0(
        if_else(sample_i == "all", "All construction", "Multifamily construction"),
        ": density sensitivity to score construction"
      ),
      subtitle = "Points are coefficients; bars are 95% confidence intervals clustered by ward pair",
      x = "Estimate",
      y = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )

  ggsave(
    paste0("../output/density_score_sensitivity_", sample_i, ".png"),
    sample_plot,
    width = 13,
    height = 14,
    dpi = 220,
    bg = "white"
  )
}

correlation_plot <- score_comparisons %>%
  mutate(
    label = factor(label, levels = rev(specifications$label)),
    cutoff = factor(cutoff)
  ) %>%
  ggplot(aes(x = spearman, y = label, shape = cutoff)) +
  geom_vline(xintercept = c(0.8, 0.9), color = "gray75", linetype = "dashed") +
  geom_point(size = 2.2) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(
    title = "Rank correlation with the new all-permit-workload baseline",
    x = "Spearman correlation",
    y = NULL,
    shape = "Score cutoff"
  ) +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave(
  "../output/score_sensitivity_correlations.png",
  correlation_plot,
  width = 10,
  height = 12,
  dpi = 220,
  bg = "white"
)

write_csv(score_comparisons, "../output/score_comparisons.csv")
write_csv(score_rank_changes, "../output/score_rank_changes.csv")
write_csv(model_comparisons, "../output/density_score_sensitivity_comparisons.csv")
write_csv(summary_table, "../output/density_score_sensitivity_summary.csv")
