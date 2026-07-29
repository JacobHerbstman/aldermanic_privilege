# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/alderman_uncertainty_helpers.R")

positive_permits <- read_csv(
  "../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv",
  show_col_types = FALSE,
  col_types = cols(id = col_character(), pin = col_character(), .default = col_guess())
) %>%
  mutate(month = as.yearmon(month)) %>%
  filter(month <= as.yearmon("Dec 2022"))

no_volume_config <- default_uncertainty_config()
no_volume_config$volume_ctrl <- "NONE"

no_volume <- build_residualized_uncertainty_index(
  permits = positive_permits,
  config = no_volume_config,
  variant_id = "positive_log_no_volume",
  stage1_outcome = "log_processing_time",
  drop_covariates = "share_bach_plus",
  construction_rule = "Positive-duration log processing time without permit-volume controls"
)$alderman_index %>%
  transmute(
    score_spec = "positive_log_no_volume",
    alderman,
    n_permits,
    score = uncertainty_index,
    alderman_effect_raw = alderman_fe_raw,
    alderman_effect_se = alderman_se,
    shrinkage = shrinkage_B
  )

volume_scores <- read_csv(
  "../output/permit_zero_day_score_aldermen.csv",
  show_col_types = FALSE
) %>%
  filter(
    cutoff == 2022,
    variant %in% c("official", "positive_log_all_permit_volume")
  ) %>%
  transmute(
    score_spec = recode(
      variant,
      official = "positive_log_positive_permit_volume",
      positive_log_all_permit_volume = "positive_log_all_permit_volume"
    ),
    alderman,
    n_permits,
    score,
    alderman_effect_raw,
    alderman_effect_se,
    shrinkage
  )

score_comparison <- bind_rows(volume_scores, no_volume) %>%
  group_by(score_spec) %>%
  mutate(
    strictness_rank = min_rank(desc(score)),
    burnett_reilly_pair = alderman %in% c("Walter Burnett, Jr.", "Brendan Reilly")
  ) %>%
  ungroup() %>%
  arrange(score_spec, strictness_rank, alderman)

write_csv(
  score_comparison,
  "../output/permit_score_volume_control_comparison.csv"
)

score_changes <- score_comparison %>%
  filter(
    score_spec %in% c(
      "positive_log_positive_permit_volume",
      "positive_log_all_permit_volume"
    )
  ) %>%
  select(alderman, score_spec, score, strictness_rank) %>%
  pivot_wider(
    names_from = score_spec,
    values_from = c(score, strictness_rank)
  ) %>%
  transmute(
    alderman,
    current_score = score_positive_log_positive_permit_volume,
    all_permit_workload_score = score_positive_log_all_permit_volume,
    score_change = all_permit_workload_score - current_score,
    absolute_score_change = abs(score_change),
    current_rank = strictness_rank_positive_log_positive_permit_volume,
    all_permit_workload_rank = strictness_rank_positive_log_all_permit_volume,
    rank_change_toward_stringent = current_rank - all_permit_workload_rank
  ) %>%
  arrange(desc(absolute_score_change), alderman)

write_csv(
  score_changes,
  "../output/permit_score_all_workload_changes.csv"
)

score_correlation <- cor(
  score_changes$current_score,
  score_changes$all_permit_workload_score
)
rank_correlation <- cor(
  score_changes$current_score,
  score_changes$all_permit_workload_score,
  method = "spearman"
)
plot_labels <- bind_rows(
  score_changes %>% slice_head(n = 12),
  score_changes %>%
    filter(alderman %in% c("Walter Burnett, Jr.", "Brendan Reilly"))
) %>%
  distinct(alderman, .keep_all = TRUE)

score_plot <- ggplot(
  score_changes,
  aes(current_score, all_permit_workload_score)
) +
  geom_abline(slope = 1, intercept = 0, color = "gray55", linetype = "dashed") +
  geom_point(
    aes(color = alderman %in% c("Walter Burnett, Jr.", "Brendan Reilly")),
    size = 2,
    alpha = 0.8
  ) +
  ggrepel::geom_text_repel(
    data = plot_labels,
    aes(label = alderman),
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.35,
    point.padding = 0.2,
    min.segment.length = 0
  ) +
  scale_color_manual(values = c("FALSE" = "gray35", "TRUE" = "#B22222"), guide = "none") +
  coord_equal() +
  labs(
    title = "Aldermanic stringency scores under alternative workload counts",
    subtitle = sprintf(
      "Positive-duration log processing time; Pearson = %.3f, Spearman = %.3f",
      score_correlation,
      rank_correlation
    ),
    x = "Current score: positive-duration permit workload",
    y = "Corrected score: all-permit workload"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(
  "../output/permit_score_all_workload_comparison.png",
  score_plot,
  width = 8,
  height = 6.5,
  dpi = 220,
  bg = "white"
)
