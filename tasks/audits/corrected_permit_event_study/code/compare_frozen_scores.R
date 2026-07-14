# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")

source("../../../setup_environment/code/packages.R")

scores_2022 <- read_csv("../input/aldermen_uncertainty_scores_2022.csv", show_col_types = FALSE) %>%
  select(alderman, score_2022 = uncertainty_index)
scores_2014 <- read_csv("../input/aldermen_uncertainty_scores_2014.csv", show_col_types = FALSE) %>%
  select(alderman, score_2014 = uncertainty_index)

panel <- read_parquet("../output/corrected_permit_block_year_panel.parquet")
active_2014 <- panel %>%
  filter(year == 2014L) %>%
  select(alderman = alderman_origin_2014) %>%
  bind_rows(panel %>% filter(year == 2014L) %>% select(alderman = alderman_dest_2014)) %>%
  distinct(alderman) %>%
  mutate(active_in_2014_mapping = TRUE)

alder_comparison <- scores_2022 %>%
  inner_join(scores_2014, by = "alderman", relationship = "one-to-one") %>%
  left_join(active_2014, by = "alderman", relationship = "one-to-one") %>%
  mutate(
    active_in_2014_mapping = replace_na(active_in_2014_mapping, FALSE),
    rank_2022 = min_rank(desc(score_2022)),
    rank_2014 = min_rank(desc(score_2014)),
    rank_change_toward_stringent = rank_2022 - rank_2014,
    score_change = score_2014 - score_2022
  ) %>%
  arrange(rank_2014)

write_csv(alder_comparison, "../output/frozen_score_alder_comparison.csv")

block_500 <- panel %>%
  filter(year == 2014L, switched, dist_m <= 152.4) %>%
  distinct(block_id, assigned_change_2022, assigned_change_2014)
block_1000 <- panel %>%
  filter(year == 2014L, switched, dist_m <= 304.8) %>%
  distinct(block_id, assigned_change_2022, assigned_change_2014)

write_csv(
  bind_rows(
    tibble(
      sample = "All aldermen observed in both scores",
      observations = nrow(alder_comparison),
      pearson_correlation = cor(alder_comparison$score_2022, alder_comparison$score_2014),
      spearman_correlation = cor(
        alder_comparison$score_2022,
        alder_comparison$score_2014,
        method = "spearman"
      ),
      sign_agreement = NA_real_
    ),
    alder_comparison %>%
      filter(active_in_2014_mapping) %>%
      summarise(
        sample = "Aldermen active in June 2014 mapping",
        observations = n(),
        pearson_correlation = cor(score_2022, score_2014),
        spearman_correlation = cor(score_2022, score_2014, method = "spearman"),
        sign_agreement = NA_real_
      ),
    tibble(
      sample = "Switching blocks within 500 feet",
      observations = nrow(block_500),
      pearson_correlation = cor(block_500$assigned_change_2022, block_500$assigned_change_2014),
      spearman_correlation = cor(
        block_500$assigned_change_2022,
        block_500$assigned_change_2014,
        method = "spearman"
      ),
      sign_agreement = mean(sign(block_500$assigned_change_2022) == sign(block_500$assigned_change_2014))
    ),
    tibble(
      sample = "Switching blocks within 1,000 feet",
      observations = nrow(block_1000),
      pearson_correlation = cor(block_1000$assigned_change_2022, block_1000$assigned_change_2014),
      spearman_correlation = cor(
        block_1000$assigned_change_2022,
        block_1000$assigned_change_2014,
        method = "spearman"
      ),
      sign_agreement = mean(sign(block_1000$assigned_change_2022) == sign(block_1000$assigned_change_2014))
    )
  ),
  "../output/frozen_score_correlations.csv"
)

write_csv(
  panel %>%
    filter(year == 2014L, switched, dist_m <= 304.8) %>%
    distinct(
      block_id, ward_pair_id, alderman_origin_2014, alderman_dest_2014,
      assigned_change_2022, assigned_change_2014
    ) %>%
    filter(sign(assigned_change_2022) != sign(assigned_change_2014)) %>%
    group_by(
      ward_pair_id, alderman_origin_2014, alderman_dest_2014,
      assigned_change_2022, assigned_change_2014
    ) %>%
    summarise(blocks = n(), .groups = "drop") %>%
    mutate(
      direction_2022 = if_else(assigned_change_2022 > 0, "stricter", "more_lenient"),
      direction_2014 = if_else(assigned_change_2014 > 0, "stricter", "more_lenient")
    ) %>%
    arrange(desc(blocks)),
  "../output/frozen_score_treatment_sign_switches_1000ft.csv"
)
