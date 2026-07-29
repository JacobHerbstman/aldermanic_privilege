# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

blocks <- read_csv(
  "../../../create_block_treatment_panel/output/block_treatment_pre_scores.csv",
  show_col_types = FALSE
) %>%
  filter(as.character(cohort) == "2015")

aldermen <- read_csv(
  "../../../create_alderman_data/output/chicago_alderman_panel.csv",
  show_col_types = FALSE
) %>%
  mutate(
    month_date = as.Date(paste("01", month), format = "%d %b %Y"),
    month_key = format(month_date, "%Y-%m")
  ) %>%
  filter(month_key %in% c("2014-06", "2015-06")) %>%
  select(month_key, ward, alderman)

scores_2014 <- read_csv(
  "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score_2014 = uncertainty_index)
scores_202604 <- read_csv(
  "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through202604.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score_202604 = uncertainty_index)

aldermen_2014 <- aldermen %>%
  filter(month_key == "2014-06") %>%
  select(ward, alderman_2014 = alderman)
aldermen_2015 <- aldermen %>%
  filter(month_key == "2015-06") %>%
  select(ward, alderman_2015 = alderman)

lineage <- blocks %>%
  left_join(
    aldermen_2014 %>% rename(ward_origin = ward, alderman_origin_2014 = alderman_2014),
    by = "ward_origin", relationship = "many-to-one"
  ) %>%
  left_join(
    aldermen_2014 %>% rename(ward_dest = ward, alderman_dest_2014 = alderman_2014),
    by = "ward_dest", relationship = "many-to-one"
  ) %>%
  left_join(
    aldermen_2015 %>% rename(ward_dest = ward, alderman_dest_2015 = alderman_2015),
    by = "ward_dest", relationship = "many-to-one"
  ) %>%
  left_join(
    scores_2014 %>% rename(alderman_origin_2014 = alderman, frozen_origin_score = score_2014),
    by = "alderman_origin_2014", relationship = "many-to-one"
  ) %>%
  left_join(
    scores_2014 %>% rename(alderman_dest_2014 = alderman, frozen_dest_score = score_2014),
    by = "alderman_dest_2014", relationship = "many-to-one"
  ) %>%
  left_join(
    scores_202604 %>% rename(alderman_origin_2014 = alderman, current_origin_score = score_202604),
    by = "alderman_origin_2014", relationship = "many-to-one"
  ) %>%
  left_join(
    scores_202604 %>% rename(alderman_dest_2015 = alderman, current_dest_score = score_202604),
    by = "alderman_dest_2015", relationship = "many-to-one"
  ) %>%
  mutate(
    frozen_score_available = is.finite(frozen_origin_score) & is.finite(frozen_dest_score),
    current_score_filter_available = is.finite(current_origin_score) & is.finite(current_dest_score),
    intended_frozen_eligible = valid & frozen_score_available,
    production_upstream_eligible = valid & current_score_filter_available,
    eligibility_difference = case_when(
      intended_frozen_eligible & !production_upstream_eligible ~ "excluded_only_by_202604_filter",
      !intended_frozen_eligible & production_upstream_eligible ~ "retained_only_by_202604_filter",
      intended_frozen_eligible & production_upstream_eligible ~ "eligible_under_both",
      TRUE ~ "ineligible_under_both"
    )
  )

summary <- lineage %>%
  count(switched, valid, eligibility_difference, name = "blocks") %>%
  arrange(valid, switched, eligibility_difference)
write_csv(summary, "../output/permit_event_score_lineage_summary.csv")

write_csv(
  lineage %>%
    filter(eligibility_difference %in% c(
      "excluded_only_by_202604_filter",
      "retained_only_by_202604_filter"
    )) %>%
    select(
      block_id, switched, ward_origin, ward_dest, ward_had_turnover, valid,
      alderman_origin_2014, alderman_dest_2014, alderman_dest_2015,
      frozen_origin_score, frozen_dest_score, current_origin_score, current_dest_score,
      frozen_score_available, current_score_filter_available,
      intended_frozen_eligible, production_upstream_eligible, eligibility_difference
    ),
  "../output/permit_event_score_lineage_blocks.csv"
)
