# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/merge_extended_event_study_scores/code")

source("../../../setup_environment/code/packages.R")

scores <- read_csv("../input/aldermen_uncertainty_scores_through202604.csv", show_col_types = FALSE) %>%
  select(alderman, score = uncertainty_index) %>%
  filter(!is.na(alderman))
if (anyDuplicated(scores$alderman) > 0 || any(!is.finite(scores$score))) {
  stop("Extended scores must be finite and unique by alderman.", call. = FALSE)
}

treatment <- read_csv("../input/block_treatment_pre_scores.csv", show_col_types = FALSE)
if (anyDuplicated(paste(treatment$cohort, treatment$block_id, sep = "\r")) > 0) {
  stop("Treatment assignments must be unique by cohort and block.", call. = FALSE)
}

aldermen <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(
    month_date = as.Date(paste("01", month), format = "%d %b %Y"),
    month_key = format(month_date, "%Y-%m")
  ) %>%
  select(month_key, ward, alderman)
if (anyDuplicated(aldermen[c("month_key", "ward")]) > 0) {
  stop("Alderman lookup must be unique by month and ward.", call. = FALSE)
}

treatment <- treatment %>%
  mutate(
    cohort = as.character(cohort),
    score_month_origin = case_when(
      cohort == "2015" ~ "2014-06",
      cohort == "2023" ~ "2023-04",
      TRUE ~ NA_character_
    ),
    score_month_dest = case_when(
      cohort == "2015" ~ "2015-06",
      cohort == "2023" ~ "2023-06",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    aldermen %>% rename(ward_origin = ward, alderman_origin = alderman),
    by = c("score_month_origin" = "month_key", "ward_origin"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    aldermen %>% rename(ward_dest = ward, alderman_dest = alderman),
    by = c("score_month_dest" = "month_key", "ward_dest"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_origin = alderman, strictness_origin = score),
    by = "alderman_origin",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_dest = alderman, strictness_dest = score),
    by = "alderman_dest",
    relationship = "many-to-one"
  ) %>%
  mutate(
    strictness_change = strictness_dest - strictness_origin,
    switch_type = case_when(
      is.na(strictness_change) ~ "No Data",
      strictness_change > 0 ~ "Moved to Stricter",
      strictness_change < 0 ~ "Moved to More Lenient",
      TRUE ~ "No Change"
    )
  )

if (any(treatment$valid & is.na(treatment$strictness_change), na.rm = TRUE)) {
  stop("Valid treatment rows have missing extended scores.", call. = FALSE)
}

write_csv(treatment, "../output/block_treatment_panel_through202604.csv")
