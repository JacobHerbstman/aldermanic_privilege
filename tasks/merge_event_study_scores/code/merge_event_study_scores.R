# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_event_study_scores/code")

source("../../setup_environment/code/packages.R")

treatment <- read_csv("../input/block_treatment_pre_scores.csv", show_col_types = FALSE) %>%
  mutate(block_id = as.character(block_id), cohort = as.character(cohort)) %>%
  filter(cohort == "2015")
if (anyDuplicated(treatment$block_id) > 0) {
  stop("The 2015 treatment input must be unique by block.", call. = FALSE)
}

aldermen <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(
    month_date = as.Date(paste("01", month), format = "%d %b %Y"),
    month_key = format(month_date, "%Y-%m")
  ) %>%
  filter(month_key %in% c("2014-06", "2015-06")) %>%
  select(month_key, ward, alderman)
if (anyDuplicated(aldermen[c("month_key", "ward")]) > 0) {
  stop("Alderman lookup must be unique by month and ward.", call. = FALSE)
}

aldermen_2014 <- aldermen %>%
  filter(month_key == "2014-06") %>%
  select(ward, alderman_2014 = alderman)
aldermen_2015 <- aldermen %>%
  filter(month_key == "2015-06") %>%
  select(ward, alderman_2015 = alderman)

scores <- read_csv("../input/aldermen_uncertainty_scores_through2014.csv", show_col_types = FALSE) %>%
  select(alderman, strictness = uncertainty_index)
if (anyDuplicated(scores$alderman) > 0 || any(!is.finite(scores$strictness))) {
  stop("Frozen stringency scores must be finite and unique by alderman.", call. = FALSE)
}

treatment <- treatment %>%
  left_join(
    aldermen_2014 %>% rename(ward_origin = ward, alderman_origin_2014 = alderman_2014),
    by = "ward_origin",
    relationship = "many-to-one"
  ) %>%
  left_join(
    aldermen_2015 %>% rename(ward_origin = ward, alderman_origin_2015 = alderman_2015),
    by = "ward_origin",
    relationship = "many-to-one"
  ) %>%
  left_join(
    aldermen_2014 %>% rename(ward_dest = ward, alderman_dest_2014 = alderman_2014),
    by = "ward_dest",
    relationship = "many-to-one"
  ) %>%
  left_join(
    aldermen_2015 %>% rename(ward_dest = ward, alderman_dest_2015 = alderman_2015),
    by = "ward_dest",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_origin_2014 = alderman, strictness_origin_frozen = strictness),
    by = "alderman_origin_2014",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_dest_2014 = alderman, strictness_dest_frozen = strictness),
    by = "alderman_dest_2014",
    relationship = "many-to-one"
  ) %>%
  mutate(
    stable_origin = alderman_origin_2014 == alderman_origin_2015,
    stable_dest = alderman_dest_2014 == alderman_dest_2015,
    stable_both = stable_origin & stable_dest,
    strictness_origin = strictness_origin_frozen,
    strictness_dest = strictness_dest_frozen,
    strictness_change = strictness_dest - strictness_origin,
    strictness_change_frozen = strictness_change,
    switch_type = case_when(
      strictness_change > 0 ~ "Moved to Stricter",
      strictness_change < 0 ~ "Moved to More Lenient",
      TRUE ~ "No Change"
    )
  )

if (any(treatment$valid & (is.na(treatment$strictness_change) | is.na(treatment$stable_both)))) {
  stop("Valid treatment rows must have complete frozen scores and incumbent status.", call. = FALSE)
}

write_csv(treatment, "../output/block_treatment_panel_frozen2014.csv")
