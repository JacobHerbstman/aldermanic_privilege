# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")

source("../../../setup_environment/code/packages.R")

panel <- read_parquet("../input/permit_block_year_panel_2015.parquet")
if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("Permit panel must be unique by block and year.", call. = FALSE)
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

scores_2022 <- read_csv("../input/aldermen_uncertainty_scores_2022.csv", show_col_types = FALSE) %>%
  select(alderman, score_2022 = uncertainty_index)
scores_2014 <- read_csv("../input/aldermen_uncertainty_scores_2014.csv", show_col_types = FALSE) %>%
  select(alderman, score_2014 = uncertainty_index)
if (anyDuplicated(scores_2022$alderman) > 0 || anyDuplicated(scores_2014$alderman) > 0) {
  stop("Stringency scores must be unique by alderman.", call. = FALSE)
}

block_mapping <- panel %>%
  distinct(block_id, ward_origin, ward_dest, switched) %>%
  left_join(
    aldermen_2014 %>% rename(ward_origin = ward, alderman_origin_2014 = alderman_2014),
    by = "ward_origin", relationship = "many-to-one"
  ) %>%
  left_join(
    aldermen_2015 %>% rename(ward_origin = ward, alderman_origin_2015 = alderman_2015),
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
    scores_2022 %>% rename(alderman_origin_2014 = alderman, strictness_origin_2022 = score_2022),
    by = "alderman_origin_2014", relationship = "many-to-one"
  ) %>%
  left_join(
    scores_2022 %>% rename(alderman_dest_2014 = alderman, strictness_dest_2022 = score_2022),
    by = "alderman_dest_2014", relationship = "many-to-one"
  ) %>%
  left_join(
    scores_2022 %>% rename(alderman_dest_2015 = alderman, realized_strictness_dest_2022 = score_2022),
    by = "alderman_dest_2015", relationship = "many-to-one"
  ) %>%
  left_join(
    scores_2014 %>% rename(alderman_origin_2014 = alderman, strictness_origin_2014 = score_2014),
    by = "alderman_origin_2014", relationship = "many-to-one"
  ) %>%
  left_join(
    scores_2014 %>% rename(alderman_dest_2014 = alderman, strictness_dest_2014 = score_2014),
    by = "alderman_dest_2014", relationship = "many-to-one"
  ) %>%
  left_join(
    scores_2014 %>% rename(alderman_dest_2015 = alderman, realized_strictness_dest_2014 = score_2014),
    by = "alderman_dest_2015", relationship = "many-to-one"
  ) %>%
  mutate(
    stable_origin = alderman_origin_2014 == alderman_origin_2015,
    stable_dest = alderman_dest_2014 == alderman_dest_2015,
    stable_both = stable_origin & stable_dest,
    assigned_change_2022 = strictness_dest_2022 - strictness_origin_2022,
    realized_change_2022 = realized_strictness_dest_2022 - strictness_origin_2022,
    assigned_change_2014 = strictness_dest_2014 - strictness_origin_2014,
    realized_change_2014 = realized_strictness_dest_2014 - strictness_origin_2014
  )
if (anyNA(block_mapping$stable_both) || anyNA(block_mapping$assigned_change_2022) ||
    anyNA(block_mapping$realized_change_2022)) {
  stop("Corrected 2022-score treatment mapping contains missing values.", call. = FALSE)
}

panel <- panel %>%
  select(-strictness_origin, -strictness_dest, -strictness_change) %>%
  left_join(
    block_mapping %>% select(
      block_id, alderman_origin_2014, alderman_dest_2014,
      stable_origin, stable_dest, stable_both,
      strictness_origin_2022, strictness_dest_2022, assigned_change_2022,
      realized_strictness_dest_2022, realized_change_2022,
      strictness_origin_2014, strictness_dest_2014, assigned_change_2014,
      realized_strictness_dest_2014, realized_change_2014
    ),
    by = "block_id", relationship = "many-to-one"
  )

write_parquet(panel, "../output/corrected_permit_block_year_panel.parquet")
write_csv(
  panel %>%
    filter(year == 2014L, dist_m <= 304.8) %>%
    summarise(
      blocks_existing_panel = n_distinct(block_id),
      switchers_existing_panel = n_distinct(block_id[switched]),
      stable_blocks = n_distinct(block_id[stable_both]),
      stable_switchers = n_distinct(block_id[stable_both & switched]),
      unstable_switchers_recovered_by_itt = n_distinct(block_id[!stable_both & switched]),
      ward_pairs = n_distinct(ward_pair_id)
    ),
  "../output/corrected_permit_sample_summary.csv"
)
