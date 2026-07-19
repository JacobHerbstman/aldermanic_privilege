# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_rent_scores/code")

source("../../setup_environment/code/packages.R")

scores <- read_csv("../input/aldermen_uncertainty_scores_through2022.csv", show_col_types = FALSE) %>%
  select(alderman, score = uncertainty_index) %>%
  filter(!is.na(alderman))
if (anyDuplicated(scores$alderman) > 0 || any(!is.finite(scores$score))) {
  stop("Stringency scores must be finite and unique by alderman.", call. = FALSE)
}

rent_pre <- read_parquet("../input/rent_pre_scores_full.parquet") %>% as_tibble()
if (!all(c("alderman_own", "alderman_neighbor", "dist_m") %in% names(rent_pre))) {
  stop("Rental input must include alderman_own, alderman_neighbor, and dist_m.", call. = FALSE)
}

rent <- rent_pre %>%
  left_join(scores, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_own = score) %>%
  left_join(scores, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_dist_m = dist_m * sign,
    dist_ft = dist_m / 0.3048,
    signed_dist = signed_dist_m / 0.3048
  )

if (nrow(rent) != nrow(rent_pre)) {
  stop("Row count changed while attaching rental scores.", call. = FALSE)
}

rent <- rent %>% filter(!is.na(sign))
if (!"rent_panel_id" %in% names(rent) ||
    any(is.na(rent$rent_panel_id) | rent$rent_panel_id == "") ||
    anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Scored rental data must have a unique, nonmissing rent_panel_id.", call. = FALSE)
}
if (any(
  is.finite(rent$signed_dist) & rent$signed_dist != 0 &
    sign(rent$signed_dist) != rent$sign,
  na.rm = TRUE
)) {
  stop("Rental signed distance does not agree with the stringency ordering.", call. = FALSE)
}

write_parquet(rent, "../output/rent_with_ward_distances_full_through2022.parquet")
