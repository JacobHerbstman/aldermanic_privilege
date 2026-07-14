# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_event_study_scores/code")

source("../../setup_environment/code/packages.R")

scores_raw <- read_csv("../input/aldermen_uncertainty_scores_through2022.csv", show_col_types = FALSE)
if (!"uncertainty_index" %in% names(scores_raw)) {
  stop("Score input must include uncertainty_index.", call. = FALSE)
}

scores <- scores_raw %>%
  select(alderman, score = uncertainty_index) %>%
  filter(!is.na(alderman))
if (anyDuplicated(scores$alderman) > 0) {
  stop("Score input must be unique by alderman.", call. = FALSE)
}
if (any(!is.finite(scores$score))) {
  stop("Score input contains non-finite scores.", call. = FALSE)
}

rent_pre <- read_parquet("../input/rent_pre_scores_full.parquet") %>%
  as_tibble()
if (!all(c("alderman_own", "alderman_neighbor", "dist_m") %in% names(rent_pre))) {
  stop("Rent pre-score input must include alderman_own, alderman_neighbor, and dist_m.", call. = FALSE)
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
  stop("Row-count mismatch after rent score merge.", call. = FALSE)
}

rent <- rent %>%
  filter(!is.na(sign))

if (!"rent_panel_id" %in% names(rent)) {
  stop("Rent score merge output must include rent_panel_id.", call. = FALSE)
}
if (any(is.na(rent$rent_panel_id) | rent$rent_panel_id == "")) {
  stop("Rent score merge output contains missing rent_panel_id values.", call. = FALSE)
}
if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Rent score merge output must be unique by rent_panel_id.", call. = FALSE)
}

n_signed_dist_sign_mismatch <- sum(
  is.finite(rent$signed_dist) & rent$signed_dist != 0 &
    sign(rent$signed_dist) != rent$sign,
  na.rm = TRUE
)
if (n_signed_dist_sign_mismatch > 0) {
  stop("Rental signed-distance sign does not agree with strictness sign.", call. = FALSE)
}

write_parquet(rent, "../output/rent_with_ward_distances_full_through2022.parquet")
