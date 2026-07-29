# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")

scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L, spec_id == "controls_drop_income") %>%
  transmute(
    alderman_key = str_squish(str_to_lower(alderman)),
    score
  )
if (anyDuplicated(scores$alderman_key) > 0) {
  stop("The through-2022 no-income score is not unique by alderman.", call. = FALSE)
}

reassign_score <- function(data) {
  data %>%
    select(-any_of(c("strictness_own", "strictness_neighbor", "sign", "signed_dist_m", "signed_dist"))) %>%
    mutate(
      alderman_own_key = str_squish(str_to_lower(alderman_own)),
      alderman_neighbor_key = str_squish(str_to_lower(alderman_neighbor))
    ) %>%
    left_join(
      scores %>% rename(alderman_own_key = alderman_key, strictness_own = score),
      by = "alderman_own_key",
      relationship = "many-to-one"
    ) %>%
    left_join(
      scores %>% rename(alderman_neighbor_key = alderman_key, strictness_neighbor = score),
      by = "alderman_neighbor_key",
      relationship = "many-to-one"
    ) %>%
    mutate(
      sign = if_else(strictness_own > strictness_neighbor, 1, -1),
      signed_dist_m = abs(dist_m) * sign,
      signed_dist = signed_dist_m / 0.3048
    ) %>%
    select(-alderman_own_key, -alderman_neighbor_key)
}

rent <- read_parquet("../input/rent_with_ward_distances_full_source.parquet") %>%
  reassign_score()
sales <- read_parquet("../input/sales_with_hedonics_amenities_source.parquet") %>%
  reassign_score()

if (
  anyNA(rent$strictness_own) || anyNA(rent$strictness_neighbor) ||
    anyNA(sales$strictness_own) || anyNA(sales$strictness_neighbor)
) {
  stop("A price-RD alderman is missing a no-income score.", call. = FALSE)
}

write_parquet(rent, "../output/rent_with_ward_distances_full.parquet")
write_parquet(sales, "../output/sales_with_hedonics_amenities.parquet")
