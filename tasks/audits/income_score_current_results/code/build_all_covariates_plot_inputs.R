# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/income_score_current_results/code")

source("../../../setup_environment/code/packages.R")

scores <- read_csv("../output/current_income_scores.csv", show_col_types = FALSE)

scores_2022 <- scores %>%
  filter(cutoff == 2022L, variant == "all_covariates") %>%
  select(alderman, score)

scores_2014 <- scores %>%
  filter(cutoff == 2014L, variant == "all_covariates") %>%
  select(alderman, score)

if (anyDuplicated(scores_2022$alderman) > 0 || anyDuplicated(scores_2014$alderman) > 0) {
  stop("All-covariate scores must be unique by alderman.", call. = FALSE)
}

score_map_2022 <- deframe(scores_2022)
score_map_2014 <- deframe(scores_2014)

density <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  mutate(
    strictness_own = unname(score_map_2022[alderman_own]),
    strictness_neighbor = unname(score_map_2022[alderman_neighbor]),
    sign = if_else(strictness_own > strictness_neighbor, 1, -1, missing = NA_real_),
    signed_distance = abs(dist_to_boundary) * sign,
    signed_distance_m = abs(dist_to_boundary_m) * sign
  )

rent <- read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble() %>%
  mutate(
    strictness_own = unname(score_map_2022[alderman_own]),
    strictness_neighbor = unname(score_map_2022[alderman_neighbor]),
    sign = if_else(strictness_own > strictness_neighbor, 1, -1, missing = NA_real_),
    signed_dist_m = abs(dist_m) * sign,
    signed_dist = abs(dist_ft) * sign,
    signed_dist_ft = signed_dist,
    right = as.integer(sign > 0)
  )

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    strictness_own = unname(score_map_2022[alderman_own]),
    strictness_neighbor = unname(score_map_2022[alderman_neighbor]),
    sign = if_else(strictness_own > strictness_neighbor, 1, -1, missing = NA_real_),
    signed_dist_m = abs(dist_m) * sign
  )

permit <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  as_tibble() %>%
  mutate(
    strictness_origin_frozen = unname(score_map_2014[alderman_origin_2014]),
    strictness_dest_frozen = unname(score_map_2014[alderman_dest_2014]),
    strictness_change_frozen = strictness_dest_frozen - strictness_origin_frozen
  )

if (any(rent$right != as.integer(rent$strictness_own > rent$strictness_neighbor), na.rm = TRUE)) {
  stop("Rental score ordering is inconsistent after remapping.", call. = FALSE)
}
if (any(sign(sales$signed_dist_m) != sign(sales$strictness_own - sales$strictness_neighbor), na.rm = TRUE)) {
  stop("Sales score ordering is inconsistent after remapping.", call. = FALSE)
}

write_csv(density, "../output/all_covariates_parcels_with_ward_distances.csv")
write_parquet(rent, "../output/all_covariates_rental_rd_characteristics_panel_bw500.parquet")
write_parquet(sales, "../output/all_covariates_sales_with_hedonics_amenities.parquet")
write_parquet(permit, "../output/all_covariates_permit_block_year_panel_2015.parquet")
write_csv(
  scores_2022 %>% rename(uncertainty_index = score),
  "../output/all_covariates_alderman_uncertainty_index_through2022.csv"
)
