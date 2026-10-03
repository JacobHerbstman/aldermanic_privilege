# setwd("tasks/audits/scores_through_2026/code")
# Exploratory: which aldermen's new scores change the more-stringent side for multifamily buildings in the paper's
# density sample (2006-2022 construction, within 500 ft), and how much each change moves the paper's average
# difference. A building's side flips when its own and neighboring aldermen trade places between the published
# 2006-2022 score and the score through June 2026 without self-certification permits; all buildings sharing an
# alderman pair flip together. Each flipped pair is then flipped alone, starting from the published sides, and the
# multifamily average difference re-estimated (density specification of tasks/shared/code/density_boundary_helpers.R).
new_version <- "through_2026_no_self_cert"

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/density_boundary_helpers.R")

published <- read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE)
new <- read_csv("../output/scores_through_2026.csv", show_col_types = FALSE) |> filter(score_version == new_version)
score_old <- setNames(published$uncertainty_index, published$alderman)
score_new <- setNames(new$uncertainty_index, new$alderman)

buildings <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample() |>
  filter(abs(distance_to_boundary_ft) < density_bandwidth_ft, !is.na(alderman_own), !is.na(alderman_neighbor)) |>
  filter_density_sample("multifamily") |>
  mutate(old_own = unname(score_old[alderman_own]), old_neighbor = unname(score_old[alderman_neighbor]),
    new_own = unname(score_new[alderman_own]), new_neighbor = unname(score_new[alderman_neighbor]),
    old_side = sign(old_own - old_neighbor), new_side = sign(new_own - new_neighbor),
    alderman_pair = paste(pmin(alderman_own, alderman_neighbor), pmax(alderman_own, alderman_neighbor), sep = " | "))
stopifnot(!anyNA(buildings$old_side), !anyNA(buildings$new_side), all(buildings$old_side != 0), all(buildings$new_side != 0))

fit_average <- function(data, side) {
  fit <- data |>
    mutate(running_distance_ft = abs(distance_to_boundary_ft) * side) |>
    bin_running_distance() |>
    fit_density_boundary()
  fit$average
}
published_fit <- fit_average(buildings, buildings$old_side)
new_fit <- fit_average(buildings, buildings$new_side)

# Each alderman pair: its aldermen and scores, buildings, and whether its sides flip.
pairs <- buildings |>
  mutate(alderman_a = pmin(alderman_own, alderman_neighbor), alderman_b = pmax(alderman_own, alderman_neighbor)) |>
  summarise(alderman_a = first(alderman_a), alderman_b = first(alderman_b), ward_pairs = paste(sort(unique(ward_pair)), collapse = "/"),
    n_buildings = n(), first_year = min(construction_year), last_year = max(construction_year),
    flipped = first(old_side != new_side), .by = alderman_pair) |>
  mutate(score_a_old = unname(score_old[alderman_a]), score_b_old = unname(score_old[alderman_b]),
    score_a_new = unname(score_new[alderman_a]), score_b_new = unname(score_new[alderman_b]),
    stricter_old = if_else(score_a_old > score_b_old, alderman_a, alderman_b),
    stricter_new = if_else(score_a_new > score_b_new, alderman_a, alderman_b))
stopifnot(all(buildings |> summarise(n = n_distinct(old_side != new_side), .by = alderman_pair) |> pull(n) == 1))

# Flipping each flipped pair alone, from the published sides.
one_flip <- pairs |>
  filter(flipped) |>
  mutate(estimate_with_flip = purrr::map_dbl(alderman_pair, function(p) {
    side <- if_else(buildings$alderman_pair == p, buildings$new_side, buildings$old_side)
    fit_average(buildings, side)$estimate
  }), change_from_published = estimate_with_flip - published_fit$estimate) |>
  arrange(change_from_published)

# Each alderman: score under both versions and the multifamily buildings in flipped pairs they belong to.
aldermen <- one_flip |>
  select(alderman_pair, n_buildings, change_from_published, alderman_a, alderman_b) |>
  tidyr::pivot_longer(c(alderman_a, alderman_b), values_to = "alderman") |>
  summarise(flipped_pairs = n(), buildings_in_flipped_pairs = sum(n_buildings),
    sum_of_single_flip_changes = sum(change_from_published), .by = alderman) |>
  mutate(score_old = unname(score_old[alderman]), score_new = unname(score_new[alderman]),
    score_change = score_new - score_old) |>
  arrange(desc(buildings_in_flipped_pairs))

write_csv(bind_rows(mutate(published_fit, sides = "published"), mutate(new_fit, sides = new_version)) |>
  mutate(n_buildings = nrow(.env$buildings), n_flipped = sum(.env$buildings$old_side != .env$buildings$new_side)),
  "../output/density_side_flips_summary.csv")
write_csv(one_flip, "../output/density_side_flips_by_pair.csv")
write_csv(aldermen, "../output/density_side_flips_by_alderman.csv")
