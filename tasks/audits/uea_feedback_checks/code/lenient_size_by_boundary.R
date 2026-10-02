# setwd("tasks/audits/uea_feedback_checks/code")
# Exploratory (UEA feedback, follow-up to continuous_stringency.R): the size of a move toward a more lenient alderman
# predicts more high-discretion permits even though the lenient indicator alone does not. Where does that come from?
#   1. Effects by size of move: reassigned blocks split into thirds of the score change within each direction.
#   2. Each move (blocks moved from one origin ward to one destination ward), with its aldermen, score change and
#      blocks, and the continuous lenient coefficient when that move's reassigned blocks are left out.
# Same samples and pooled PPML specifications as continuous_stringency.R.
bandwidth_m <- 152.4
event_specs <- tibble::tribble(
  ~spec, ~fixed_effect, ~first_year, ~last_year,
  "both_sides_2010_2020", "ward_pair_id^year", 2010L, 2020L,
  "same_side_2012_2018", "ward_pair_side^year", 2012L, 2018L
)

source("../../../setup_environment/code/packages.R")

panel <- arrow::read_parquet("../input/permit_block_year_panel_2015.parquet") |>
  filter(dist_m <= bandwidth_m, relative_year >= -5L, relative_year <= 5L, !is.na(strictness_change_frozen),
    !is.na(ward_pair_id), ward_pair_id != "", stable_both)
active <- panel |>
  filter(relative_year < 0L) |>
  summarise(pre_permits = sum(n_high_discretion_application), .by = block_id) |>
  filter(pre_permits > 0)
panel <- panel |>
  filter(block_id %in% active$block_id) |>
  mutate(
    change = strictness_change_frozen,
    direction = case_when(change > 0 ~ "stricter", change < 0 ~ "lenient", TRUE ~ "unchanged"),
    move = if_else(switched, paste0(ward_origin, "->", ward_dest), NA_character_),
    post = as.integer(year >= 2015L),
    post_stricter_size = post * pmax(change, 0), post_lenient_size = post * pmax(-change, 0)
  )
blocks <- panel |>
  distinct(block_id, switched, direction, change, move, ward_pair_id, alderman_origin_2014, alderman_dest_2014)
stopifnot(!anyDuplicated(blocks$block_id))
# Thirds of the size of the score change among reassigned blocks, within each direction.
size_groups <- blocks |>
  filter(switched) |>
  mutate(size_third = dplyr::ntile(abs(change), 3), .by = direction) |>
  select(block_id, size_third)
panel <- panel |>
  left_join(size_groups, by = "block_id", relationship = "many-to-one") |>
  mutate(size_group = if_else(switched, paste0(direction, "_", c("small", "medium", "large")[size_third]), "unchanged"))
size_summary <- panel |>
  filter(switched) |>
  distinct(block_id, size_group, move, change) |>
  summarise(blocks = n(), moves = n_distinct(move), min_change = min(abs(change)), max_change = max(abs(change)),
    .by = size_group)

size_results <- list()
move_results <- list()
for (s in seq_len(nrow(event_specs))) {
  d <- filter(panel, year >= event_specs$first_year[s], year <= event_specs$last_year[s])
  fit <- function(rhs, data = d) fixest::fepois(as.formula(paste("n_high_discretion_application ~", rhs, "| block_id +",
    event_specs$fixed_effect[s])), data = data, cluster = ~ward_pair_id, notes = FALSE)

  # 1. Effects by size of move.
  by_size <- fit("i(size_group, post, ref = 'unchanged')")
  ct <- fixest::coeftable(by_size)
  size_results[[s]] <- tibble(spec = event_specs$spec[s], term = rownames(ct), estimate = ct[, 1], std_error = ct[, 2],
      p_value = ct[, 4]) |>
    mutate(size_group = sub("^size_group::(.*):post$", "\\1", term)) |>
    left_join(size_summary, by = "size_group", relationship = "one-to-one") |>
    select(-term)

  # 2. Moves toward more lenient aldermen, and the continuous lenient coefficient without each.
  full <- fit("post_stricter_size + post_lenient_size")
  lenient_moves <- blocks |>
    filter(switched, direction == "lenient") |>
    summarise(origin_alderman = first(alderman_origin_2014), destination_alderman = first(alderman_dest_2014),
      score_change = first(change), reassigned_blocks = n(), ward_pair_id = first(ward_pair_id), .by = move)
  stopifnot(all(lenient_moves$score_change < 0))
  pre_permits <- d |>
    filter(switched, year < 2015L) |>
    summarise(pre_permits = sum(n_high_discretion_application), .by = move)
  post_permits <- d |>
    filter(switched, year >= 2015L) |>
    summarise(post_permits = sum(n_high_discretion_application), .by = move)
  leave_out <- lapply(lenient_moves$move, function(m) {
    model <- fit("post_stricter_size + post_lenient_size", filter(d, is.na(move) | move != m))
    tibble(move = m, estimate_without = coef(model)[["post_lenient_size"]],
      std_error_without = fixest::se(model)[["post_lenient_size"]])
  })
  move_results[[s]] <- lenient_moves |>
    left_join(pre_permits, by = "move", relationship = "one-to-one") |>
    left_join(post_permits, by = "move", relationship = "one-to-one") |>
    left_join(bind_rows(leave_out), by = "move", relationship = "one-to-one") |>
    mutate(spec = event_specs$spec[s], estimate_all = coef(full)[["post_lenient_size"]],
      std_error_all = fixest::se(full)[["post_lenient_size"]], .before = 1) |>
    arrange(estimate_without)
}
write_csv(bind_rows(size_results), "../output/lenient_size_by_size_group.csv")
write_csv(bind_rows(move_results), "../output/lenient_size_by_move.csv")
