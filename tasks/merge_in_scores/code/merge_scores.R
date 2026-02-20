source("../../setup_environment/code/packages.R")

score_column <- "strictness_index"

name_aliases <- c(
  "Felix Cardona Jr" = "Felix Cardona Jr.",
  "Michael Scott Jr" = "Michael Scott Jr.",
  "Walter Burnett, Jr" = "Walter Burnett, Jr."
)

parcels <- read_csv("../input/parcels_pre_scores.csv", show_col_types = FALSE)
scores <- read_csv("../input/aldermen_strictness_scores.csv", show_col_types = FALSE)

parcels <- parcels %>%
  mutate(
    alderman_own = recode(str_squish(as.character(alderman_own)), !!!name_aliases),
    alderman_neighbor = recode(str_squish(as.character(alderman_neighbor)), !!!name_aliases)
  )

if (!score_column %in% names(scores)) {
  stop("Score column not found in score file: ", score_column)
}

score_lookup <- scores %>%
  transmute(
    alderman = str_squish(as.character(alderman)),
    score = .data[[score_column]]
  ) %>%
  group_by(alderman) %>%
  summarise(score = dplyr::first(score), .groups = "drop")

score_status <- scores %>%
  transmute(
    alderman = str_squish(as.character(alderman)),
    has_score = !is.na(.data[[score_column]])
  ) %>%
  group_by(alderman) %>%
  summarise(has_score = any(has_score), .groups = "drop") %>%
  mutate(missing_reason = if_else(has_score, "has_score", "score_missing"))

parcels_with_scores <- parcels %>%
  left_join(score_lookup, by = c("alderman_own" = "alderman")) %>%
  rename(strictness_own = score) %>%
  left_join(score_lookup, by = c("alderman_neighbor" = "alderman")) %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_distance = dist_to_boundary * sign
  )

parcels_final <- parcels_with_scores %>%
  filter(!is.na(signed_distance))

write_csv(parcels_final, "../output/parcels_with_ward_distances.csv")

boundary_summary <- parcels_final %>%
  summarise(
    n_parcels = n(),
    n_wards = n_distinct(ward),
    n_ward_pairs = n_distinct(ward_pair, na.rm = TRUE),
    n_aldermen = n_distinct(alderman_own, na.rm = TRUE),
    mean_dist = mean(signed_distance, na.rm = TRUE),
    median_dist = median(signed_distance, na.rm = TRUE),
    .by = c(boundary_year, construction_year)
  ) %>%
  arrange(construction_year)

write_csv(boundary_summary, "../output/boundary_distance_summary.csv")

n_total <- nrow(parcels)
n_own <- sum(!is.na(parcels_with_scores$strictness_own))
n_neighbor <- sum(!is.na(parcels_with_scores$strictness_neighbor))
n_signed <- sum(!is.na(parcels_with_scores$signed_distance))

own_missing <- parcels %>%
  count(alderman_own, name = "parcels") %>%
  rename(alderman = alderman_own) %>%
  left_join(score_status, by = "alderman") %>%
  mutate(
    side = "own",
    missing_reason = case_when(
      is.na(missing_reason) ~ "alderman_not_in_score_file",
      TRUE ~ missing_reason
    )
  )

neighbor_missing <- parcels %>%
  count(alderman_neighbor, name = "parcels") %>%
  rename(alderman = alderman_neighbor) %>%
  left_join(score_status, by = "alderman") %>%
  mutate(
    side = "neighbor",
    missing_reason = case_when(
      is.na(missing_reason) ~ "alderman_not_in_score_file",
      TRUE ~ missing_reason
    )
  )

missing_by_alderman <- bind_rows(own_missing, neighbor_missing) %>%
  arrange(side, desc(parcels), alderman)

missing_bucket <- missing_by_alderman %>%
  group_by(side, missing_reason) %>%
  summarise(parcels = sum(parcels), aldermen = n(), .groups = "drop")

coverage_qc <- tibble(
  section = "coverage",
  side = c("own", "neighbor", "signed_distance"),
  missing_reason = NA_character_,
  alderman = NA_character_,
  parcels = c(n_own, n_neighbor, n_signed),
  value_name = c("coverage_share", "coverage_share", "coverage_share"),
  value_numeric = c(n_own / n_total, n_neighbor / n_total, n_signed / n_total)
)

bucket_qc <- missing_bucket %>%
  transmute(
    section = "missing_bucket",
    side,
    missing_reason,
    alderman = NA_character_,
    parcels,
    value_name = "aldermen",
    value_numeric = as.numeric(aldermen)
  )

alderman_qc <- missing_by_alderman %>%
  transmute(
    section = "missing_by_alderman",
    side,
    missing_reason,
    alderman,
    parcels,
    value_name = NA_character_,
    value_numeric = NA_real_
  )

merge_qc <- bind_rows(coverage_qc, bucket_qc, alderman_qc)
write_csv(merge_qc, "../output/merge_qc.csv")

own_top_missing <- missing_by_alderman %>%
  filter(side == "own", missing_reason != "has_score") %>%
  slice_head(n = 10)

neighbor_top_missing <- missing_by_alderman %>%
  filter(side == "neighbor", missing_reason != "has_score") %>%
  slice_head(n = 10)

txt_lines <- c(
  "merge qc summary",
  paste0("parcels_total: ", n_total),
  paste0("own_score_coverage: ", n_own, " (", round(n_own / n_total, 4), ")"),
  paste0("neighbor_score_coverage: ", n_neighbor, " (", round(n_neighbor / n_total, 4), ")"),
  paste0("signed_distance_coverage: ", n_signed, " (", round(n_signed / n_total, 4), ")"),
  "own_missing_buckets:",
  paste0(
    "  ",
    missing_bucket %>% filter(side == "own") %>% pull(missing_reason),
    " | parcels=",
    missing_bucket %>% filter(side == "own") %>% pull(parcels),
    " | aldermen=",
    missing_bucket %>% filter(side == "own") %>% pull(aldermen)
  ),
  "neighbor_missing_buckets:",
  paste0(
    "  ",
    missing_bucket %>% filter(side == "neighbor") %>% pull(missing_reason),
    " | parcels=",
    missing_bucket %>% filter(side == "neighbor") %>% pull(parcels),
    " | aldermen=",
    missing_bucket %>% filter(side == "neighbor") %>% pull(aldermen)
  ),
  "top_own_missing_aldermen:",
  paste0("  ", own_top_missing$alderman, " | ", own_top_missing$missing_reason, " | parcels=", own_top_missing$parcels),
  "top_neighbor_missing_aldermen:",
  paste0("  ", neighbor_top_missing$alderman, " | ", neighbor_top_missing$missing_reason, " | parcels=", neighbor_top_missing$parcels)
)

writeLines(txt_lines, "../output/merge_qc.txt")

message("Wrote ../output/parcels_with_ward_distances.csv")
message("Wrote ../output/boundary_distance_summary.csv")
message("Wrote ../output/merge_qc.csv")
message("Wrote ../output/merge_qc.txt")
