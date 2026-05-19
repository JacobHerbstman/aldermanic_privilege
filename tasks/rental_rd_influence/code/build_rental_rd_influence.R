# Diagnose whether the 500ft rental RD is driven by a narrow set of pairs.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_influence/code")

source("../../setup_environment/code/packages.R")

message("=== Rental RD influence diagnostics ===")

if ("setFixest_nthreads" %in% getNamespaceExports("fixest")) {
  fixest::setFixest_nthreads(max(1L, parallel::detectCores(logical = FALSE) - 1L))
}

rent <- read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble()

required_cols <- c(
  "rent_panel_id",
  "rent_price",
  "right",
  "segment_id",
  "year_month",
  "ward",
  "neighbor_ward",
  "ward_pair",
  "alderman_own",
  "alderman_neighbor",
  "flag_clean_location_sample",
  "log_sqft",
  "log_beds",
  "log_baths",
  "building_type_factor",
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft"
)
missing_cols <- setdiff(required_cols, names(rent))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

control_cols <- c(
  "log_sqft",
  "log_beds",
  "log_baths",
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft"
)

rent <- rent %>%
  mutate(
    ward = as.character(ward),
    neighbor_ward = as.character(neighbor_ward),
    ward_pair = as.character(ward_pair),
    segment_id = as.character(segment_id),
    alderman_own = as.character(alderman_own),
    alderman_neighbor = as.character(alderman_neighbor),
    building_type_factor = factor(building_type_factor)
  ) %>%
  filter(
    flag_clean_location_sample,
    is.finite(rent_price),
    rent_price > 0,
    right %in% c(0, 1),
    !is.na(segment_id),
    segment_id != "",
    !is.na(year_month),
    !is.na(ward_pair),
    if_all(all_of(control_cols), ~ is.finite(.x))
  )

if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Influence sample is not unique by rent_panel_id.", call. = FALSE)
}
if (n_distinct(rent$right) < 2 || n_distinct(rent$segment_id) < 2) {
  stop("Influence sample lacks RD support.", call. = FALSE)
}

message(sprintf(
  "Influence sample: %s rows, %s ward pairs, %s segments.",
  format(nrow(rent), big.mark = ","),
  format(n_distinct(rent$ward_pair), big.mark = ","),
  format(n_distinct(rent$segment_id), big.mark = ",")
))

rd_formula <- as.formula(paste0(
  "log(rent_price) ~ right + ",
  paste(control_cols, collapse = " + "),
  " + building_type_factor | segment_id^year_month"
))

pct_effect <- function(x) 100 * (exp(x) - 1)

fit_rd <- function(data) {
  if (nrow(data) < 1000 || n_distinct(data$right) < 2 || n_distinct(data$segment_id) < 2) {
    return(tibble(
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(data),
      n_segments = n_distinct(data$segment_id),
      n_ward_pairs = n_distinct(data$ward_pair)
    ))
  }

  model <- tryCatch(
    feols(rd_formula, data = data, cluster = ~segment_id),
    error = function(e) e
  )
  if (inherits(model, "error") || !"right" %in% names(coef(model))) {
    return(tibble(
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(data),
      n_segments = n_distinct(data$segment_id),
      n_ward_pairs = n_distinct(data$ward_pair)
    ))
  }

  tibble(
    estimate = unname(coef(model)[["right"]]),
    std_error = unname(se(model)[["right"]]),
    p_value = unname(pvalue(model)[["right"]]),
    n_obs = model$nobs,
    n_segments = n_distinct(data$segment_id),
    n_ward_pairs = n_distinct(data$ward_pair)
  )
}

baseline <- fit_rd(rent) %>%
  mutate(
    diagnostic = "baseline",
    pct_change = pct_effect(estimate),
    pct_ci_low = pct_effect(estimate - 1.96 * std_error),
    pct_ci_high = pct_effect(estimate + 1.96 * std_error)
  )
if (!is.finite(baseline$estimate[[1]])) {
  stop("Baseline model failed.", call. = FALSE)
}

message(sprintf(
  "Baseline clean-location amenity-adjusted effect: %.2f%% [%.2f, %.2f].",
  baseline$pct_change[[1]],
  baseline$pct_ci_low[[1]],
  baseline$pct_ci_high[[1]]
))

pair_support <- rent %>%
  group_by(ward_pair) %>%
  summarise(
    n_obs = n(),
    n_segments = n_distinct(segment_id),
    n_segment_months = n_distinct(paste(segment_id, year_month)),
    n_aldermen = n_distinct(c(alderman_own, alderman_neighbor)),
    n_right = sum(right == 1),
    n_left = sum(right == 0),
    top_aldermen = paste(head(sort(table(c(alderman_own, alderman_neighbor)), decreasing = TRUE), 4) %>% names(), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_obs))
write_csv(pair_support, "../output/rental_rd_pair_support.csv")

pairs <- sort(unique(rent$ward_pair))
leave_one_pair <- bind_rows(lapply(seq_along(pairs), function(i) {
  if (i %% 20 == 0) {
    message(sprintf("Leave-one ward pair: %d / %d", i, length(pairs)))
  }
  pair_i <- pairs[i]
  out <- fit_rd(rent %>% filter(ward_pair != pair_i))
  out %>%
    mutate(
      dropped_pair = pair_i,
      dropped_obs = sum(rent$ward_pair == pair_i),
      dropped_segments = n_distinct(rent$segment_id[rent$ward_pair == pair_i]),
      estimate_delta = estimate - baseline$estimate[[1]],
      pct_change = pct_effect(estimate),
      pct_ci_low = pct_effect(estimate - 1.96 * std_error),
      pct_ci_high = pct_effect(estimate + 1.96 * std_error),
      pct_delta = pct_change - baseline$pct_change[[1]]
    )
})) %>%
  left_join(pair_support, by = c("dropped_pair" = "ward_pair"), suffix = c("", "_dropped")) %>%
  arrange(pct_delta)
write_csv(leave_one_pair, "../output/rental_rd_leave_one_pair.csv")

wards <- sort(unique(c(rent$ward, rent$neighbor_ward)))
leave_one_ward <- bind_rows(lapply(seq_along(wards), function(i) {
  ward_i <- wards[i]
  drop_idx <- rent$ward == ward_i | rent$neighbor_ward == ward_i
  out <- fit_rd(rent[!drop_idx, ])
  out %>%
    mutate(
      dropped_ward = ward_i,
      dropped_obs = sum(drop_idx),
      dropped_pairs = n_distinct(rent$ward_pair[drop_idx]),
      dropped_segments = n_distinct(rent$segment_id[drop_idx]),
      estimate_delta = estimate - baseline$estimate[[1]],
      pct_change = pct_effect(estimate),
      pct_ci_low = pct_effect(estimate - 1.96 * std_error),
      pct_ci_high = pct_effect(estimate + 1.96 * std_error),
      pct_delta = pct_change - baseline$pct_change[[1]]
    )
})) %>%
  arrange(pct_delta)
write_csv(leave_one_ward, "../output/rental_rd_leave_one_ward.csv")

aldermen <- sort(unique(stats::na.omit(c(rent$alderman_own, rent$alderman_neighbor))))
leave_one_alderman <- bind_rows(lapply(seq_along(aldermen), function(i) {
  alder_i <- aldermen[i]
  drop_idx <- rent$alderman_own == alder_i | rent$alderman_neighbor == alder_i
  out <- fit_rd(rent[!drop_idx, ])
  out %>%
    mutate(
      dropped_alderman = alder_i,
      dropped_obs = sum(drop_idx),
      dropped_pairs = n_distinct(rent$ward_pair[drop_idx]),
      dropped_segments = n_distinct(rent$segment_id[drop_idx]),
      estimate_delta = estimate - baseline$estimate[[1]],
      pct_change = pct_effect(estimate),
      pct_ci_low = pct_effect(estimate - 1.96 * std_error),
      pct_ci_high = pct_effect(estimate + 1.96 * std_error),
      pct_delta = pct_change - baseline$pct_change[[1]]
    )
})) %>%
  arrange(pct_delta)
write_csv(leave_one_alderman, "../output/rental_rd_leave_one_alderman.csv")

segment_support <- rent %>%
  group_by(segment_id, ward_pair) %>%
  summarise(
    n_obs = n(),
    n_right = sum(right == 1),
    n_left = sum(right == 0),
    .groups = "drop"
  ) %>%
  arrange(desc(n_obs))

top_segments <- head(segment_support$segment_id, 100)
leave_one_top_segment <- bind_rows(lapply(seq_along(top_segments), function(i) {
  if (i %% 25 == 0) {
    message(sprintf("Leave-one top segment: %d / %d", i, length(top_segments)))
  }
  segment_i <- top_segments[i]
  out <- fit_rd(rent %>% filter(segment_id != segment_i))
  out %>%
    mutate(
      dropped_segment = segment_i,
      dropped_obs = sum(rent$segment_id == segment_i),
      dropped_pair = segment_support$ward_pair[match(segment_i, segment_support$segment_id)],
      estimate_delta = estimate - baseline$estimate[[1]],
      pct_change = pct_effect(estimate),
      pct_ci_low = pct_effect(estimate - 1.96 * std_error),
      pct_ci_high = pct_effect(estimate + 1.96 * std_error),
      pct_delta = pct_change - baseline$pct_change[[1]]
    )
})) %>%
  arrange(pct_delta)
write_csv(leave_one_top_segment, "../output/rental_rd_leave_one_top_segment.csv")

pair_estimates <- bind_rows(lapply(seq_len(nrow(pair_support)), function(i) {
  pair_i <- pair_support$ward_pair[i]
  d <- rent %>% filter(ward_pair == pair_i)
  out <- fit_rd(d)
  out %>%
    mutate(
      ward_pair = pair_i,
      pct_change = pct_effect(estimate),
      pct_ci_low = pct_effect(estimate - 1.96 * std_error),
      pct_ci_high = pct_effect(estimate + 1.96 * std_error)
    )
})) %>%
  left_join(pair_support, by = "ward_pair", suffix = c("", "_support")) %>%
  arrange(desc(n_obs_support))
write_csv(pair_estimates, "../output/rental_rd_pair_estimates.csv")

summary_rows <- tibble(
  metric = c(
    "baseline_pct",
    "baseline_pct_ci_low",
    "baseline_pct_ci_high",
    "baseline_p_value",
    "baseline_n_obs",
    "baseline_n_segments",
    "baseline_n_ward_pairs",
    "leave_one_pair_min_pct",
    "leave_one_pair_max_pct",
    "leave_one_pair_share_positive",
    "leave_one_pair_share_p_lt_005",
    "leave_one_ward_min_pct",
    "leave_one_ward_max_pct",
    "leave_one_ward_share_positive",
    "leave_one_alderman_min_pct",
    "leave_one_alderman_max_pct",
    "leave_one_alderman_share_positive",
    "leave_one_top_segment_min_pct",
    "leave_one_top_segment_max_pct",
    "leave_one_top_segment_share_positive",
    "pair_specific_n_estimated",
    "pair_specific_share_positive",
    "pair_specific_weighted_mean_pct"
  ),
  value = c(
    baseline$pct_change[[1]],
    baseline$pct_ci_low[[1]],
    baseline$pct_ci_high[[1]],
    baseline$p_value[[1]],
    baseline$n_obs[[1]],
    baseline$n_segments[[1]],
    baseline$n_ward_pairs[[1]],
    min(leave_one_pair$pct_change, na.rm = TRUE),
    max(leave_one_pair$pct_change, na.rm = TRUE),
    mean(leave_one_pair$estimate > 0, na.rm = TRUE),
    mean(leave_one_pair$p_value < 0.05, na.rm = TRUE),
    min(leave_one_ward$pct_change, na.rm = TRUE),
    max(leave_one_ward$pct_change, na.rm = TRUE),
    mean(leave_one_ward$estimate > 0, na.rm = TRUE),
    min(leave_one_alderman$pct_change, na.rm = TRUE),
    max(leave_one_alderman$pct_change, na.rm = TRUE),
    mean(leave_one_alderman$estimate > 0, na.rm = TRUE),
    min(leave_one_top_segment$pct_change, na.rm = TRUE),
    max(leave_one_top_segment$pct_change, na.rm = TRUE),
    mean(leave_one_top_segment$estimate > 0, na.rm = TRUE),
    sum(is.finite(pair_estimates$estimate)),
    mean(pair_estimates$estimate > 0, na.rm = TRUE),
    weighted.mean(pair_estimates$pct_change, pair_estimates$n_obs_support, na.rm = TRUE)
  )
)
write_csv(summary_rows, "../output/rental_rd_influence_summary.csv")

plot_pairs <- leave_one_pair %>%
  mutate(
    dropped_pair = reorder(dropped_pair, pct_delta),
    ci_low = pct_ci_low,
    ci_high = pct_ci_high
  )

leave_pair_plot <- ggplot(plot_pairs, aes(x = dropped_pair, y = pct_change)) +
  geom_hline(yintercept = baseline$pct_change[[1]], linetype = "dashed", color = "gray45") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_point(size = 1.2, color = "#1f77b4") +
  coord_flip() +
  labs(
    title = "RentHub RD Leave-One-Ward-Pair Sensitivity",
    subtitle = "Clean-location 500ft sample, hedonic and amenity controls; dashed line is baseline",
    x = "Dropped ward pair",
    y = "Rent jump on stricter side (%)"
  ) +
  theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank())

ggsave("../output/rental_rd_leave_one_pair.png", leave_pair_plot, width = 8, height = 12, dpi = 220, bg = "white")

plot_pair_est <- pair_estimates %>%
  filter(is.finite(pct_change)) %>%
  mutate(
    significant = p_value < 0.05,
    positive = estimate > 0
  )

pair_est_plot <- ggplot(plot_pair_est, aes(x = n_obs_support, y = pct_change)) +
  geom_hline(yintercept = baseline$pct_change[[1]], linetype = "dashed", color = "gray45") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_point(aes(size = n_segments_support, color = positive), alpha = 0.7) +
  scale_x_log10() +
  scale_color_manual(values = c(`TRUE` = "#1f77b4", `FALSE` = "#d95f02")) +
  labs(
    title = "RentHub RD Pair-Specific Estimates",
    subtitle = "Each point is one ward pair estimated separately with segment-by-month FE",
    x = "Pair observations, log scale",
    y = "Rent jump on stricter side (%)",
    size = "Segments",
    color = "Positive"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("../output/rental_rd_pair_estimates.png", pair_est_plot, width = 8.5, height = 5.5, dpi = 220, bg = "white")

message("Saved rental RD influence diagnostics.")
