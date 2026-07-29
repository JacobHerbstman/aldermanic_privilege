# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/permit_itt_spatial_permutation/code")
# bandwidth_m <- 152.4
# bandwidth_label <- "500ft"
# match_calipers_ft <- "2640,5280"
# match_label <- "2640-5280"
# boundary_distance_caliper_ft <- 100
# permutations <- 999
# seed <- 20260714

source("../../../setup_environment/code/packages.R")
library(fixest)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    bandwidth_m,
    bandwidth_label,
    match_calipers_ft,
    match_label,
    boundary_distance_caliper_ft,
    permutations,
    seed
  )
}
if (length(cli_args) != 7) {
  stop(
    "Script requires bandwidth, bandwidth label, match calipers, match label, boundary-distance caliper, permutation count, and seed.",
    call. = FALSE
  )
}

bandwidth_m <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]
match_calipers_ft <- as.numeric(strsplit(cli_args[3], ",", fixed = TRUE)[[1]])
match_label <- cli_args[4]
boundary_distance_caliper_ft <- as.numeric(cli_args[5])
permutations <- as.integer(cli_args[6])
seed <- as.integer(cli_args[7])

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}
if (length(match_calipers_ft) == 0 || any(!is.finite(match_calipers_ft)) || any(match_calipers_ft <= 0)) {
  stop("match calipers must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", match_label)) {
  stop("match label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}
if (!is.finite(boundary_distance_caliper_ft) || boundary_distance_caliper_ft <= 0) {
  stop("boundary-distance caliper must be positive.", call. = FALSE)
}
if (!is.finite(permutations) || permutations < 99) {
  stop("at least 99 permutations are required.", call. = FALSE)
}
if (!is.finite(seed)) {
  stop("seed must be an integer.", call. = FALSE)
}

panel <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  mutate(block_id = as.character(block_id))

if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("Permit panel must be unique by block and year.", call. = FALSE)
}

blocks <- panel %>%
  filter(
    cohort == "2015",
    relative_year == -1,
    dist_m <= bandwidth_m,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != "",
    !is.na(segment_id_cohort),
    segment_id_cohort != ""
  ) %>%
  distinct(block_id, .keep_all = TRUE) %>%
  transmute(
    block_id,
    ward_pair_id = as.character(ward_pair_id),
    ward_origin = as.integer(ward_origin),
    switched,
    strictness_change_frozen,
    segment_id_cohort = as.character(segment_id_cohort),
    distance_to_boundary_ft = dist_m / 0.3048
  )

if (anyDuplicated(blocks$block_id) > 0) {
  stop("Spatial-permutation block sample must be unique by block.", call. = FALSE)
}

segments <- st_read(
  "../input/boundary_segments_1320ft.gpkg",
  layer = "2003_2014",
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  select(segment_id, centroid_lon, centroid_lat) %>%
  st_as_sf(coords = c("centroid_lon", "centroid_lat"), crs = 4326) %>%
  st_transform(3435)

segment_coordinates <- st_coordinates(segments)
segments <- segments %>%
  st_drop_geometry() %>%
  transmute(
    segment_id_cohort = as.character(segment_id),
    segment_x = segment_coordinates[, "X"],
    segment_y = segment_coordinates[, "Y"]
  )

if (anyDuplicated(segments$segment_id_cohort) > 0) {
  stop("Boundary segments must be unique by segment ID.", call. = FALSE)
}

blocks <- blocks %>%
  left_join(segments, by = "segment_id_cohort", relationship = "many-to-one")

if (anyNA(blocks[c("segment_x", "segment_y")])) {
  stop("Every analysis block must match a boundary segment.", call. = FALSE)
}

match_blocks <- function(match_caliper_ft) {
  matched_edges <- list()
  strata <- blocks %>%
    distinct(ward_pair_id, ward_origin)

  for (stratum_i in seq_len(nrow(strata))) {
    treated_blocks <- blocks %>%
      filter(
        ward_pair_id == strata$ward_pair_id[stratum_i],
        ward_origin == strata$ward_origin[stratum_i],
        switched
      ) %>%
      transmute(
        treated_block_id = block_id,
        score_gap = strictness_change_frozen,
        treated_x = segment_x,
        treated_y = segment_y,
        treated_boundary_distance_ft = distance_to_boundary_ft
      )

    control_blocks <- blocks %>%
      filter(
        ward_pair_id == strata$ward_pair_id[stratum_i],
        ward_origin == strata$ward_origin[stratum_i],
        !switched
      ) %>%
      transmute(
        control_block_id = block_id,
        control_x = segment_x,
        control_y = segment_y,
        control_boundary_distance_ft = distance_to_boundary_ft
      )

    if (nrow(treated_blocks) == 0 || nrow(control_blocks) == 0) {
      next
    }

    candidate_edges <- merge(treated_blocks, control_blocks, by = NULL) %>%
      mutate(
        along_boundary_distance_ft = sqrt(
          (treated_x - control_x)^2 + (treated_y - control_y)^2
        ),
        boundary_distance_gap_ft = abs(
          treated_boundary_distance_ft - control_boundary_distance_ft
        ),
        match_distance_ft = sqrt(
          along_boundary_distance_ft^2 + boundary_distance_gap_ft^2
        )
      ) %>%
      filter(
        along_boundary_distance_ft <= match_caliper_ft,
        boundary_distance_gap_ft <= boundary_distance_caliper_ft
      ) %>%
      arrange(match_distance_ft)

    if (nrow(candidate_edges) == 0) {
      next
    }

    used_treated_blocks <- character()
    used_control_blocks <- character()
    for (edge_i in seq_len(nrow(candidate_edges))) {
      if (
        candidate_edges$treated_block_id[edge_i] %in% used_treated_blocks ||
          candidate_edges$control_block_id[edge_i] %in% used_control_blocks
      ) {
        next
      }
      matched_edges[[length(matched_edges) + 1L]] <- candidate_edges[edge_i, ]
      used_treated_blocks <- c(
        used_treated_blocks,
        candidate_edges$treated_block_id[edge_i]
      )
      used_control_blocks <- c(
        used_control_blocks,
        candidate_edges$control_block_id[edge_i]
      )
    }
  }

  matches <- bind_rows(matched_edges) %>%
    mutate(match_id = row_number())

  if (nrow(matches) == 0) {
    stop("No treated-control matches satisfy the requested calipers.", call. = FALSE)
  }
  if (
    anyDuplicated(matches$treated_block_id) > 0 ||
      anyDuplicated(matches$control_block_id) > 0
  ) {
    stop("Spatial matches must be one-to-one without replacement.", call. = FALSE)
  }
  matches
}

set.seed(seed)
permutation_results <- vector("list", length(match_calipers_ft))

for (caliper_i in seq_along(match_calipers_ft)) {
  match_caliper_ft <- match_calipers_ft[caliper_i]
  matches <- match_blocks(match_caliper_ft)

  assignments <- bind_rows(
    matches %>%
      transmute(
        match_id,
        block_id = treated_block_id,
        score_gap,
        observed_change = score_gap,
        actual_switcher = 1L
      ),
    matches %>%
      transmute(
        match_id,
        block_id = control_block_id,
        score_gap,
        observed_change = 0,
        actual_switcher = 0L
      )
  ) %>%
    arrange(match_id, desc(actual_switcher))

  if (anyDuplicated(assignments$block_id) > 0) {
    stop("A block cannot appear in more than one matched pair.", call. = FALSE)
  }

  model_data <- panel %>%
    filter(relative_year >= -5, relative_year <= 5) %>%
    semi_join(assignments %>% select(block_id), by = "block_id") %>%
    mutate(outcome = n_high_discretion_application)

  pre_period_controls <- model_data %>%
    filter(relative_year < 0) %>%
    group_by(block_id) %>%
    summarise(
      pre_period_permit_volume = sum(outcome),
      .groups = "drop"
    ) %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

  model_data <- model_data %>%
    left_join(
      pre_period_controls,
      by = "block_id",
      relationship = "many-to-one"
    ) %>%
    mutate(
      assignment_row = match(block_id, assignments$block_id),
      post = as.integer(relative_year >= 0),
      post_treat = post * assignments$observed_change[assignment_row]
    )

  observed_model <- fepois(
    outcome ~ post_treat +
      pre_period_permit_volume:factor(year) +
      no_pre_period_permits:factor(year) |
      block_id + ward_pair_id^year,
    data = model_data,
    cluster = ~ward_pair_id,
    notes = FALSE,
    warn = FALSE
  )

  observed_estimate <- coef(observed_model)[["post_treat"]]
  observed_standard_error <- se(observed_model)[["post_treat"]]
  observed_p_value <- pvalue(observed_model)[["post_treat"]]
  permuted_estimates <- numeric(permutations)

  for (permutation_i in seq_len(permutations)) {
    pseudo_treated_member <- rbinom(nrow(matches), 1L, 0.5)
    permuted_change <- if_else(
      assignments$actual_switcher == 1L,
      assignments$score_gap * (1 - pseudo_treated_member[assignments$match_id]),
      assignments$score_gap * pseudo_treated_member[assignments$match_id]
    )
    model_data$post_treat <- model_data$post * permuted_change[model_data$assignment_row]

    permuted_estimates[permutation_i] <- fepois(
      outcome ~ post_treat +
        pre_period_permit_volume:factor(year) +
        no_pre_period_permits:factor(year) |
        block_id + ward_pair_id^year,
      data = model_data,
      notes = FALSE,
      warn = FALSE,
      only.coef = TRUE
    )[["post_treat"]]
  }

  two_sided_permutation_p <- (
    1 + sum(abs(permuted_estimates) >= abs(observed_estimate))
  ) / (permutations + 1)
  lower_tail_permutation_p <- (
    1 + sum(permuted_estimates <= observed_estimate)
  ) / (permutations + 1)

  caliper_label <- if (match_caliper_ft == 2640) {
    "Half-mile match caliper"
  } else if (match_caliper_ft == 5280) {
    "One-mile match caliper"
  } else {
    sprintf("%s-foot match caliper", format(match_caliper_ft, trim = TRUE))
  }

  permutation_results[[caliper_i]] <- tibble(
    match_caliper_ft,
    caliper_label = sprintf(
      "%s: observed = %.3f (SE %.3f); permutation p = %.3f",
      caliper_label,
      observed_estimate,
      observed_standard_error,
      two_sided_permutation_p
    ),
    estimate = permuted_estimates,
    observed_estimate,
    observed_standard_error,
    observed_p_value,
    two_sided_permutation_p,
    lower_tail_permutation_p,
    n_matches = nrow(matches),
    n_blocks = 2L * nrow(matches),
    n_ward_pairs = n_distinct(model_data$ward_pair_id),
    median_along_boundary_distance_ft = median(matches$along_boundary_distance_ft),
    p90_along_boundary_distance_ft = quantile(
      matches$along_boundary_distance_ft,
      0.90,
      names = FALSE
    ),
    median_boundary_distance_gap_ft = median(matches$boundary_distance_gap_ft)
  )

  message(sprintf(
    paste0(
      "%s ft: %d matches across %d ward pairs; observed %.3f (SE %.3f); ",
      "two-sided permutation p %.3f; lower-tail p %.3f."
    ),
    format(match_caliper_ft, trim = TRUE),
    nrow(matches),
    n_distinct(model_data$ward_pair_id),
    observed_estimate,
    observed_standard_error,
    two_sided_permutation_p,
    lower_tail_permutation_p
  ))
}

permutation_results <- bind_rows(permutation_results)

plot <- ggplot(permutation_results, aes(estimate)) +
  geom_histogram(bins = 35, fill = "#8DB6C7", color = "white", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = 0.4) +
  geom_vline(
    aes(xintercept = observed_estimate),
    color = "#A33A2B",
    linewidth = 0.9
  ) +
  facet_wrap(~caliper_label, ncol = 1, scales = "free_y") +
  labs(
    title = "Spatially matched conditional permutation",
    subtitle = paste0(
      "High-discretion permit applications; ",
      bandwidth_label,
      " sample; 100-foot boundary-distance caliper; ",
      format(permutations, big.mark = ","),
      " permutations"
    ),
    x = "Pooled post-period coefficient on assigned stringency",
    y = "Permutation draws",
    caption = paste0(
      "Within each origin ward and ward pair, real switchers are matched without replacement to nearby stable blocks.\n",
      "The frozen score gap is randomly assigned within each matched pair."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(hjust = 0)
  )

ggsave(
  sprintf(
    paste0(
      "../output/permit_itt_spatial_permutation_%s_match%s_dist%d_B%d.pdf"
    ),
    bandwidth_label,
    match_label,
    as.integer(boundary_distance_caliper_ft),
    permutations
  ),
  plot,
  width = 8.2,
  height = 7.0,
  bg = "white"
)
