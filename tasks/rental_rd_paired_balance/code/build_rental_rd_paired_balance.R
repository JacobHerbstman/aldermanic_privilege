# Build paired balance tables from the listed-rent RD characteristics panel.

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_paired_balance/code")
# bandwidth_ft <- 500

source("../../setup_environment/code/packages.R", local = new.env(parent = globalenv()))

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <bandwidth_ft>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))
excluded_balance_covariates <- c("log_sqft", "year_built")
min_cluster_ward_pairs <- 20L

message(sprintf("=== Listed-Rent RD Paired Balance | bandwidth=%sft ===", bandwidth_label))

rent <- read_parquet(sprintf("../input/rental_rd_characteristics_panel_bw%s.parquet", bandwidth_label)) %>%
  as_tibble()

if (!all(c("segment_id", "ward_pair", "signed_dist_ft") %in% names(rent))) {
  stop("Rental characteristics panel must include segment_id, ward_pair, and signed_dist_ft.", call. = FALSE)
}

rent <- rent %>%
  mutate(
    segment_id = as.character(segment_id),
    ward_pair = as.character(ward_pair),
    score_side = case_when(
      signed_dist_ft < 0 ~ "lenient",
      signed_dist_ft > 0 ~ "strict",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != "",
    !is.na(score_side)
  )

sample_defs <- tibble::tribble(
  ~sample, ~sample_label, ~filter_column,
  "all", "All", NA_character_,
  "clean_location", "Clean location", "flag_clean_location_sample",
  "no_modal_pair_change", "No modal pair change", "flag_no_modal_pair_change_sample",
  "no_modal_ward_change", "No modal ward change", "flag_no_modal_ward_change_sample",
  "no_questionable_address", "No questionable address", "flag_no_questionable_address_sample"
)

paired_test <- function(paired_df) {
  if (nrow(paired_df) < 2) {
    return(tibble(se = NA_real_, p_value = NA_real_))
  }
  difference_sd <- sd(paired_df$difference, na.rm = TRUE)
  if (!is.finite(difference_sd) || difference_sd == 0) {
    return(tibble(
      se = 0,
      p_value = ifelse(mean(paired_df$difference, na.rm = TRUE) == 0, 1, 0)
    ))
  }
  if (n_distinct(paired_df$ward_pair) >= min_cluster_ward_pairs) {
    model <- feols(difference ~ 1, data = paired_df, cluster = ~ward_pair, warn = FALSE)
    return(tibble(
      se = unname(se(model)[["(Intercept)"]]),
      p_value = unname(pvalue(model)[["(Intercept)"]])
    ))
  }
  se_i <- difference_sd / sqrt(nrow(paired_df))
  tibble(
    se = se_i,
    p_value = 2 * pt(abs(mean(paired_df$difference, na.rm = TRUE) / se_i), df = nrow(paired_df) - 1, lower.tail = FALSE)
  )
}

covariates <- tibble::tribble(
  ~variable, ~label, ~group, ~digits,
  "beds", "Beds", "Hedonics", 2,
  "baths", "Baths", "Hedonics", 2,
  "sqft", "Sqft", "Hedonics", 0,
  "log_sqft", "Log sqft", "Hedonics", 2,
  "is_multifamily", "Multifamily", "Building type", 3,
  "is_single_family", "Single family", "Building type", 3,
  "is_condo", "Condo", "Building type", 3,
  "is_townhouse", "Townhouse", "Building type", 3,
  "laundry", "Laundry", "Unit amenities", 3,
  "gym", "Gym", "Unit amenities", 3,
  "doorman", "Doorman", "Unit amenities", 3,
  "furnished", "Furnished", "Unit amenities", 3,
  "pool", "Pool", "Unit amenities", 3,
  "year_built", "Year built", "Hedonics", 1,
  "active_days", "Active days in month", "Listing quality", 2,
  "raw_rows_month", "Raw rows in month", "Listing quality", 2,
  "address_missing", "Missing address", "Listing quality", 3,
  "flag_location_questionable", "Questionable location", "Location quality", 3,
  "flag_modal_changes_pair", "Modal pair changes", "Location quality", 3,
  "flag_rd_location_questionable", "RD-location flag", "Location quality", 3,
  "flag_building_type_conflict", "Building-type conflict", "Listing quality", 3,
  "nearest_school_dist_ft", "Dist. to school", "External amenities", 0,
  "nearest_park_dist_ft", "Dist. to park", "External amenities", 0,
  "nearest_major_road_dist_ft", "Dist. to major road", "External amenities", 0,
  "nearest_cta_stop_dist_ft", "Dist. to CTA stop", "External amenities", 0,
  "lake_michigan_dist_ft", "Dist. to Lake Michigan", "External amenities", 0
) %>%
  filter(variable %in% names(rent))

covariate_coverage <- bind_rows(lapply(covariates$variable, function(variable) {
  tibble(
    variable = variable,
    all_sample_coverage = mean(is.finite(as.numeric(rent[[variable]])))
  )
}))

covariates <- covariates %>%
  left_join(covariate_coverage, by = "variable", relationship = "one-to-one")

dropped_covariates <- covariates %>%
  filter(variable %in% excluded_balance_covariates)

if (nrow(dropped_covariates) > 0) {
  message(sprintf(
    "Dropping excluded balance covariates: %s",
    paste(dropped_covariates$label, collapse = ", ")
  ))
}

covariates <- covariates %>%
  filter(!variable %in% excluded_balance_covariates)

if (nrow(covariates) == 0) {
  stop("No rental covariates remain after applying balance-table exclusions.", call. = FALSE)
}

paired_balance_rows <- list()
for (i in seq_len(nrow(sample_defs))) {
  sample_name <- sample_defs$sample[[i]]
  sample_label <- sample_defs$sample_label[[i]]
  filter_column <- sample_defs$filter_column[[i]]
  d_sample <- rent
  if (!is.na(filter_column)) {
    if (!filter_column %in% names(d_sample)) {
      stop(sprintf("Missing sample flag column: %s", filter_column), call. = FALSE)
    }
    d_sample <- d_sample %>% filter(.data[[filter_column]])
  }
  message(sprintf("Paired balance sample %s: %s rows", sample_name, format(nrow(d_sample), big.mark = ",")))

  for (j in seq_len(nrow(covariates))) {
    variable <- covariates$variable[[j]]
    side_means <- d_sample %>%
      mutate(balance_value = as.numeric(.data[[variable]])) %>%
      filter(is.finite(balance_value)) %>%
      group_by(ward_pair, segment_id, score_side) %>%
      summarise(
        side_mean = mean(balance_value),
        side_n = n(),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from = score_side,
        values_from = c(side_mean, side_n)
      )

    if (!all(c("side_mean_lenient", "side_mean_strict") %in% names(side_means))) {
      next
    }

    paired <- side_means %>%
      filter(is.finite(side_mean_lenient), is.finite(side_mean_strict)) %>%
      mutate(difference = side_mean_strict - side_mean_lenient)

    if (nrow(paired) == 0) {
      next
    }

    pooled_sd <- sqrt((var(paired$side_mean_lenient) + var(paired$side_mean_strict)) / 2)
    paired_test_result <- paired_test(paired)

    paired_balance_rows[[length(paired_balance_rows) + 1]] <- tibble(
      sample = sample_name,
      sample_label = sample_label,
      variable = variable,
      label = covariates$label[[j]],
      group = covariates$group[[j]],
      lenient_mean = mean(paired$side_mean_lenient),
      strict_mean = mean(paired$side_mean_strict),
      difference = mean(paired$difference),
      se = paired_test_result$se[[1]],
      p_value = paired_test_result$p_value[[1]],
      normalized_difference = ifelse(is.finite(pooled_sd) && pooled_sd > 0, mean(paired$difference) / pooled_sd, NA_real_),
      n_segments = nrow(paired),
      n_ward_pairs = n_distinct(paired$ward_pair),
      n_lenient_obs = sum(paired$side_n_lenient),
      n_strict_obs = sum(paired$side_n_strict),
      all_sample_coverage = covariates$all_sample_coverage[[j]],
      bandwidth_ft = bandwidth_ft,
      digits = covariates$digits[[j]]
    )
  }
}

paired_balance <- bind_rows(paired_balance_rows)
write_csv(paired_balance, sprintf("../temp/rental_rd_paired_covariate_balance_bw%s.csv", bandwidth_label))
message("Saved listed-rent RD paired balance data.")
