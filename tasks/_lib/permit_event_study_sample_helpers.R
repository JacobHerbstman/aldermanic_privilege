get_permit_sample_restriction_info <- function(sample_restriction) {
  sample_restriction <- tolower(sample_restriction)

  if (sample_restriction == "none") {
    return(list(
      sample_restriction = "none",
      flag_col = NA_character_,
      suffix_tag = NA_character_,
      label = "No additional sample restriction"
    ))
  }

  if (sample_restriction == "single_pair_bg") {
    return(list(
      sample_restriction = "single_pair_bg",
      flag_col = "keep_single_pair_bg",
      suffix_tag = "singlepairbg",
      label = "Keep only block groups that appear in one ward pair"
    ))
  }

  if (sample_restriction == "unmixed_treat_bg") {
    return(list(
      sample_restriction = "unmixed_treat_bg",
      flag_col = "keep_unmixed_treat_bg",
      suffix_tag = "unmixedtreatbg",
      label = "Keep only block groups that do not mix treated and control blocks"
    ))
  }

  if (sample_restriction == "clean_bg") {
    return(list(
      sample_restriction = "clean_bg",
      flag_col = "keep_clean_bg",
      suffix_tag = "cleanbg",
      label = "Keep only block groups with one ward pair, one treatment status, and one stringency change"
    ))
  }

  stop(
    "--sample_restriction must be one of: none, single_pair_bg, unmixed_treat_bg, clean_bg",
    call. = FALSE
  )
}

apply_permit_bg_sample_restriction <- function(
    df,
    block_var,
    pair_var,
    sample_restriction = "none",
    block_id_var = "block_id",
    cohort_var = "cohort",
    treat_var = "treat",
    change_var = "strictness_change") {
  restriction_info <- get_permit_sample_restriction_info(sample_restriction)
  has_cohort <- cohort_var %in% names(df)

  data_with_bg <- df %>%
    mutate(
      block_group_id = substr(as.character(.data[[block_id_var]]), 1, 12),
      sample_bg_id = if (has_cohort) {
        paste(.data[[cohort_var]], block_group_id, sep = "_")
      } else {
        block_group_id
      }
    )

  block_level_bg <- data_with_bg %>%
    distinct(
      .data[[block_var]],
      .data[[pair_var]],
      .data[[treat_var]],
      .data[[change_var]],
      block_group_id,
      sample_bg_id
    )

  bg_flags <- block_level_bg %>%
    group_by(sample_bg_id, block_group_id) %>%
    summarise(
      n_sample_blocks = n_distinct(.data[[block_var]]),
      n_pairs = n_distinct(.data[[pair_var]]),
      n_treat_status = n_distinct(.data[[treat_var]]),
      n_change_values = n_distinct(.data[[change_var]]),
      keep_single_pair_bg = n_pairs == 1L,
      keep_unmixed_treat_bg = n_treat_status == 1L,
      keep_clean_bg = n_pairs == 1L & n_treat_status == 1L & n_change_values == 1L,
      .groups = "drop"
    )

  if (restriction_info$sample_restriction == "none") {
    return(list(
      data = data_with_bg %>% select(-sample_bg_id),
      summary = tibble(
        sample_restriction = restriction_info$sample_restriction,
        sample_restriction_label = restriction_info$label,
        n_obs_before = nrow(data_with_bg),
        n_obs_after = nrow(data_with_bg),
        n_obs_dropped = 0L,
        n_blocks_before = n_distinct(data_with_bg[[block_var]]),
        n_blocks_after = n_distinct(data_with_bg[[block_var]]),
        n_blocks_dropped = 0L,
        n_bg_groups_before = n_distinct(data_with_bg$sample_bg_id),
        n_bg_groups_after = n_distinct(data_with_bg$sample_bg_id),
        n_bg_groups_dropped = 0L
      ),
      bg_flags = bg_flags
    ))
  }

  filtered_data_full <- data_with_bg %>%
    left_join(
      bg_flags %>% select(sample_bg_id, all_of(restriction_info$flag_col)),
      by = "sample_bg_id"
    ) %>%
    filter(.data[[restriction_info$flag_col]])

  filtered_data <- filtered_data_full %>%
    select(-sample_bg_id, -all_of(restriction_info$flag_col))

  list(
    data = filtered_data,
    summary = tibble(
      sample_restriction = restriction_info$sample_restriction,
      sample_restriction_label = restriction_info$label,
      n_obs_before = nrow(data_with_bg),
      n_obs_after = nrow(filtered_data),
      n_obs_dropped = nrow(data_with_bg) - nrow(filtered_data),
      n_blocks_before = n_distinct(data_with_bg[[block_var]]),
      n_blocks_after = n_distinct(filtered_data[[block_var]]),
      n_blocks_dropped = n_distinct(data_with_bg[[block_var]]) - n_distinct(filtered_data[[block_var]]),
      n_bg_groups_before = n_distinct(data_with_bg$sample_bg_id),
      n_bg_groups_after = n_distinct(filtered_data_full$sample_bg_id),
      n_bg_groups_dropped = n_distinct(data_with_bg$sample_bg_id) - n_distinct(filtered_data_full$sample_bg_id)
    ),
    bg_flags = bg_flags
  )
}
