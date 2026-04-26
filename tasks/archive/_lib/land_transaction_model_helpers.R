normalize_land_transaction_party_name <- function(x) {
  x <- toupper(dplyr::coalesce(as.character(x), ""))
  stringr::str_replace_all(x, "[^A-Z0-9]", "")
}

get_land_transaction_sample_restriction_info <- function(sample_restriction) {
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
      label = "Keep only block groups that do not mix treated and control parcels"
    ))
  }

  stop(
    "--sample_restriction must be one of: none, single_pair_bg, unmixed_treat_bg",
    call. = FALSE
  )
}

apply_land_transaction_sample_restriction <- function(df, sample_restriction, unit_id_var = "pin10") {
  restriction_info <- get_land_transaction_sample_restriction_info(sample_restriction)

  if (restriction_info$sample_restriction == "none") {
    return(list(
      data = df,
      summary = tibble::tibble(
        sample_restriction = restriction_info$sample_restriction,
        sample_restriction_label = restriction_info$label,
        n_obs_before = nrow(df),
        n_obs_after = nrow(df),
        n_obs_dropped = 0L,
        n_units_before = dplyr::n_distinct(df[[unit_id_var]]),
        n_units_after = dplyr::n_distinct(df[[unit_id_var]]),
        n_units_dropped = 0L
      )
    ))
  }

  if (!restriction_info$flag_col %in% names(df)) {
    stop(
      sprintf("Missing sample restriction flag column: %s", restriction_info$flag_col),
      call. = FALSE
    )
  }

  filtered_df <- df %>%
    dplyr::filter(.data[[restriction_info$flag_col]] %in% TRUE)

  list(
    data = filtered_df,
    summary = tibble::tibble(
      sample_restriction = restriction_info$sample_restriction,
      sample_restriction_label = restriction_info$label,
      n_obs_before = nrow(df),
      n_obs_after = nrow(filtered_df),
      n_obs_dropped = nrow(df) - nrow(filtered_df),
      n_units_before = dplyr::n_distinct(df[[unit_id_var]]),
      n_units_after = dplyr::n_distinct(filtered_df[[unit_id_var]]),
      n_units_dropped = dplyr::n_distinct(df[[unit_id_var]]) - dplyr::n_distinct(filtered_df[[unit_id_var]])
    )
  )
}

prepare_land_transaction_event_time <- function(
    df,
    style = c("yearly", "binned"),
    outcome_family = c("price", "incidence"),
    event_time_var = "relative_year",
    min_period = -5L,
    max_period) {
  style <- match.arg(style)
  outcome_family <- match.arg(outcome_family)
  min_period <- as.integer(min_period)
  max_period <- as.integer(max_period)

  if (style == "yearly") {
    labels <- tibble::tibble(
      event_time_model = seq.int(min_period, max_period),
      event_time_label = as.character(seq.int(min_period, max_period))
    )

    data <- df %>%
      dplyr::mutate(
        event_time_model = as.integer(.data[[event_time_var]])
      ) %>%
      dplyr::left_join(labels, by = "event_time_model")

    return(list(
      data = data,
      labels = labels,
      min_period = min_period,
      max_period = max_period,
      suffix_tag = "yearly"
    ))
  }

  if (outcome_family == "price") {
    labels <- tibble::tibble(
      event_time_model = c(-2L, -1L, 1L, 2L),
      event_time_label = c("<=-2", "-1", "0-1", "2-3")
    )

    data <- df %>%
      dplyr::mutate(
        event_time_model = dplyr::case_when(
          .data[[event_time_var]] <= -2 ~ -2L,
          .data[[event_time_var]] == -1 ~ -1L,
          .data[[event_time_var]] %in% 0:1 ~ 1L,
          .data[[event_time_var]] %in% 2:3 ~ 2L,
          TRUE ~ NA_integer_
        )
      ) %>%
      dplyr::filter(!is.na(event_time_model)) %>%
      dplyr::left_join(labels, by = "event_time_model")

    return(list(
      data = data,
      labels = labels,
      min_period = -2L,
      max_period = 2L,
      suffix_tag = "binned"
    ))
  }

  labels <- tibble::tibble(
    event_time_model = c(-2L, -1L, 1L, 2L, 4L),
    event_time_label = c("<=-2", "-1", "0-1", "2-3", "4-5")
  )

  data <- df %>%
    dplyr::mutate(
      event_time_model = dplyr::case_when(
        .data[[event_time_var]] <= -2 ~ -2L,
        .data[[event_time_var]] == -1 ~ -1L,
        .data[[event_time_var]] %in% 0:1 ~ 1L,
        .data[[event_time_var]] %in% 2:3 ~ 2L,
        .data[[event_time_var]] %in% 4:5 ~ 4L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::filter(!is.na(event_time_model)) %>%
    dplyr::left_join(labels, by = "event_time_model")

  list(
    data = data,
    labels = labels,
    min_period = -2L,
    max_period = 4L,
    suffix_tag = "binned"
  )
}

extract_fixest_event_study_coefficients <- function(
    model,
    support_by_event_time,
    min_period,
    max_period,
    group_label,
    display_mode = c("multiply100", "exp_minus_one", "log_points")) {
  display_mode <- match.arg(display_mode)

  iplot_data <- tryCatch(fixest::iplot(model, .plot = FALSE)[[1]], error = function(e) NULL)
  if (is.null(iplot_data) || nrow(iplot_data) == 0) {
    return(NULL)
  }

  out <- iplot_data %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      event_time = as.integer(x),
      estimate,
      ci_low,
      ci_high,
      std_error = dplyr::if_else(
        is_ref,
        0,
        (ci_high - estimate) / stats::qnorm(0.975)
      ),
      estimate_name = estimate_names,
      estimate_name_raw = estimate_names_raw,
      is_reference = is_ref,
      group = group_label
    ) %>%
    dplyr::filter(event_time >= min_period, event_time <= max_period) %>%
    dplyr::left_join(support_by_event_time, by = "event_time")

  if (display_mode == "exp_minus_one") {
    out <- out %>%
      dplyr::mutate(
        estimate_display = 100 * (exp(estimate) - 1),
        ci_low_display = 100 * (exp(ci_low) - 1),
        ci_high_display = 100 * (exp(ci_high) - 1),
        display_unit = "%"
      )
  } else {
    out <- out %>%
      dplyr::mutate(
        estimate_display = 100 * estimate,
        ci_low_display = 100 * ci_low,
        ci_high_display = 100 * ci_high,
        display_unit = if (display_mode == "log_points") "100_log_points" else "pp"
      )
  }

  out
}

winsorize_numeric_series <- function(x, lower_prob = 0.01, upper_prob = 0.99) {
  x <- as.numeric(x)
  finite_mask <- is.finite(x)

  if (!any(finite_mask)) {
    return(list(
      values = x,
      lower = NA_real_,
      upper = NA_real_,
      n_modified = 0L,
      share_modified = NA_real_
    ))
  }

  bounds <- as.numeric(
    stats::quantile(
      x[finite_mask],
      probs = c(lower_prob, upper_prob),
      names = FALSE,
      na.rm = TRUE
    )
  )

  values <- x
  values[finite_mask] <- pmax(bounds[1], pmin(x[finite_mask], bounds[2]))
  modified_mask <- finite_mask & abs(values - x) > 1e-8

  list(
    values = values,
    lower = bounds[1],
    upper = bounds[2],
    n_modified = sum(modified_mask),
    share_modified = mean(modified_mask[finite_mask])
  )
}
