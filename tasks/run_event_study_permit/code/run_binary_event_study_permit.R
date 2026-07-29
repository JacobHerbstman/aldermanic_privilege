# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# outcome_family <- "high_discretion"
# sample_rule <- "stable"
# direction_rule <- "signed"
# bandwidth_m <- 152.4
# bandwidth_label <- "500ft"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0L) {
  cli_args <- c(
    outcome_family,
    sample_rule,
    direction_rule,
    bandwidth_m,
    bandwidth_label
  )
}
if (length(cli_args) != 5L) {
  stop(
    "Expected outcome, sample rule, direction rule, bandwidth, and label.",
    call. = FALSE
  )
}

outcome_family <- cli_args[1]
sample_rule <- cli_args[2]
direction_rule <- cli_args[3]
bandwidth_m <- as.numeric(cli_args[4])
bandwidth_label <- cli_args[5]

if (
  !outcome_family %in% c(
    "high_discretion",
    "low_discretion_nosigns"
  ) ||
    !sample_rule %in% c("stable", "all") ||
    !direction_rule %in% c("signed", "separate") ||
    !is.finite(bandwidth_m) ||
    bandwidth_m <= 0 ||
    !grepl("^[A-Za-z0-9_-]+$", bandwidth_label)
) {
  stop("Invalid event-study argument.", call. = FALSE)
}

outcome_variable <- if (outcome_family == "high_discretion") {
  "n_high_discretion_application"
} else {
  "n_low_discretion_nosigns_application"
}

data <- arrow::read_parquet(
  "../input/permit_block_year_panel_2015.parquet"
) |>
  dplyr::filter(
    dist_m <= bandwidth_m,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) |>
  dplyr::mutate(
    outcome = .data[[outcome_variable]],
    post = as.integer(relative_year >= 0L),
    stricter = as.integer(strictness_change_frozen > 0),
    lenient = as.integer(strictness_change_frozen < 0),
    signed_direction = stricter - lenient
  )

if (sample_rule == "stable") {
  data <- data |>
    dplyr::filter(stable_both)
}

if (anyDuplicated(data[c("block_id", "year")])) {
  stop("Event-study data must be unique by block and year.", call. = FALSE)
}
if (
  any(is.na(data$outcome)) ||
    any(data$outcome < 0) ||
    any(data$stricter + data$lenient > 1L)
) {
  stop("Event-study data failed validation.", call. = FALSE)
}

pre_period_controls <- data |>
  dplyr::filter(relative_year < 0L) |>
  dplyr::summarise(
    pre_period_permit_volume = sum(
      n_high_discretion_application,
      na.rm = TRUE
    ),
    .by = block_id
  ) |>
  dplyr::mutate(
    no_pre_period_permits = as.integer(pre_period_permit_volume == 0)
  )

if (anyDuplicated(pre_period_controls$block_id)) {
  stop("Pre-period controls must be unique by block.", call. = FALSE)
}

data <- data |>
  dplyr::left_join(
    pre_period_controls,
    by = "block_id",
    relationship = "many-to-one"
  )

event_times <- -5L:5L
pre_periods <- -5L:-2L

if (direction_rule == "signed") {
  event_model <- fixest::fepois(
    outcome ~
      i(relative_year, signed_direction, ref = -1) +
      pre_period_permit_volume:factor(year) +
      post:no_pre_period_permits |
      block_id + ward_pair_id^year,
    data = data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  pooled_data <- data |>
    dplyr::mutate(post_treatment = post * signed_direction)
  pooled_model <- fixest::fepois(
    outcome ~
      post_treatment +
      pre_period_permit_volume:factor(year) +
      post:no_pre_period_permits |
      block_id + ward_pair_id^year,
    data = pooled_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  event_coefficients <- stats::coef(event_model)
  event_vcov <- stats::vcov(event_model)
  event_df <- fixest::degrees_freedom(event_model, type = "t")
  critical_value <- stats::qt(0.975, df = event_df)

  event_rows <- lapply(event_times, function(event_time) {
    if (event_time == -1L) {
      return(tibble::tibble(event_time, estimate_log = 0, se = 0))
    }
    coefficient_name <- paste0(
      "relative_year::",
      event_time,
      ":signed_direction"
    )
    tibble::tibble(
      event_time,
      estimate_log = unname(event_coefficients[coefficient_name]),
      se = sqrt(event_vcov[coefficient_name, coefficient_name])
    )
  })
  event_results <- dplyr::bind_rows(event_rows)

  pretrend_matrix <- matrix(
    0,
    nrow = length(pre_periods),
    ncol = length(event_coefficients),
    dimnames = list(NULL, names(event_coefficients))
  )
  for (i in seq_along(pre_periods)) {
    pretrend_matrix[
      i,
      paste0("relative_year::", pre_periods[i], ":signed_direction")
    ] <- 1
  }
} else {
  event_model <- fixest::fepois(
    outcome ~
      i(relative_year, stricter, ref = -1) +
      i(relative_year, lenient, ref = -1) +
      pre_period_permit_volume:factor(year) +
      post:no_pre_period_permits |
      block_id + ward_pair_id^year,
    data = data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  pooled_data <- data |>
    dplyr::mutate(
      post_stricter = post * stricter,
      post_lenient = post * lenient
    )
  pooled_model <- fixest::fepois(
    outcome ~
      post_stricter +
      post_lenient +
      pre_period_permit_volume:factor(year) +
      post:no_pre_period_permits |
      block_id + ward_pair_id^year,
    data = pooled_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  event_coefficients <- stats::coef(event_model)
  event_vcov <- stats::vcov(event_model)
  event_df <- fixest::degrees_freedom(event_model, type = "t")
  critical_value <- stats::qt(0.975, df = event_df)

  event_rows <- lapply(event_times, function(event_time) {
    if (event_time == -1L) {
      return(tibble::tibble(event_time, estimate_log = 0, se = 0))
    }
    stricter_name <- paste0(
      "relative_year::",
      event_time,
      ":stricter"
    )
    lenient_name <- paste0(
      "relative_year::",
      event_time,
      ":lenient"
    )
    estimate_log <- (
      event_coefficients[stricter_name] -
        event_coefficients[lenient_name]
    ) / 2
    variance <- (
      event_vcov[stricter_name, stricter_name] +
        event_vcov[lenient_name, lenient_name] -
        2 * event_vcov[stricter_name, lenient_name]
    ) / 4
    tibble::tibble(
      event_time,
      estimate_log = unname(estimate_log),
      se = sqrt(variance)
    )
  })
  event_results <- dplyr::bind_rows(event_rows)

  pretrend_matrix <- matrix(
    0,
    nrow = length(pre_periods),
    ncol = length(event_coefficients),
    dimnames = list(NULL, names(event_coefficients))
  )
  for (i in seq_along(pre_periods)) {
    pretrend_matrix[
      i,
      paste0("relative_year::", pre_periods[i], ":stricter")
    ] <- 0.5
    pretrend_matrix[
      i,
      paste0("relative_year::", pre_periods[i], ":lenient")
    ] <- -0.5
  }
}

pretrend_coefficients <- drop(pretrend_matrix %*% event_coefficients)
pretrend_vcov <- pretrend_matrix %*%
  event_vcov %*%
  t(pretrend_matrix)
pretrend_f <- drop(
  t(pretrend_coefficients) %*%
    solve(pretrend_vcov, pretrend_coefficients)
) / length(pre_periods)
pretrend_p_value <- stats::pf(
  pretrend_f,
  df1 = length(pre_periods),
  df2 = event_df,
  lower.tail = FALSE
)

if (direction_rule == "signed") {
  pooled_estimate <- stats::coef(pooled_model)[["post_treatment"]]
  pooled_se <- sqrt(
    stats::vcov(pooled_model)["post_treatment", "post_treatment"]
  )
} else {
  pooled_coefficients <- stats::coef(pooled_model)
  pooled_vcov <- stats::vcov(pooled_model)
  pooled_estimate <- (
    pooled_coefficients["post_stricter"] -
      pooled_coefficients["post_lenient"]
  ) / 2
  pooled_se <- sqrt(
    (
      pooled_vcov["post_stricter", "post_stricter"] +
        pooled_vcov["post_lenient", "post_lenient"] -
        2 * pooled_vcov["post_stricter", "post_lenient"]
    ) / 4
  )
}

pooled_df <- fixest::degrees_freedom(pooled_model, type = "t")
pooled_p_value <- 2 * stats::pt(
  -abs(pooled_estimate / pooled_se),
  df = pooled_df
)
pooled_stars <- dplyr::case_when(
  pooled_p_value <= 0.01 ~ "***",
  pooled_p_value <= 0.05 ~ "**",
  pooled_p_value <= 0.10 ~ "*",
  TRUE ~ ""
)

event_results <- event_results |>
  dplyr::mutate(
    estimate = expm1(estimate_log),
    ci_low = expm1(estimate_log - critical_value * se),
    ci_high = expm1(estimate_log + critical_value * se)
  )

outcome_label <- dplyr::if_else(
  outcome_family == "high_discretion",
  "High-discretion permits by application year",
  "Low-discretion permits (excluding signs) by application year"
)
subtitle_label <- dplyr::if_else(
  direction_rule == "signed",
  "Pooled estimate",
  "Pooled directional contrast"
)

plot <- ggplot2::ggplot(
  event_results,
  ggplot2::aes(event_time, estimate)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = "gray50",
    linewidth = 0.4
  ) +
  ggplot2::geom_vline(
    xintercept = -0.5,
    linetype = "dashed",
    color = "gray60",
    linewidth = 0.4
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = ci_low, ymax = ci_high),
    fill = "#176B58",
    alpha = 0.18
  ) +
  ggplot2::geom_line(color = "#176B58", linewidth = 0.85) +
  ggplot2::geom_point(color = "#176B58", size = 2) +
  ggplot2::scale_x_continuous(breaks = event_times) +
  ggplot2::labs(
    title = outcome_label,
    subtitle = sprintf(
      "%s = %.3f%s (SE %.3f)",
      subtitle_label,
      pooled_estimate,
      pooled_stars,
      pooled_se
    ),
    x = "Years relative to the 2015 ward remap",
    y = "Effect of assignment toward greater stringency"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    axis.title.x = ggplot2::element_text(
      hjust = 0.5,
      margin = ggplot2::margin(t = 8)
    )
  )

ggplot2::ggsave(
  sprintf(
    "../output/permit_event_study_%s_%s_%s_%s.pdf",
    outcome_family,
    sample_rule,
    direction_rule,
    bandwidth_label
  ),
  plot,
  width = 7.6,
  height = 5.5,
  bg = "white"
)
