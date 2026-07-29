# setwd("tasks/audits/permit_event_study_audit/code")
# bandwidth_m <- 152.4
# bandwidth_label <- "500ft"
# score_variant <- "all_covariates"
# outcome_name <- "high_discretion"
# sample_rule <- "all"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    bandwidth_m,
    bandwidth_label,
    score_variant,
    outcome_name,
    sample_rule
  )
}
if (!length(cli_args) %in% c(4L, 5L)) {
  stop(
    paste(
      "Script requires a bandwidth, bandwidth label,",
      "score variant, outcome, and optional sample rule."
    ),
    call. = FALSE
  )
}

bandwidth_m <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]
score_variant <- cli_args[3]
outcome_name <- cli_args[4]
sample_rule <- if (length(cli_args) == 5L) cli_args[5] else "all"
valid_score_variants <- c(
  "current_no_income",
  "education_added_back",
  "income_added_back",
  "all_covariates"
)
valid_outcomes <- c(
  "high_discretion",
  "low_discretion_nosigns"
)
if (
  !is.finite(bandwidth_m) ||
    bandwidth_m <= 0 ||
    !grepl("^[A-Za-z0-9_-]+$", bandwidth_label) ||
    !score_variant %in% valid_score_variants ||
    !outcome_name %in% valid_outcomes ||
    !sample_rule %in% c("all", "stable")
) {
  stop(
    "Invalid bandwidth, score variant, outcome, or sample rule.",
    call. = FALSE
  )
}

scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    cutoff == 2014L,
    variant == score_variant
  ) |>
  dplyr::select(alderman, score)

if (
  nrow(scores) == 0L ||
    anyDuplicated(scores$alderman) ||
    any(is.na(scores$alderman)) ||
    any(is.na(scores$score))
) {
  stop("The selected score crosswalk failed validation.", call. = FALSE)
}

data <- arrow::read_parquet(
  "../input/permit_block_year_panel_2015.parquet"
) |>
  dplyr::filter(
    dist_m <= bandwidth_m,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) |>
  dplyr::left_join(
    scores |>
      dplyr::rename(
        alderman_origin_2014 = alderman,
        score_origin = score
      ),
    by = "alderman_origin_2014",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    scores |>
      dplyr::rename(
        alderman_dest_2014 = alderman,
        score_dest = score
      ),
    by = "alderman_dest_2014",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    score_change = score_dest - score_origin,
    outcome = dplyr::case_when(
      outcome_name == "high_discretion" ~
        n_high_discretion_application,
      outcome_name == "low_discretion_nosigns" ~
        n_low_discretion_nosigns_application
    ),
    post = as.integer(relative_year >= 0L),
    stricter = as.integer(score_change > 0),
    lenient = as.integer(score_change < 0),
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
  any(is.na(data$score_origin)) ||
    any(is.na(data$score_dest)) ||
    any(is.na(data$outcome)) ||
    any(data$outcome < 0) ||
  any(data$stricter + data$lenient > 1L) ||
    any(!data$signed_direction %in% c(-1L, 0L, 1L))
) {
  stop("The event-study data failed validation.", call. = FALSE)
}
if (
  score_variant == "all_covariates" &&
    max(
      abs(data$score_change - data$strictness_change_frozen),
      na.rm = TRUE
    ) > 1e-8
) {
  stop("The all-covariate scores do not reproduce the frozen score.", call. = FALSE)
}

pre_period_controls <- data |>
  dplyr::filter(relative_year < 0L) |>
  dplyr::group_by(block_id) |>
  dplyr::summarise(
    pre_period_permit_volume = sum(
      n_high_discretion_application,
      na.rm = TRUE
    ),
    .groups = "drop"
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

joint_event_model <- fixest::fepois(
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

signed_event_model <- fixest::fepois(
  outcome ~
    i(relative_year, signed_direction, ref = -1) +
    pre_period_permit_volume:factor(year) +
    post:no_pre_period_permits |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

joint_coefficients <- stats::coef(joint_event_model)
joint_vcov <- stats::vcov(joint_event_model)
signed_coefficients <- stats::coef(signed_event_model)
signed_vcov <- stats::vcov(signed_event_model)
joint_df <- fixest::degrees_freedom(joint_event_model, type = "t")
signed_df <- fixest::degrees_freedom(signed_event_model, type = "t")
joint_critical_value <- stats::qt(0.975, df = joint_df)
signed_critical_value <- stats::qt(0.975, df = signed_df)

event_times <- c(-5L, -4L, -3L, -2L, -1L, 0L, 1L, 2L, 3L, 4L, 5L)
joint_rows <- vector("list", length(event_times))
signed_rows <- vector("list", length(event_times))

for (i in seq_along(event_times)) {
  event_time <- event_times[i]
  if (event_time == -1L) {
    joint_rows[[i]] <- tibble::tibble(
      event_time,
      stricter_log = 0,
      stricter_se = 0,
      lenient_log = 0,
      lenient_se = 0,
      aggregate_log = 0,
      aggregate_se = 0,
      aggregate_p_value = NA_real_,
      symmetry_sum = 0,
      symmetry_se = 0,
      symmetry_p_value = NA_real_,
      is_reference = TRUE
    )
    signed_rows[[i]] <- tibble::tibble(
      event_time,
      estimate_log = 0,
      se = 0,
      p_value = NA_real_,
      is_reference = TRUE
    )
    next
  }

  stricter_name <- paste0("relative_year::", event_time, ":stricter")
  lenient_name <- paste0("relative_year::", event_time, ":lenient")
  signed_name <- paste0(
    "relative_year::",
    event_time,
    ":signed_direction"
  )
  if (
    !all(c(stricter_name, lenient_name) %in% names(joint_coefficients)) ||
      !signed_name %in% names(signed_coefficients)
  ) {
    stop("An event-time coefficient is missing.", call. = FALSE)
  }

  stricter_log <- unname(joint_coefficients[stricter_name])
  lenient_log <- unname(joint_coefficients[lenient_name])
  stricter_variance <- joint_vcov[stricter_name, stricter_name]
  lenient_variance <- joint_vcov[lenient_name, lenient_name]
  covariance <- joint_vcov[stricter_name, lenient_name]
  aggregate_log <- (stricter_log - lenient_log) / 2
  aggregate_se <- sqrt(
    (stricter_variance + lenient_variance - 2 * covariance) / 4
  )
  symmetry_sum <- stricter_log + lenient_log
  symmetry_se <- sqrt(
    stricter_variance + lenient_variance + 2 * covariance
  )
  signed_log <- unname(signed_coefficients[signed_name])
  signed_se <- sqrt(signed_vcov[signed_name, signed_name])

  joint_rows[[i]] <- tibble::tibble(
    event_time,
    stricter_log,
    stricter_se = sqrt(stricter_variance),
    lenient_log,
    lenient_se = sqrt(lenient_variance),
    aggregate_log,
    aggregate_se,
    aggregate_p_value = 2 * stats::pt(
      -abs(aggregate_log / aggregate_se),
      df = joint_df
    ),
    symmetry_sum,
    symmetry_se,
    symmetry_p_value = 2 * stats::pt(
      -abs(symmetry_sum / symmetry_se),
      df = joint_df
    ),
    is_reference = FALSE
  )
  signed_rows[[i]] <- tibble::tibble(
    event_time,
    estimate_log = signed_log,
    se = signed_se,
    p_value = 2 * stats::pt(
      -abs(signed_log / signed_se),
      df = signed_df
    ),
    is_reference = FALSE
  )
}

joint_event <- dplyr::bind_rows(joint_rows) |>
  dplyr::mutate(
    stricter = expm1(stricter_log),
    stricter_ci_low = expm1(
      stricter_log - joint_critical_value * stricter_se
    ),
    stricter_ci_high = expm1(
      stricter_log + joint_critical_value * stricter_se
    ),
    lenient = expm1(lenient_log),
    lenient_ci_low = expm1(
      lenient_log - joint_critical_value * lenient_se
    ),
    lenient_ci_high = expm1(
      lenient_log + joint_critical_value * lenient_se
    ),
    aggregate = expm1(aggregate_log),
    aggregate_ci_low = expm1(
      aggregate_log - joint_critical_value * aggregate_se
    ),
    aggregate_ci_high = expm1(
      aggregate_log + joint_critical_value * aggregate_se
    )
  )

signed_event <- dplyr::bind_rows(signed_rows) |>
  dplyr::mutate(
    estimate = expm1(estimate_log),
    ci_low = expm1(estimate_log - signed_critical_value * se),
    ci_high = expm1(estimate_log + signed_critical_value * se)
  )

pre_periods <- c(-5L, -4L, -3L, -2L)
joint_pretrend_matrix <- matrix(
  0,
  nrow = length(pre_periods),
  ncol = length(joint_coefficients),
  dimnames = list(NULL, names(joint_coefficients))
)
signed_pretrend_matrix <- matrix(
  0,
  nrow = length(pre_periods),
  ncol = length(signed_coefficients),
  dimnames = list(NULL, names(signed_coefficients))
)
for (i in seq_along(pre_periods)) {
  joint_pretrend_matrix[
    i,
    paste0("relative_year::", pre_periods[i], ":stricter")
  ] <- 0.5
  joint_pretrend_matrix[
    i,
    paste0("relative_year::", pre_periods[i], ":lenient")
  ] <- -0.5
  signed_pretrend_matrix[
    i,
    paste0("relative_year::", pre_periods[i], ":signed_direction")
  ] <- 1
}

joint_pretrend_coefficients <- drop(
  joint_pretrend_matrix %*% joint_coefficients
)
joint_pretrend_vcov <- joint_pretrend_matrix %*%
  joint_vcov %*%
  t(joint_pretrend_matrix)
signed_pretrend_coefficients <- drop(
  signed_pretrend_matrix %*% signed_coefficients
)
signed_pretrend_vcov <- signed_pretrend_matrix %*%
  signed_vcov %*%
  t(signed_pretrend_matrix)
joint_pretrend_f <- drop(
  t(joint_pretrend_coefficients) %*%
    solve(joint_pretrend_vcov, joint_pretrend_coefficients)
) / length(pre_periods)
signed_pretrend_f <- drop(
  t(signed_pretrend_coefficients) %*%
    solve(signed_pretrend_vcov, signed_pretrend_coefficients)
) / length(pre_periods)
joint_pretrend_p_value <- stats::pf(
  joint_pretrend_f,
  df1 = length(pre_periods),
  df2 = joint_df,
  lower.tail = FALSE
)
signed_pretrend_p_value <- stats::pf(
  signed_pretrend_f,
  df1 = length(pre_periods),
  df2 = signed_df,
  lower.tail = FALSE
)

pooled_data <- data |>
  dplyr::mutate(
    post_stricter = post * stricter,
    post_lenient = post * lenient,
    post_signed = post * signed_direction
  )

joint_pooled_model <- fixest::fepois(
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

signed_pooled_model <- fixest::fepois(
  outcome ~
    post_signed +
    pre_period_permit_volume:factor(year) +
    post:no_pre_period_permits |
    block_id + ward_pair_id^year,
  data = pooled_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

joint_pooled_coefficients <- stats::coef(joint_pooled_model)
joint_pooled_vcov <- stats::vcov(joint_pooled_model)
signed_pooled_coefficients <- stats::coef(signed_pooled_model)
signed_pooled_vcov <- stats::vcov(signed_pooled_model)
joint_pooled_df <- fixest::degrees_freedom(
  joint_pooled_model,
  type = "t"
)
signed_pooled_df <- fixest::degrees_freedom(
  signed_pooled_model,
  type = "t"
)

stricter_log <- unname(joint_pooled_coefficients["post_stricter"])
lenient_log <- unname(joint_pooled_coefficients["post_lenient"])
stricter_variance <- joint_pooled_vcov[
  "post_stricter",
  "post_stricter"
]
lenient_variance <- joint_pooled_vcov["post_lenient", "post_lenient"]
covariance <- joint_pooled_vcov["post_stricter", "post_lenient"]
aggregate_log <- (stricter_log - lenient_log) / 2
aggregate_se <- sqrt(
  (stricter_variance + lenient_variance - 2 * covariance) / 4
)
symmetry_sum <- stricter_log + lenient_log
symmetry_se <- sqrt(
  stricter_variance + lenient_variance + 2 * covariance
)
signed_log <- unname(signed_pooled_coefficients["post_signed"])
signed_se <- sqrt(signed_pooled_vcov["post_signed", "post_signed"])

block_counts <- data |>
  dplyr::distinct(block_id, stricter, lenient) |>
  dplyr::summarise(
    control_blocks = sum(stricter == 0L & lenient == 0L),
    stricter_blocks = sum(stricter == 1L),
    lenient_blocks = sum(lenient == 1L)
  )

pooled <- tibble::tibble(
  specification = c(
    "joint_stricter_vs_unchanged",
    "joint_lenient_vs_unchanged",
    "joint_aggregate_contrast",
    "signed_binary_constrained"
  ),
  estimate_log = c(
    stricter_log,
    lenient_log,
    aggregate_log,
    signed_log
  ),
  se = c(
    sqrt(stricter_variance),
    sqrt(lenient_variance),
    aggregate_se,
    signed_se
  ),
  degrees_freedom = c(
    joint_pooled_df,
    joint_pooled_df,
    joint_pooled_df,
    signed_pooled_df
  )
) |>
  dplyr::mutate(
    outcome = outcome_name,
    score_variant = score_variant,
    p_value = 2 * stats::pt(
      -abs(estimate_log / se),
      df = degrees_freedom
    ),
    estimate = expm1(estimate_log),
    n_obs = c(
      stats::nobs(joint_pooled_model),
      stats::nobs(joint_pooled_model),
      stats::nobs(joint_pooled_model),
      stats::nobs(signed_pooled_model)
    ),
    ward_pair_clusters = c(
      joint_pooled_df + 1,
      joint_pooled_df + 1,
      joint_pooled_df + 1,
      signed_pooled_df + 1
    ),
    control_blocks = block_counts$control_blocks,
    stricter_blocks = block_counts$stricter_blocks,
    lenient_blocks = block_counts$lenient_blocks,
    symmetry_sum = dplyr::if_else(
      specification == "joint_aggregate_contrast",
      symmetry_sum,
      NA_real_
    ),
    symmetry_se = dplyr::if_else(
      specification == "joint_aggregate_contrast",
      symmetry_se,
      NA_real_
    ),
    symmetry_p_value = dplyr::if_else(
      specification == "joint_aggregate_contrast",
      2 * stats::pt(
        -abs(symmetry_sum / symmetry_se),
        df = joint_pooled_df
      ),
      NA_real_
    ),
    pretrend_p_value = dplyr::case_when(
      specification == "joint_aggregate_contrast" ~
        joint_pretrend_p_value,
      specification == "signed_binary_constrained" ~
        signed_pretrend_p_value,
      TRUE ~ NA_real_
    )
  )

outcome_label <- dplyr::case_when(
  outcome_name == "high_discretion" ~ "High-discretion permits",
  outcome_name == "low_discretion_nosigns" ~
    "Low-discretion permits (excluding signs)"
)
score_label <- dplyr::case_when(
  score_variant == "current_no_income" ~
    "score omitting income and bachelor's share",
  score_variant == "education_added_back" ~
    "score including bachelor's share only",
  score_variant == "income_added_back" ~
    "score including income only",
  score_variant == "all_covariates" ~
    "score including income and bachelor's share"
)
output_stem <- paste(
  outcome_name,
  score_variant,
  paste0(
    bandwidth_label,
    ifelse(sample_rule == "stable", "_stable", "")
  ),
  sep = "_"
)

directional_plot_data <- dplyr::bind_rows(
  joint_event |>
    dplyr::transmute(
      event_time,
      group = "Assigned toward more stringent",
      estimate = stricter,
      ci_low = stricter_ci_low,
      ci_high = stricter_ci_high
    ),
  joint_event |>
    dplyr::transmute(
      event_time,
      group = "Assigned toward more lenient",
      estimate = lenient,
      ci_low = lenient_ci_low,
      ci_high = lenient_ci_high
    )
)

joint_stricter_pooled <- pooled |>
  dplyr::filter(specification == "joint_stricter_vs_unchanged")
joint_lenient_pooled <- pooled |>
  dplyr::filter(specification == "joint_lenient_vs_unchanged")

directional_plot <- ggplot2::ggplot(
  directional_plot_data,
  ggplot2::aes(event_time, estimate, color = group, fill = group)
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
    alpha = 0.18,
    color = NA
  ) +
  ggplot2::geom_line(linewidth = 0.85) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_color_manual(
    values = c(
      "Assigned toward more stringent" = "#176B58",
      "Assigned toward more lenient" = "#B0473E"
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Assigned toward more stringent" = "#176B58",
      "Assigned toward more lenient" = "#B0473E"
    )
  ) +
  ggplot2::scale_x_continuous(breaks = event_times) +
  ggplot2::labs(
    title = paste(outcome_label, "by application year"),
    subtitle = sprintf(
      paste0(
        "Pooled years 0-5: stricter = %.3f (SE %.3f); ",
        "lenient = %.3f (SE %.3f)\n%s"
      ),
      joint_stricter_pooled$estimate_log,
      joint_stricter_pooled$se,
      joint_lenient_pooled$estimate_log,
      joint_lenient_pooled$se,
      score_label
    ),
    x = "Years relative to the 2015 ward remap",
    y = "Effect relative to unchanged blocks",
    color = NULL,
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    axis.title.x = ggplot2::element_text(
      hjust = 0.5,
      margin = ggplot2::margin(t = 8)
    ),
    legend.position = "top"
  )

aggregate_plot_data <- dplyr::bind_rows(
  joint_event |>
    dplyr::transmute(
      event_time,
      group = "Joint directional contrast",
      estimate = aggregate,
      ci_low = aggregate_ci_low,
      ci_high = aggregate_ci_high
    ),
  signed_event |>
    dplyr::transmute(
      event_time,
      group = "Signed binary model",
      estimate,
      ci_low,
      ci_high
    )
)

joint_aggregate_pooled <- pooled |>
  dplyr::filter(specification == "joint_aggregate_contrast")
signed_aggregate_pooled <- pooled |>
  dplyr::filter(specification == "signed_binary_constrained")

aggregate_stars <- dplyr::case_when(
  joint_aggregate_pooled$p_value <= 0.01 ~ "***",
  joint_aggregate_pooled$p_value <= 0.05 ~ "**",
  joint_aggregate_pooled$p_value <= 0.10 ~ "*",
  TRUE ~ ""
)

signed_stars <- dplyr::case_when(
  signed_aggregate_pooled$p_value <= 0.01 ~ "***",
  signed_aggregate_pooled$p_value <= 0.05 ~ "**",
  signed_aggregate_pooled$p_value <= 0.10 ~ "*",
  TRUE ~ ""
)

paper_directional_plot <- ggplot2::ggplot(
  joint_event,
  ggplot2::aes(event_time, aggregate)
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
    ggplot2::aes(
      ymin = aggregate_ci_low,
      ymax = aggregate_ci_high
    ),
    fill = "#176B58",
    alpha = 0.18
  ) +
  ggplot2::geom_line(
    color = "#176B58",
    linewidth = 0.85
  ) +
  ggplot2::geom_point(
    color = "#176B58",
    size = 2
  ) +
  ggplot2::scale_x_continuous(breaks = event_times) +
  ggplot2::labs(
    title = paste(outcome_label, "by application year"),
    subtitle = sprintf(
      "%sPooled directional contrast = %.3f%s (SE %.3f)",
      ifelse(sample_rule == "stable", "Stable-incumbent sample; ", ""),
      joint_aggregate_pooled$estimate_log,
      aggregate_stars,
      joint_aggregate_pooled$se
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

paper_signed_plot <- ggplot2::ggplot(
  signed_event,
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
  ggplot2::geom_line(
    color = "#176B58",
    linewidth = 0.85
  ) +
  ggplot2::geom_point(
    color = "#176B58",
    size = 2
  ) +
  ggplot2::scale_x_continuous(breaks = event_times) +
  ggplot2::labs(
    title = paste(outcome_label, "by application year"),
    subtitle = sprintf(
      "%sPooled estimate = %.3f%s (SE %.3f)",
      ifelse(sample_rule == "stable", "Stable-incumbent sample; ", ""),
      signed_aggregate_pooled$estimate_log,
      signed_stars,
      signed_aggregate_pooled$se
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

aggregate_plot <- ggplot2::ggplot(
  aggregate_plot_data,
  ggplot2::aes(event_time, estimate, color = group, fill = group)
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
    alpha = 0.16,
    color = NA
  ) +
  ggplot2::geom_line(linewidth = 0.85) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_color_manual(
    values = c(
      "Joint directional contrast" = "#176B58",
      "Signed binary model" = "#6B4C9A"
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Joint directional contrast" = "#176B58",
      "Signed binary model" = "#6B4C9A"
    )
  ) +
  ggplot2::scale_x_continuous(breaks = event_times) +
  ggplot2::labs(
    title = paste(outcome_label, "aggregate binary estimates"),
    subtitle = sprintf(
      paste0(
        "Pooled years 0-5: joint contrast = %.3f (SE %.3f); ",
        "signed model = %.3f (SE %.3f)\n%s"
      ),
      joint_aggregate_pooled$estimate_log,
      joint_aggregate_pooled$se,
      signed_aggregate_pooled$estimate_log,
      signed_aggregate_pooled$se,
      score_label
    ),
    x = "Years relative to the 2015 ward remap",
    y = "Effect of assignment toward greater stringency",
    color = NULL,
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    axis.title.x = ggplot2::element_text(
      hjust = 0.5,
      margin = ggplot2::margin(t = 8)
    ),
    legend.position = "bottom"
  )

readr::write_csv(
  joint_event |>
    dplyr::mutate(
      outcome = outcome_name,
      score_variant = score_variant
    ),
  sprintf("../output/binary_joint_event_study_%s.csv", output_stem),
  na = ""
)
readr::write_csv(
  signed_event |>
    dplyr::mutate(
      outcome = outcome_name,
      score_variant = score_variant
    ),
  sprintf("../output/binary_signed_event_study_%s.csv", output_stem),
  na = ""
)
readr::write_csv(
  pooled,
  sprintf("../output/binary_event_study_pooled_%s.csv", output_stem),
  na = ""
)
ggplot2::ggsave(
  sprintf(
    "../output/binary_joint_directional_event_study_%s.pdf",
    output_stem
  ),
  directional_plot,
  width = 7.6,
  height = 5.8,
  bg = "white"
)
ggplot2::ggsave(
  sprintf(
    "../output/binary_joint_directional_event_study_%s.png",
    output_stem
  ),
  directional_plot,
  width = 7.6,
  height = 5.8,
  dpi = 180,
  bg = "white"
)
ggplot2::ggsave(
  sprintf(
    "../output/binary_joint_directional_event_study_%s_paper.pdf",
    output_stem
  ),
  paper_directional_plot,
  width = 7.6,
  height = 5.5,
  bg = "white"
)
ggplot2::ggsave(
  sprintf(
    "../output/binary_signed_directional_event_study_%s_paper.pdf",
    output_stem
  ),
  paper_signed_plot,
  width = 7.6,
  height = 5.5,
  bg = "white"
)
ggplot2::ggsave(
  sprintf(
    "../output/binary_aggregate_event_study_%s.pdf",
    output_stem
  ),
  aggregate_plot,
  width = 7.6,
  height = 5.2,
  bg = "white"
)
ggplot2::ggsave(
  sprintf(
    "../output/binary_aggregate_event_study_%s.png",
    output_stem
  ),
  aggregate_plot,
  width = 7.6,
  height = 5.2,
  dpi = 180,
  bg = "white"
)
