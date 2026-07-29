# setwd("tasks/audits/new_construction_event_study/code")

source("../../../setup_environment/code/packages.R")

data <- arrow::read_parquet(
  "../output/new_construction_block_year_panel.parquet"
) |>
  dplyr::mutate(
    post = as.integer(relative_year >= 0L),
    post_signed = post * signed_direction,
    post_continuous = post * score_change
  )

if (
  anyDuplicated(data[c("block_id", "year")]) ||
    any(is.na(data$n_construction)) ||
    any(data$n_construction < 0L) ||
    any(!data$signed_direction %in% c(-1L, 0L, 1L))
) {
  stop("The construction event-study panel failed validation.", call. = FALSE)
}

count_event_model <- fixest::fepois(
  n_construction ~
    i(relative_year, signed_direction, ref = -1) +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

count_pooled_model <- fixest::fepois(
  n_construction ~
    post_signed +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

count_permit_controls_event_model <- fixest::fepois(
  n_construction ~
    i(relative_year, signed_direction, ref = -1) +
    pre_period_permit_volume:factor(year) +
    no_pre_period_permits:factor(year) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

count_permit_controls_pooled_model <- fixest::fepois(
  n_construction ~
    post_signed +
    pre_period_permit_volume:factor(year) +
    no_pre_period_permits:factor(year) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

count_no_controls_event_model <- fixest::fepois(
  n_construction ~
    i(relative_year, signed_direction, ref = -1) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

count_no_controls_pooled_model <- fixest::fepois(
  n_construction ~
    post_signed |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

continuous_count_event_model <- fixest::fepois(
  n_construction ~
    i(relative_year, score_change, ref = -1) +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

continuous_count_pooled_model <- fixest::fepois(
  n_construction ~
    post_continuous +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

continuous_count_permit_controls_event_model <- fixest::fepois(
  n_construction ~
    i(relative_year, score_change, ref = -1) +
    pre_period_permit_volume:factor(year) +
    no_pre_period_permits:factor(year) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

continuous_count_permit_controls_pooled_model <- fixest::fepois(
  n_construction ~
    post_continuous +
    pre_period_permit_volume:factor(year) +
    no_pre_period_permits:factor(year) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

continuous_count_no_controls_event_model <- fixest::fepois(
  n_construction ~
    i(relative_year, score_change, ref = -1) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

continuous_count_no_controls_pooled_model <- fixest::fepois(
  n_construction ~
    post_continuous |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
  glm.iter = 1000,
  notes = FALSE
)

far_data <- data |>
  dplyr::filter(!is.na(mean_log_far))

dupac_data <- data |>
  dplyr::filter(!is.na(mean_log_dupac))

far_event_model <- fixest::feols(
  mean_log_far ~
    i(relative_year, signed_direction, ref = -1) +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = far_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

far_pooled_model <- fixest::feols(
  mean_log_far ~
    post_signed +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = far_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

dupac_event_model <- fixest::feols(
  mean_log_dupac ~
    i(relative_year, signed_direction, ref = -1) +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = dupac_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

dupac_pooled_model <- fixest::feols(
  mean_log_dupac ~
    post_signed +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = dupac_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

continuous_far_event_model <- fixest::feols(
  mean_log_far ~
    i(relative_year, score_change, ref = -1) +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = far_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

continuous_far_pooled_model <- fixest::feols(
  mean_log_far ~
    post_continuous +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = far_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

continuous_dupac_event_model <- fixest::feols(
  mean_log_dupac ~
    i(relative_year, score_change, ref = -1) +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = dupac_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

continuous_dupac_pooled_model <- fixest::feols(
  mean_log_dupac ~
    post_continuous +
    pre_period_construction:factor(year) +
    no_pre_period_construction:factor(year) |
    block_id + ward_pair_id^year,
  data = dupac_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

event_models <- list(
  construction_count_preconstruction_controls = count_event_model,
  construction_count_permit_controls =
    count_permit_controls_event_model,
  construction_count_no_controls = count_no_controls_event_model,
  continuous_construction_count_preconstruction_controls =
    continuous_count_event_model,
  continuous_construction_count_permit_controls =
    continuous_count_permit_controls_event_model,
  continuous_construction_count_no_controls =
    continuous_count_no_controls_event_model,
  conditional_log_far = far_event_model,
  conditional_log_dupac = dupac_event_model,
  continuous_conditional_log_far = continuous_far_event_model,
  continuous_conditional_log_dupac = continuous_dupac_event_model
)
pooled_models <- list(
  construction_count_preconstruction_controls = count_pooled_model,
  construction_count_permit_controls =
    count_permit_controls_pooled_model,
  construction_count_no_controls = count_no_controls_pooled_model,
  continuous_construction_count_preconstruction_controls =
    continuous_count_pooled_model,
  continuous_construction_count_permit_controls =
    continuous_count_permit_controls_pooled_model,
  continuous_construction_count_no_controls =
    continuous_count_no_controls_pooled_model,
  conditional_log_far = far_pooled_model,
  conditional_log_dupac = dupac_pooled_model,
  continuous_conditional_log_far = continuous_far_pooled_model,
  continuous_conditional_log_dupac = continuous_dupac_pooled_model
)
model_data <- list(
  construction_count_preconstruction_controls = data,
  construction_count_permit_controls = data,
  construction_count_no_controls = data,
  continuous_construction_count_preconstruction_controls = data,
  continuous_construction_count_permit_controls = data,
  continuous_construction_count_no_controls = data,
  conditional_log_far = far_data,
  conditional_log_dupac = dupac_data,
  continuous_conditional_log_far = far_data,
  continuous_conditional_log_dupac = dupac_data
)
model_treatment <- c(
  construction_count_preconstruction_controls = "signed_direction",
  construction_count_permit_controls = "signed_direction",
  construction_count_no_controls = "signed_direction",
  continuous_construction_count_preconstruction_controls =
    "score_change",
  continuous_construction_count_permit_controls = "score_change",
  continuous_construction_count_no_controls = "score_change",
  conditional_log_far = "signed_direction",
  conditional_log_dupac = "signed_direction",
  continuous_conditional_log_far = "score_change",
  continuous_conditional_log_dupac = "score_change"
)
pooled_treatment <- c(
  construction_count_preconstruction_controls = "post_signed",
  construction_count_permit_controls = "post_signed",
  construction_count_no_controls = "post_signed",
  continuous_construction_count_preconstruction_controls =
    "post_continuous",
  continuous_construction_count_permit_controls = "post_continuous",
  continuous_construction_count_no_controls = "post_continuous",
  conditional_log_far = "post_signed",
  conditional_log_dupac = "post_signed",
  continuous_conditional_log_far = "post_continuous",
  continuous_conditional_log_dupac = "post_continuous"
)

event_rows <- list()
pretrend_rows <- list()
support_rows <- list()
event_times <- -5:5
pre_periods <- -5:-2

for (outcome_name in names(event_models)) {
  model <- event_models[[outcome_name]]
  treatment_name <- model_treatment[[outcome_name]]
  coefficients <- stats::coef(model)
  variance <- stats::vcov(model)
  degrees_freedom <- fixest::degrees_freedom(model, type = "t")
  critical_value <- stats::qt(0.975, df = degrees_freedom)
  rows <- vector("list", length(event_times))

  for (i in seq_along(event_times)) {
    event_time <- event_times[i]
    if (event_time == -1L) {
      rows[[i]] <- tibble::tibble(
        outcome = outcome_name,
        event_time,
        estimate_log = 0,
        se = 0,
        p_value = NA_real_,
        ci_low_log = 0,
        ci_high_log = 0
      )
      next
    }

    coefficient_name <- paste0(
      "relative_year::",
      event_time,
      ":",
      treatment_name
    )
    if (!coefficient_name %in% names(coefficients)) {
      stop(
        paste("Missing event coefficient for", outcome_name, event_time),
        call. = FALSE
      )
    }

    estimate <- unname(coefficients[coefficient_name])
    se <- sqrt(variance[coefficient_name, coefficient_name])
    rows[[i]] <- tibble::tibble(
      outcome = outcome_name,
      event_time,
      estimate_log = estimate,
      se,
      p_value = 2 * stats::pt(
        -abs(estimate / se),
        df = degrees_freedom
      ),
      ci_low_log = estimate - critical_value * se,
      ci_high_log = estimate + critical_value * se
    )
  }

  event_rows[[outcome_name]] <- dplyr::bind_rows(rows)

  restriction_matrix <- matrix(
    0,
    nrow = length(pre_periods),
    ncol = length(coefficients),
    dimnames = list(NULL, names(coefficients))
  )
  for (i in seq_along(pre_periods)) {
    restriction_matrix[
      i,
      paste0(
        "relative_year::",
        pre_periods[i],
        ":",
        treatment_name
      )
    ] <- 1
  }
  restricted_coefficients <- drop(
    restriction_matrix %*% coefficients
  )
  restricted_variance <- restriction_matrix %*%
    variance %*%
    t(restriction_matrix)
  f_statistic <- drop(
    t(restricted_coefficients) %*%
      solve(restricted_variance, restricted_coefficients)
  ) / length(pre_periods)

  pretrend_rows[[outcome_name]] <- tibble::tibble(
    outcome = outcome_name,
    f_statistic,
    numerator_df = length(pre_periods),
    denominator_df = degrees_freedom,
    p_value = stats::pf(
      f_statistic,
      df1 = length(pre_periods),
      df2 = degrees_freedom,
      lower.tail = FALSE
    )
  )

  used_data <- model_data[[outcome_name]][fixest::obs(model), ]
  support_rows[[outcome_name]] <- tibble::tibble(
    outcome = outcome_name,
    input_observations = nrow(model_data[[outcome_name]]),
    estimation_observations = stats::nobs(model),
    estimation_blocks = dplyr::n_distinct(used_data$block_id),
    estimation_ward_pairs = dplyr::n_distinct(used_data$ward_pair_id),
    positive_outcome_block_years = if (
      grepl("construction_count", outcome_name, fixed = TRUE)
    ) {
      sum(used_data$n_construction > 0L)
    } else {
      nrow(used_data)
    }
  )
}

event_results <- dplyr::bind_rows(event_rows) |>
  dplyr::mutate(
    estimate = dplyr::if_else(
      grepl("construction_count", outcome, fixed = TRUE),
      expm1(estimate_log),
      estimate_log
    ),
    ci_low = dplyr::if_else(
      grepl("construction_count", outcome, fixed = TRUE),
      expm1(ci_low_log),
      ci_low_log
    ),
    ci_high = dplyr::if_else(
      grepl("construction_count", outcome, fixed = TRUE),
      expm1(ci_high_log),
      ci_high_log
    )
  )

pooled_rows <- list()
for (outcome_name in names(pooled_models)) {
  model <- pooled_models[[outcome_name]]
  treatment_name <- pooled_treatment[[outcome_name]]
  estimate_log <- unname(stats::coef(model)[treatment_name])
  se <- sqrt(stats::vcov(model)[treatment_name, treatment_name])
  degrees_freedom <- fixest::degrees_freedom(model, type = "t")
  p_value <- 2 * stats::pt(
    -abs(estimate_log / se),
    df = degrees_freedom
  )

  pooled_rows[[outcome_name]] <- tibble::tibble(
    outcome = outcome_name,
    estimate_log,
    se,
    p_value,
    estimate = if (
      grepl("construction_count", outcome_name, fixed = TRUE)
    ) {
      expm1(estimate_log)
    } else {
      estimate_log
    },
    n_obs = stats::nobs(model)
  )
}
pooled_results <- dplyr::bind_rows(pooled_rows)

stars <- function(p_value) {
  dplyr::case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

plot_event_study <- function(
  outcome_name,
  title,
  y_label,
  filename,
  color
) {
  plot_data <- event_results |>
    dplyr::filter(outcome == outcome_name)
  pooled_row <- pooled_results |>
    dplyr::filter(outcome == outcome_name)

  plot <- ggplot2::ggplot(
    plot_data,
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
      fill = color,
      alpha = 0.16
    ) +
    ggplot2::geom_line(color = color, linewidth = 0.85) +
    ggplot2::geom_point(color = color, size = 2) +
    ggplot2::scale_x_continuous(breaks = event_times) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf(
        "Pooled years 0-5 = %.3f%s (SE %.3f)",
        pooled_row$estimate_log,
        stars(pooled_row$p_value),
        pooled_row$se
      ),
      x = "Years relative to the 2015 ward remap",
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(
    filename,
    plot,
    width = 7.4,
    height = 4.8
  )
  plot
}

count_plot <- plot_event_study(
  "construction_count_preconstruction_controls",
  "Completed residential construction: construction controls",
  "Proportional effect of assignment toward greater stringency",
  "../output/new_construction_count_event_study.pdf",
  "#176B58"
)
count_permit_controls_plot <- plot_event_study(
  "construction_count_permit_controls",
  "Completed residential construction: permit controls",
  "Proportional effect of assignment toward greater stringency",
  "../output/new_construction_count_permit_controls_event_study.pdf",
  "#176B58"
)
count_no_controls_plot <- plot_event_study(
  "construction_count_no_controls",
  "Completed residential construction: no pre-period controls",
  "Proportional effect of assignment toward greater stringency",
  "../output/new_construction_count_no_controls_event_study.pdf",
  "#176B58"
)
continuous_count_plot <- plot_event_study(
  "continuous_construction_count_preconstruction_controls",
  "Completed construction: continuous score, construction controls",
  "Effect of a 1 SD increase in assigned stringency",
  "../output/new_construction_continuous_count_event_study.pdf",
  "#176B58"
)
continuous_count_permit_controls_plot <- plot_event_study(
  "continuous_construction_count_permit_controls",
  "Completed construction: continuous score, permit controls",
  "Effect of a 1 SD increase in assigned stringency",
  "../output/new_construction_continuous_count_permit_controls_event_study.pdf",
  "#176B58"
)
continuous_count_no_controls_plot <- plot_event_study(
  "continuous_construction_count_no_controls",
  "Completed construction: continuous score, no pre-period controls",
  "Effect of a 1 SD increase in assigned stringency",
  "../output/new_construction_continuous_count_no_controls_event_study.pdf",
  "#176B58"
)
far_plot <- plot_event_study(
  "conditional_log_far",
  "Log FAR, conditional on construction",
  "Effect on mean log FAR",
  "../output/new_construction_far_event_study.pdf",
  "#2B6CB0"
)
dupac_plot <- plot_event_study(
  "conditional_log_dupac",
  "Log units per acre, conditional on construction",
  "Effect on mean log units per acre",
  "../output/new_construction_dupac_event_study.pdf",
  "#B33A3A"
)
continuous_far_plot <- plot_event_study(
  "continuous_conditional_log_far",
  "Log FAR: continuous score, conditional on construction",
  "Effect of a 1 SD increase in assigned stringency",
  "../output/new_construction_continuous_far_event_study.pdf",
  "#2B6CB0"
)
continuous_dupac_plot <- plot_event_study(
  "continuous_conditional_log_dupac",
  "Log units per acre: continuous score, conditional on construction",
  "Effect of a 1 SD increase in assigned stringency",
  "../output/new_construction_continuous_dupac_event_study.pdf",
  "#B33A3A"
)

ggplot2::ggsave(
  "../output/new_construction_event_studies.pdf",
  count_plot / (far_plot | dupac_plot),
  width = 12,
  height = 9
)
ggplot2::ggsave(
  "../output/new_construction_count_specification_comparison.pdf",
  count_plot /
    count_permit_controls_plot /
    count_no_controls_plot,
  width = 8.5,
  height = 12
)
ggplot2::ggsave(
  "../output/new_construction_continuous_event_studies.pdf",
  continuous_count_plot /
    (continuous_far_plot | continuous_dupac_plot),
  width = 12,
  height = 9
)
ggplot2::ggsave(
  "../output/new_construction_continuous_count_specification_comparison.pdf",
  continuous_count_plot /
    continuous_count_permit_controls_plot /
    continuous_count_no_controls_plot,
  width = 8.5,
  height = 12
)

readr::write_csv(
  event_results,
  "../output/new_construction_event_study_coefficients.csv"
)
readr::write_csv(
  pooled_results,
  "../output/new_construction_event_study_pooled.csv"
)
readr::write_csv(
  dplyr::bind_rows(support_rows),
  "../output/new_construction_event_study_support.csv"
)
readr::write_csv(
  dplyr::bind_rows(pretrend_rows),
  "../output/new_construction_event_study_pretrends.csv"
)
