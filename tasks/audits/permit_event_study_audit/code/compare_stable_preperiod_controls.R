# setwd("tasks/audits/permit_event_study_audit/code")

source("../../../setup_environment/code/packages.R")

scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    cutoff == 2014L,
    variant == "income_added_back"
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
    dist_m <= 152.4,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(ward_pair_id),
    ward_pair_id != "",
    stable_both
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
    stricter = as.integer(score_change > 0),
    lenient = as.integer(score_change < 0),
    signed_direction = stricter - lenient,
    post = as.integer(relative_year >= 0L),
    post_stricter = post * stricter,
    post_lenient = post * lenient,
    post_signed = post * signed_direction
  )

if (
  anyDuplicated(data[c("block_id", "year")]) ||
    any(is.na(data$score_origin)) ||
    any(is.na(data$score_dest))
) {
  stop("The stable event-study sample failed validation.", call. = FALSE)
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

data <- data |>
  dplyr::left_join(
    pre_period_controls,
    by = "block_id",
    relationship = "many-to-one"
  )

pre_period_balance <- data |>
  dplyr::distinct(
    block_id,
    stricter,
    lenient,
    pre_period_permit_volume,
    no_pre_period_permits
  ) |>
  dplyr::mutate(
    assignment = dplyr::case_when(
      stricter == 1L ~ "more_stringent",
      lenient == 1L ~ "more_lenient",
      TRUE ~ "unchanged"
    )
  ) |>
  dplyr::group_by(assignment) |>
  dplyr::summarise(
    blocks = dplyr::n(),
    mean_pre_period_permits = mean(pre_period_permit_volume),
    share_no_pre_period_permits = mean(no_pre_period_permits),
    .groups = "drop"
  )

control_specs <- c(
  both = paste(
    "pre_period_permit_volume:factor(year)",
    "no_pre_period_permits:factor(year)",
    sep = " + "
  ),
  volume_only = "pre_period_permit_volume:factor(year)",
  indicator_only = "no_pre_period_permits:factor(year)",
  volume_year_zero_post = paste(
    "pre_period_permit_volume:factor(year)",
    "post:no_pre_period_permits",
    sep = " + "
  ),
  zero_post_only = "post:no_pre_period_permits",
  none = "1"
)
outcomes <- c(
  high_discretion = "n_high_discretion_application",
  low_discretion = "n_low_discretion_nosigns_application"
)

results <- list()
models <- list()
result_index <- 1L

for (outcome_name in names(outcomes)) {
  model_data <- data |>
    dplyr::mutate(outcome = .data[[outcomes[[outcome_name]]]])

  for (control_spec in names(control_specs)) {
    controls <- control_specs[[control_spec]]
    joint_model <- fixest::fepois(
      stats::as.formula(
        paste(
          "outcome ~ post_stricter + post_lenient +",
          controls,
          "| block_id + ward_pair_id^year"
        )
      ),
      data = model_data,
      cluster = ~ward_pair_id,
      notes = FALSE
    )
    signed_model <- fixest::fepois(
      stats::as.formula(
        paste(
          "outcome ~ post_signed +",
          controls,
          "| block_id + ward_pair_id^year"
        )
      ),
      data = model_data,
      cluster = ~ward_pair_id,
      notes = FALSE
    )
    event_model <- fixest::fepois(
      stats::as.formula(
        paste(
          paste0(
            "outcome ~ i(relative_year, signed_direction, ref = -1) + "
          ),
          controls,
          "| block_id + ward_pair_id^year"
        )
      ),
      data = model_data,
      cluster = ~ward_pair_id,
      notes = FALSE
    )

    joint_coefficients <- stats::coef(joint_model)
    joint_vcov <- stats::vcov(joint_model)
    signed_coefficients <- stats::coef(signed_model)
    signed_vcov <- stats::vcov(signed_model)
    joint_df <- fixest::degrees_freedom(joint_model, type = "t")
    signed_df <- fixest::degrees_freedom(signed_model, type = "t")

    stricter <- unname(joint_coefficients["post_stricter"])
    lenient <- unname(joint_coefficients["post_lenient"])
    stricter_variance <- joint_vcov[
      "post_stricter",
      "post_stricter"
    ]
    lenient_variance <- joint_vcov["post_lenient", "post_lenient"]
    covariance <- joint_vcov["post_stricter", "post_lenient"]
    contrast <- (stricter - lenient) / 2
    contrast_se <- sqrt(
      (
        stricter_variance +
          lenient_variance -
          2 * covariance
      ) / 4
    )
    symmetry_sum <- stricter + lenient
    symmetry_se <- sqrt(
      stricter_variance +
        lenient_variance +
        2 * covariance
    )
    signed <- unname(signed_coefficients["post_signed"])
    signed_se <- sqrt(signed_vcov["post_signed", "post_signed"])

    event_coefficients <- stats::coef(event_model)
    event_vcov <- stats::vcov(event_model)
    event_df <- fixest::degrees_freedom(event_model, type = "t")
    pre_periods <- c(-5L, -4L, -3L, -2L)
    pretrend_matrix <- matrix(
      0,
      nrow = length(pre_periods),
      ncol = length(event_coefficients),
      dimnames = list(NULL, names(event_coefficients))
    )
    for (i in seq_along(pre_periods)) {
      pretrend_matrix[
        i,
        paste0(
          "relative_year::",
          pre_periods[i],
          ":signed_direction"
        )
      ] <- 1
    }
    pretrend_coefficients <- drop(
      pretrend_matrix %*% event_coefficients
    )
    pretrend_vcov <- pretrend_matrix %*%
      event_vcov %*%
      t(pretrend_matrix)
    pretrend_f <- drop(
      t(pretrend_coefficients) %*%
        solve(pretrend_vcov, pretrend_coefficients)
    ) / length(pre_periods)
    pretrend_p <- stats::pf(
      pretrend_f,
      df1 = length(pre_periods),
      df2 = event_df,
      lower.tail = FALSE
    )

    results[[result_index]] <- tibble::tibble(
      outcome = outcome_name,
      control_spec,
      stricter,
      stricter_se = sqrt(stricter_variance),
      stricter_p = 2 * stats::pt(
        -abs(stricter / sqrt(stricter_variance)),
        df = joint_df
      ),
      lenient,
      lenient_se = sqrt(lenient_variance),
      lenient_p = 2 * stats::pt(
        -abs(lenient / sqrt(lenient_variance)),
        df = joint_df
      ),
      directional_contrast = contrast,
      directional_contrast_se = contrast_se,
      directional_contrast_p = 2 * stats::pt(
        -abs(contrast / contrast_se),
        df = joint_df
      ),
      symmetry_p = 2 * stats::pt(
        -abs(symmetry_sum / symmetry_se),
        df = joint_df
      ),
      signed_direction = signed,
      signed_direction_se = signed_se,
      signed_direction_p = 2 * stats::pt(
        -abs(signed / signed_se),
        df = signed_df
      ),
      signed_pretrend_p = pretrend_p,
      observations = stats::nobs(signed_model),
      ward_pairs = signed_df + 1L
    )
    models[[paste(outcome_name, control_spec, sep = "_")]] <- list(
      joint = joint_model,
      signed = signed_model
    )
    result_index <- result_index + 1L
  }
}

results <- dplyr::bind_rows(results)

stacked_data <- dplyr::bind_rows(
  data |>
    dplyr::mutate(
      outcome_name = "high",
      outcome = n_high_discretion_application
    ),
  data |>
    dplyr::mutate(
      outcome_name = "low",
      outcome = n_low_discretion_nosigns_application
    )
) |>
  dplyr::mutate(
    high_post_stricter = post_stricter * (outcome_name == "high"),
    low_post_stricter = post_stricter * (outcome_name == "low"),
    high_post_lenient = post_lenient * (outcome_name == "high"),
    low_post_lenient = post_lenient * (outcome_name == "low"),
    high_post_signed = post_signed * (outcome_name == "high"),
    low_post_signed = post_signed * (outcome_name == "low"),
    high_post_no_pre = post *
      no_pre_period_permits *
      (outcome_name == "high"),
    low_post_no_pre = post *
      no_pre_period_permits *
      (outcome_name == "low"),
    outcome_year = paste(outcome_name, year, sep = "_"),
    outcome_block = paste(outcome_name, block_id, sep = "_"),
    outcome_pair_year = paste(
      outcome_name,
      ward_pair_id,
      year,
      sep = "_"
    )
  )

stacked_control_specs <- c(
  both = paste(
    "pre_period_permit_volume:factor(outcome_year)",
    "no_pre_period_permits:factor(outcome_year)",
    sep = " + "
  ),
  volume_only = "pre_period_permit_volume:factor(outcome_year)",
  indicator_only = "no_pre_period_permits:factor(outcome_year)",
  volume_year_zero_post = paste(
    "pre_period_permit_volume:factor(outcome_year)",
    "high_post_no_pre",
    "low_post_no_pre",
    sep = " + "
  ),
  zero_post_only = "high_post_no_pre + low_post_no_pre",
  none = "1"
)

comparisons <- list()
comparison_index <- 1L

for (control_spec in names(stacked_control_specs)) {
  controls <- stacked_control_specs[[control_spec]]
  joint_model <- fixest::fepois(
    stats::as.formula(
      paste(
        paste(
          "outcome ~ high_post_stricter + low_post_stricter +",
          "high_post_lenient + low_post_lenient +"
        ),
        controls,
        "| outcome_block + outcome_pair_year"
      )
    ),
    data = stacked_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  signed_model <- fixest::fepois(
    stats::as.formula(
      paste(
        "outcome ~ high_post_signed + low_post_signed +",
        controls,
        "| outcome_block + outcome_pair_year"
      )
    ),
    data = stacked_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  joint_coefficients <- stats::coef(joint_model)
  joint_vcov <- stats::vcov(joint_model)
  signed_coefficients <- stats::coef(signed_model)
  signed_vcov <- stats::vcov(signed_model)
  joint_df <- fixest::degrees_freedom(joint_model, type = "t")
  signed_df <- fixest::degrees_freedom(signed_model, type = "t")

  joint_weights <- list(
    stricter = c(
      high_post_stricter = 1,
      low_post_stricter = -1
    ),
    lenient = c(
      high_post_lenient = 1,
      low_post_lenient = -1
    ),
    directional_contrast = c(
      high_post_stricter = 0.5,
      high_post_lenient = -0.5,
      low_post_stricter = -0.5,
      low_post_lenient = 0.5
    )
  )

  for (specification in names(joint_weights)) {
    weights <- rep(0, length(joint_coefficients))
    names(weights) <- names(joint_coefficients)
    weights[names(joint_weights[[specification]])] <-
      joint_weights[[specification]]
    estimate <- drop(weights %*% joint_coefficients)
    se <- sqrt(drop(weights %*% joint_vcov %*% weights))
    comparisons[[comparison_index]] <- tibble::tibble(
      control_spec,
      specification,
      high_minus_low = estimate,
      se,
      p_value = 2 * stats::pt(
        -abs(estimate / se),
        df = joint_df
      )
    )
    comparison_index <- comparison_index + 1L
  }

  weights <- rep(0, length(signed_coefficients))
  names(weights) <- names(signed_coefficients)
  weights[c("high_post_signed", "low_post_signed")] <- c(1, -1)
  estimate <- drop(weights %*% signed_coefficients)
  se <- sqrt(drop(weights %*% signed_vcov %*% weights))
  comparisons[[comparison_index]] <- tibble::tibble(
    control_spec,
    specification = "signed_direction",
    high_minus_low = estimate,
    se,
    p_value = 2 * stats::pt(
      -abs(estimate / se),
      df = signed_df
    )
  )
  comparison_index <- comparison_index + 1L
}

comparisons <- dplyr::bind_rows(comparisons)

readr::write_csv(
  results,
  "../output/stable_preperiod_control_sensitivity_500ft.csv"
)
readr::write_csv(
  comparisons,
  "../output/stable_preperiod_control_outcome_tests_500ft.csv"
)
readr::write_csv(
  pre_period_balance,
  "../output/stable_preperiod_control_balance_500ft.csv"
)
