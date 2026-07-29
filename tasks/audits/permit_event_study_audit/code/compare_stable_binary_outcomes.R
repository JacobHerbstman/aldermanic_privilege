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
    post = as.integer(relative_year >= 0L)
  )

if (
  anyDuplicated(data[c("block_id", "year")]) ||
    any(is.na(data$score_origin)) ||
    any(is.na(data$score_dest))
) {
  stop("The stable event-study sample failed validation.", call. = FALSE)
}

high_controls <- data |>
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

stacked_data <- dplyr::bind_rows(
  data |>
    dplyr::left_join(
      high_controls,
      by = "block_id",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      outcome_name = "high",
      outcome = n_high_discretion_application
    ),
  data |>
    dplyr::left_join(
      high_controls,
      by = "block_id",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      outcome_name = "low",
      outcome = n_low_discretion_nosigns_application
    )
) |>
  dplyr::mutate(
    high_post_stricter = post * stricter * (outcome_name == "high"),
    low_post_stricter = post * stricter * (outcome_name == "low"),
    high_post_lenient = post * lenient * (outcome_name == "high"),
    low_post_lenient = post * lenient * (outcome_name == "low"),
    high_post_signed = post * signed_direction * (outcome_name == "high"),
    low_post_signed = post * signed_direction * (outcome_name == "low"),
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

joint_model <- fixest::fepois(
  outcome ~
    high_post_stricter +
    low_post_stricter +
    high_post_lenient +
    low_post_lenient +
    pre_period_permit_volume:factor(outcome_year) +
    high_post_no_pre +
    low_post_no_pre |
    outcome_block + outcome_pair_year,
  data = stacked_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

signed_model <- fixest::fepois(
  outcome ~
    high_post_signed +
    low_post_signed +
    pre_period_permit_volume:factor(outcome_year) +
    high_post_no_pre +
    low_post_no_pre |
    outcome_block + outcome_pair_year,
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

comparison_rows <- lapply(names(joint_weights), function(specification) {
  weights <- rep(0, length(joint_coefficients))
  names(weights) <- names(joint_coefficients)
  weights[names(joint_weights[[specification]])] <-
    joint_weights[[specification]]
  estimate <- drop(weights %*% joint_coefficients)
  se <- sqrt(drop(weights %*% joint_vcov %*% weights))
  tibble::tibble(
    specification,
    high_minus_low = estimate,
    se,
    degrees_freedom = joint_df,
    p_value = 2 * stats::pt(-abs(estimate / se), df = joint_df)
  )
})

signed_weights <- rep(0, length(signed_coefficients))
names(signed_weights) <- names(signed_coefficients)
signed_weights[c("high_post_signed", "low_post_signed")] <- c(1, -1)
signed_estimate <- drop(signed_weights %*% signed_coefficients)
signed_se <- sqrt(
  drop(signed_weights %*% signed_vcov %*% signed_weights)
)

comparisons <- dplyr::bind_rows(
  comparison_rows,
  tibble::tibble(
    specification = "signed_direction",
    high_minus_low = signed_estimate,
    se = signed_se,
    degrees_freedom = signed_df,
    p_value = 2 * stats::pt(
      -abs(signed_estimate / signed_se),
      df = signed_df
    )
  )
)

high_results <- readr::read_csv(
  paste0(
    "../output/binary_event_study_pooled_high_discretion_",
    "income_added_back_500ft_stable.csv"
  ),
  show_col_types = FALSE
)
low_results <- readr::read_csv(
  paste0(
    "../output/binary_event_study_pooled_low_discretion_nosigns_",
    "income_added_back_500ft_stable.csv"
  ),
  show_col_types = FALSE
)

expected_joint <- c(
  high_post_stricter = high_results$estimate_log[
    high_results$specification == "joint_stricter_vs_unchanged"
  ],
  low_post_stricter = low_results$estimate_log[
    low_results$specification == "joint_stricter_vs_unchanged"
  ],
  high_post_lenient = high_results$estimate_log[
    high_results$specification == "joint_lenient_vs_unchanged"
  ],
  low_post_lenient = low_results$estimate_log[
    low_results$specification == "joint_lenient_vs_unchanged"
  ]
)
expected_signed <- c(
  high_post_signed = high_results$estimate_log[
    high_results$specification == "signed_binary_constrained"
  ],
  low_post_signed = low_results$estimate_log[
    low_results$specification == "signed_binary_constrained"
  ]
)

if (
  max(
    abs(joint_coefficients[names(expected_joint)] - expected_joint)
  ) > 1e-6 ||
    max(
      abs(signed_coefficients[names(expected_signed)] - expected_signed)
    ) > 1e-6
) {
  stop(
    sprintf(
      paste(
        "The stacked models do not reproduce the separate estimates.",
        "Maximum joint difference: %.10f.",
        "Maximum signed difference: %.10f."
      ),
      max(
        abs(joint_coefficients[names(expected_joint)] - expected_joint)
      ),
      max(
        abs(signed_coefficients[names(expected_signed)] - expected_signed)
      )
    )
  )
}

readr::write_csv(
  comparisons,
  "../output/stable_binary_outcome_comparison_500ft.csv"
)
