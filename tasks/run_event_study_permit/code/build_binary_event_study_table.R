# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# bandwidth_m <- 152.4
# bandwidth_label <- "500ft"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0L) {
  cli_args <- c(bandwidth_m, bandwidth_label)
}
if (length(cli_args) != 2L) {
  stop("Expected a bandwidth and bandwidth label.", call. = FALSE)
}

bandwidth_m <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]

if (
  !is.finite(bandwidth_m) ||
    bandwidth_m <= 0 ||
    !grepl("^[A-Za-z0-9_-]+$", bandwidth_label)
) {
  stop("Invalid bandwidth or bandwidth label.", call. = FALSE)
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
    ward_pair_id != "",
    stable_both
  ) |>
  dplyr::mutate(
    stricter = as.integer(strictness_change_frozen > 0),
    lenient = as.integer(strictness_change_frozen < 0),
    signed_direction = stricter - lenient,
    post = as.integer(relative_year >= 0L),
    post_stricter = post * stricter,
    post_lenient = post * lenient,
    post_signed = post * signed_direction
  )

if (anyDuplicated(data[c("block_id", "year")])) {
  stop("Event-study data must be unique by block and year.", call. = FALSE)
}
if (
  any(data$stricter + data$lenient > 1L) ||
    any(!data$signed_direction %in% c(-1L, 0L, 1L))
) {
  stop("Binary treatment assignment failed validation.", call. = FALSE)
}

pre_period_activity <- data |>
  dplyr::filter(relative_year < 0L) |>
  dplyr::summarise(
    pre_period_permit_volume = sum(
      n_high_discretion_application,
      na.rm = TRUE
    ),
    .by = block_id
  )

if (anyDuplicated(pre_period_activity$block_id)) {
  stop("Pre-period activity must be unique by block.", call. = FALSE)
}

data <- data |>
  dplyr::left_join(
    pre_period_activity,
    by = "block_id",
    relationship = "many-to-one"
  ) |>
  dplyr::filter(pre_period_permit_volume > 0)

outcomes <- c(
  high_discretion = "n_high_discretion_application",
  low_discretion = "n_low_discretion_nosigns_application"
)
results <- vector("list", length(outcomes))

for (i in seq_along(outcomes)) {
  model_data <- data |>
    dplyr::mutate(outcome = .data[[outcomes[i]]])

  joint_model <- fixest::fepois(
    outcome ~
      post_stricter +
      post_lenient |
      block_id + ward_pair_id^year,
    data = model_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  signed_model <- fixest::fepois(
    outcome ~ post_signed |
      block_id + ward_pair_id^year,
    data = model_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  joint_coef <- stats::coef(joint_model)
  joint_vcov <- stats::vcov(joint_model)
  joint_df <- fixest::degrees_freedom(joint_model, type = "t")
  signed_coef <- stats::coef(signed_model)[["post_signed"]]
  signed_se <- sqrt(stats::vcov(signed_model)["post_signed", "post_signed"])
  signed_df <- fixest::degrees_freedom(signed_model, type = "t")

  stricter_coef <- unname(joint_coef["post_stricter"])
  lenient_coef <- unname(joint_coef["post_lenient"])
  stricter_se <- sqrt(joint_vcov["post_stricter", "post_stricter"])
  lenient_se <- sqrt(joint_vcov["post_lenient", "post_lenient"])
  stricter_lenient_cov <- joint_vcov[
    "post_stricter",
    "post_lenient"
  ]
  contrast_coef <- (stricter_coef - lenient_coef) / 2
  contrast_se <- sqrt(
    (
      stricter_se^2 +
        lenient_se^2 -
        2 * stricter_lenient_cov
    ) / 4
  )
  symmetry_coef <- stricter_coef + lenient_coef
  symmetry_se <- sqrt(
    stricter_se^2 +
      lenient_se^2 +
      2 * stricter_lenient_cov
  )

  results[[i]] <- tibble::tibble(
    outcome = names(outcomes)[i],
    specification = c(
      "signed",
      "stricter",
      "lenient",
      "contrast"
    ),
    estimate = c(
      signed_coef,
      stricter_coef,
      lenient_coef,
      contrast_coef
    ),
    standard_error = c(
      signed_se,
      stricter_se,
      lenient_se,
      contrast_se
    ),
    p_value = c(
      2 * stats::pt(-abs(signed_coef / signed_se), df = signed_df),
      2 * stats::pt(-abs(stricter_coef / stricter_se), df = joint_df),
      2 * stats::pt(-abs(lenient_coef / lenient_se), df = joint_df),
      2 * stats::pt(-abs(contrast_coef / contrast_se), df = joint_df)
    ),
    symmetry_p_value = c(
      NA_real_,
      NA_real_,
      NA_real_,
      2 * stats::pt(-abs(symmetry_coef / symmetry_se), df = joint_df)
    ),
    observations = c(
      stats::nobs(signed_model),
      rep(stats::nobs(joint_model), 3L)
    )
  )
}

results <- dplyr::bind_rows(results)

format_estimate <- function(outcome, specification) {
  row <- results |>
    dplyr::filter(
      .data$outcome == .env$outcome,
      .data$specification == .env$specification
    )
  stars <- dplyr::case_when(
    row$p_value <= 0.01 ~ "***",
    row$p_value <= 0.05 ~ "**",
    row$p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )
  sprintf("%.3f%s", row$estimate, stars)
}

format_se <- function(outcome, specification) {
  row <- results |>
    dplyr::filter(
      .data$outcome == .env$outcome,
      .data$specification == .env$specification
    )
  sprintf("(%.3f)", row$standard_error)
}

table_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & High-Discretion & Low-Discretion \\\\",
  "\\midrule",
  sprintf(
    "Main estimate & %s & %s \\\\",
    format_estimate("high_discretion", "signed"),
    format_estimate("low_discretion", "signed")
  ),
  sprintf(
    " & %s & %s \\\\",
    format_se("high_discretion", "signed"),
    format_se("low_discretion", "signed")
  ),
  "\\addlinespace",
  "\\multicolumn{3}{l}{\\textit{Directions estimated separately}} \\\\",
  sprintf(
    "Assigned toward more stringent & %s & %s \\\\",
    format_estimate("high_discretion", "stricter"),
    format_estimate("low_discretion", "stricter")
  ),
  sprintf(
    " & %s & %s \\\\",
    format_se("high_discretion", "stricter"),
    format_se("low_discretion", "stricter")
  ),
  sprintf(
    "Assigned toward more lenient & %s & %s \\\\",
    format_estimate("high_discretion", "lenient"),
    format_estimate("low_discretion", "lenient")
  ),
  sprintf(
    " & %s & %s \\\\",
    format_se("high_discretion", "lenient"),
    format_se("low_discretion", "lenient")
  ),
  sprintf(
    "One-half difference between directions & %s & %s \\\\",
    format_estimate("high_discretion", "contrast"),
    format_estimate("low_discretion", "contrast")
  ),
  sprintf(
    " & %s & %s \\\\",
    format_se("high_discretion", "contrast"),
    format_se("low_discretion", "contrast")
  ),
  sprintf(
    "Equal-and-opposite test $p$-value & %.3f & %.3f \\\\",
    results |>
      dplyr::filter(
        outcome == "high_discretion",
        specification == "contrast"
      ) |>
      dplyr::pull(symmetry_p_value),
    results |>
      dplyr::filter(
        outcome == "low_discretion",
        specification == "contrast"
      ) |>
      dplyr::pull(symmetry_p_value)
  ),
  "\\midrule",
  "Block fixed effects & Yes & Yes \\\\",
  "Ward-pair $\\times$ year fixed effects & Yes & Yes \\\\",
  "Positive pre-period permit activity required & Yes & Yes \\\\",
  "\\midrule",
  sprintf(
    "N & %s & %s \\\\",
    format(
      results |>
        dplyr::filter(
          outcome == "high_discretion",
          specification == "signed"
        ) |>
        dplyr::pull(observations),
      big.mark = ","
    ),
    format(
      results |>
        dplyr::filter(
          outcome == "low_discretion",
          specification == "signed"
        ) |>
        dplyr::pull(observations),
      big.mark = ","
    )
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(
  table_lines,
  sprintf(
    "../output/permit_event_study_appendix_%s.tex",
    bandwidth_label
  )
)
