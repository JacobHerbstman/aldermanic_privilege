# setwd("tasks/run_event_study_permit/code")
# Pooled 2015-2020 permit effects for the appendix table: the combined model (moves toward greater stringency and
# greater leniency as opposite changes) and the model estimating the two directions separately, for high- and
# low-discretion permits. The main comparison group is unchanged blocks on both sides of the old boundary between the
# origin and destination wards; the second compares reassigned blocks only with unchanged blocks in the ward they
# left. Blocks within 500 ft (152.4 m) of the ward boundary, as in run_binary_event_study_permit.R.
bandwidth_m <- 152.4
bandwidth_label <- "500ft"

source("../../setup_environment/code/packages.R")

data <- arrow::read_parquet("../input/permit_block_year_panel_2015.parquet") |>
  dplyr::filter(dist_m <= bandwidth_m, relative_year >= -5L, relative_year <= 5L, !is.na(strictness_change_frozen),
    !is.na(ward_pair_id), ward_pair_id != "", stable_both) |>
  dplyr::mutate(
    stricter = as.integer(strictness_change_frozen > 0),
    lenient = as.integer(strictness_change_frozen < 0),
    post = as.integer(relative_year >= 0L),
    post_stricter = post * stricter,
    post_lenient = post * lenient,
    post_signed = post * (stricter - lenient)
  )
stopifnot(!anyDuplicated(data[c("block_id", "year")]), all(data$stricter + data$lenient <= 1L))

# Keep blocks with at least one high-discretion permit in 2010-2014.
pre_period_activity <- data |>
  dplyr::filter(relative_year < 0L) |>
  dplyr::summarise(pre_period_permit_volume = sum(n_high_discretion_application, na.rm = TRUE), .by = block_id)
data <- data |>
  dplyr::left_join(pre_period_activity, by = "block_id", relationship = "many-to-one") |>
  dplyr::filter(pre_period_permit_volume > 0)

outcomes <- c(high_discretion = "n_high_discretion_application", low_discretion = "n_low_discretion_nosigns_application")
comparisons <- c(both_sides = "ward_pair_id^year", original_ward = "ward_pair_side^year")
t_test_p <- function(estimate, std_error, df) 2 * stats::pt(-abs(estimate / std_error), df = df)

results <- list()
for (outcome in names(outcomes)) {
  for (comparison in names(comparisons)) {
    model_data <- dplyr::mutate(data, outcome = .data[[outcomes[[outcome]]]])
    fixed_effects <- paste("block_id +", comparisons[[comparison]])
    combined <- fixest::fepois(stats::as.formula(paste("outcome ~ post_signed |", fixed_effects)),
      data = model_data, cluster = ~ward_pair_id, notes = FALSE)
    separate <- fixest::fepois(stats::as.formula(paste("outcome ~ post_stricter + post_lenient |", fixed_effects)),
      data = model_data, cluster = ~ward_pair_id, notes = FALSE)
    b <- stats::coef(separate)
    v <- stats::vcov(separate)
    separate_df <- fixest::degrees_freedom(separate, type = "t")
    # The half difference puts the separate estimates on the combined model's scale; the symmetry test asks whether
    # the two directions are equal in size and opposite in sign.
    half_difference <- (b[["post_stricter"]] - b[["post_lenient"]]) / 2
    half_difference_se <- sqrt(v["post_stricter", "post_stricter"] + v["post_lenient", "post_lenient"] -
      2 * v["post_stricter", "post_lenient"]) / 2
    symmetry_se <- sqrt(v["post_stricter", "post_stricter"] + v["post_lenient", "post_lenient"] +
      2 * v["post_stricter", "post_lenient"])
    results[[paste(outcome, comparison)]] <- tibble::tibble(
      outcome, comparison,
      specification = c("combined", "stricter", "lenient", "half_difference"),
      estimate = c(stats::coef(combined)[["post_signed"]], b[["post_stricter"]], b[["post_lenient"]], half_difference),
      std_error = c(fixest::se(combined)[["post_signed"]], sqrt(v["post_stricter", "post_stricter"]),
        sqrt(v["post_lenient", "post_lenient"]), half_difference_se),
      df = c(fixest::degrees_freedom(combined, type = "t"), rep(separate_df, 3L)),
      symmetry_p_value = c(NA, NA, NA, t_test_p(b[["post_stricter"]] + b[["post_lenient"]], symmetry_se, separate_df)),
      observations = c(stats::nobs(combined), rep(stats::nobs(separate), 3L))
    )
  }
}
results <- dplyr::bind_rows(results) |> dplyr::mutate(p_value = t_test_p(estimate, std_error, df))

# One estimate row and one standard-error row, high-discretion then low-discretion permits.
table_rows <- function(label, comparison, specification) {
  rows <- results[results$comparison == comparison & results$specification == specification, ]
  stopifnot(identical(rows$outcome, names(outcomes)))
  stars <- dplyr::case_when(rows$p_value <= 0.01 ~ "***", rows$p_value <= 0.05 ~ "**", rows$p_value <= 0.10 ~ "*",
    TRUE ~ "")
  c(sprintf("%s & %s & %s \\\\", label, sprintf("%.3f%s", rows$estimate[1], stars[1]),
      sprintf("%.3f%s", rows$estimate[2], stars[2])),
    sprintf(" & (%.3f) & (%.3f) \\\\", rows$std_error[1], rows$std_error[2]))
}
symmetry <- results[results$comparison == "both_sides" & results$specification == "half_difference", ]
observations <- results[results$comparison == "both_sides" & results$specification == "combined", ]
stopifnot(identical(symmetry$outcome, names(outcomes)), identical(observations$outcome, names(outcomes)))

writeLines(c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & High-Discretion & Low-Discretion \\\\",
  "\\midrule",
  table_rows("Combined reassignment effect", "both_sides", "combined"),
  "\\addlinespace",
  "\\multicolumn{3}{l}{\\textit{Directions estimated separately}} \\\\",
  table_rows("Assigned to more stringent aldermen", "both_sides", "stricter"),
  table_rows("Assigned to more lenient aldermen", "both_sides", "lenient"),
  table_rows("One-half difference between directions", "both_sides", "half_difference"),
  sprintf("Equal-and-opposite test $p$-value & %.3f & %.3f \\\\", symmetry$symmetry_p_value[1],
    symmetry$symmetry_p_value[2]),
  "\\addlinespace",
  "\\multicolumn{3}{l}{\\textit{Comparison blocks from the original ward only}} \\\\",
  table_rows("Combined reassignment effect", "original_ward", "combined"),
  table_rows("Assigned to more stringent aldermen", "original_ward", "stricter"),
  table_rows("Assigned to more lenient aldermen", "original_ward", "lenient"),
  "\\midrule",
  "Block fixed effects & Yes & Yes \\\\",
  "Ward-pair $\\times$ year fixed effects & Yes & Yes \\\\",
  "Positive pre-period permit activity required & Yes & Yes \\\\",
  "\\midrule",
  sprintf("N & %s & %s \\\\", format(observations$observations[1], big.mark = ","),
    format(observations$observations[2], big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}"
), sprintf("../output/permit_event_study_appendix_%s.tex", bandwidth_label))
