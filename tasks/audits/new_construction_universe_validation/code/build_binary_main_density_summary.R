# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    cutoff == 2022L,
    variant == "income_added_back"
  ) |>
  dplyr::select(alderman, score)

if (
  nrow(scores) == 0L ||
    anyDuplicated(scores$alderman) ||
    any(is.na(scores$score))
) {
  stop("The selected alderman score crosswalk failed validation.")
}

data <- readr::read_csv(
  "../output/provisional_validated_density_input.csv",
  show_col_types = FALSE,
  col_select = c(
    construction_year,
    ward_pair,
    distance_to_boundary_ft,
    within_500ft,
    allow_far,
    allow_dupac,
    segment_id,
    dwelling_units,
    density_far,
    density_dupac,
    zone_group,
    alderman_own,
    alderman_neighbor,
    share_white_own,
    share_black_own,
    median_hh_income_own,
    share_bach_plus_own,
    homeownership_rate_own,
    external_multifamily
  )
) |>
  dplyr::select(
    -dplyr::any_of(c("score_own", "score_neighbor"))
  ) |>
  dplyr::left_join(
    scores |>
      dplyr::rename(alderman_own = alderman, score_own = score),
    by = "alderman_own",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    scores |>
      dplyr::rename(alderman_neighbor = alderman, score_neighbor = score),
    by = "alderman_neighbor",
    relationship = "many-to-one"
  ) |>
  dplyr::filter(
    construction_year >= 2006,
    construction_year <= 2022,
    within_500ft,
    dwelling_units > 0,
    allow_far,
    allow_dupac,
    is.finite(density_far),
    density_far > 0,
    is.finite(density_dupac),
    density_dupac > 0,
    is.finite(score_own),
    is.finite(score_neighbor),
    is.finite(share_white_own),
    is.finite(share_black_own),
    is.finite(median_hh_income_own),
    is.finite(share_bach_plus_own),
    is.finite(homeownership_rate_own),
    !is.na(zone_group),
    !is.na(segment_id),
    segment_id != ""
  )

summaries <- dplyr::bind_rows(
  data |>
    dplyr::mutate(sample = "All Construction"),
  data |>
    dplyr::filter(external_multifamily) |>
    dplyr::mutate(sample = "Multifamily")
) |>
  dplyr::group_by(sample) |>
  dplyr::summarise(
    average_far = mean(density_far),
    average_units = mean(dwelling_units),
    average_dupac = mean(density_dupac),
    median_distance = median(abs(distance_to_boundary_ft)),
    ward_pairs = dplyr::n_distinct(ward_pair),
    n = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(factor(sample, c("All Construction", "Multifamily")))

if (
  nrow(summaries) != 2L ||
    !identical(summaries$n, c(3752L, 850L))
) {
  stop("The summary sample does not match the binary density regressions.")
}

table_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Summary Statistics for the Density Analysis Sample}",
  "\\label{tab:summary_stats}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & All New Construction & Multifamily \\\\",
  "\\midrule",
  sprintf(
    "Average FAR & %.2f & %.2f \\\\",
    summaries$average_far[1],
    summaries$average_far[2]
  ),
  sprintf(
    "Average Units & %.2f & %.2f \\\\",
    summaries$average_units[1],
    summaries$average_units[2]
  ),
  sprintf(
    "Average DUPAC & %.2f & %.2f \\\\",
    summaries$average_dupac[1],
    summaries$average_dupac[2]
  ),
  sprintf(
    "Median Distance to Boundary (ft) & %.0f & %.0f \\\\",
    summaries$median_distance[1],
    summaries$median_distance[2]
  ),
  sprintf(
    "Ward Pairs & %s & %s \\\\",
    summaries$ward_pairs[1],
    summaries$ward_pairs[2]
  ),
  "\\midrule",
  sprintf(
    "N & %s & %s \\\\",
    trimws(format(summaries$n[1], big.mark = ",")),
    trimws(format(summaries$n[2], big.mark = ","))
  ),
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize ",
    "Notes: Samples include new residential construction from 2006--2022 ",
    "within 500ft of ward boundaries and use the common FAR-DUPAC sample ",
    "from Table~\\ref{tab:density_main_table}. Multifamily status follows ",
    "the final project classification used in the density analysis.}"
  ),
  "\\end{table}"
)

writeLines(
  table_lines,
  "../output/binary_density_main_summary_income_added_back.tex"
)
