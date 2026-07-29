# setwd("tasks/audits/income_score_current_results/code")

source("../../../_lib/alderman_uncertainty_helpers.R")

permits <- load_uncertainty_permits(
  "../input/permits_for_uncertainty_index.csv"
) |>
  dplyr::filter(month <= zoo::as.yearmon("2022-12"))

result <- build_residualized_uncertainty_index(
  permits = permits,
  config = default_uncertainty_config(),
  variant_id = "income_added_back",
  stage1_outcome = "log_processing_time",
  drop_covariates = "share_bach_plus",
  construction_rule = paste(
    "Median household income included;",
    "bachelor's share omitted"
  )
)

labels <- c(
  median_hh_income_10k = "Median HH Income (\\$10k)",
  share_black = "Share Black",
  share_hisp = "Share Hispanic",
  share_white = "Share White",
  homeownership_rate = "Homeownership Rate",
  pop_total_10k = "Population (10k)",
  dist_cbd_km = "Dist. to CBD (km)",
  dist_lake_km = "Dist. to Lake (km)",
  n_rail_stations_800m = "CTA Stations (800m)",
  n_permits_wm_l1 = "Lag Permits"
)

terms <- result$stage1_terms |>
  dplyr::filter(term %in% names(labels)) |>
  dplyr::mutate(
    term = factor(term, levels = names(labels)),
    label = unname(labels[as.character(term)]),
    stars = dplyr::case_when(
      p_value <= 0.01 ~ "***",
      p_value <= 0.05 ~ "**",
      p_value <= 0.10 ~ "*",
      TRUE ~ ""
    )
  ) |>
  dplyr::arrange(term)

if (nrow(terms) != length(labels)) {
  stop("The income-only Stage 1 table is missing terms.")
}

table_rows <- unlist(
  lapply(seq_len(nrow(terms)), function(i) {
    c(
      sprintf(
        "%s & %.3f%s \\\\",
        terms$label[i],
        terms$estimate[i],
        terms$stars[i]
      ),
      sprintf(" & (%.3f) \\\\", terms$std_error[i])
    )
  })
)

writeLines(
  c(
    "\\begin{tabular}{lc}",
    "\\toprule",
    " & Log Processing Time \\\\",
    "\\midrule",
    table_rows,
    "\\midrule",
    "Year $\\times$ Month FE & $\\checkmark$ \\\\",
    "Permit Type FE & $\\checkmark$ \\\\",
    "Review Type FE & $\\checkmark$ \\\\",
    sprintf(
      "N & %s \\\\",
      format(result$metadata$stage1_nobs, big.mark = ",")
    ),
    "\\bottomrule",
    "\\end{tabular}"
  ),
  "../output/income_score_stage1_2022.tex"
)
