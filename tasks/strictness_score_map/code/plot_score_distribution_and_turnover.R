# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/strictness_score_map/code")
# uncertainty_spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(uncertainty_spec)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <uncertainty_spec>.", call. = FALSE)
}
uncertainty_spec <- cli_args[1]

scores <- readr::read_csv(
  sprintf("../input/alderman_uncertainty_index_%s.csv", uncertainty_spec),
  show_col_types = FALSE
) |>
  dplyr::transmute(
    alderman = stringr::str_squish(stringr::str_to_lower(alderman)),
    score = as.numeric(uncertainty_index)
  )

if (
  nrow(scores) == 0L ||
    anyDuplicated(scores$alderman) > 0L ||
    anyNA(scores$alderman) ||
    anyNA(scores$score)
) {
  stop("Alderman scores failed validation.")
}

distribution_plot <- ggplot2::ggplot(scores, ggplot2::aes(score)) +
  ggplot2::geom_vline(
    xintercept = 0,
    color = "gray45",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  ggplot2::geom_histogram(
    bins = 18,
    fill = "#3B7EA1",
    color = "white",
    linewidth = 0.35
  ) +
  ggplot2::labs(
    x = "Standardized aldermanic stringency score",
    y = "Number of aldermen"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

ggplot2::ggsave(
  sprintf(
    "../output/uncertainty_score_distribution_%s.pdf",
    uncertainty_spec
  ),
  distribution_plot,
  width = 6.5,
  height = 4.2,
  bg = "white"
)

panel <- readr::read_csv(
  "../input/chicago_alderman_panel.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(
    month = zoo::as.yearmon(month),
    alderman = stringr::str_squish(stringr::str_to_lower(alderman))
  )

if (anyDuplicated(panel[c("ward", "month")]) > 0L) {
  stop("Alderman panel must be unique by ward and month.")
}

pairs <- panel |>
  dplyr::filter(month >= zoo::as.yearmon("2006-01")) |>
  dplyr::summarise(first_month = min(month), .by = c(ward, alderman)) |>
  dplyr::arrange(ward, first_month) |>
  dplyr::mutate(
    predecessor = alderman,
    successor = dplyr::lead(alderman),
    .by = ward
  ) |>
  dplyr::filter(!is.na(successor)) |>
  dplyr::select(ward, predecessor, successor) |>
  dplyr::inner_join(
    scores |>
      dplyr::rename(
        predecessor = alderman,
        predecessor_score = score
      ),
    by = "predecessor",
    relationship = "many-to-one"
  ) |>
  dplyr::inner_join(
    scores |>
      dplyr::rename(
        successor = alderman,
        successor_score = score
      ),
    by = "successor",
    relationship = "many-to-one"
  )

if (nrow(pairs) < 3L) {
  stop("Not enough predecessor-successor pairs to plot.")
}

correlation <- stats::cor(
  pairs$predecessor_score,
  pairs$successor_score
)

turnover_plot <- ggplot2::ggplot(
  pairs,
  ggplot2::aes(predecessor_score, successor_score)
) +
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "gray55"
  ) +
  ggplot2::geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#3B7EA1",
    fill = "#3B7EA1",
    alpha = 0.15
  ) +
  ggplot2::geom_point(
    color = "#B4473D",
    size = 2.4,
    alpha = 0.75
  ) +
  ggplot2::labs(
    subtitle = sprintf("Correlation = %.2f", correlation),
    x = "Predecessor stringency score",
    y = "Successor stringency score"
  ) +
  ggplot2::coord_fixed() +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_text(size = 14, face = "bold")
  )

ggplot2::ggsave(
  sprintf(
    "../output/uncertainty_score_predecessor_successor_%s.pdf",
    uncertainty_spec
  ),
  turnover_plot,
  width = 6,
  height = 5.5,
  bg = "white"
)
