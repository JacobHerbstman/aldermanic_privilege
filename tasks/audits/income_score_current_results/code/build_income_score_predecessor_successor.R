# setwd("tasks/audits/income_score_current_results/code")

source("../../../setup_environment/code/packages.R")

scores <- readr::read_csv(
  "../output/current_income_scores.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    cutoff == 2022L,
    variant == "income_added_back"
  ) |>
  dplyr::select(alderman, score)

panel <- readr::read_csv(
  "../input/chicago_alderman_panel.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(month = zoo::as.yearmon(month))

if (
  anyDuplicated(scores$alderman) ||
    anyDuplicated(panel[c("ward", "month")])
) {
  stop("The predecessor-successor inputs failed validation.")
}

pairs <- panel |>
  dplyr::filter(month >= zoo::as.yearmon("2006-01")) |>
  dplyr::group_by(ward, alderman) |>
  dplyr::summarise(first_month = min(month), .groups = "drop") |>
  dplyr::arrange(ward, first_month) |>
  dplyr::group_by(ward) |>
  dplyr::mutate(
    predecessor = alderman,
    successor = dplyr::lead(alderman)
  ) |>
  dplyr::ungroup() |>
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

plot <- ggplot2::ggplot(
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
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

ggplot2::ggsave(
  "../output/income_score_predecessor_successor_2022.pdf",
  plot,
  width = 6,
  height = 5.5,
  bg = "white"
)
