# setwd("tasks/audits/income_score_current_results/code")

source("../../../setup_environment/code/packages.R")

scores <- readr::read_csv(
  "../output/current_income_scores.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    cutoff == 2022L,
    variant == "income_added_back"
  )

if (nrow(scores) == 0L || anyDuplicated(scores$alderman)) {
  stop("The income-only score distribution failed validation.")
}

plot <- ggplot2::ggplot(scores, ggplot2::aes(score)) +
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
  "../output/income_score_distribution_2022.pdf",
  plot,
  width = 6.5,
  height = 4.2,
  bg = "white"
)
