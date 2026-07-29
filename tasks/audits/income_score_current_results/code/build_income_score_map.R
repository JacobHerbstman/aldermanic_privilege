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
  dplyr::transmute(
    alderman = stringr::str_squish(stringr::str_to_lower(alderman)),
    score
  )

if (
  nrow(scores) == 0L ||
    anyDuplicated(scores$alderman) ||
    any(is.na(scores$score))
) {
  stop("The selected score crosswalk failed validation.")
}

aldermen <- readr::read_csv(
  "../input/chicago_alderman_panel.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(month = zoo::as.yearmon(month)) |>
  dplyr::filter(month == zoo::as.yearmon("2022-01-01")) |>
  dplyr::transmute(
    ward,
    alderman = stringr::str_squish(stringr::str_to_lower(alderman))
  )

if (anyDuplicated(aldermen$ward)) {
  stop("The alderman panel is not unique by ward in January 2022.")
}

wards <- sf::st_read(
  "../input/ward_panel.gpkg",
  quiet = TRUE
) |>
  dplyr::filter(year == 2022L) |>
  dplyr::left_join(
    aldermen,
    by = "ward",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    scores,
    by = "alderman",
    relationship = "many-to-one"
  )

if (any(is.na(wards$score))) {
  stop("Some January 2022 wards lack a stringency score.")
}

plot <- ggplot2::ggplot(wards) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = score),
    color = "grey20",
    linewidth = 0.2
  ) +
  ggplot2::scale_fill_distiller(
    palette = "RdYlBu",
    direction = -1,
    name = "Regulatory Stringency",
    na.value = "grey90"
  ) +
  ggplot2::labs(
    title = "Regulatory Stringency by Ward"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

ggplot2::ggsave(
  "../output/income_score_map_2022.pdf",
  plot,
  width = 8,
  height = 10
)
