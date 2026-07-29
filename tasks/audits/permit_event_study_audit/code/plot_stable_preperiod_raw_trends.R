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
    assignment = dplyr::case_when(
      score_change > 0 ~ "More stringent",
      score_change < 0 ~ "More lenient",
      TRUE ~ "Unchanged"
    )
  )

if (
  anyDuplicated(data[c("block_id", "year")]) ||
    any(is.na(data$score_origin)) ||
    any(is.na(data$score_dest))
) {
  stop("The stable event-study sample failed validation.")
}

pre_period <- data |>
  dplyr::filter(relative_year < 0L) |>
  dplyr::group_by(block_id) |>
  dplyr::summarise(
    pre_period_high_discretion = sum(
      n_high_discretion_application,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    permit_history = dplyr::if_else(
      pre_period_high_discretion == 0,
      "No high-discretion permits, 2010–2014",
      "At least one high-discretion permit, 2010–2014"
    )
  )

data <- data |>
  dplyr::left_join(
    pre_period,
    by = "block_id",
    relationship = "many-to-one"
  )

long_data <- dplyr::bind_rows(
  data |>
    dplyr::transmute(
      block_id,
      ward_pair_id,
      year,
      assignment,
      permit_history,
      outcome = "High-discretion permits",
      permits = n_high_discretion_application
    ),
  data |>
    dplyr::transmute(
      block_id,
      ward_pair_id,
      year,
      assignment,
      permit_history,
      outcome = "Low-discretion permits",
      permits = n_low_discretion_nosigns_application
    )
) |>
  dplyr::mutate(
    permit_history = factor(
      permit_history,
      levels = c(
        "At least one high-discretion permit, 2010–2014",
        "No high-discretion permits, 2010–2014"
      )
    ),
    assignment = factor(
      assignment,
      levels = c("Unchanged", "More stringent", "More lenient")
    )
  )

raw_measures <- long_data |>
  dplyr::group_by(outcome, permit_history, year) |>
  dplyr::summarise(
    `Mean permits per block` = mean(permits),
    `Share of blocks with any permit` = mean(permits > 0),
    `Mean permits among blocks with a permit` = mean(
      permits[permits > 0]
    ),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = c(
      "Mean permits per block",
      "Share of blocks with any permit",
      "Mean permits among blocks with a permit"
    ),
    names_to = "measure",
    values_to = "value"
  ) |>
  dplyr::mutate(
    measure = factor(
      measure,
      levels = c(
        "Mean permits per block",
        "Share of blocks with any permit",
        "Mean permits among blocks with a permit"
      )
    )
  )

history_colors <- c(
  "At least one high-discretion permit, 2010–2014" = "#2B6F92",
  "No high-discretion permits, 2010–2014" = "#B5483A"
)

raw_plot <- ggplot2::ggplot(
  raw_measures,
  ggplot2::aes(
    year,
    value,
    color = permit_history,
    group = permit_history
  )
) +
  ggplot2::geom_vline(
    xintercept = 2014.5,
    linetype = "dashed",
    color = "gray55",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(measure),
    cols = ggplot2::vars(outcome),
    scales = "free_y"
  ) +
  ggplot2::scale_color_manual(values = history_colors) +
  ggplot2::scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  ggplot2::labs(
    title = "Raw permit trends by pre-redistricting permit history",
    subtitle = paste(
      "Stable-incumbent blocks within 500 feet of a ward boundary;",
      "the dashed line marks the 2015 remap"
    ),
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  "../output/stable_preperiod_raw_trends_500ft.png",
  raw_plot,
  width = 12,
  height = 9,
  dpi = 180,
  bg = "white"
)

assignment_means <- long_data |>
  dplyr::group_by(
    outcome,
    permit_history,
    assignment,
    year
  ) |>
  dplyr::summarise(
    mean_permits = mean(permits),
    blocks = dplyr::n(),
    .groups = "drop"
  )

assignment_colors <- c(
  "Unchanged" = "#5A5A5A",
  "More stringent" = "#176B58",
  "More lenient" = "#B0473E"
)

assignment_plot <- ggplot2::ggplot(
  assignment_means,
  ggplot2::aes(
    year,
    mean_permits,
    color = assignment,
    group = assignment
  )
) +
  ggplot2::geom_vline(
    xintercept = 2014.5,
    linetype = "dashed",
    color = "gray55",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(outcome),
    cols = ggplot2::vars(permit_history),
    scales = "free_y"
  ) +
  ggplot2::scale_color_manual(values = assignment_colors) +
  ggplot2::scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  ggplot2::labs(
    title = "Raw permit counts by assignment and prior permit history",
    subtitle = paste(
      "Annual mean permits per block;",
      "the dashed line marks the 2015 remap"
    ),
    x = NULL,
    y = "Mean permits per block",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  "../output/stable_preperiod_raw_assignment_trends_500ft.png",
  assignment_plot,
  width = 12,
  height = 7.5,
  dpi = 180,
  bg = "white"
)

pair_means <- long_data |>
  dplyr::group_by(
    outcome,
    permit_history,
    ward_pair_id,
    year,
    assignment
  ) |>
  dplyr::summarise(
    mean_permits = mean(permits),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = assignment,
    values_from = mean_permits
  )

pair_differences <- dplyr::bind_rows(
  pair_means |>
    dplyr::filter(
      is.finite(`More stringent`),
      is.finite(Unchanged)
    ) |>
    dplyr::transmute(
      outcome,
      permit_history,
      ward_pair_id,
      year,
      direction = "More stringent minus unchanged",
      difference = `More stringent` - Unchanged
    ),
  pair_means |>
    dplyr::filter(
      is.finite(`More lenient`),
      is.finite(Unchanged)
    ) |>
    dplyr::transmute(
      outcome,
      permit_history,
      ward_pair_id,
      year,
      direction = "More lenient minus unchanged",
      difference = `More lenient` - Unchanged
    )
) |>
  dplyr::group_by(
    outcome,
    permit_history,
    year,
    direction
  ) |>
  dplyr::summarise(
    difference = mean(difference),
    standard_error = stats::sd(difference) / sqrt(dplyr::n()),
    pairs = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    ci_low = difference - 1.96 * standard_error,
    ci_high = difference + 1.96 * standard_error
  )

difference_colors <- c(
  "More stringent minus unchanged" = "#176B58",
  "More lenient minus unchanged" = "#B0473E"
)

difference_plot <- ggplot2::ggplot(
  pair_differences,
  ggplot2::aes(
    year,
    difference,
    color = direction,
    fill = direction,
    group = direction
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = "gray50",
    linewidth = 0.4
  ) +
  ggplot2::geom_vline(
    xintercept = 2014.5,
    linetype = "dashed",
    color = "gray55",
    linewidth = 0.4
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = ci_low, ymax = ci_high),
    alpha = 0.15,
    color = NA
  ) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(outcome),
    cols = ggplot2::vars(permit_history),
    scales = "free_y"
  ) +
  ggplot2::scale_color_manual(values = difference_colors) +
  ggplot2::scale_fill_manual(values = difference_colors) +
  ggplot2::scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  ggplot2::labs(
    title = "Within-pair differences in raw permit counts",
    subtitle = paste(
      "Equal-weighted ward-pair differences;",
      "bands are descriptive 95% intervals"
    ),
    x = NULL,
    y = "Difference in mean permits per block",
    color = NULL,
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  "../output/stable_preperiod_raw_pair_differences_500ft.png",
  difference_plot,
  width = 12,
  height = 7.5,
  dpi = 180,
  bg = "white"
)
