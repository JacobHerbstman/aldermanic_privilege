# setwd("tasks/density_appendix_results/code")
# check_type <- "placebo_neg1000ft"
# bins_per_side <- 5L

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0L) {
  cli_args <- c(check_type, bins_per_side)
}
if (length(cli_args) != 2L) {
  stop("Expected a check type and bins per side.")
}

check_type <- cli_args[1]
bins_per_side <- as.integer(cli_args[2])

valid_checks <- c(
  "placebo_neg1000ft",
  "placebo_pos1000ft",
  "donut25ft",
  "donut50ft"
)
if (
  !check_type %in% valid_checks ||
    is.na(bins_per_side) ||
    bins_per_side < 2L
) {
  stop("Invalid density appendix specification.")
}

projects <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(
    score_own = strictness_own,
    score_neighbor = strictness_neighbor,
    true_side = side,
    true_distance_ft = signed_distance_m / 0.3048
  )

if (grepl("^placebo_", check_type)) {
  cutoff_ft <- if (check_type == "placebo_neg1000ft") {
    -1000
  } else {
    1000
  }
  projects <- projects |>
    dplyr::mutate(running_distance_ft = true_distance_ft - cutoff_ft)
  check_label <- if (cutoff_ft < 0) {
    "Cutoff shifted 1000ft into lenient side"
  } else {
    "Cutoff shifted 1000ft into stringent side"
  }
} else {
  donut_ft <- if (check_type == "donut25ft") 25 else 50
  projects <- projects |>
    dplyr::mutate(running_distance_ft = true_distance_ft)
  check_label <- sprintf("%dft donut", donut_ft)
}

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

panel_specs <- tibble::tribble(
  ~sample, ~outcome, ~panel_title,
  "all", "density_far", "All construction: Log(FAR)",
  "multifamily", "density_far", "Multifamily: Log(FAR)",
  "all", "density_dupac", "All construction: Log(DUPAC)",
  "multifamily", "density_dupac", "Multifamily: Log(DUPAC)"
)

panels <- vector("list", nrow(panel_specs))

for (i in seq_len(nrow(panel_specs))) {
  sample_name <- panel_specs$sample[i]
  outcome <- panel_specs$outcome[i]

  model_data <- projects |>
    dplyr::filter(
      construction_year >= 2006L,
      construction_year <= 2022L,
      dwelling_units > 0,
      sample_name == "all" | external_multifamily,
      allow_far,
      allow_dupac,
      is.finite(density_far),
      density_far > 0,
      is.finite(density_dupac),
      density_dupac > 0,
      is.finite(score_own),
      is.finite(score_neighbor),
      is.finite(pair_average_score),
      dplyr::if_all(
        dplyr::all_of(demographic_controls),
        is.finite
      ),
      !is.na(zone_group),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      ward_pair != "",
      abs(running_distance_ft) <= 500
    )

  if (grepl("^donut", check_type)) {
    model_data <- model_data |>
      dplyr::filter(abs(running_distance_ft) >= donut_ft)
  }

  model_data <- model_data |>
    dplyr::mutate(
      outcome_value = log(.data[[outcome]]),
      side = as.integer(running_distance_ft > 0),
      running_distance_m = running_distance_ft * 0.3048,
      lenient_dist = abs(running_distance_m) *
        as.integer(side == 0L),
      strict_dist = abs(running_distance_m) *
        as.integer(side == 1L)
    )

  model <- fixest::feols(
    outcome_value ~
      side +
      pair_average_score +
      lenient_dist +
      strict_dist +
      share_white_own +
      share_black_own +
      median_hh_income_own +
      share_bach_plus_own +
      homeownership_rate_own |
      zone_group + segment_id + construction_year,
    data = model_data,
    cluster = ~ward_pair
  )

  model_row <- fixest::coeftable(model)["side", ]
  estimate <- unname(model_row["Estimate"])
  standard_error <- unname(model_row["Std. Error"])
  p_value <- unname(model_row["Pr(>|t|)"])
  stars <- dplyr::case_when(
    p_value <= 0.01 ~ "***",
    p_value <= 0.05 ~ "**",
    p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )

  residual_model <- fixest::feols(
    outcome_value ~
      pair_average_score +
      share_white_own +
      share_black_own +
      median_hh_income_own +
      share_bach_plus_own +
      homeownership_rate_own |
      zone_group + segment_id + construction_year,
    data = model_data
  )

  removed <- residual_model$obs_selection$obsRemoved
  keep_rows <- if (is.null(removed)) {
    seq_len(nrow(model_data))
  } else {
    setdiff(seq_len(nrow(model_data)), abs(as.integer(removed)))
  }

  display_data <- model_data[keep_rows, , drop = FALSE] |>
    dplyr::mutate(
      residualized_outcome = as.numeric(
        stats::resid(residual_model)
      )
    )

  display_model <- fixest::feols(
    residualized_outcome ~ side * running_distance_ft,
    data = display_data,
    cluster = ~ward_pair
  )

  breaks_ft <- seq(
    -500,
    500,
    length.out = 2L * bins_per_side + 1L
  )
  bin_width_ft <- 500 / bins_per_side

  bins <- display_data |>
    dplyr::mutate(
      bin = pmin(
        findInterval(
          running_distance_ft,
          breaks_ft,
          rightmost.closed = TRUE,
          all.inside = TRUE
        ),
        length(breaks_ft) - 1L
      ),
      bin_center_ft = breaks_ft[bin] + bin_width_ft / 2
    ) |>
    dplyr::group_by(bin, bin_center_ft, side) |>
    dplyr::summarise(
      mean_outcome = mean(residualized_outcome),
      .groups = "drop"
    )

  if (grepl("^donut", check_type)) {
    line_distance <- c(
      seq(-500, -donut_ft, length.out = 180),
      seq(donut_ft, 500, length.out = 180)
    )
  } else {
    line_distance <- c(
      seq(-500, 0, length.out = 180),
      seq(0, 500, length.out = 180)[-1]
    )
  }

  line_data <- tibble::tibble(
    running_distance_ft = line_distance
  ) |>
    dplyr::mutate(side = as.integer(running_distance_ft > 0))

  design_matrix <- stats::model.matrix(
    ~side * running_distance_ft,
    data = line_data
  )
  design_matrix <- design_matrix[
    ,
    names(stats::coef(display_model)),
    drop = FALSE
  ]
  model_vcov <- stats::vcov(display_model)
  critical_value <- stats::qt(
    0.975,
    df = max(
      dplyr::n_distinct(display_data$ward_pair) - 1L,
      1L
    )
  )

  line_data <- line_data |>
    dplyr::mutate(
      fitted = as.numeric(
        design_matrix %*% stats::coef(display_model)
      ),
      fitted_se = sqrt(pmax(
        rowSums(
          (design_matrix %*% model_vcov) * design_matrix
        ),
        0
      )),
      lower = fitted - critical_value * fitted_se,
      upper = fitted + critical_value * fitted_se
    )

  panels[[i]] <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = line_data,
      ggplot2::aes(
        running_distance_ft,
        ymin = lower,
        ymax = upper,
        fill = factor(side)
      ),
      alpha = 0.14,
      color = NA
    ) +
    ggplot2::geom_line(
      data = line_data,
      ggplot2::aes(
        running_distance_ft,
        fitted,
        color = factor(side)
      ),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(
        bin_center_ft,
        mean_outcome,
        fill = factor(side)
      ),
      shape = 21,
      color = "white",
      stroke = 0.4,
      size = 2.1
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray35",
      linewidth = 0.5
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "gray65",
      linewidth = 0.4
    ) +
    ggplot2::scale_color_manual(
      values = c("0" = "#1F77B4", "1" = "#D62728"),
      guide = "none"
    ) +
    ggplot2::scale_fill_manual(
      values = c("0" = "#1F77B4", "1" = "#D62728"),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-500, 500),
      breaks = c(-500, -250, 0, 250, 500)
    ) +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = sprintf(
        "Estimate = %.3f%s (SE %.3f), N = %s",
        estimate,
        stars,
        standard_error,
        format(stats::nobs(model), big.mark = ",")
      ),
      x = "Distance to cutoff (feet)",
      y = "Residualized log density"
    ) +
    ggplot2::theme_minimal(base_size = 9.5) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

combined <- (panels[[1]] + panels[[2]]) /
  (panels[[3]] + panels[[4]]) +
  patchwork::plot_annotation(title = check_label)

ggplot2::ggsave(
  sprintf("../output/density_%s.pdf", check_type),
  combined,
  width = 10.5,
  height = 7.4,
  bg = "white"
)
