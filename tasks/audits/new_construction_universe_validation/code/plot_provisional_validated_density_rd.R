# setwd("tasks/audits/new_construction_universe_validation/code")
# score_variant <- "all_covariates"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0L) {
  cli_args <- score_variant
}
if (length(cli_args) != 1L) {
  stop("Expected one score variant.", call. = FALSE)
}
score_variant <- cli_args[1]

valid_variants <- c(
  "current_no_income",
  "education_added_back",
  "income_added_back",
  "all_covariates"
)
if (!score_variant %in% valid_variants) {
  stop("Unknown score variant.", call. = FALSE)
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
  ),
  col_types = readr::cols(
    construction_year = readr::col_double(),
    ward_pair = readr::col_character(),
    distance_to_boundary_ft = readr::col_double(),
    within_500ft = readr::col_logical(),
    allow_far = readr::col_logical(),
    allow_dupac = readr::col_logical(),
    segment_id = readr::col_character(),
    dwelling_units = readr::col_double(),
    density_far = readr::col_double(),
    density_dupac = readr::col_double(),
    zone_group = readr::col_character(),
    alderman_own = readr::col_character(),
    alderman_neighbor = readr::col_character(),
    share_white_own = readr::col_double(),
    share_black_own = readr::col_double(),
    median_hh_income_own = readr::col_double(),
    share_bach_plus_own = readr::col_double(),
    homeownership_rate_own = readr::col_double(),
    external_multifamily = readr::col_logical()
  )
)

scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(cutoff == 2022, variant == score_variant) |>
  dplyr::select(alderman, score)

if (anyDuplicated(scores$alderman)) {
  stop("Score file has duplicate aldermen.", call. = FALSE)
}

score_map <- tibble::deframe(scores)

data <- data |>
  dplyr::mutate(
    score_own = unname(score_map[alderman_own]),
    score_neighbor = unname(score_map[alderman_neighbor]),
    side = as.integer(score_own > score_neighbor),
    running_distance_ft = ifelse(
      side == 1L,
      abs(distance_to_boundary_ft),
      -abs(distance_to_boundary_ft)
    ),
    lenient_dist = abs(distance_to_boundary_ft * 0.3048) *
      as.integer(side == 0L),
    strict_dist = abs(distance_to_boundary_ft * 0.3048) *
      as.integer(side == 1L),
    pair_average_score = (score_own + score_neighbor) / 2
  )

stored_results <- readr::read_csv(
  "../output/provisional_validated_density_results.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    variant == score_variant,
    sample_rule == "common_density",
    treatment == "binary",
    cluster_level == "ward_pair"
  )

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

panel_specs <- tibble::tribble(
  ~sample,       ~outcome,        ~panel_title,
  "all",         "density_far",   "All construction: Log(FAR)",
  "multifamily", "density_far",   "Multifamily: Log(FAR)",
  "all",         "density_dupac", "All construction: Log(DUPAC)",
  "multifamily", "density_dupac", "Multifamily: Log(DUPAC)"
)

panels <- vector("list", nrow(panel_specs))
plot_estimates <- vector("list", nrow(panel_specs))

for (i in seq_len(nrow(panel_specs))) {
  sample_name <- panel_specs$sample[i]
  outcome <- panel_specs$outcome[i]
  eligibility_field <- if (outcome == "density_far") "allow_far" else "allow_dupac"

  model_data <- data |>
    dplyr::filter(
      construction_year >= 2006,
      construction_year <= 2022,
      within_500ft,
      dwelling_units > 0,
      sample_name == "all" | external_multifamily,
      allow_far,
      allow_dupac,
      is.finite(density_far),
      density_far > 0,
      is.finite(density_dupac),
      density_dupac > 0,
      .data[[eligibility_field]],
      is.finite(.data[[outcome]]),
      .data[[outcome]] > 0,
      is.finite(score_own),
      is.finite(score_neighbor),
      is.finite(pair_average_score),
      dplyr::if_all(dplyr::all_of(demographic_controls), is.finite),
      !is.na(zone_group),
      !is.na(segment_id),
      segment_id != ""
    ) |>
    dplyr::mutate(log_outcome = log(.data[[outcome]]))

  main_model <- fixest::feols(
    log_outcome ~
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

  expected <- stored_results |>
    dplyr::filter(
      sample == .env$sample_name,
      .data$outcome == .env$outcome
    )
  if (nrow(expected) != 1L) {
    stop("Could not identify the stored binary estimate.", call. = FALSE)
  }

  main_row <- fixest::coeftable(main_model)["side", ]
  if (
    abs(unname(main_row["Estimate"]) - expected$estimate) > 1e-8 ||
      abs(unname(main_row["Std. Error"]) - expected$se) > 1e-8 ||
      stats::nobs(main_model) != expected$n_obs
  ) {
    stop("Plot sample does not reproduce the stored regression.", call. = FALSE)
  }

  residual_model <- fixest::feols(
    log_outcome ~
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
    dplyr::mutate(residualized_outcome = as.numeric(stats::resid(residual_model)))

  display_model <- fixest::feols(
    residualized_outcome ~ side * running_distance_ft,
    data = display_data,
    cluster = ~ward_pair
  )

  display_row <- fixest::coeftable(display_model)["side", ]
  display_estimate <- unname(display_row["Estimate"])
  display_se <- unname(display_row["Std. Error"])
  display_p <- unname(display_row["Pr(>|t|)"])
  display_stars <- dplyr::case_when(
    display_p <= 0.01 ~ "***",
    display_p <= 0.05 ~ "**",
    display_p <= 0.10 ~ "*",
    TRUE ~ ""
  )

  plot_estimates[[i]] <- tibble::tibble(
    variant = score_variant,
    sample_rule = "common_density",
    sample = sample_name,
    outcome,
    visual_estimate = display_estimate,
    visual_se = display_se,
    visual_p_value = display_p,
    regression_estimate = expected$estimate,
    regression_se = expected$se,
    regression_p_value = expected$p_value,
    n_obs = expected$n_obs
  )

  display_data <- display_data |>
    dplyr::mutate(
      bin = dplyr::case_when(
        running_distance_ft < 0 ~
          pmax(floor((running_distance_ft + 500) / 100) + 1L, 1L),
        TRUE ~ pmin(floor(running_distance_ft / 100) + 6L, 10L)
      ),
      bin_center_ft = c(
        -450, -350, -250, -150, -50,
        50, 150, 250, 350, 450
      )[bin]
    )

  bins <- display_data |>
    dplyr::group_by(bin, bin_center_ft, side) |>
    dplyr::summarise(
      mean_outcome = mean(residualized_outcome),
      .groups = "drop"
    )

  line_data <- tibble::tibble(
    running_distance_ft = c(
      seq(-500, 0, length.out = 201),
      seq(0, 500, length.out = 201)[-1]
    )
  ) |>
    dplyr::mutate(side = as.integer(running_distance_ft > 0))

  design_matrix <- stats::model.matrix(
    ~side * running_distance_ft,
    data = line_data
  )
  design_matrix <- design_matrix[, names(stats::coef(display_model)), drop = FALSE]
  model_vcov <- stats::vcov(display_model)
  critical_value <- stats::qt(
    0.975,
    df = max(dplyr::n_distinct(display_data$ward_pair) - 1L, 1L)
  )

  line_data <- line_data |>
    dplyr::mutate(
      fitted = as.numeric(design_matrix %*% stats::coef(display_model)),
      fitted_se = sqrt(pmax(
        rowSums((design_matrix %*% model_vcov) * design_matrix),
        0
      )),
      lower = fitted - critical_value * fitted_se,
      upper = fitted + critical_value * fitted_se
    )

  panels[[i]] <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = line_data,
      ggplot2::aes(
        x = running_distance_ft,
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
        x = running_distance_ft,
        y = fitted,
        color = factor(side)
      ),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(
        x = bin_center_ft,
        y = mean_outcome,
        fill = factor(side)
      ),
      shape = 21,
      color = "white",
      stroke = 0.4,
      size = 2.2
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray35",
      linewidth = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "gray60",
      linewidth = 0.35
    ) +
    ggplot2::scale_color_manual(
      values = c("0" = "#1f77b4", "1" = "#d62728"),
      guide = "none"
    ) +
    ggplot2::scale_fill_manual(
      values = c("0" = "#1f77b4", "1" = "#d62728"),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-500, 500),
      breaks = c(-500, -250, 0, 250, 500)
    ) +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = sprintf(
        "Visual estimate = %.3f%s (SE %.3f)",
        display_estimate,
        display_stars,
        display_se
      ),
      x = "Distance to ward boundary (feet)",
      y = "Residualized log density"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )
}

variant_title <- dplyr::case_when(
  score_variant == "current_no_income" ~
    "Score excludes income and bachelor's share",
  score_variant == "education_added_back" ~
    "Score includes bachelor's share, excludes income",
  score_variant == "income_added_back" ~
    "Score includes income, excludes bachelor's share",
  score_variant == "all_covariates" ~
    "Score includes income and bachelor's share"
)

combined_plot <- (
  panels[[1]] | panels[[2]]
) / (
  panels[[3]] | panels[[4]]
) +
  patchwork::plot_annotation(
    title = variant_title,
    subtitle = paste(
      "Corrected audited construction sample; 500ft bandwidth.",
      "Visual estimates are residualized local-linear discontinuities."
    )
  )

ggplot2::ggsave(
  paste0("../output/audited_density_rd_", score_variant, ".pdf"),
  combined_plot,
  width = 11.2,
  height = 8.4
)

readr::write_csv(
  dplyr::bind_rows(plot_estimates),
  paste0("../output/audited_density_rd_", score_variant, "_estimates.csv")
)
