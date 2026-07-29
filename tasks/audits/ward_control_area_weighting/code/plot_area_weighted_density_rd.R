# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/ward_control_area_weighting/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

scores <- read_csv(
  "../output/alderman_scores_area_weighted_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index)

if (anyDuplicated(scores$alderman) > 0) {
  stop("Area-weighted scores are not unique by alderman.", call. = FALSE)
}

base_dat <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  select(-strictness_own, -strictness_neighbor, -sign, -signed_distance, -signed_distance_m) %>%
  left_join(
    scores,
    by = c("alderman_own" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(strictness_own = score) %>%
  left_join(
    scores,
    by = c("alderman_neighbor" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_distance_m = dist_to_boundary_m * sign,
    pair_average_score = (strictness_own + strictness_neighbor) / 2,
    zone_group = construction_zone_group
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(segment_id),
    segment_id != "",
    !is.na(zone_group),
    abs(signed_distance_m) <= 152.4
  )

controls <- c(
  "pair_average_score",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

panel_specs <- tribble(
  ~outcome, ~sample, ~title,
  "density_far", "all", "All Construction: Log(FAR)",
  "density_far", "multifamily", "Multifamily: Log(FAR)",
  "density_dupac", "all", "All Construction: Log(DUPAC)",
  "density_dupac", "multifamily", "Multifamily: Log(DUPAC)"
)

panels <- vector("list", nrow(panel_specs))
visual_rows <- vector("list", nrow(panel_specs))

for (i in seq_len(nrow(panel_specs))) {
  outcome <- panel_specs$outcome[i]
  sample <- panel_specs$sample[i]

  data <- base_dat %>%
    filter(if (sample == "all") unitscount > 0 else unitscount > 1) %>%
    filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
    mutate(
      log_outcome = log(.data[[outcome]]),
      running_distance = signed_distance_m,
      side = as.integer(running_distance > 0)
    )

  residual_model <- feols(
    as.formula(sprintf(
      "log_outcome ~ %s | zone_group + segment_id + construction_year",
      paste(controls, collapse = " + ")
    )),
    data = data
  )

  removed <- residual_model$obs_selection$obsRemoved
  keep_rows <- if (is.null(removed)) {
    seq_len(nrow(data))
  } else {
    setdiff(seq_len(nrow(data)), abs(as.integer(removed)))
  }

  display_data <- data[keep_rows, , drop = FALSE] %>%
    mutate(residualized_outcome = as.numeric(resid(residual_model)))

  display_model <- feols(
    residualized_outcome ~ side * running_distance,
    data = display_data,
    cluster = ~ward_pair
  )
  display_row <- coeftable(display_model)["side", , drop = FALSE]
  estimate <- unname(display_row[1, "Estimate"])
  std_error <- unname(display_row[1, "Std. Error"])
  p_value <- unname(display_row[1, "Pr(>|t|)"])
  stars <- case_when(
    p_value <= 0.01 ~ "***",
    p_value <= 0.05 ~ "**",
    p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )

  visual_rows[[i]] <- tibble(
    outcome,
    sample,
    estimate,
    std_error,
    p_value
  )

  breaks_m <- seq(-152.4, 152.4, length.out = 11)
  bin_width_m <- 152.4 / 5
  bins <- display_data %>%
    mutate(
      bin = pmin(
        findInterval(
          running_distance,
          breaks_m,
          rightmost.closed = TRUE,
          all.inside = TRUE
        ),
        length(breaks_m) - 1L
      ),
      bin_center_m = breaks_m[bin] + bin_width_m / 2
    ) %>%
    group_by(bin, bin_center_m, side) %>%
    summarise(mean_outcome = mean(residualized_outcome), .groups = "drop") %>%
    mutate(bin_center_ft = bin_center_m / 0.3048)

  line_data <- tibble(
    running_distance = c(
      seq(-152.4, 0, length.out = 200),
      seq(0, 152.4, length.out = 200)[-1]
    )
  ) %>%
    mutate(side = as.integer(running_distance > 0))

  coefficient_names <- names(coef(display_model))
  design_matrix <- model.matrix(~ side * running_distance, data = line_data)[
    , coefficient_names, drop = FALSE
  ]
  model_vcov <- vcov(display_model)
  critical_value <- qt(
    0.975,
    df = max(n_distinct(display_data$ward_pair) - 1, 1)
  )

  line_data <- line_data %>%
    mutate(
      fit = as.numeric(design_matrix %*% coef(display_model)),
      fit_se = sqrt(pmax(rowSums((design_matrix %*% model_vcov) * design_matrix), 0)),
      ci_low = fit - critical_value * fit_se,
      ci_high = fit + critical_value * fit_se,
      distance_ft = running_distance / 0.3048
    )

  panels[[i]] <- ggplot() +
    geom_ribbon(
      data = line_data,
      aes(x = distance_ft, ymin = ci_low, ymax = ci_high, fill = factor(side)),
      alpha = 0.16,
      color = NA
    ) +
    geom_line(
      data = line_data,
      aes(x = distance_ft, y = fit, color = factor(side)),
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray30",
      linewidth = 0.35
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "gray55",
      linewidth = 0.35
    ) +
    geom_point(
      data = bins,
      aes(x = bin_center_ft, y = mean_outcome, fill = factor(side)),
      shape = 21,
      color = "white",
      stroke = 0.35,
      size = 2
    ) +
    scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_x_continuous(limits = c(-500, 500), breaks = seq(-500, 500, 250)) +
    labs(
      title = panel_specs$title[i],
      subtitle = sprintf("Visual estimate = %.3f%s (SE %.3f)", estimate, stars, std_error),
      x = "Distance to ward boundary (feet)",
      y = "Residualized log density"
    ) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 8.5),
      axis.title = element_text(size = 8.5),
      axis.text = element_text(size = 7.5),
      panel.grid.minor = element_blank()
    )
}

combined_plot <- (panels[[1]] | panels[[2]]) / (panels[[3]] | panels[[4]])

ggsave(
  "../output/density_rd_area_weighted_4panel.pdf",
  combined_plot,
  width = 11.2,
  height = 8.4,
  dpi = 300,
  bg = "white"
)
ggsave(
  "../output/density_rd_area_weighted_4panel.png",
  combined_plot,
  width = 11.2,
  height = 8.4,
  dpi = 180,
  bg = "white"
)
write_csv(bind_rows(visual_rows), "../output/density_rd_area_weighted_visual_estimates.csv")
