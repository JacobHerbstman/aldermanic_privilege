# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L, spec_id == "controls_drop_income") %>%
  select(alderman, score)

parcels <- read_csv(
  "../input/density_score_sensitivity_sample.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  left_join(
    scores %>% rename(alderman_own = alderman, score_own = score),
    by = "alderman_own",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_neighbor = alderman, score_neighbor = score),
    by = "alderman_neighbor",
    relationship = "many-to-one"
  ) %>%
  mutate(
    side = as.integer(score_own > score_neighbor),
    running_distance = if_else(
      side == 1L,
      abs(dist_to_boundary_m),
      -abs(dist_to_boundary_m)
    )
  )

model_results <- read_csv(
  "../input/density_score_sensitivity_models.csv",
  show_col_types = FALSE
)

if (anyNA(parcels$score_own) || anyNA(parcels$score_neighbor)) {
  stop("The selected score is missing for a density-sample alderman.", call. = FALSE)
}

bandwidth_m <- 152.4
bins_per_side <- 5L
bin_width_m <- bandwidth_m / bins_per_side
distance_display <- distance_display_config("ft")

panel_specs <- tribble(
  ~outcome, ~construction_sample, ~title,
  "density_far", "all", "All Construction: FAR",
  "density_far", "multifamily", "Multifamily: FAR",
  "density_dupac", "all", "All Construction: DUPAC",
  "density_dupac", "multifamily", "Multifamily: DUPAC"
)

panels <- vector("list", nrow(panel_specs))

for (panel_i in seq_len(nrow(panel_specs))) {
  outcome_i <- panel_specs$outcome[panel_i]
  sample_i <- panel_specs$construction_sample[panel_i]

  model_data <- if (sample_i == "all") {
    parcels %>% filter(unitscount > 0)
  } else {
    parcels %>% filter(unitscount > 1)
  }

  model_data <- model_data %>%
    filter(
      is.finite(.data[[outcome_i]]),
      .data[[outcome_i]] > 0,
      abs(running_distance) <= bandwidth_m
    ) %>%
    mutate(
      outcome_value = log(.data[[outcome_i]]),
      distance_bin = pmin(
        floor((running_distance + bandwidth_m) / bin_width_m) + 1L,
        2L * bins_per_side
      )
    )

  bin_model <- feols(
    as.formula(paste0(
      "outcome_value ~ i(distance_bin, ref = 5) + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = model_data,
    cluster = ~ward_pair,
    notes = FALSE
  )

  coefficient_table <- as.data.frame(coeftable(bin_model)) %>%
    rownames_to_column("term") %>%
    filter(str_detect(term, "^distance_bin::")) %>%
    transmute(
      distance_bin = as.integer(str_extract(term, "[0-9]+$")),
      estimate = Estimate,
      std_error = `Std. Error`
    )

  plot_data <- bind_rows(
    coefficient_table,
    tibble(distance_bin = 5L, estimate = 0, std_error = 0)
  ) %>%
    mutate(
      bin_center_m = -bandwidth_m + (distance_bin - 0.5) * bin_width_m,
      bin_center_ft = bin_center_m * distance_display$scale,
      side = if_else(bin_center_m < 0, "Less Stringent", "More Stringent"),
      ci_low = estimate - 1.96 * std_error,
      ci_high = estimate + 1.96 * std_error
    ) %>%
    arrange(distance_bin)

  local_result <- model_results %>%
    filter(
      spec_id == "controls_drop_income",
      construction_sample == sample_i,
      outcome == outcome_i,
      treatment == "binary"
    )

  if (nrow(local_result) != 1L) {
    stop("Could not recover the corresponding local-linear estimate.", call. = FALSE)
  }

  local_stars <- case_when(
    local_result$p_value <= 0.01 ~ "***",
    local_result$p_value <= 0.05 ~ "**",
    local_result$p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )

  panels[[panel_i]] <- ggplot(
    plot_data,
    aes(x = bin_center_ft, y = estimate, color = side, fill = side, group = side)
  ) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.16, color = NA) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50", linewidth = 0.35) +
    scale_color_manual(
      values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"),
      guide = "none"
    ) +
    scale_fill_manual(
      values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"),
      guide = "none"
    ) +
    scale_x_continuous(limits = c(-500, 500), breaks = seq(-500, 500, by = 250)) +
    labs(
      title = panel_specs$title[panel_i],
      subtitle = sprintf(
        "Local-linear jump = %.3f%s (SE %.3f)",
        local_result$estimate,
        local_stars,
        local_result$se
      ),
      x = "Distance to ward boundary (feet; positive = more stringent side)",
      y = "Bin coefficient relative to the closest lenient-side bin"
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
}

combined_plot <- (panels[[1]] | panels[[2]]) / (panels[[3]] | panels[[4]]) +
  plot_annotation(
    title = "Density Around Ward Boundaries: Nonparametric Distance-Bin Estimates"
  ) &
  theme(plot.title = element_text(face = "bold", size = 13))

ggsave(
  "../output/density_drop_income_rd_bin_coefficients_4panel.png",
  combined_plot,
  width = 12,
  height = 8.6,
  dpi = 220,
  bg = "white"
)
ggsave(
  "../output/density_drop_income_rd_bin_coefficients_4panel.pdf",
  combined_plot,
  width = 12,
  height = 8.6,
  bg = "white"
)
