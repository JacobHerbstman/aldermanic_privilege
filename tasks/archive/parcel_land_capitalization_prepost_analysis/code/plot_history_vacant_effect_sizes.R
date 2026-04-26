source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/parcel_land_capitalization_prepost_analysis/code")
# output_dir <- "../output"
# out_segment_continuous_pdf <- "../output/parcel_land_history_vacant_segment_effect_sizes_continuous.pdf"
# out_segment_continuous_png <- "../output/parcel_land_history_vacant_segment_effect_sizes_continuous.png"
# out_segment_sign_pdf <- "../output/parcel_land_history_vacant_segment_effect_sizes_sign.pdf"
# out_segment_sign_png <- "../output/parcel_land_history_vacant_segment_effect_sizes_sign.png"
# out_fe_compare_pdf <- "../output/parcel_land_history_vacant_core_fe_comparison.pdf"
# out_fe_compare_png <- "../output/parcel_land_history_vacant_core_fe_comparison.png"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    output_dir,
    out_segment_continuous_pdf,
    out_segment_continuous_png,
    out_segment_sign_pdf,
    out_segment_sign_png,
    out_fe_compare_pdf,
    out_fe_compare_png
  )
}

if (length(cli_args) != 7) {
  stop(
    paste(
      "FATAL: Script requires 7 args:",
      "<output_dir> <out_segment_continuous_pdf> <out_segment_continuous_png>",
      "<out_segment_sign_pdf> <out_segment_sign_png> <out_fe_compare_pdf> <out_fe_compare_png>"
    ),
    call. = FALSE
  )
}

output_dir <- cli_args[1]
out_segment_continuous_pdf <- cli_args[2]
out_segment_continuous_png <- cli_args[3]
out_segment_sign_pdf <- cli_args[4]
out_segment_sign_png <- cli_args[5]
out_fe_compare_pdf <- cli_args[6]
out_fe_compare_png <- cli_args[7]

coefficient_files <- list.files(
  output_dir,
  pattern = "^model_coefficients_history_vacant_.*\\.csv$",
  full.names = TRUE
)

if (length(coefficient_files) == 0) {
  stop("No history-vacant coefficient CSVs found in output_dir.", call. = FALSE)
}

coefficients_df <- bind_rows(
  lapply(sort(coefficient_files), function(path) {
    stem <- basename(path) %>%
      stringr::str_remove("^model_coefficients_") %>%
      stringr::str_remove("\\.csv$")

    fe_scope_from_name <- if (stringr::str_detect(stem, "__ward_pair_id$")) {
      "ward_pair_id"
    } else {
      "segment_id"
    }

    stem_core <- stringr::str_remove(stem, "__ward_pair_id$")
    stem_parts <- stringr::str_split_fixed(stem_core, "__", 5)

    coefficients_part <- readr::read_csv(path, show_col_types = FALSE)

    if (!"sample_scope" %in% names(coefficients_part)) {
      coefficients_part$sample_scope <- stem_parts[1]
    }
    if (!"year_scope" %in% names(coefficients_part)) {
      coefficients_part$year_scope <- stem_parts[2]
    }
    if (!"bandwidth" %in% names(coefficients_part)) {
      coefficients_part$bandwidth <- stem_parts[3]
    }
    if (!"treatment_spec" %in% names(coefficients_part)) {
      coefficients_part$treatment_spec <- stem_parts[4]
    }
    if (!"outcome" %in% names(coefficients_part)) {
      coefficients_part$outcome <- stem_parts[5]
    }
    if (!"fe_scope" %in% names(coefficients_part)) {
      coefficients_part$fe_scope <- fe_scope_from_name
    }

    coefficients_part
  })
)

sample_scope_labels <- c(
  history_vacant_loose = "History vacant loose",
  history_vacant_core = "History vacant core",
  history_vacant_urban_core = "History vacant urban core"
)

outcome_labels <- c(
  delta_log_land_psf_2016_minus_2014 = "Delta log land value per sq ft",
  delta_log_land_sum_2016_minus_2014 = "Delta log land value",
  building_positive_2016 = "Positive building value in 2016",
  gained_positive_building_2014_2016 = "Gained positive building value"
)

term_labels <- c(
  strictness_change = "Strictness change",
  "treatment_sign::to_lenient" = "To more lenient",
  "treatment_sign::to_stricter" = "To stricter"
)

fe_scope_labels <- c(
  segment_id = "Segment FE",
  ward_pair_id = "Ward-pair FE"
)

bandwidth_labels <- c(
  `1000ft` = "1,000 ft",
  `500ft` = "500 ft"
)

coefficients_df <- coefficients_df %>%
  filter(
    sample_scope %in% names(sample_scope_labels),
    year_scope == "admin_only",
    outcome %in% names(outcome_labels),
    term %in% names(term_labels),
    fe_scope %in% names(fe_scope_labels)
  ) %>%
  mutate(
    sample_scope_label = factor(
      sample_scope_labels[sample_scope],
      levels = unname(sample_scope_labels)
    ),
    outcome_label = factor(
      outcome_labels[outcome],
      levels = rev(unname(outcome_labels))
    ),
    term_label = factor(
      term_labels[term],
      levels = c("Strictness change", "To more lenient", "To stricter")
    ),
    fe_scope_label = factor(
      fe_scope_labels[fe_scope],
      levels = unname(fe_scope_labels)
    ),
    bandwidth_label = factor(
      bandwidth_labels[bandwidth],
      levels = unname(bandwidth_labels)
    ),
    display_label = factor(
      paste0(outcome_labels[outcome], " | ", term_labels[term]),
      levels = rev(c(
        paste0(unname(outcome_labels), " | To more lenient"),
        paste0(unname(outcome_labels), " | To stricter")
      ))
    )
  )

save_plot_pair <- function(plot_obj, pdf_path, png_path, width, height) {
  ggplot2::ggsave(pdf_path, plot_obj, width = width, height = height, bg = "white")
  ggplot2::ggsave(png_path, plot_obj, width = width, height = height, dpi = 220, bg = "white")
}

segment_continuous_plot <- coefficients_df %>%
  filter(
    fe_scope == "segment_id",
    treatment_spec == "continuous",
    term == "strictness_change"
  ) %>%
  ggplot(aes(x = estimate, y = outcome_label)) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = 0.35) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.14, color = "#4C78A8", linewidth = 0.5) +
  geom_point(color = "#4C78A8", size = 2.1) +
  facet_grid(sample_scope_label ~ bandwidth_label) +
  labs(
    title = "History-vacant pre-period samples: continuous effect sizes",
    subtitle = "Admin-only 2014 to 2016 segment-FE estimates",
    x = "Coefficient",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

segment_sign_plot <- coefficients_df %>%
  filter(
    fe_scope == "segment_id",
    treatment_spec == "sign",
    term %in% c("treatment_sign::to_lenient", "treatment_sign::to_stricter")
  ) %>%
  ggplot(aes(x = estimate, y = display_label, color = term_label)) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = 0.35) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.14, linewidth = 0.5) +
  geom_point(size = 2.0) +
  facet_grid(sample_scope_label ~ bandwidth_label) +
  scale_color_manual(
    values = c(
      "To more lenient" = "#1B9E77",
      "To stricter" = "#D95F02"
    ),
    name = NULL
  ) +
  labs(
    title = "History-vacant pre-period samples: sign-spec effect sizes",
    subtitle = "Admin-only 2014 to 2016 segment-FE estimates",
    x = "Coefficient",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

fe_compare_plot <- coefficients_df %>%
  filter(
    sample_scope == "history_vacant_core",
    outcome == "delta_log_land_psf_2016_minus_2014"
  ) %>%
  mutate(
    compare_label = factor(
      term_labels[term],
      levels = rev(c("Strictness change", "To more lenient", "To stricter"))
    )
  ) %>%
  ggplot(aes(x = estimate, y = compare_label, color = fe_scope_label)) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = 0.35) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.14, linewidth = 0.5, position = position_dodge(width = 0.4)) +
  geom_point(size = 2.1, position = position_dodge(width = 0.4)) +
  facet_grid(treatment_spec ~ bandwidth_label) +
  scale_color_manual(
    values = c(
      "Segment FE" = "#4C78A8",
      "Ward-pair FE" = "#E15759"
    ),
    name = NULL
  ) +
  labs(
    title = "History vacant core: FE comparison for land-value effects",
    subtitle = "Admin-only 2014 to 2016 delta log land value per sq ft",
    x = "Coefficient",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

save_plot_pair(segment_continuous_plot, out_segment_continuous_pdf, out_segment_continuous_png, width = 13, height = 10.5)
save_plot_pair(segment_sign_plot, out_segment_sign_pdf, out_segment_sign_png, width = 14, height = 12.5)
save_plot_pair(fe_compare_plot, out_fe_compare_pdf, out_fe_compare_png, width = 12.5, height = 7.5)

message(sprintf("Saved %s", out_segment_continuous_pdf))
message(sprintf("Saved %s", out_segment_sign_pdf))
message(sprintf("Saved %s", out_fe_compare_pdf))
