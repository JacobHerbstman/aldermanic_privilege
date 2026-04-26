source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/parcel_land_capitalization_prepost_analysis/code")
# output_dir <- "../output"
# out_coefficients <- "../output/parcel_land_capitalization_prepost_coefficients.csv"
# out_metadata <- "../output/parcel_land_capitalization_prepost_metadata.csv"
# out_sign_pdf <- "../output/parcel_land_capitalization_prepost_sign_coefficients.pdf"
# out_sign_png <- "../output/parcel_land_capitalization_prepost_sign_coefficients.png"
# out_continuous_pdf <- "../output/parcel_land_capitalization_prepost_continuous_coefficients.pdf"
# out_continuous_png <- "../output/parcel_land_capitalization_prepost_continuous_coefficients.png"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    output_dir,
    out_coefficients,
    out_metadata,
    out_sign_pdf,
    out_sign_png,
    out_continuous_pdf,
    out_continuous_png
  )
}

if (length(cli_args) != 7) {
  stop(
    paste(
      "FATAL: Script requires 7 args:",
      "<output_dir> <out_coefficients_csv> <out_metadata_csv>",
      "<out_sign_pdf> <out_sign_png> <out_continuous_pdf> <out_continuous_png>"
    ),
    call. = FALSE
  )
}

output_dir <- cli_args[1]
out_coefficients <- cli_args[2]
out_metadata <- cli_args[3]
out_sign_pdf <- cli_args[4]
out_sign_png <- cli_args[5]
out_continuous_pdf <- cli_args[6]
out_continuous_png <- cli_args[7]

save_plot_pair <- function(plot_obj, pdf_path, png_path, width, height) {
  ggsave(pdf_path, plot_obj, width = width, height = height, bg = "white")
  ggsave(png_path, plot_obj, width = width, height = height, dpi = 220, bg = "white")
}

coefficient_files <- list.files(output_dir, pattern = "^model_coefficients_.*\\.csv$", full.names = TRUE)
metadata_files <- list.files(output_dir, pattern = "^model_metadata_.*\\.csv$", full.names = TRUE)

if (length(coefficient_files) == 0) {
  stop("No model coefficient CSVs found in output_dir.", call. = FALSE)
}
if (length(metadata_files) == 0) {
  stop("No model metadata CSVs found in output_dir.", call. = FALSE)
}

coefficients_df <- bind_rows(lapply(sort(coefficient_files), readr::read_csv, show_col_types = FALSE))
metadata_df <- bind_rows(lapply(sort(metadata_files), readr::read_csv, show_col_types = FALSE))

year_scope_labels <- c(
  admin_only = "Admin year only",
  admin_plus_fallback = "Admin + fallback"
)

sample_scope_labels <- c(
  all_baseline_empty = "All baseline-empty",
  no_ex = "Exclude EX",
  developable_loose = "Developable loose",
  developable_core = "Developable core",
  history_vacant_loose = "History vacant loose",
  history_vacant_core = "History vacant core",
  history_vacant_urban_core = "History vacant urban core",
  current_vacant_loose = "Current vacant loose",
  current_vacant_core = "Current vacant core",
  current_vacant_private_core = "Current vacant private core"
)

bandwidth_labels <- c(
  `1000ft` = "1,000 ft",
  `500ft` = "500 ft"
)

outcome_labels <- c(
  delta_log_land_psf_2016_minus_2014 = "Delta log land value per sq ft",
  delta_log_land_sum_2016_minus_2014 = "Delta log land value",
  delta_land_share_2016_minus_2014 = "Delta land share",
  building_positive_2016 = "Positive building value in 2016",
  gained_positive_building_2014_2016 = "Gained positive building value"
)

term_labels <- c(
  "Strictness change" = "Strictness change",
  "To more lenient" = "To more lenient",
  "To stricter" = "To stricter"
)

coefficients_df <- coefficients_df %>%
  mutate(
    sample_scope = factor(sample_scope, levels = names(sample_scope_labels)),
    sample_scope_label = factor(sample_scope_labels[as.character(sample_scope)], levels = unname(sample_scope_labels)),
    year_scope = factor(year_scope, levels = c("admin_only", "admin_plus_fallback")),
    year_scope_label = factor(year_scope_labels[as.character(year_scope)], levels = unname(year_scope_labels)),
    bandwidth = factor(bandwidth, levels = c("1000ft", "500ft")),
    bandwidth_label = factor(bandwidth_labels[as.character(bandwidth)], levels = unname(bandwidth_labels)),
    outcome = factor(outcome, levels = names(outcome_labels)),
    outcome_label = factor(outcome_labels[as.character(outcome)], levels = rev(unname(outcome_labels))),
    term_label = factor(term_labels[as.character(term_label)], levels = c("Strictness change", "To more lenient", "To stricter")),
    display_label = factor(
      if_else(
        treatment_spec == "sign",
        paste0(outcome_labels[as.character(outcome)], " | ", as.character(term_label)),
        outcome_labels[as.character(outcome)]
      ),
      levels = rev(c(
        paste0(unname(outcome_labels), " | To more lenient"),
        paste0(unname(outcome_labels), " | To stricter"),
        unname(outcome_labels)
      ))
    )
  ) %>%
  arrange(sample_scope, year_scope, bandwidth, treatment_spec, outcome, term_label)

metadata_df <- metadata_df %>%
  mutate(
    sample_scope = factor(sample_scope, levels = names(sample_scope_labels)),
    sample_scope_label = factor(sample_scope_labels[as.character(sample_scope)], levels = unname(sample_scope_labels)),
    year_scope = factor(year_scope, levels = c("admin_only", "admin_plus_fallback")),
    year_scope_label = factor(year_scope_labels[as.character(year_scope)], levels = unname(year_scope_labels)),
    bandwidth = factor(bandwidth, levels = c("1000ft", "500ft")),
    bandwidth_label = factor(bandwidth_labels[as.character(bandwidth)], levels = unname(bandwidth_labels)),
    outcome = factor(outcome, levels = names(outcome_labels)),
    outcome_label = factor(outcome_labels[as.character(outcome)], levels = unname(outcome_labels))
  ) %>%
  arrange(sample_scope, year_scope, bandwidth, treatment_spec, outcome)

continuous_plot_data <- coefficients_df %>%
  filter(treatment_spec == "continuous", term == "strictness_change")

continuous_plot <- ggplot(
  continuous_plot_data,
  aes(x = estimate, y = outcome_label)
) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = 0.35) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.14, color = "#4C78A8", linewidth = 0.5) +
  geom_point(color = "#4C78A8", size = 2.1) +
  facet_grid(sample_scope_label ~ year_scope_label + bandwidth_label) +
  labs(
    title = "2014 to 2016 pre/post segment-FE estimates",
    subtitle = "Continuous strictness-change treatment",
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

sign_plot_data <- coefficients_df %>%
  filter(treatment_spec == "sign", term %in% c("treatment_sign::to_lenient", "treatment_sign::to_stricter"))

sign_plot <- ggplot(
  sign_plot_data,
  aes(x = estimate, y = display_label, color = term_label)
) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = 0.35) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.14, linewidth = 0.5, position = position_identity()) +
  geom_point(size = 2.0) +
  facet_grid(sample_scope_label ~ year_scope_label + bandwidth_label) +
  scale_color_manual(
    values = c(
      "To more lenient" = "#1B9E77",
      "To stricter" = "#D95F02"
    ),
    name = NULL
  ) +
  labs(
    title = "2014 to 2016 pre/post segment-FE estimates",
    subtitle = "Treatment-group coefficients relative to no-change parcels",
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

readr::write_csv(coefficients_df, out_coefficients)
readr::write_csv(metadata_df, out_metadata)

save_plot_pair(continuous_plot, out_continuous_pdf, out_continuous_png, width = 15.5, height = 11.5)
save_plot_pair(sign_plot, out_sign_pdf, out_sign_png, width = 16.0, height = 14.0)

message(sprintf("Saved %s", out_coefficients))
message(sprintf("Saved %s", out_metadata))
