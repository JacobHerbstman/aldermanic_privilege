source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/repeat_sales_event_study/code")
# sample_summary_csv <- "../output/repeat_sales_sample_summary.csv"
# spec_comparison_csv <- "../output/repeat_sales_spec_comparison.csv"
# spec_comparison_tex <- "../output/repeat_sales_spec_comparison.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(sample_summary_csv, spec_comparison_csv, spec_comparison_tex)
}

if (length(args) != 3) {
  stop(
    "Usage: Rscript summarize_repeat_sales_results.R <sample_summary_csv> <spec_comparison_csv> <spec_comparison_tex>",
    call. = FALSE
  )
}

sample_summary_csv <- args[1]
spec_comparison_csv <- args[2]
spec_comparison_tex <- args[3]
output_dir <- dirname(sample_summary_csv)

metadata_files <- list.files(output_dir, pattern = "^repeat_sales_metadata_.*\\.csv$", full.names = TRUE)
if (length(metadata_files) == 0) {
  stop("No repeat-sales metadata files found in output directory.", call. = FALSE)
}

read_bundle <- function(metadata_file) {
  suffix <- sub("^repeat_sales_metadata_", "", tools::file_path_sans_ext(basename(metadata_file)))

  list(
    metadata = read_csv(metadata_file, show_col_types = FALSE),
    coefficients = read_csv(file.path(output_dir, sprintf("repeat_sales_coefficients_%s.csv", suffix)), show_col_types = FALSE),
    support = read_csv(file.path(output_dir, sprintf("repeat_sales_support_%s.csv", suffix)), show_col_types = FALSE),
    pretrend = read_csv(file.path(output_dir, sprintf("repeat_sales_pretrend_%s.csv", suffix)), show_col_types = FALSE)
  )
}

panel_order <- c(
  "cohort_2012",
  "cohort_2015",
  "cohort_2022",
  "cohort_2023",
  "stacked_announcement",
  "stacked_implementation"
)
approach_order <- c("bw1000", "corridor1320", "citywide_valid")
treatment_order <- c("continuous", "continuous_split")

bundle_to_spec_row <- function(bundle) {
  metadata <- bundle$metadata
  coefficients <- bundle$coefficients
  pretrend <- bundle$pretrend

  post_coefficients <- coefficients %>%
    filter(event_time >= 0, !is_reference)

  mean_post_effect_pct <- if (nrow(post_coefficients) == 0) NA_real_ else mean(post_coefficients$estimate_pct, na.rm = TRUE)
  terminal_post_effect_pct <- if (nrow(post_coefficients) == 0) {
    NA_real_
  } else {
    post_coefficients %>%
      filter(event_time == max(event_time)) %>%
      summarise(value = mean(estimate_pct, na.rm = TRUE)) %>%
      pull(value)
  }

  stricter_post <- coefficients %>%
    filter(group == "Moved to Stricter", event_time >= 0, !is_reference)
  lenient_post <- coefficients %>%
    filter(group == "Moved to More Lenient", event_time >= 0, !is_reference)

  stricter_pretrend <- pretrend %>%
    filter(group == "Moved to Stricter") %>%
    pull(p_value)
  lenient_pretrend <- pretrend %>%
    filter(group == "Moved to More Lenient") %>%
    pull(p_value)

  scalar_pretrend <- pretrend %>%
    pull(p_value)
  scalar_pretrend <- scalar_pretrend[is.finite(scalar_pretrend)]

  tibble(
    panel_mode = metadata$panel_mode[1],
    panel_title = metadata$panel_title[1],
    approach = metadata$approach[1],
    approach_title = metadata$approach_title[1],
    treatment_mode = metadata$treatment_mode[1],
    event_year = as.character(metadata$event_year_label[1]),
    analysis_n = metadata$analysis_n[1],
    n_repeat_pins = metadata$n_repeat_pins[1],
    n_treated_repeat_pins = metadata$n_treated_repeat_pins[1],
    n_control_repeat_pins = metadata$n_control_repeat_pins[1],
    n_pin_year_obs = metadata$n_pin_year_obs[1],
    n_identifying_geography_year_cells = metadata$n_identifying_geography_year_cells_total[1],
    n_identifying_geographies = metadata$n_identifying_geographies_total[1],
    supported_event_times = metadata$plotted_supported_periods[1],
    pretrend_p_value = if (length(scalar_pretrend) == 0) NA_real_ else min(scalar_pretrend),
    mean_post_effect_pct = mean_post_effect_pct,
    terminal_post_effect_pct = terminal_post_effect_pct,
    pretrend_p_value_stricter = if (length(stricter_pretrend) == 0) NA_real_ else stricter_pretrend[1],
    pretrend_p_value_lenient = if (length(lenient_pretrend) == 0) NA_real_ else lenient_pretrend[1],
    mean_post_effect_pct_stricter = if (nrow(stricter_post) == 0) NA_real_ else mean(stricter_post$estimate_pct, na.rm = TRUE),
    mean_post_effect_pct_lenient = if (nrow(lenient_post) == 0) NA_real_ else mean(lenient_post$estimate_pct, na.rm = TRUE),
    terminal_post_effect_pct_stricter = if (nrow(stricter_post) == 0) NA_real_ else stricter_post %>%
      filter(event_time == max(event_time)) %>%
      summarise(value = mean(estimate_pct, na.rm = TRUE)) %>%
      pull(value),
    terminal_post_effect_pct_lenient = if (nrow(lenient_post) == 0) NA_real_ else lenient_post %>%
      filter(event_time == max(event_time)) %>%
      summarise(value = mean(estimate_pct, na.rm = TRUE)) %>%
      pull(value)
  )
}

bundles <- lapply(sort(metadata_files), read_bundle)

spec_comparison <- bind_rows(lapply(bundles, bundle_to_spec_row)) %>%
  mutate(
    panel_mode = factor(panel_mode, levels = panel_order),
    approach = factor(approach, levels = approach_order),
    treatment_mode = factor(treatment_mode, levels = treatment_order)
  ) %>%
  arrange(panel_mode, approach, treatment_mode) %>%
  mutate(
    panel_mode = as.character(panel_mode),
    approach = as.character(approach),
    treatment_mode = as.character(treatment_mode)
  )

sample_summary <- spec_comparison %>%
  filter(treatment_mode == "continuous") %>%
  select(
    panel_mode,
    approach,
    event_year,
    n_repeat_pins,
    n_treated_repeat_pins,
    n_control_repeat_pins,
    n_pin_year_obs,
    n_identifying_geography_year_cells,
    n_identifying_geographies
  )

format_number <- function(x, digits = 1) {
  ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
}

continuous_table <- spec_comparison %>%
  filter(treatment_mode == "continuous") %>%
  transmute(
    panel = panel_title,
    approach = approach_title,
    repeat_pins = format(n_repeat_pins, big.mark = ","),
    pin_year_obs = format(n_pin_year_obs, big.mark = ","),
    pretrend_p = format_number(pretrend_p_value, 3),
    mean_post = format_number(mean_post_effect_pct, 1),
    terminal_post = format_number(terminal_post_effect_pct, 1)
  )

tex_lines <- c(
  "\\begin{tabular}{llrrrrr}",
  "\\hline",
  "Panel & Approach & Repeat PINs & PIN-year obs. & Pretrend $p$ & Mean post (\\%) & Terminal post (\\%) \\\\",
  "\\hline",
  apply(continuous_table, 1, function(row) {
    paste(row, collapse = " & ") |> paste0(" \\\\")
  }),
  "\\hline",
  "\\end{tabular}"
)

write_csv(sample_summary, sample_summary_csv)
write_csv(spec_comparison, spec_comparison_csv)
writeLines(tex_lines, spec_comparison_tex)
