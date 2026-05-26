# Render listed-rent RD attenuation tables.

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_rent_attenuation/code")
# bandwidth_ft <- 500
# sample_name <- "clean_location"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft, sample_name)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <bandwidth_ft> <sample_name>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
sample_name <- cli_args[2]
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
if (!sample_name %in% c("all", "clean_location")) {
  stop("sample_name must be all or clean_location.", call. = FALSE)
}

bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))
output_tex <- if (sample_name == "all") {
  sprintf("../output/rental_rd_rent_attenuation_bw%s.tex", bandwidth_label)
} else {
  sprintf("../output/rental_rd_rent_attenuation_clean_location_bw%s.tex", bandwidth_label)
}

table_data <- read_csv(
  sprintf("../output/rental_rd_rent_attenuation_bw%s.csv", bandwidth_label),
  show_col_types = FALSE
) %>%
  filter(sample == sample_name) %>%
  mutate(
    spec_label = as.character(spec_label),
    star_text = case_when(
      is.na(p_value) ~ "",
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    coefficient = paste0(formatC(100 * estimate, format = "f", digits = 2, big.mark = ","), star_text),
    std_error_cell = paste0("(", formatC(100 * std_error, format = "f", digits = 2, big.mark = ","), ")")
  ) %>%
  arrange(match(spec_label, c("No controls", "Hedonics", "Hedonics + amenities")))

if (nrow(table_data) != 3L) {
  stop(sprintf("Expected three rent attenuation rows for sample %s.", sample_name), call. = FALSE)
}

clean_note <- if (sample_name == "clean_location") {
  " The clean-location sample excludes observations with modal-coordinate ward, pair, or distance-instability flags."
} else {
  ""
}

writeLines(
  c(
    "\\begingroup",
    "\\centering",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) & (2) & (3) \\\\",
    "\\midrule",
    sprintf(
      "Stringency Index & %s & %s & %s \\\\",
      table_data$coefficient[1],
      table_data$coefficient[2],
      table_data$coefficient[3]
    ),
    sprintf(
      " & %s & %s & %s \\\\",
      table_data$std_error_cell[1],
      table_data$std_error_cell[2],
      table_data$std_error_cell[3]
    ),
    "\\\\",
    sprintf(
      "N & %s & %s & %s \\\\",
      formatC(round(table_data$n_obs[1]), format = "d", big.mark = ","),
      formatC(round(table_data$n_obs[2]), format = "d", big.mark = ","),
      formatC(round(table_data$n_obs[3]), format = "d", big.mark = ",")
    ),
    sprintf(
      "Dep. Var. Mean & %s & %s & %s \\\\",
      formatC(table_data$dep_var_mean[1], format = "f", digits = 0, big.mark = ","),
      formatC(table_data$dep_var_mean[2], format = "f", digits = 0, big.mark = ","),
      formatC(table_data$dep_var_mean[3], format = "f", digits = 0, big.mark = ",")
    ),
    sprintf(
      "Segments & %s & %s & %s \\\\",
      formatC(round(table_data$n_segments[1]), format = "d", big.mark = ","),
      formatC(round(table_data$n_segments[2]), format = "d", big.mark = ","),
      formatC(round(table_data$n_segments[3]), format = "d", big.mark = ",")
    ),
    sprintf(
      "Ward Pairs & %s & %s & %s \\\\",
      formatC(round(table_data$n_ward_pairs[1]), format = "d", big.mark = ","),
      formatC(round(table_data$n_ward_pairs[2]), format = "d", big.mark = ","),
      formatC(round(table_data$n_ward_pairs[3]), format = "d", big.mark = ",")
    ),
    "Hedonic Controls & & $\\checkmark$ & $\\checkmark$ \\\\",
    "Amenity Controls & & & $\\checkmark$ \\\\",
    "Segment $\\times$ Month FE & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
    "Cluster Level & Segment & Segment & Segment \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    sprintf(
      "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: Entries are percent log-rent effects of a one-standard-deviation increase in the aldermanic stringency index, with standard errors in parentheses. The sample uses listed-rent floorplan-month observations from 2014--2022 within %sft of ward boundaries. Dependent-variable means are real listed rents in 2022 dollars. Hedonic controls are log square feet, log bedrooms, log bathrooms, and building type. Amenity controls are distances to the nearest school, CPD park-boundary polygon, major street, CTA stop open by the listing month, and Lake Michigan.%s * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}",
      bandwidth_label,
      clean_note
    ),
    "\\par\\endgroup"
  ),
  output_tex
)

message(sprintf("Saved listed-rent RD attenuation table for %s.", sample_name))
