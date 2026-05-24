# Build listed-rent RD attenuation tables and plot from the characteristics panel.

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_rent_attenuation/code")
# bandwidth_ft <- 500

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <bandwidth_ft>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))

message(sprintf("=== Listed-Rent RD Attenuation | bandwidth=%sft ===", bandwidth_label))

rent <- read_parquet(sprintf("../input/rental_rd_characteristics_panel_bw%s.parquet", bandwidth_label)) %>%
  as_tibble()

sample_defs <- tibble::tribble(
  ~sample, ~sample_label,
  "all", "All",
  "clean_location", "Clean location",
  "no_modal_pair_change", "No modal pair change",
  "no_modal_ward_change", "No modal ward change",
  "no_questionable_address", "No questionable address"
)

filter_sample <- function(df, sample_name) {
  if (sample_name == "all") {
    return(df)
  }
  if (sample_name == "clean_location") {
    return(df %>% filter(flag_clean_location_sample))
  }
  if (sample_name == "no_modal_pair_change") {
    return(df %>% filter(flag_no_modal_pair_change_sample))
  }
  if (sample_name == "no_modal_ward_change") {
    return(df %>% filter(flag_no_modal_ward_change_sample))
  }
  if (sample_name == "no_questionable_address") {
    return(df %>% filter(flag_no_questionable_address_sample))
  }
  stop(sprintf("Unknown sample: %s", sample_name), call. = FALSE)
}

format_tex_number <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits, big.mark = ",")
  )
}

format_tex_int <- function(x) {
  ifelse(
    is.na(x),
    "",
    formatC(round(x), format = "d", big.mark = ",")
  )
}

tex_stars <- function(p_value) {
  case_when(
    is.na(p_value) ~ "",
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

attenuation_rows <- list()
for (i in seq_len(nrow(sample_defs))) {
  sample_name <- sample_defs$sample[i]
  sample_label <- sample_defs$sample_label[i]
  d_sample <- filter_sample(rent, sample_name) %>%
    filter(
      is.finite(log_sqft),
      is.finite(log_beds),
      is.finite(log_baths),
      if_all(
        all_of(c(
          "nearest_school_dist_kft",
          "nearest_park_dist_kft",
          "nearest_major_road_dist_kft",
          "nearest_cta_stop_dist_kft",
          "lake_michigan_dist_kft"
        )),
        is.finite
      )
    ) %>%
    mutate(strictness_std = strictness_own / sd(strictness_own, na.rm = TRUE))

  if (nrow(d_sample) < 100 || n_distinct(d_sample$segment_id) < 2 || n_distinct(d_sample$strictness_std) < 2) {
    next
  }

  hedonic_rhs <- "strictness_std + log_sqft + log_beds + log_baths"
  if (n_distinct(d_sample$building_type_factor) > 1) {
    hedonic_rhs <- paste0(hedonic_rhs, " + building_type_factor")
  }
  amenity_rhs <- paste(
    hedonic_rhs,
    "nearest_school_dist_kft",
    "nearest_park_dist_kft",
    "nearest_major_road_dist_kft",
    "nearest_cta_stop_dist_kft",
    "lake_michigan_dist_kft",
    sep = " + "
  )

  model_specs <- tibble::tribble(
    ~specification, ~spec_label, ~rhs,
    "no_controls_common", "No controls", "strictness_std",
    "hedonic_common", "Hedonics", hedonic_rhs,
    "hedonic_amenity_common", "Hedonics + amenities", amenity_rhs
  )

  for (j in seq_len(nrow(model_specs))) {
    model <- feols(
      as.formula(paste0("log(rent_price) ~ ", model_specs$rhs[j], " | segment_id^year_month")),
      data = d_sample,
      cluster = ~segment_id
    )
    if (!"strictness_std" %in% names(coef(model))) {
      stop(sprintf("RD attenuation model failed to estimate strictness_std for %s / %s.", sample_name, model_specs$specification[j]), call. = FALSE)
    }
    attenuation_rows[[length(attenuation_rows) + 1]] <- tibble(
      sample = sample_name,
      sample_label = sample_label,
      specification = model_specs$specification[j],
      spec_label = model_specs$spec_label[j],
      estimate = coef(model)[["strictness_std"]],
      std_error = se(model)[["strictness_std"]],
      p_value = pvalue(model)[["strictness_std"]],
      n_obs = model$nobs,
      n_segments = n_distinct(d_sample$segment_id),
      n_ward_pairs = n_distinct(d_sample$ward_pair),
      dep_var_mean = mean(d_sample$rent_price, na.rm = TRUE),
      bandwidth_ft = bandwidth_ft,
      common_sample = TRUE
    )
  }
}

attenuation <- bind_rows(attenuation_rows) %>%
  mutate(
    ci_low = estimate - 1.96 * std_error,
    ci_high = estimate + 1.96 * std_error,
    spec_label = factor(spec_label, levels = c("No controls", "Hedonics", "Hedonics + amenities")),
    sample_label = factor(sample_label, levels = sample_defs$sample_label)
  )
write_csv(attenuation, sprintf("../output/rental_rd_rent_attenuation_bw%s.csv", bandwidth_label))

write_attenuation_table <- function(sample_name, output_path, include_clean_note = FALSE) {
  table_data <- attenuation %>%
    filter(sample == sample_name) %>%
    mutate(
      spec_label = as.character(spec_label),
      coefficient = paste0(format_tex_number(100 * estimate, 2), tex_stars(p_value)),
      std_error_cell = paste0("(", format_tex_number(100 * std_error, 2), ")")
    ) %>%
    arrange(match(spec_label, c("No controls", "Hedonics", "Hedonics + amenities")))

  if (nrow(table_data) != 3L) {
    stop(sprintf("Expected three rent attenuation rows for sample %s.", sample_name), call. = FALSE)
  }

  clean_note <- if (include_clean_note) {
    " The clean-location sample excludes observations with modal-coordinate ward, pair, or distance-instability flags."
  } else {
    ""
  }

  table_lines <- c(
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
      format_tex_int(table_data$n_obs[1]),
      format_tex_int(table_data$n_obs[2]),
      format_tex_int(table_data$n_obs[3])
    ),
    sprintf(
      "Dep. Var. Mean & %s & %s & %s \\\\",
      format_tex_number(table_data$dep_var_mean[1], 0),
      format_tex_number(table_data$dep_var_mean[2], 0),
      format_tex_number(table_data$dep_var_mean[3], 0)
    ),
    sprintf(
      "Segments & %s & %s & %s \\\\",
      format_tex_int(table_data$n_segments[1]),
      format_tex_int(table_data$n_segments[2]),
      format_tex_int(table_data$n_segments[3])
    ),
    sprintf(
      "Ward Pairs & %s & %s & %s \\\\",
      format_tex_int(table_data$n_ward_pairs[1]),
      format_tex_int(table_data$n_ward_pairs[2]),
      format_tex_int(table_data$n_ward_pairs[3])
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
  )

  writeLines(table_lines, output_path)
}

write_attenuation_table(
  "all",
  sprintf("../output/rental_rd_rent_attenuation_bw%s.tex", bandwidth_label),
  include_clean_note = FALSE
)
write_attenuation_table(
  "clean_location",
  sprintf("../output/rental_rd_rent_attenuation_clean_location_bw%s.tex", bandwidth_label),
  include_clean_note = TRUE
)

attenuation_plot <- ggplot(
  attenuation,
  aes(x = spec_label, y = estimate, ymin = ci_low, ymax = ci_high, color = sample_label)
) +
  geom_hline(yintercept = 0, color = "gray55", linetype = "dotted") +
  geom_pointrange(position = position_dodge(width = 0.55), linewidth = 0.45) +
  labs(
    title = "Listed-Rent Stringency Estimates With Hedonic And Amenity Controls",
    subtitle = sprintf("Common complete-case sample within %.0fft; segment-by-month FE", bandwidth_ft),
    x = NULL,
    y = "Coefficient on stringency index",
    color = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(
  sprintf("../output/rental_rd_rent_attenuation_bw%s.pdf", bandwidth_label),
  attenuation_plot,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
ggsave(
  sprintf("../output/rental_rd_rent_attenuation_bw%s.png", bandwidth_label),
  attenuation_plot,
  width = 9,
  height = 5.5,
  dpi = 220,
  bg = "white"
)

message("Saved listed-rent RD attenuation outputs.")
