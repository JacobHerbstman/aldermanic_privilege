# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_rent_attenuation/code")
# bandwidth_ft <- 500

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft)
}
if (length(cli_args) != 1) {
  stop("Usage: Rscript build_rental_rd_rent_attenuation.R <bandwidth_ft>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))

rent <- read_parquet(sprintf("../input/rental_rd_characteristics_panel_bw%s.parquet", bandwidth_label)) %>%
  as_tibble()

model_specs <- tibble::tribble(
  ~spec_label, ~rhs,
  "No controls", "strictness_std",
  "Hedonics", "hedonic_rhs",
  "Hedonics + amenities", "amenity_rhs"
)

if (!"flag_clean_location_sample" %in% names(rent)) {
  stop("Missing sample flag column: flag_clean_location_sample", call. = FALSE)
}

d_sample <- rent %>%
  filter(flag_clean_location_sample) %>%
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
  stop("Clean-location rent attenuation sample has insufficient support.", call. = FALSE)
}

rhs_values <- c(
  hedonic_rhs = "strictness_std + log_sqft + log_beds + log_baths",
  amenity_rhs = paste(
    "strictness_std + log_sqft + log_beds + log_baths",
    "nearest_school_dist_kft",
    "nearest_park_dist_kft",
    "nearest_major_road_dist_kft",
    "nearest_cta_stop_dist_kft",
    "lake_michigan_dist_kft",
    sep = " + "
  )
)
if (n_distinct(d_sample$building_type_factor) > 1) {
  rhs_values["hedonic_rhs"] <- paste0(rhs_values["hedonic_rhs"], " + building_type_factor")
  rhs_values["amenity_rhs"] <- paste0(rhs_values["amenity_rhs"], " + building_type_factor")
}

attenuation_rows <- vector("list", nrow(model_specs))
for (j in seq_len(nrow(model_specs))) {
  rhs <- if (model_specs$rhs[j] %in% names(rhs_values)) {
    rhs_values[[model_specs$rhs[j]]]
  } else {
    model_specs$rhs[j]
  }
  model <- feols(
    as.formula(paste0("log(rent_price) ~ ", rhs, " | segment_id^year_month")),
    data = d_sample,
    cluster = ~segment_id
  )
  if (!"strictness_std" %in% names(coef(model))) {
    stop(sprintf("RD attenuation model failed to estimate strictness_std for %s.", model_specs$spec_label[j]), call. = FALSE)
  }
  attenuation_rows[[j]] <- tibble(
    spec_label = model_specs$spec_label[j],
    estimate = coef(model)[["strictness_std"]],
    std_error = se(model)[["strictness_std"]],
    p_value = pvalue(model)[["strictness_std"]],
    n_obs = model$nobs,
    n_segments = n_distinct(d_sample$segment_id),
    n_ward_pairs = n_distinct(d_sample$ward_pair),
    dep_var_mean = mean(d_sample$rent_price, na.rm = TRUE)
  )
}

attenuation <- bind_rows(attenuation_rows) %>%
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
  )

table_data <- attenuation %>%
  arrange(match(spec_label, c("No controls", "Hedonics", "Hedonics + amenities")))

if (nrow(table_data) != 3L) {
  stop("Expected three rent attenuation rows.", call. = FALSE)
}

clean_note <- " The clean-location sample excludes observations with modal-coordinate ward, pair, or distance-instability flags."

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
  sprintf("../output/rental_rd_rent_attenuation_clean_location_bw%s.tex", bandwidth_label)
)
