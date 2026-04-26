source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")
library(fixest)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/density_covariate_balance/code")
# parcels_input <- "../input/parcels_with_ward_distances.csv"
# ward_controls_input <- "../input/ward_controls_2000_2023.csv"
# output_main_csv <- "../output/density_covariate_balance_main_bw500_multifamily.csv"
# output_main_tex <- "../output/density_covariate_balance_main_bw500_multifamily.tex"
# output_snapshot_csv <- "../output/density_covariate_balance_snapshot2014_bw500_multifamily.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    parcels_input,
    ward_controls_input,
    output_main_csv,
    output_main_tex,
    output_snapshot_csv
  )
}

if (length(args) != 5) {
  stop(
    paste(
      "FATAL: Script requires 5 args:",
      "<parcels_input> <ward_controls_input> <output_main_csv> <output_main_tex> <output_snapshot_csv>"
    ),
    call. = FALSE
  )
}

parcels_input <- args[1]
ward_controls_input <- args[2]
output_main_csv <- args[3]
output_main_tex <- args[4]
output_snapshot_csv <- args[5]

covariates <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

covariate_labels <- c(
  share_white_own = "Ward share White",
  share_black_own = "Ward share Black",
  median_hh_income_own = "Ward median household income",
  share_bach_plus_own = "Ward share BA+",
  homeownership_rate_own = "Ward homeownership rate"
)

snapshot_mapping <- c(
  share_white_own = "share_white",
  share_black_own = "share_black",
  median_hh_income_own = "median_hh_income",
  share_bach_plus_own = "share_bach_plus",
  homeownership_rate_own = "homeownership_rate"
)

fmt_num <- function(x, digits = 3) {
  ifelse(is.finite(x), formatC(x, digits = digits, format = "f"), "")
}

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

build_balance_results <- function(df, covariate_names, sample_label) {
  bind_rows(lapply(covariate_names, function(covariate_name) {
    analysis_df <- df %>%
      filter(!is.na(.data[[covariate_name]])) %>%
      mutate(
        outcome_std = (.data[[covariate_name]] - mean(.data[[covariate_name]], na.rm = TRUE)) /
          sd(.data[[covariate_name]], na.rm = TRUE)
      )

    model <- feols(
      outcome_std ~ strictness_own + lenient_dist + strict_dist | zone_group + segment_id + construction_year,
      data = analysis_df,
      cluster = ~ward_pair,
      warn = FALSE
    )

    coef_table <- coeftable(model)
    tibble(
      sample = sample_label,
      covariate = covariate_name,
      covariate_label = covariate_labels[[covariate_name]],
      estimate = unname(coef_table["strictness_own", "Estimate"]),
      se = unname(coef_table["strictness_own", "Std. Error"]),
      p_value = unname(coef_table["strictness_own", "Pr(>|t|)"]),
      n_obs = nobs(model),
      n_ward_pairs = n_distinct(analysis_df$ward_pair)
    )
  }))
}

parcels <- read_csv(parcels_input, show_col_types = FALSE) %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year)),
    zone_group = zone_group_from_code(zone_code),
    strictness_own = strictness_own / sd(strictness_own, na.rm = TRUE),
    lenient_dist = abs(signed_distance) * as.integer(signed_distance <= 0),
    strict_dist = abs(signed_distance) * as.integer(signed_distance > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    unitscount > 1,
    dist_to_boundary <= 500,
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != "",
    !is.na(zone_group),
    density_far > 0,
    density_dupac > 0,
    !is.na(strictness_own),
    !is.na(lenient_dist),
    !is.na(strict_dist)
  )

sample_missing_counts <- colSums(is.na(parcels[, covariates]))
if (any(sample_missing_counts > 0)) {
  stop(
    paste(
      "Main density sample still has missing control-set covariates:",
      paste(names(sample_missing_counts)[sample_missing_counts > 0], collapse = ", ")
    ),
    call. = FALSE
  )
}

main_results <- build_balance_results(parcels, covariates, "matched_year_controls")
write_csv(main_results, output_main_csv)

ward_controls_2014 <- read_csv(ward_controls_input, show_col_types = FALSE) %>%
  filter(year == 2014) %>%
  transmute(
    ward = as.integer(ward),
    share_white_2014 = share_white,
    share_black_2014 = share_black,
    median_hh_income_2014 = median_hh_income,
    share_bach_plus_2014 = share_bach_plus,
    homeownership_rate_2014 = homeownership_rate
  )

snapshot_sample <- parcels %>%
  left_join(ward_controls_2014, by = "ward") %>%
  mutate(
    share_white_own = share_white_2014,
    share_black_own = share_black_2014,
    median_hh_income_own = median_hh_income_2014,
    share_bach_plus_own = share_bach_plus_2014,
    homeownership_rate_own = homeownership_rate_2014
  ) %>%
  select(-ends_with("_2014"))

snapshot_missing_counts <- colSums(is.na(snapshot_sample[, covariates]))
if (any(snapshot_missing_counts > 0)) {
  stop(
    paste(
      "2014 snapshot sample has missing ward-control covariates:",
      paste(names(snapshot_missing_counts)[snapshot_missing_counts > 0], collapse = ", ")
    ),
    call. = FALSE
  )
}

snapshot_results <- build_balance_results(snapshot_sample, covariates, "snapshot_2014_controls")
write_csv(snapshot_results, output_snapshot_csv)

table_rows <- main_results %>%
  mutate(
    estimate_display = paste0(fmt_num(estimate), stars(p_value)),
    se_display = paste0("(", fmt_num(se), ")"),
    p_display = fmt_num(p_value),
    n_display = formatC(n_obs, format = "d", big.mark = ","),
    ward_pair_display = formatC(n_ward_pairs, format = "d", big.mark = ",")
  )

tex_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "Covariate & Coef. on Stringency Index & S.E. & p-value & N & Ward Pairs \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(table_rows))) {
  tex_lines <- c(
    tex_lines,
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      table_rows$covariate_label[i],
      table_rows$estimate_display[i],
      table_rows$se_display[i],
      table_rows$p_display[i],
      table_rows$n_display[i],
      table_rows$ward_pair_display[i]
    )
  )
}

tex_lines <- c(
  tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(tex_lines, output_main_tex)

message("Saved: ", output_main_csv)
message("Saved: ", output_main_tex)
message("Saved: ", output_snapshot_csv)
