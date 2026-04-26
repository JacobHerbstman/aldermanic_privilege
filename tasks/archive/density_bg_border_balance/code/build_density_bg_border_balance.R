source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")
library(fixest)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/density_bg_border_balance/code")
# parcels_input <- "../input/parcels_with_ward_distances.csv"
# bg_controls_input <- "../input/block_group_controls.csv"
# output_main_csv <- "../output/density_bg_border_balance_snapshot2014.csv"
# output_drop_csv <- "../output/density_bg_border_balance_snapshot2014_drop_straddle.csv"
# output_summary_csv <- "../output/density_bg_border_straddle_summary_snapshot2014.csv"
# output_tex <- "../output/density_bg_border_balance_snapshot2014.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    parcels_input,
    bg_controls_input,
    output_main_csv,
    output_drop_csv,
    output_summary_csv,
    output_tex
  )
}

if (length(args) != 6) {
  stop(
    paste(
      "FATAL: Script requires 6 args:",
      "<parcels_input> <bg_controls_input> <output_main_csv> <output_drop_csv> <output_summary_csv> <output_tex>"
    ),
    call. = FALSE
  )
}

parcels_input <- args[1]
bg_controls_input <- args[2]
output_main_csv <- args[3]
output_drop_csv <- args[4]
output_summary_csv <- args[5]
output_tex <- args[6]

snapshot_year <- 2014L
covariates <- c(
  "percent_white_bg",
  "percent_black_bg",
  "median_income_bg",
  "share_bach_plus_bg",
  "homeownership_rate_bg"
)

covariate_labels <- c(
  percent_white_bg = "Block-group share White",
  percent_black_bg = "Block-group share Black",
  median_income_bg = "Block-group median income",
  share_bach_plus_bg = "Block-group share BA+",
  homeownership_rate_bg = "Block-group homeownership rate"
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

build_balance_results <- function(df, sample_label, covariate_names) {
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
    GEOID = as.character(GEOID),
    construction_year = suppressWarnings(as.integer(construction_year)),
    zone_group = zone_group_from_code(zone_code),
    strictness_own = strictness_own / sd(strictness_own, na.rm = TRUE),
    lenient_dist = abs(signed_distance) * as.integer(signed_distance <= 0),
    strict_dist = abs(signed_distance) * as.integer(signed_distance > 0),
    side = if_else(signed_distance > 0, "strict", "lenient")
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
    !is.na(GEOID),
    GEOID != "",
    density_far > 0,
    density_dupac > 0,
    !is.na(strictness_own),
    !is.na(lenient_dist),
    !is.na(strict_dist)
  )

bg_snapshot <- read_csv(bg_controls_input, show_col_types = FALSE) %>%
  filter(year == snapshot_year) %>%
  transmute(
    GEOID = as.character(GEOID),
    percent_white_bg = percent_white,
    percent_black_bg = percent_black,
    median_income_bg = median_income,
    share_bach_plus_bg = share_bach_plus,
    homeownership_rate_bg = homeownership_rate
  )

analysis_sample <- parcels %>%
  select(-any_of(covariates)) %>%
  left_join(bg_snapshot, by = "GEOID")

bg_segment_side <- analysis_sample %>%
  distinct(GEOID, ward_pair, segment_id, side) %>%
  count(GEOID, ward_pair, segment_id, name = "n_sides_present") %>%
  mutate(straddle_bg_segment = n_sides_present > 1)

analysis_sample <- analysis_sample %>%
  left_join(
    bg_segment_side %>% select(GEOID, ward_pair, segment_id, straddle_bg_segment),
    by = c("GEOID", "ward_pair", "segment_id")
  ) %>%
  mutate(straddle_bg_segment = coalesce(straddle_bg_segment, FALSE))

straddle_summary <- tibble(
  snapshot_year = snapshot_year,
  n_obs_total = nrow(analysis_sample),
  n_obs_straddle = sum(analysis_sample$straddle_bg_segment, na.rm = TRUE),
  share_obs_straddle = mean(analysis_sample$straddle_bg_segment, na.rm = TRUE),
  n_geoid_segment_total = n_distinct(paste(analysis_sample$GEOID, analysis_sample$ward_pair, analysis_sample$segment_id, sep = "::")),
  n_geoid_segment_straddle = n_distinct(paste(
    analysis_sample$GEOID[analysis_sample$straddle_bg_segment],
    analysis_sample$ward_pair[analysis_sample$straddle_bg_segment],
    analysis_sample$segment_id[analysis_sample$straddle_bg_segment],
    sep = "::"
  )),
  n_geoid_total = n_distinct(analysis_sample$GEOID),
  n_geoid_any_straddle = n_distinct(analysis_sample$GEOID[analysis_sample$straddle_bg_segment]),
  n_ward_pairs = n_distinct(analysis_sample$ward_pair)
)

write_csv(straddle_summary, output_summary_csv)

main_results <- build_balance_results(analysis_sample, "snapshot2014_all_bg", covariates)
drop_straddle_results <- build_balance_results(
  analysis_sample %>% filter(!straddle_bg_segment),
  "snapshot2014_drop_straddle_bg",
  covariates
)

write_csv(main_results, output_main_csv)
write_csv(drop_straddle_results, output_drop_csv)

main_rows <- main_results %>%
  mutate(
    estimate_display = paste0(fmt_num(estimate), stars(p_value)),
    se_display = paste0("(", fmt_num(se), ")"),
    p_display = fmt_num(p_value),
    n_display = formatC(n_obs, format = "d", big.mark = ","),
    ward_pair_display = formatC(n_ward_pairs, format = "d", big.mark = ",")
  )

drop_rows <- drop_straddle_results %>%
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
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "Covariate & Full sample & Drop straddling BGs \\\\",
  "\\midrule"
)

for (covariate_name in covariates) {
  row_main <- main_rows %>% filter(covariate == covariate_name)
  row_drop <- drop_rows %>% filter(covariate == covariate_name)
  tex_lines <- c(
    tex_lines,
    sprintf(
      "%s & %s & %s \\\\",
      row_main$covariate_label[[1]],
      row_main$estimate_display[[1]],
      row_drop$estimate_display[[1]]
    ),
    sprintf(
      " & %s & %s \\\\",
      row_main$se_display[[1]],
      row_drop$se_display[[1]]
    )
  )
}

tex_lines <- c(
  tex_lines,
  "\\midrule",
  sprintf("N & %s & %s \\\\", main_rows$n_display[[1]], drop_rows$n_display[[1]]),
  sprintf("Ward Pairs & %s & %s \\\\", main_rows$ward_pair_display[[1]], drop_rows$ward_pair_display[[1]]),
  "\\bottomrule",
  "\\end{tabular}",
  sprintf(
    "\\par\\vspace{0.4em}\\parbox{0.88\\linewidth}{\\footnotesize Notes: Sample uses the exact main density design: multifamily projects within 500 feet of a ward boundary with zoning-group, segment, and year support. Covariates are fixed to the 2014 block-group ACS snapshot and assigned to parcels by GEOID. In this sample, %s of %s parcels (%.1f\\%%) belong to block groups that appear on both sides of the same ward-pair segment; the second column drops those observations. Coefficients report the slope on the standardized stringency index from separate regressions of standardized covariates on stringency and side-specific distance controls with zoning-group, segment, and construction-year fixed effects and ward-pair clustered standard errors.}",
    formatC(straddle_summary$n_obs_straddle[[1]], format = "d", big.mark = ","),
    formatC(straddle_summary$n_obs_total[[1]], format = "d", big.mark = ","),
    100 * straddle_summary$share_obs_straddle[[1]]
  ),
  "\\par\\endgroup"
)

writeLines(tex_lines, output_tex)

message("Saved: ", output_main_csv)
message("Saved: ", output_drop_csv)
message("Saved: ", output_summary_csv)
message("Saved: ", output_tex)
