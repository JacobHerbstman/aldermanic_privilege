# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/density_corner_clean_table/code")
# bandwidth_m <- 152.4
# sample_filter <- "all"
# bandwidth_label <- "500ft"

source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_m, sample_filter, bandwidth_label)
}

if (length(cli_args) != 3) {
  stop(
    "FATAL: Script requires 3 args: <bandwidth_m> <sample_filter> <bandwidth_label>.",
    call. = FALSE
  )
}

bandwidth_m <- as.numeric(cli_args[1])
sample_filter <- cli_args[2]
bandwidth_label <- cli_args[3]

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be a positive number.", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample_filter must be one of: all, multifamily.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}

outcome_order <- c("log(density_far)", "log(density_dupac)")
controls <- c(
  "strictness_own",
  "lenient_dist",
  "strict_dist",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year)),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    if (sample_filter == "all") unitscount > 0 else unitscount > 1
  )

ambiguity <- read_csv(
  "../input/parcel_other_pair_distance.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year))
  ) %>%
  select(pin, construction_year, nearest_other_pair_dist_m)

if (anyDuplicated(ambiguity[c("pin", "construction_year")]) > 0) {
  stop("Ambiguity input has duplicate pin-construction_year keys.", call. = FALSE)
}

parcels <- parcels %>%
  left_join(ambiguity, by = c("pin", "construction_year"), relationship = "many-to-one")

ambiguity_summary <- read_csv("../input/boundary_ambiguity_by_bw.csv", show_col_types = FALSE)
if (!"bandwidth_m" %in% names(ambiguity_summary)) {
  if (!"bw_ft" %in% names(ambiguity_summary)) {
    stop("Ambiguity summary must contain bandwidth_m.", call. = FALSE)
  }
  ambiguity_summary <- ambiguity_summary %>%
    mutate(bandwidth_m = as.numeric(bw_ft) * 0.3048)
}

ambiguity_row <- ambiguity_summary %>%
  mutate(bandwidth_m = as.numeric(bandwidth_m)) %>%
  filter(sample_filter == .env$sample_filter, round(bandwidth_m) == round(.env$bandwidth_m))

if (nrow(ambiguity_row) != 1) {
  stop("Expected one ambiguity row for the requested sample and bandwidth.", call. = FALSE)
}

summary_rows <- list()
for (drop_ambiguous in c(FALSE, TRUE)) {
  for (yvar in outcome_order) {
    base_var <- gsub("^log\\(|\\)$", "", yvar)
    df <- parcels %>%
      filter(
        dist_to_boundary_m <= bandwidth_m,
        !is.na(segment_id),
        segment_id != "",
        is.finite(.data[[base_var]])
      )

    if (grepl("^log\\(.+\\)$", yvar)) {
      df <- df %>% filter(.data[[base_var]] > 0)
    }

    if (drop_ambiguous) {
      if (anyNA(df$nearest_other_pair_dist_m)) {
        stop("Missing ambiguity distances for filtered rows.", call. = FALSE)
      }
      df <- df %>% filter(nearest_other_pair_dist_m > bandwidth_m)
    }

    if (nrow(df) == 0) {
      stop(sprintf("No rows remain for '%s' after filtering.", yvar), call. = FALSE)
    }

    model <- feols(
      as.formula(paste0(
        yvar,
        " ~ ",
        paste(controls, collapse = " + "),
        " | zone_group + segment_id + construction_year"
      )),
      data = df,
      cluster = ~ward_pair
    )

    coef_table <- coeftable(model)
    if (!"strictness_own" %in% rownames(coef_table)) {
      stop(sprintf("Model failed to estimate strictness_own for '%s'.", yvar), call. = FALSE)
    }

    summary_rows[[length(summary_rows) + 1]] <- tibble(
      drop_ambiguous = drop_ambiguous,
      yvar = yvar,
      estimate = unname(coef_table["strictness_own", "Estimate"]),
      se = unname(coef_table["strictness_own", "Std. Error"]),
      p_value = unname(coef_table["strictness_own", "Pr(>|t|)"]),
      n_obs = nobs(model),
      n_ward_pairs = n_distinct(df$ward_pair),
      depvar_mean = mean(df[[base_var]], na.rm = TRUE)
    )
  }
}

summary_panels <- bind_rows(summary_rows)

baseline_panel <- summary_panels %>% filter(!drop_ambiguous)
corner_clean_panel <- summary_panels %>% filter(drop_ambiguous)

if (nrow(baseline_panel) != 2 || nrow(corner_clean_panel) != 2) {
  stop("Expected exactly two outcomes in each summary file.", call. = FALSE)
}
if (!all(baseline_panel$yvar == outcome_order) || !all(corner_clean_panel$yvar == outcome_order)) {
  stop("Summary rows are not aligned to FAR and DUPAC in the expected order.", call. = FALSE)
}

model_drop_n <- baseline_panel$n_obs - corner_clean_panel$n_obs
if (length(unique(model_drop_n)) != 1) {
  stop("Corner-clean model drops differ across outcomes.", call. = FALSE)
}
if (!identical(as.numeric(model_drop_n[[1]]), as.numeric(ambiguity_row$n_ambiguous[[1]]))) {
  stop("Corner-clean model drop count does not match ambiguity summary at requested bandwidth.", call. = FALSE)
}

baseline_panel <- baseline_panel %>%
  mutate(
    stars = case_when(
      !is.finite(p_value) ~ "",
      p_value <= 0.01 ~ "$^{***}$",
      p_value <= 0.05 ~ "$^{**}$",
      p_value <= 0.10 ~ "$^{*}$",
      TRUE ~ ""
    )
  )
corner_clean_panel <- corner_clean_panel %>%
  mutate(
    stars = case_when(
      !is.finite(p_value) ~ "",
      p_value <= 0.01 ~ "$^{***}$",
      p_value <= 0.05 ~ "$^{**}$",
      p_value <= 0.10 ~ "$^{*}$",
      TRUE ~ ""
    )
  )

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "   \\toprule",
  "                    & \\multicolumn{2}{c}{Baseline} & \\multicolumn{2}{c}{Corner-Clean}\\\\",
  "                    & ln(FAR) & ln(DUPAC) & ln(FAR) & ln(DUPAC)\\\\",
  "                    & (1) & (2) & (3) & (4)\\\\",
  "   \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "   Stringency Index",
  sprintf(
    "                    & %s%s & %s%s & %s%s & %s%s\\\\",
    sprintf("%.2f", baseline_panel$estimate[[1]]),
    baseline_panel$stars[[1]],
    sprintf("%.2f", baseline_panel$estimate[[2]]),
    baseline_panel$stars[[2]],
    sprintf("%.2f", corner_clean_panel$estimate[[1]]),
    corner_clean_panel$stars[[1]],
    sprintf("%.2f", corner_clean_panel$estimate[[2]]),
    corner_clean_panel$stars[[2]]
  ),
  sprintf(
    "                    & (%s) & (%s) & (%s) & (%s)\\\\",
    sprintf("%.2f", baseline_panel$se[[1]]),
    sprintf("%.2f", baseline_panel$se[[2]]),
    sprintf("%.2f", corner_clean_panel$se[[1]]),
    sprintf("%.2f", corner_clean_panel$se[[2]])
  ),
  "    \\\\",
  "   Zoning Group FE  & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$\\\\",
  "   Segment FE       & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$\\\\",
  "   Year FE          & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$\\\\",
  sprintf(
    "   N                & %s & %s & %s & %s\\\\",
    format(baseline_panel$n_obs[[1]], big.mark = ","),
    format(baseline_panel$n_obs[[2]], big.mark = ","),
    format(corner_clean_panel$n_obs[[1]], big.mark = ","),
    format(corner_clean_panel$n_obs[[2]], big.mark = ",")
  ),
  sprintf(
    "   Dep. Var. Mean   & %s & %s & %s & %s\\\\",
    sprintf("%.2f", baseline_panel$depvar_mean[[1]]),
    sprintf("%.2f", baseline_panel$depvar_mean[[2]]),
    sprintf("%.2f", corner_clean_panel$depvar_mean[[1]]),
    sprintf("%.2f", corner_clean_panel$depvar_mean[[2]])
  ),
  sprintf(
    "   Ward Pairs       & %s & %s & %s & %s\\\\",
    format(baseline_panel$n_ward_pairs[[1]], big.mark = ","),
    format(baseline_panel$n_ward_pairs[[2]], big.mark = ","),
    format(corner_clean_panel$n_ward_pairs[[1]], big.mark = ","),
    format(corner_clean_panel$n_ward_pairs[[2]], big.mark = ",")
  ),
  "   \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup",
  ""
)

writeLines(
  table_lines,
  sprintf("../output/fe_table_%s_%s_corner_clean_compare.tex", bandwidth_label, sample_filter)
)
