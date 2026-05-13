source("../../setup_environment/code/packages.R")
source("../../_lib/event_study_plot_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/permit_did_control_exploration/code")
# bandwidth_m <- 250

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth_m)
}

if (length(args) != 1) {
  stop("FATAL: Script requires args: <bandwidth_m>", call. = FALSE)
}
bandwidth_m <- as.numeric(args[1])
if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be positive.", call. = FALSE)
}
bandwidth_label <- sprintf("%dm", as.integer(round(bandwidth_m)))

stars <- function(p_value) {
  if (!is.finite(p_value)) {
    return("")
  }
  if (p_value < 0.01) {
    return("***")
  }
  if (p_value < 0.05) {
    return("**")
  }
  if (p_value < 0.1) {
    return("*")
  }
  ""
}

format_number <- function(x, digits = 4) {
  if (!is.finite(x)) {
    return("")
  }
  sprintf(paste0("%.", digits, "f"), x)
}

run_spec <- function(data, spec_id, spec_label, control_terms, complete_vars, control_note) {
  spec_data <- data
  if (length(complete_vars) > 0) {
    spec_data <- spec_data %>%
      filter(if_all(all_of(complete_vars), ~ !is.na(.x)))
  }

  rhs_terms <- c("post_treat", control_terms)
  formula_txt <- sprintf(
    "n_high_discretion_issue ~ %s | block_id + ward_pair_id^year",
    paste(rhs_terms, collapse = " + ")
  )

  message(sprintf(
    "Running %s | n=%s | formula=%s",
    spec_id,
    format(nrow(spec_data), big.mark = ","),
    formula_txt
  ))

  model <- fepois(
    as.formula(formula_txt),
    data = spec_data,
    weights = ~weight,
    cluster = ~block_id
  )

  ct <- coeftable(model)
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  estimate <- coef(model)[["post_treat"]]
  std_error <- se(model)[["post_treat"]]
  p_value <- ct["post_treat", p_col]

  tibble(
    spec_id = spec_id,
    spec_label = spec_label,
    estimator = "PPML",
    estimate = estimate,
    std_error = std_error,
    p_value = p_value,
    stars = stars(p_value),
    effect_pct = 100 * (exp(estimate) - 1),
    n_obs = nobs(model),
    n_blocks = n_distinct(spec_data$block_id[obs(model)]),
    input_blocks = n_distinct(spec_data$block_id),
    ward_pairs = n_distinct(spec_data$ward_pair_id),
    dep_var_mean = mean(spec_data$n_high_discretion_issue, na.rm = TRUE),
    total_outcome = sum(spec_data$n_high_discretion_issue, na.rm = TRUE),
    input_obs = nrow(spec_data),
    dropped_for_fixed_effects = nrow(spec_data) - nobs(model),
    control_note = control_note,
    formula = formula_txt
  )
}

run_linear_spec <- function(data, spec_id, spec_label, control_terms, complete_vars, control_note, sample_note) {
  spec_data <- data
  if (length(complete_vars) > 0) {
    spec_data <- spec_data %>%
      filter(if_all(all_of(complete_vars), ~ !is.na(.x)))
  }

  rhs_terms <- c("post_treat", control_terms)
  formula_txt <- sprintf(
    "n_high_discretion_issue ~ %s | block_id + ward_pair_id^year",
    paste(rhs_terms, collapse = " + ")
  )

  message(sprintf(
    "Running %s | n=%s | formula=%s",
    spec_id,
    format(nrow(spec_data), big.mark = ","),
    formula_txt
  ))

  model <- feols(
    as.formula(formula_txt),
    data = spec_data,
    weights = ~weight,
    cluster = ~block_id
  )

  ct <- coeftable(model)
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  estimate <- coef(model)[["post_treat"]]
  std_error <- se(model)[["post_treat"]]
  p_value <- ct["post_treat", p_col]

  tibble(
    spec_id = spec_id,
    spec_label = spec_label,
    estimator = "Linear FE",
    estimate = estimate,
    std_error = std_error,
    p_value = p_value,
    stars = stars(p_value),
    effect_pct = NA_real_,
    n_obs = nobs(model),
    n_blocks = n_distinct(spec_data$block_id[obs(model)]),
    input_blocks = n_distinct(spec_data$block_id),
    ward_pairs = n_distinct(spec_data$ward_pair_id),
    dep_var_mean = mean(spec_data$n_high_discretion_issue, na.rm = TRUE),
    total_outcome = sum(spec_data$n_high_discretion_issue, na.rm = TRUE),
    input_obs = nrow(spec_data),
    dropped_for_fixed_effects = nrow(spec_data) - nobs(model),
    control_note = paste(control_note, sample_note),
    formula = formula_txt
  )
}

run_event_spec <- function(data, spec_id, spec_label, control_terms, complete_vars, control_note) {
  spec_data <- data
  if (length(complete_vars) > 0) {
    spec_data <- spec_data %>%
      filter(if_all(all_of(complete_vars), ~ !is.na(.x)))
  }

  rhs_terms <- c("i(relative_year, strictness_change, ref = -1)", control_terms)
  formula_txt <- sprintf(
    "n_high_discretion_issue ~ %s | block_id + ward_pair_id^year",
    paste(rhs_terms, collapse = " + ")
  )

  message(sprintf(
    "Running event study %s | n=%s | formula=%s",
    spec_id,
    format(nrow(spec_data), big.mark = ","),
    formula_txt
  ))

  model <- fepois(
    as.formula(formula_txt),
    data = spec_data,
    weights = ~weight,
    cluster = ~block_id
  )

  support_by_event_time <- spec_data %>%
    group_by(event_time = relative_year) %>%
    summarise(
      n_obs = n(),
      n_treated = sum(treat == 1, na.rm = TRUE),
      n_control = sum(treat == 0, na.rm = TRUE),
      contributing_cohorts = paste(sort(unique(cohort)), collapse = "|"),
      n_fe_groups = n_distinct(ward_pair_id),
      n_blocks = n_distinct(block_id),
      n_segments = n_distinct(segment_id_cohort[!is.na(segment_id_cohort)]),
      total_outcome = sum(n_high_discretion_issue, na.rm = TRUE),
      n_positive_rows = sum(n_high_discretion_issue > 0, na.rm = TRUE),
      n_fe_group_time_cells = n_distinct(paste(ward_pair_id, year, sep = "_")),
      n_identifying_fe_group_time_cells = n_fe_group_time_cells,
      n_identifying_fe_groups = n_fe_groups,
      has_treated_and_control = n_treated > 0 & n_control > 0,
      has_identifying_support = TRUE,
      .groups = "drop"
    )

  plot_data <- build_event_study_plot_data(
    model,
    support_by_event_time,
    min_period = -5,
    max_period = 5,
    group_label = spec_label,
    display_mode = "exp_minus_one"
  )

  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop(sprintf("No event-study coefficients were available for %s.", spec_id), call. = FALSE)
  }

  pretrend <- compute_event_study_pretrend(model, plot_data, spec_label) %>%
    mutate(
      spec_id = spec_id,
      spec_label = spec_label,
      .before = group
    )

  list(
    coefficients = plot_data %>%
      mutate(
        spec_id = spec_id,
        spec_label = spec_label,
        control_note = control_note,
        formula = formula_txt,
        .before = group
      ),
    pretrend = pretrend,
    model = model
  )
}

message("Loading 2015 permit block-year panel...")
permit_panel <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    !is.na(block_id), block_id != "",
    !is.na(ward_pair_id), ward_pair_id != "",
    !is.na(strictness_change),
    !is.na(n_high_discretion_issue),
    dist_m <= bandwidth_m,
    relative_year >= -5,
    relative_year <= 5
  ) %>%
  mutate(
    block_id = as.character(block_id),
    weight = 1,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

message("Building pre-period permit activity controls...")
pre_permit_controls <- permit_panel %>%
  filter(relative_year < 0) %>%
  group_by(block_id) %>%
  summarise(
    pre_high_discretion_issue = sum(n_high_discretion_issue, na.rm = TRUE),
    pre_high_new_construction_issue = sum(n_new_construction_issue, na.rm = TRUE),
    pre_high_demolition_issue = sum(n_demolition_issue, na.rm = TRUE),
    pre_high_renovation_issue = sum(n_renovation_issue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    no_pre_high_discretion = as.integer(pre_high_discretion_issue == 0),
    pre_share_high_new_construction = if_else(
      pre_high_discretion_issue > 0,
      pre_high_new_construction_issue / pre_high_discretion_issue,
      0
    ),
    pre_share_high_demolition = if_else(
      pre_high_discretion_issue > 0,
      pre_high_demolition_issue / pre_high_discretion_issue,
      0
    ),
    pre_share_high_renovation = if_else(
      pre_high_discretion_issue > 0,
      pre_high_renovation_issue / pre_high_discretion_issue,
      0
    )
  )

if (anyDuplicated(pre_permit_controls$block_id) > 0) {
  stop("Pre-period permit controls are not unique by block_id.", call. = FALSE)
}

message("Loading block parcel and amenity baselines...")
block_controls <- read_csv("../input/block_parcel_baselines_2014.csv", show_col_types = FALSE) %>%
  mutate(block_id = as.character(block_id))

if (anyDuplicated(block_controls$block_id) > 0) {
  stop("Block parcel baseline controls are not unique by block_id.", call. = FALSE)
}

block_control_vars <- c(
  "n_parcels",
  "mean_zoned_far",
  "mean_dist_cbd_m",
  "mean_nearest_school_dist_m",
  "mean_nearest_park_dist_m",
  "mean_nearest_major_road_dist_m",
  "mean_lake_michigan_dist_m"
)

analysis_data <- permit_panel %>%
  left_join(pre_permit_controls, by = "block_id", relationship = "many-to-one") %>%
  left_join(block_controls, by = "block_id", relationship = "many-to-one") %>%
  mutate(
    no_pre_high_discretion = coalesce(as.integer(no_pre_high_discretion), 1L),
    block_controls_missing = as.integer(if_any(all_of(block_control_vars), ~ is.na(.x))),
    across(all_of(block_control_vars), ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), as.numeric(.x)), .names = "{.col}_imputed"),
    same_year_low_discretion_nosigns_issue = as.numeric(n_low_discretion_nosigns_issue)
  ) %>%
  group_by(block_id) %>%
  mutate(block_high_discretion_total = sum(n_high_discretion_issue, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ward_pair_id, year) %>%
  mutate(ward_pair_year_high_discretion_total = sum(n_high_discretion_issue, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    ppml_kept_support = block_high_discretion_total > 0L & ward_pair_year_high_discretion_total > 0L
  )

block_control_imputed_vars <- paste0(block_control_vars, "_imputed")
pre_high_count_vars <- c(
  "pre_high_new_construction_issue",
  "pre_high_demolition_issue",
  "pre_high_renovation_issue"
)
pre_high_share_vars <- c(
  "pre_share_high_new_construction",
  "pre_share_high_demolition",
  "pre_share_high_renovation"
)
no_pre_high_year_term <- "no_pre_high_discretion:factor(year)"
pre_high_level_year_terms <- c(
  "pre_high_discretion_issue:factor(year)",
  no_pre_high_year_term
)
pre_high_count_year_terms <- c(
  sprintf("%s:factor(year)", pre_high_count_vars),
  no_pre_high_year_term
)
pre_high_share_year_terms <- c(
  sprintf("%s:factor(year)", pre_high_share_vars),
  no_pre_high_year_term
)
pre_high_share_level_year_terms <- c(
  pre_high_share_year_terms,
  "pre_high_discretion_issue:factor(year)"
)
block_control_year_terms <- sprintf("%s:factor(year)", block_control_vars)
block_control_imputed_year_terms <- c(
  sprintf("%s:factor(year)", block_control_imputed_vars),
  "block_controls_missing:factor(year)"
)

results <- bind_rows(
  run_spec(
    analysis_data,
    "baseline",
    "Baseline",
    character(0),
    character(0),
    "Main 250m PPML DID: block FE and ward-pair-by-year FE."
  ),
  run_spec(
    analysis_data,
    "pre_high_level_by_year",
    "Pre-period high-discretion level x year",
    pre_high_level_year_terms,
    c("pre_high_discretion_issue", "no_pre_high_discretion"),
    "Block-level pre-period high-discretion permit count and no-pre-period high-discretion indicator, interacted with calendar year."
  ),
  run_spec(
    analysis_data,
    "pre_high_type_share_by_year",
    "Pre-period NC/demolition/renovation shares x year",
    pre_high_share_year_terms,
    c(pre_high_share_vars, "no_pre_high_discretion"),
    "Block-level pre-period shares of high-discretion permits that are new construction, demolition, or renovation, plus a no-pre-period high-discretion indicator, interacted with calendar year. Porch/reinstatement are the omitted residual category."
  ),
  run_spec(
    analysis_data,
    "pre_high_type_count_by_year",
    "Pre-period NC/demolition/renovation counts x year",
    pre_high_count_year_terms,
    c(pre_high_count_vars, "no_pre_high_discretion"),
    "Block-level pre-period counts of high-discretion new construction, demolition, and renovation permits, plus a no-pre-period high-discretion indicator, interacted with calendar year."
  ),
  run_spec(
    analysis_data,
    "pre_high_type_share_level_by_year",
    "Pre-period type shares + high-discretion level x year",
    pre_high_share_level_year_terms,
    c(pre_high_share_vars, "pre_high_discretion_issue", "no_pre_high_discretion"),
    "Adds the pre-period high-discretion permit count and no-pre-period high-discretion indicator to the type shares, all interacted with calendar year."
  ),
  run_spec(
    analysis_data,
    "same_year_low_discretion",
    "Same-year low-discretion count",
    "same_year_low_discretion_nosigns_issue",
    "same_year_low_discretion_nosigns_issue",
    "Diagnostic only: controls for contemporaneous low-discretion permit counts, which could itself respond to treatment."
  ),
  run_spec(
    analysis_data,
    "baseline_block_control_sample",
    "Baseline on block-control sample",
    character(0),
    block_control_vars,
    "No controls, but restricted to observations with complete block parcel/amenity baselines."
  ),
  run_spec(
    analysis_data,
    "block_controls_imputed_by_year",
    "Block controls x year, full sample with missing flag",
    block_control_imputed_year_terms,
    c(block_control_imputed_vars, "block_controls_missing"),
    "Block-level parcel/amenity baselines interacted with year; missing baseline values are mean-imputed and a missing-baseline flag is also interacted with year."
  ),
  run_spec(
    analysis_data,
    "block_controls_by_year",
    "Block parcel/amenity controls x year",
    block_control_year_terms,
    block_control_vars,
    "Block-level 2014 parcel, zoning, and amenity-distance baselines interacted with calendar year."
  ),
  run_spec(
    analysis_data,
    "pre_high_share_plus_block_imputed_by_year",
    "Type shares + full-sample block controls x year",
    c(pre_high_share_year_terms, block_control_imputed_year_terms),
    c(pre_high_share_vars, "no_pre_high_discretion", block_control_imputed_vars, "block_controls_missing"),
    "Combines high-discretion type shares and full-sample imputed block parcel/amenity controls, all interacted with calendar year."
  ),
  run_spec(
    analysis_data,
    "all_exploratory_controls",
    "All exploratory controls",
    c(pre_high_share_year_terms, block_control_imputed_year_terms, "same_year_low_discretion_nosigns_issue"),
    c(pre_high_share_vars, "no_pre_high_discretion", block_control_imputed_vars, "block_controls_missing", "same_year_low_discretion_nosigns_issue"),
    "Diagnostic only: combines high-discretion type shares, full-sample block controls, and contemporaneous low-discretion counts."
  ),
  run_linear_spec(
    analysis_data,
    "linear_baseline_full_panel",
    "Linear FE baseline, full panel",
    character(0),
    character(0),
    "Linear count model with block FE and ward-pair-by-year FE.",
    "Uses the full panel, including all-zero blocks."
  ),
  run_linear_spec(
    analysis_data,
    "linear_pre_high_level_full_panel",
    "Linear FE pre high-discretion level, full panel",
    pre_high_level_year_terms,
    c("pre_high_discretion_issue", "no_pre_high_discretion"),
    "Linear count model with pre-period high-discretion permit count and no-pre-period indicator interacted with calendar year.",
    "Uses the full panel, including all-zero blocks."
  ),
  run_linear_spec(
    analysis_data %>% filter(ppml_kept_support),
    "linear_baseline_ppml_support",
    "Linear FE baseline, PPML support",
    character(0),
    character(0),
    "Linear count model with block FE and ward-pair-by-year FE.",
    "Restricts to the non-separated PPML support."
  ),
  run_linear_spec(
    analysis_data %>% filter(ppml_kept_support),
    "linear_pre_high_level_ppml_support",
    "Linear FE pre high-discretion level, PPML support",
    pre_high_level_year_terms,
    c("pre_high_discretion_issue", "no_pre_high_discretion"),
    "Linear count model with pre-period high-discretion permit count and no-pre-period indicator interacted with calendar year.",
    "Restricts to the non-separated PPML support."
  )
) %>%
  mutate(
    bandwidth_m = bandwidth_m,
    cluster_level = "block",
    fixed_effects = "block_id + ward_pair_id^year",
    weighting = "uniform"
  )

write_csv(results, sprintf("../output/permit_did_control_exploration_%s.csv", bandwidth_label))

event_specs <- list(
  list(
    spec_id = "baseline",
    spec_label = "Baseline",
    control_terms = character(0),
    complete_vars = character(0),
    control_note = "Main 250m PPML event study: block FE and ward-pair-by-year FE."
  ),
  list(
    spec_id = "pre_high_level_by_year",
    spec_label = "Pre-period high-discretion level x year",
    control_terms = pre_high_level_year_terms,
    complete_vars = c("pre_high_discretion_issue", "no_pre_high_discretion"),
    control_note = "Pre-period high-discretion permit count and no-pre-period high-discretion indicator, interacted with calendar year."
  ),
  list(
    spec_id = "pre_high_type_share_by_year",
    spec_label = "Pre-period type shares x year",
    control_terms = pre_high_share_year_terms,
    complete_vars = c(pre_high_share_vars, "no_pre_high_discretion"),
    control_note = "Pre-period high-discretion new-construction, demolition, and renovation shares plus no-pre-period high-discretion indicator, interacted with calendar year."
  ),
  list(
    spec_id = "pre_high_type_count_by_year",
    spec_label = "Pre-period type counts x year",
    control_terms = pre_high_count_year_terms,
    complete_vars = c(pre_high_count_vars, "no_pre_high_discretion"),
    control_note = "Pre-period high-discretion new-construction, demolition, and renovation counts plus no-pre-period high-discretion indicator, interacted with calendar year."
  ),
  list(
    spec_id = "pre_high_type_share_level_by_year",
    spec_label = "Pre-period type shares + level x year",
    control_terms = pre_high_share_level_year_terms,
    complete_vars = c(pre_high_share_vars, "pre_high_discretion_issue", "no_pre_high_discretion"),
    control_note = "Pre-period high-discretion type shares, total high-discretion permit count, and no-pre-period high-discretion indicator, interacted with calendar year."
  )
)

event_outputs <- lapply(event_specs, function(spec_i) {
  run_event_spec(
    data = analysis_data,
    spec_id = spec_i$spec_id,
    spec_label = spec_i$spec_label,
    control_terms = spec_i$control_terms,
    complete_vars = spec_i$complete_vars,
    control_note = spec_i$control_note
  )
})

event_coefficients <- bind_rows(lapply(event_outputs, `[[`, "coefficients"))
event_pretrends <- bind_rows(lapply(event_outputs, `[[`, "pretrend"))

write_csv(event_coefficients, sprintf("../output/permit_event_study_control_exploration_%s.csv", bandwidth_label))
write_csv(event_pretrends, sprintf("../output/permit_event_study_control_pretrends_%s.csv", bandwidth_label))

for (spec_i in event_specs) {
  plot_i <- event_coefficients %>%
    filter(spec_id == spec_i$spec_id)
  event_plot_i <- make_event_study_single_series_plot(
    plot_i,
    plot_title = spec_i$spec_label,
    x_label = "Years relative to redistricting",
    y_label = "Effect on issued high-discretion permits",
    display_suffix = "%"
  )
  ggsave(
    sprintf("../output/permit_event_study_control_%s_%s.pdf", spec_i$spec_id, bandwidth_label),
    event_plot_i,
    width = 7,
    height = 4.5,
    bg = "white"
  )
  ggsave(
    sprintf("../output/permit_event_study_control_%s_%s.png", spec_i$spec_id, bandwidth_label),
    event_plot_i,
    width = 7,
    height = 4.5,
    dpi = 220,
    bg = "white"
  )
}

combined_event_plot <- ggplot(event_coefficients, aes(x = event_time, y = estimate_display)) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ci_low_display, ymax = ci_high_display), fill = solid_event_study_band_fill("#009E73", 0.18), color = NA) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
  geom_line(color = "#009E73", linewidth = 0.8) +
  geom_point(color = "#009E73", size = 1.8) +
  scale_x_continuous(breaks = -5:5) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  facet_wrap(~spec_label, ncol = 2) +
  labs(
    x = "Years relative to redistricting",
    y = "Effect on issued high-discretion permits"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    axis.line = element_line(color = "gray40", linewidth = 0.3),
    axis.ticks = element_line(color = "gray40", linewidth = 0.3),
    axis.title = element_text(size = 10, color = "gray20"),
    axis.text = element_text(size = 8, color = "gray30"),
    strip.text = element_text(face = "bold", size = 9),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )

ggsave(
  sprintf("../output/permit_event_study_control_comparison_%s.pdf", bandwidth_label),
  combined_event_plot,
  width = 9,
  height = 7,
  bg = "white"
)
ggsave(
  sprintf("../output/permit_event_study_control_comparison_%s.png", bandwidth_label),
  combined_event_plot,
  width = 9,
  height = 7,
  dpi = 220,
  bg = "white"
)

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\small",
  "\\begin{tabular}{llccccc}",
  "\\toprule",
  "Specification & Estimator & Estimate & SE & p-value & N & Blocks \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(results))) {
  row_i <- results[i, ]
  table_lines <- c(
    table_lines,
    sprintf(
      "%s & %s & %s%s & (%s) & %s & %s & %s \\\\",
      row_i$spec_label,
      row_i$estimator,
      format_number(row_i$estimate, 4),
      row_i$stars,
      format_number(row_i$std_error, 4),
      format_number(row_i$p_value, 3),
      format(row_i$n_obs, big.mark = ","),
      format(row_i$n_blocks, big.mark = ",")
    )
  )
}

table_lines <- c(
  table_lines,
  "\\bottomrule",
  "\\end{tabular}",
  sprintf(
    "\\par\\vspace{0.5em}\\parbox{0.95\\linewidth}{\\footnotesize Notes: Exploratory 250m permit DID controls. Outcome is issued high-discretion permit count. PPML rows report semi-elasticities; linear FE rows report permit-count effects. All specifications use block fixed effects, ward-pair-by-year fixed effects, uniform weights, and block-clustered standard errors. Time-invariant controls enter interacted with calendar year. Same-year low-discretion controls are diagnostic only.}"
  ),
  "\\par\\endgroup"
)

writeLines(table_lines, sprintf("../output/permit_did_control_exploration_%s.tex", bandwidth_label))

print(results %>% select(spec_label, estimator, estimate, std_error, p_value, effect_pct, n_obs, n_blocks, ward_pairs))
