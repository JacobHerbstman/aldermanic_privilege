source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/event_study_permit_diagnostics/code")
# panel_input <- "../input/permit_block_year_panel.parquet"
# controls_input <- "../input/block_group_controls.csv"
# balance_output <- "../output/permit_balance_summary.csv"
# balance_tex_output <- "../output/permit_balance_summary.tex"
# joint_output <- "../output/permit_balance_joint_summary.csv"
# joint_tex_output <- "../output/permit_balance_joint_summary.tex"
# did_output <- "../output/permit_balance_did_shift.csv"
# did_tex_output <- "../output/permit_balance_did_shift.tex"
# plot_output <- "../output/permit_balance_love_plot.pdf"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(panel_input, controls_input, balance_output, balance_tex_output, joint_output, joint_tex_output, did_output, did_tex_output, plot_output)
}

if (length(cli_args) != 9) {
  stop(
    "FATAL: Script requires 9 args: <panel_input> <controls_input> <balance_output> <balance_tex_output> <joint_output> <joint_tex_output> <did_output> <did_tex_output> <plot_output>",
    call. = FALSE
  )
}

panel_input <- cli_args[1]
controls_input <- cli_args[2]
balance_output <- cli_args[3]
balance_tex_output <- cli_args[4]
joint_output <- cli_args[5]
joint_tex_output <- cli_args[6]
did_output <- cli_args[7]
did_tex_output <- cli_args[8]
plot_output <- cli_args[9]

safe_scale <- function(x) {
  sigma <- sd(x, na.rm = TRUE)
  mu <- mean(x, na.rm = TRUE)
  if (!is.finite(sigma) || sigma == 0) {
    return(rep(0, length(x)))
  }
  (x - mu) / sigma
}

pooled_sd <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 2 || length(y) < 2) {
    return(NA_real_)
  }
  pooled_var <- ((length(x) - 1) * stats::var(x) + (length(y) - 1) * stats::var(y)) /
    (length(x) + length(y) - 2)
  if (!is.finite(pooled_var) || pooled_var <= 0) {
    return(NA_real_)
  }
  sqrt(pooled_var)
}

compute_linear_r2 <- function(df, outcome_var, covariates) {
  model <- lm(
    reformulate(covariates, response = outcome_var),
    data = df
  )
  summary(model)$r.squared
}

extract_p_value <- function(coef_row) {
  p_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), names(coef_row))
  if (length(p_col) == 0) {
    return(NA_real_)
  }
  unname(coef_row[p_col[[1]]])
}

fmt_decimal <- function(x, digits = 2) {
  if (!is.finite(x)) {
    return("")
  }
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_integer <- function(x) {
  if (!is.finite(x)) {
    return("")
  }
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_share_level <- function(x) {
  fmt_decimal(100 * x, 1)
}

fmt_covariate_value <- function(covariate_name, x) {
  if (!is.finite(x)) {
    return("")
  }
  if (covariate_name %in% c("baseline_homeownership_rate", "baseline_share_bach_plus", "baseline_percent_black", "baseline_percent_hispanic")) {
    return(fmt_share_level(x))
  }
  if (covariate_name == "baseline_median_income") {
    return(fmt_integer(x))
  }
  if (covariate_name %in% c("baseline_population_density", "dist_ft")) {
    return(fmt_integer(x))
  }
  if (covariate_name == "baseline_median_age") {
    return(fmt_decimal(x, 1))
  }
  if (covariate_name == "pre_high_discretion_count") {
    return(fmt_decimal(x, 2))
  }
  fmt_decimal(x, 2)
}

write_lines_safe <- function(lines, path) {
  writeLines(lines, path)
}

comparison_catalog <- tibble(
  comparison = c("treated_vs_control", "to_stricter_vs_control", "to_lenient_vs_control"),
  comparison_label = c("Treated vs Control", "To Stricter vs Control", "To Lenient vs Control")
)

covariate_catalog <- tibble(
  covariate = c(
    "baseline_median_income",
    "baseline_homeownership_rate",
    "baseline_share_bach_plus",
    "baseline_median_age",
    "baseline_population_density",
    "baseline_percent_black",
    "baseline_percent_hispanic",
    "dist_ft",
    "pre_high_discretion_count"
  ),
  covariate_label = c(
    "Median Income",
    "Homeownership Rate",
    "Bachelor's Share",
    "Median Age",
    "Population Density",
    "Percent Black",
    "Percent Hispanic",
    "Distance to Boundary (ft)",
    "High-Discretion Permits at t = -1"
  )
)

control_vars <- covariate_catalog$covariate[grepl("^baseline_", covariate_catalog$covariate)]

message("Loading permit panel...")
permit_panel <- read_parquet(panel_input)

message("Loading block-group controls...")
baseline_controls <- read_csv(controls_input, show_col_types = FALSE) %>%
  transmute(
    block_group_id = as.character(GEOID),
    baseline_year = as.integer(year),
    baseline_median_income = median_income,
    baseline_homeownership_rate = homeownership_rate,
    baseline_share_bach_plus = share_bach_plus,
    baseline_median_age = median_age,
    baseline_population_density = population_density,
    baseline_percent_black = percent_black,
    baseline_percent_hispanic = percent_hispanic
  )

build_balance_sample <- function(panel_mode) {
  sample_df <- permit_panel
  if (panel_mode == "cohort_2015") {
    sample_df <- sample_df %>% filter(cohort == "2015")
  }
  if (panel_mode == "cohort_2023") {
    sample_df <- sample_df %>% filter(cohort == "2023")
  }

  pair_var <- if (panel_mode == "stacked_implementation") "cohort_ward_pair" else "ward_pair_id"
  block_var <- if (panel_mode == "stacked_implementation") "cohort_block_id" else "block_id"

  sample_df %>%
    filter(!is.na(strictness_change), dist_ft <= 1000, relative_year == -1) %>%
    filter(!is.na(.data[[pair_var]]), .data[[pair_var]] != "") %>%
    mutate(
      sample = panel_mode,
      sample_label = case_when(
        panel_mode == "cohort_2015" ~ "2015 Cohort",
        panel_mode == "cohort_2023" ~ "2023 Cohort",
        TRUE ~ "Stacked"
      ),
      pair_id = .data[[pair_var]],
      block_sample_id = .data[[block_var]],
      block_group_id = substr(as.character(block_id), 1, 12),
      baseline_year = case_when(
        cohort == "2015" ~ 2014L,
        cohort == "2023" ~ 2022L,
        TRUE ~ NA_integer_
      ),
      group = case_when(
        treat == 0 ~ "control",
        strictness_change > 0 ~ "to_stricter",
        strictness_change < 0 ~ "to_lenient",
        TRUE ~ "treated_zero_change"
      ),
      pre_high_discretion_count = n_high_discretion_issue
    ) %>%
    left_join(baseline_controls, by = c("block_group_id", "baseline_year")) %>%
    mutate(complete_controls = if_all(all_of(control_vars), ~ !is.na(.x))) %>%
    filter(group != "treated_zero_change")
}

build_balance_rows <- function(sample_df) {
  detail_df <- sample_df %>%
    filter(complete_controls) %>%
    mutate(
      treated_any = as.integer(group != "control")
    ) %>%
    group_by(pair_id) %>%
    mutate(across(all_of(covariate_catalog$covariate), ~ .x - mean(.x, na.rm = TRUE), .names = "{.col}_within_pair")) %>%
    ungroup()

  bind_rows(lapply(seq_len(nrow(comparison_catalog)), function(i) {
    comparison_i <- comparison_catalog[i, ]
    subset_df <- if (comparison_i$comparison == "treated_vs_control") {
      detail_df %>%
        mutate(comparison_group = if_else(group == "control", "control", "treated")) %>%
        filter(comparison_group %in% c("treated", "control"))
    } else if (comparison_i$comparison == "to_stricter_vs_control") {
      detail_df %>%
        mutate(comparison_group = case_when(
          group == "to_stricter" ~ "treated",
          group == "control" ~ "control",
          TRUE ~ NA_character_
        )) %>%
        filter(!is.na(comparison_group))
    } else {
      detail_df %>%
        mutate(comparison_group = case_when(
          group == "to_lenient" ~ "treated",
          group == "control" ~ "control",
          TRUE ~ NA_character_
        )) %>%
        filter(!is.na(comparison_group))
    }

    subset_df <- subset_df %>%
      mutate(comparison_treated = as.integer(comparison_group == "treated"))

    bind_rows(lapply(covariate_catalog$covariate, function(covariate_name) {
      treated_values <- subset_df %>%
        filter(comparison_group == "treated") %>%
        pull(!!sym(covariate_name))
      control_values <- subset_df %>%
        filter(comparison_group == "control") %>%
        pull(!!sym(covariate_name))
      treated_within <- subset_df %>%
        filter(comparison_group == "treated") %>%
        pull(!!sym(paste0(covariate_name, "_within_pair")))
      control_within <- subset_df %>%
        filter(comparison_group == "control") %>%
        pull(!!sym(paste0(covariate_name, "_within_pair")))

      treated_mean <- mean(treated_values, na.rm = TRUE)
      control_mean <- mean(control_values, na.rm = TRUE)
      raw_diff <- treated_mean - control_mean
      within_pair_diff <- mean(treated_within, na.rm = TRUE) - mean(control_within, na.rm = TRUE)
      raw_model <- feols(
        as.formula(sprintf("%s ~ comparison_treated", covariate_name)),
        data = subset_df,
        cluster = ~pair_id
      )
      within_pair_model <- feols(
        as.formula(sprintf("%s ~ comparison_treated | pair_id", covariate_name)),
        data = subset_df,
        cluster = ~pair_id
      )
      raw_coef <- coeftable(raw_model)["comparison_treated", ]
      within_pair_coef <- coeftable(within_pair_model)["comparison_treated", ]

      tibble(
        sample = first(subset_df$sample),
        sample_label = first(subset_df$sample_label),
        comparison = comparison_i$comparison,
        comparison_label = comparison_i$comparison_label,
        covariate = covariate_name,
        n_treated = sum(is.finite(treated_values)),
        n_control = sum(is.finite(control_values)),
        treated_mean = treated_mean,
        control_mean = control_mean,
        raw_diff = raw_diff,
        raw_diff_se = unname(raw_coef["Std. Error"]),
        raw_diff_p_value = extract_p_value(raw_coef),
        raw_smd = raw_diff / pooled_sd(treated_values, control_values),
        within_pair_diff = within_pair_diff,
        within_pair_diff_se = unname(within_pair_coef["Std. Error"]),
        within_pair_diff_p_value = extract_p_value(within_pair_coef),
        within_pair_smd = within_pair_diff / pooled_sd(treated_within, control_within)
      )
    }))
  })) %>%
    left_join(covariate_catalog, by = "covariate")
}

build_joint_rows <- function(sample_df, balance_rows) {
  detail_df <- sample_df %>%
    filter(complete_controls) %>%
    mutate(
      treated_any = as.integer(group != "control")
    ) %>%
    group_by(pair_id) %>%
    mutate(across(all_of(c(control_vars, "dist_ft", "pre_high_discretion_count")), ~ .x - mean(.x, na.rm = TRUE), .names = "{.col}_within_pair")) %>%
    ungroup()

  bind_rows(lapply(seq_len(nrow(comparison_catalog)), function(i) {
    comparison_i <- comparison_catalog[i, ]

    subset_df <- if (comparison_i$comparison == "treated_vs_control") {
      detail_df %>%
        mutate(outcome_binary = as.integer(group != "control")) %>%
        filter(group %in% c("control", "to_stricter", "to_lenient"))
    } else if (comparison_i$comparison == "to_stricter_vs_control") {
      detail_df %>%
        filter(group %in% c("control", "to_stricter")) %>%
        mutate(outcome_binary = as.integer(group == "to_stricter"))
    } else {
      detail_df %>%
        filter(group %in% c("control", "to_lenient")) %>%
        mutate(outcome_binary = as.integer(group == "to_lenient"))
    }

    within_df <- subset_df %>%
      group_by(pair_id) %>%
      mutate(
        outcome_binary_within_pair = outcome_binary - mean(outcome_binary, na.rm = TRUE),
        across(all_of(c(control_vars, "dist_ft")), ~ .x - mean(.x, na.rm = TRUE), .names = "{.col}_within_pair")
      ) %>%
      ungroup()

    summary_row <- balance_rows %>%
      filter(sample == first(subset_df$sample), comparison == comparison_i$comparison) %>%
      summarise(
        mean_abs_raw_smd = mean(abs(raw_smd), na.rm = TRUE),
        max_abs_raw_smd = max(abs(raw_smd), na.rm = TRUE),
        mean_abs_within_pair_smd = mean(abs(within_pair_smd), na.rm = TRUE),
        max_abs_within_pair_smd = max(abs(within_pair_smd), na.rm = TRUE)
      )

    tibble(
      sample = first(subset_df$sample),
      sample_label = first(subset_df$sample_label),
      comparison = comparison_i$comparison,
      comparison_label = comparison_i$comparison_label,
      n_blocks_before_control_filter = nrow(sample_df),
      n_blocks_complete_controls = nrow(detail_df),
      dropped_blocks_for_missing_controls = sum(!sample_df$complete_controls),
      dropped_share_pct = 100 * mean(!sample_df$complete_controls),
      n_pairs = n_distinct(subset_df$pair_id),
      n_treated = sum(subset_df$outcome_binary == 1),
      n_control = sum(subset_df$outcome_binary == 0),
      raw_r2 = compute_linear_r2(subset_df, "outcome_binary", c(control_vars, "dist_ft", "pre_high_discretion_count")),
      within_pair_r2 = compute_linear_r2(within_df, "outcome_binary_within_pair", paste0(c(control_vars, "dist_ft", "pre_high_discretion_count"), "_within_pair")),
      mean_abs_raw_smd = summary_row$mean_abs_raw_smd,
      max_abs_raw_smd = summary_row$max_abs_raw_smd,
      mean_abs_within_pair_smd = summary_row$mean_abs_within_pair_smd,
      max_abs_within_pair_smd = summary_row$max_abs_within_pair_smd
    )
  }))
}

build_did_sample <- function(panel_mode) {
  sample_df <- permit_panel
  if (panel_mode == "cohort_2015") {
    sample_df <- sample_df %>% filter(cohort == "2015")
  }
  if (panel_mode == "cohort_2023") {
    sample_df <- sample_df %>% filter(cohort == "2023")
  }

  sample_df <- sample_df %>%
    filter(!is.na(strictness_change), dist_ft <= 1000, relative_year >= -5, relative_year <= 5) %>%
    mutate(
      block_group_id = substr(as.character(block_id), 1, 12),
      baseline_year = case_when(
        cohort == "2015" ~ 2014L,
        cohort == "2023" ~ 2022L,
        TRUE ~ NA_integer_
      )
    ) %>%
    left_join(baseline_controls, by = c("block_group_id", "baseline_year"))

  if (panel_mode == "stacked_implementation") {
    sample_df <- sample_df %>%
      mutate(
        block_fe_id = cohort_block_id,
        pair_fe_id = cohort_ward_pair
      )
  } else {
    sample_df <- sample_df %>%
      mutate(
        block_fe_id = block_id,
        pair_fe_id = ward_pair_id
      )
  }

  sample_df %>%
    filter(!is.na(pair_fe_id), pair_fe_id != "") %>%
    filter(if_all(all_of(control_vars), ~ !is.na(.x))) %>%
    mutate(
      sample = panel_mode,
      sample_label = case_when(
        panel_mode == "cohort_2015" ~ "2015 Cohort",
        panel_mode == "cohort_2023" ~ "2023 Cohort",
        TRUE ~ "Stacked"
      ),
      weight = 1,
      post = as.integer(relative_year >= 0),
      post_treat = post * strictness_change,
      across(all_of(control_vars), safe_scale, .names = "{.col}_z")
    )
}

fit_did_rows <- function(did_df) {
  fe_formula <- if (first(did_df$sample) == "stacked_implementation") {
    "cohort_block_id + cohort_ward_pair^year"
  } else {
    "block_id + ward_pair_id^year"
  }
  cluster_formula <- if (first(did_df$sample) == "stacked_implementation") ~cohort_block_id else ~block_id

  rhs_no_controls <- "post_treat"
  rhs_with_controls <- paste(
    rhs_no_controls,
    paste(sprintf("%s:factor(year)", paste0(control_vars, "_z")), collapse = " + "),
    sep = " + "
  )

  no_controls_model <- fepois(
    as.formula(sprintf("n_high_discretion_issue ~ %s | %s", rhs_no_controls, fe_formula)),
    data = did_df,
    weights = ~weight,
    cluster = cluster_formula
  )
  with_controls_model <- fepois(
    as.formula(sprintf("n_high_discretion_issue ~ %s | %s", rhs_with_controls, fe_formula)),
    data = did_df,
    weights = ~weight,
    cluster = cluster_formula
  )

  no_controls_coef <- coeftable(no_controls_model)["post_treat", ]
  with_controls_coef <- coeftable(with_controls_model)["post_treat", ]

  tibble(
    sample = first(did_df$sample),
    sample_label = first(did_df$sample_label),
    outcome = "Issued High-Discretion Permits",
    estimate_no_controls = unname(no_controls_coef["Estimate"]),
    se_no_controls = unname(no_controls_coef["Std. Error"]),
    p_no_controls = unname(no_controls_coef["Pr(>|z|)"]),
    implied_pct_no_controls = 100 * (exp(estimate_no_controls) - 1),
    estimate_with_controls = unname(with_controls_coef["Estimate"]),
    se_with_controls = unname(with_controls_coef["Std. Error"]),
    p_with_controls = unname(with_controls_coef["Pr(>|z|)"]),
    implied_pct_with_controls = 100 * (exp(estimate_with_controls) - 1),
    beta_shift = estimate_with_controls - estimate_no_controls,
    beta_shift_pct_of_baseline = if_else(
      abs(estimate_no_controls) > 1e-12,
      100 * (estimate_with_controls - estimate_no_controls) / abs(estimate_no_controls),
      NA_real_
    ),
    implied_pct_shift = implied_pct_with_controls - implied_pct_no_controls,
    n_obs = nrow(did_df),
    n_blocks = n_distinct(did_df$block_fe_id),
    n_pairs = n_distinct(did_df$pair_fe_id)
  )
}

write_balance_tex <- function(balance_df, output_path) {
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\small",
    "\\caption{Permit Event-Study Balance in Raw Levels}",
    "\\label{tab:permit_balance_raw_levels}",
    "\\begin{tabular}{lrrrrr}",
    "\\toprule",
    "Variable & Control & Treated & Resid. Diff & SE & $p$-value \\\\"
  )

  lines <- c(lines, "\\midrule")

  for (covariate_i in covariate_catalog$covariate) {
    row_i <- balance_df %>%
      filter(covariate == covariate_i)

    if (nrow(row_i) == 0) {
      next
    }

    lines <- c(
      lines,
      sprintf(
        "%s & %s & %s & %s & %s & %s \\\\",
        covariate_catalog$covariate_label[covariate_catalog$covariate == covariate_i],
        fmt_covariate_value(covariate_i, row_i$control_mean[[1]]),
        fmt_covariate_value(covariate_i, row_i$treated_mean[[1]]),
        fmt_covariate_value(covariate_i, row_i$within_pair_diff[[1]]),
        fmt_covariate_value(covariate_i, row_i$within_pair_diff_se[[1]]),
        fmt_decimal(row_i$within_pair_diff_p_value[[1]], 3)
      )
    )
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: 2015 cohort only. Sample uses blocks within 1,000 feet of a ward boundary in relative year $-1$ for the permit event-study design. Treated blocks are all blocks that switch wards at redistricting; control blocks remain in the origin ward. Residualized difference equals treated minus control after demeaning each covariate within ward pair. Standard errors and $p$-values come from block-level regressions with ward-pair fixed effects and standard errors clustered by ward pair. Shares are reported in percentage points. Median income is in dollars. High-discretion permits at $t=-1$ is the pre-treatment block-year count. Baseline block-group covariates use 2014 ACS values.}",
    "\\end{table}"
  )

  write_lines_safe(lines, output_path)
}

write_joint_tex <- function(joint_df, output_path) {
  row_i <- joint_df[1, ]

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\small",
    "\\caption{Joint Predictability of Permit Treatment}",
    "\\label{tab:permit_balance_joint}",
    "\\begin{tabular}{rrrrrr}",
    "\\toprule",
    "Drop \\% & Raw $R^2$ & Within-Pair $R^2$ & Max $|$Raw SMD$|$ & Max $|$Within SMD$|$ & Blocks \\\\",
    "\\midrule",
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      fmt_decimal(row_i$dropped_share_pct, 2),
      fmt_decimal(row_i$raw_r2, 3),
      fmt_decimal(row_i$within_pair_r2, 3),
      fmt_decimal(row_i$max_abs_raw_smd, 3),
      fmt_decimal(row_i$max_abs_within_pair_smd, 3),
      fmt_integer(row_i$n_blocks_complete_controls)
    )
  )

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\vspace{0.5em}\\parbox{0.8\\linewidth}{\\footnotesize Notes: 2015 cohort only. Raw $R^2$ comes from a linear probability regression of treatment assignment on baseline observables and distance to boundary. Within-pair $R^2$ runs the same regression after demeaning treatment and observables within ward pair. Blocks count refers to the sample with complete baseline controls.}",
    "\\end{table}"
  )

  write_lines_safe(lines, output_path)
}

write_did_tex <- function(did_df, output_path) {
  row_i <- did_df[1, ]

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\small",
    "\\caption{How Much Baseline Controls Move the Permit DID Estimate}",
    "\\label{tab:permit_balance_did_shift}",
    "\\begin{tabular}{rrrrrr}",
    "\\toprule",
    "No Controls & With Controls & Shift as \\% of Baseline & Implied \\% No Ctrls & Implied \\% With Ctrls & Observations \\\\",
    "\\midrule",
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      paste0(fmt_decimal(row_i$estimate_no_controls, 4), " (", fmt_decimal(row_i$se_no_controls, 4), ")"),
      paste0(fmt_decimal(row_i$estimate_with_controls, 4), " (", fmt_decimal(row_i$se_with_controls, 4), ")"),
      fmt_decimal(row_i$beta_shift_pct_of_baseline, 2),
      fmt_decimal(row_i$implied_pct_no_controls, 2),
      fmt_decimal(row_i$implied_pct_with_controls, 2),
      fmt_integer(row_i$n_obs)
    )
  )

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\vspace{0.5em}\\parbox{0.8\\linewidth}{\\footnotesize Notes: 2015 cohort only. Entries compare the main PPML permit DID with and without the baseline demographic controls interacted with year. Standard errors are in parentheses.}",
    "\\end{table}"
  )

  write_lines_safe(lines, output_path)
}

message("Building block-level balance samples...")
balance_samples <- bind_rows(
  build_balance_sample("cohort_2015"),
  build_balance_sample("cohort_2023"),
  build_balance_sample("stacked_implementation")
)

message("Computing balance detail...")
balance_summary_full <- bind_rows(lapply(
  split(balance_samples, balance_samples$sample),
  build_balance_rows
)) %>%
  arrange(sample, comparison, match(covariate, covariate_catalog$covariate))

message("Computing joint treatment-predictability summary...")
joint_summary_full <- bind_rows(lapply(
  split(balance_samples, balance_samples$sample),
  function(sample_df) build_joint_rows(sample_df, balance_summary_full)
)) %>%
  arrange(sample, comparison)

balance_summary <- balance_summary_full %>%
  filter(comparison == "treated_vs_control", sample == "cohort_2015")

joint_summary <- joint_summary_full %>%
  filter(comparison == "treated_vs_control", sample == "cohort_2015")

message("Running DID robustness checks...")
did_shift <- bind_rows(
  fit_did_rows(build_did_sample("cohort_2015")),
  fit_did_rows(build_did_sample("cohort_2023")),
  fit_did_rows(build_did_sample("stacked_implementation"))
) %>%
  arrange(match(sample, c("cohort_2015", "cohort_2023", "stacked_implementation"))) %>%
  filter(sample == "cohort_2015")

message("Saving CSV outputs...")
write_csv(balance_summary, balance_output)
write_csv(joint_summary, joint_output)
write_csv(did_shift, did_output)

message("Saving TeX tables...")
write_balance_tex(balance_summary, balance_tex_output)
write_joint_tex(joint_summary, joint_tex_output)
write_did_tex(did_shift, did_tex_output)

plot_data <- balance_summary %>%
  select(sample_label, covariate_label, raw_smd, within_pair_smd) %>%
  pivot_longer(
    cols = c(raw_smd, within_pair_smd),
    names_to = "balance_type",
    values_to = "smd"
  ) %>%
  mutate(
    balance_type = recode(
      balance_type,
      raw_smd = "Raw",
      within_pair_smd = "Within Ward Pair"
    ),
    covariate_label = factor(covariate_label, levels = rev(covariate_catalog$covariate_label))
  )

message("Saving love plot...")
balance_plot <- ggplot(plot_data, aes(x = smd, y = covariate_label, color = balance_type)) +
  geom_vline(xintercept = 0, color = "gray75", linewidth = 0.3) +
  geom_vline(xintercept = c(-0.1, 0.1), color = "gray85", linewidth = 0.3, linetype = "dashed") +
  geom_point(size = 2) +
  scale_color_manual(values = c("Raw" = "#b24a33", "Within Ward Pair" = "#1f6f78")) +
  labs(
    x = "Standardized Mean Difference",
    y = NULL,
    color = NULL,
    title = "Permit Event-Study Balance on Baseline Observables",
    subtitle = "2015 cohort only, overall treated vs control at year -1"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )
ggsave(plot_output, balance_plot, width = 11, height = 8.5, bg = "white")

message("Done!")
