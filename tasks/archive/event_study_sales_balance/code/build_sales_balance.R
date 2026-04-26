source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/event_study_sales_balance/code")
# panel_input <- "../input/sales_transaction_panel_2015.parquet"
# controls_input <- "../input/block_group_controls.csv"
# balance_output <- "../output/sales_balance_summary.csv"
# balance_tex_output <- "../output/sales_balance_summary.tex"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(panel_input, controls_input, balance_output, balance_tex_output)
}

if (length(cli_args) != 4) {
  stop(
    "FATAL: Script requires 4 args: <panel_input> <controls_input> <balance_output> <balance_tex_output>",
    call. = FALSE
  )
}

panel_input <- cli_args[1]
controls_input <- cli_args[2]
balance_output <- cli_args[3]
balance_tex_output <- cli_args[4]

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
  if (covariate_name %in% c("baseline_median_income", "pre_sale_price")) {
    return(fmt_integer(x))
  }
  if (covariate_name %in% c("baseline_population_density", "dist_ft")) {
    return(fmt_integer(x))
  }
  if (covariate_name == "baseline_median_age") {
    return(fmt_decimal(x, 1))
  }
  if (covariate_name == "pre_sales_count") {
    return(fmt_decimal(x, 2))
  }
  fmt_decimal(x, 2)
}

message("Loading sales transaction panel...")
sales_panel <- read_parquet(panel_input) %>%
  as_tibble()

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

covariate_catalog <- tibble(
  covariate = c(
    "baseline_median_income",
    "baseline_homeownership_rate",
    "baseline_share_bach_plus",
    "baseline_median_age",
    "baseline_population_density",
    "baseline_percent_black",
    "baseline_percent_hispanic",
    "pre_sale_price",
    "pre_sales_count"
  ),
  covariate_label = c(
    "Median Income",
    "Homeownership Rate",
    "Bachelor's Share",
    "Median Age",
    "Population Density",
    "Percent Black",
    "Percent Hispanic",
    "Sale Price at t = -1",
    "Sales at t = -1"
  )
)

control_vars <- covariate_catalog$covariate[grepl("^baseline_", covariate_catalog$covariate)]

message("Building block-level pre-period sample...")
balance_sample <- sales_panel %>%
  filter(dist_ft <= 1000, relative_year == -1, !is.na(strictness_change)) %>%
  mutate(
    group = case_when(
      treat == 0 ~ "control",
      strictness_change > 0 ~ "to_stricter",
      strictness_change < 0 ~ "to_lenient",
      TRUE ~ "treated_zero_change"
    ),
    block_group_id = substr(as.character(block_id), 1, 12),
    baseline_year = 2014L
  ) %>%
  filter(group != "treated_zero_change") %>%
  group_by(block_id, ward_pair_id, block_group_id, baseline_year, group) %>%
  summarise(
    dist_ft = mean(dist_ft, na.rm = TRUE),
    pre_sale_price = mean(sale_price, na.rm = TRUE),
    pre_sales_count = n(),
    .groups = "drop"
  ) %>%
  left_join(baseline_controls, by = c("block_group_id", "baseline_year")) %>%
  mutate(
    treated_any = as.integer(group != "control"),
    complete_controls = if_all(all_of(control_vars), ~ !is.na(.x))
  ) %>%
  filter(complete_controls)

message("Computing treated-control balance rows...")
balance_rows <- lapply(covariate_catalog$covariate, function(covariate_i) {
  row_df <- balance_sample %>%
    filter(!is.na(.data[[covariate_i]])) %>%
    mutate(outcome = .data[[covariate_i]])

  if (nrow(row_df) == 0) {
    return(NULL)
  }

  model_i <- feols(outcome ~ treated_any | ward_pair_id, data = row_df, cluster = ~ward_pair_id)
  coef_i <- coeftable(model_i)["treated_any", ]

  tibble(
    covariate = covariate_i,
    control_mean = mean(row_df$outcome[row_df$treated_any == 0], na.rm = TRUE),
    treated_mean = mean(row_df$outcome[row_df$treated_any == 1], na.rm = TRUE),
    within_pair_diff = unname(coef_i["Estimate"]),
    within_pair_diff_se = unname(coef_i["Std. Error"]),
    within_pair_diff_p_value = unname(coef_i["Pr(>|t|)"]),
    n_blocks = nrow(row_df),
    n_pairs = n_distinct(row_df$ward_pair_id)
  )
})

balance_summary <- bind_rows(balance_rows) %>%
  left_join(covariate_catalog, by = "covariate") %>%
  select(
    covariate,
    covariate_label,
    control_mean,
    treated_mean,
    within_pair_diff,
    within_pair_diff_se,
    within_pair_diff_p_value,
    n_blocks,
    n_pairs
  )

write_csv(balance_summary, balance_output)

message("Writing LaTeX table...")
lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\small",
  "\\caption{Home-Sales Event-Study Balance in Raw Levels}",
  "\\label{tab:sales_balance_raw_levels}",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Variable & Control & Treated & Resid. Diff & SE & $p$-value \\\\",
  "\\midrule"
)

for (covariate_i in covariate_catalog$covariate) {
  row_i <- balance_summary %>%
    filter(covariate == covariate_i)

  if (nrow(row_i) == 0) {
    next
  }

  lines <- c(
    lines,
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      row_i$covariate_label[[1]],
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
  "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: 2015 cohort only. Sample uses census blocks within 1,000 feet of a ward boundary with at least one observed sale in relative year $-1$ for the home-sales event-study design. Treated blocks are all blocks that switch wards at redistricting; control blocks remain in the origin ward. Residualized difference equals treated minus control after demeaning each covariate within ward pair. Standard errors and $p$-values come from block-level regressions with ward-pair fixed effects and standard errors clustered by ward pair. Shares are reported in percentage points. Median income and sale price are in dollars. Sale price at $t=-1$ is the mean transaction price within the block in the year before redistricting; sales at $t=-1$ is the number of transactions on the block in that year. Baseline block-group covariates use 2014 ACS values.}",
  "\\end{table}"
)

writeLines(lines, balance_tex_output)
