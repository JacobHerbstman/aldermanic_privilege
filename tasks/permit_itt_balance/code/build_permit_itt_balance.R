# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/permit_itt_balance/code")
# bandwidth_m <- 152.4
# bandwidth_label <- "500ft"
# baseline_year <- 2014

source("../../setup_environment/code/packages.R")
library(fixest)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_m, bandwidth_label, baseline_year)
}
if (length(cli_args) != 3) {
  stop("Script requires bandwidth, bandwidth label, and baseline year.", call. = FALSE)
}

bandwidth_m <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]
baseline_year <- as.integer(cli_args[3])

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}
if (!is.finite(baseline_year)) {
  stop("baseline year must be an integer.", call. = FALSE)
}

panel <- read_parquet("../input/permit_block_year_panel_2015.parquet")

if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("Permit panel must be unique by block and year.", call. = FALSE)
}

balance_sample <- panel %>%
  filter(
    cohort == "2015",
    relative_year == -1,
    dist_m <= bandwidth_m,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) %>%
  transmute(
    block_id = as.character(block_id),
    block_group_id = substr(as.character(block_id), 1, 12),
    ward_pair_id = as.character(ward_pair_id),
    treated = as.integer(switched)
  )

if (anyDuplicated(balance_sample$block_id) > 0) {
  stop("Balance sample must be unique by block.", call. = FALSE)
}
if (!all(balance_sample$treated %in% c(0L, 1L))) {
  stop("Treatment must indicate whether a block switched wards.", call. = FALSE)
}

identifying_pairs <- balance_sample %>%
  group_by(ward_pair_id) %>%
  summarise(treatment_groups = n_distinct(treated), .groups = "drop") %>%
  filter(treatment_groups == 2L) %>%
  select(ward_pair_id)

if (anyDuplicated(identifying_pairs$ward_pair_id) > 0) {
  stop("Identifying ward pairs must be unique.", call. = FALSE)
}

balance_sample <- balance_sample %>%
  semi_join(identifying_pairs, by = "ward_pair_id")

pre_period <- panel %>%
  transmute(
    block_id = as.character(block_id),
    relative_year,
    n_high_discretion_application,
    n_low_discretion_nosigns_application,
    n_new_construction_application
  ) %>%
  semi_join(balance_sample %>% select(block_id), by = "block_id") %>%
  filter(relative_year >= -5, relative_year <= -1)

pre_period_counts <- pre_period %>%
  count(block_id, name = "pre_period_years")

if (any(pre_period_counts$pre_period_years != 5L) || nrow(pre_period_counts) != nrow(balance_sample)) {
  stop("Every balance-sample block must have five pre-period years.", call. = FALSE)
}
if (anyNA(pre_period[c(
  "n_high_discretion_application",
  "n_low_discretion_nosigns_application",
  "n_new_construction_application"
)])) {
  stop("Pre-period permit outcomes must not be missing.", call. = FALSE)
}

pre_period <- pre_period %>%
  group_by(block_id) %>%
  summarise(
    pre_high_discretion_applications = sum(n_high_discretion_application),
    no_pre_high_discretion_applications = as.integer(pre_high_discretion_applications == 0),
    pre_low_discretion_applications = sum(n_low_discretion_nosigns_application),
    no_pre_low_discretion_applications = as.integer(pre_low_discretion_applications == 0),
    pre_new_construction_applications = sum(n_new_construction_application),
    no_pre_new_construction_applications = as.integer(pre_new_construction_applications == 0),
    .groups = "drop"
  )

if (anyDuplicated(pre_period$block_id) > 0) {
  stop("Pre-period permit measures must be unique by block.", call. = FALSE)
}

block_group_controls <- read_csv(
  "../input/block_group_controls.csv",
  col_types = cols(GEOID = col_character()),
  show_col_types = FALSE
) %>%
  filter(year == baseline_year) %>%
  transmute(
    block_group_id = GEOID,
    black_share = percent_black,
    hispanic_share = percent_hispanic,
    homeownership_rate,
    median_gross_rent = median_rent,
    median_home_value,
    median_household_income = median_income,
    bachelors_share = share_bach_plus,
    average_household_size = avg_household_size,
    median_age,
    population_density
  )

if (nrow(block_group_controls) == 0) {
  stop("No block-group controls found for the requested baseline year.", call. = FALSE)
}
if (anyDuplicated(block_group_controls$block_group_id) > 0) {
  stop("Block-group controls must be unique by block group and year.", call. = FALSE)
}

balance_sample <- balance_sample %>%
  left_join(pre_period, by = "block_id", relationship = "one-to-one") %>%
  left_join(block_group_controls, by = "block_group_id", relationship = "many-to-one")

covariates <- tribble(
  ~section, ~variable, ~label, ~format,
  "Permit history", "pre_high_discretion_applications", "High-discretion applications, 2010--2014", "decimal",
  "Permit history", "no_pre_high_discretion_applications", "No high-discretion application, 2010--2014", "decimal",
  "Permit history", "pre_low_discretion_applications", "Low-discretion applications, 2010--2014", "decimal",
  "Permit history", "no_pre_low_discretion_applications", "No low-discretion application, 2010--2014", "decimal",
  "Permit history", "pre_new_construction_applications", "New-construction applications, 2010--2014", "decimal",
  "Permit history", "no_pre_new_construction_applications", "No new-construction application, 2010--2014", "decimal",
  "Neighborhood characteristics", "median_household_income", "Median household income", "integer",
  "Neighborhood characteristics", "homeownership_rate", "Homeownership rate", "decimal",
  "Neighborhood characteristics", "bachelors_share", "Bachelor's degree or higher share", "decimal",
  "Neighborhood characteristics", "black_share", "Black population share", "decimal",
  "Neighborhood characteristics", "hispanic_share", "Hispanic population share", "decimal",
  "Neighborhood characteristics", "median_gross_rent", "Median gross rent", "integer",
  "Neighborhood characteristics", "median_home_value", "Median home value", "integer",
  "Neighborhood characteristics", "average_household_size", "Average household size", "decimal",
  "Neighborhood characteristics", "median_age", "Median age", "one_decimal",
  "Neighborhood characteristics", "population_density", "Population per square kilometer", "integer"
)

balance_results <- vector("list", nrow(covariates))

for (i in seq_len(nrow(covariates))) {
  variable_i <- covariates$variable[i]
  regression_data <- balance_sample %>%
    transmute(
      ward_pair_id,
      treated,
      outcome = .data[[variable_i]]
    ) %>%
    filter(is.finite(outcome))

  model_i <- feols(
    outcome ~ treated | ward_pair_id,
    data = regression_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  balance_results[[i]] <- tibble(
    section = covariates$section[i],
    variable = variable_i,
    label = covariates$label[i],
    format = covariates$format[i],
    control_mean = mean(regression_data$outcome[regression_data$treated == 0]),
    treated_mean = mean(regression_data$outcome[regression_data$treated == 1]),
    difference = coef(model_i)[["treated"]],
    standard_error = se(model_i)[["treated"]],
    p_value = pvalue(model_i)[["treated"]],
    observations = nrow(regression_data)
  )
}

balance_results <- bind_rows(balance_results)

joint_data <- balance_sample %>%
  select(ward_pair_id, treated, all_of(covariates$variable)) %>%
  drop_na()

joint_model <- feols(
  as.formula(paste(
    "treated ~",
    paste(sprintf("scale(%s)", covariates$variable), collapse = " + "),
    "| ward_pair_id"
  )),
  data = joint_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)
joint_test <- wald(joint_model, print = FALSE)

format_value <- function(value, format) {
  if (!is.finite(value)) {
    return("")
  }
  if (format == "integer") {
    return(format(round(value), big.mark = ",", scientific = FALSE, trim = TRUE))
  }
  if (format == "one_decimal") {
    return(sprintf("%.1f", value))
  }
  sprintf("%.3f", value)
}

n_treated <- sum(balance_sample$treated == 1L)
n_control <- sum(balance_sample$treated == 0L)
n_pairs <- n_distinct(balance_sample$ward_pair_id)

lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Pre-Redistricting Balance Between Reassigned and Unchanged Blocks}",
  "\\label{tab:permit_itt_balance}",
  "\\footnotesize",
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrrrr}",
  "\\toprule",
  "Variable & Control & Treated & Difference & SE & $p$-value & $N$ \\\\",
  "\\midrule"
)

previous_section <- ""
for (i in seq_len(nrow(balance_results))) {
  row_i <- balance_results[i, ]
  if (row_i$section != previous_section) {
    if (previous_section != "") {
      lines <- c(lines, "\\addlinespace")
    }
    lines <- c(lines, sprintf("\\multicolumn{7}{l}{\\textit{%s}} \\\\", row_i$section))
    previous_section <- row_i$section
  }
  lines <- c(
    lines,
    sprintf(
      "%s & %s & %s & %s & %s & %.3f & %s \\\\",
      row_i$label,
      format_value(row_i$control_mean, row_i$format),
      format_value(row_i$treated_mean, row_i$format),
      format_value(row_i$difference, row_i$format),
      format_value(row_i$standard_error, row_i$format),
      row_i$p_value,
      format(row_i$observations, big.mark = ",", scientific = FALSE, trim = TRUE)
    )
  )
}

lines <- c(
  lines,
  "\\bottomrule",
  "\\end{tabular*}",
  "\\vspace{0.5em}",
  sprintf(
    paste0(
      "\\begin{minipage}{0.98\\textwidth}\\footnotesize ",
      "\\textit{Notes:} Treated blocks were reassigned to a different ward by the 2015 ward map; control blocks remained in their original ward. ",
      "The sample contains %s treated and %s control blocks within %s of a ward boundary in %s ward pairs containing both groups and is constructed before estimation. ",
      "The event-study samples are smaller because PPML drops blocks and ward-pair-year cells with no permits for the outcome being studied. ",
      "Permit measures use application dates. Neighborhood characteristics are 2014 ACS five-year block-group estimates. ",
      "Difference is the coefficient on the treated indicator from a regression with ward-pair fixed effects; standard errors are clustered by ward pair. ",
      "Means are unadjusted block-level means. The joint-test p-value for all listed covariates is %.3f ",
      "(complete-case $N=%s$).\\end{minipage}"
    ),
    format(n_treated, big.mark = ",", scientific = FALSE, trim = TRUE),
    format(n_control, big.mark = ",", scientific = FALSE, trim = TRUE),
    bandwidth_label,
    format(n_pairs, big.mark = ",", scientific = FALSE, trim = TRUE),
    joint_test$p,
    format(nrow(joint_data), big.mark = ",", scientific = FALSE, trim = TRUE)
  ),
  "\\end{table}"
)

writeLines(
  lines,
  sprintf("../output/permit_itt_balance_%s.tex", bandwidth_label)
)
