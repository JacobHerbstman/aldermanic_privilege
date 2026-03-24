source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_permit_style/code")
# Rscript run_did_sales_permit_style.R uniform
# =======================================================================================

dir.create("../output", showWarnings = FALSE, recursive = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
weighting_spec <- if (length(cli_args) >= 1) cli_args[1] else "sales_count_weighted"
if (!weighting_spec %in% c("sales_count_weighted", "uniform")) {
  stop("FATAL: weighting_spec must be one of: sales_count_weighted, uniform", call. = FALSE)
}

output_suffix <- if (weighting_spec == "uniform") "_uniform" else ""

source("sales_permit_style_helpers.R")

prepare_analysis_data <- function(df, panel_mode) {
  if (panel_mode == "cohort_2015") {
    data <- prepare_block_year_cohort(df, "2015", "2015", 0)
    block_var <- "block_id"
    fe_formula <- "block_id + ward_pair_id^year"
    cluster_formula <- ~block_id
  } else {
    data <- bind_rows(
      prepare_block_year_cohort(df, "2015", "2015", 0),
      prepare_block_year_cohort(df, "2023", "2023", 0)
    )
    block_var <- "cohort_block_id"
    fe_formula <- "cohort_block_id + cohort_ward_pair^year"
    cluster_formula <- ~cohort_block_id
  }

  data <- data %>%
    filter(
      has_sales,
      !is.na(mean_price), mean_price > 0,
      !is.na(ward_pair_id), ward_pair_id != "",
      !is.na(mean_dist_to_boundary), mean_dist_to_boundary <= 1000,
      relative_year >= -5, relative_year <= 5
    ) %>%
    mutate(
      weight = if (weighting_spec == "uniform") 1 else as.numeric(n_sales),
      post_treat = as.integer(relative_year >= 0) * strictness_change,
      outcome_estimation = log(mean_price)
    )

  list(
    data = data,
    block_var = block_var,
    fe_formula = fe_formula,
    cluster_formula = cluster_formula
  )
}

extract_post_treat_row <- function(model, specification, data, block_var) {
  p_value <- coeftable(model)["post_treat", "Pr(>|t|)"]

  tibble(
    specification = specification,
    estimate = unname(coef(model)["post_treat"]),
    std_error = unname(se(model)["post_treat"]),
    p_value = unname(p_value),
    n_obs = unname(model$nobs),
    n_units = n_distinct(data[[block_var]]),
    effective_weight_n = sum(data$weight, na.rm = TRUE)
  ) %>%
    mutate(
      estimate_pct = 100 * estimate,
      ci_low_pct = 100 * (estimate - 1.96 * std_error),
      ci_high_pct = 100 * (estimate + 1.96 * std_error)
    )
}

raw_block_year <- read_csv(
  "../input/sales_block_year_panel.csv",
  col_types = cols(
    block_id = col_character(),
    ward_pre_2015 = col_character(),
    ward_pre_2023 = col_character(),
    ward = col_character(),
    ward_pair_id = col_character(),
    block_group_id = col_character()
  ),
  show_col_types = FALSE
)

data_2015 <- prepare_analysis_data(raw_block_year, "cohort_2015")
data_stacked <- prepare_analysis_data(raw_block_year, "stacked_implementation")

formula_2015 <- sprintf("outcome_estimation ~ post_treat | %s", data_2015$fe_formula)
formula_stacked <- sprintf("outcome_estimation ~ post_treat | %s", data_stacked$fe_formula)

message(sprintf("Running aligned block-year DID: %s", formula_2015))
m_2015 <- feols(
  as.formula(formula_2015),
  data = data_2015$data,
  weights = ~weight,
  cluster = data_2015$cluster_formula
)

message(sprintf("Running aligned block-year DID: %s", formula_stacked))
m_stacked <- feols(
  as.formula(formula_stacked),
  data = data_stacked$data,
  weights = ~weight,
  cluster = data_stacked$cluster_formula
)

results <- bind_rows(
  extract_post_treat_row(m_2015, "2015 implementation", data_2015$data, data_2015$block_var),
  extract_post_treat_row(m_stacked, "stacked implementation", data_stacked$data, data_stacked$block_var)
)

write_csv(results, sprintf("../output/did_sales_permit_style_block_year_implementation%s.csv", output_suffix))

etable(
  list(m_2015, m_stacked),
  title = "Aligned Block-Year Home Price DID",
  fitstat = ~ n + r2,
  style.tex = style.tex("aer"),
  depvar = FALSE,
  digits = 3,
  digits.stats = 2,
  drop = "Intercept",
  drop.section = "fixef",
  order = "post_treat",
  dict = c(post_treat = "Post $\\times$ Strictness $\\Delta$"),
  extralines = list(
    "_Panel" = c("2015", "Stacked Implementation"),
    "_Outcome" = c("Log Mean Block-Year Sale Price", "Log Mean Block-Year Sale Price"),
    "_Weights" = c(if (weighting_spec == "uniform") "Uniform" else "Block-Year Sales Count", if (weighting_spec == "uniform") "Uniform" else "Block-Year Sales Count"),
    "_Bandwidth" = c("1000 ft", "1000 ft"),
    "_FE" = c("Block + Ward Pair $\\times$ Year", "Cohort-Block + Cohort-Ward Pair $\\times$ Year"),
    "_Cluster" = c("Block", "Cohort-Block")
  ),
  notes = if (weighting_spec == "uniform") {
    "Block-year regressions of log mean sale price on a post indicator interacted with change in alderman strictness. Sample restricted to blocks within 1000 feet of ward boundaries with positive sales counts and non-missing ward-pair assignment. Block-year cells are equally weighted."
  } else {
    "Block-year regressions of log mean sale price on a post indicator interacted with change in alderman strictness. Sample restricted to blocks within 1000 feet of ward boundaries with positive sales counts and non-missing ward-pair assignment. Observations are weighted by the number of sales in the block-year cell."
  },
  label = "tab:did_sales_permit_style_block_year",
  float = TRUE,
  file = sprintf("../output/did_sales_permit_style_block_year_implementation%s.tex", output_suffix),
  replace = TRUE
)
