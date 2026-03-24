source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_disaggregate/code")
# Rscript did_table_sales_2015.R 1000 uniform ../output/did_table_sales_2015_uniform_1000ft_geo_wardpair_clust_block.tex
# =======================================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 3) {
  bandwidth <- as.numeric(args[1])
  weighting <- args[2]
  output_tex <- args[3]
} else {
  stop("FATAL: Script requires args: <bandwidth> <weighting> <output_tex>", call. = FALSE)
}

if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!weighting %in% c("uniform", "triangular")) {
  stop("weighting must be one of: uniform, triangular", call. = FALSE)
}

hedonic_vars <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")

data <- read_parquet("../input/sales_transaction_panel_2015.parquet") %>%
  mutate(ward_pair = sub("_[0-9]+$", "", ward_pair_side)) %>%
  filter(
    dist_ft <= bandwidth,
    relative_year >= -5, relative_year <= 5,
    !is.na(ward_pair), ward_pair != "",
    !is.na(ward_pair_side), ward_pair_side != "",
    !is.na(block_id), block_id != "",
    sale_price > 0,
    if_all(all_of(hedonic_vars), ~ !is.na(.x))
  ) %>%
  mutate(
    weight = if (weighting == "triangular") pmax(0, 1 - dist_ft / bandwidth) else 1,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

fe_formula <- "ward_pair_side + ward_pair^sale_year"
hedonic_formula <- paste(hedonic_vars, collapse = " + ")

m_no_ctrl <- feols(
  as.formula(sprintf("log(sale_price) ~ post_treat | %s", fe_formula)),
  data = data,
  weights = ~weight,
  cluster = ~block_id
)

m_ctrl <- feols(
  as.formula(sprintf("log(sale_price) ~ post_treat + %s | %s", hedonic_formula, fe_formula)),
  data = data,
  weights = ~weight,
  cluster = ~block_id
)

effect_no_ctrl <- 100 * (exp(coef(m_no_ctrl)[["post_treat"]]) - 1)
effect_ctrl <- 100 * (exp(coef(m_ctrl)[["post_treat"]]) - 1)

setFixest_dict(c(
  post_treat = "Post $\\times$ Strictness $\\Delta$",
  log_sqft = "Log Building Sqft",
  log_land_sqft = "Log Land Sqft",
  log_building_age = "Log Building Age",
  log_bedrooms = "Log Bedrooms",
  log_baths = "Log Bathrooms",
  has_garage = "Has Garage"
))

etable(
  list(m_no_ctrl, m_ctrl),
  title = "Effect of Alderman Strictness on Home Sale Prices",
  fitstat = ~n + r2,
  style.tex = style.tex("aer"),
  depvar = FALSE,
  digits = 3,
  digits.stats = 2,
  drop = "Intercept",
  order = c("Post", "Log"),
  drop.section = "fixef",
  extralines = list(
    "_Sample" = c("2015 Implementation Cohort", "2015 Implementation Cohort"),
    "_Hedonic Controls" = c("", "$\\checkmark$"),
    "_Bandwidth" = c(sprintf("%d ft", as.integer(bandwidth)), sprintf("%d ft", as.integer(bandwidth))),
    "_Weights" = c(tools::toTitleCase(weighting), tools::toTitleCase(weighting)),
    "_Fixed Effects" = c("Border-Pair Side + Border-Pair $\\times$ Year", "Border-Pair Side + Border-Pair $\\times$ Year"),
    "_Cluster Level" = c("Block", "Block"),
    "_Implied Effect" = c(sprintf("%.2f\\%%", effect_no_ctrl), sprintf("%.2f\\%%", effect_ctrl))
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  notes = sprintf(
    "Transaction-level regressions of log sale price on a post-period indicator interacted with change in alderman strictness, restricted to the 2015 implementation cohort and to transactions within %d feet of the ward border. Both columns use the same complete hedonic sample so the controls comparison is not driven by sample changes. %s weighting. Standard errors clustered by block.",
    as.integer(bandwidth),
    tools::toTitleCase(weighting)
  ),
  label = "tab:did_sales_2015_current",
  float = TRUE,
  file = output_tex,
  replace = TRUE
)

message(sprintf(
  "Sales DID | no controls = %.4f (%.2f%%) | with controls = %.4f (%.2f%%) | N = %s",
  coef(m_no_ctrl)[["post_treat"]],
  effect_no_ctrl,
  coef(m_ctrl)[["post_treat"]],
  effect_ctrl,
  format(nobs(m_ctrl), big.mark = ",")
))
