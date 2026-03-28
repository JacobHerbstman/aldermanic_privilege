source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_disaggregate/code")
# bandwidth <- 1000
# weighting <- "uniform"
# output_tex <- "../output/did_table_sales_2015_uniform_1000ft_geo_wardpair_clust_block.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth, weighting, output_tex)
}

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
    if_all(all_of(hedonic_vars), ~ is.finite(.x))
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
dep_var_mean_no_ctrl <- mean(data$sale_price, na.rm = TRUE)
dep_var_mean_ctrl <- dep_var_mean_no_ctrl
ward_pairs_no_ctrl <- n_distinct(data$ward_pair)
ward_pairs_ctrl <- ward_pairs_no_ctrl

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
  fitstat = NULL,
  style.tex = style.tex("aer"),
  depvar = FALSE,
  digits = 3,
  digits.stats = 2,
  drop = "Intercept",
  order = c("Post", "Log"),
  drop.section = "fixef",
  extralines = list(
    "_N" = c(format(nobs(m_no_ctrl), big.mark = ","), format(nobs(m_ctrl), big.mark = ",")),
    "_Dep. Var. Mean" = c(
      paste0("\\$", format(round(dep_var_mean_no_ctrl, 0), big.mark = ",")),
      paste0("\\$", format(round(dep_var_mean_ctrl, 0), big.mark = ","))
    ),
    "_Ward Pairs" = c(format(ward_pairs_no_ctrl, big.mark = ","), format(ward_pairs_ctrl, big.mark = ",")),
    "_Border-Pair Side FE" = c("$\\checkmark$", "$\\checkmark$"),
    "_Border-Pair $\\times$ Year FE" = c("$\\checkmark$", "$\\checkmark$")
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  float = FALSE,
  file = output_tex,
  replace = TRUE
)

table_tex <- readLines(output_tex)
drop_patterns <- c(
  "^\\s*Observations\\s*&",
  "^\\s*R\\$\\^2\\$\\s*&",
  "^\\s*Within R\\$\\^2\\$\\s*&"
)
table_tex <- table_tex[!vapply(
  table_tex,
  function(line) any(vapply(drop_patterns, grepl, logical(1), x = line)),
  logical(1)
)]
centering_idx <- match("   \\centering", table_tex)
if (!is.na(centering_idx)) {
  table_tex <- append(
    table_tex,
    c(
      "   \\small",
      "   \\setlength{\\tabcolsep}{4pt}",
      "   \\renewcommand{\\arraystretch}{0.97}"
    ),
    after = centering_idx
  )
}
notes_idx <- match("   \\par \\raggedright ", table_tex)
if (!is.na(notes_idx)) {
  table_tex[notes_idx] <- "   \\par \\raggedright \\footnotesize "
}
writeLines(table_tex, output_tex)

message(sprintf(
  "Sales DID | no controls = %.4f (%.2f%%) | with controls = %.4f (%.2f%%) | N = %s",
  coef(m_no_ctrl)[["post_treat"]],
  effect_no_ctrl,
  coef(m_ctrl)[["post_treat"]],
  effect_ctrl,
  format(nobs(m_ctrl), big.mark = ",")
))
