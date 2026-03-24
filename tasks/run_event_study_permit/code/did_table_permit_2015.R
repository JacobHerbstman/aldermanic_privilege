source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# Rscript did_table_permit_2015.R 1000 uniform block ../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_1000ft_geo_wardpair.tex
# =======================================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 4) {
  bandwidth <- as.numeric(args[1])
  weighting <- args[2]
  cluster_level <- args[3]
  output_tex <- args[4]
} else {
  stop("FATAL: Script requires args: <bandwidth> <weighting> <cluster_level> <output_tex>", call. = FALSE)
}

if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!weighting %in% c("uniform", "triangular")) {
  stop("weighting must be one of: uniform, triangular", call. = FALSE)
}
if (!cluster_level %in% c("block", "ward_pair")) {
  stop("cluster_level must be one of: block, ward_pair", call. = FALSE)
}

data <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    !is.na(ward_pair_id), ward_pair_id != "",
    !is.na(block_id), block_id != "",
    !is.na(strictness_change),
    !is.na(n_high_discretion_issue),
    dist_ft <= bandwidth,
    relative_year >= -5, relative_year <= 5
  ) %>%
  mutate(
    weight = if (weighting == "triangular") pmax(0, 1 - dist_ft / bandwidth) else 1,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

model <- fepois(
  n_high_discretion_issue ~ post_treat | block_id + ward_pair_id^year,
  data = data,
  weights = ~weight,
  cluster = if (cluster_level == "block") ~block_id else ~ward_pair_id
)

estimate <- coef(model)[["post_treat"]]
std_error <- se(model)[["post_treat"]]
p_value <- coeftable(model)["post_treat", grep("^Pr\\(", colnames(coeftable(model)), value = TRUE)[1]]
effect_pct <- 100 * (exp(estimate) - 1)

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

setFixest_dict(c(post_treat = "Post $\\times$ Strictness $\\Delta$"))
table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\small",
  "\\begin{tabular}{lc}",
  "\\toprule",
  " & 2015 \\\\",
  "\\midrule",
  sprintf("Post $\\times$ Strictness $\\Delta$ & %.4f%s \\\\", estimate, stars(p_value)),
  sprintf(" & (%.4f) \\\\", std_error),
  "\\\\",
  "Block FE & $\\checkmark$ \\\\",
  "Border-Pair $\\times$ Year FE & $\\checkmark$ \\\\",
  sprintf("N & %s \\\\", format(nobs(model), big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(table_lines, output_tex)

message(sprintf(
  "Permit DID | beta = %.4f%s | effect = %.2f%% | se = %.4f | p = %.3f | N = %s",
  estimate,
  stars(p_value),
  effect_pct,
  std_error,
  p_value,
  format(nobs(model), big.mark = ",")
))
