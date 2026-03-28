source("../../setup_environment/code/packages.R")
library(data.table)
library(fixest)
library(arrow)


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental_disaggregate/code")
# rental_panel <- "../input/rental_listing_panel.parquet"
# block_treatment_pre <- "../../create_block_treatment_panel/output/block_treatment_pre_scores.csv"
# alderman_panel <- "../../create_alderman_data/output/chicago_alderman_panel.csv"
# old_score_file <- "../../_deprecated/create_alderman_strictness_scores/output/alderman_restrictiveness_scores_ward_month_FEs.csv"
# new_score_file <- "../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage.csv"
# old_score_column <- "uncertainty_index"
# new_score_column <- "uncertainty_index"
# bandwidth <- 1000
# weighting <- "triangular"
# output_block <- "../output/old_vs_new_score_block_influence.csv"
# output_top <- "../output/old_vs_new_score_block_influence_top50.csv"
# output_summary <- "../output/old_vs_new_score_summary.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(rental_panel, block_treatment_pre, alderman_panel, old_score_file, new_score_file, old_score_column, new_score_column, bandwidth, weighting, output_block, output_top, output_summary)
}

if (length(cli_args) >= 12) {
  rental_panel <- cli_args[1]
  block_treatment_pre <- cli_args[2]
  alderman_panel <- cli_args[3]
  old_score_file <- cli_args[4]
  new_score_file <- cli_args[5]
  old_score_column <- cli_args[6]
  new_score_column <- cli_args[7]
  bandwidth <- suppressWarnings(as.integer(cli_args[8]))
  weighting <- cli_args[9]
  output_block <- cli_args[10]
  output_top <- cli_args[11]
  output_summary <- cli_args[12]
} else {
  if (!exists("rental_panel") || !exists("block_treatment_pre") || !exists("alderman_panel") || !exists("old_score_file") || !exists("new_score_file") || !exists("old_score_column") || !exists("new_score_column") || !exists("bandwidth") || !exists("weighting") || !exists("output_block") || !exists("output_top") || !exists("output_summary")) {
    stop("FATAL: Script requires 12 args: <rental_panel> <block_treatment_pre> <alderman_panel> <old_score_file> <new_score_file> <old_score_column> <new_score_column> <bandwidth> <weighting> <output_block> <output_top> <output_summary>", call. = FALSE)
  }
}

if (!weighting %in% c("triangular", "uniform")) {
  stop("--weighting must be one of: triangular, uniform", call. = FALSE)
}

message("=== Diagnose old vs new rental score leverage ===")
message("Bandwidth: ", bandwidth, " ft")
message("Weighting: ", weighting)
message("Old score file: ", old_score_file)
message("New score file: ", new_score_file)

read_score <- function(path, score_column, out_name) {
  dt <- fread(path)
  if (!score_column %in% names(dt)) {
    stop(sprintf("Column '%s' not found in %s", score_column, path), call. = FALSE)
  }
  out <- unique(dt[, .(alderman, score = as.numeric(get(score_column)))], by = "alderman")
  setnames(out, "score", out_name)
  out
}

scores_old <- read_score(old_score_file, old_score_column, "score_old")
scores_new <- read_score(new_score_file, new_score_column, "score_new")

treat_pre <- fread(block_treatment_pre)
treat_pre[, `:=`(
  block_id = as.character(block_id),
  cohort = as.character(cohort),
  ward_origin = as.integer(ward_origin),
  ward_dest = as.integer(ward_dest),
  score_year = fifelse(cohort == "2015", 2014L,
    fifelse(cohort == "2023", 2022L, NA_integer_)
  )
)]

alderman_panel <- fread(alderman_panel)
alderman_panel[, month_date := as.Date(paste("01", month), format = "%d %b %Y")]
alderman_lookup <- unique(
  alderman_panel[
    !is.na(month_date) & as.integer(format(month_date, "%m")) == 6,
    .(score_year = as.integer(format(month_date, "%Y")), ward = as.integer(ward), alderman)
  ],
  by = c("score_year", "ward")
)

treat_map <- merge(
  treat_pre,
  alderman_lookup,
  by.x = c("score_year", "ward_origin"),
  by.y = c("score_year", "ward"),
  all.x = TRUE,
  sort = FALSE
)
setnames(treat_map, "alderman", "alderman_origin")

treat_map <- merge(
  treat_map,
  alderman_lookup,
  by.x = c("score_year", "ward_dest"),
  by.y = c("score_year", "ward"),
  all.x = TRUE,
  sort = FALSE
)
setnames(treat_map, "alderman", "alderman_dest")

treat_map <- merge(treat_map, scores_old, by.x = "alderman_origin", by.y = "alderman", all.x = TRUE, sort = FALSE)
setnames(treat_map, "score_old", "strictness_origin_old")
treat_map <- merge(treat_map, scores_old, by.x = "alderman_dest", by.y = "alderman", all.x = TRUE, sort = FALSE)
setnames(treat_map, "score_old", "strictness_dest_old")

treat_map <- merge(treat_map, scores_new, by.x = "alderman_origin", by.y = "alderman", all.x = TRUE, sort = FALSE)
setnames(treat_map, "score_new", "strictness_origin_new")
treat_map <- merge(treat_map, scores_new, by.x = "alderman_dest", by.y = "alderman", all.x = TRUE, sort = FALSE)
setnames(treat_map, "score_new", "strictness_dest_new")

treat_map[, `:=`(
  strictness_change_old = strictness_dest_old - strictness_origin_old,
  strictness_change_new = strictness_dest_new - strictness_origin_new
)]

treat_map <- unique(
  treat_map[, .(
    block_id, cohort, ward_origin, ward_dest,
    strictness_change_old, strictness_change_new
  )],
  by = c("block_id", "cohort")
)

message("Loading rental panel...")
keep_cols <- c(
  "block_id", "cohort", "year", "relative_year_capped", "dist_ft", "rent_price",
  "log_sqft", "log_beds", "log_baths", "building_type_factor",
  "cohort_block_id", "cohort_ward_pair", "cohort_ward_pair_side"
)
rental <- as.data.table(read_parquet(rental_panel, col_select = keep_cols))
rental[, `:=`(block_id = as.character(block_id), cohort = as.character(cohort))]

rental <- merge(
  rental,
  treat_map,
  by = c("block_id", "cohort"),
  all.x = TRUE,
  sort = FALSE
)

rental <- rental[
  dist_ft <= bandwidth &
    !is.na(strictness_change_old) &
    !is.na(strictness_change_new) &
    !is.na(log_sqft) &
    !is.na(log_beds) &
    !is.na(log_baths) &
    !is.na(building_type_factor) &
    rent_price > 0
]

if (!is.factor(rental$building_type_factor)) {
  rental[, building_type_factor := as.factor(building_type_factor)]
}

rental[, `:=`(
  post = as.integer(relative_year_capped >= 0),
  weight = if (weighting == "triangular") pmax(0, 1 - dist_ft / bandwidth) else 1
)]
rental[, `:=`(
  post_treat_old = post * strictness_change_old,
  post_treat_new = post * strictness_change_new
)]

message("Usable observations: ", format(nrow(rental), big.mark = ","))
message("Unique blocks: ", format(uniqueN(rental$block_id), big.mark = ","))

controls <- "building_type_factor + log_sqft + log_beds + log_baths"
fe_part <- "cohort_ward_pair_side + cohort_ward_pair^year"

form_old <- as.formula(sprintf("log(rent_price) ~ post_treat_old + %s | %s", controls, fe_part))
form_new <- as.formula(sprintf("log(rent_price) ~ post_treat_new + %s | %s", controls, fe_part))
form_y <- as.formula(sprintf("log(rent_price) ~ %s | %s", controls, fe_part))
form_x_old <- as.formula(sprintf("post_treat_old ~ %s | %s", controls, fe_part))
form_x_new <- as.formula(sprintf("post_treat_new ~ %s | %s", controls, fe_part))

message("Estimating direct models...")
m_old <- feols(form_old, data = rental, weights = ~weight, cluster = ~cohort_block_id, warn = FALSE)
m_new <- feols(form_new, data = rental, weights = ~weight, cluster = ~cohort_block_id, warn = FALSE)

beta_old <- as.numeric(coef(m_old)["post_treat_old"])
beta_new <- as.numeric(coef(m_new)["post_treat_new"])
se_old <- as.numeric(se(m_old)["post_treat_old"])
se_new <- as.numeric(se(m_new)["post_treat_new"])
gap_full <- beta_old - beta_new

message(sprintf("Direct beta old: %.4f (SE %.4f)", beta_old, se_old))
message(sprintf("Direct beta new: %.4f (SE %.4f)", beta_new, se_new))
message(sprintf("Gap old-new: %.4f", gap_full))

message("Residualizing y and x (FWL)...")
m_y <- feols(form_y, data = rental, weights = ~weight, warn = FALSE)
m_x_old <- feols(form_x_old, data = rental, weights = ~weight, warn = FALSE)
m_x_new <- feols(form_x_new, data = rental, weights = ~weight, warn = FALSE)

rental[, `:=`(
  y_tilde = resid(m_y),
  x_old_tilde = resid(m_x_old),
  x_new_tilde = resid(m_x_new)
)]
rental[, `:=`(
  sxy_old = weight * x_old_tilde * y_tilde,
  sxx_old = weight * x_old_tilde * x_old_tilde,
  sxy_new = weight * x_new_tilde * y_tilde,
  sxx_new = weight * x_new_tilde * x_new_tilde
)]

Sxy_old <- rental[, sum(sxy_old)]
Sxx_old <- rental[, sum(sxx_old)]
Sxy_new <- rental[, sum(sxy_new)]
Sxx_new <- rental[, sum(sxx_new)]

beta_old_fwl <- Sxy_old / Sxx_old
beta_new_fwl <- Sxy_new / Sxx_new
gap_fwl <- beta_old_fwl - beta_new_fwl

block_meta <- treat_map[, .(
  cohorts = paste(sort(unique(cohort)), collapse = ";"),
  ward_pairs = paste(sort(unique(sprintf("%d-%d", pmin(ward_origin, ward_dest), pmax(ward_origin, ward_dest)))), collapse = ";"),
  old_change_2015 = strictness_change_old[cohort == "2015"][1],
  old_change_2023 = strictness_change_old[cohort == "2023"][1],
  new_change_2015 = strictness_change_new[cohort == "2015"][1],
  new_change_2023 = strictness_change_new[cohort == "2023"][1]
), by = block_id]

block_stats <- rental[, .(
  n_obs = .N,
  n_pre = sum(post == 0),
  n_post = sum(post == 1),
  n_cohort_pairs = uniqueN(cohort_ward_pair),
  mean_dist_ft = mean(dist_ft),
  mean_abs_post_treat_old = mean(abs(post_treat_old)),
  mean_abs_post_treat_new = mean(abs(post_treat_new)),
  mean_strictness_change_old = mean(strictness_change_old),
  mean_strictness_change_new = mean(strictness_change_new),
  sxy_old = sum(sxy_old),
  sxx_old = sum(sxx_old),
  sxy_new = sum(sxy_new),
  sxx_new = sum(sxx_new)
), by = block_id]

block_stats <- merge(block_stats, block_meta, by = "block_id", all.x = TRUE, sort = FALSE)

block_stats[, `:=`(
  beta_old_wo = fifelse((Sxx_old - sxx_old) > 0, (Sxy_old - sxy_old) / (Sxx_old - sxx_old), NA_real_),
  beta_new_wo = fifelse((Sxx_new - sxx_new) > 0, (Sxy_new - sxy_new) / (Sxx_new - sxx_new), NA_real_)
)]
block_stats[, gap_wo := beta_old_wo - beta_new_wo]
block_stats[, `:=`(
  influence_old = beta_old_fwl - beta_old_wo,
  influence_new = beta_new_fwl - beta_new_wo,
  influence_gap = gap_fwl - gap_wo
)]
block_stats[, abs_influence_gap := abs(influence_gap)]
setorder(block_stats, -abs_influence_gap, -n_obs)

block_stats[, rank_abs_influence_gap := .I]
block_stats[, rank_push_old_more_negative := frank(influence_old, ties.method = "first")]
block_stats[, rank_push_new_more_negative := frank(influence_new, ties.method = "first")]

top_dt <- block_stats[1:min(50L, .N)]

summary_dt <- data.table(
  metric = c(
    "n_obs", "n_blocks",
    "beta_old_direct", "se_old_direct",
    "beta_new_direct", "se_new_direct",
    "gap_direct_old_minus_new",
    "beta_old_fwl", "beta_new_fwl", "gap_fwl",
    "corr_block_mean_old_new_change"
  ),
  value = c(
    nrow(rental), uniqueN(rental$block_id),
    beta_old, se_old, beta_new, se_new, gap_full,
    beta_old_fwl, beta_new_fwl, gap_fwl,
    suppressWarnings(cor(block_stats$mean_strictness_change_old, block_stats$mean_strictness_change_new, use = "complete.obs"))
  )
)

fwrite(block_stats, output_block)
fwrite(top_dt, output_top)
fwrite(summary_dt, output_summary)

message("Saved full block influence: ", output_block)
message("Saved top-50 block influence: ", output_top)
message("Saved summary: ", output_summary)

message("\nTop 10 blocks by absolute influence on old-vs-new beta gap:")
print(top_dt[1:min(10L, .N), .(
  block_id, n_obs, cohorts, ward_pairs,
  old_change_2015, old_change_2023,
  new_change_2015, new_change_2023,
  influence_gap, influence_old, influence_new
)])
