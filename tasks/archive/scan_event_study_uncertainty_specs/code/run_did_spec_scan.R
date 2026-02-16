source("../../setup_environment/code/packages.R")
library(optparse)
library(fixest)
library(data.table)
library(arrow)

option_list <- list(
  make_option("--scores_dir", type = "character", default = "../input/uncertainty_scores_output"),
  make_option("--block_treatment_pre", type = "character", default = "../input/block_treatment_pre_scores.csv"),
  make_option("--alderman_panel", type = "character", default = "../input/chicago_alderman_panel.csv"),
  make_option("--sales_2012", type = "character", default = "../input/sales_transaction_panel_2012.parquet"),
  make_option("--sales_2015", type = "character", default = "../input/sales_transaction_panel_2015.parquet"),
  make_option("--rental_panel", type = "character", default = "../input/rental_listing_panel.parquet"),
  make_option("--bandwidth", type = "integer", default = 1000),
  make_option("--output_long", type = "character", default = "../output/did_spec_scan_long.csv"),
  make_option("--output_summary", type = "character", default = "../output/did_spec_scan_summary.csv")
)
opt <- parse_args(OptionParser(option_list = option_list))

message("=== Scan uncertainty specs on sales+rental DiD ===")
message("Bandwidth: ", opt$bandwidth, " ft")

score_files <- list.files(
  opt$scores_dir,
  pattern = "^alderman_uncertainty_index_.*\\.csv$",
  full.names = TRUE
)
if (length(score_files) == 0) {
  stop("No score files found in ", opt$scores_dir, call. = FALSE)
}
score_files <- sort(score_files)
message("Score specs found: ", length(score_files))

spec_from_file <- function(path) {
  gsub("\\.csv$", "", sub("^alderman_uncertainty_index_", "", basename(path)))
}

# -----------------------------------------------------------------------------
# Load fixed inputs once
# -----------------------------------------------------------------------------

# Block treatment pre-scores and alderman lookup (same logic as merge_event_study_scores)
treat_pre <- fread(opt$block_treatment_pre)
treat_pre[, cohort := as.character(cohort)]
treat_pre[, block_id := as.character(block_id)]
treat_pre[, `:=`(
  score_year = fifelse(cohort == "2015", 2014L,
    fifelse(cohort == "2023", 2022L, NA_integer_)
  ),
  ward_origin = as.integer(ward_origin),
  ward_dest = as.integer(ward_dest)
)]

alderman_panel <- fread(opt$alderman_panel)
alderman_panel[, month_date := as.Date(paste("01", month), format = "%d %b %Y")]
alderman_lookup <- alderman_panel[
  !is.na(month_date) & as.integer(format(month_date, "%m")) == 6,
  .(score_year = as.integer(format(month_date, "%Y")), ward = as.integer(ward), alderman)
]

# Base treatment map keyed to alderman names only (scores merged inside loop)
treat_base <- merge(
  treat_pre,
  alderman_lookup,
  by.x = c("score_year", "ward_origin"),
  by.y = c("score_year", "ward"),
  all.x = TRUE,
  sort = FALSE
)
setnames(treat_base, "alderman", "alderman_origin")

treat_base <- merge(
  treat_base,
  alderman_lookup,
  by.x = c("score_year", "ward_dest"),
  by.y = c("score_year", "ward"),
  all.x = TRUE,
  sort = FALSE
)
setnames(treat_base, "alderman", "alderman_dest")

# Sales base panels
sales_2012 <- as.data.table(read_parquet(opt$sales_2012))
sales_2015 <- as.data.table(read_parquet(opt$sales_2015))

sales_2012[, block_id := as.character(block_id)]
sales_2015[, block_id := as.character(block_id)]
if (!"ward_pair" %in% names(sales_2012)) {
  sales_2012[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]
}
if (!"ward_pair" %in% names(sales_2015)) {
  sales_2015[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]
}
sales_2012[, post := as.integer(relative_year >= 0)]
sales_2015[, post := as.integer(relative_year >= 0)]

sales_2012 <- sales_2012[dist_ft <= opt$bandwidth]
sales_2015 <- sales_2015[dist_ft <= opt$bandwidth]

# Hedonic complete-case restriction (matching run_did_sales_disaggregate.R)
sales_hedonics <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")
sales_2012[, cc_hedonic := complete.cases(.SD), .SDcols = sales_hedonics]
sales_2015[, cc_hedonic := complete.cases(.SD), .SDcols = sales_hedonics]

# Triangular weights
sales_2012[, weight := pmax(0, 1 - dist_ft / opt$bandwidth)]
sales_2015[, weight := pmax(0, 1 - dist_ft / opt$bandwidth)]

# Rental base panel
rental <- as.data.table(read_parquet(opt$rental_panel))
rental[, `:=`(
  block_id = as.character(block_id),
  cohort = as.character(cohort)
)]
if (!"cohort_block_id" %in% names(rental)) {
  rental[, cohort_block_id := paste(cohort, block_id, sep = "_")]
}
if (!"cohort_ward_pair" %in% names(rental)) {
  rental[, ward_pair_side := sub("^[0-9]+_", "", cohort_ward_pair_side)]
  rental[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]
  rental[, cohort_ward_pair := paste(cohort, ward_pair, sep = "_")]
}

rental <- rental[dist_ft <= opt$bandwidth]
rental[, post := as.integer(relative_year_capped >= 0)]
if (!is.factor(rental$building_type_factor)) {
  rental[, building_type_factor := as.factor(building_type_factor)]
}
rental[, cc_hedonic := !is.na(log_sqft) & !is.na(log_beds) & !is.na(log_baths) & !is.na(building_type_factor)]
rental[, weight := pmax(0, 1 - dist_ft / opt$bandwidth)]

# Merge keys for fast strictness assignment
sales_2012[, key := block_id]
sales_2015[, key := block_id]
rental[, key := paste(block_id, cohort, sep = "||")]

# -----------------------------------------------------------------------------
# Model helpers
# -----------------------------------------------------------------------------

extract_post_treat <- function(model, model_name, spec_name) {
  out <- data.table(
    spec = spec_name,
    model = model_name,
    estimate = NA_real_,
    se = NA_real_,
    p_value = NA_real_,
    n_obs = NA_integer_
  )
  if (is.null(model)) return(out)
  out$n_obs <- model$nobs
  cf <- coef(model)
  if (!"post_treat" %in% names(cf)) return(out)

  ct <- coeftable(model)
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  out$estimate <- unname(ct["post_treat", "Estimate"])
  out$se <- unname(ct["post_treat", "Std. Error"])
  out$p_value <- unname(ct["post_treat", p_col])
  out
}

safe_feols <- function(formula, data, subset, weights, cluster) {
  tryCatch(
    feols(
      formula,
      data = data,
      subset = subset,
      weights = weights,
      cluster = cluster,
      warn = FALSE
    ),
    error = function(e) {
      message("  Model failed: ", e$message)
      NULL
    }
  )
}

# -----------------------------------------------------------------------------
# Loop specs
# -----------------------------------------------------------------------------

results <- vector("list", length(score_files) * 6)
idx <- 1L

for (score_file in score_files) {
  spec <- spec_from_file(score_file)
  message("\n--- Spec: ", spec, " ---")

  scores <- fread(score_file)
  if (!"uncertainty_index" %in% names(scores)) {
    stop("uncertainty_index missing in ", score_file, call. = FALSE)
  }
  scores <- unique(scores[, .(alderman, score = as.numeric(uncertainty_index))])

  treat_spec <- merge(treat_base, scores,
    by.x = "alderman_origin", by.y = "alderman",
    all.x = TRUE, sort = FALSE
  )
  setnames(treat_spec, "score", "strictness_origin")

  treat_spec <- merge(treat_spec, scores,
    by.x = "alderman_dest", by.y = "alderman",
    all.x = TRUE, sort = FALSE
  )
  setnames(treat_spec, "score", "strictness_dest")

  treat_spec[, strictness_change := strictness_dest - strictness_origin]

  # Maps used in assignment
  map_sales <- treat_spec[
    cohort == "2015",
    .(key = block_id, strictness_change)
  ]
  map_sales <- unique(map_sales, by = "key")
  sales_lookup <- setNames(map_sales$strictness_change, map_sales$key)

  map_rent <- treat_spec[, .(key = paste(block_id, cohort, sep = "||"), strictness_change)]
  map_rent <- unique(map_rent, by = "key")
  rental_lookup <- setNames(map_rent$strictness_change, map_rent$key)

  # Assign strictness change and post_treat
  sales_2012[, strictness_change := as.numeric(sales_lookup[key])]
  sales_2015[, strictness_change := as.numeric(sales_lookup[key])]
  rental[, strictness_change := as.numeric(rental_lookup[key])]

  sales_2012[, `:=`(
    post_treat = post * strictness_change,
    analysis_keep = cc_hedonic & !is.na(strictness_change)
  )]
  sales_2015[, `:=`(
    post_treat = post * strictness_change,
    analysis_keep = cc_hedonic & !is.na(strictness_change)
  )]
  rental[, `:=`(
    post_treat = post * strictness_change,
    analysis_keep = cc_hedonic & !is.na(strictness_change)
  )]

  message(sprintf("  Sales 2012 usable obs: %s", format(sum(sales_2012$analysis_keep), big.mark = ",")))
  message(sprintf("  Sales 2015 usable obs: %s", format(sum(sales_2015$analysis_keep), big.mark = ",")))
  message(sprintf("  Rental usable obs: %s", format(sum(rental$analysis_keep), big.mark = ",")))

  # Sales models
  m_s12_no <- safe_feols(
    log(sale_price) ~ post_treat | ward_pair_side + ward_pair^sale_year,
    data = sales_2012,
    subset = ~analysis_keep,
    weights = ~weight,
    cluster = ~block_id
  )
  m_s12_ctrl <- safe_feols(
    log(sale_price) ~ post_treat + log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage | ward_pair_side + ward_pair^sale_year,
    data = sales_2012,
    subset = ~analysis_keep,
    weights = ~weight,
    cluster = ~block_id
  )

  m_s15_no <- safe_feols(
    log(sale_price) ~ post_treat | ward_pair_side + ward_pair^sale_year,
    data = sales_2015,
    subset = ~analysis_keep,
    weights = ~weight,
    cluster = ~block_id
  )
  m_s15_ctrl <- safe_feols(
    log(sale_price) ~ post_treat + log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage | ward_pair_side + ward_pair^sale_year,
    data = sales_2015,
    subset = ~analysis_keep,
    weights = ~weight,
    cluster = ~block_id
  )

  # Rental models
  m_rent_no <- safe_feols(
    log(rent_price) ~ post_treat | cohort_ward_pair_side + cohort_ward_pair^year,
    data = rental,
    subset = ~analysis_keep,
    weights = ~weight,
    cluster = ~cohort_block_id
  )
  m_rent_ctrl <- safe_feols(
    log(rent_price) ~ post_treat + building_type_factor + log_sqft + log_beds + log_baths | cohort_ward_pair_side + cohort_ward_pair^year,
    data = rental,
    subset = ~analysis_keep,
    weights = ~weight,
    cluster = ~cohort_block_id
  )

  results[[idx]] <- extract_post_treat(m_s12_no, "sales_2012_no_ctrl", spec); idx <- idx + 1L
  results[[idx]] <- extract_post_treat(m_s12_ctrl, "sales_2012_ctrl", spec); idx <- idx + 1L
  results[[idx]] <- extract_post_treat(m_s15_no, "sales_2015_no_ctrl", spec); idx <- idx + 1L
  results[[idx]] <- extract_post_treat(m_s15_ctrl, "sales_2015_ctrl", spec); idx <- idx + 1L
  results[[idx]] <- extract_post_treat(m_rent_no, "rent_no_ctrl", spec); idx <- idx + 1L
  results[[idx]] <- extract_post_treat(m_rent_ctrl, "rent_ctrl", spec); idx <- idx + 1L
}

results_long <- rbindlist(results, fill = TRUE)
results_long[, stars := fifelse(is.na(p_value), "",
  fifelse(p_value < 0.01, "***", fifelse(p_value < 0.05, "**", fifelse(p_value < 0.1, "*", "")))
)]
results_long[, estimate_fmt := fifelse(is.na(estimate), NA_character_, sprintf("%.3f%s", estimate, stars))]

# Build summary table focused on controlled specs
sum_models <- c("sales_2012_ctrl", "sales_2015_ctrl", "rent_ctrl", "sales_2012_no_ctrl", "sales_2015_no_ctrl", "rent_no_ctrl")
sum_dt <- results_long[model %in% sum_models]

cast_metric <- function(metric) {
  out <- dcast(sum_dt, spec ~ model, value.var = metric)
  setnames(out, old = setdiff(names(out), "spec"), new = paste0(setdiff(names(out), "spec"), "_", metric))
  out
}

summary_dt <- Reduce(function(x, y) merge(x, y, by = "spec", all = TRUE), list(
  cast_metric("estimate"),
  cast_metric("se"),
  cast_metric("p_value"),
  cast_metric("n_obs")
))

summary_dt[, `:=`(
  all_neg_ctrl = sales_2012_ctrl_estimate < 0 & sales_2015_ctrl_estimate < 0 & rent_ctrl_estimate < 0,
  all_sig10_ctrl = sales_2012_ctrl_p_value < 0.10 & sales_2015_ctrl_p_value < 0.10 & rent_ctrl_p_value < 0.10,
  all_sig05_ctrl = sales_2012_ctrl_p_value < 0.05 & sales_2015_ctrl_p_value < 0.05 & rent_ctrl_p_value < 0.05
)]
summary_dt[, `:=`(
  all_neg_sig10_ctrl = all_neg_ctrl & all_sig10_ctrl,
  all_neg_sig05_ctrl = all_neg_ctrl & all_sig05_ctrl,
  both_sales_neg_sig10_ctrl = sales_2012_ctrl_estimate < 0 & sales_2012_ctrl_p_value < 0.10 &
    sales_2015_ctrl_estimate < 0 & sales_2015_ctrl_p_value < 0.10,
  rent_neg_sig10_ctrl = rent_ctrl_estimate < 0 & rent_ctrl_p_value < 0.10
)]

setorder(summary_dt, -all_neg_sig10_ctrl, -both_sales_neg_sig10_ctrl, rent_ctrl_p_value)

fwrite(results_long, opt$output_long)
fwrite(summary_dt, opt$output_summary)

message("\nSaved long results: ", opt$output_long)
message("Saved summary: ", opt$output_summary)
message("\nTop 10 specs by controlled-model pattern:")
print(summary_dt[1:min(10L, .N), .(
  spec,
  sales_2012_ctrl_estimate,
  sales_2012_ctrl_p_value,
  sales_2015_ctrl_estimate,
  sales_2015_ctrl_p_value,
  rent_ctrl_estimate,
  rent_ctrl_p_value,
  all_neg_sig10_ctrl
)])
