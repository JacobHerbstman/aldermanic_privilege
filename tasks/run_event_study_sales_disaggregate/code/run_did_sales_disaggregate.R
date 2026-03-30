# run_did_sales_disaggregate.R
# Pooled difference-in-differences for home sales
# Compares 2012 (announcement) vs 2015 (implementation) timing
# Produces a clean, publication-ready table

source("../../setup_environment/code/packages.R")

# CONFIGURATION

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_disaggregate/code")
# bandwidth <- 1000
# weighting <- "triangular"
# fe_type <- "strict_pair_x_year"
# geo_fe_level <- "segment"
# cluster_level <- "twoway_block_segment"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth, weighting, fe_type, geo_fe_level, cluster_level)
}

if (length(cli_args) >= 5) {
  bandwidth <- as.numeric(cli_args[1])
  weighting <- cli_args[2]
  fe_type <- cli_args[3]
  geo_fe_level <- tolower(cli_args[4])
  cluster_level <- tolower(cli_args[5])
} else if (length(cli_args) >= 3) {
  bandwidth <- as.numeric(cli_args[1])
  weighting <- cli_args[2]
  fe_type <- cli_args[3]
  geo_fe_level <- tolower(Sys.getenv("GEO_FE_LEVEL", "segment"))
  cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "twoway_block_segment"))
} else {
  stop("FATAL: Script requires args: <bandwidth> <weighting> <fe_type> [<geo_fe_level> <cluster_level>]", call. = FALSE)
}

BANDWIDTH <- bandwidth
WEIGHTING <- weighting
FE_TYPE <- fe_type
GEO_FE_LEVEL <- geo_fe_level
CLUSTER_LEVEL <- cluster_level

if (!FE_TYPE %in% c("strict_pair_x_year", "pair_trend_plus_year", "side_plus_year")) {
    stop("--fe_type must be one of: strict_pair_x_year, pair_trend_plus_year, side_plus_year")
}

if (!WEIGHTING %in% c("triangular", "uniform")) {
    stop("--weighting must be one of: triangular, uniform")
}
if (!GEO_FE_LEVEL %in% c("segment", "ward_pair")) {
    stop("--geo_fe_level must be one of: segment, ward_pair", call. = FALSE)
}
if (!CLUSTER_LEVEL %in% c("twoway_block_segment", "block", "segment")) {
    stop("--cluster_level must be one of: twoway_block_segment, block, segment", call. = FALSE)
}
if (GEO_FE_LEVEL == "segment" && BANDWIDTH > 1000) {
    stop("Segment FE requested with bandwidth > 1000. Use bandwidth <= 1000.", call. = FALSE)
}

fe_suffix <- ifelse(
    FE_TYPE == "strict_pair_x_year",
    "",
    ifelse(FE_TYPE == "pair_trend_plus_year", "_pairtrend", "_yearfe")
)
geo_suffix <- ifelse(GEO_FE_LEVEL == "segment", "", "_geo_wardpair")
cluster_suffix <- ifelse(CLUSTER_LEVEL == "twoway_block_segment", "", paste0("_clust_", CLUSTER_LEVEL))
did_output <- sprintf("../output/did_table_sales%s.tex", fe_suffix)
did_csv_output <- sprintf("../output/did_table_sales%s.csv", fe_suffix)
did_coef_output <- sprintf("../output/did_coefficients_sales%s.csv", fe_suffix)
did_clean_output <- sprintf("../output/did_table_sales_clean%s.tex", fe_suffix)
if (geo_suffix != "" || cluster_suffix != "") {
    did_output <- sub("\\.tex$", paste0(geo_suffix, cluster_suffix, ".tex"), did_output)
    did_csv_output <- sub("\\.csv$", paste0(geo_suffix, cluster_suffix, ".csv"), did_csv_output)
    did_coef_output <- sub("\\.csv$", paste0(geo_suffix, cluster_suffix, ".csv"), did_coef_output)
    did_clean_output <- sub("\\.tex$", paste0(geo_suffix, cluster_suffix, ".tex"), did_clean_output)
}

message("\n=== Pooled DiD: Home Sales ===")
message(sprintf("Bandwidth: %d ft", BANDWIDTH))
message(sprintf("Weighting: %s", WEIGHTING))
message(sprintf("FE Type: %s", FE_TYPE))
message(sprintf("Geo FE Level: %s", GEO_FE_LEVEL))
message(sprintf("Cluster Level: %s", CLUSTER_LEVEL))

# =============================================================================
# LOAD DATA - Individual Cohorts (Not Stacked)
# =============================================================================
message("\nLoading data...")

# 2012 cohort (announcement timing for 2015 redistricting)
data_2012 <- read_parquet("../input/sales_transaction_panel_2012.parquet")
setDT(data_2012)

# 2015 cohort (implementation timing for 2015 redistricting)
data_2015 <- read_parquet("../input/sales_transaction_panel_2015.parquet")
setDT(data_2015)

message(sprintf("2012 cohort: %s transactions", format(nrow(data_2012), big.mark = ",")))
message(sprintf("2015 cohort: %s transactions", format(nrow(data_2015), big.mark = ",")))

# =============================================================================
# PREPARE DATA
# =============================================================================
data_2012 <- data_2012[dist_ft <= BANDWIDTH]
if (WEIGHTING == "triangular") {
    data_2012[, weight := pmax(0, 1 - dist_ft / BANDWIDTH)]
} else {
    data_2012[, weight := 1]
}
data_2012[, `:=`(
    post = as.integer(relative_year >= 0),
    post_treat = as.integer(relative_year >= 0) * strictness_change
)]

data_2015 <- data_2015[dist_ft <= BANDWIDTH]
if (WEIGHTING == "triangular") {
    data_2015[, weight := pmax(0, 1 - dist_ft / BANDWIDTH)]
} else {
    data_2015[, weight := 1]
}
data_2015[, `:=`(
    post = as.integer(relative_year >= 0),
    post_treat = as.integer(relative_year >= 0) * strictness_change
)]

message(sprintf("After bandwidth filter - 2012: %s", format(nrow(data_2012), big.mark = ",")))
message(sprintf("After bandwidth filter - 2015: %s", format(nrow(data_2015), big.mark = ",")))

# =============================================================================
# RESTRICT TO COMPLETE HEDONIC SAMPLE (for comparability across specs)
# =============================================================================
hedonic_vars_list <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")

n_before_2012 <- nrow(data_2012)
data_2012 <- data_2012[complete.cases(data_2012[, ..hedonic_vars_list])]
message(sprintf(
    "After complete cases - 2012: %s (dropped %s)",
    format(nrow(data_2012), big.mark = ","),
    format(n_before_2012 - nrow(data_2012), big.mark = ",")
))

n_before_2015 <- nrow(data_2015)
data_2015 <- data_2015[complete.cases(data_2015[, ..hedonic_vars_list])]
message(sprintf("After complete cases - 2015: %s (dropped %s)",
    format(nrow(data_2015), big.mark = ","),
    format(n_before_2015 - nrow(data_2015), big.mark = ",")
))

# =============================================================================
# BUILD FE / CLUSTER VARIABLES
# =============================================================================
cluster_label <- ifelse(
    CLUSTER_LEVEL == "twoway_block_segment",
    "Two-way: Block + Segment",
    ifelse(CLUSTER_LEVEL == "segment", "Segment", "Block")
)

fe_label <- switch(FE_TYPE,
    "strict_pair_x_year" = ifelse(GEO_FE_LEVEL == "segment", "Segment-Side + Segment $\\times$ Year FE", "Border-Pair Side + Border-Pair $\\times$ Year FE"),
    "pair_trend_plus_year" = ifelse(GEO_FE_LEVEL == "segment", "Segment-Side + Year FE + Segment Trends", "Border-Pair Side + Year FE + Border-Pair Trends"),
    "side_plus_year" = ifelse(GEO_FE_LEVEL == "segment", "Segment-Side + Year FE", "Border-Pair Side + Year FE")
)

data_2012[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]
data_2015[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]

needs_segment <- GEO_FE_LEVEL == "segment" || CLUSTER_LEVEL %in% c("segment", "twoway_block_segment")
if (needs_segment) {
    req_cols <- c("segment_id_cohort", "segment_side")
    missing_cols_2012 <- setdiff(req_cols, names(data_2012))
    missing_cols_2015 <- setdiff(req_cols, names(data_2015))
    if (length(missing_cols_2012) > 0) {
        stop(sprintf("Missing required segment columns in 2012 sales cohort data: %s", paste(missing_cols_2012, collapse = ", ")), call. = FALSE)
    }
    if (length(missing_cols_2015) > 0) {
        stop(sprintf("Missing required segment columns in 2015 sales cohort data: %s", paste(missing_cols_2015, collapse = ", ")), call. = FALSE)
    }
    data_2012 <- data_2012[!is.na(segment_id_cohort) & segment_id_cohort != ""]
    data_2015 <- data_2015[!is.na(segment_id_cohort) & segment_id_cohort != ""]
}

if (GEO_FE_LEVEL == "segment") {
    unstacked_fe_side_var <- "segment_side"
    unstacked_fe_group_var <- "segment_id_cohort"
} else {
    unstacked_fe_side_var <- "ward_pair_side"
    unstacked_fe_group_var <- "ward_pair"
}
data_2012 <- data_2012[!is.na(get(unstacked_fe_side_var)) & get(unstacked_fe_side_var) != "" & !is.na(get(unstacked_fe_group_var)) & get(unstacked_fe_group_var) != ""]
data_2015 <- data_2015[!is.na(get(unstacked_fe_side_var)) & get(unstacked_fe_side_var) != "" & !is.na(get(unstacked_fe_group_var)) & get(unstacked_fe_group_var) != ""]

if (FE_TYPE == "strict_pair_x_year") {
    fe_formula_unstacked <- sprintf("%s + %s^sale_year", unstacked_fe_side_var, unstacked_fe_group_var)
} else if (FE_TYPE == "pair_trend_plus_year") {
    fe_formula_unstacked <- sprintf("%s + sale_year + %s[sale_year]", unstacked_fe_side_var, unstacked_fe_group_var)
} else {
    fe_formula_unstacked <- sprintf("%s + sale_year", unstacked_fe_side_var)
}

if (CLUSTER_LEVEL == "twoway_block_segment") {
    cluster_formula_unstacked <- ~block_id + segment_id_cohort
} else if (CLUSTER_LEVEL == "segment") {
    cluster_formula_unstacked <- ~segment_id_cohort
} else {
    cluster_formula_unstacked <- ~block_id
}

message(sprintf("Unstacked FE formula: %s", fe_formula_unstacked))
message(sprintf("Unstacked cluster formula: %s", paste(deparse(cluster_formula_unstacked), collapse = "")))

message("\n=== EFFECTIVE OBSERVATIONS DIAGNOSTIC ===")
data_2012[, is_switcher := abs(strictness_change) > 0]
pair_summary_2012 <- data_2012[, .(
    n_sides = uniqueN(get(unstacked_fe_side_var)),
    n_obs = .N,
    n_switcher = sum(is_switcher),
    n_stayer = sum(!is_switcher)
), by = c(unstacked_fe_group_var)]
identifying_pairs_2012 <- pair_summary_2012[n_sides == 2]
effective_obs_2012 <- data_2012[get(unstacked_fe_group_var) %in% identifying_pairs_2012[[unstacked_fe_group_var]], .N]
message("\n2012 Cohort:")
message(sprintf("  Total %s groups: %d", unstacked_fe_group_var, nrow(pair_summary_2012)))
message(sprintf("  Pairs with observations on BOTH sides: %d (%.1f%%)",
    nrow(identifying_pairs_2012), 100 * nrow(identifying_pairs_2012) / nrow(pair_summary_2012)))
message(sprintf("  Effective observations (in identifying groups): %s of %s (%.1f%%)",
    format(effective_obs_2012, big.mark = ","),
    format(nrow(data_2012), big.mark = ","),
    100 * effective_obs_2012 / nrow(data_2012)))
message(sprintf("  Transactions in switcher blocks: %s", format(sum(data_2012$is_switcher, na.rm = TRUE), big.mark = ",")))
message(sprintf("  Transactions in stayer blocks: %s", format(sum(!data_2012$is_switcher, na.rm = TRUE), big.mark = ",")))
message(sprintf("  Transactions with missing switcher flag: %s", format(sum(is.na(data_2012$is_switcher)), big.mark = ",")))

data_2015[, is_switcher := abs(strictness_change) > 0]
pair_summary_2015 <- data_2015[, .(
    n_sides = uniqueN(get(unstacked_fe_side_var)),
    n_obs = .N,
    n_switcher = sum(is_switcher),
    n_stayer = sum(!is_switcher)
), by = c(unstacked_fe_group_var)]
identifying_pairs_2015 <- pair_summary_2015[n_sides == 2]
effective_obs_2015 <- data_2015[get(unstacked_fe_group_var) %in% identifying_pairs_2015[[unstacked_fe_group_var]], .N]
message("\n2015 Cohort:")
message(sprintf("  Total %s groups: %d", unstacked_fe_group_var, nrow(pair_summary_2015)))
message(sprintf("  Pairs with observations on BOTH sides: %d (%.1f%%)",
    nrow(identifying_pairs_2015), 100 * nrow(identifying_pairs_2015) / nrow(pair_summary_2015)))
message(sprintf("  Effective observations (in identifying groups): %s of %s (%.1f%%)",
    format(effective_obs_2015, big.mark = ","),
    format(nrow(data_2015), big.mark = ","),
    100 * effective_obs_2015 / nrow(data_2015)))
message(sprintf("  Transactions in switcher blocks: %s", format(sum(data_2015$is_switcher, na.rm = TRUE), big.mark = ",")))
message(sprintf("  Transactions in stayer blocks: %s", format(sum(!data_2015$is_switcher, na.rm = TRUE), big.mark = ",")))
message(sprintf("  Transactions with missing switcher flag: %s", format(sum(is.na(data_2015$is_switcher)), big.mark = ",")))

# =============================================================================
# RUN REGRESSIONS
# =============================================================================
message("\nRunning regressions...")

# Hedonic controls formula component
hedonic_vars <- "+ log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage"

# --- 2012 Cohort (Announcement Timing) ---
message("\n--- 2012 Cohort (Announcement Timing) ---")

# Without controls
m_2012_no_ctrl <- feols(
    as.formula(sprintf("log(sale_price) ~ post_treat | %s", fe_formula_unstacked)),
    data = data_2012,
    weights = ~weight,
    cluster = cluster_formula_unstacked
)
message(sprintf("2012 (no controls): N = %s", format(m_2012_no_ctrl$nobs, big.mark = ",")))

# With controls
m_2012_ctrl <- feols(
    as.formula(sprintf("log(sale_price) ~ post_treat %s | %s", hedonic_vars, fe_formula_unstacked)),
    data = data_2012,
    weights = ~weight,
    cluster = cluster_formula_unstacked
)
message(sprintf("2012 (with controls): N = %s", format(m_2012_ctrl$nobs, big.mark = ",")))

# --- 2015 Cohort (Implementation Timing) ---
message("\n--- 2015 Cohort (Implementation Timing) ---")

# Without controls
m_2015_no_ctrl <- feols(
    as.formula(sprintf("log(sale_price) ~ post_treat | %s", fe_formula_unstacked)),
    data = data_2015,
    weights = ~weight,
    cluster = cluster_formula_unstacked
)
message(sprintf("2015 (no controls): N = %s", format(m_2015_no_ctrl$nobs, big.mark = ",")))

# With controls
m_2015_ctrl <- feols(
    as.formula(sprintf("log(sale_price) ~ post_treat %s | %s", hedonic_vars, fe_formula_unstacked)),
    data = data_2015,
    weights = ~weight,
    cluster = cluster_formula_unstacked
)
message(sprintf("2015 (with controls): N = %s", format(m_2015_ctrl$nobs, big.mark = ",")))
mean_price_2012 <- mean(data_2012$sale_price, na.rm = TRUE)
mean_price_2015 <- mean(data_2015$sale_price, na.rm = TRUE)
ward_pairs_2012 <- n_distinct(data_2012$ward_pair)
ward_pairs_2015 <- n_distinct(data_2015$ward_pair)

# =============================================================================
# CREATE CLEAN TABLE
# =============================================================================
message("\nCreating table...")

# Set variable labels for clean output
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
    list(m_2012_no_ctrl, m_2012_ctrl, m_2015_no_ctrl, m_2015_ctrl),
    title = "Effect of Alderman Strictness on Home Sale Prices",
    fitstat = ~ n + r2,
    style.tex = style.tex("aer"),
    depvar = FALSE,
    digits = 3,
    digits.stats = 2,
    drop = "Intercept",
    order = c("Post", "Log"),
    drop.section = "fixef",
    extralines = list(
        "_N" = c(
            format(nobs(m_2012_no_ctrl), big.mark = ","),
            format(nobs(m_2012_ctrl), big.mark = ","),
            format(nobs(m_2015_no_ctrl), big.mark = ","),
            format(nobs(m_2015_ctrl), big.mark = ",")
        ),
        "_Dep. Var. Mean" = c(
            paste0("\\$", format(round(mean_price_2012, 0), big.mark = ",")),
            paste0("\\$", format(round(mean_price_2012, 0), big.mark = ",")),
            paste0("\\$", format(round(mean_price_2015, 0), big.mark = ",")),
            paste0("\\$", format(round(mean_price_2015, 0), big.mark = ","))
        ),
        "_Ward Pairs" = c(
            format(ward_pairs_2012, big.mark = ","),
            format(ward_pairs_2012, big.mark = ","),
            format(ward_pairs_2015, big.mark = ","),
            format(ward_pairs_2015, big.mark = ",")
        ),
        "_Timing" = c("Announcement", "Announcement", "Implementation", "Implementation"),
        "_Hedonic Controls" = c("", "$\\checkmark$", "", "$\\checkmark$"),
        "_Geography FE Level" = rep(ifelse(GEO_FE_LEVEL == "segment", "Segment", "Ward Pair"), 4),
        "_Fixed Effects Spec" = c(fe_label, fe_label, fe_label, fe_label),
        "_Cluster Level" = rep(cluster_label, 4)
    ),
    se.below = TRUE,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    notes = sprintf(
        "Transaction-level regressions of log sale price on post-redistricting indicator interacted with change in alderman strictness. Columns (1)--(2) use 2012 announcement timing; columns (3)--(4) use 2015 implementation timing. Sample restricted to transactions within %d feet of ward boundaries with non-missing hedonic characteristics. %s kernel weighting. Standard errors clustered at: %s.",
        as.integer(BANDWIDTH),
        tools::toTitleCase(WEIGHTING),
        cluster_label
    ),
    label = "tab:did_sales",
    float = TRUE,
    file = did_output,
    replace = TRUE
)

message(sprintf("Saved: %s", did_output))

# =============================================================================
# ALSO SAVE CSV FOR QUICK INSPECTION
# =============================================================================
results_df <- data.frame(
    specification = c(
        "2012 Announcement (no controls)", "2012 Announcement (with controls)",
        "2015 Implementation (no controls)", "2015 Implementation (with controls)"
    ),
    estimate = c(
        coef(m_2012_no_ctrl)["post_treat"], coef(m_2012_ctrl)["post_treat"],
        coef(m_2015_no_ctrl)["post_treat"], coef(m_2015_ctrl)["post_treat"]
    ),
    std_error = c(
        se(m_2012_no_ctrl)["post_treat"], se(m_2012_ctrl)["post_treat"],
        se(m_2015_no_ctrl)["post_treat"], se(m_2015_ctrl)["post_treat"]
    ),
    n_obs = c(m_2012_no_ctrl$nobs, m_2012_ctrl$nobs, m_2015_no_ctrl$nobs, m_2015_ctrl$nobs),
    geo_fe_level = GEO_FE_LEVEL,
    cluster_level = CLUSTER_LEVEL
)
results_df$estimate_pct <- results_df$estimate * 100
results_df$ci_low_pct <- (results_df$estimate - 1.96 * results_df$std_error) * 100
results_df$ci_high_pct <- (results_df$estimate + 1.96 * results_df$std_error) * 100

write_csv(results_df, did_csv_output)
message(sprintf("Saved: %s", did_csv_output))

# =============================================================================
# EXPORT FULL COEFFICIENTS FOR COMBINED TABLE
# =============================================================================
# 2012 Announcement timing
coef_2012_no_ctrl <- data.frame(
    variable = names(coef(m_2012_no_ctrl)),
    estimate = coef(m_2012_no_ctrl),
    se = se(m_2012_no_ctrl)
)
coef_2012_no_ctrl$specification <- "2012_no_ctrl"

coef_2012_ctrl <- data.frame(
    variable = names(coef(m_2012_ctrl)),
    estimate = coef(m_2012_ctrl),
    se = se(m_2012_ctrl)
)
coef_2012_ctrl$specification <- "2012_ctrl"

# 2015 Implementation timing
coef_2015_no_ctrl <- data.frame(
    variable = names(coef(m_2015_no_ctrl)),
    estimate = coef(m_2015_no_ctrl),
    se = se(m_2015_no_ctrl)
)
coef_2015_no_ctrl$specification <- "2015_no_ctrl"

coef_2015_ctrl <- data.frame(
    variable = names(coef(m_2015_ctrl)),
    estimate = coef(m_2015_ctrl),
    se = se(m_2015_ctrl)
)
coef_2015_ctrl$specification <- "2015_ctrl"

# Combine all
coef_all <- rbind(coef_2012_no_ctrl, coef_2012_ctrl, coef_2015_no_ctrl, coef_2015_ctrl)

# Add sample sizes and R2
coef_all$n_obs <- NA
coef_all$r2 <- NA
coef_all$n_obs[coef_all$specification == "2012_no_ctrl"] <- m_2012_no_ctrl$nobs
coef_all$n_obs[coef_all$specification == "2012_ctrl"] <- m_2012_ctrl$nobs
coef_all$n_obs[coef_all$specification == "2015_no_ctrl"] <- m_2015_no_ctrl$nobs
coef_all$n_obs[coef_all$specification == "2015_ctrl"] <- m_2015_ctrl$nobs
coef_all$r2[coef_all$specification == "2012_no_ctrl"] <- fitstat(m_2012_no_ctrl, "r2")$r2
coef_all$r2[coef_all$specification == "2012_ctrl"] <- fitstat(m_2012_ctrl, "r2")$r2
coef_all$r2[coef_all$specification == "2015_no_ctrl"] <- fitstat(m_2015_no_ctrl, "r2")$r2
coef_all$r2[coef_all$specification == "2015_ctrl"] <- fitstat(m_2015_ctrl, "r2")$r2
coef_all$dep_var_mean <- NA_real_
coef_all$ward_pairs <- NA_real_
coef_all$dep_var_mean[coef_all$specification %in% c("2012_no_ctrl", "2012_ctrl")] <- mean(data_2012$sale_price, na.rm = TRUE)
coef_all$dep_var_mean[coef_all$specification %in% c("2015_no_ctrl", "2015_ctrl")] <- mean(data_2015$sale_price, na.rm = TRUE)
coef_all$ward_pairs[coef_all$specification %in% c("2012_no_ctrl", "2012_ctrl")] <- n_distinct(data_2012$ward_pair)
coef_all$ward_pairs[coef_all$specification %in% c("2015_no_ctrl", "2015_ctrl")] <- n_distinct(data_2015$ward_pair)

write_csv(coef_all, did_coef_output)
message(sprintf("Saved: %s", did_coef_output))

# =============================================================================
# CREATE CLEAN SLIDE TABLE (Stacked Announcement Timing)
# =============================================================================
message("\nCreating clean slide table...")

# Load stacked announcement panel
data_ann <- read_parquet("../input/sales_transaction_panel_announcement.parquet")
setDT(data_ann)
message(sprintf("Stacked announcement panel: %s transactions", format(nrow(data_ann), big.mark = ",")))

# Prepare data
data_ann <- data_ann[dist_ft <= BANDWIDTH]
if (WEIGHTING == "triangular") {
    data_ann[, weight := pmax(0, 1 - dist_ft / BANDWIDTH)]
} else {
    data_ann[, weight := 1]
}
data_ann[, `:=`(
    post = as.integer(relative_year >= 0),
    post_treat = as.integer(relative_year >= 0) * strictness_change
)]

# Restrict to complete hedonic sample
data_ann <- data_ann[complete.cases(data_ann[, ..hedonic_vars_list])]
message(sprintf("After complete cases filter: %s transactions", format(nrow(data_ann), big.mark = ",")))

# Build stacked geo FE variables
data_ann[, ward_pair_side_temp := sub("^[0-9]+_", "", cohort_ward_pair_side)]
data_ann[, ward_pair := sub("_[0-9]+$", "", ward_pair_side_temp)]
data_ann[, cohort_ward_pair := paste(cohort, ward_pair, sep = "_")]

if (needs_segment) {
    req_cols_ann <- c("segment_id_cohort", "cohort_segment", "cohort_segment_side")
    missing_cols_ann <- setdiff(req_cols_ann, names(data_ann))
    if (length(missing_cols_ann) > 0) {
        stop(sprintf("Missing required segment columns in stacked sales data: %s", paste(missing_cols_ann, collapse = ", ")), call. = FALSE)
    }
    data_ann <- data_ann[!is.na(segment_id_cohort) & segment_id_cohort != ""]
}

if (GEO_FE_LEVEL == "segment") {
    stacked_fe_side_var <- "cohort_segment_side"
    stacked_fe_group_var <- "cohort_segment"
} else {
    stacked_fe_side_var <- "cohort_ward_pair_side"
    stacked_fe_group_var <- "cohort_ward_pair"
}
data_ann <- data_ann[!is.na(get(stacked_fe_side_var)) & get(stacked_fe_side_var) != "" & !is.na(get(stacked_fe_group_var)) & get(stacked_fe_group_var) != ""]

if (FE_TYPE == "strict_pair_x_year") {
    fe_formula_stacked <- sprintf("%s + %s^sale_year", stacked_fe_side_var, stacked_fe_group_var)
} else if (FE_TYPE == "pair_trend_plus_year") {
    fe_formula_stacked <- sprintf("%s + cohort^sale_year + %s[sale_year]", stacked_fe_side_var, stacked_fe_group_var)
} else {
    fe_formula_stacked <- sprintf("%s + cohort^sale_year", stacked_fe_side_var)
}

if (CLUSTER_LEVEL == "twoway_block_segment") {
    cluster_formula_stacked <- ~cohort_block_id + cohort_segment
} else if (CLUSTER_LEVEL == "segment") {
    cluster_formula_stacked <- ~cohort_segment
} else {
    cluster_formula_stacked <- ~cohort_block_id
}
message(sprintf("Stacked FE formula: %s", fe_formula_stacked))
message(sprintf("Stacked cluster formula: %s", paste(deparse(cluster_formula_stacked), collapse = "")))

# Run regressions with stacked FE structure
m_ann_no_ctrl <- feols(
    as.formula(sprintf("log(sale_price) ~ post_treat | %s", fe_formula_stacked)),
    data = data_ann,
    weights = ~weight,
    cluster = cluster_formula_stacked
)
message(sprintf("Stacked announcement (no controls): N = %s", format(m_ann_no_ctrl$nobs, big.mark = ",")))

m_ann_ctrl <- feols(
    as.formula(sprintf("log(sale_price) ~ post_treat %s | %s", hedonic_vars, fe_formula_stacked)),
    data = data_ann,
    weights = ~weight,
    cluster = cluster_formula_stacked
)
message(sprintf("Stacked announcement (with controls): N = %s", format(m_ann_ctrl$nobs, big.mark = ",")))

# Format coefficient and SE nicely
format_coef <- function(est, se, digits = 3) {
    stars <- ifelse(abs(est/se) > 2.576, "***",
             ifelse(abs(est/se) > 1.96, "**",
             ifelse(abs(est/se) > 1.645, "*", "")))
    sprintf("%.*f%s", digits, est, stars)
}

format_se <- function(se, digits = 3) {
    sprintf("(%.*f)", digits, se)
}

format_n <- function(n) {
    format(n, big.mark = ",")
}

# Extract values (using stacked announcement timing to match figures)
est_no_ctrl <- coef(m_ann_no_ctrl)["post_treat"]
se_no_ctrl <- se(m_ann_no_ctrl)["post_treat"]
est_ctrl <- coef(m_ann_ctrl)["post_treat"]
se_ctrl <- se(m_ann_ctrl)["post_treat"]
n_obs_no_ctrl <- m_ann_no_ctrl$nobs
n_obs_ctrl <- m_ann_ctrl$nobs
mean_price <- mean(data_ann$sale_price, na.rm = TRUE)
median_price <- median(data_ann$sale_price, na.rm = TRUE)

# Create minimal LaTeX table for slides
slide_table <- sprintf('
\\begin{tabular}{lcc}
\\toprule
 & No Hedonics & Hedonics \\\\
\\midrule
Post $\\times$ $\\Delta$Strict & %s & %s \\\\
 & %s & %s \\\\
\\midrule
N & %s & %s \\\\
Dep.\\ Var. & \\multicolumn{2}{c}{\\footnotesize Mean: \\$%s / Median: \\$%s} \\\\
\\bottomrule
\\end{tabular}
',
    format_coef(est_no_ctrl, se_no_ctrl),
    format_coef(est_ctrl, se_ctrl),
    format_se(se_no_ctrl),
    format_se(se_ctrl),
    format_n(n_obs_no_ctrl),
    format_n(n_obs_ctrl),
    sprintf("%.0fK", mean_price / 1000),
    sprintf("%.0fK", median_price / 1000)
)

writeLines(slide_table, did_clean_output)
message(sprintf("Saved: %s", did_clean_output))

message("\n=== Summary (Percentage Points) ===")
print(results_df[, c("specification", "estimate_pct", "ci_low_pct", "ci_high_pct", "n_obs")])

message("\nDone!")
