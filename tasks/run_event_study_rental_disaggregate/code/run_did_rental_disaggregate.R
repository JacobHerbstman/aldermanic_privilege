# run_did_rental_disaggregate.R
# Pooled difference-in-differences for rentals
# Uses stacked 2015+2023 implementation timing (data constraint)
# Produces a clean, publication-ready table

source("../../setup_environment/code/packages.R")

# =============================================================================
# CONFIGURATION
# =============================================================================
# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental_disaggregate/code")
# bandwidth <- 1000
# weighting <- "triangular"
# fe_type <- "strict_pair_x_year"
# geo_fe_level <- "segment"
# cluster_level <- "twoway_block_segment"
# Rscript run_did_rental_disaggregate.R 1000 "triangular" "strict_pair_x_year" "segment" "twoway_block_segment"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
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
  if (!exists("bandwidth") || !exists("weighting") || !exists("fe_type") || !exists("geo_fe_level") || !exists("cluster_level")) {
    stop("FATAL: Script requires args: <bandwidth> <weighting> <fe_type> [<geo_fe_level> <cluster_level>]", call. = FALSE)
  }
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
did_output <- sprintf("../output/did_table_rental%s.tex", fe_suffix)
did_csv_output <- sprintf("../output/did_table_rental%s.csv", fe_suffix)
did_coef_output <- sprintf("../output/did_coefficients_rental%s.csv", fe_suffix)
did_clean_output <- sprintf("../output/did_table_rental_clean%s.tex", fe_suffix)
if (geo_suffix != "" || cluster_suffix != "") {
    did_output <- sub("\\.tex$", paste0(geo_suffix, cluster_suffix, ".tex"), did_output)
    did_csv_output <- sub("\\.csv$", paste0(geo_suffix, cluster_suffix, ".csv"), did_csv_output)
    did_coef_output <- sub("\\.csv$", paste0(geo_suffix, cluster_suffix, ".csv"), did_coef_output)
    did_clean_output <- sub("\\.tex$", paste0(geo_suffix, cluster_suffix, ".tex"), did_clean_output)
}

message("\n=== Pooled DiD: Rentals ===")
message(sprintf("Bandwidth: %d ft", BANDWIDTH))
message(sprintf("Weighting: %s", WEIGHTING))
message(sprintf("FE Type: %s", FE_TYPE))
message(sprintf("Geo FE Level: %s", GEO_FE_LEVEL))
message(sprintf("Cluster Level: %s", CLUSTER_LEVEL))

# =============================================================================
# LOAD DATA
# =============================================================================
message("\nLoading data...")

data <- read_parquet("../input/rental_listing_panel.parquet")
setDT(data)

message(sprintf("Loaded: %s listings", format(nrow(data), big.mark = ",")))

# =============================================================================
# PREPARE DATA
# =============================================================================
message("\nPreparing data...")

# Apply bandwidth filter
data <- data[dist_ft <= BANDWIDTH]

# Construct weights
if (WEIGHTING == "triangular") {
    data[, weight := pmax(0, 1 - dist_ft / BANDWIDTH)]
} else {
    data[, weight := 1]
}

# Create post indicator and interaction
data[, `:=`(
    post = as.integer(relative_year_capped >= 0),
    post_treat = as.integer(relative_year_capped >= 0) * strictness_change
)]

# Ensure building_type_factor exists
if (!"building_type_factor" %in% names(data)) {
    data[, building_type_factor := as.factor(building_type_clean)]
}

message(sprintf("After bandwidth filter: %s listings", format(nrow(data), big.mark = ",")))

# Filter to complete cases for hedonic controls so both specs have identical samples
data <- data[!is.na(log_sqft) & !is.na(log_beds) & !is.na(log_baths) & !is.na(building_type_factor)]
message(sprintf("After complete cases filter: %s listings", format(nrow(data), big.mark = ",")))

# =============================================================================
# RUN REGRESSIONS
# =============================================================================
message("\nRunning regressions...")

# =============================================================================
# BUILD FE/CLUSTER VARIABLES + EFFECTIVE OBSERVATION DIAGNOSTIC
# =============================================================================
data[, ward_pair_side := sub("^[0-9]+_", "", cohort_ward_pair_side)]
data[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]
data[, cohort_ward_pair := paste(cohort, ward_pair, sep = "_")]

if (GEO_FE_LEVEL == "segment" || CLUSTER_LEVEL %in% c("segment", "twoway_block_segment")) {
    required_cols <- c("segment_id_cohort", "cohort_segment", "cohort_segment_side")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop(sprintf("Missing required segment columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
    data <- data[!is.na(segment_id_cohort) & segment_id_cohort != ""]
}

if (GEO_FE_LEVEL == "segment") {
    fe_side_var <- "cohort_segment_side"
    fe_group_var <- "cohort_segment"
} else {
    fe_side_var <- "cohort_ward_pair_side"
    fe_group_var <- "cohort_ward_pair"
}
data <- data[!is.na(get(fe_side_var)) & get(fe_side_var) != "" & !is.na(get(fe_group_var)) & get(fe_group_var) != ""]

fe_formula <- switch(FE_TYPE,
    "strict_pair_x_year" = sprintf("%s + %s^year", fe_side_var, fe_group_var),
    "pair_trend_plus_year" = sprintf("%s + cohort^year + %s[year]", fe_side_var, fe_group_var),
    "side_plus_year" = sprintf("%s + cohort^year", fe_side_var)
)

if (CLUSTER_LEVEL == "twoway_block_segment") {
    cluster_formula <- ~cohort_block_id + cohort_segment
    cluster_label <- "Two-way: Cohort Block + Cohort Segment"
} else if (CLUSTER_LEVEL == "segment") {
    cluster_formula <- ~cohort_segment
    cluster_label <- "Cohort Segment"
} else {
    cluster_formula <- ~cohort_block_id
    cluster_label <- "Cohort Block"
}

fe_label <- switch(FE_TYPE,
    "strict_pair_x_year" = ifelse(GEO_FE_LEVEL == "segment", "Cohort Segment-Side + Cohort Segment $\\times$ Year FE", "Cohort Border-Pair Side + Cohort Border-Pair $\\times$ Year FE"),
    "pair_trend_plus_year" = ifelse(GEO_FE_LEVEL == "segment", "Cohort Segment-Side + Cohort Year FE + Cohort Segment Trends", "Cohort Border-Pair Side + Cohort Year FE + Cohort Border-Pair Trends"),
    "side_plus_year" = ifelse(GEO_FE_LEVEL == "segment", "Cohort Segment-Side + Cohort Year FE", "Cohort Border-Pair Side + Cohort Year FE")
)

message(sprintf("FE formula: %s", fe_formula))
message(sprintf("Cluster formula: %s", paste(deparse(cluster_formula), collapse = "")))

message("\n=== EFFECTIVE OBSERVATIONS DIAGNOSTIC ===")
data[, is_switcher := abs(strictness_change) > 0]

pair_summary <- data[, .(
    n_sides = uniqueN(get(fe_side_var)),
    n_obs = .N,
    n_switcher = sum(is_switcher),
    n_stayer = sum(!is_switcher)
), by = c(fe_group_var)]

identifying_pairs <- pair_summary[n_sides == 2]
effective_obs <- data[get(fe_group_var) %in% identifying_pairs[[fe_group_var]], .N]

message(sprintf("Total %s groups: %d", fe_group_var, nrow(pair_summary)))
message(sprintf("Pairs with observations on BOTH sides: %d (%.1f%%)", 
    nrow(identifying_pairs), 100 * nrow(identifying_pairs) / nrow(pair_summary)))
message(sprintf("Effective observations (in identifying groups): %s of %s (%.1f%%)",
    format(effective_obs, big.mark = ","),
    format(nrow(data), big.mark = ","),
    100 * effective_obs / nrow(data)))
message(sprintf("Listings in switcher blocks: %s", format(sum(data$is_switcher, na.rm = TRUE), big.mark = ",")))
message(sprintf("Listings in stayer blocks: %s", format(sum(!data$is_switcher, na.rm = TRUE), big.mark = ",")))
message(sprintf("Listings with missing switcher flag: %s", format(sum(is.na(data$is_switcher)), big.mark = ",")))

# Hedonic controls (matching the event study script)
hedonic_vars <- "+ building_type_factor + log_sqft + log_beds + log_baths"

# --- Without Controls ---
message("\n--- Without Hedonic Controls ---")

m_no_ctrl <- feols(
    as.formula(sprintf("log(rent_price) ~ post_treat | %s", fe_formula)),
    data = data,
    weights = ~weight,
    cluster = cluster_formula
)
message(sprintf("No controls: N = %s", format(m_no_ctrl$nobs, big.mark = ",")))

# --- With Controls ---
message("\n--- With Hedonic Controls ---")

m_ctrl <- feols(
    as.formula(sprintf("log(rent_price) ~ post_treat %s | %s", hedonic_vars, fe_formula)),
    data = data,
    weights = ~weight,
    cluster = cluster_formula
)
message(sprintf("With controls: N = %s", format(m_ctrl$nobs, big.mark = ",")))

# =============================================================================
# CREATE CLEAN TABLE
# =============================================================================
message("\nCreating table...")

# Set variable labels for clean output
setFixest_dict(c(
    post_treat = "Post $\\times$ Strictness $\\Delta$",
    log_sqft = "Log Sqft",
    log_beds = "Log Bedrooms",
    log_baths = "Log Bathrooms",
    building_type_factormulti_family = "Multi-family",
    building_type_factorcondo = "Condo",
    building_type_factortownhouse = "Townhouse",
    building_type_factorother = "Other"
))

etable(
    list(m_no_ctrl, m_ctrl),
    title = "Effect of Alderman Strictness on Rents",
    fitstat = ~ n + r2,
    style.tex = style.tex("aer"),
    depvar = FALSE,
    digits = 3,
    digits.stats = 2,
    drop = c("Intercept", "building_type"),
    order = c("Post", "Log"),
    drop.section = "fixef",
    extralines = list(
        "_Hedonic Controls" = c("", "$\\checkmark$"),
        "_Geography FE Level" = c(ifelse(GEO_FE_LEVEL == "segment", "Segment", "Ward Pair"), ifelse(GEO_FE_LEVEL == "segment", "Segment", "Ward Pair")),
        "_Fixed Effects Spec" = c(fe_label, fe_label),
        "_Cluster Level" = c(cluster_label, cluster_label)
    ),
    se.below = TRUE,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    notes = sprintf("Listing-level regressions of log rent on post-redistricting indicator interacted with change in alderman strictness. Stacked estimator combining 2015 and 2023 redistricting events (implementation timing). Sample restricted to listings within %d feet of ward boundaries with non-missing hedonic characteristics. %s kernel weighting. Standard errors clustered at: %s.", as.integer(BANDWIDTH), tools::toTitleCase(WEIGHTING), cluster_label),
    label = "tab:did_rental",
    float = TRUE,
    file = did_output,
    replace = TRUE
)

message(sprintf("Saved: %s", did_output))

# =============================================================================
# ALSO SAVE CSV FOR QUICK INSPECTION
# =============================================================================
results_df <- data.frame(
    specification = c("No controls", "With controls"),
    estimate = c(coef(m_no_ctrl)["post_treat"], coef(m_ctrl)["post_treat"]),
    std_error = c(se(m_no_ctrl)["post_treat"], se(m_ctrl)["post_treat"]),
    n_obs = c(m_no_ctrl$nobs, m_ctrl$nobs),
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
coef_no_ctrl <- data.frame(
    variable = names(coef(m_no_ctrl)),
    estimate_no_ctrl = coef(m_no_ctrl),
    se_no_ctrl = se(m_no_ctrl)
)
coef_ctrl <- data.frame(
    variable = names(coef(m_ctrl)),
    estimate_ctrl = coef(m_ctrl),
    se_ctrl = se(m_ctrl)
)
coef_all <- merge(coef_no_ctrl, coef_ctrl, by = "variable", all = TRUE)
coef_all$n_obs_no_ctrl <- m_no_ctrl$nobs
coef_all$n_obs_ctrl <- m_ctrl$nobs
coef_all$r2_no_ctrl <- fitstat(m_no_ctrl, "r2")$r2
coef_all$r2_ctrl <- fitstat(m_ctrl, "r2")$r2
write_csv(coef_all, did_coef_output)
message(sprintf("Saved: %s", did_coef_output))

# =============================================================================
# CREATE CLEAN SLIDE TABLE (minimal format for slides)
# =============================================================================
message("\nCreating clean slide table...")

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

format_n_millions <- function(n) {
    sprintf("%.2fM", n / 1e6)
}

# Extract values
est_no_ctrl <- coef(m_no_ctrl)["post_treat"]
se_no_ctrl <- se(m_no_ctrl)["post_treat"]
est_ctrl <- coef(m_ctrl)["post_treat"]
se_ctrl <- se(m_ctrl)["post_treat"]
n_obs <- m_no_ctrl$nobs
mean_rent <- mean(data$rent_price, na.rm = TRUE)
median_rent <- median(data$rent_price, na.rm = TRUE)

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
    format_n_millions(n_obs),
    format_n_millions(n_obs),
    format(round(mean_rent, 0), big.mark = ","),
    format(round(median_rent, 0), big.mark = ",")
)

writeLines(slide_table, did_clean_output)
message(sprintf("Saved: %s", did_clean_output))

message("\n=== Summary (Percentage Points) ===")
print(results_df[, c("specification", "estimate_pct", "ci_low_pct", "ci_high_pct", "n_obs")])

message("\nDone!")
