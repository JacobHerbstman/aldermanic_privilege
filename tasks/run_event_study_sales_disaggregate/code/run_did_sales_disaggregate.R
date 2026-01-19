# run_did_sales_disaggregate.R
# Pooled difference-in-differences for home sales
# Compares 2012 (announcement) vs 2015 (implementation) timing
# Produces a clean, publication-ready table

source("../../setup_environment/code/packages.R")

# =============================================================================
# CONFIGURATION
# =============================================================================
BANDWIDTH <- 1000
WEIGHTING <- "triangular"

message("\n=== Pooled DiD: Home Sales ===")
message(sprintf("Bandwidth: %d ft", BANDWIDTH))
message(sprintf("Weighting: %s", WEIGHTING))

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
prepare_did_data <- function(data, bandwidth, weighting) {
    # Apply bandwidth filter
    data <- data[dist_ft <= bandwidth]

    # Construct weights
    if (weighting == "triangular") {
        data[, weight := pmax(0, 1 - dist_ft / bandwidth)]
    } else {
        data[, weight := 1]
    }

    # Create post indicator and interaction
    data[, `:=`(
        post = as.integer(relative_year >= 0),
        post_treat = as.integer(relative_year >= 0) * strictness_change
    )]

    return(data)
}

data_2012 <- prepare_did_data(data_2012, BANDWIDTH, WEIGHTING)
data_2015 <- prepare_did_data(data_2015, BANDWIDTH, WEIGHTING)

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
# CREATE WARD_PAIR VARIABLE AND EFFECTIVE OBSERVATIONS DIAGNOSTIC
# =============================================================================
# ward_pair_side looks like "13_23_13" (border between 13-23, on side 13)
# ward_pair is "13_23" (just the border, not the side)
data_2012[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]
data_2015[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]

message("\n=== EFFECTIVE OBSERVATIONS DIAGNOSTIC ===")
message(sprintf("2012 Cohort: %d ward_pairs, %d ward_pair_sides", 
    uniqueN(data_2012$ward_pair), uniqueN(data_2012$ward_pair_side)))
message(sprintf("2015 Cohort: %d ward_pairs, %d ward_pair_sides", 
    uniqueN(data_2015$ward_pair), uniqueN(data_2015$ward_pair_side)))

# Count observations with variation across sides within each ward_pair
calc_effective_obs <- function(dt, cohort_label) {
    # Create switcher indicator
    dt[, is_switcher := abs(strictness_change) > 0]
    
    # Count by ward_pair: need observations on BOTH sides
    pair_summary <- dt[, .(
        n_sides = uniqueN(ward_pair_side),
        n_obs = .N,
        n_switcher = sum(is_switcher),
        n_stayer = sum(!is_switcher)
    ), by = ward_pair]
    
    # Pairs with observations on both sides contribute to identification
    identifying_pairs <- pair_summary[n_sides == 2]
    
    # Observations in identifying pairs
    effective_obs <- dt[ward_pair %in% identifying_pairs$ward_pair, .N]
    
    message(sprintf("\n%s Cohort:", cohort_label))
    message(sprintf("  Total ward_pairs: %d", nrow(pair_summary)))
    message(sprintf("  Pairs with observations on BOTH sides: %d (%.1f%%)", 
        nrow(identifying_pairs), 100 * nrow(identifying_pairs) / nrow(pair_summary)))
    message(sprintf("  Effective observations (in identifying pairs): %s of %s (%.1f%%)",
        format(effective_obs, big.mark = ","),
        format(nrow(dt), big.mark = ","),
        100 * effective_obs / nrow(dt)))
    message(sprintf("  Transactions in switcher blocks: %s", format(sum(dt$is_switcher), big.mark = ",")))
    message(sprintf("  Transactions in stayer blocks: %s", format(sum(!dt$is_switcher), big.mark = ",")))
}

calc_effective_obs(data_2012, "2012")
calc_effective_obs(data_2015, "2015")

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
    log(sale_price) ~ post_treat | ward_pair_side + ward_pair^sale_year,
    data = data_2012,
    weights = ~weight,
    cluster = ~block_id
)
message(sprintf("2012 (no controls): N = %s", format(m_2012_no_ctrl$nobs, big.mark = ",")))

# With controls
m_2012_ctrl <- feols(
    as.formula(paste("log(sale_price) ~ post_treat", hedonic_vars, "| ward_pair_side + ward_pair^sale_year")),
    data = data_2012,
    weights = ~weight,
    cluster = ~block_id
)
message(sprintf("2012 (with controls): N = %s", format(m_2012_ctrl$nobs, big.mark = ",")))

# --- 2015 Cohort (Implementation Timing) ---
message("\n--- 2015 Cohort (Implementation Timing) ---")

# Without controls
m_2015_no_ctrl <- feols(
    log(sale_price) ~ post_treat | ward_pair_side + ward_pair^sale_year,
    data = data_2015,
    weights = ~weight,
    cluster = ~block_id
)
message(sprintf("2015 (no controls): N = %s", format(m_2015_no_ctrl$nobs, big.mark = ",")))

# With controls
m_2015_ctrl <- feols(
    as.formula(paste("log(sale_price) ~ post_treat", hedonic_vars, "| ward_pair_side + ward_pair^sale_year")),
    data = data_2015,
    weights = ~weight,
    cluster = ~block_id
)
message(sprintf("2015 (with controls): N = %s", format(m_2015_ctrl$nobs, big.mark = ",")))

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
        "_Timing" = c("Announcement", "Announcement", "Implementation", "Implementation"),
        "_Hedonic Controls" = c("", "$\\checkmark$", "", "$\\checkmark$"),
        "_Border Pair $\\times$ Side FE" = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
        "_Border Pair $\\times$ Year FE" = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
    ),
    se.below = TRUE,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    notes = "Transaction-level regressions of log sale price on post-redistricting indicator interacted with change in alderman strictness. Columns (1)--(2) use 2012 announcement timing; columns (3)--(4) use 2015 implementation timing. Sample restricted to transactions within 1,000 feet of ward boundaries with non-missing hedonic characteristics. Triangular kernel weighting. Standard errors clustered by census block in parentheses.",
    label = "tab:did_sales",
    float = TRUE,
    file = "../output/did_table_sales.tex",
    replace = TRUE
)

message("Saved: ../output/did_table_sales.tex")

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
    n_obs = c(m_2012_no_ctrl$nobs, m_2012_ctrl$nobs, m_2015_no_ctrl$nobs, m_2015_ctrl$nobs)
)
results_df$estimate_pct <- results_df$estimate * 100
results_df$ci_low_pct <- (results_df$estimate - 1.96 * results_df$std_error) * 100
results_df$ci_high_pct <- (results_df$estimate + 1.96 * results_df$std_error) * 100

write_csv(results_df, "../output/did_table_sales.csv")
message("Saved: ../output/did_table_sales.csv")

# =============================================================================
# CREATE CLEAN SLIDE TABLE (Just 2015 cohort, minimal format)
# =============================================================================
message("\nCreating clean slide table...")

# For slides: Just show 2015 (implementation timing) with no hedonics vs hedonics
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

# Extract values (using 2015 implementation timing to match figures)
est_no_ctrl <- coef(m_2015_no_ctrl)["post_treat"]
se_no_ctrl <- se(m_2015_no_ctrl)["post_treat"]
est_ctrl <- coef(m_2015_ctrl)["post_treat"]
se_ctrl <- se(m_2015_ctrl)["post_treat"]
n_obs <- m_2015_no_ctrl$nobs
r2_no_ctrl <- fitstat(m_2015_no_ctrl, "r2")$r2
r2_ctrl <- fitstat(m_2015_ctrl, "r2")$r2

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
$R^2$ & %.2f & %.2f \\\\
\\bottomrule
\\end{tabular}
',
    format_coef(est_no_ctrl, se_no_ctrl),
    format_coef(est_ctrl, se_ctrl),
    format_se(se_no_ctrl),
    format_se(se_ctrl),
    format_n(n_obs),
    format_n(n_obs),
    r2_no_ctrl,
    r2_ctrl
)

writeLines(slide_table, "../output/did_table_sales_clean.tex")
message("Saved: ../output/did_table_sales_clean.tex")

message("\n=== Summary (Percentage Points) ===")
print(results_df[, c("specification", "estimate_pct", "ci_low_pct", "ci_high_pct", "n_obs")])

message("\nDone!")
