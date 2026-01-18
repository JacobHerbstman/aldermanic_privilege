# run_did_rental_disaggregate.R
# Pooled difference-in-differences for rentals
# Uses stacked 2015+2023 implementation timing (data constraint)
# Produces a clean, publication-ready table

source("../../setup_environment/code/packages.R")

# =============================================================================
# CONFIGURATION
# =============================================================================
BANDWIDTH <- 1000
WEIGHTING <- "uniform"

message("\n=== Pooled DiD: Rentals ===")
message(sprintf("Bandwidth: %d ft", BANDWIDTH))
message(sprintf("Weighting: %s", WEIGHTING))

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

# Hedonic controls (matching the event study script)
hedonic_vars <- "+ building_type_factor + log_sqft + log_beds + log_baths"

# --- Without Controls ---
message("\n--- Without Hedonic Controls ---")

m_no_ctrl <- feols(
    log(rent_price) ~ post_treat | cohort_ward_pair_side + cohort^year,
    data = data,
    weights = ~weight,
    cluster = ~cohort_block_id
)
message(sprintf("No controls: N = %s", format(m_no_ctrl$nobs, big.mark = ",")))

# --- With Controls ---
message("\n--- With Hedonic Controls ---")

m_ctrl <- feols(
    as.formula(paste("log(rent_price) ~ post_treat", hedonic_vars, "| cohort_ward_pair_side + cohort^year")),
    data = data,
    weights = ~weight,
    cluster = ~cohort_block_id
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
        "_Border Pair $\\times$ Side FE" = c("$\\checkmark$", "$\\checkmark$"),
        "_Cohort $\\times$ Year FE" = c("$\\checkmark$", "$\\checkmark$")
    ),
    se.below = TRUE,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    notes = "Listing-level regressions of log rent on post-redistricting indicator interacted with change in alderman strictness. Stacked estimator combining 2015 and 2023 redistricting events (implementation timing). Sample restricted to listings within 1,000 feet of ward boundaries with non-missing hedonic characteristics. Standard errors clustered by census block in parentheses.",
    label = "tab:did_rental",
    float = TRUE,
    file = "../output/did_table_rental.tex",
    replace = TRUE
)

message("Saved: ../output/did_table_rental.tex")

# =============================================================================
# ALSO SAVE CSV FOR QUICK INSPECTION
# =============================================================================
results_df <- data.frame(
    specification = c("No controls", "With controls"),
    estimate = c(coef(m_no_ctrl)["post_treat"], coef(m_ctrl)["post_treat"]),
    std_error = c(se(m_no_ctrl)["post_treat"], se(m_ctrl)["post_treat"]),
    n_obs = c(m_no_ctrl$nobs, m_ctrl$nobs)
)
results_df$estimate_pct <- results_df$estimate * 100
results_df$ci_low_pct <- (results_df$estimate - 1.96 * results_df$std_error) * 100
results_df$ci_high_pct <- (results_df$estimate + 1.96 * results_df$std_error) * 100

write_csv(results_df, "../output/did_table_rental.csv")
message("Saved: ../output/did_table_rental.csv")

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
r2_no_ctrl <- fitstat(m_no_ctrl, "r2")$r2
r2_ctrl <- fitstat(m_ctrl, "r2")$r2

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
    format_n_millions(n_obs),
    format_n_millions(n_obs),
    r2_no_ctrl,
    r2_ctrl
)

writeLines(slide_table, "../output/did_table_rental_clean.tex")
message("Saved: ../output/did_table_rental_clean.tex")

message("\n=== Summary (Percentage Points) ===")
print(results_df[, c("specification", "estimate_pct", "ci_low_pct", "ci_high_pct", "n_obs")])

message("\nDone!")
