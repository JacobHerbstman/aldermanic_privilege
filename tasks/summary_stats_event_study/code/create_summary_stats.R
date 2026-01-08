# create_summary_stats.R
# Summary statistics for event study samples (sales and rentals)
# Updated for transaction/listing-level analysis
# - Sales: 2012 cohort (announcement timing)
# - Rentals: Stacked 2015+2023 (implementation timing)

source("../../setup_environment/code/packages.R")

# =============================================================================
# LOAD DATA
# =============================================================================
message("Loading data...")

# Sales: 2012 cohort (announcement timing for 2015 redistricting)
sales <- read_parquet("../input/sales_transaction_panel_2012.parquet")
setDT(sales)
message(sprintf("Sales (2012 cohort): %s transactions", format(nrow(sales), big.mark = ",")))

# Rentals: Stacked 2015+2023 (implementation timing)
rental <- read_parquet("../input/rental_listing_panel.parquet")
setDT(rental)
message(sprintf("Rentals (stacked): %s listings", format(nrow(rental), big.mark = ",")))

# =============================================================================
# APPLY SAMPLE RESTRICTIONS (match regression sample)
# =============================================================================
message("\nApplying sample restrictions...")

# Sales: 1000ft bandwidth
sales_sample <- sales[dist_ft <= 1000]
message(sprintf("Sales after 1000ft restriction: %s transactions", format(nrow(sales_sample), big.mark = ",")))

# Rentals: 1000ft bandwidth + complete hedonics (to match regression sample)
rental_sample <- rental[dist_ft <= 1000]

# Restrict to complete hedonic sample for comparability
hedonic_vars <- c("log_sqft", "log_beds", "log_baths", "building_type_clean")
rental_sample <- rental_sample[complete.cases(rental_sample[, ..hedonic_vars])]
message(sprintf("Rentals after 1000ft + complete hedonics: %s listings", format(nrow(rental_sample), big.mark = ",")))

# =============================================================================
# CREATE TREATMENT GROUP VARIABLE
# =============================================================================
message("\nCreating treatment groups...")

sales_sample[, treatment_group := fcase(
    strictness_change > 0, "To Stricter",
    strictness_change < 0, "To Lenient",
    default = "Control"
)]

rental_sample[, treatment_group := fcase(
    strictness_change > 0, "To Stricter",
    strictness_change < 0, "To Lenient",
    default = "Control"
)]

# Verify
message("Sales treatment distribution:")
print(sales_sample[, .N, by = treatment_group])

message("\nRental treatment distribution:")
print(rental_sample[, .N, by = treatment_group])

# =============================================================================
# COMPUTE STATISTICS: SALES
# =============================================================================
message("\nComputing sales statistics...")

compute_sales_stats <- function(data, group_name = "Full Sample") {
    list(
        group = group_name,
        # Sample size
        n_transactions = nrow(data),
        n_blocks = uniqueN(data$block_id),
        n_ward_pairs = uniqueN(data$ward_pair_id),
        # Outcome
        mean_price = mean(data$sale_price, na.rm = TRUE),
        median_price = median(data$sale_price, na.rm = TRUE),
        # Treatment
        mean_strictness = if (group_name == "Full Sample") NA_real_ else mean(data$strictness_change, na.rm = TRUE),
        mean_dist = mean(data$dist_ft, na.rm = TRUE),
        # Hedonics (use raw values, not logs)
        mean_sqft = mean(data$building_sqft, na.rm = TRUE),
        mean_age = mean(data$building_age, na.rm = TRUE),
        mean_bedrooms = mean(data$num_bedrooms, na.rm = TRUE)
    )
}

sales_full <- compute_sales_stats(sales_sample, "Full Sample")
sales_control <- compute_sales_stats(sales_sample[treatment_group == "Control"], "Control")
sales_stricter <- compute_sales_stats(sales_sample[treatment_group == "To Stricter"], "To Stricter")
sales_lenient <- compute_sales_stats(sales_sample[treatment_group == "To Lenient"], "To Lenient")

sales_stats <- rbindlist(list(sales_full, sales_control, sales_stricter, sales_lenient))

# Verify counts add up
total_from_groups <- sales_stats[group == "Control", n_transactions] +
    sales_stats[group == "To Stricter", n_transactions] +
    sales_stats[group == "To Lenient", n_transactions]
stopifnot(sales_stats[group == "Full Sample", n_transactions] == total_from_groups)
message("✓ Sales transaction counts verified")

# =============================================================================
# COMPUTE STATISTICS: RENTALS
# =============================================================================
message("\nComputing rental statistics...")

compute_rental_stats <- function(data, group_name = "Full Sample") {
    # Building type shares
    bt_table <- data[, .N, by = building_type_clean]
    bt_table[, share := N / sum(N)]

    get_share <- function(type) {
        val <- bt_table[building_type_clean == type, share]
        if (length(val) == 0) 0 else val
    }

    list(
        group = group_name,
        # Sample size
        n_listings = nrow(data),
        n_blocks = uniqueN(data$block_id),
        n_ward_pairs = uniqueN(data$ward_pair_id),
        # Outcome
        mean_rent = mean(data$rent_price, na.rm = TRUE),
        median_rent = median(data$rent_price, na.rm = TRUE),
        # Treatment
        mean_strictness = if (group_name == "Full Sample") NA_real_ else mean(data$strictness_change, na.rm = TRUE),
        mean_dist = mean(data$dist_ft, na.rm = TRUE),
        # Hedonics (use raw values)
        mean_sqft = mean(data$sqft, na.rm = TRUE),
        mean_bedrooms = mean(data$beds, na.rm = TRUE),
        # Building type distribution
        share_single_family = get_share("single_family"),
        share_multi_family = get_share("multi_family"),
        share_condo = get_share("condo")
    )
}

rental_full <- compute_rental_stats(rental_sample, "Full Sample")
rental_control <- compute_rental_stats(rental_sample[treatment_group == "Control"], "Control")
rental_stricter <- compute_rental_stats(rental_sample[treatment_group == "To Stricter"], "To Stricter")
rental_lenient <- compute_rental_stats(rental_sample[treatment_group == "To Lenient"], "To Lenient")

rental_stats <- rbindlist(list(rental_full, rental_control, rental_stricter, rental_lenient))

# Verify counts add up
total_from_groups <- rental_stats[group == "Control", n_listings] +
    rental_stats[group == "To Stricter", n_listings] +
    rental_stats[group == "To Lenient", n_listings]
stopifnot(rental_stats[group == "Full Sample", n_listings] == total_from_groups)
message("✓ Rental listing counts verified")

# =============================================================================
# FORMATTING FUNCTIONS
# =============================================================================
fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
fmt_dollar <- function(x) paste0("\\$", format(round(x), big.mark = ",", scientific = FALSE))
fmt_dec1 <- function(x) sprintf("%.1f", x)
fmt_dec2 <- function(x) sprintf("%.2f", x)
fmt_pct <- function(x) sprintf("%.1f", x * 100)
fmt_strictness <- function(x) {
    if (is.na(x)) {
        return("---")
    }
    sprintf("%.3f", x)
}

# =============================================================================
# CREATE LATEX TABLE
# =============================================================================
message("\nCreating LaTeX table...")

# Helper to create a row
make_row <- function(label, values, format_fn = fmt_int) {
    formatted <- sapply(values, format_fn)
    paste0(label, " & ", paste(formatted, collapse = " & "), " \\\\")
}

tex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Summary Statistics: Event Study Samples}",
    "\\label{tab:summary_stats_event_study}",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    " & Full Sample & Control & To Stricter & To Lenient \\\\",
    "\\midrule",
    "\\addlinespace",
    "\\multicolumn{5}{l}{\\textit{Panel A: Home Sales (2012 Announcement Cohort)}} \\\\",
    "\\addlinespace"
)

# Panel A: Sales
tex_lines <- c(
    tex_lines,
    make_row("Transactions", sales_stats$n_transactions, fmt_int),
    make_row("Census blocks", sales_stats$n_blocks, fmt_int),
    make_row("Ward boundary pairs", sales_stats$n_ward_pairs, fmt_int),
    "\\addlinespace",
    make_row("Mean sale price (\\$)", sales_stats$mean_price, fmt_dollar),
    make_row("Median sale price (\\$)", sales_stats$median_price, fmt_dollar),
    make_row("Mean $\\Delta$ strictness", sales_stats$mean_strictness, fmt_strictness),
    make_row("Mean dist.\\ to boundary (ft)", sales_stats$mean_dist, fmt_int),
    "\\addlinespace",
    make_row("Mean building sqft", sales_stats$mean_sqft, fmt_int),
    make_row("Mean building age (years)", sales_stats$mean_age, fmt_dec1),
    make_row("Mean bedrooms", sales_stats$mean_bedrooms, fmt_dec2)
)

tex_lines <- c(
    tex_lines,
    "\\addlinespace",
    "\\midrule",
    "\\addlinespace",
    "\\multicolumn{5}{l}{\\textit{Panel B: Rental Listings (Stacked 2015 + 2023 Cohorts)}} \\\\",
    "\\addlinespace"
)

# Panel B: Rentals
tex_lines <- c(
    tex_lines,
    make_row("Listings", rental_stats$n_listings, fmt_int),
    make_row("Census blocks", rental_stats$n_blocks, fmt_int),
    make_row("Ward boundary pairs", rental_stats$n_ward_pairs, fmt_int),
    "\\addlinespace",
    make_row("Mean rent (\\$)", rental_stats$mean_rent, fmt_dollar),
    make_row("Median rent (\\$)", rental_stats$median_rent, fmt_dollar),
    make_row("Mean $\\Delta$ strictness", rental_stats$mean_strictness, fmt_strictness),
    make_row("Mean dist.\\ to boundary (ft)", rental_stats$mean_dist, fmt_int),
    "\\addlinespace",
    make_row("Mean sqft", rental_stats$mean_sqft, fmt_int),
    make_row("Mean bedrooms", rental_stats$mean_bedrooms, fmt_dec2),
    make_row("Share single-family (\\%)", rental_stats$share_single_family, fmt_pct),
    make_row("Share multi-family (\\%)", rental_stats$share_multi_family, fmt_pct),
    make_row("Share condo (\\%)", rental_stats$share_condo, fmt_pct)
)

# Close table
tex_lines <- c(
    tex_lines,
    "\\addlinespace",
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}",
    "\\small",
    "\\item \\textit{Notes:} Sample restricted to transactions/listings within 1,000 feet of ward boundaries.",
    "Panel A uses the 2012 announcement cohort for the 2015 redistricting (sales 2007--2017).",
    "Panel B pools the 2015 and 2023 redistricting cohorts using implementation timing;",
    "rental sample restricted to listings with non-missing hedonic characteristics.",
    "Control units did not switch aldermen during redistricting.",
    "Strictness is standardized (mean 0, SD 1) based on permit processing times.",
    "\\end{tablenotes}",
    "\\end{table}"
)

# Write LaTeX file
writeLines(tex_lines, "../output/summary_stats_event_study.tex")
message("Saved: ../output/summary_stats_event_study.tex")

# =============================================================================
# SAVE CSV FOR INSPECTION
# =============================================================================
csv_sales <- as.data.frame(sales_stats)
csv_sales$panel <- "Sales (2012)"

csv_rental <- as.data.frame(rental_stats)
csv_rental$panel <- "Rental (2015+2023)"

csv_output <- bind_rows(csv_sales, csv_rental) %>%
    select(panel, group, everything())

write_csv(csv_output, "../output/summary_stats_event_study.csv")
message("Saved: ../output/summary_stats_event_study.csv")

# =============================================================================
# PRINT SUMMARY
# =============================================================================
message("\n=== SUMMARY ===")

message("\nPanel A: Home Sales (2012 Cohort)")
message(sprintf("  Total transactions: %s", format(sales_stats[group == "Full Sample", n_transactions], big.mark = ",")))
message(sprintf(
    "  Control: %s | To Stricter: %s | To Lenient: %s",
    format(sales_stats[group == "Control", n_transactions], big.mark = ","),
    format(sales_stats[group == "To Stricter", n_transactions], big.mark = ","),
    format(sales_stats[group == "To Lenient", n_transactions], big.mark = ",")
))

message("\nPanel B: Rental Listings (Stacked 2015+2023)")
message(sprintf("  Total listings: %s", format(rental_stats[group == "Full Sample", n_listings], big.mark = ",")))
message(sprintf(
    "  Control: %s | To Stricter: %s | To Lenient: %s",
    format(rental_stats[group == "Control", n_listings], big.mark = ","),
    format(rental_stats[group == "To Stricter", n_listings], big.mark = ","),
    format(rental_stats[group == "To Lenient", n_listings], big.mark = ",")
))

message("\nDone!")
