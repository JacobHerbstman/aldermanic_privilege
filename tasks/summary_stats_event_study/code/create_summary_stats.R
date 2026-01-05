# create_summary_stats.R
# Summary statistics for event study samples (sales and rentals)
# Generates publication-ready LaTeX table

source("../../setup_environment/code/packages.R")

# =============================================================================
# LOAD DATA
# =============================================================================

message("Loading data...")

sales <- read_csv("../input/sales_stacked_panel.csv", show_col_types = FALSE)
rental <- read_csv("../input/rental_stacked_panel.csv", show_col_types = FALSE)

message(sprintf("Sales: %s rows", format(nrow(sales), big.mark = ",")))
message(sprintf("Rental: %s rows", format(nrow(rental), big.mark = ",")))

# =============================================================================
# APPLY SAMPLE RESTRICTIONS
# =============================================================================

sales_sample <- sales %>%
    filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000)

rental_sample <- rental %>%
    filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000)

message(sprintf("Sales after restrictions: %s rows", format(nrow(sales_sample), big.mark = ",")))
message(sprintf("Rental after restrictions: %s rows", format(nrow(rental_sample), big.mark = ",")))

# =============================================================================
# CREATE TREATMENT GROUP VARIABLE
# =============================================================================

# Use cohort_block_id as the unit of observation (blocks can appear in both cohorts)
sales_sample <- sales_sample %>%
    mutate(treatment_group = case_when(
        strictness_change > 0 ~ "To Stricter",
        strictness_change < 0 ~ "To Lenient",
        TRUE ~ "Control"
    ))

rental_sample <- rental_sample %>%
    mutate(treatment_group = case_when(
        strictness_change > 0 ~ "To Stricter",
        strictness_change < 0 ~ "To Lenient",
        TRUE ~ "Control"
    ))

# =============================================================================
# FUNCTION TO COMPUTE STATISTICS
# =============================================================================

# NOTE: We count cohort_block_id (not block_id) because blocks can appear in 
# both 2015 and 2023 cohorts with potentially different treatment statuses.
# This ensures columns add up correctly.

compute_stats_sales <- function(data, group_name = "Full Sample") {
    # For price stats, filter to block-years with sales
    data_with_sales <- data %>% filter(n_sales > 0)

    tibble(
        group = group_name,
        # Count cohort-block pairs so columns add up
        n_blocks = n_distinct(data$cohort_block_id),
        n_block_years = nrow(data),
        total_sales = sum(data$n_sales, na.rm = TRUE),
        mean_price = sum(data_with_sales$mean_price * data_with_sales$n_sales, na.rm = TRUE) /
            sum(data_with_sales$n_sales, na.rm = TRUE),
        median_price = median(data_with_sales$median_price, na.rm = TRUE),
        mean_strictness = if (group_name == "Full Sample") NA_real_ else mean(data$strictness_change, na.rm = TRUE),
        mean_dist = mean(data$mean_dist_to_boundary, na.rm = TRUE),
        mean_homeownership = mean(data$homeownership_rate, na.rm = TRUE),
        mean_share_white = mean(data$share_white, na.rm = TRUE),
        mean_share_bach = mean(data$share_bach_plus, na.rm = TRUE),
        mean_income = mean(data$median_hh_income_1000s, na.rm = TRUE)
    )
}

compute_stats_rental <- function(data, group_name = "Full Sample") {
    # For rent stats, filter to block-years with listings
    data_with_listings <- data %>% filter(n_listings > 0)

    tibble(
        group = group_name,
        # Count cohort-block pairs so columns add up
        n_blocks = n_distinct(data$cohort_block_id),
        n_block_years = nrow(data),
        total_listings = sum(data$n_listings, na.rm = TRUE),
        mean_rent = sum(data_with_listings$mean_rent * data_with_listings$n_listings, na.rm = TRUE) /
            sum(data_with_listings$n_listings, na.rm = TRUE),
        median_rent = median(data_with_listings$median_rent, na.rm = TRUE),
        mean_strictness = if (group_name == "Full Sample") NA_real_ else mean(data$strictness_change, na.rm = TRUE),
        mean_dist = mean(data$mean_dist_to_boundary, na.rm = TRUE),
        mean_homeownership = mean(data$homeownership_rate, na.rm = TRUE),
        mean_share_white = mean(data$share_white, na.rm = TRUE),
        mean_share_bach = mean(data$share_bach_plus, na.rm = TRUE),
        mean_income = mean(data$median_hh_income_1000s, na.rm = TRUE)
    )
}

# =============================================================================
# COMPUTE STATISTICS
# =============================================================================

message("Computing statistics...")

# Sales statistics
sales_full <- compute_stats_sales(sales_sample, "Full Sample")
sales_control <- compute_stats_sales(sales_sample %>% filter(treatment_group == "Control"), "Control")
sales_stricter <- compute_stats_sales(sales_sample %>% filter(treatment_group == "To Stricter"), "To Stricter")
sales_lenient <- compute_stats_sales(sales_sample %>% filter(treatment_group == "To Lenient"), "To Lenient")

sales_stats <- bind_rows(sales_full, sales_control, sales_stricter, sales_lenient)

# Verify counts add up
stopifnot(
    "Block counts don't add up!" = 
        sales_stats$n_blocks[1] == sum(sales_stats$n_blocks[2:4])
)
message("✓ Block counts verified: Full Sample = Control + To Stricter + To Lenient")

# Rental statistics
rental_full <- compute_stats_rental(rental_sample, "Full Sample")
rental_control <- compute_stats_rental(rental_sample %>% filter(treatment_group == "Control"), "Control")
rental_stricter <- compute_stats_rental(rental_sample %>% filter(treatment_group == "To Stricter"), "To Stricter")
rental_lenient <- compute_stats_rental(rental_sample %>% filter(treatment_group == "To Lenient"), "To Lenient")

rental_stats <- bind_rows(rental_full, rental_control, rental_stricter, rental_lenient)

# Verify counts add up
stopifnot(
    "Block counts don't add up!" = 
        rental_stats$n_blocks[1] == sum(rental_stats$n_blocks[2:4])
)
message("✓ Rental block counts verified")

# =============================================================================
# FORMATTING FUNCTIONS
# =============================================================================

fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
fmt_dollar <- function(x) paste0("\\$", format(round(x), big.mark = ",", scientific = FALSE))
fmt_dec3 <- function(x) sprintf("%.3f", x)
fmt_dec1 <- function(x) sprintf("%.1f", x)
fmt_strictness <- function(x) {
    if (is.na(x)) {
        return("---")
    }
    sprintf("%.3f", x)
}

# =============================================================================
# CREATE LATEX TABLE
# =============================================================================

message("Creating LaTeX table...")

# Helper to create a row
make_row <- function(label, values, format_fn = fmt_int) {
    formatted <- sapply(values, format_fn)
    paste0(label, " & ", paste(formatted, collapse = " & "), " \\\\")
}

# Build table content
tex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Summary Statistics: Event Study Samples}",
    "\\label{tab:summary_stats}",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    " & Full Sample & Control & To Stricter & To Lenient \\\\",
    "\\midrule",
    "\\addlinespace",
    "\\multicolumn{5}{l}{\\textit{Panel A: Home Sales}} \\\\",
    "\\addlinespace"
)

# Panel A rows
tex_lines <- c(
    tex_lines,
    make_row("Block-cohort pairs", sales_stats$n_blocks, fmt_int),
    make_row("Block-years", sales_stats$n_block_years, fmt_int),
    make_row("Total sales", sales_stats$total_sales, fmt_int),
    "\\addlinespace",
    make_row("Mean sale price (\\$)", sales_stats$mean_price, fmt_dollar),
    make_row("Median sale price (\\$)", sales_stats$median_price, fmt_dollar),
    make_row("Mean $\\Delta$ strictness", sales_stats$mean_strictness, fmt_strictness),
    make_row("Mean dist. to boundary (ft)", sales_stats$mean_dist, fmt_int),
    "\\addlinespace",
    make_row("Mean homeownership rate", sales_stats$mean_homeownership, fmt_dec3),
    make_row("Mean share white", sales_stats$mean_share_white, fmt_dec3),
    make_row("Mean share bachelor's+", sales_stats$mean_share_bach, fmt_dec3),
    make_row("Mean median HH income (\\$000s)", sales_stats$mean_income, fmt_dec1)
)

tex_lines <- c(
    tex_lines,
    "\\addlinespace",
    "\\midrule",
    "\\addlinespace",
    "\\multicolumn{5}{l}{\\textit{Panel B: Rental Listings}} \\\\",
    "\\addlinespace"
)

# Panel B rows
tex_lines <- c(
    tex_lines,
    make_row("Block-cohort pairs", rental_stats$n_blocks, fmt_int),
    make_row("Block-years", rental_stats$n_block_years, fmt_int),
    make_row("Total listings", rental_stats$total_listings, fmt_int),
    "\\addlinespace",
    make_row("Mean rent (\\$)", rental_stats$mean_rent, fmt_dollar),
    make_row("Median rent (\\$)", rental_stats$median_rent, fmt_dollar),
    make_row("Mean $\\Delta$ strictness", rental_stats$mean_strictness, fmt_strictness),
    make_row("Mean dist. to boundary (ft)", rental_stats$mean_dist, fmt_int),
    "\\addlinespace",
    make_row("Mean homeownership rate", rental_stats$mean_homeownership, fmt_dec3),
    make_row("Mean share white", rental_stats$mean_share_white, fmt_dec3),
    make_row("Mean share bachelor's+", rental_stats$mean_share_bach, fmt_dec3),
    make_row("Mean median HH income (\\$000s)", rental_stats$mean_income, fmt_dec1)
)

tex_lines <- c(
    tex_lines,
    "\\addlinespace",
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}",
    "\\small",
    "\\item \\textit{Notes:} Sample restricted to census blocks within 1,000 feet of ward boundaries.",
    "Pools 2015 and 2023 redistricting cohorts; blocks may appear in both cohorts.",
    "Control blocks did not switch aldermen during redistricting.",
    "Strictness is standardized (mean 0, SD 1) based on permit processing times.",
    "Prices and rents are transaction-weighted means.",
    "\\end{tablenotes}",
    "\\end{table}"
)

# Write LaTeX file
writeLines(tex_lines, "../output/summary_stats_event_study.tex")
message("Saved: ../output/summary_stats_event_study.tex")

# =============================================================================
# CREATE CSV OUTPUT
# =============================================================================

# Combine all stats for CSV
csv_output <- bind_rows(
    sales_stats %>% mutate(panel = "Sales"),
    rental_stats %>% mutate(panel = "Rental")
) %>%
    select(panel, group, everything())

write_csv(csv_output, "../output/summary_stats_event_study.csv")
message("Saved: ../output/summary_stats_event_study.csv")

message("\nDone!")
