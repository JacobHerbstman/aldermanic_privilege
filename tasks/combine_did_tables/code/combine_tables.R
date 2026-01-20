source("../../setup_environment/code/packages.R")

# =============================================================================
# COMBINE DiD TABLES
# Creates combined panel table and appendix table from coefficient CSVs
# =============================================================================

message("\n=== Combining DiD Tables ===")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

format_coef <- function(est, se, digits = 3) {
    if (is.na(est) | is.na(se)) return("")
    t_stat <- abs(est / se)
    stars <- ifelse(t_stat > 2.576, "***",
             ifelse(t_stat > 1.96, "**",
             ifelse(t_stat > 1.645, "*", "")))
    sprintf("%.*f%s", digits, est, stars)
}

format_se <- function(se, digits = 3) {
    if (is.na(se)) return("")
    sprintf("(%.*f)", digits, se)
}

format_n <- function(n) {
    format(n, big.mark = ",")
}

format_r2 <- function(r2) {
    sprintf("%.2f", r2)
}

# =============================================================================
# LOAD DATA
# =============================================================================

rental_coef <- read_csv("../input/did_coefficients_rental.csv", show_col_types = FALSE)
sales_coef <- read_csv("../input/did_coefficients_sales.csv", show_col_types = FALSE)

message(sprintf("Loaded rental coefficients: %d rows", nrow(rental_coef)))
message(sprintf("Loaded sales coefficients: %d rows", nrow(sales_coef)))

# =============================================================================
# VARIABLE DISPLAY NAMES
# =============================================================================

rental_var_names <- c(
    "post_treat" = "Post $\\times$ Strictness $\\Delta$",
    "log_sqft" = "Log Sqft",
    "log_beds" = "Log Bedrooms",
    "log_baths" = "Log Bathrooms",
    "building_type_factorcondo" = "Condo",
    "building_type_factortownhouse" = "Townhouse",
    "building_type_factorsingle_family" = "Single Family",
    "building_type_factorother" = "Other"
)

sales_var_names <- c(
    "post_treat" = "Post $\\times$ Strictness $\\Delta$",
    "log_sqft" = "Log Building Sqft",
    "log_land_sqft" = "Log Land Sqft",
    "log_building_age" = "Log Building Age",
    "log_bedrooms" = "Log Bedrooms",
    "log_baths" = "Log Bathrooms",
    "has_garage" = "Has Garage"
)

# Variable order
rental_var_order <- c("post_treat", "log_sqft", "log_beds", "log_baths", 
                      "building_type_factorcondo", "building_type_factortownhouse",
                      "building_type_factorsingle_family", "building_type_factorother")

sales_var_order <- c("post_treat", "log_sqft", "log_land_sqft", "log_building_age",
                     "log_bedrooms", "log_baths", "has_garage")

# =============================================================================
# BUILD PANEL A: RENTS
# =============================================================================

build_panel_rows <- function(coef_df, var_order, var_names, col1_est, col1_se, col2_est, col2_se) {
    rows <- ""
    for (var in var_order) {
        row_data <- coef_df[coef_df$variable == var, ]
        if (nrow(row_data) == 0) next
        
        display_name <- var_names[var]
        if (is.na(display_name)) display_name <- var
        
        est1 <- row_data[[col1_est]][1]
        se1 <- row_data[[col1_se]][1]
        est2 <- row_data[[col2_est]][1]
        se2 <- row_data[[col2_se]][1]
        
        rows <- paste0(rows, sprintf("      %s & %s & %s \\\\\n", 
                                     display_name, format_coef(est1, se1), format_coef(est2, se2)))
        rows <- paste0(rows, sprintf("       & %s & %s \\\\\n", 
                                     format_se(se1), format_se(se2)))
    }
    return(rows)
}

# Get N and R2 for rental
rental_n_no_ctrl <- rental_coef$n_obs_no_ctrl[1]
rental_n_ctrl <- rental_coef$n_obs_ctrl[1]
rental_r2_no_ctrl <- rental_coef$r2_no_ctrl[1]
rental_r2_ctrl <- rental_coef$r2_ctrl[1]

panel_a_rows <- build_panel_rows(rental_coef, rental_var_order, rental_var_names,
                                  "estimate_no_ctrl", "se_no_ctrl", "estimate_ctrl", "se_ctrl")

# =============================================================================
# BUILD PANEL B: HOME SALES (2015 Implementation)
# =============================================================================

# Filter to 2015 specifications
sales_2015_no_ctrl <- sales_coef %>% filter(specification == "2015_no_ctrl")
sales_2015_ctrl <- sales_coef %>% filter(specification == "2015_ctrl")

# Reshape to wide format for the function
sales_2015_wide <- sales_2015_no_ctrl %>%
    select(variable, estimate, se) %>%
    rename(estimate_no_ctrl = estimate, se_no_ctrl = se) %>%
    full_join(
        sales_2015_ctrl %>% select(variable, estimate, se) %>% 
            rename(estimate_ctrl = estimate, se_ctrl = se),
        by = "variable"
    )

sales_n_no_ctrl <- sales_2015_no_ctrl$n_obs[1]
sales_n_ctrl <- sales_2015_ctrl$n_obs[1]
sales_r2_no_ctrl <- sales_2015_no_ctrl$r2[1]
sales_r2_ctrl <- sales_2015_ctrl$r2[1]

panel_b_rows <- build_panel_rows(sales_2015_wide, sales_var_order, sales_var_names,
                                  "estimate_no_ctrl", "se_no_ctrl", "estimate_ctrl", "se_ctrl")

# =============================================================================
# BUILD COMBINED TABLE
# =============================================================================

combined_table <- sprintf('
\\begin{table}[htbp]
\\caption{Effect of Alderman Strictness on Housing Costs}
\\label{tab:did_combined}
\\centering

\\textbf{Panel A: Rents}
\\vspace{0.5em}

\\begin{tabular}{lcc}
   \\toprule
                                       & (1)           & (2) \\\\
   \\midrule
%s   \\\\
   Observations                        & %s     & %s \\\\
   R$^2$                               & %s          & %s \\\\
   \\bottomrule
\\end{tabular}

\\vspace{1em}
\\textbf{Panel B: Home Sale Prices}
\\vspace{0.5em}

\\begin{tabular}{lcc}
   \\toprule
                                       & (1)           & (2) \\\\
   \\midrule
%s   \\\\
   Observations                        & %s        & %s \\\\
   R$^2$                               & %s          & %s \\\\
   \\bottomrule
\\end{tabular}

\\vspace{0.5em}
\\par \\raggedright \\footnotesize
\\textit{Notes:} Both panels show regressions of log price on post-redistricting indicator interacted with change in alderman strictness. Panel A: listing-level regressions using stacked 2015 and 2023 redistricting cohorts. Panel B: transaction-level regressions using 2015 implementation timing. All specifications include border-pair $\\times$ origin-side and border-pair $\\times$ year fixed effects. Sample restricted to within 1,000 feet of ward boundaries. Triangular kernel weighting. Standard errors clustered by census block in parentheses. Column (1) excludes hedonic controls; column (2) includes hedonic controls. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.
\\end{table}
',
    panel_a_rows,
    format_n(rental_n_no_ctrl), format_n(rental_n_ctrl),
    format_r2(rental_r2_no_ctrl), format_r2(rental_r2_ctrl),
    panel_b_rows,
    format_n(sales_n_no_ctrl), format_n(sales_n_ctrl),
    format_r2(sales_r2_no_ctrl), format_r2(sales_r2_ctrl)
)

writeLines(combined_table, "../output/did_table_combined.tex")
message("\nSaved: ../output/did_table_combined.tex")

# =============================================================================
# BUILD APPENDIX TABLE: 2012 ANNOUNCEMENT TIMING
# =============================================================================

# Filter to 2012 specifications
sales_2012_no_ctrl <- sales_coef %>% filter(specification == "2012_no_ctrl")
sales_2012_ctrl <- sales_coef %>% filter(specification == "2012_ctrl")

# Reshape to wide format
sales_2012_wide <- sales_2012_no_ctrl %>%
    select(variable, estimate, se) %>%
    rename(estimate_no_ctrl = estimate, se_no_ctrl = se) %>%
    full_join(
        sales_2012_ctrl %>% select(variable, estimate, se) %>% 
            rename(estimate_ctrl = estimate, se_ctrl = se),
        by = "variable"
    )

sales_2012_n_no_ctrl <- sales_2012_no_ctrl$n_obs[1]
sales_2012_n_ctrl <- sales_2012_ctrl$n_obs[1]
sales_2012_r2_no_ctrl <- sales_2012_no_ctrl$r2[1]
sales_2012_r2_ctrl <- sales_2012_ctrl$r2[1]

panel_ann_rows <- build_panel_rows(sales_2012_wide, sales_var_order, sales_var_names,
                                    "estimate_no_ctrl", "se_no_ctrl", "estimate_ctrl", "se_ctrl")

appendix_table <- sprintf('
\\begin{table}[htbp]
\\caption{Effect of Alderman Strictness on Home Sale Prices: Announcement Timing}
\\label{tab:did_sales_announcement}
\\centering

\\begin{tabular}{lcc}
   \\toprule
                                       & (1)           & (2) \\\\
   \\midrule
%s   \\\\
   Observations                        & %s        & %s \\\\
   R$^2$                               & %s          & %s \\\\
   \\bottomrule
\\end{tabular}

\\vspace{0.5em}
\\par \\raggedright \\footnotesize
\\textit{Notes:} Transaction-level regressions of log sale price on post-redistricting indicator interacted with change in alderman strictness. Uses 2012 announcement timing as robustness check (main results use 2015 implementation timing). All specifications include border-pair $\\times$ origin-side and border-pair $\\times$ year fixed effects. Sample restricted to within 1,000 feet of ward boundaries. Triangular kernel weighting. Standard errors clustered by census block in parentheses. Column (1) excludes hedonic controls; column (2) includes hedonic controls. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.
\\end{table}
',
    panel_ann_rows,
    format_n(sales_2012_n_no_ctrl), format_n(sales_2012_n_ctrl),
    format_r2(sales_2012_r2_no_ctrl), format_r2(sales_2012_r2_ctrl)
)

writeLines(appendix_table, "../output/did_table_sales_announcement.tex")
message("Saved: ../output/did_table_sales_announcement.tex")

# =============================================================================
# BUILD SIMPLE 4-COLUMN TABLE (no individual hedonic coefficients)
# =============================================================================

# Get treatment coefficients
rental_treat <- rental_coef[rental_coef$variable == "post_treat", ]
sales_treat <- sales_2015_wide[sales_2015_wide$variable == "post_treat", ]

simple_table <- sprintf('
\\begin{table}[htbp]
\\caption{Effect of Alderman Strictness on Housing Costs}
\\label{tab:did_simple}
\\centering
\\begin{tabular}{l*{4}{c}}
\\toprule
 & \\multicolumn{2}{c}{Rents} & \\multicolumn{2}{c}{Home Sale Prices} \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}
 & (1) & (2) & (3) & (4) \\\\
\\midrule
Post $\\times$ Strictness $\\Delta$ & %s & %s & %s & %s \\\\
 & %s & %s & %s & %s \\\\
\\midrule
Observations & %s & %s & %s & %s \\\\
R$^2$ & %s & %s & %s & %s \\\\
Hedonic Controls & & $\\checkmark$ & & $\\checkmark$ \\\\
Border Pair $\\times$ Side FE & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\
Border Pair $\\times$ Year FE & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\
\\bottomrule
\\end{tabular}
\\figurenotes{Columns (1)--(2): listing-level regressions of log rent on post-redistricting indicator interacted with change in alderman strictness; stacked estimator combining 2015 and 2023 redistricting cohorts. Columns (3)--(4): transaction-level regressions of log sale price using 2015 implementation timing. Rental hedonic controls include log sqft, log bedrooms, log bathrooms, and building type. Sales hedonic controls include log building sqft, log land sqft, log building age, log bedrooms, log bathrooms, and garage indicator. Sample restricted to observations within 1,000 feet of ward boundaries. Triangular kernel weights. Standard errors clustered by census block in parentheses. $^{*}$ $p<0.10$, $^{**}$ $p<0.05$, $^{***}$ $p<0.01$.}
\\end{table}
',
    format_coef(rental_treat$estimate_no_ctrl, rental_treat$se_no_ctrl),
    format_coef(rental_treat$estimate_ctrl, rental_treat$se_ctrl),
    format_coef(sales_treat$estimate_no_ctrl, sales_treat$se_no_ctrl),
    format_coef(sales_treat$estimate_ctrl, sales_treat$se_ctrl),
    format_se(rental_treat$se_no_ctrl),
    format_se(rental_treat$se_ctrl),
    format_se(sales_treat$se_no_ctrl),
    format_se(sales_treat$se_ctrl),
    format_n(rental_n_no_ctrl), format_n(rental_n_ctrl),
    format_n(sales_n_no_ctrl), format_n(sales_n_ctrl),
    format_r2(rental_r2_no_ctrl), format_r2(rental_r2_ctrl),
    format_r2(sales_r2_no_ctrl), format_r2(sales_r2_ctrl)
)

writeLines(simple_table, "../output/did_table_simple.tex")
message("Saved: ../output/did_table_simple.tex")

message("\nDone!")
