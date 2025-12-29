# run_event_study.R
# Modular event study for home sales around ward redistricting
# Usage: Rscript run_event_study.R --frequency=yearly --treatment_date=2015 --weighted=TRUE --stacked=TRUE --treatment_type=continuous

source("../../setup_environment/code/packages.R")

# =============================================================================
# COMMAND LINE ARGUMENTS
# =============================================================================
parser <- OptionParser()
parser <- add_option(parser, c("-f", "--frequency"),
    type = "character", default = "yearly",
    help = "Analysis frequency: yearly, quarterly, monthly [default: yearly]"
)
parser <- add_option(parser, c("-t", "--treatment_date"),
    type = "integer", default = 2015,
    help = "Treatment year: 2012 or 2015 [default: 2015]"
)
parser <- add_option(parser, c("-w", "--weighted"),
    type = "logical", default = TRUE,
    help = "Weight by number of sales [default: TRUE]"
)
parser <- add_option(parser, c("-s", "--stacked"),
    type = "logical", default = TRUE,
    help = "Use stacked design with 2015+2023 cohorts [default: TRUE]"
)
parser <- add_option(parser, c("-x", "--treatment_type"),
    type = "character", default = "continuous",
    help = "Treatment type: continuous or binary_direction [default: continuous]"
)

args <- parse_args(parser)

FREQUENCY <- args$frequency
TREATMENT_YEAR <- args$treatment_date
WEIGHTED <- args$weighted
STACKED <- args$stacked
TREATMENT_TYPE <- args$treatment_type

message(sprintf("\n=== Event Study Configuration ==="))
message(sprintf("Frequency: %s", FREQUENCY))
message(sprintf("Treatment Date: %d", TREATMENT_YEAR))
message(sprintf("Weighted: %s", WEIGHTED))
message(sprintf("Stacked: %s", STACKED))
message(sprintf("Treatment Type: %s", TREATMENT_TYPE))

# Output suffix
suffix <- sprintf(
    "%s_%d_%s_%s_%s", FREQUENCY, TREATMENT_YEAR,
    ifelse(WEIGHTED, "weighted", "unweighted"),
    ifelse(STACKED, "stacked", "unstacked"),
    TREATMENT_TYPE
)

# =============================================================================
# LOAD DATA
# =============================================================================
message("\nLoading data...")

if (FREQUENCY == "yearly") {
    if (STACKED) {
        data <- read_csv("../input/sales_stacked_panel.csv", show_col_types = FALSE) %>%
            filter(n_sales > 0, !is.na(strictness_change)) %>%
            # Restrict to blocks near ward boundaries (within 2000 ft)
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 2000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0.1),
                treat_lenient = as.integer(strictness_change < -0.1),
                relative_time = case_when(
                    TREATMENT_YEAR == 2015 ~ relative_year,
                    TREATMENT_YEAR == 2012 ~ relative_year + 3
                ),
                relative_time_capped = pmax(pmin(relative_time, 5), -5)
            )
        # Border-pair × cohort FE: compare within ward boundary pairs
        fe_formula <- "cohort_ward_pair + cohort^year"
        cluster_var <- "cohort_block_id"
    } else {
        data <- read_csv("../input/sales_block_year_panel.csv", show_col_types = FALSE) %>%
            filter(n_sales > 0, !is.na(strictness_change), year >= 2010, year <= 2020) %>%
            # Restrict to blocks near ward boundaries (within 2000 ft)
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 2000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0.1),
                treat_lenient = as.integer(strictness_change < -0.1),
                relative_time = case_when(
                    TREATMENT_YEAR == 2015 ~ relative_year,
                    TREATMENT_YEAR == 2012 ~ year - 2012
                ),
                relative_time_capped = pmax(pmin(relative_time, 5), -5)
            )
        # Border-pair FE: compare within ward boundary pairs
        fe_formula <- "ward_pair_id + year"
        cluster_var <- "block_id"
    }
    time_var <- "relative_time_capped"
    time_label <- "Years"
} else if (FREQUENCY == "quarterly") {
    if (STACKED) {
        data <- read_csv("../input/sales_stacked_quarterly_panel.csv", show_col_types = FALSE) %>%
            filter(n_sales > 0, !is.na(strictness_change)) %>%
            # Restrict to blocks near ward boundaries (within 2000 ft)
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 2000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0.1),
                treat_lenient = as.integer(strictness_change < -0.1),
                relative_time = relative_quarter,
                relative_time_capped = pmax(pmin(relative_quarter, 12), -12)
            )
        # Border-pair × cohort FE + quarter seasonality
        fe_formula <- "cohort_ward_pair + cohort^year + quarter"
        cluster_var <- "cohort_block_id"
    } else {
        data <- read_csv("../input/sales_block_month_panel.csv", show_col_types = FALSE) %>%
            filter(!is.na(switched_2015), !is.na(strictness_change_2015)) %>%
            # Restrict to blocks near ward boundaries (within 2000 ft)
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 2000) %>%
            mutate(
                treatment_continuous = strictness_change_2015,
                treat_stricter = as.integer(strictness_change_2015 > 0.1),
                treat_lenient = as.integer(strictness_change_2015 < -0.1),
                relative_month = case_when(
                    TREATMENT_YEAR == 2015 ~ (year - 2015) * 12 + (month - 5),
                    TREATMENT_YEAR == 2012 ~ (year - 2012) * 12 + (month - 4)
                ),
                relative_quarter = floor(relative_month / 3),
                relative_time_capped = pmax(pmin(relative_quarter, 12), -12),
                year_month = paste(year, sprintf("%02d", month), sep = "-")
            )
        # Border-pair FE: compare within ward boundary pairs
        fe_formula <- "ward_pair_id + year_month"
        cluster_var <- "block_id"
    }
    time_var <- "relative_time_capped"
    time_label <- "Quarters"
} else if (FREQUENCY == "monthly") {
    if (STACKED) {
        data <- read_csv("../input/sales_stacked_monthly_panel.csv", show_col_types = FALSE) %>%
            filter(n_sales > 0, !is.na(strictness_change)) %>%
            # Restrict to blocks near ward boundaries (within 2000 ft)
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 2000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0.1),
                treat_lenient = as.integer(strictness_change < -0.1),
                relative_time = relative_month,
                relative_time_capped = pmax(pmin(relative_month, 36), -36)
            )
        # Border-pair × cohort FE + month seasonality
        fe_formula <- "cohort_ward_pair + cohort^year + month"
        cluster_var <- "cohort_block_id"
    } else {
        data <- read_csv("../input/sales_block_month_panel.csv", show_col_types = FALSE) %>%
            filter(!is.na(switched_2015), !is.na(strictness_change_2015)) %>%
            # Restrict to blocks near ward boundaries (within 2000 ft)
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 2000) %>%
            mutate(
                treatment_continuous = strictness_change_2015,
                treat_stricter = as.integer(strictness_change_2015 > 0.1),
                treat_lenient = as.integer(strictness_change_2015 < -0.1),
                relative_month = case_when(
                    TREATMENT_YEAR == 2015 ~ (year - 2015) * 12 + (month - 5),
                    TREATMENT_YEAR == 2012 ~ (year - 2012) * 12 + (month - 4)
                ),
                relative_time_capped = pmax(pmin(relative_month, 36), -36),
                year_month = paste(year, sprintf("%02d", month), sep = "-")
            )
        # Border-pair FE + month seasonality
        fe_formula <- "ward_pair_id + year + month"
        cluster_var <- "block_id"
    }
    time_var <- "relative_time_capped"
    time_label <- "Months"
}

message(sprintf("Loaded %s observations", format(nrow(data), big.mark = ",")))

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================
extract_iplot_data <- function(model, group_label) {
    iplot_data <- tryCatch(iplot(model, .plot = FALSE)[[1]], error = function(e) NULL)
    if (is.null(iplot_data) || nrow(iplot_data) == 0) {
        return(NULL)
    }
    iplot_data %>% mutate(
        group = group_label,
        estimate_pct = estimate * 100,
        ci_low_pct = ci_low * 100,
        ci_high_pct = ci_high * 100
    )
}

run_model <- function(formula_str, data_subset) {
    if (WEIGHTED) {
        feols(as.formula(formula_str),
            data = data_subset, weights = ~n_sales,
            cluster = as.formula(sprintf("~%s", cluster_var))
        )
    } else {
        feols(as.formula(formula_str),
            data = data_subset,
            cluster = as.formula(sprintf("~%s", cluster_var))
        )
    }
}

# =============================================================================
# RUN REGRESSIONS
# =============================================================================

if (TREATMENT_TYPE == "continuous") {
    # CONTINUOUS: One regression on full sample
    message("\n=== Continuous Treatment (Full Sample) ===")

    formula_str <- sprintf(
        "log(mean_price) ~ i(%s, treatment_continuous, ref = -1) | %s",
        time_var, fe_formula
    )
    m <- run_model(formula_str, data)
    summary(m)

    # Plot
    plot_data <- extract_iplot_data(m, "All Blocks")

    if (!is.null(plot_data) && nrow(plot_data) > 0) {
        x_breaks <- switch(FREQUENCY,
            "yearly" = -5:5,
            "quarterly" = seq(-12, 12, 4),
            "monthly" = seq(-36, 36, 12)
        )

        p <- ggplot(plot_data, aes(x = x, y = estimate_pct)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
            geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray70") +
            geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.2, fill = "#009E73") +
            geom_line(color = "#009E73", linewidth = 1) +
            geom_point(size = 2.5, color = "#009E73") +
            scale_x_continuous(breaks = x_breaks) +
            labs(
                title = "Event Study: Effect of Alderman Strictness on Home Prices",
                subtitle = sprintf(
                    "%s, %d, %s, %s | Continuous Treatment",
                    tools::toTitleCase(FREQUENCY), TREATMENT_YEAR,
                    ifelse(WEIGHTED, "Weighted", "Unweighted"),
                    ifelse(STACKED, "Stacked", "2015 Only")
                ),
                x = sprintf(
                    "%s Relative to %s %d", time_label,
                    ifelse(TREATMENT_YEAR == 2012, "Q2", "May"), TREATMENT_YEAR
                ),
                y = "Effect per 1-Unit Increase in Strictness (%)",
                caption = "Positive = stricter alderman raises prices. Negative = stricter alderman lowers prices."
            ) +
            theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

        ggsave(sprintf("../output/event_study_%s.pdf", suffix), p, width = 10, height = 6, bg = "white")
        message(sprintf("Saved: ../output/event_study_%s.pdf", suffix))
    }

    # Table
    etable(list(m),
        fitstat = ~ n + r2,
        style.tex = style.tex("aer", model.format = "", fixef.title = "", fixef.suffix = "", yesNo = c("$\\checkmark$", "")),
        depvar = FALSE, digits = 3, headers = c("Continuous"),
        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
        float = FALSE, file = sprintf("../output/did_table_%s.tex", suffix), replace = TRUE
    )
    message(sprintf("Saved: ../output/did_table_%s.tex", suffix))
} else if (TREATMENT_TYPE == "binary_direction") {
    # BINARY DIRECTION: Two separate regressions with 0/1 treatment
    message("\n=== Binary Direction Treatment ===")

    # Moved to Stricter (compare to non-switchers)
    message("\n--- Moved to Stricter ---")
    data_for_stricter <- data %>% filter(treat_lenient == 0) # Exclude lenient movers
    formula_stricter <- sprintf("log(mean_price) ~ i(%s, treat_stricter, ref = -1) | %s", time_var, fe_formula)
    m_stricter <- run_model(formula_stricter, data_for_stricter)
    summary(m_stricter)

    # Moved to More Lenient (compare to non-switchers)
    message("\n--- Moved to More Lenient ---")
    data_for_lenient <- data %>% filter(treat_stricter == 0) # Exclude stricter movers
    formula_lenient <- sprintf("log(mean_price) ~ i(%s, treat_lenient, ref = -1) | %s", time_var, fe_formula)
    m_lenient <- run_model(formula_lenient, data_for_lenient)
    summary(m_lenient)

    # Plot
    plot_data <- bind_rows(
        extract_iplot_data(m_stricter, "Moved to Stricter"),
        extract_iplot_data(m_lenient, "Moved to More Lenient")
    ) %>% filter(!is.na(estimate))

    if (nrow(plot_data) > 0) {
        x_breaks <- switch(FREQUENCY,
            "yearly" = -5:5,
            "quarterly" = seq(-12, 12, 4),
            "monthly" = seq(-36, 36, 12)
        )

        p <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
            geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray70") +
            geom_errorbar(aes(ymin = ci_low_pct, ymax = ci_high_pct), width = 0.2, alpha = 0.6) +
            geom_point(size = 2) +
            geom_line(alpha = 0.5) +
            facet_wrap(~group, ncol = 1) +
            scale_color_manual(values = c("Moved to Stricter" = "#D55E00", "Moved to More Lenient" = "#0072B2")) +
            scale_x_continuous(breaks = x_breaks) +
            labs(
                title = "Event Study: Effect of Alderman Strictness on Home Prices",
                subtitle = sprintf(
                    "%s, %d, %s, %s | Binary Direction",
                    tools::toTitleCase(FREQUENCY), TREATMENT_YEAR,
                    ifelse(WEIGHTED, "Weighted", "Unweighted"),
                    ifelse(STACKED, "Stacked", "2015 Only")
                ),
                x = sprintf(
                    "%s Relative to %s %d", time_label,
                    ifelse(TREATMENT_YEAR == 2012, "Q2", "May"), TREATMENT_YEAR
                ),
                y = "Effect on Log(Sale Price) (%)",
                caption = "Binary treatment: switched to stricter/lenient vs all other blocks."
            ) +
            theme_minimal(base_size = 12) +
            theme(
                legend.position = "none", plot.title = element_text(face = "bold"),
                panel.grid.minor = element_blank(), strip.text = element_text(face = "bold")
            )

        ggsave(sprintf("../output/event_study_%s.pdf", suffix), p, width = 10, height = 8, bg = "white")
        message(sprintf("Saved: ../output/event_study_%s.pdf", suffix))
    }

    # Table
    etable(list(m_stricter, m_lenient),
        fitstat = ~ n + r2,
        style.tex = style.tex("aer", model.format = "", fixef.title = "", fixef.suffix = "", yesNo = c("$\\checkmark$", "")),
        depvar = FALSE, digits = 3, headers = c("To Stricter", "To More Lenient"),
        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
        float = FALSE, file = sprintf("../output/did_table_%s.tex", suffix), replace = TRUE
    )
    message(sprintf("Saved: ../output/did_table_%s.tex", suffix))
}

message("\n\nDone!")
