# run_event_study.R
# Event study for home sales around 2015 ward redistricting
# Usage: Rscript run_event_study.R --frequency=yearly --weighted=TRUE --stacked=TRUE --treatment_type=continuous
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales/code")

source("../../setup_environment/code/packages.R")

# =============================================================================
# TEST BLOCK (uncomment to run interactively)
# =============================================================================
# FREQUENCY <- "quarterly"
# WEIGHTED <- TRUE
# STACKED <- TRUE
# TREATMENT_TYPE <- "continuous"

# =============================================================================
# COMMAND LINE ARGUMENTS
# =============================================================================
parser <- OptionParser()
parser <- add_option(parser, c("-f", "--frequency"),
    type = "character", default = "yearly",
    help = "Analysis frequency: yearly or quarterly [default: yearly]"
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
WEIGHTED <- args$weighted
STACKED <- args$stacked
TREATMENT_TYPE <- args$treatment_type

message("\n=== Event Study Configuration ===")
message(sprintf("Frequency: %s", FREQUENCY))
message(sprintf("Weighted: %s", WEIGHTED))
message(sprintf("Stacked: %s", STACKED))
message(sprintf("Treatment Type: %s", TREATMENT_TYPE))

# Output suffix
suffix <- sprintf(
    "%s_%s_%s_%s", FREQUENCY,
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
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0.1),
                treat_lenient = as.integer(strictness_change < -0.1),
                relative_time_capped = pmax(pmin(relative_year, 5), -5)
            )
        fe_formula <- "cohort_ward_pair + cohort^year"
        cluster_var <- "cohort_block_id"
    } else {
        data <- read_csv("../input/sales_block_year_panel.csv", show_col_types = FALSE) %>%
            filter(n_sales > 0, !is.na(strictness_change), year >= 2010, year <= 2020) %>%
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0),
                treat_lenient = as.integer(strictness_change < 0),
                relative_time_capped = pmax(pmin(relative_year, 5), -5)
            )
        fe_formula <- "ward_pair_id + year"
        cluster_var <- "block_id"
    }
    time_var <- "relative_time_capped"
    time_label <- "Years"
    x_breaks <- -5:5
} else if (FREQUENCY == "quarterly") {
    if (STACKED) {
        data <- read_csv("../input/sales_stacked_quarterly_panel.csv", show_col_types = FALSE) %>%
            filter(n_sales > 0, !is.na(strictness_change)) %>%
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0.1),
                treat_lenient = as.integer(strictness_change < -0.1),
                relative_time_capped = pmax(pmin(relative_quarter, 12), -12)
            )
        fe_formula <- "cohort_ward_pair + cohort^year_quarter"
        cluster_var <- "cohort_block_id"
    } else {
        data <- read_csv("../input/sales_block_year_panel.csv", show_col_types = FALSE) %>%
            filter(n_sales > 0, !is.na(strictness_change), year >= 2010, year <= 2020) %>%
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0),
                treat_lenient = as.integer(strictness_change < 0),
                relative_quarter = relative_year * 4,
                relative_time_capped = pmax(pmin(relative_quarter, 12), -12)
            )
        fe_formula <- "ward_pair_id + year"
        cluster_var <- "block_id"
    }
    time_var <- "relative_time_capped"
    time_label <- "Quarters"
    x_breaks <- seq(-12, 12, 4)
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
    message("\n=== Continuous Treatment ===")

    formula_str <- sprintf("log(mean_price) ~ i(%s, treatment_continuous, ref = -1) | %s", time_var, fe_formula)
    m <- run_model(formula_str, data)
    summary(m)

    plot_data <- extract_iplot_data(m, "All Blocks")

    if (!is.null(plot_data) && nrow(plot_data) > 0) {
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
                    "%s, %s, %s | Continuous Treatment",
                    tools::toTitleCase(FREQUENCY),
                    ifelse(WEIGHTED, "Weighted", "Unweighted"),
                    ifelse(STACKED, "Stacked", "Unstacked")
                ),
                x = sprintf("%s Relative to May 2015", time_label),
                y = "Effect per 1-Unit Increase in Strictness (%)",
                caption = "Positive = stricter alderman raises prices."
            ) +
            theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

        ggsave(sprintf("../output/event_study_%s.pdf", suffix), p, width = 10, height = 6, bg = "white")
        message(sprintf("Saved: ../output/event_study_%s.pdf", suffix))
    }

    etable(list(m),
        fitstat = ~ n + r2,
        style.tex = style.tex("aer", model.format = "", fixef.title = "", fixef.suffix = "", yesNo = c("$\\checkmark$", "")),
        depvar = FALSE, digits = 3, headers = c("Continuous"),
        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
        float = FALSE, file = sprintf("../output/did_table_%s.tex", suffix), replace = TRUE
    )
    message(sprintf("Saved: ../output/did_table_%s.tex", suffix))
} else if (TREATMENT_TYPE == "binary_direction") {
    message("\n=== Binary Direction Treatment ===")

    # Stricter
    message("\n--- Moved to Stricter ---")
    data_for_stricter <- data %>% filter(treat_lenient == 0)
    formula_stricter <- sprintf("log(mean_price) ~ i(%s, treat_stricter, ref = -1) | %s", time_var, fe_formula)
    m_stricter <- run_model(formula_stricter, data_for_stricter)
    summary(m_stricter)

    # Lenient
    message("\n--- Moved to More Lenient ---")
    data_for_lenient <- data %>% filter(treat_stricter == 0)
    formula_lenient <- sprintf("log(mean_price) ~ i(%s, treat_lenient, ref = -1) | %s", time_var, fe_formula)
    m_lenient <- run_model(formula_lenient, data_for_lenient)
    summary(m_lenient)

    plot_data <- bind_rows(
        extract_iplot_data(m_stricter, "Moved to Stricter"),
        extract_iplot_data(m_lenient, "Moved to More Lenient")
    ) %>% filter(!is.na(estimate))

    if (nrow(plot_data) > 0) {
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
                    "%s, %s, %s | Binary Direction",
                    tools::toTitleCase(FREQUENCY),
                    ifelse(WEIGHTED, "Weighted", "Unweighted"),
                    ifelse(STACKED, "Stacked", "Unstacked")
                ),
                x = sprintf("%s Relative to May 2015", time_label),
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
