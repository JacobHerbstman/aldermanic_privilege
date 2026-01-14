# run_event_study_fe_comparison.R
# Event study with different fixed effects specifications
# Usage: Rscript run_event_study_fe_comparison.R --fe_type=block
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales/code")

source("../../setup_environment/code/packages.R")

# =============================================================================
# COMMAND LINE ARGUMENTS
# =============================================================================
parser <- OptionParser()
parser <- add_option(parser, c("-e", "--fe_type"),
    type = "character", default = "block",
    help = "Fixed effects type: block, block_group, or ward_pair [default: block]"
)
parser <- add_option(parser, c("-w", "--weighted"),
    type = "logical", default = FALSE,
    help = "Weight by number of sales [default: FALSE]"
)
parser <- add_option(parser, c("-x", "--treatment_type"),
    type = "character", default = "continuous",
    help = "Treatment type: continuous or binary_direction [default: continuous]"
)
parser <- add_option(parser, c("-c", "--include_controls"),
    type = "logical", default = FALSE,
    help = "Include demographic controls [default: FALSE]"
)

args <- parse_args(parser)

FE_TYPE <- args$fe_type
WEIGHTED <- args$weighted
TREATMENT_TYPE <- args$treatment_type
INCLUDE_CONTROLS <- args$include_controls

message("\n=== Event Study FE Comparison ===")
message(sprintf("FE Type: %s", FE_TYPE))
message(sprintf("Weighted: %s", WEIGHTED))
message(sprintf("Treatment Type: %s", TREATMENT_TYPE))
message(sprintf("Include Controls: %s", INCLUDE_CONTROLS))

# Output suffix
suffix <- sprintf(
    "yearly_%s_%s_%s%s",
    ifelse(WEIGHTED, "weighted", "unweighted"),
    TREATMENT_TYPE,
    FE_TYPE,
    ifelse(INCLUDE_CONTROLS, "_with_controls", "")
)

# Control variables formula component
control_vars <- if (INCLUDE_CONTROLS) {
    "+ homeownership_rate + share_white + share_black + share_bach_plus + median_hh_income_1000s"
} else {
    ""
}

# =============================================================================
# LOAD DATA
# =============================================================================
message("\nLoading data...")

data <- read_csv("../input/sales_stacked_panel.csv", show_col_types = FALSE) %>%
    filter(n_sales > 0, !is.na(strictness_change)) %>%
    filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000) %>%
    mutate(
        treatment_continuous = strictness_change,
        treat_stricter = as.integer(strictness_change > 0),
        treat_lenient = as.integer(strictness_change < 0),
        relative_time_capped = pmax(pmin(relative_year, 5), -5),
        # Create cohort-specific IDs for block group
        cohort_block_group_id = paste(cohort, block_group_id, sep = "_")
    )

# Set FE formula and cluster variable based on type
if (FE_TYPE == "block") {
    # Block FEs: within-block variation only (most stringent)
    fe_formula <- "cohort_block_id + cohort^year"
    cluster_var <- "cohort_block_id"
    fe_label <- "Block-by-Cohort FE"
} else if (FE_TYPE == "block_group") {
    # Block group FEs: within-block-group variation
    fe_formula <- "cohort_block_group_id + cohort^year"
    cluster_var <- "cohort_block_id"
    fe_label <- "Block-Group-by-Cohort FE"
} else if (FE_TYPE == "ward_pair") {
    # Ward pair side FEs: within-border-segment-side variation (consistent with main run_event_study.R)
    fe_formula <- "cohort_ward_pair_side + cohort^year"
    cluster_var <- "cohort_block_id"
    fe_label <- "Ward-Pair-Side-by-Cohort FE"
} else {
    stop("Invalid fe_type. Choose from: block, block_group, ward_pair")
}

message(sprintf("FE Formula: %s", fe_formula))
message(sprintf("Cluster Variable: %s", cluster_var))
message(sprintf("Loaded %s observations", format(nrow(data), big.mark = ",")))

# Check how many FE groups we have
n_fe_groups <- n_distinct(data[[ifelse(FE_TYPE == "block", "cohort_block_id",
    ifelse(FE_TYPE == "block_group", "cohort_block_group_id",
        "cohort_ward_pair_side"
    )
)]])
message(sprintf("Number of %s groups: %s", FE_TYPE, format(n_fe_groups, big.mark = ",")))

time_var <- "relative_time_capped"
time_label <- "Years"
x_breaks <- -5:5

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

    formula_str <- sprintf("log(mean_price) ~ i(%s, treatment_continuous, ref = -1) %s | %s", time_var, control_vars, fe_formula)
    message(sprintf("Formula: %s", formula_str))

    m <- run_model(formula_str, data)
    print(summary(m))

    plot_data <- extract_iplot_data(m, "All Blocks")

    if (!is.null(plot_data) && nrow(plot_data) > 0) {
        p <- ggplot(plot_data, aes(x = x, y = estimate_pct)) +
            geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
            geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
            geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.2, fill = "#009E73", color = NA) +
            geom_line(color = "#009E73", linewidth = 1) +
            geom_point(size = 2.5, color = "#009E73") +
            scale_x_continuous(breaks = x_breaks) +
            scale_y_continuous(labels = function(x) paste0(x, "%")) +
            labs(
                title = sprintf("Event Study: %s", fe_label),
                x = sprintf("%s Relative to Alderman Switch", time_label),
                y = "Effect on Home Prices"
            ) +
            theme_minimal(base_size = 11) +
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
                axis.line = element_line(color = "gray40", linewidth = 0.3),
                axis.ticks = element_line(color = "gray40", linewidth = 0.3),
                axis.title = element_text(size = 10, color = "gray20"),
                axis.text = element_text(size = 9, color = "gray30"),
                plot.title = element_text(size = 12, face = "bold"),
                plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
            )

        ggsave(sprintf("../output/event_study_%s.pdf", suffix), p, width = 7, height = 4.5, bg = "white")
        message(sprintf("Saved: ../output/event_study_%s.pdf", suffix))
    }
} else if (TREATMENT_TYPE == "binary_direction") {
    message("\n=== Binary Direction Treatment ===")

    # Stricter
    message("\n--- Moved to Stricter ---")
    data_for_stricter <- data %>% filter(treat_lenient == 0)
    formula_stricter <- sprintf("log(mean_price) ~ i(%s, treat_stricter, ref = -1) %s | %s", time_var, control_vars, fe_formula)
    message(sprintf("Formula: %s", formula_stricter))
    m_stricter <- run_model(formula_stricter, data_for_stricter)
    print(summary(m_stricter))

    # Lenient
    message("\n--- Moved to More Lenient ---")
    data_for_lenient <- data %>% filter(treat_stricter == 0)
    formula_lenient <- sprintf("log(mean_price) ~ i(%s, treat_lenient, ref = -1) %s | %s", time_var, control_vars, fe_formula)
    m_lenient <- run_model(formula_lenient, data_for_lenient)
    print(summary(m_lenient))

    plot_data <- bind_rows(
        extract_iplot_data(m_stricter, "Moved to Stricter"),
        extract_iplot_data(m_lenient, "Moved to More Lenient")
    ) %>% filter(!is.na(estimate))

    if (nrow(plot_data) > 0) {
        p_combined <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
            geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
            geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
            geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5, shape = 21, stroke = 0.5) +
            scale_color_manual(
                values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
                labels = c("Moved to Stricter" = "To Stricter", "Moved to More Lenient" = "To More Lenient"),
                name = NULL
            ) +
            scale_fill_manual(
                values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
                labels = c("Moved to Stricter" = "To Stricter", "Moved to More Lenient" = "To More Lenient"),
                name = NULL
            ) +
            scale_x_continuous(breaks = x_breaks) +
            scale_y_continuous(labels = function(x) paste0(x, "%")) +
            labs(
                title = sprintf("Event Study: %s", fe_label),
                x = sprintf("%s Relative to Alderman Switch", time_label),
                y = "Effect on Home Prices"
            ) +
            theme_minimal(base_size = 11) +
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
                axis.line = element_line(color = "gray40", linewidth = 0.3),
                axis.ticks = element_line(color = "gray40", linewidth = 0.3),
                axis.title = element_text(size = 10, color = "gray20"),
                axis.text = element_text(size = 9, color = "gray30"),
                plot.title = element_text(size = 12, face = "bold"),
                legend.position = "bottom",
                plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
            )

        ggsave(sprintf("../output/event_study_combined_%s.pdf", suffix), p_combined, width = 7, height = 4.5, bg = "white")
        message(sprintf("Saved: ../output/event_study_combined_%s.pdf", suffix))
    }
}

message("\n\nDone!")
