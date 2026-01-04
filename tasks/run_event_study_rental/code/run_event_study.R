# run_event_study.R
# Event study for rental prices around 2015 ward redistricting
# Usage: Rscript run_event_study.R --frequency=yearly --weighted=TRUE --stacked=TRUE --treatment_type=continuous

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental/code")
source("../../setup_environment/code/packages.R")

# =============================================================================
# TEST BLOCK (uncomment to run interactively)
# =============================================================================
# FREQUENCY <- "yearly"
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
    help = "Weight by number of listings [default: TRUE]"
)
parser <- add_option(parser, c("-s", "--stacked"),
    type = "logical", default = TRUE,
    help = "Use stacked design with 2015+2023 cohorts [default: TRUE]"
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

FREQUENCY <- args$frequency
WEIGHTED <- args$weighted
STACKED <- args$stacked
TREATMENT_TYPE <- args$treatment_type
INCLUDE_CONTROLS <- args$include_controls

message("\n=== Event Study Configuration ===")
message(sprintf("Frequency: %s", FREQUENCY))
message(sprintf("Weighted: %s", WEIGHTED))
message(sprintf("Stacked: %s", STACKED))
message(sprintf("Treatment Type: %s", TREATMENT_TYPE))
message(sprintf("Include Controls: %s", INCLUDE_CONTROLS))

# Output suffix
suffix <- sprintf(
    "%s_%s_%s_%s%s", FREQUENCY,
    ifelse(WEIGHTED, "weighted", "unweighted"),
    ifelse(STACKED, "stacked", "unstacked"),
    TREATMENT_TYPE,
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

if (FREQUENCY == "yearly") {
    if (STACKED) {
        data <- read_csv("../input/rental_stacked_panel.csv", show_col_types = FALSE) %>%
            filter(n_listings > 0, !is.na(strictness_change)) %>%
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0),
                treat_lenient = as.integer(strictness_change < 0),
                relative_time_capped = pmax(pmin(relative_year, 5), -5)
            )
        fe_formula <- "cohort_ward_pair + cohort^year"
        cluster_var <- "cohort_block_id"
    } else {
        # Unstacked 2015 cohort only - uses file with generic column names
        data <- read_csv("../input/rental_unstacked_2015_panel.csv", show_col_types = FALSE) %>%
            filter(n_listings > 0, !is.na(strictness_change)) %>%
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
        data <- read_csv("../input/rental_stacked_quarterly_panel.csv", show_col_types = FALSE) %>%
            filter(n_listings > 0, !is.na(strictness_change)) %>%
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0),
                treat_lenient = as.integer(strictness_change < 0),
                relative_time_capped = pmax(pmin(relative_quarter, 16), -8)
            )
        fe_formula <- "cohort_ward_pair + cohort^year_quarter"
        cluster_var <- "cohort_block_id"
    } else {
        data <- read_csv("../input/rental_stacked_quarterly_panel.csv", show_col_types = FALSE) %>%
            filter(n_listings > 0, !is.na(strictness_change), cohort == "2015") %>%
            filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000) %>%
            mutate(
                treatment_continuous = strictness_change,
                treat_stricter = as.integer(strictness_change > 0),
                treat_lenient = as.integer(strictness_change < 0),
                relative_time_capped = pmax(pmin(relative_quarter, 16), -8),
                block_id = sub("^2015_", "", cohort_block_id)
            )
        fe_formula <- "ward_pair_id + year_quarter"
        cluster_var <- "block_id"
    }
    time_var <- "relative_time_capped"
    time_label <- "Quarters"
    x_breaks <- seq(-8, 16, 4)
}

message(sprintf("Loaded %s observations", format(nrow(data), big.mark = ",")))

# Note: strictness_change is the difference between destination and origin alderman strictness.
# Since the underlying strictness_index is already standardized (SD=1), a 1-unit change in
# strictness_change represents moving to an alderman who is 1-SD stricter/more lenient.

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
            data = data_subset, weights = ~n_listings,
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

    formula_str <- sprintf("log(mean_rent) ~ i(%s, treatment_continuous, ref = -1) %s | %s", time_var, control_vars, fe_formula)
    m <- run_model(formula_str, data)
    summary(m)

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
                x = sprintf("%s Relative to Alderman Switch", time_label),
                y = "Effect on Rents"
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
                plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
            )

        ggsave(sprintf("../output/event_study_%s.pdf", suffix), p, width = 7, height = 4.5, bg = "white")
        message(sprintf("Saved: ../output/event_study_%s.pdf", suffix))
    }

    etable(list(m),
        fitstat = ~ n + r2,
        style.tex = style.tex("aer", model.format = "", fixef.title = "", fixef.suffix = "", yesNo = c("$\\checkmark$", "")),
        depvar = FALSE, digits = 3, headers = c("Continuous"),
        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
        notes = "Alderman strictness is standardized (mean 0, SD 1). Coefficients represent the effect of moving to an alderman who is 1-SD stricter.",
        float = FALSE, file = sprintf("../output/did_table_%s.tex", suffix), replace = TRUE
    )
    message(sprintf("Saved: ../output/did_table_%s.tex", suffix))
} else if (TREATMENT_TYPE == "binary_direction") {
    message("\n=== Binary Direction Treatment ===")

    # Stricter
    message("\n--- Moved to Stricter ---")
    data_for_stricter <- data %>% filter(treat_lenient == 0)
    formula_stricter <- sprintf("log(mean_rent) ~ i(%s, treat_stricter, ref = -1) %s | %s", time_var, control_vars, fe_formula)
    m_stricter <- run_model(formula_stricter, data_for_stricter)
    summary(m_stricter)

    # Lenient
    message("\n--- Moved to More Lenient ---")
    data_for_lenient <- data %>% filter(treat_stricter == 0)
    formula_lenient <- sprintf("log(mean_rent) ~ i(%s, treat_lenient, ref = -1) %s | %s", time_var, control_vars, fe_formula)
    m_lenient <- run_model(formula_lenient, data_for_lenient)
    summary(m_lenient)

    plot_data <- bind_rows(
        extract_iplot_data(m_stricter, "Moved to Stricter"),
        extract_iplot_data(m_lenient, "Moved to More Lenient")
    ) %>% filter(!is.na(estimate))

    if (nrow(plot_data) > 0) {
        p <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
            geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
            geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
            geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5, shape = 21, stroke = 0.5) +
            scale_color_manual(
                values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
                labels = c("Moved to Stricter" = "Moved to Stricter Alderman", "Moved to More Lenient" = "Moved to More Lenient Alderman"),
                name = NULL
            ) +
            scale_fill_manual(
                values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
                labels = c("Moved to Stricter" = "Moved to Stricter Alderman", "Moved to More Lenient" = "Moved to More Lenient Alderman"),
                name = NULL
            ) +
            scale_x_continuous(breaks = x_breaks) +
            scale_y_continuous(labels = function(x) paste0(x, "%")) +
            facet_wrap(~group, ncol = 1) +
            labs(
                x = sprintf("%s Relative to Alderman Switch", time_label),
                y = "Effect on Rents"
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
                legend.position = "none",
                strip.text = element_text(face = "bold", size = 10),
                plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
            )

        ggsave(sprintf("../output/event_study_%s.pdf", suffix), p, width = 7, height = 6, bg = "white")
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

    # =============================================================================
    # PUBLICATION-QUALITY COMBINED PLOT (both series on same axes)
    # =============================================================================
    if (nrow(plot_data) > 0) {
        # Set up publication-quality aesthetics
        # Stricter: bold, saturated color with thicker elements
        # Lenient: lighter, more muted with thinner elements

        p_combined <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
            # Reference lines
            geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
            geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
            # Lenient series first (so stricter is on top)
            geom_ribbon(
                data = plot_data %>% filter(group == "Moved to More Lenient"),
                aes(ymin = ci_low_pct, ymax = ci_high_pct),
                alpha = 0.15, color = NA
            ) +
            geom_line(
                data = plot_data %>% filter(group == "Moved to More Lenient"),
                linewidth = 0.8, alpha = 0.7
            ) +
            geom_point(
                data = plot_data %>% filter(group == "Moved to More Lenient"),
                size = 2, alpha = 0.7, shape = 21, stroke = 0.5
            ) +
            # Stricter series on top (emphasized)
            geom_ribbon(
                data = plot_data %>% filter(group == "Moved to Stricter"),
                aes(ymin = ci_low_pct, ymax = ci_high_pct),
                alpha = 0.2, color = NA
            ) +
            geom_line(
                data = plot_data %>% filter(group == "Moved to Stricter"),
                linewidth = 1.2
            ) +
            geom_point(
                data = plot_data %>% filter(group == "Moved to Stricter"),
                size = 3, shape = 21, stroke = 0.8
            ) +
            # Colors: Stricter = dark red/orange, Lenient = muted blue/gray
            scale_color_manual(
                values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
                labels = c("Moved to Stricter" = "Moved to Stricter Alderman", "Moved to More Lenient" = "Moved to More Lenient Alderman"),
                name = NULL
            ) +
            scale_fill_manual(
                values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
                labels = c("Moved to Stricter" = "Moved to Stricter Alderman", "Moved to More Lenient" = "Moved to More Lenient Alderman"),
                name = NULL
            ) +
            scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.02, 0.02))) +
            scale_y_continuous(labels = function(x) paste0(x, "%")) +
            labs(
                x = sprintf("%s Relative to Alderman Switch", time_label),
                y = "Effect on Rents"
            ) +
            theme_minimal(base_size = 11, base_family = "") +
            theme(
                # Clean, minimal appearance for publication
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
                # Axis styling
                axis.line = element_line(color = "gray40", linewidth = 0.3),
                axis.ticks = element_line(color = "gray40", linewidth = 0.3),
                axis.ticks.length = unit(0.15, "cm"),
                axis.title = element_text(size = 10, color = "gray20"),
                axis.text = element_text(size = 9, color = "gray30"),
                # Legend at bottom, horizontal
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.text = element_text(size = 9),
                legend.key.width = unit(1.5, "cm"),
                legend.margin = margin(t = 5, b = 0),
                # Margins
                plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
            ) +
            guides(
                color = guide_legend(override.aes = list(linewidth = c(1.2, 0.8), size = c(3, 2))),
                fill = guide_legend(override.aes = list(alpha = c(0.2, 0.15)))
            )

        outfile_combined <- sprintf("../output/event_study_combined_%s.pdf", suffix)
        ggsave(outfile_combined, p_combined, width = 7, height = 4.5, bg = "white")
        message(sprintf("Saved: %s", outfile_combined))
    }
}

message("\n\nDone!")
