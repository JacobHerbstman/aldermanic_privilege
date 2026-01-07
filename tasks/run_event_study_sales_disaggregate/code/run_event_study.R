# run_event_study.R
# Transaction-level event study for home sales with hedonic controls
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_disaggregate/code")

source("../../setup_environment/code/packages.R")

# =============================================================================
# TEST ARGUMENTS
# =============================================================================
# For testing in interactive R session
# TREATMENT_TYPE <- "continuous"  # "continuous" or "binary_direction"
# INCLUDE_HEDONICS <- FALSE               # TRUE or FALSE


# =============================================================================
# COMMAND LINE ARGUMENTS
# =============================================================================
parser <- OptionParser()
parser <- add_option(parser, c("-x", "--treatment_type"),
    type = "character", default = "continuous",
    help = "Treatment type: continuous or binary_direction [default: continuous]"
)
parser <- add_option(parser, c("-c", "--include_hedonics"),
    type = "logical", default = TRUE,
    help = "Include hedonic controls [default: TRUE]"
)
parser <- add_option(parser, c("-t", "--time_unit"),
    type = "character", default = "yearly",
    help = "Time unit: yearly or quarterly [default: yearly]"
)
parser <- add_option(parser, c("-f", "--fe_type"),
    type = "character", default = "ward_pair_side",
    help = "Fixed effect type: ward_pair_side, block, or block_group [default: ward_pair_side]"
)

args <- parse_args(parser)

TREATMENT_TYPE <- args$treatment_type
INCLUDE_HEDONICS <- args$include_hedonics
TIME_UNIT <- args$time_unit
FE_TYPE <- args$fe_type

message("\n=== Disaggregate Sales Event Study ===")
message(sprintf("Treatment Type: %s", TREATMENT_TYPE))
message(sprintf("Include Hedonics: %s", INCLUDE_HEDONICS))
message(sprintf("Time Unit: %s", TIME_UNIT))
message(sprintf("FE Type: %s", FE_TYPE))

# Output suffix
fe_suffix <- switch(FE_TYPE,
    "ward_pair_side" = "",
    "block" = "_block_fe",
    "block_group" = "_bg_fe"
)
suffix <- sprintf(
    "disaggregate_%s%s%s%s",
    TIME_UNIT,
    ifelse(TREATMENT_TYPE == "continuous", "_continuous", "_binary"),
    fe_suffix,
    ifelse(INCLUDE_HEDONICS, "", "_no_hedonics")
)

# =============================================================================
# LOAD DATA
# =============================================================================
message("\nLoading transaction panel...")

data <- read_parquet("../input/sales_transaction_panel.parquet")
setDT(data)
message(sprintf("Loaded %s transactions", format(nrow(data), big.mark = ",")))

# Create treatment indicators for binary analysis
data[, `:=`(
    treatment_continuous = strictness_change,
    treat_stricter = as.integer(strictness_change > 0),
    treat_lenient = as.integer(strictness_change < 0)
)]

# =============================================================================
# CREATE TIME VARIABLES BASED ON TIME_UNIT
# =============================================================================
if (TIME_UNIT == "quarterly") {
    message("\nCreating quarterly relative periods...")

    # Extract year and quarter from sale_date
    data[, `:=`(
        sale_quarter = quarter(sale_date),
        sale_yearqtr = year(sale_date) + (quarter(sale_date) - 1) / 4
    )]

    # Calculate relative quarter for each cohort
    # 2015 cohort: Q1 2015 = 2015.0, treatment effective after redistricting
    # 2023 cohort: Q1 2023 = 2023.0
    data[, cohort_yearqtr := as.numeric(cohort)]
    data[, relative_qtr := round((sale_yearqtr - cohort_yearqtr) * 4)]
    data[, relative_qtr_capped := pmax(pmin(relative_qtr, 12), -12)] # Cap at +/- 12 quarters (3 years)

    # Use quarterly variables for regression
    data[, relative_period := relative_qtr_capped]
    time_fe <- "cohort^sale_yearqtr"
    x_label <- "Quarters Relative to Redistricting"
    x_breaks <- seq(-12, 12, by = 2)

    message(sprintf(
        "Relative quarter range: %d to %d",
        min(data$relative_qtr, na.rm = TRUE),
        max(data$relative_qtr, na.rm = TRUE)
    ))
} else {
    # Yearly (default)
    data[, relative_period := relative_year_capped]
    time_fe <- "cohort^sale_year"
    x_label <- "Years Relative to Redistricting"
    x_breaks <- -5:5
}

# =============================================================================
# SET UNIT FIXED EFFECTS BASED ON FE_TYPE
# =============================================================================
message(sprintf("\nSetting unit FE: %s", FE_TYPE))

if (FE_TYPE == "block") {
    # Block FE: use cohort_block_id (already in data)
    unit_fe <- "cohort_block_id"
    cluster_var <- "cohort_block_id"
} else if (FE_TYPE == "block_group") {
    # Block group FE: extract from block_id (first 12 digits)
    data[, block_group_id := substr(block_id, 1, 12)]
    data[, cohort_block_group_id := paste(cohort, block_group_id, sep = "_")]
    unit_fe <- "cohort_block_group_id"
    cluster_var <- "cohort_block_id" # Still cluster at block level
} else {
    # Ward pair side FE (default)
    unit_fe <- "cohort_ward_pair_side"
    cluster_var <- "cohort_block_id"
}

message(sprintf("Unit FE: %s", unit_fe))
message(sprintf("Cluster var: %s", cluster_var))

# =============================================================================
# SPECIFY HEDONIC CONTROLS
# =============================================================================
if (INCLUDE_HEDONICS) {
    hedonic_formula <- "+ log_sqft + sqft_missing + log_land_sqft + land_sqft_missing + building_age + age_missing + bedrooms_factor + bedrooms_missing + baths_total + baths_missing + has_garage"
    message("\nHedonic controls: ON")
} else {
    hedonic_formula <- ""
    message("\nHedonic controls: OFF")
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================
extract_iplot_data <- function(model, group_label) {
    iplot_data <- tryCatch(iplot(model, .plot = FALSE)[[1]], error = function(e) NULL)
    if (is.null(iplot_data) || nrow(iplot_data) == 0) {
        return(NULL)
    }
    iplot_data %>%
        as_tibble() %>%
        mutate(
            group = group_label,
            estimate_pct = estimate * 100,
            ci_low_pct = ci_low * 100,
            ci_high_pct = ci_high * 100
        )
}

make_event_study_plot <- function(plot_data, title = NULL) {
    p <- ggplot(plot_data, aes(x = x, y = estimate_pct)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
        geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
        geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.2, fill = "#009E73", color = NA) +
        geom_line(color = "#009E73", linewidth = 1) +
        geom_point(size = 2.5, color = "#009E73") +
        scale_x_continuous(breaks = x_breaks) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        labs(
            x = x_label,
            y = "Effect on Home Prices",
            title = title
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
    return(p)
}

# =============================================================================
# RUN REGRESSIONS
# =============================================================================

if (TREATMENT_TYPE == "continuous") {
    message("\n=== Continuous Treatment ===")

    formula_str <- sprintf(
        "log(sale_price) ~ i(relative_period, treatment_continuous, ref = -1) %s | cohort_ward_pair_side + %s",
        hedonic_formula, time_fe
    )
    message(sprintf("Formula: %s", formula_str))

    message("\nEstimating model (this may take a minute with large data)...")
    t0 <- Sys.time()

    m <- feols(
        as.formula(formula_str),
        data = data,
        cluster = ~cohort_block_id
    )

    t1 <- Sys.time()
    message(sprintf("Estimation time: %.1f seconds", difftime(t1, t0, units = "secs")))

    message("\n--- Regression Summary ---")
    print(summary(m))

    # Extract and plot
    plot_data <- extract_iplot_data(m, "Continuous Treatment")

    if (!is.null(plot_data) && nrow(plot_data) > 0) {
        p <- make_event_study_plot(plot_data)

        outfile <- sprintf("../output/event_study_%s.pdf", suffix)
        ggsave(outfile, p, width = 7, height = 4.5, bg = "white")
        message(sprintf("\nSaved: %s", outfile))
    }

    # Save regression table
    etable(list(m),
        fitstat = ~ n + r2,
        style.tex = style.tex("aer",
            model.format = "", fixef.title = "", fixef.suffix = "",
            yesNo = c("$\\checkmark$", "")
        ),
        depvar = FALSE,
        digits = 3,
        headers = c("Continuous"),
        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
        notes = "Transaction-level regression with hedonic controls. Strictness is standardized (SD=1).",
        float = FALSE,
        file = sprintf("../output/did_table_%s.tex", suffix),
        replace = TRUE
    )
    message(sprintf("Saved: ../output/did_table_%s.tex", suffix))
} else if (TREATMENT_TYPE == "binary_direction") {
    message("\n=== Binary Direction Treatment ===")

    # --- Moved to Stricter ---
    message("\n--- Estimating: Moved to Stricter ---")

    data_stricter <- data[treat_lenient == 0] # Keep stricter + controls, drop lenient
    message(sprintf("Stricter sample: %s transactions", format(nrow(data_stricter), big.mark = ",")))

    formula_stricter <- sprintf(
        "log(sale_price) ~ i(relative_period, treat_stricter, ref = -1) %s | cohort_ward_pair_side + %s",
        hedonic_formula, time_fe
    )

    t0 <- Sys.time()
    m_stricter <- feols(as.formula(formula_stricter), data = data_stricter, cluster = ~cohort_block_id)
    t1 <- Sys.time()
    message(sprintf("Estimation time: %.1f seconds", difftime(t1, t0, units = "secs")))

    print(summary(m_stricter))

    # --- Moved to More Lenient ---
    message("\n--- Estimating: Moved to More Lenient ---")

    data_lenient <- data[treat_stricter == 0] # Keep lenient + controls, drop stricter
    message(sprintf("Lenient sample: %s transactions", format(nrow(data_lenient), big.mark = ",")))

    formula_lenient <- sprintf(
        "log(sale_price) ~ i(relative_period, treat_lenient, ref = -1) %s | cohort_ward_pair_side + %s",
        hedonic_formula, time_fe
    )

    t0 <- Sys.time()
    m_lenient <- feols(as.formula(formula_lenient), data = data_lenient, cluster = ~cohort_block_id)
    t1 <- Sys.time()
    message(sprintf("Estimation time: %.1f seconds", difftime(t1, t0, units = "secs")))

    print(summary(m_lenient))

    # --- Combined Plot ---
    plot_data <- bind_rows(
        extract_iplot_data(m_stricter, "Moved to Stricter"),
        extract_iplot_data(m_lenient, "Moved to More Lenient")
    ) %>% filter(!is.na(estimate))

    if (nrow(plot_data) > 0) {
        # Faceted plot
        p_facet <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
            geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
            geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
            geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5, shape = 21, stroke = 0.5) +
            scale_color_manual(
                values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
                name = NULL
            ) +
            scale_fill_manual(
                values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
                name = NULL
            ) +
            scale_x_continuous(breaks = x_breaks) +
            scale_y_continuous(labels = function(x) paste0(x, "%")) +
            facet_wrap(~group, ncol = 1) +
            labs(
                x = x_label,
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
                legend.position = "none",
                strip.text = element_text(face = "bold", size = 10),
                plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
            )

        outfile <- sprintf("../output/event_study_%s.pdf", suffix)
        ggsave(outfile, p_facet, width = 7, height = 6, bg = "white")
        message(sprintf("\nSaved: %s", outfile))

        # Combined (overlaid) plot
        p_combined <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
            geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
            geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
            geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5, shape = 21, stroke = 0.5) +
            scale_color_manual(
                values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
                labels = c(
                    "Moved to Stricter" = "Moved to Stricter Alderman",
                    "Moved to More Lenient" = "Moved to More Lenient Alderman"
                ),
                name = NULL
            ) +
            scale_fill_manual(
                values = c("Moved to Stricter" = "#c23616", "Moved to More Lenient" = "#7f8fa6"),
                labels = c(
                    "Moved to Stricter" = "Moved to Stricter Alderman",
                    "Moved to More Lenient" = "Moved to More Lenient Alderman"
                ),
                name = NULL
            ) +
            scale_x_continuous(breaks = x_breaks) +
            scale_y_continuous(labels = function(x) paste0(x, "%")) +
            labs(
                x = x_label,
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
                legend.position = "bottom",
                legend.direction = "horizontal",
                plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
            )

        outfile_combined <- sprintf("../output/event_study_combined_%s.pdf", suffix)
        ggsave(outfile_combined, p_combined, width = 7, height = 4.5, bg = "white")
        message(sprintf("Saved: %s", outfile_combined))
    }

    # Save regression table
    etable(list(m_stricter, m_lenient),
        fitstat = ~ n + r2,
        style.tex = style.tex("aer",
            model.format = "", fixef.title = "", fixef.suffix = "",
            yesNo = c("$\\checkmark$", "")
        ),
        depvar = FALSE,
        digits = 3,
        headers = c("To Stricter", "To More Lenient"),
        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
        notes = "Transaction-level regression with hedonic controls.",
        float = FALSE,
        file = sprintf("../output/did_table_%s.tex", suffix),
        replace = TRUE
    )
    message(sprintf("Saved: ../output/did_table_%s.tex", suffix))
}

message("\n\nDone!")
