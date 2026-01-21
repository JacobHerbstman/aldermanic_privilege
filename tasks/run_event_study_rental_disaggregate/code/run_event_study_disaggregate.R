# run_event_study_disaggregate.R
# Disaggregate (listing-level) event study for rental prices around ward redistricting
# Uses hedonic controls and individual listing observations instead of block-level aggregation
#
# Usage: Rscript run_event_study_disaggregate.R --frequency="yearly" --stacked=TRUE --treatment_type="continuous" --include_controls=TRUE

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental_disaggregate/code")
source("../../setup_environment/code/packages.R")

# =============================================================================
# TEST ARGUMENTS
# =============================================================================
# For testing in interactive R session

# FREQUENCY <- "yearly"          # "yearly" or "quarterly"
# STACKED <- T                 # TRUE or FALSE
# TREATMENT_TYPE <- "continuous"  # "continuous" or "binary_direction"
# INCLUDE_CONTROLS <- T        # TRUE or FALSE

# =============================================================================
# COMMAND LINE ARGUMENTS
# =============================================================================
parser <- OptionParser()
parser <- add_option(parser, c("-f", "--frequency"),
    type = "character", default = "yearly",
    help = "Analysis frequency: yearly or quarterly [default: yearly]"
)
parser <- add_option(parser, c("-s", "--stacked"),
    type = "logical", default = TRUE,
    help = "Use stacked design with 2015+2023 cohorts [default: TRUE]"
)
parser <- add_option(parser, c("-x", "--treatment_type"),
    type = "character", default = "continuous",
    help = "Treatment type: continuous, binary_direction, or continuous_split [default: continuous]"
)
parser <- add_option(parser, c("-c", "--include_controls"),
    type = "logical", default = TRUE,
    help = "Include hedonic controls [default: TRUE]"
)
parser <- add_option(parser, c("-w", "--weighting"),
    type = "character", default = "uniform",
    help = "Weighting scheme: uniform or triangular [default: uniform]"
)
parser <- add_option(parser, c("-b", "--bandwidth"),
    type = "numeric", default = 1000,
    help = "Bandwidth in feet for distance weighting [default: 1000]"
)
parser <- add_option(parser, c("-m", "--sample_filter"),
    type = "character", default = "full_sample",
    help = "Sample filter: full_sample or multifamily_only [default: full_sample]"
)
parser <- add_option(parser, c("-p", "--post_window"),
    type = "character", default = "full",
    help = "Post-period window: 'short' (truncated) or 'full' [default: full]"
)

args <- parse_args(parser)

FREQUENCY <- args$frequency
STACKED <- args$stacked
TREATMENT_TYPE <- args$treatment_type
INCLUDE_CONTROLS <- args$include_controls
WEIGHTING <- args$weighting
BANDWIDTH <- args$bandwidth
SAMPLE_FILTER <- args$sample_filter
POST_WINDOW <- args$post_window

message("\n=== Disaggregate Event Study Configuration ===")
message(sprintf("Frequency: %s", FREQUENCY))
message(sprintf("Stacked: %s", STACKED))
message(sprintf("Treatment Type: %s", TREATMENT_TYPE))
message(sprintf("Include Hedonic Controls: %s", INCLUDE_CONTROLS))
message(sprintf("Weighting: %s", WEIGHTING))
message(sprintf("Bandwidth: %d ft", BANDWIDTH))
message(sprintf("Sample Filter: %s", SAMPLE_FILTER))
message(sprintf("Post Window: %s", POST_WINDOW))

# Output suffix
sample_suffix <- ifelse(SAMPLE_FILTER == "multifamily_only", "_mf", "")

suffix <- sprintf(
    "disaggregate_%s_%s_%s_%s_%dft%s%s%s",
    FREQUENCY,
    ifelse(STACKED, "stacked", "unstacked"),
    TREATMENT_TYPE,
    WEIGHTING,
    as.integer(BANDWIDTH),
    sample_suffix,
    ifelse(INCLUDE_CONTROLS, "", "_no_hedonics"),
    ifelse(POST_WINDOW == "short", "_short", "")
)

# Hedonic controls formula component
# Available: beds_factor, beds_missing, baths_factor, baths_missing, has_gym, has_laundry, building_type_factor
hedonic_controls <- if (INCLUDE_CONTROLS) {
    "+ building_type_factor + log_sqft + log_beds+log_baths"
} else {
    ""
}

# =============================================================================
# LOAD DATA
# =============================================================================
message("\nLoading listing-level panel data...")

if (STACKED) {
    data <- read_parquet("../input/rental_listing_panel.parquet") %>%
        filter(!is.na(strictness_change), !is.na(rent_price), rent_price > 0)

    if (FREQUENCY == "yearly") {
        time_fe_var <- "year"
        time_var <- "relative_year_capped"
    } else {
        time_fe_var <- "year_quarter"
        time_var <- "relative_quarter_capped"
    }
    cluster_var <- "cohort_block_id"
    fe_group_var <- "cohort_ward_pair"
} else {
    # Unstacked: use 2015 cohort only
    data <- read_parquet("../input/rental_listing_panel_2015.parquet") %>%
        filter(!is.na(strictness_change), !is.na(rent_price), rent_price > 0)

    if (FREQUENCY == "yearly") {
        time_fe_var <- "year"
        time_var <- "relative_year_capped"
    } else {
        time_fe_var <- "year_quarter"
        time_var <- "relative_quarter_capped"
    }
    cluster_var <- "block_id"
    fe_group_var <- "ward_pair"
}

message(sprintf("Loaded %s observations", format(nrow(data), big.mark = ",")))
message(sprintf("Unique blocks: %s", format(n_distinct(data$block_id), big.mark = ",")))
message(sprintf("Unique listings: %s", format(n_distinct(data$id), big.mark = ",")))

# =============================================================================
# APPLY SAMPLE FILTER
# =============================================================================
if (SAMPLE_FILTER == "multifamily_only") {
    message("\nFiltering to multifamily buildings only...")
    message(sprintf("Observations before filter: %s", format(nrow(data), big.mark = ",")))

    data <- data %>% filter(building_type_clean == "multi_family")

    message(sprintf("Observations after filter: %s", format(nrow(data), big.mark = ",")))
} else {
    message("\nUsing full sample (all building types)")
}

# =============================================================================
# APPLY BANDWIDTH AND CONSTRUCT WEIGHTS
# =============================================================================
message(sprintf("\nApplying bandwidth filter: %d ft", BANDWIDTH))
message(sprintf("Observations before filter: %s", format(nrow(data), big.mark = ",")))

data <- data %>%
    filter(dist_ft <= BANDWIDTH) %>%
    mutate(
        weight = case_when(
            WEIGHTING == "uniform" ~ 1,
            WEIGHTING == "triangular" ~ pmax(0, 1 - dist_ft / BANDWIDTH),
            TRUE ~ 1
        )
    )

message(sprintf("Observations after filter: %s", format(nrow(data), big.mark = ",")))

# Create continuous split treatment variables
data <- data %>%
    mutate(
        treatment_stricter_continuous = pmax(strictness_change, 0),
        treatment_lenient_continuous = pmax(-strictness_change, 0)
    )

# =============================================================================
# RESTRICT TO COMPLETE HEDONIC SAMPLE (for comparability across specs)
# =============================================================================
hedonic_vars_list <- c("log_sqft", "log_beds", "log_baths", "building_type_clean")

n_before <- nrow(data)
data <- data[complete.cases(data[, hedonic_vars_list]), ]
n_after <- nrow(data)

message(sprintf(
    "Restricted to complete hedonic sample: %s -> %s listings (dropped %s)",
    format(n_before, big.mark = ","),
    format(n_after, big.mark = ","),
    format(n_before - n_after, big.mark = ",")
))

# Weighting diagnostics
message("\n=== WEIGHTING DIAGNOSTICS ===")
message(sprintf("Sum of weights (effective N): %.0f", sum(data$weight)))
message(sprintf("Efficiency ratio: %.1f%%", 100 * sum(data$weight) / nrow(data)))

message("\nWeight distribution by treatment status:")
data %>%
    group_by(treat) %>%
    summarise(
        n = n(),
        sum_weights = sum(weight),
        mean_weight = mean(weight),
        mean_dist_ft = mean(dist_ft),
        .groups = "drop"
    ) %>%
    print()

message("\nWeight distribution by distance bins:")
bin_width <- min(250, BANDWIDTH / 4)
data %>%
    mutate(dist_bin = cut(dist_ft, breaks = seq(0, BANDWIDTH, by = bin_width), include.lowest = TRUE)) %>%
    group_by(dist_bin) %>%
    summarise(
        n = n(),
        mean_weight = mean(weight),
        .groups = "drop"
    ) %>%
    print()

if (FREQUENCY == "yearly") {
    time_label <- "Years"
    # Set window based on post_window argument
    if (POST_WINDOW == "short") {
        min_period <- -5
        max_period <- 2
        x_breaks <- -5:2
    } else {
        min_period <- -5
        max_period <- 5
        x_breaks <- -5:5
    }
} else {
    time_label <- "Quarters"
    # Set window based on post_window argument
    if (POST_WINDOW == "short") {
        min_period <- -20
        max_period <- 8
        x_breaks <- seq(-20, 8, by = 4)
    } else {
        min_period <- -8
        max_period <- 16
        x_breaks <- seq(-8, 16, by = 4)
    }
}

message(sprintf("Post-period window: [%d, %d]", min_period, max_period))

# =============================================================================
# CREATE WARD_PAIR VARIABLE AND BUILD FE FORMULA
# =============================================================================
message("\n=== CREATING WARD_PAIR VARIABLE ===")

if (STACKED) {
    # cohort_ward_pair_side looks like "2015_13_23_13" (cohort, border, side)
    data <- data %>%
        mutate(
            ward_pair_side_temp = sub("^[0-9]+_", "", cohort_ward_pair_side),  # Remove cohort prefix
            ward_pair = sub("_[0-9]+$", "", ward_pair_side_temp),  # Remove side suffix
            cohort_ward_pair = paste(cohort, ward_pair, sep = "_")  # Add cohort back
        )
    fe_formula <- sprintf("cohort_ward_pair_side + cohort_ward_pair^%s", time_fe_var)
} else {
    data <- data %>%
        mutate(
            ward_pair = sub("_[0-9]+$", "", ward_pair_side)  # Remove side suffix
        )
    fe_formula <- sprintf("ward_pair_side + ward_pair^%s", time_fe_var)
}

message(sprintf("FE formula: %s", fe_formula))

# =============================================================================
# EFFECTIVE OBSERVATIONS DIAGNOSTIC
# =============================================================================
message("\n=== EFFECTIVE OBSERVATIONS DIAGNOSTIC ===")

# Create switcher indicator
data$is_switcher <- abs(data$strictness_change) > 0

# Count by ward_pair: need observations on BOTH sides
if (STACKED) {
    pair_summary <- data %>%
        group_by(cohort_ward_pair) %>%
        summarise(
            n_sides = n_distinct(cohort_ward_pair_side),
            n_obs = n(),
            n_switcher = sum(is_switcher),
            n_stayer = sum(!is_switcher),
            .groups = "drop"
        )
    identifying_pairs <- pair_summary %>% filter(n_sides == 2)
    effective_obs <- data %>%
        filter(cohort_ward_pair %in% identifying_pairs$cohort_ward_pair) %>%
        nrow()
} else {
    pair_summary <- data %>%
        group_by(ward_pair) %>%
        summarise(
            n_sides = n_distinct(ward_pair_side),
            n_obs = n(),
            n_switcher = sum(is_switcher),
            n_stayer = sum(!is_switcher),
            .groups = "drop"
        )
    identifying_pairs <- pair_summary %>% filter(n_sides == 2)
    effective_obs <- data %>%
        filter(ward_pair %in% identifying_pairs$ward_pair) %>%
        nrow()
}

message(sprintf("Total %s groups: %d", fe_group_var, nrow(pair_summary)))
message(sprintf("Pairs with observations on BOTH sides: %d (%.1f%%)", 
    nrow(identifying_pairs), 100 * nrow(identifying_pairs) / nrow(pair_summary)))
message(sprintf("Effective observations (in identifying pairs): %s of %s (%.1f%%)",
    format(effective_obs, big.mark = ","),
    format(nrow(data), big.mark = ","),
    100 * effective_obs / nrow(data)))
message(sprintf("Listings in switcher blocks: %s", format(sum(data$is_switcher), big.mark = ",")))
message(sprintf("Listings in stayer blocks: %s", format(sum(!data$is_switcher), big.mark = ",")))

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================
extract_iplot_data <- function(model, group_label) {
    iplot_data <- tryCatch(iplot(model, .plot = FALSE)[[1]], error = function(e) NULL)
    if (is.null(iplot_data) || nrow(iplot_data) == 0) {
        return(NULL)
    }
    iplot_data %>% 
        filter(x >= min_period & x <= max_period) %>%  # Apply window filtering
        mutate(
            group = group_label,
            estimate_pct = estimate * 100,
            ci_low_pct = ci_low * 100,
            ci_high_pct = ci_high * 100
        )
}

run_model <- function(formula_str, data_subset) {
    message(sprintf(
        "Running regression with %s observations (effective N: %.0f)...",
        format(nrow(data_subset), big.mark = ","),
        sum(data_subset$weight)
    ))
    message(sprintf("Formula: %s", formula_str))

    feols(as.formula(formula_str),
        data = data_subset,
        weights = ~weight,
        cluster = as.formula(sprintf("~%s", cluster_var))
    )
}

# =============================================================================
# RUN REGRESSIONS
# =============================================================================

if (TREATMENT_TYPE == "continuous") {
    message("\n=== Continuous Treatment ===")

    formula_str <- sprintf(
        "log(rent_price) ~ i(%s, treatment_continuous, ref = -1) %s | %s",
        time_var, hedonic_controls, fe_formula
    )
    m <- run_model(formula_str, data)
    print(summary(m))

    plot_data <- extract_iplot_data(m, "All Listings")

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

    # etable(list(m),
    #     fitstat = ~ n + r2,
    #     style.tex = style.tex("aer", model.format = "", fixef.title = "", fixef.suffix = "", yesNo = c("$\\checkmark$", "")),
    #     depvar = FALSE, digits = 3, headers = c("Continuous"),
    #     signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    #     notes = sprintf(
    #         "Listing-level regression. %s weighting with %dft bandwidth. SEs clustered by block.%s",
    #         tools::toTitleCase(WEIGHTING), as.integer(BANDWIDTH),
    #         ifelse(SAMPLE_FILTER == "multifamily_only", " Multifamily buildings only.", "")
    #     ),
    #     float = FALSE, file = sprintf("../output/did_table_%s.tex", suffix), replace = TRUE
    # )
    # message(sprintf("Saved: ../output/did_table_%s.tex", suffix))
} else if (TREATMENT_TYPE == "binary_direction") {
    message("\n=== Binary Direction Treatment ===")

    # Stricter: compare stricter-bound listings to control listings
    message("\n--- Moved to Stricter ---")
    data_for_stricter <- data %>%
        filter(strictness_change > 0 | treat == 0)
    formula_stricter <- sprintf(
        "log(rent_price) ~ i(%s, treat_stricter, ref = -1) %s | %s",
        time_var, hedonic_controls, fe_formula
    )
    m_stricter <- run_model(formula_stricter, data_for_stricter)
    print(summary(m_stricter))

    # Lenient: compare lenient-bound listings to control listings
    message("\n--- Moved to More Lenient ---")
    data_for_lenient <- data %>%
        filter(strictness_change < 0 | treat == 0)
    formula_lenient <- sprintf(
        "log(rent_price) ~ i(%s, treat_lenient, ref = -1) %s | %s",
        time_var, hedonic_controls, fe_formula
    )
    m_lenient <- run_model(formula_lenient, data_for_lenient)
    print(summary(m_lenient))

    plot_data <- bind_rows(
        extract_iplot_data(m_stricter, "Moved to Stricter"),
        extract_iplot_data(m_lenient, "Moved to More Lenient")
    ) %>% filter(!is.na(estimate))

    if (nrow(plot_data) > 0) {
        # Faceted plot
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

        # Combined plot (both series on same axes)
        p_combined <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
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
            theme_minimal(base_size = 11) +
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
                axis.line = element_line(color = "gray40", linewidth = 0.3),
                axis.ticks = element_line(color = "gray40", linewidth = 0.3),
                axis.ticks.length = unit(0.15, "cm"),
                axis.title = element_text(size = 10, color = "gray20"),
                axis.text = element_text(size = 9, color = "gray30"),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.text = element_text(size = 9),
                legend.key.width = unit(1.5, "cm"),
                legend.margin = margin(t = 5, b = 0),
                plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
            ) +
            guides(
                color = guide_legend(override.aes = list(linewidth = c(1.2, 0.8), size = c(3, 2))),
                fill = guide_legend(override.aes = list(alpha = c(0.2, 0.15)))
            )

        ggsave(sprintf("../output/event_study_combined_%s.pdf", suffix), p_combined, width = 7, height = 4.5, bg = "white")
        message(sprintf("Saved: ../output/event_study_combined_%s.pdf", suffix))
    }

    # etable(list(m_stricter, m_lenient),
    #     fitstat = ~ n + r2,
    #     style.tex = style.tex("aer", model.format = "", fixef.title = "", fixef.suffix = "", yesNo = c("$\\checkmark$", "")),
    #     depvar = FALSE, digits = 3, headers = c("To Stricter", "To More Lenient"),
    #     signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    #     notes = sprintf(
    #         "Listing-level regression. %s weighting with %dft bandwidth. SEs clustered by block.%s",
    #         tools::toTitleCase(WEIGHTING), as.integer(BANDWIDTH),
    #         ifelse(SAMPLE_FILTER == "multifamily_only", " Multifamily buildings only.", "")
    #     ),
    #     float = FALSE, file = sprintf("../output/did_table_%s.tex", suffix), replace = TRUE
    # )
    message(sprintf("Saved: ../output/did_table_%s.tex", suffix))
} else if (TREATMENT_TYPE == "continuous_split") {
    message("\n=== Continuous Split Treatment ===")
    message("Running two regressions on FULL sample with continuous treatment intensity")
    message(sprintf("Full sample size: %s observations", format(nrow(data), big.mark = ",")))
    message(sprintf("  - Stricter-movers (Δ > 0): %s", format(sum(data$strictness_change > 0), big.mark = ",")))
    message(sprintf("  - Lenient-movers (Δ < 0): %s", format(sum(data$strictness_change < 0), big.mark = ",")))
    message(sprintf("  - Controls (Δ = 0): %s", format(sum(data$strictness_change == 0), big.mark = ",")))

    # Stricter regression: full sample, continuous treatment
    message("\n--- Moved to Stricter (Continuous) ---")
    formula_stricter <- sprintf(
        "log(rent_price) ~ i(%s, treatment_stricter_continuous, ref = -1) %s | %s",
        time_var, hedonic_controls, fe_formula
    )
    m_stricter <- run_model(formula_stricter, data)  # Note: full data, no subsetting
    print(summary(m_stricter))

    # Lenient regression: full sample, continuous treatment
    message("\n--- Moved to More Lenient (Continuous) ---")
    formula_lenient <- sprintf(
        "log(rent_price) ~ i(%s, treatment_lenient_continuous, ref = -1) %s | %s",
        time_var, hedonic_controls, fe_formula
    )
    m_lenient <- run_model(formula_lenient, data)  # Note: full data, no subsetting
    print(summary(m_lenient))

    # Extract plot data
    plot_data <- bind_rows(
        extract_iplot_data(m_stricter, "Moved to Stricter (Continuous)"),
        extract_iplot_data(m_lenient, "Moved to More Lenient (Continuous)")
    ) %>% filter(!is.na(estimate))

    if (nrow(plot_data) > 0) {
        # Faceted plot
        p <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
            geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
            geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
            geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5, shape = 21, stroke = 0.5) +
            scale_color_manual(
                values = c("Moved to Stricter (Continuous)" = "#c23616", 
                          "Moved to More Lenient (Continuous)" = "#7f8fa6"),
                name = NULL
            ) +
            scale_fill_manual(
                values = c("Moved to Stricter (Continuous)" = "#c23616", 
                          "Moved to More Lenient (Continuous)" = "#7f8fa6"),
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

        # Combined plot (both series on same axes)
        p_combined <- ggplot(plot_data, aes(x = x, y = estimate_pct, color = group, fill = group)) +
            geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.4) +
            geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
            geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5, shape = 21, stroke = 0.5) +
            scale_color_manual(
                values = c("Moved to Stricter (Continuous)" = "#c23616", 
                          "Moved to More Lenient (Continuous)" = "#7f8fa6"),
                labels = c("Moved to Stricter (Continuous)" = "Moved to Stricter Alderman", 
                          "Moved to More Lenient (Continuous)" = "Moved to More Lenient Alderman"),
                name = NULL
            ) +
            scale_fill_manual(
                values = c("Moved to Stricter (Continuous)" = "#c23616", 
                          "Moved to More Lenient (Continuous)" = "#7f8fa6"),
                labels = c("Moved to Stricter (Continuous)" = "Moved to Stricter Alderman", 
                          "Moved to More Lenient (Continuous)" = "Moved to More Lenient Alderman"),
                name = NULL
            ) +
            scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.02, 0.02))) +
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
                axis.ticks.length = unit(0.15, "cm"),
                axis.title = element_text(size = 10, color = "gray20"),
                axis.text = element_text(size = 9, color = "gray30"),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.text = element_text(size = 9),
                legend.key.width = unit(1.5, "cm"),
                legend.margin = margin(t = 5, b = 0),
                plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
            )

        ggsave(sprintf("../output/event_study_combined_%s.pdf", suffix), p_combined, width = 7, height = 4.5, bg = "white")
        message(sprintf("Saved: ../output/event_study_combined_%s.pdf", suffix))
    }
}

message("\n\nDone!")
