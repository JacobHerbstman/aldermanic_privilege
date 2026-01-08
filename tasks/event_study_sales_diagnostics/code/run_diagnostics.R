# run_diagnostics.R
# Diagnostics for event study on home sales around ward redistricting
# Generates: summary tables, ward pair diagnostics, covariate balance,
#            treatment/control maps, pre-trend tests

source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading data...")

# Yearly stacked panel (main analysis panel)
stacked_yearly <- read_csv("../input/sales_stacked_panel.csv", show_col_types = FALSE) %>%
    mutate(block_id = as.character(block_id)) %>%
    filter(n_sales > 0, !is.na(strictness_change)) %>%
    filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000)

# Quarterly stacked panel
stacked_quarterly <- read_csv("../input/sales_stacked_quarterly_panel.csv", show_col_types = FALSE) %>%
    mutate(block_id = as.character(block_id)) %>%
    filter(n_sales > 0, !is.na(strictness_change)) %>%
    filter(!is.na(ward_pair_id), mean_dist_to_boundary < 1000)

# Census blocks for mapping
census_blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    rename(block_id = GEOID10) %>%
    mutate(block_id = as.character(block_id))

# Ward boundaries
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

# Transform census blocks to match ward panel CRS
census_blocks <- st_transform(census_blocks, st_crs(ward_panel))

# Treatment panel for additional diagnostics
treatment_panel <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE) %>%
    mutate(block_id = as.character(block_id))

message(sprintf("Yearly panel: %s observations", format(nrow(stacked_yearly), big.mark = ",")))
message(sprintf("Quarterly panel: %s observations", format(nrow(stacked_quarterly), big.mark = ",")))

# =============================================================================
# 2. SAMPLE SUMMARY STATISTICS
# =============================================================================
message("\nCreating sample summary statistics...")

# Treatment categories
categorize_treatment <- function(strictness_change) {
    case_when(
        strictness_change > 0 ~ "Moved to Stricter",
        strictness_change < 0 ~ "Moved to Lenient",
        TRUE ~ "Control (No Change)"
    )
}

# Yearly summary by cohort and treatment
yearly_summary <- stacked_yearly %>%
    mutate(treatment_group = categorize_treatment(strictness_change)) %>%
    group_by(cohort, treatment_group) %>%
    summarise(
        n_blocks = n_distinct(block_id),
        n_block_years = n(),
        total_sales = sum(n_sales),
        mean_strictness_change = mean(strictness_change, na.rm = TRUE),
        mean_price = weighted.mean(mean_price, n_sales, na.rm = TRUE),
        mean_dist_boundary = mean(mean_dist_to_boundary, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(cohort, treatment_group)

# Create LaTeX table
yearly_tex <- yearly_summary %>%
    mutate(
        cohort = as.character(cohort),
        mean_price = scales::dollar(mean_price, accuracy = 1),
        mean_strictness_change = sprintf("%.3f", mean_strictness_change),
        mean_dist_boundary = sprintf("%.0f", mean_dist_boundary)
    )

cat("\\begin{table}[htbp]
\\centering
\\caption{Sample Summary Statistics by Treatment Group}
\\label{tab:sample_summary}
\\begin{tabular}{llrrrrr}
\\toprule
Cohort & Treatment Group & Blocks & Block-Years & Sales & Mean $\\Delta$Strict & Mean Price \\\\
\\midrule
", file = "../output/sample_summary.tex")

for (i in seq_len(nrow(yearly_tex))) {
    row <- yearly_tex[i, ]
    cat(
        sprintf(
            "%s & %s & %s & %s & %s & %s & %s \\\\\n",
            row$cohort, row$treatment_group,
            format(row$n_blocks, big.mark = ","),
            format(row$n_block_years, big.mark = ","),
            format(row$total_sales, big.mark = ","),
            row$mean_strictness_change, row$mean_price
        ),
        file = "../output/sample_summary.tex", append = TRUE
    )
}

cat("\\bottomrule
\\end{tabular}
\\begin{tablenotes}
\\small
\\item \\textit{Notes:} Sample restricted to blocks within 1000 feet of ward boundaries with positive sales.
Strictness change uses predetermined approach: both origin and destination ward strictness measured in year before redistricting.
Controls are blocks that stayed in same ward where the alderman did not change; non-switchers in wards with electoral turnover are excluded.
\\end{tablenotes}
\\end{table}
", file = "../output/sample_summary.tex", append = TRUE)

message("Saved: ../output/sample_summary.tex")

# =============================================================================
# 2.5 IDENTIFICATION STRATEGY VALIDATION
# =============================================================================
message("\nValidating identification strategy (predetermined strictness approach)...")

# With predetermined strictness:
# - Treatment = strictly redistricted blocks (ward changed)
# - Controls = non-switchers whose ward had NO electoral turnover
# - Contaminated controls (non-switchers in turnover wards) are already EXCLUDED

# Validate that all significant strictness changes come from redistricted blocks
id_validation <- stacked_yearly %>%
    filter(relative_year == 0) %>% # Focus on treatment year
    mutate(
        has_strictness_change = abs(strictness_change) > 0,
        # redistricted indicates ward boundary changed
        status = case_when(
            redistricted == TRUE & has_strictness_change ~ "Treated (Redistricted)",
            redistricted == TRUE & !has_strictness_change ~ "Redistricted, No Strictness Change",
            redistricted == FALSE & !has_strictness_change ~ "Control (Clean)",
            redistricted == FALSE & has_strictness_change ~ "ERROR: Non-switcher with change"
        )
    )

# Summary by identification status
id_summary <- id_validation %>%
    group_by(cohort, status) %>%
    summarise(
        n_blocks = n_distinct(block_id),
        total_sales = sum(n_sales),
        mean_strictness_change = mean(strictness_change, na.rm = TRUE),
        mean_price = weighted.mean(mean_price, n_sales, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(cohort, status)

# Print to console for validation
message("\nIdentification Strategy Validation:")
print(id_summary)

# Check for any errors (non-switchers with strictness change)
n_errors <- sum(id_validation$status == "ERROR: Non-switcher with change", na.rm = TRUE)
if (n_errors > 0) {
    warning(sprintf("IDENTIFICATION ERROR: %d non-switching blocks have strictness change!", n_errors))
} else {
    message("✓ Identification validated: all strictness changes come from redistricted blocks only")
}

# LaTeX table
cat("\\begin{table}[htbp]
\\centering
\\caption{Identification Strategy Validation}
\\label{tab:identification_validation}
\\begin{tabular}{llrrrr}
\\toprule
Cohort & Status & Blocks & Sales & Mean $\\Delta$Strict & Mean Price \\\\
\\midrule
", file = "../output/identification_validation.tex")

for (i in seq_len(nrow(id_summary))) {
    row <- id_summary[i, ]
    cat(
        sprintf(
            "%s & %s & %s & %s & %.3f & %s \\\\\\n",
            row$cohort, row$status,
            format(row$n_blocks, big.mark = ","),
            format(row$total_sales, big.mark = ","),
            row$mean_strictness_change,
            scales::dollar(row$mean_price, accuracy = 1)
        ),
        file = "../output/identification_validation.tex", append = TRUE
    )
}

cat("\\bottomrule
\\end{tabular}
\\begin{tablenotes}
\\small
\\item \\textit{Notes:} Validates the predetermined strictness identification strategy.
``Treated'' = blocks whose ward changed due to redistricting.
``Control (Clean)'' = blocks that stayed in same ward where the alderman did not change.
Non-switchers in wards with electoral turnover are excluded from the sample.
Strictness change uses predetermined approach: both origin and destination ward strictness measured in year before redistricting.
\\end{tablenotes}
\\end{table}
", file = "../output/identification_validation.tex", append = TRUE)

message("Saved: ../output/identification_validation.tex")

# Also save detailed CSV
id_by_block <- id_validation %>%
    select(
        block_id, cohort, redistricted, strictness_change, status,
        n_sales, mean_price, ward_pair_id
    ) %>%
    arrange(cohort, status, desc(abs(strictness_change)))

write_csv(id_by_block, "../output/identification_validation_detail.csv")
message("Saved: ../output/identification_validation_detail.csv")

# =============================================================================
# 3. WARD PAIR DIAGNOSTICS
# =============================================================================
message("\nCreating ward pair diagnostics...")

# Calculate variation within each ward pair
ward_pair_diag <- stacked_yearly %>%
    filter(cohort == "2015") %>% # Focus on 2015 for clarity
    group_by(ward_pair_id) %>%
    summarise(
        n_blocks = n_distinct(block_id),
        n_block_years = n(),
        total_sales = sum(n_sales),
        mean_strictness_change = mean(strictness_change, na.rm = TRUE),
        sd_strictness_change = sd(strictness_change, na.rm = TRUE),
        range_strictness = max(strictness_change) - min(strictness_change),
        n_stricter = sum(strictness_change > 0),
        n_lenient = sum(strictness_change < 0),
        n_control = sum(strictness_change == 0),
        .groups = "drop"
    ) %>%
    filter(!is.na(sd_strictness_change), sd_strictness_change > 0) %>%
    arrange(desc(sd_strictness_change))

# Top 15 ward pairs by variation
top_pairs <- head(ward_pair_diag, 15)

cat("\\begin{table}[htbp]
\\centering
\\caption{Ward Pair Diagnostics: Variation in Treatment (2015 Cohort)}
\\label{tab:ward_pair_diagnostics}
\\small
\\begin{tabular}{lrrrrrr}
\\toprule
Ward Pair & Blocks & Sales & SD($\\Delta$Strict) & Stricter & Lenient & Control \\\\
\\midrule
", file = "../output/ward_pair_diagnostics.tex")

for (i in seq_len(nrow(top_pairs))) {
    row <- top_pairs[i, ]
    cat(
        sprintf(
            "%s & %d & %s & %.3f & %d & %d & %d \\\\\n",
            row$ward_pair_id, row$n_blocks,
            format(row$total_sales, big.mark = ","),
            row$sd_strictness_change,
            row$n_stricter, row$n_lenient, row$n_control
        ),
        file = "../output/ward_pair_diagnostics.tex", append = TRUE
    )
}

cat("\\bottomrule
\\end{tabular}
\\begin{tablenotes}
\\small
\\item \\textit{Notes:} Top 15 ward pairs ranked by standard deviation of strictness change within pair.
``Stricter'' = $\\Delta$Strict $> 0$, ``Lenient'' = $\\Delta$Strict $< 0$, ``Control'' = $\\Delta$Strict $= 0$.
\\end{tablenotes}
\\end{table}
", file = "../output/ward_pair_diagnostics.tex", append = TRUE)

message("Saved: ../output/ward_pair_diagnostics.tex")

# =============================================================================
# 4. COVARIATE BALANCE TABLE
# =============================================================================
message("\nCreating covariate balance table...")

# Pre-period (t = -1) balance check
balance_data <- stacked_yearly %>%
    filter(relative_year == -1) %>%
    mutate(treatment_group = categorize_treatment(strictness_change))

balance_summary <- balance_data %>%
    group_by(treatment_group) %>%
    summarise(
        n_blocks = n(),
        mean_price = mean(mean_price, na.rm = TRUE),
        sd_price = sd(mean_price, na.rm = TRUE),
        mean_sales = mean(n_sales, na.rm = TRUE),
        mean_dist = mean(mean_dist_to_boundary, na.rm = TRUE),
        .groups = "drop"
    )

# T-tests for balance (stricter vs lenient)
stricter_data <- balance_data %>% filter(treatment_group == "Moved to Stricter")
lenient_data <- balance_data %>% filter(treatment_group == "Moved to Lenient")
control_data <- balance_data %>% filter(treatment_group == "Control (No Change)")

price_test <- t.test(stricter_data$mean_price, lenient_data$mean_price)
sales_test <- t.test(stricter_data$n_sales, lenient_data$n_sales)
dist_test <- t.test(stricter_data$mean_dist_to_boundary, lenient_data$mean_dist_to_boundary)

cat("\\begin{table}[htbp]
\\centering
\\caption{Covariate Balance at $t = -1$ (Year Before Redistricting)}
\\label{tab:covariate_balance}
\\begin{tabular}{lrrrrr}
\\toprule
 & \\multicolumn{3}{c}{Mean by Group} & \\multicolumn{2}{c}{Stricter vs Lenient} \\\\
\\cmidrule(lr){2-4} \\cmidrule(lr){5-6}
Variable & Stricter & Lenient & Control & Diff & $p$-value \\\\
\\midrule
", file = "../output/covariate_balance.tex")

# Price row
cat(
    sprintf(
        "Sale Price (\\$) & %s & %s & %s & %s & %.3f \\\\\n",
        scales::dollar(balance_summary$mean_price[balance_summary$treatment_group == "Moved to Stricter"], accuracy = 1),
        scales::dollar(balance_summary$mean_price[balance_summary$treatment_group == "Moved to Lenient"], accuracy = 1),
        scales::dollar(balance_summary$mean_price[balance_summary$treatment_group == "Control (No Change)"], accuracy = 1),
        scales::dollar(price_test$estimate[1] - price_test$estimate[2], accuracy = 1),
        price_test$p.value
    ),
    file = "../output/covariate_balance.tex", append = TRUE
)

# Sales row
cat(
    sprintf(
        "N Sales & %.2f & %.2f & %.2f & %.2f & %.3f \\\\\n",
        balance_summary$mean_sales[balance_summary$treatment_group == "Moved to Stricter"],
        balance_summary$mean_sales[balance_summary$treatment_group == "Moved to Lenient"],
        balance_summary$mean_sales[balance_summary$treatment_group == "Control (No Change)"],
        sales_test$estimate[1] - sales_test$estimate[2],
        sales_test$p.value
    ),
    file = "../output/covariate_balance.tex", append = TRUE
)

# Distance row
cat(
    sprintf(
        "Dist to Boundary (ft) & %.0f & %.0f & %.0f & %.0f & %.3f \\\\\n",
        balance_summary$mean_dist[balance_summary$treatment_group == "Moved to Stricter"],
        balance_summary$mean_dist[balance_summary$treatment_group == "Moved to Lenient"],
        balance_summary$mean_dist[balance_summary$treatment_group == "Control (No Change)"],
        dist_test$estimate[1] - dist_test$estimate[2],
        dist_test$p.value
    ),
    file = "../output/covariate_balance.tex", append = TRUE
)

# N blocks row
cat(
    sprintf(
        "N Blocks & %d & %d & %d & -- & -- \\\\\n",
        balance_summary$n_blocks[balance_summary$treatment_group == "Moved to Stricter"],
        balance_summary$n_blocks[balance_summary$treatment_group == "Moved to Lenient"],
        balance_summary$n_blocks[balance_summary$treatment_group == "Control (No Change)"]
    ),
    file = "../output/covariate_balance.tex", append = TRUE
)

cat("\\bottomrule
\\end{tabular}
\\begin{tablenotes}
\\small
\\item \\textit{Notes:} Balance measured at $t = -1$ (one year before redistricting).
$p$-values from two-sample $t$-tests comparing blocks moving to stricter vs.~more lenient alderman.
Strictness change uses predetermined approach (both wards measured pre-redistricting).
\\end{tablenotes}
\\end{table}
", file = "../output/covariate_balance.tex", append = TRUE)

message("Saved: ../output/covariate_balance.tex")

# =============================================================================
# 5. TREATMENT/CONTROL MAPS
# =============================================================================
message("\nCreating treatment/control maps...")

# Get block-level treatment assignments
block_treatment <- stacked_yearly %>%
    filter(relative_year == 0) %>%
    mutate(treatment_group = categorize_treatment(strictness_change)) %>%
    select(block_id, cohort, treatment_group, strictness_change, ward_pair_id) %>%
    distinct()

# Merge with census block geometry
blocks_for_map <- census_blocks %>%
    inner_join(block_treatment, by = "block_id")

# Get ward boundaries for overlay
create_treatment_map <- function(cohort_year, ward_year, include_title = TRUE) {
    # Filter to cohort
    cohort_blocks <- blocks_for_map %>%
        filter(cohort == as.character(cohort_year))

    # Get ward boundaries
    wards <- ward_panel %>%
        filter(year == ward_year)

    # Create map
    p <- ggplot() +
        geom_sf(data = wards, fill = NA, color = "gray50", linewidth = 0.3) +
        geom_sf(data = cohort_blocks, aes(fill = treatment_group), color = NA, alpha = 1) +
        scale_fill_manual(
            values = c(
                "Control (No Change)" = "#D9D9D9",
                "Moved to Lenient" = "#4DABF7",
                "Moved to Stricter" = "#E63946"
            ),
            name = NULL
        ) +
        coord_sf(expand = FALSE) +
        theme_void(base_size = 11) +
        theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            plot.margin = margin(2, 2, 2, 2)
        )

    if (include_title) {
        p <- p + labs(
            title = sprintf("%d Redistricting", cohort_year),
            subtitle = sprintf(
                "N = %s blocks within 1,000 ft of ward boundaries",
                format(nrow(cohort_blocks), big.mark = ",")
            )
        )
    }

    p
}

# Individual maps (for reference)
p_2015 <- create_treatment_map(2015, 2015)
ggsave("../output/treatment_control_map_2015.pdf", p_2015, width = 8, height = 10, bg = "white")
message("Saved: ../output/treatment_control_map_2015.pdf")

p_2023 <- create_treatment_map(2023, 2024)
ggsave("../output/treatment_control_map_2023.pdf", p_2023, width = 8, height = 10, bg = "white")
message("Saved: ../output/treatment_control_map_2023.pdf")

# =========================================================================
# Combined citywide figure: both years side by side (simple approach)
# =========================================================================

# Create maps - hide legend on first, show on second for shared effect
p_2015_for_combine <- create_treatment_map(2015, 2015, include_title = FALSE) +
    labs(title = "2015 Redistricting") +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10, margin = margin(b = 10)),
        legend.position = "none",
        plot.margin = margin(5, 5, 15, 5)
    )

p_2023_for_combine <- create_treatment_map(2023, 2024, include_title = FALSE) +
    labs(title = "2023 Redistricting") +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10, margin = margin(b = 15)),
        legend.position = "bottom",
        legend.justification = "right",
        legend.text = element_text(size = 8),
        legend.margin = margin(t = 10),
        plot.margin = margin(5, 5, 5, 5)
    ) +
    guides(fill = guide_legend(nrow = 1))

# Simple side by side combination
combined_citywide <- p_2015_for_combine + p_2023_for_combine

# Save - use dimensions that match Chicago's tall/narrow shape (wider to fit legend)
ggsave("../output/treatment_control_maps_combined.pdf", combined_citywide,
    width = 5, height = 4, bg = "white"
)
message("Saved: ../output/treatment_control_maps_combined.pdf")

# =============================================================================
# 6. BEFORE/AFTER WARD PAIR MAPS
# =============================================================================
message("\nCreating before/after ward pair maps...")

# For before/after maps, use treatment_panel directly to get blocks
# Join with census_blocks for geometry
treatment_2015 <- treatment_panel %>%
    filter(cohort == "2015") %>%
    mutate(treatment_group = categorize_treatment(strictness_change))

# Get block distances from stacked panel (blocks near boundaries)
# Filter to 1000 ft from boundary
block_distances <- stacked_yearly %>%
    filter(cohort == "2015") %>%
    group_by(block_id) %>%
    summarise(
        mean_dist_to_boundary = mean(mean_dist_to_boundary, na.rm = TRUE),
        ward_pair_id = first(ward_pair_id),
        .groups = "drop"
    ) %>%
    filter(mean_dist_to_boundary < 1000) # 1000 ft filter

# Join treatment data with distance-filtered blocks
all_blocks_for_map <- census_blocks %>%
    inner_join(treatment_2015, by = "block_id") %>%
    inner_join(block_distances, by = "block_id")

message(sprintf("  Blocks within 1000 ft of boundary: %d", nrow(all_blocks_for_map)))

# Ward pairs to map - top 3 by variation plus extras
top_3_pairs <- head(ward_pair_diag$ward_pair_id, 3)
extra_pairs <- c("44-46") # Additional ward pairs to include
all_pairs_to_map <- unique(c(top_3_pairs, extra_pairs))

# Get ward polygons for before/after
wards_2014 <- ward_panel %>% filter(year == 2014) # Pre-redistricting
wards_2015 <- ward_panel %>% filter(year == 2015) # Post-redistricting

for (wp in all_pairs_to_map) {
    # Parse ward pair
    wards_in_pair <- as.numeric(strsplit(wp, "-")[[1]])
    ward_a <- wards_in_pair[1]
    ward_b <- wards_in_pair[2]

    # Get blocks that are in this ward pair (either origin or destination is one of these wards)
    wp_blocks <- all_blocks_for_map %>%
        filter(
            (ward_origin == ward_a & ward_dest == ward_b) |
                (ward_origin == ward_b & ward_dest == ward_a) |
                (ward_origin %in% c(ward_a, ward_b) & ward_dest %in% c(ward_a, ward_b))
        )

    message(sprintf("  Processing ward pair %s: %d blocks found", wp, nrow(wp_blocks)))

    if (nrow(wp_blocks) == 0) {
        message("    No blocks found, skipping...")
        next
    }

    # Debug info
    n_switchers <- sum(wp_blocks$switched, na.rm = TRUE)
    n_controls <- sum(!wp_blocks$switched, na.rm = TRUE)
    message(sprintf("    Switchers: %d, Non-switchers: %d", n_switchers, n_controls))

    # Get bounding box with small buffer
    bbox <- st_bbox(wp_blocks)
    x_range <- as.numeric(bbox["xmax"]) - as.numeric(bbox["xmin"])
    y_range <- as.numeric(bbox["ymax"]) - as.numeric(bbox["ymin"])
    buffer <- max(x_range, y_range) * 0.1 # Small buffer for tighter zoom

    xmin_exp <- as.numeric(bbox["xmin"]) - buffer
    ymin_exp <- as.numeric(bbox["ymin"]) - buffer
    xmax_exp <- as.numeric(bbox["xmax"]) + buffer
    ymax_exp <- as.numeric(bbox["ymax"]) + buffer

    # Crop ward polygons to view area
    wards_2014_crop <- st_crop(wards_2014, xmin = xmin_exp, ymin = ymin_exp, xmax = xmax_exp, ymax = ymax_exp)
    wards_2015_crop <- st_crop(wards_2015, xmin = xmin_exp, ymin = ymin_exp, xmax = xmax_exp, ymax = ymax_exp)

    # Color blocks by their ward assignment
    ward_nums <- sort(unique(c(wp_blocks$ward_origin, wp_blocks$ward_dest)))
    ward_colors <- setNames(c("#D62728", "#1F77B4"), as.character(ward_nums))

    # Common theme for all panels
    map_theme <- theme_void(base_size = 12) +
        theme(
            legend.position = "right",
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 11),
            plot.margin = margin(10, 10, 10, 10)
        )

    # BEFORE: color by origin ward
    p_before <- ggplot() +
        geom_sf(data = wards_2014_crop, fill = NA, color = "black", linewidth = 1.0) +
        geom_sf(data = wp_blocks, aes(fill = factor(ward_origin)), color = "gray30", linewidth = 0.2, alpha = 1) +
        scale_fill_manual(values = ward_colors, name = "Ward") +
        labs(
            title = "Before Redistricting (2014 Ward Boundaries)",
            subtitle = "Blocks colored by pre-redistricting ward assignment"
        ) +
        map_theme

    # AFTER: color by destination ward
    p_after <- ggplot() +
        geom_sf(data = wards_2015_crop, fill = NA, color = "black", linewidth = 1.0) +
        geom_sf(data = wp_blocks, aes(fill = factor(ward_dest)), color = "gray30", linewidth = 0.2, alpha = 1) +
        scale_fill_manual(values = ward_colors, name = "Ward") +
        labs(
            title = "After Redistricting (2015 Ward Boundaries)",
            subtitle = "Blocks colored by post-redistricting ward assignment"
        ) +
        map_theme

    # TREATMENT: color by treatment group
    p_treatment <- ggplot() +
        geom_sf(data = wards_2015_crop, fill = NA, color = "black", linewidth = 1.0) +
        geom_sf(data = wp_blocks, aes(fill = treatment_group), color = "gray30", linewidth = 0.2, alpha = 1) +
        scale_fill_manual(
            values = c(
                "Moved to Stricter" = "#D62728",
                "Moved to Lenient" = "#1F77B4",
                "Control (No Change)" = "#999999"
            ),
            name = "Treatment"
        ) +
        labs(
            title = "Treatment Status",
            subtitle = "Based on change in alderman strictness"
        ) +
        map_theme

    # =========================================================================
    # VERTICAL LAYOUT: Stack 3 maps vertically for full-width display
    # =========================================================================
    combined_vertical <- p_before / p_after / p_treatment +
        plot_annotation(
            title = sprintf("Identification Example: Ward Pair %d–%d (2015 Redistricting)", ward_a, ward_b),
            subtitle = sprintf(
                "%d blocks total: %d switched wards, %d remained in place",
                nrow(wp_blocks), n_switchers, n_controls
            ),
            theme = theme(
                plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                plot.subtitle = element_text(hjust = 0.5, size = 12)
            )
        )

    # Save VERTICAL version (full page width, fits on single page)
    outfile_vertical <- sprintf("../output/ward_pair_vertical_%s.pdf", gsub("_", "-", wp))
    ggsave(outfile_vertical, combined_vertical, width = 8, height = 10, bg = "white")
    message(sprintf("    Saved: %s", outfile_vertical))

    # =========================================================================
    # Also save original HORIZONTAL layout for reference
    # =========================================================================
    combined_horizontal <- p_before + p_after + p_treatment +
        plot_layout(ncol = 3) +
        plot_annotation(
            title = sprintf("Ward Pair %s: Before vs After Redistricting", wp),
            subtitle = sprintf(
                "%d blocks total | %d switched wards | %d stayed",
                nrow(wp_blocks), n_switchers, n_controls
            ),
            theme = theme(
                plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                plot.subtitle = element_text(hjust = 0.5, size = 11)
            )
        )

    outfile_horizontal <- sprintf("../output/ward_pair_before_after_%s.pdf", gsub("_", "-", wp))
    ggsave(outfile_horizontal, combined_horizontal, width = 15, height = 6, bg = "white")
    message(sprintf("    Saved: %s", outfile_horizontal))
}

# =============================================================================
# 7. PRE-TREND DIAGNOSTICS
# =============================================================================
message("\nRunning pre-trend diagnostics...")

# Run the event study regression to get coefficients
yearly_for_reg <- stacked_yearly %>%
    mutate(
        treatment_continuous = strictness_change,
        relative_time_capped = pmax(pmin(relative_year, 5), -5)
    )

# Event study regression
fe_formula <- "cohort_ward_pair + cohort^year"
model <- feols(
    log(mean_price) ~ i(relative_time_capped, treatment_continuous, ref = -1) | cohort_ward_pair + cohort^year,
    data = yearly_for_reg,
    weights = ~n_sales,
    cluster = ~cohort_block_id
)

# Extract coefficients
coef_table <- broom::tidy(model, conf.int = TRUE) %>%
    filter(str_detect(term, "relative_time_capped")) %>%
    mutate(
        time = as.numeric(str_extract(term, "-?\\d+")),
        is_pre = time < 0
    )

# Pre-period coefficients
pre_coefs <- coef_table %>% filter(is_pre)

# Joint test of pre-trends = 0
pre_terms <- pre_coefs$term
if (length(pre_terms) > 1) {
    joint_test <- wald(model, pre_terms)
    f_stat <- joint_test$stat
    f_pval <- joint_test$p
} else {
    f_stat <- NA
    f_pval <- NA
}

# LaTeX table
cat("\\begin{table}[htbp]
\\centering
\\caption{Pre-Trend Diagnostic Tests}
\\label{tab:pre_trend_tests}
\\begin{tabular}{lrrrr}
\\toprule
Period & Coefficient & Std. Error & 95\\% CI & \\\\
\\midrule
", file = "../output/pre_trend_tests.tex")

for (i in seq_len(nrow(pre_coefs))) {
    row <- pre_coefs[i, ]
    cat(
        sprintf(
            "$t = %d$ & %.4f & %.4f & [%.4f, %.4f] \\\\\n",
            row$time, row$estimate, row$std.error, row$conf.low, row$conf.high
        ),
        file = "../output/pre_trend_tests.tex", append = TRUE
    )
}

cat(sprintf("\\midrule
\\multicolumn{5}{l}{Joint $F$-test of pre-trends = 0: $F$ = %.2f, $p$ = %.4f} \\\\
\\bottomrule
\\end{tabular}
\\begin{tablenotes}
\\small
\\item \\textit{Notes:} Coefficients from event study specification with continuous treatment (strictness change).
Pre-period is $t < 0$ where $t = 0$ is year of redistricting. Reference period is $t = -1$.
\\end{tablenotes}
\\end{table}
", f_stat, f_pval), file = "../output/pre_trend_tests.tex", append = TRUE)

message("Saved: ../output/pre_trend_tests.tex")

# Pre-trend coefficient plot
p_pretrend <- ggplot(coef_table, aes(x = time, y = estimate * 100)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray70") +
    geom_errorbar(aes(ymin = conf.low * 100, ymax = conf.high * 100),
        width = 0.2, color = "#009E73"
    ) +
    geom_point(aes(fill = is_pre), size = 3, shape = 21, color = "white") +
    scale_fill_manual(
        values = c("TRUE" = "#E69F00", "FALSE" = "#009E73"),
        labels = c("TRUE" = "Pre-Period", "FALSE" = "Post-Period"),
        name = ""
    ) +
    scale_x_continuous(breaks = -5:5) +
    labs(
        title = "Pre-Trend Check: Event Study Coefficients",
        subtitle = sprintf("Joint F-test of pre-trends = 0: F = %.2f, p = %.4f", f_stat, f_pval),
        x = "Years Relative to Redistricting",
        y = "Effect per 1-Unit Increase in Strictness (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
    )

ggsave("../output/pre_trend_coefficients.pdf", p_pretrend, width = 10, height = 6, bg = "white")
message("Saved: ../output/pre_trend_coefficients.pdf")

# =============================================================================
# 8. SAVE DETAILED DIAGNOSTIC DATA
# =============================================================================
message("\nSaving detailed diagnostic data...")

# Block-level summary for manual inspection
block_summary <- stacked_yearly %>%
    filter(relative_year == 0) %>%
    select(
        block_id, cohort, ward_pair_id, strictness_change,
        n_sales, mean_price, mean_dist_to_boundary
    ) %>%
    mutate(treatment_group = categorize_treatment(strictness_change)) %>%
    arrange(ward_pair_id, desc(abs(strictness_change)))

write_csv(block_summary, "../output/block_level_summary.csv")
message("Saved: ../output/block_level_summary.csv")

# Ward pair summary
write_csv(ward_pair_diag, "../output/ward_pair_summary.csv")
message("Saved: ../output/ward_pair_summary.csv")

message("\n\nDone! All diagnostics complete.")
