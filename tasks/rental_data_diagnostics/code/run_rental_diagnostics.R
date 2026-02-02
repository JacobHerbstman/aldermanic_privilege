# run_rental_diagnostics.R
# Diagnostics for rental panel data quality
# Investigates: 1) Discrete change around 2019 in building types and listing counts
#               2) When available_date becomes non-NA and whether this coincides with other anomalies

source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading rental panel data...")

rent_panel <- arrow::read_parquet("../input/chicago_rent_panel.parquet")

message(sprintf("Loaded %s observations", format(nrow(rent_panel), big.mark = ",")))
message(sprintf("Date range: %s to %s", min(rent_panel$file_date, na.rm = TRUE), max(rent_panel$file_date, na.rm = TRUE)))

# Create year-month variable (date column is called file_date, rent is rent_price)
rent_panel <- rent_panel %>%
    mutate(
        date = file_date,
        rent = rent_price,
        year = lubridate::year(date),
        month = lubridate::month(date),
        year_month = lubridate::floor_date(date, "month")
    )

# =============================================================================
# 2. MONTHLY LISTING COUNTS
# =============================================================================
message("\nCreating monthly listing counts figure...")

monthly_counts <- rent_panel %>%
    group_by(year_month) %>%
    summarise(
        n_listings = n(),
        .groups = "drop"
    )

p_monthly <- ggplot(monthly_counts, aes(x = year_month, y = n_listings)) +
    annotate("rect",
        xmin = as.Date("2019-01-01"), xmax = as.Date("2020-01-01"),
        ymin = -Inf, ymax = Inf,
        fill = "orange", alpha = 0.2
    ) +
    geom_line(color = "#1F77B4", linewidth = 0.8) +
    geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed", color = "red", linewidth = 0.5) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    labs(
        title = "Monthly Listing Counts Over Time",
        subtitle = "Orange shaded region = 2019 (period of interest)",
        x = "Date",
        y = "Number of Listings"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
    )

ggsave("../output/fig_listing_counts_monthly.pdf", p_monthly, width = 10, height = 6, bg = "white")
message("Saved: ../output/fig_listing_counts_monthly.pdf")

# =============================================================================
# 3. BUILDING TYPE COMPOSITION OVER TIME
# =============================================================================
message("\nCreating building type composition figures...")

# Annual building type counts
building_type_annual <- rent_panel %>%
    group_by(year, building_type) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(year) %>%
    mutate(
        total = sum(n),
        share = n / total
    ) %>%
    ungroup()

# Handle NA building types
building_type_annual <- building_type_annual %>%
    mutate(building_type = ifelse(is.na(building_type), "Unknown/NA", building_type))

# Panel A: Absolute counts
p_type_counts <- ggplot(building_type_annual, aes(x = year, y = n, fill = building_type)) +
    annotate("rect",
        xmin = 2019 - 0.5, xmax = 2020 - 0.5,
        ymin = -Inf, ymax = Inf,
        fill = "orange", alpha = 0.2
    ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_brewer(palette = "Set2", name = "Building Type") +
    scale_y_continuous(labels = scales::comma) +
    labs(
        title = "A: Building Type Counts by Year",
        x = "Year",
        y = "Number of Listings"
    ) +
    theme_minimal(base_size = 11) +
    theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
    )

# Panel B: Shares
p_type_shares <- ggplot(building_type_annual, aes(x = year, y = share, fill = building_type)) +
    annotate("rect",
        xmin = 2019 - 0.5, xmax = 2020 - 0.5,
        ymin = -Inf, ymax = Inf,
        fill = "orange", alpha = 0.2
    ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_brewer(palette = "Set2", name = "Building Type") +
    scale_y_continuous(labels = scales::percent) +
    labs(
        title = "B: Building Type Shares by Year",
        x = "Year",
        y = "Share of Listings"
    ) +
    theme_minimal(base_size = 11) +
    theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
    )

# Combine panels
p_building_type <- p_type_counts + p_type_shares +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")

ggsave("../output/fig_building_type_over_time.pdf", p_building_type, width = 12, height = 6, bg = "white")
message("Saved: ../output/fig_building_type_over_time.pdf")

# =============================================================================
# 4. VARIABLE COVERAGE OVER TIME
# =============================================================================
message("\nCreating variable coverage figure...")

# Calculate coverage rates by year-month
coverage_monthly <- rent_panel %>%
    group_by(year_month) %>%
    summarise(
        n_total = n(),
        available_date_coverage = mean(!is.na(available_date)),
        sqft_coverage = mean(!is.na(sqft)),
        beds_coverage = mean(!is.na(beds)),
        baths_coverage = mean(!is.na(baths)),
        year_built_coverage = mean(!is.na(year_built)),
        building_type_coverage = mean(!is.na(building_type)),
        .groups = "drop"
    )

# Reshape for plotting
coverage_long <- coverage_monthly %>%
    pivot_longer(
        cols = ends_with("_coverage"),
        names_to = "variable",
        values_to = "coverage"
    ) %>%
    mutate(variable = str_replace(variable, "_coverage", ""))

p_coverage <- ggplot(coverage_long, aes(x = year_month, y = coverage, color = variable)) +
    annotate("rect",
        xmin = as.Date("2019-01-01"), xmax = as.Date("2020-01-01"),
        ymin = -Inf, ymax = Inf,
        fill = "orange", alpha = 0.2
    ) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed", color = "red", linewidth = 0.5) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_color_brewer(palette = "Dark2", name = "Variable") +
    labs(
        title = "Variable Coverage Over Time",
        subtitle = "Proportion of listings with non-missing values for each variable",
        x = "Date",
        y = "Coverage Rate"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold"),
        legend.position = "right",
        panel.grid.minor = element_blank()
    )

ggsave("../output/fig_variable_coverage_over_time.pdf", p_coverage, width = 12, height = 6, bg = "white")
message("Saved: ../output/fig_variable_coverage_over_time.pdf")

# =============================================================================
# 5. RENT PRICES BY BUILDING TYPE OVER TIME
# =============================================================================
message("\nCreating rent prices by building type figure...")

# Calculate median rent by year and building type
rent_by_type <- rent_panel %>%
    filter(!is.na(rent), !is.na(building_type)) %>%
    group_by(year, building_type) %>%
    summarise(
        median_rent = median(rent, na.rm = TRUE),
        mean_rent = mean(rent, na.rm = TRUE),
        n = n(),
        .groups = "drop"
    )

p_rent_by_type <- ggplot(rent_by_type, aes(x = year, y = median_rent, color = building_type)) +
    annotate("rect",
        xmin = 2019 - 0.5, xmax = 2020 - 0.5,
        ymin = -Inf, ymax = Inf,
        fill = "orange", alpha = 0.2
    ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::dollar) +
    scale_color_brewer(palette = "Set1", name = "Building Type") +
    labs(
        title = "Median Rent by Building Type Over Time",
        subtitle = "Orange shaded region = 2019",
        x = "Year",
        y = "Median Monthly Rent"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold"),
        legend.position = "right",
        panel.grid.minor = element_blank()
    )

ggsave("../output/fig_rent_prices_by_type.pdf", p_rent_by_type, width = 10, height = 6, bg = "white")
message("Saved: ../output/fig_rent_prices_by_type.pdf")

# =============================================================================
# 6. 2019 DISCONTINUITY ANALYSIS (4-PANEL FOCUSED VIEW)
# =============================================================================
message("\nCreating 2019 discontinuity analysis figure...")

# Filter to 2018-2020 window for focused analysis
rent_window <- rent_panel %>%
    filter(year >= 2018 & year <= 2020)

# Monthly summary for window
window_monthly <- rent_window %>%
    group_by(year_month) %>%
    summarise(
        n_listings = n(),
        median_rent = median(rent, na.rm = TRUE),
        available_date_coverage = mean(!is.na(available_date)),
        sqft_coverage = mean(!is.na(sqft)),
        .groups = "drop"
    )

# Building type composition in window (monthly)
window_type <- rent_window %>%
    mutate(building_type = ifelse(is.na(building_type), "Unknown/NA", building_type)) %>%
    group_by(year_month, building_type) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(year_month) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()

# Panel A: Listing counts
p_window_counts <- ggplot(window_monthly, aes(x = year_month, y = n_listings)) +
    geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_line(color = "#1F77B4", linewidth = 1) +
    geom_point(color = "#1F77B4", size = 2) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "A: Monthly Listing Counts", x = NULL, y = "Listings") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))

# Panel B: Building type shares
p_window_type <- ggplot(window_type, aes(x = year_month, y = share, fill = building_type)) +
    geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_area(alpha = 0.8) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "Set2", name = "Type") +
    labs(title = "B: Building Type Shares", x = NULL, y = "Share") +
    theme_minimal(base_size = 10) +
    theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
    )

# Panel C: Variable coverage
window_coverage <- window_monthly %>%
    pivot_longer(
        cols = c(available_date_coverage, sqft_coverage),
        names_to = "variable",
        values_to = "coverage"
    ) %>%
    mutate(variable = case_when(
        variable == "available_date_coverage" ~ "available_date",
        variable == "sqft_coverage" ~ "sqft"
    ))

p_window_coverage <- ggplot(window_coverage, aes(x = year_month, y = coverage, color = variable)) +
    geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_color_brewer(palette = "Set1", name = "Variable") +
    labs(title = "C: Variable Coverage", x = NULL, y = "Coverage") +
    theme_minimal(base_size = 10) +
    theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
    )

# Panel D: Median rent
p_window_rent <- ggplot(window_monthly, aes(x = year_month, y = median_rent)) +
    geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_line(color = "#2CA02C", linewidth = 1) +
    geom_point(color = "#2CA02C", size = 2) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::dollar) +
    labs(title = "D: Median Rent", x = "Date", y = "Median Rent") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))

# Combine all panels
p_discontinuity <- (p_window_counts | p_window_type) / (p_window_coverage | p_window_rent) +
    plot_annotation(
        title = "2018-2020 Discontinuity Analysis",
        subtitle = "Red dashed line = January 2019",
        theme = theme(
            plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
            plot.subtitle = element_text(size = 11, hjust = 0.5)
        )
    )

ggsave("../output/fig_2019_discontinuity_analysis.pdf", p_discontinuity, width = 14, height = 10, bg = "white")
message("Saved: ../output/fig_2019_discontinuity_analysis.pdf")

# =============================================================================
# 7. SUMMARY STATISTICS TABLE
# =============================================================================
message("\nCreating annual summary statistics...")

annual_summary <- rent_panel %>%
    mutate(building_type = ifelse(is.na(building_type), "Unknown/NA", building_type)) %>%
    group_by(year) %>%
    summarise(
        n_listings = n(),
        median_rent = median(rent, na.rm = TRUE),
        mean_rent = mean(rent, na.rm = TRUE),
        sd_rent = sd(rent, na.rm = TRUE),
        available_date_pct = mean(!is.na(available_date)) * 100,
        sqft_pct = mean(!is.na(sqft)) * 100,
        beds_pct = mean(!is.na(beds)) * 100,
        baths_pct = mean(!is.na(baths)) * 100,
        year_built_pct = mean(!is.na(year_built)) * 100,
        building_type_pct = mean(building_type != "Unknown/NA") * 100,
        .groups = "drop"
    )

# Add building type breakdown
type_breakdown <- rent_panel %>%
    mutate(building_type = ifelse(is.na(building_type), "Unknown/NA", building_type)) %>%
    group_by(year, building_type) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(year) %>%
    mutate(pct = n / sum(n) * 100) %>%
    select(-n) %>%
    pivot_wider(names_from = building_type, values_from = pct, names_prefix = "pct_")

annual_summary <- annual_summary %>%
    left_join(type_breakdown, by = "year")

write_csv(annual_summary, "../output/rental_data_diagnostics_summary.csv")
message("Saved: ../output/rental_data_diagnostics_summary.csv")

# Print summary to console
message("\n========================================")
message("Annual Summary Statistics")
message("========================================")
print(annual_summary %>% select(year, n_listings, median_rent, available_date_pct, sqft_pct))

# =============================================================================
# 8. KEY FINDINGS
# =============================================================================
message("\n========================================")
message("KEY DIAGNOSTIC FINDINGS")
message("========================================")

# Check for discontinuity around 2019
pre_2019 <- annual_summary %>% filter(year < 2019)
post_2019 <- annual_summary %>% filter(year >= 2019)

message("\nListing counts:")
message(sprintf("  Pre-2019 average: %s", format(mean(pre_2019$n_listings), big.mark = ",")))
message(sprintf("  Post-2019 average: %s", format(mean(post_2019$n_listings), big.mark = ",")))

message("\navailable_date coverage:")
message(sprintf("  Pre-2019 average: %.1f%%", mean(pre_2019$available_date_pct)))
message(sprintf("  Post-2019 average: %.1f%%", mean(post_2019$available_date_pct)))

# Find first month with substantial available_date coverage (>5%)
first_available_date <- coverage_monthly %>%
    filter(available_date_coverage > 0.05) %>%
    slice_min(year_month, n = 1)

if (nrow(first_available_date) > 0) {
    message(sprintf("\nFirst month with >5%% available_date coverage: %s (%.1f%%)",
        first_available_date$year_month,
        first_available_date$available_date_coverage * 100
    ))
}

message("\n\nDone! All diagnostics complete.")
