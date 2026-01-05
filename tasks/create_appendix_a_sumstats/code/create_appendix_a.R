# create_appendix_a.R
# Creates summary statistics table for rental listings
# Output: LaTeX table file only

source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. HELPER FUNCTIONS
# =============================================================================

format_num <- function(x, digits = 0) {
    formatC(x, format = "f", digits = digits, big.mark = ",")
}

make_row <- function(label, mean, sd, p10, p50, p90, n, mean_digits = 0, use_comma = TRUE) {
    big_mark <- if (use_comma) "," else ""
    format_stat <- function(x, digits) {
        formatC(x, format = "f", digits = digits, big.mark = big_mark)
    }
    paste0(
        label, " & ",
        format_stat(mean, mean_digits), " & ",
        format_stat(sd, mean_digits), " & ",
        format_stat(p10, mean_digits), " & ",
        format_stat(p50, mean_digits), " & ",
        format_stat(p90, mean_digits), " & ",
        format_num(n), " \\\\"
    )
}

compute_stats <- function(x) {
    x <- x[!is.na(x)]
    tibble(
        mean = mean(x),
        sd = sd(x),
        p10 = quantile(x, 0.10),
        p50 = quantile(x, 0.50),
        p90 = quantile(x, 0.90),
        n = length(x)
    )
}

# =============================================================================
# 2. LOAD AND FILTER DATA
# =============================================================================
message("Loading rental data...")
rent_data <- read_parquet("../input/rent_with_ward_distances_full.parquet")

message(sprintf("Raw observations: %s", format(nrow(rent_data), big.mark = ",")))

# Track sample attrition
n_raw <- nrow(rent_data)

rent_clean <- rent_data %>%
    filter(!is.na(rent_price), rent_price > 0)
n_after_valid_price <- nrow(rent_clean)

rent_clean <- rent_clean %>%
    filter(!is.na(latitude), !is.na(longitude))
n_after_valid_coords <- nrow(rent_clean)

rent_clean <- rent_clean %>%
    filter(!is.na(ward))
n_after_spatial_join <- nrow(rent_clean)

n_final <- nrow(rent_clean)
message(sprintf("Final sample: %s", format(n_final, big.mark = ",")))

# =============================================================================
# 3. COMPUTE SUMMARY STATISTICS
# =============================================================================
message("Computing summary statistics...")

# Panel A: Price and Unit Characteristics
stats_rent <- compute_stats(rent_clean$rent_price)
stats_beds <- compute_stats(rent_clean$beds)
stats_baths <- compute_stats(rent_clean$baths)
stats_year_built <- compute_stats(rent_clean$year_built)

# Panel B: Building Type Distribution
rent_clean <- rent_clean %>%
    mutate(
        building_type_display = case_when(
            building_type_clean == "single_family" ~ "Single-family",
            building_type_clean == "multi_family" ~ "Multi-family",
            building_type_clean == "condo" ~ "Condo",
            building_type_clean == "townhouse" ~ "Townhouse",
            TRUE ~ "Other"
        )
    )

building_type_dist <- rent_clean %>%
    group_by(building_type_display) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(pct = 100 * count / sum(count)) %>%
    arrange(desc(count))

# Panel C: Amenities
pct_laundry <- 100 * mean(rent_clean$laundry, na.rm = TRUE)
n_laundry <- sum(!is.na(rent_clean$laundry))
pct_gym <- 100 * mean(rent_clean$gym, na.rm = TRUE)
n_gym <- sum(!is.na(rent_clean$gym))

# Panel D: Location
stats_dist <- compute_stats(abs(rent_clean$signed_dist))

# =============================================================================
# 4. GENERATE LATEX TABLE
# =============================================================================
message("Generating LaTeX table...")

table_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\begin{threeparttable}",
    "\\caption{Rental Listings Summary Statistics}",
    "\\label{tab:rental_sumstats}",
    "\\begin{tabular}{lrrrrrr}",
    "\\toprule",
    "& Mean & SD & P10 & P50 & P90 & N \\\\",
    "\\midrule",
    "\\textit{Panel A: Price and Unit Characteristics} \\\\[0.5ex]",
    make_row("Rent (\\$/month)", stats_rent$mean, stats_rent$sd, stats_rent$p10, stats_rent$p50, stats_rent$p90, stats_rent$n, 0),
    make_row("Bedrooms", stats_beds$mean, stats_beds$sd, stats_beds$p10, stats_beds$p50, stats_beds$p90, stats_beds$n, 2),
    make_row("Bathrooms", stats_baths$mean, stats_baths$sd, stats_baths$p10, stats_baths$p50, stats_baths$p90, stats_baths$n, 2),
    make_row("Year built", stats_year_built$mean, stats_year_built$sd, stats_year_built$p10, stats_year_built$p50, stats_year_built$p90, stats_year_built$n, 0, use_comma = FALSE),
    "\\midrule",
    "\\textit{Panel B: Building Type Distribution} & \\multicolumn{2}{c}{Count} & \\multicolumn{3}{c}{Percent} & \\\\[0.5ex]"
)

# Add building type rows
for (i in seq_len(nrow(building_type_dist))) {
    table_lines <- c(
        table_lines,
        paste0(
            building_type_dist$building_type_display[i], " & \\multicolumn{2}{c}{",
            format_num(building_type_dist$count[i]), "} & \\multicolumn{3}{c}{",
            format_num(building_type_dist$pct[i], 1), "\\%} & \\\\"
        )
    )
}

table_lines <- c(
    table_lines,
    "\\midrule",
    "\\textit{Panel C: Amenities} & \\multicolumn{5}{c}{Percent Present} & N \\\\[0.5ex]",
    paste0("Laundry & \\multicolumn{5}{c}{", format_num(pct_laundry, 1), "\\%} & ", format_num(n_laundry), " \\\\"),
    paste0("Gym & \\multicolumn{5}{c}{", format_num(pct_gym, 1), "\\%} & ", format_num(n_gym), " \\\\"),
    "\\midrule",
    "\\textit{Panel D: Location} \\\\[0.5ex]",
    make_row("Distance to boundary (ft)", stats_dist$mean, stats_dist$sd, stats_dist$p10, stats_dist$p50, stats_dist$p90, stats_dist$n, 0),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\small",
    paste0("\\item \\textit{Notes:} Sample includes ", format_num(n_final), " rental listings from 2014--2025 with valid price, coordinates, and ward assignment. Distance to boundary is the absolute distance to the nearest shared ward boundary."),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
)

# Write output
output_path <- "../output/rental_sumstats_table.tex"
writeLines(table_lines, output_path)

message(sprintf("Done! Saved table to %s", output_path))
message(sprintf("Total observations: %s", format(n_final, big.mark = ",")))

# Also print sample attrition for reference
message("\nSample Attrition:")
message(sprintf("  Raw observations: %s", format(n_raw, big.mark = ",")))
message(sprintf("  After valid price: %s", format(n_after_valid_price, big.mark = ",")))
message(sprintf("  After valid coords: %s", format(n_after_valid_coords, big.mark = ",")))
message(sprintf("  After spatial join: %s", format(n_after_spatial_join, big.mark = ",")))
