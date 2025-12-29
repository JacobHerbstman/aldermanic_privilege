# create_event_study_data.R
# Creates block-level panels for event study analysis of home sales
# Outputs: block-year and block-month panels with sales counts and prices

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_event_study_sales_data/code")
source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading sales data...")
sales <- read_csv("../input/sales_with_ward_distances.csv", show_col_types = FALSE) %>%
    filter(!is.na(sale_price), sale_price > 0) %>%
    mutate(
        sale_date = as.Date(sale_date),
        sale_month = as.yearmon(sale_date),
        year = year(sale_date)
    )
message(sprintf("Loaded %s sales", format(nrow(sales), big.mark = ",")))

message("Loading ward panel...")
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

message("Loading census blocks...")
census_blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_transform(st_crs(ward_panel)) %>%
    rename(block_id = GEOID10) %>%
    mutate(block_id = as.character(block_id))

message("Loading alderman data...")
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
    mutate(month_date = as.Date(paste("01", month), format = "%d %b %Y")) %>%
    filter(month(month_date) == 6) %>%
    mutate(year = year(month_date)) %>%
    select(year, ward, alderman)

strictness <- read_csv("../input/alderman_restrictiveness_scores_month_FEs.csv", show_col_types = FALSE) %>%
    select(alderman, strictness_index)

message("Loading block group controls...")
bg_controls <- read_csv("../input/block_group_controls.csv", show_col_types = FALSE) %>%
    rename(block_group_id = GEOID) %>%
    mutate(block_group_id = as.character(block_group_id))

# =============================================================================
# 2. ASSIGN SALES TO CENSUS BLOCKS
# =============================================================================
message("Assigning sales to census blocks...")

# Convert sales to sf
sales_sf <- sales %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(st_crs(census_blocks))

# Spatial join to get block_id for each sale
sales_with_blocks <- st_join(sales_sf, census_blocks %>% select(block_id), join = st_within) %>%
    st_drop_geometry() %>%
    filter(!is.na(block_id))

message(sprintf("Sales assigned to blocks: %s", format(nrow(sales_with_blocks), big.mark = ",")))

# =============================================================================
# 3. CREATE WARD PANEL FOR BLOCKS (Multiple Redistricting Events)
# =============================================================================
message("Creating block-ward panel across redistricting events...")

# Define redistricting cutoffs
# Pre-2003: not in panel (strictness scores start ~2006)
# 2003-2015: Use 2014 map
# 2015-2023: Use 2015 map
# 2023+: Use 2024 map

# Get unique years in data
year_range <- range(sales_with_blocks$year)
all_years <- seq(year_range[1], year_range[2])

# Get block centroids for spatial joins
block_centroids <- st_centroid(census_blocks)

# Create map assignments for each relevant year
map_years <- list(
    pre_2015 = 2014,
    post_2015 = 2015,
    post_2023 = 2024
)

# Spatial join blocks to ward maps
ward_assignments <- tibble(block_id = census_blocks$block_id)

for (map_name in names(map_years)) {
    map_year <- map_years[[map_name]]
    message(sprintf("  Joining blocks to %d ward map...", map_year))

    ward_polys <- ward_panel %>% filter(year == map_year)
    if (nrow(ward_polys) == 0) {
        # Try nearest year
        available_years <- unique(ward_panel$year)
        map_year <- available_years[which.min(abs(available_years - map_year))]
        ward_polys <- ward_panel %>% filter(year == map_year)
    }

    joined <- st_join(block_centroids, ward_polys, join = st_within) %>%
        st_drop_geometry() %>%
        select(block_id, ward) %>%
        distinct(block_id, .keep_all = TRUE)

    names(joined)[names(joined) == "ward"] <- paste0("ward_", map_name)

    ward_assignments <- ward_assignments %>%
        left_join(joined, by = "block_id")
}

# Expand to full block-year panel
block_year_base <- ward_assignments %>%
    tidyr::crossing(year = all_years) %>%
    mutate(
        # Assign ward based on year and redistricting
        ward = case_when(
            year < 2015 ~ ward_pre_2015,
            year >= 2015 & year < 2023 ~ ward_post_2015,
            year >= 2023 ~ ward_post_2023
        ),
        # Identify switching blocks for BOTH redistrictings
        switched_2015 = (ward_pre_2015 != ward_post_2015) & !is.na(ward_pre_2015) & !is.na(ward_post_2015),
        switched_2023 = (ward_post_2015 != ward_post_2023) & !is.na(ward_post_2015) & !is.na(ward_post_2023),
        # Event time relative to each event
        relative_year_2015 = year - 2015,
        relative_year_2023 = year - 2023
    ) %>%
    filter(!is.na(ward))

n_switching_blocks_2015 <- block_year_base %>%
    filter(switched_2015) %>%
    distinct(block_id) %>%
    nrow()
n_switching_blocks_2023 <- block_year_base %>%
    filter(switched_2023) %>%
    distinct(block_id) %>%
    nrow()
message(sprintf("Blocks that switched in 2015: %d", n_switching_blocks_2015))
message(sprintf("Blocks that switched in 2023: %d", n_switching_blocks_2023))

# =============================================================================
# 4. MERGE ALDERMAN AND STRICTNESS DATA
# =============================================================================
message("Merging alderman and strictness data...")

# Create alderman-strictness lookup by ward-year
alderman_scores <- alderman_panel %>%
    left_join(strictness, by = "alderman")

block_year_base <- block_year_base %>%
    left_join(alderman_scores, by = c("ward", "year"))

# For 2015 switching blocks, get pre and post strictness
block_treatment_2015 <- block_year_base %>%
    filter(year %in% c(2014, 2015)) %>%
    select(block_id, year, strictness_index) %>%
    pivot_wider(names_from = year, values_from = strictness_index, names_prefix = "strictness_") %>%
    mutate(
        strictness_change_2015 = strictness_2015 - strictness_2014,
        switch_type_2015 = case_when(
            is.na(strictness_change_2015) ~ "No Data",
            strictness_change_2015 > 0.1 ~ "Moved to Stricter",
            strictness_change_2015 < -0.1 ~ "Moved to More Lenient",
            TRUE ~ "No Significant Change"
        )
    ) %>%
    select(block_id, strictness_2014, strictness_2015, strictness_change_2015, switch_type_2015)

# For 2023 switching blocks, get pre and post strictness
block_treatment_2023 <- block_year_base %>%
    filter(year %in% c(2022, 2023)) %>%
    select(block_id, year, strictness_index) %>%
    pivot_wider(names_from = year, values_from = strictness_index, names_prefix = "strictness_") %>%
    mutate(
        strictness_change_2023 = strictness_2023 - strictness_2022,
        switch_type_2023 = case_when(
            is.na(strictness_change_2023) ~ "No Data",
            strictness_change_2023 > 0.1 ~ "Moved to Stricter",
            strictness_change_2023 < -0.1 ~ "Moved to More Lenient",
            TRUE ~ "No Significant Change"
        )
    ) %>%
    select(block_id, strictness_2022, strictness_2023, strictness_change_2023, switch_type_2023)

block_year_base <- block_year_base %>%
    left_join(block_treatment_2015, by = "block_id") %>%
    left_join(block_treatment_2023, by = "block_id") %>%
    # Add backwards-compatible columns
    mutate(
        relative_year = relative_year_2015,
        switch_type = switch_type_2015,
        strictness_change = strictness_change_2015
    )

# =============================================================================
# 5. AGGREGATE SALES TO BLOCK-YEAR
# =============================================================================
message("Aggregating sales to block-year...")

# Helper function to get mode (most common value)
get_mode <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
        return(NA_character_)
    }
    names(sort(table(x), decreasing = TRUE))[1]
}

sales_block_year <- sales_with_blocks %>%
    group_by(block_id, year) %>%
    summarise(
        n_sales = n(),
        mean_price = mean(sale_price, na.rm = TRUE),
        median_price = median(sale_price, na.rm = TRUE),
        sd_price = sd(sale_price, na.rm = TRUE),
        min_price = min(sale_price, na.rm = TRUE),
        max_price = max(sale_price, na.rm = TRUE),
        # Border-pair info: use most common ward_pair for this block-year
        ward_pair_id = get_mode(ward_pair_id),
        mean_dist_to_boundary = mean(abs(as.numeric(signed_dist)), na.rm = TRUE),
        .groups = "drop"
    )

# Merge with block panel
block_year_panel <- block_year_base %>%
    left_join(sales_block_year, by = c("block_id", "year")) %>%
    mutate(
        n_sales = replace_na(n_sales, 0),
        has_sales = n_sales > 0,
        block_group_id = substr(block_id, 1, 12)
    )

# Add block group controls
block_year_panel <- block_year_panel %>%
    left_join(bg_controls, by = c("block_group_id", "year"))

# Drop geometry columns if any remain
block_year_panel <- block_year_panel %>%
    select(-starts_with("ward_pre"), -starts_with("ward_post"))

# =============================================================================
# 6. AGGREGATE SALES TO BLOCK-MONTH (Observed sales only, cover both events)
# =============================================================================
message("Aggregating sales to block-month...")

# Aggregate monthly sales - cover both 2015 and 2023 windows
sales_block_month <- sales_with_blocks %>%
    group_by(block_id, sale_month) %>%
    summarise(
        n_sales = n(),
        mean_price = mean(sale_price, na.rm = TRUE),
        median_price = median(sale_price, na.rm = TRUE),
        # Border-pair info for monthly data
        ward_pair_id = get_mode(ward_pair_id),
        mean_dist_to_boundary = mean(abs(as.numeric(signed_dist)), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        year = as.integer(format(as.Date(sale_month), "%Y")),
        month = as.integer(format(as.Date(sale_month), "%m")),
        # Relative months for each event (May = month 5)
        relative_month_2015 = (year - 2015) * 12 + (month - 5),
        relative_month_2023 = (year - 2023) * 12 + (month - 5)
    )

# Get treatment info from block_year_base
block_treatment_info <- block_year_base %>%
    filter(year == 2015) %>%
    distinct(block_id, .keep_all = TRUE) %>%
    select(
        block_id, switched_2015, switched_2023, switch_type_2015, switch_type_2023,
        strictness_2014, strictness_2015, strictness_change_2015,
        strictness_2022, strictness_2023, strictness_change_2023
    )

# Merge treatment info onto monthly sales
block_month_panel <- sales_block_month %>%
    left_join(block_treatment_info, by = "block_id") %>%
    mutate(
        has_sales = TRUE
    ) %>%
    filter(!is.na(switched_2015) | !is.na(switched_2023))

message(sprintf("Block-month panel: %s rows", format(nrow(block_month_panel), big.mark = ",")))

# =============================================================================
# 8. CREATE STACKED COHORT PANEL
# =============================================================================
message("Creating stacked cohort panel...")

# 2015 cohort: blocks that switched in 2015, event window 2010-2020
cohort_2015 <- block_year_panel %>%
    filter(year >= 2010, year <= 2020) %>%
    mutate(
        cohort = "2015",
        treat = as.integer(switched_2015),
        relative_year = relative_year_2015,
        switch_type = switch_type_2015,
        strictness_change = strictness_change_2015
    ) %>%
    select(
        block_id, year, cohort, treat, relative_year, switch_type, strictness_change,
        n_sales, mean_price, median_price, has_sales,
        ward_pair_id, mean_dist_to_boundary
    )

# 2023 cohort: blocks that switched in 2023, event window 2018-2025
cohort_2023 <- block_year_panel %>%
    filter(year >= 2018, year <= 2025) %>%
    mutate(
        cohort = "2023",
        treat = as.integer(switched_2023),
        relative_year = relative_year_2023,
        switch_type = switch_type_2023,
        strictness_change = strictness_change_2023
    ) %>%
    select(
        block_id, year, cohort, treat, relative_year, switch_type, strictness_change,
        n_sales, mean_price, median_price, has_sales,
        ward_pair_id, mean_dist_to_boundary
    )

# Stack cohorts
stacked_panel <- bind_rows(cohort_2015, cohort_2023) %>%
    # Create cohort-block identifier for FE
    mutate(
        cohort_block_id = paste(cohort, block_id, sep = "_"),
        cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_")
    )

message(sprintf(
    "Stacked panel: %s rows (%s from 2015, %s from 2023)",
    format(nrow(stacked_panel), big.mark = ","),
    format(nrow(cohort_2015), big.mark = ","),
    format(nrow(cohort_2023), big.mark = ",")
))

# =============================================================================
# 9. CREATE STACKED QUARTERLY PANEL
# =============================================================================
message("Creating stacked quarterly panel...")

# First aggregate monthly sales to quarterly
sales_block_quarter <- sales_with_blocks %>%
    mutate(
        quarter = ceiling(month(sale_date) / 3),
        year_quarter = paste(year, sprintf("Q%d", quarter), sep = "-")
    ) %>%
    group_by(block_id, year, quarter, year_quarter) %>%
    summarise(
        n_sales = n(),
        mean_price = mean(sale_price, na.rm = TRUE),
        median_price = median(sale_price, na.rm = TRUE),
        ward_pair_id = get_mode(ward_pair_id),
        mean_dist_to_boundary = mean(abs(as.numeric(signed_dist)), na.rm = TRUE),
        .groups = "drop"
    )

# 2015 cohort quarterly: event window 2010-2020 (Q2 2015 is event time 0)
cohort_2015_quarterly <- sales_block_quarter %>%
    filter(year >= 2010, year <= 2020) %>%
    left_join(block_treatment_2015, by = "block_id") %>%
    left_join(
        block_year_base %>%
            filter(year == 2015) %>%
            distinct(block_id, switched_2015),
        by = "block_id"
    ) %>%
    filter(!is.na(switched_2015)) %>%
    mutate(
        cohort = "2015",
        treat = as.integer(switched_2015),
        # Relative quarter: Q2 2015 (quarter 2, year 2015) = 0
        relative_quarter = (year - 2015) * 4 + (quarter - 2),
        switch_type = switch_type_2015,
        strictness_change = strictness_change_2015,
        has_sales = TRUE
    ) %>%
    select(
        block_id, year, quarter, year_quarter, cohort, treat, relative_quarter,
        switch_type, strictness_change,
        n_sales, mean_price, median_price, has_sales,
        ward_pair_id, mean_dist_to_boundary
    )

# 2023 cohort quarterly: event window 2018-2025 (Q2 2023 is event time 0)
cohort_2023_quarterly <- sales_block_quarter %>%
    filter(year >= 2018, year <= 2025) %>%
    left_join(block_treatment_2023, by = "block_id") %>%
    left_join(
        block_year_base %>%
            filter(year == 2023) %>%
            distinct(block_id, switched_2023),
        by = "block_id"
    ) %>%
    filter(!is.na(switched_2023)) %>%
    mutate(
        cohort = "2023",
        treat = as.integer(switched_2023),
        # Relative quarter: Q2 2023 (quarter 2, year 2023) = 0
        relative_quarter = (year - 2023) * 4 + (quarter - 2),
        switch_type = switch_type_2023,
        strictness_change = strictness_change_2023,
        has_sales = TRUE
    ) %>%
    select(
        block_id, year, quarter, year_quarter, cohort, treat, relative_quarter,
        switch_type, strictness_change,
        n_sales, mean_price, median_price, has_sales,
        ward_pair_id, mean_dist_to_boundary
    )

# Stack quarterly cohorts
stacked_quarterly_panel <- bind_rows(cohort_2015_quarterly, cohort_2023_quarterly) %>%
    mutate(
        cohort_block_id = paste(cohort, block_id, sep = "_"),
        cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_")
    )

message(sprintf(
    "Stacked quarterly panel: %s rows (%s from 2015, %s from 2023)",
    format(nrow(stacked_quarterly_panel), big.mark = ","),
    format(nrow(cohort_2015_quarterly), big.mark = ","),
    format(nrow(cohort_2023_quarterly), big.mark = ",")
))

# =============================================================================
# 10. CREATE STACKED MONTHLY PANEL
# =============================================================================
message("Creating stacked monthly panel...")

# Use sales_block_month which already has monthly aggregation
# 2015 cohort monthly: event window 2010-2020 (May 2015 is event time 0)
cohort_2015_monthly <- sales_block_month %>%
    filter(year >= 2010, year <= 2020) %>%
    left_join(block_treatment_2015, by = "block_id") %>%
    left_join(
        block_year_base %>%
            filter(year == 2015) %>%
            distinct(block_id, switched_2015),
        by = "block_id"
    ) %>%
    filter(!is.na(switched_2015)) %>%
    mutate(
        cohort = "2015",
        treat = as.integer(switched_2015),
        # Relative month: May 2015 (month 5, year 2015) = 0
        relative_month = (year - 2015) * 12 + (month - 5),
        switch_type = switch_type_2015,
        strictness_change = strictness_change_2015,
        has_sales = TRUE,
        year_month = paste(year, sprintf("%02d", month), sep = "-")
    ) %>%
    select(
        block_id, year, month, year_month, cohort, treat, relative_month,
        switch_type, strictness_change,
        n_sales, mean_price, median_price, has_sales,
        ward_pair_id, mean_dist_to_boundary
    )

# 2023 cohort monthly: event window 2018-2025 (May 2023 is event time 0)
cohort_2023_monthly <- sales_block_month %>%
    filter(year >= 2018, year <= 2025) %>%
    left_join(block_treatment_2023, by = "block_id") %>%
    left_join(
        block_year_base %>%
            filter(year == 2023) %>%
            distinct(block_id, switched_2023),
        by = "block_id"
    ) %>%
    filter(!is.na(switched_2023)) %>%
    mutate(
        cohort = "2023",
        treat = as.integer(switched_2023),
        # Relative month: May 2023 (month 5, year 2023) = 0
        relative_month = (year - 2023) * 12 + (month - 5),
        switch_type = switch_type_2023,
        strictness_change = strictness_change_2023,
        has_sales = TRUE,
        year_month = paste(year, sprintf("%02d", month), sep = "-")
    ) %>%
    select(
        block_id, year, month, year_month, cohort, treat, relative_month,
        switch_type, strictness_change,
        n_sales, mean_price, median_price, has_sales,
        ward_pair_id, mean_dist_to_boundary
    )

# Stack monthly cohorts
stacked_monthly_panel <- bind_rows(cohort_2015_monthly, cohort_2023_monthly) %>%
    mutate(
        cohort_block_id = paste(cohort, block_id, sep = "_"),
        cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_")
    )

message(sprintf(
    "Stacked monthly panel: %s rows (%s from 2015, %s from 2023)",
    format(nrow(stacked_monthly_panel), big.mark = ","),
    format(nrow(cohort_2015_monthly), big.mark = ","),
    format(nrow(cohort_2023_monthly), big.mark = ",")
))

# =============================================================================
# 11. SAVE OUTPUTS
# =============================================================================
message("Saving outputs...")

write_csv(block_year_panel, "../output/sales_block_year_panel.csv")
message(sprintf("Saved block-year panel: %s rows", format(nrow(block_year_panel), big.mark = ",")))

write_csv(block_month_panel, "../output/sales_block_month_panel.csv")
message(sprintf("Saved block-month panel: %s rows", format(nrow(block_month_panel), big.mark = ",")))

write_csv(stacked_panel, "../output/sales_stacked_panel.csv")
message(sprintf("Saved stacked cohort panel: %s rows", format(nrow(stacked_panel), big.mark = ",")))

write_csv(stacked_quarterly_panel, "../output/sales_stacked_quarterly_panel.csv")
message(sprintf("Saved stacked quarterly panel: %s rows", format(nrow(stacked_quarterly_panel), big.mark = ",")))

write_csv(stacked_monthly_panel, "../output/sales_stacked_monthly_panel.csv")
message(sprintf("Saved stacked monthly panel: %s rows", format(nrow(stacked_monthly_panel), big.mark = ",")))

# Summary stats
message("\nSummary by cohort and treatment status:")
stacked_panel %>%
    filter(n_sales > 0) %>%
    group_by(cohort, treat) %>%
    summarise(
        n_block_years = n(),
        total_sales = sum(n_sales),
        mean_price = weighted.mean(mean_price, n_sales, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    print()

message("\nDone!")
