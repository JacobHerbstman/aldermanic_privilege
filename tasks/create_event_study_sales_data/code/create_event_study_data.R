# create_event_study_data.R
# Creates block-level panels for event study analysis of home sales
# Outputs: block-year and block-month panels with sales counts and prices

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_event_study_sales_data/code")
source("../../setup_environment/code/packages.R")

# =============================================================================
# REDISTRICTING DATES
# =============================================================================
# Exact dates when new ward maps took effect
REDISTRICT_DATE_2015 <- as.Date("2015-05-18") # May 18, 2015
REDISTRICT_DATE_2023 <- as.Date("2023-05-15") # May 15, 2023

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading sales data...")
sales <- read_csv("../input/sales_with_ward_distances.csv", show_col_types = FALSE) %>%
    filter(!is.na(sale_price), sale_price > 0) %>%
    mutate(
        sale_date = as.Date(sale_date),
        sale_month = as.yearmon(sale_date),
        year = year(sale_date),
        # Determine which ward regime each sale falls under
        # This is the key fix: use exact sale_date, not year
        ward_regime = case_when(
            sale_date < REDISTRICT_DATE_2015 ~ "pre_2015",
            sale_date >= REDISTRICT_DATE_2015 & sale_date < REDISTRICT_DATE_2023 ~ "post_2015",
            sale_date >= REDISTRICT_DATE_2023 ~ "post_2023"
        )
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
    mutate(block_id = as.character(block_id)) %>%
    # Deduplicate - raw data has 46 duplicate block_ids
    distinct(block_id, .keep_all = TRUE)

message("Loading treatment panel...")
treatment_panel <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE) %>%
    mutate(block_id = as.character(block_id))

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
# 3. CREATE BLOCK-YEAR PANEL USING TREATMENT PANEL
# =============================================================================
message("Creating block-year panel using treatment panel...")

# Get unique years in data
year_range <- range(sales_with_blocks$year)
all_years <- seq(year_range[1], year_range[2])

# Get treatment info for 2015 cohort (uses 2010 blocks)
treatment_2015 <- treatment_panel %>%
    filter(cohort == "2015") %>%
    select(
        block_id,
        ward_pre_2015 = ward_origin,
        ward_post_2015 = ward_dest,
        switched_2015 = switched,
        strictness_origin_2015 = strictness_origin,
        strictness_dest_2015 = strictness_dest,
        strictness_change_2015 = strictness_change,
        switch_type_2015 = switch_type,
        valid_2015 = valid
    )

# Get treatment info for 2023 cohort
treatment_2023 <- treatment_panel %>%
    filter(cohort == "2023") %>%
    select(
        block_id,
        ward_post_2023 = ward_dest,
        switched_2023 = switched,
        strictness_origin_2023 = strictness_origin,
        strictness_dest_2023 = strictness_dest,
        strictness_change_2023 = strictness_change,
        switch_type_2023 = switch_type,
        valid_2023 = valid
    )

# Create block-year base panel from 2015 treatment data
block_year_base <- treatment_2015 %>%
    tidyr::crossing(year = all_years) %>%
    mutate(
        ward = case_when(
            year < 2015 ~ ward_pre_2015,
            year >= 2015 ~ ward_post_2015
        ),
        relative_year_2015 = year - 2015
    ) %>%
    # Join 2023 treatment info
    left_join(treatment_2023, by = "block_id") %>%
    mutate(
        relative_year_2023 = year - 2023,
        switched_2023 = replace_na(switched_2023, FALSE),
        valid_2023 = replace_na(valid_2023, TRUE)
    ) %>%
    filter(!is.na(ward))

# Create ward assignments for joining to sales
ward_assignments <- treatment_2015 %>%
    select(block_id, ward_pre_2015, ward_post_2015) %>%
    left_join(treatment_2023 %>% select(block_id, ward_post_2023), by = "block_id")

sales_with_blocks <- sales_with_blocks %>%
    left_join(ward_assignments, by = "block_id") %>%
    mutate(
        ward_at_sale = case_when(
            ward_regime == "pre_2015" ~ ward_pre_2015,
            ward_regime == "post_2015" ~ ward_post_2015,
            ward_regime == "post_2023" ~ ward_post_2023
        )
    )

n_switching_blocks_2015 <- sum(treatment_2015$switched_2015, na.rm = TRUE)
n_switching_blocks_2023 <- sum(treatment_2023$switched_2023, na.rm = TRUE)
message(sprintf("Blocks that switched in 2015: %d", n_switching_blocks_2015))
message(sprintf("Blocks that switched in 2023: %d", n_switching_blocks_2023))

# (Old Section 3-4 code removed - now using pre-computed treatment panel)
# Diagnostics are printed by create_block_treatment_panel task


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
        strictness_origin_2015 = strictness_origin_2015, # was strictness_2014
        strictness_dest_2015 = strictness_dest_2015, # was strictness_2015
        strictness_change_2015,
        strictness_origin_2023 = strictness_origin_2023, # was strictness_2022
        strictness_dest_2023 = strictness_dest_2023, # was strictness_2023
        strictness_change_2023
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

# 2015 cohort: valid units only (switchers + non-switchers in wards without turnover)
cohort_2015 <- block_year_panel %>%
    filter(year >= 2010, year <= 2020) %>%
    filter(valid_2015) %>% # DROP CONTAMINATED CONTROLS
    mutate(
        cohort = "2015",
        treat = as.integer(switched_2015),
        redistricted = switched_2015, # TRUE = ward boundary changed, FALSE = same ward but alderman changed
        relative_year = relative_year_2015,
        switch_type = switch_type_2015,
        strictness_change = strictness_change_2015
    ) %>%
    select(
        block_id, year, cohort, treat, redistricted, relative_year, switch_type, strictness_change,
        n_sales, mean_price, median_price, has_sales,
        ward_pair_id, mean_dist_to_boundary
    )

# 2023 cohort: valid units only (switchers + non-switchers in wards without turnover)
cohort_2023 <- block_year_panel %>%
    filter(year >= 2018, year <= 2025) %>%
    filter(valid_2023) %>% # DROP CONTAMINATED CONTROLS
    mutate(
        cohort = "2023",
        treat = as.integer(switched_2023),
        redistricted = switched_2023, # TRUE = ward boundary changed, FALSE = same ward but alderman changed
        relative_year = relative_year_2023,
        switch_type = switch_type_2023,
        strictness_change = strictness_change_2023
    ) %>%
    select(
        block_id, year, cohort, treat, redistricted, relative_year, switch_type, strictness_change,
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

# 2015 cohort quarterly: valid units only (Q2 2015 is event time 0)
cohort_2015_quarterly <- sales_block_quarter %>%
    filter(year >= 2010, year <= 2020) %>%
    left_join(
        treatment_2015 %>% distinct(block_id, .keep_all = TRUE),
        by = "block_id"
    ) %>%
    filter(!is.na(switched_2015), valid_2015) %>% # DROP CONTAMINATED CONTROLS
    mutate(
        cohort = "2015",
        treat = as.integer(switched_2015),
        redistricted = switched_2015,
        # Relative quarter: Q2 2015 (quarter 2, year 2015) = 0
        relative_quarter = (year - 2015) * 4 + (quarter - 2),
        switch_type = switch_type_2015,
        strictness_change = strictness_change_2015,
        has_sales = TRUE
    ) %>%
    select(
        block_id, year, quarter, year_quarter, cohort, treat, redistricted, relative_quarter,
        switch_type, strictness_change,
        n_sales, mean_price, median_price, has_sales,
        ward_pair_id, mean_dist_to_boundary
    )

# 2023 cohort quarterly: valid units only (Q2 2023 is event time 0)
cohort_2023_quarterly <- sales_block_quarter %>%
    filter(year >= 2018, year <= 2025) %>%
    left_join(
        treatment_2023 %>% distinct(block_id, .keep_all = TRUE),
        by = "block_id"
    ) %>%
    filter(!is.na(switched_2023), valid_2023) %>% # DROP CONTAMINATED CONTROLS
    mutate(
        cohort = "2023",
        treat = as.integer(switched_2023),
        redistricted = switched_2023,
        # Relative quarter: Q2 2023 (quarter 2, year 2023) = 0
        relative_quarter = (year - 2023) * 4 + (quarter - 2),
        switch_type = switch_type_2023,
        strictness_change = strictness_change_2023,
        has_sales = TRUE
    ) %>%
    select(
        block_id, year, quarter, year_quarter, cohort, treat, redistricted, relative_quarter,
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
# 2015 cohort monthly: valid units only (May 2015 is event time 0)
cohort_2015_monthly <- sales_block_month %>%
    filter(year >= 2010, year <= 2020) %>%
    left_join(
        treatment_2015 %>% distinct(block_id, .keep_all = TRUE),
        by = "block_id"
    ) %>%
    filter(!is.na(switched_2015), valid_2015) %>% # DROP CONTAMINATED CONTROLS
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

# 2023 cohort monthly: valid units only (May 2023 is event time 0)
cohort_2023_monthly <- sales_block_month %>%
    filter(year >= 2018, year <= 2025) %>%
    left_join(
        treatment_2023 %>% distinct(block_id, .keep_all = TRUE),
        by = "block_id"
    ) %>%
    filter(!is.na(switched_2023), valid_2023) %>% # DROP CONTAMINATED CONTROLS
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
# 11. TRANSITION PERIOD DIAGNOSTIC
# =============================================================================
message("\n=== TRANSITION PERIOD DIAGNOSTIC ===")
message("Verifying correct ward regime assignment for sales in transition months...")

# 2015 transition: Jan-May 2015
transition_2015 <- sales_with_blocks %>%
    filter(year == 2015, month(sale_date) <= 5) %>%
    group_by(ward_regime) %>%
    summarise(
        n_sales = n(),
        min_date = min(sale_date),
        max_date = max(sale_date),
        .groups = "drop"
    )

message("\n2015 Transition Period (Jan-May 2015):")
message(sprintf("  Redistricting date: %s", REDISTRICT_DATE_2015))
for (i in seq_len(nrow(transition_2015))) {
    row <- transition_2015[i, ]
    message(sprintf(
        "  %s: %s sales (%s to %s)",
        row$ward_regime, format(row$n_sales, big.mark = ","),
        row$min_date, row$max_date
    ))
}

# 2023 transition: Jan-May 2023
transition_2023 <- sales_with_blocks %>%
    filter(year == 2023, month(sale_date) <= 5) %>%
    group_by(ward_regime) %>%
    summarise(
        n_sales = n(),
        min_date = min(sale_date),
        max_date = max(sale_date),
        .groups = "drop"
    )

message("\n2023 Transition Period (Jan-May 2023):")
message(sprintf("  Redistricting date: %s", REDISTRICT_DATE_2023))
for (i in seq_len(nrow(transition_2023))) {
    row <- transition_2023[i, ]
    message(sprintf(
        "  %s: %s sales (%s to %s)",
        row$ward_regime, format(row$n_sales, big.mark = ","),
        row$min_date, row$max_date
    ))
}

# Verify the ward_at_sale assignment works correctly
message("\nVerifying ward_at_sale matches expected wards:")
sample_check <- sales_with_blocks %>%
    filter(!is.na(ward_at_sale)) %>%
    mutate(
        correct_assignment = case_when(
            ward_regime == "pre_2015" & ward_at_sale == ward_pre_2015 ~ TRUE,
            ward_regime == "post_2015" & ward_at_sale == ward_post_2015 ~ TRUE,
            ward_regime == "post_2023" & ward_at_sale == ward_post_2023 ~ TRUE,
            TRUE ~ FALSE
        )
    ) %>%
    summarise(
        total = n(),
        correct = sum(correct_assignment),
        pct_correct = mean(correct_assignment) * 100
    )
message(sprintf(
    "  %s of %s sales (%.1f%%) correctly assigned to ward regime",
    format(sample_check$correct, big.mark = ","),
    format(sample_check$total, big.mark = ","),
    sample_check$pct_correct
))

# =============================================================================
# 12. SAVE OUTPUTS
# =============================================================================
message("\nSaving outputs...")

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
