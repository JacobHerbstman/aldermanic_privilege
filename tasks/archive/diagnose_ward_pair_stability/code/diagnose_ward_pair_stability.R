# diagnose_ward_pair_stability.R
# Diagnoses whether ward_pair_id is stable within blocks across sales dates
# This is critical for event study FE validity

source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading transaction panel...")
data <- read_parquet("../input/sales_transaction_panel.parquet")
setDT(data)
message(sprintf("Loaded %s transactions", format(nrow(data), big.mark = ",")))

message("Loading treatment panel...")
treatment_panel <- fread("../input/block_treatment_panel.csv")
treatment_panel[, block_id := as.character(block_id)]

message("Loading census blocks for mapping...")
sf_use_s2(FALSE)
census_blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_transform(3435) %>% # Illinois East ftUS
    rename(block_id = GEOID10) %>%
    mutate(block_id = as.character(block_id)) %>%
    distinct(block_id, .keep_all = TRUE)

# Open output file for text summary
sink("../output/ward_pair_stability_summary.txt")

# =============================================================================
# STEP 1: WARD_PAIR_ID STABILITY BY BLOCK
# =============================================================================
message("\n=== STEP 1: WARD_PAIR_ID STABILITY BY BLOCK ===")
cat("\n=== STEP 1: WARD_PAIR_ID STABILITY BY BLOCK ===\n")

ward_pair_stability <- data[, .(
    n_sales = .N,
    n_ward_pairs = uniqueN(ward_pair_id),
    ward_pairs_list = paste(unique(ward_pair_id), collapse = ", "),
    n_years = uniqueN(sale_year),
    years_range = paste(min(sale_year), max(sale_year), sep = "-")
), by = .(cohort, block_id)]

# Summary: how many blocks have multiple ward_pair_ids?
cat("\nNumber of distinct ward_pair_ids per block:\n")
stability_summary <- ward_pair_stability[, .(n_blocks = .N), by = .(cohort, n_ward_pairs)]
stability_summary[, pct := 100 * n_blocks / sum(n_blocks), by = cohort]
print(stability_summary[order(cohort, n_ward_pairs)])

# Save block-level stability
fwrite(ward_pair_stability, "../output/ward_pair_stability_by_block.csv")

# =============================================================================
# STEP 2: FOCUS ON TREATED BLOCKS
# =============================================================================
message("\n=== STEP 2: TREATED BLOCKS ANALYSIS ===")
cat("\n\n=== STEP 2: TREATED BLOCKS ANALYSIS ===\n")

treated_blocks <- data[treat == 1, .(
    n_sales = .N,
    n_ward_pairs = uniqueN(ward_pair_id),
    ward_pairs_list = paste(unique(ward_pair_id), collapse = ", "),

    # Check pre vs. post treatment
    ward_pairs_pre = paste(unique(ward_pair_id[relative_year < 0]), collapse = ", "),
    ward_pairs_post = paste(unique(ward_pair_id[relative_year >= 0]), collapse = ", "),
    n_sales_pre = sum(relative_year < 0),
    n_sales_post = sum(relative_year >= 0)
), by = .(cohort, block_id)]

# Identify where ward_pair changed pre -> post
treated_blocks[, ward_pair_changed := ward_pairs_pre != ward_pairs_post &
    ward_pairs_pre != "" &
    ward_pairs_post != ""]

cat(sprintf("\nTotal treated blocks with sales: %d\n", nrow(treated_blocks)))
cat(sprintf(
    "Treated blocks with multiple ward_pair_ids: %d (%.1f%%)\n",
    sum(treated_blocks$n_ward_pairs > 1),
    100 * mean(treated_blocks$n_ward_pairs > 1)
))
cat(sprintf(
    "Treated blocks where ward_pair changed pre→post: %d (%.1f%%)\n",
    sum(treated_blocks$ward_pair_changed, na.rm = TRUE),
    100 * mean(treated_blocks$ward_pair_changed, na.rm = TRUE)
))

# By cohort
cat("\nBy cohort:\n")
treated_by_cohort <- treated_blocks[, .(
    n_blocks = .N,
    blocks_multi_wp = sum(n_ward_pairs > 1),
    pct_multi_wp = 100 * mean(n_ward_pairs > 1),
    blocks_wp_changed = sum(ward_pair_changed, na.rm = TRUE),
    pct_wp_changed = 100 * mean(ward_pair_changed, na.rm = TRUE)
), by = cohort]
print(treated_by_cohort)

# =============================================================================
# STEP 3: cohort_ward_pair_side FE STABILITY
# =============================================================================
message("\n=== STEP 3: cohort_ward_pair_side FE STABILITY ===")
cat("\n\n=== STEP 3: cohort_ward_pair_side FE STABILITY ===\n")

fe_stability <- data[, .(
    n_sales = .N,
    n_fe_cells = uniqueN(cohort_ward_pair_side),
    fe_cells_list = paste(unique(cohort_ward_pair_side), collapse = " | "),
    treat = first(treat)
), by = .(cohort, block_id)]

cat("\nBlocks with multiple FE cells (cohort_ward_pair_side):\n")
fe_by_treat <- fe_stability[, .(
    n_blocks = .N,
    blocks_single_fe = sum(n_fe_cells == 1),
    blocks_multi_fe = sum(n_fe_cells > 1),
    pct_multi_fe = 100 * mean(n_fe_cells > 1)
), by = .(cohort, treat)]
print(fe_by_treat)

cat("\nExample treated blocks with multiple FE cells:\n")
examples <- fe_stability[treat == 1 & n_fe_cells > 1][order(-n_fe_cells)][1:min(10, sum(fe_stability$treat == 1 & fe_stability$n_fe_cells > 1))]
if (nrow(examples) > 0) {
    print(examples[, .(cohort, block_id, n_sales, n_fe_cells, fe_cells_list)])
} else {
    cat("None found!\n")
}

# =============================================================================
# STEP 4: REGRESSION SAMPLE IMPACT
# =============================================================================
message("\n=== STEP 4: REGRESSION SAMPLE IMPACT ===")
cat("\n\n=== STEP 4: REGRESSION SAMPLE IMPACT ===\n")

data_with_stability <- merge(data,
    fe_stability[, .(cohort, block_id, n_fe_cells)],
    by = c("cohort", "block_id")
)

cat("\nObservations in blocks with unstable FEs:\n")
impact_summary <- data_with_stability[, .(
    total_obs = .N,
    obs_stable_fe = sum(n_fe_cells == 1),
    obs_unstable_fe = sum(n_fe_cells > 1),
    pct_unstable = 100 * mean(n_fe_cells > 1)
), by = .(cohort, treat)]
print(impact_summary)

# =============================================================================
# STEP 5: FE CELL BALANCE
# =============================================================================
message("\n=== STEP 5: FE CELL BALANCE ===")
cat("\n\n=== STEP 5: FE CELL BALANCE ===\n")

fe_cell_balance <- data[, .(
    n_obs = .N,
    n_blocks = uniqueN(block_id),
    n_treated_blocks = uniqueN(block_id[treat == 1]),
    n_control_blocks = uniqueN(block_id[treat == 0]),
    n_obs_pre = sum(relative_year < 0),
    n_obs_post = sum(relative_year >= 0),
    has_both_treat_control = (uniqueN(block_id[treat == 1]) > 0) & (uniqueN(block_id[treat == 0]) > 0),
    has_pre_and_post = (sum(relative_year < 0) > 0) & (sum(relative_year >= 0) > 0)
), by = .(cohort, cohort_ward_pair_side)]

cat(sprintf("\nTotal FE cells: %d\n", nrow(fe_cell_balance)))
cat(sprintf(
    "FE cells with both treated and control blocks: %d (%.1f%%)\n",
    sum(fe_cell_balance$has_both_treat_control),
    100 * mean(fe_cell_balance$has_both_treat_control)
))
cat(sprintf(
    "FE cells with both pre and post observations: %d (%.1f%%)\n",
    sum(fe_cell_balance$has_pre_and_post),
    100 * mean(fe_cell_balance$has_pre_and_post)
))

fwrite(fe_cell_balance, "../output/fe_cell_balance.csv")

# =============================================================================
# STEP 6: GEOGRAPHIC VISUALIZATION
# =============================================================================
message("\n=== STEP 6: CREATING MAP ===")
cat("\n\n=== STEP 6: GEOGRAPHIC VISUALIZATION ===\n")

# Get blocks with unstable ward_pair_id
unstable_blocks <- ward_pair_stability[n_ward_pairs > 1]

# Merge with treatment status
unstable_blocks <- merge(unstable_blocks,
    unique(data[, .(cohort, block_id, treat)]),
    by = c("cohort", "block_id")
)

cat(sprintf("Blocks with unstable ward_pair_id: %d\n", nrow(unstable_blocks)))
cat(sprintf("  - Treated: %d\n", sum(unstable_blocks$treat == 1)))
cat(sprintf("  - Control: %d\n", sum(unstable_blocks$treat == 0)))

# Create map if there are unstable blocks to visualize
if (nrow(unstable_blocks) > 0) {
    # Join with census blocks geometry
    map_data <- census_blocks %>%
        inner_join(unstable_blocks, by = "block_id") %>%
        mutate(treatment_status = if_else(treat == 1, "Treated", "Control"))

    if (nrow(map_data) > 0) {
        p <- ggplot(map_data) +
            geom_sf(aes(fill = treatment_status), color = NA, alpha = 0.7) +
            scale_fill_manual(
                values = c("Treated" = "#E41A1C", "Control" = "#377EB8"),
                name = "Treatment Status"
            ) +
            facet_wrap(~cohort) +
            theme_minimal() +
            labs(
                title = "Census Blocks with Unstable ward_pair_id",
                subtitle = "Blocks have different ward_pair_id across sales dates",
                caption = sprintf("Total unstable blocks: %d", nrow(unstable_blocks))
            ) +
            theme(
                legend.position = "bottom",
                plot.title = element_text(face = "bold")
            )

        ggsave("../output/unstable_blocks_map.pdf", p, width = 12, height = 8)
        message("Saved map to ../output/unstable_blocks_map.pdf")
    } else {
        message("No block geometries found for unstable blocks")
        # Create empty placeholder
        pdf("../output/unstable_blocks_map.pdf")
        plot.new()
        text(0.5, 0.5, "No unstable blocks found with matching geometries")
        dev.off()
    }
} else {
    message("No unstable blocks found - this is good!")
    # Create placeholder PDF
    pdf("../output/unstable_blocks_map.pdf")
    plot.new()
    text(0.5, 0.5, "No unstable blocks found - ward_pair_id is stable across all blocks!")
    dev.off()
}

# =============================================================================
# SEVERITY ASSESSMENT
# =============================================================================
cat("\n\n===================================================\n")
cat("SEVERITY ASSESSMENT\n")
cat("===================================================\n")

pct_treated_multi_wp <- 100 * mean(treated_blocks$n_ward_pairs > 1)
pct_treated_wp_changed <- 100 * mean(treated_blocks$ward_pair_changed, na.rm = TRUE)

if (pct_treated_wp_changed >= 10) {
    cat(sprintf(
        "\n⚠️  CRITICAL: %.1f%% of treated blocks have ward_pair_id that changed pre→post\n",
        pct_treated_wp_changed
    ))
    cat("   FIX REQUIRED: This breaks the event study identification.\n")
    cat("   Recommend implementing Option C from implementation plan.\n")
} else if (pct_treated_multi_wp >= 10) {
    cat(sprintf(
        "\n⚠️  WARNING: %.1f%% of treated blocks have multiple ward_pair_ids\n",
        pct_treated_multi_wp
    ))
    cat("   Consider implementing fix if this affects key comparisons.\n")
} else {
    cat(sprintf(
        "\n✓ LOW SEVERITY: Only %.1f%% of treated blocks have multiple ward_pair_ids\n",
        pct_treated_multi_wp
    ))
    cat("   The FE structure appears largely intact.\n")
}

sink()
message("\nDiagnostics complete! Results saved to ../output/")
message("See ward_pair_stability_summary.txt for full text summary.")
