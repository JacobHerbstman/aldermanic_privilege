# spatial_rd_same_zone_only_placebo_boundaries.R
# Placebo test: Run spatial RD at artificial boundaries (shifted cutoffs)
# Same-zone restriction still applies. If the true effect is at ward boundary (0),
# we should see NULL effects at placebo cutoffs like -500, -250, +250, +500 feet.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/spatial_rd_same_zone_only_placebo_boundaries/code")

source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block ---
# placebo_shift <- 250  # shift cutoff by 250 ft to the right
# =======================================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
    stop("Usage: Rscript spatial_rd_same_zone_only_placebo_boundaries.R <placebo_shift>")
}

placebo_shift <- as.numeric(args[1]) # e.g., -500, -250, 250, 500

# Fixed parameters for this placebo analysis
bw <- 500 # fixed bandwidth
kernel <- "triangular"
yvar <- "density_dupac" # same outcome as original
use_log <- TRUE
donut <- 0

message(sprintf("=== Placebo Boundary Test (Same Zone): Cutoff shifted by %d ft ===", placebo_shift))

# -----------------------------------------------------------------------------
# 1. LOAD & PREPARE DATA
# -----------------------------------------------------------------------------
message("Loading and preparing data...")
dat_raw <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
    filter(arealotsf > 1) %>%
    filter(areabuilding > 1) %>%
    filter(unitscount > 1) %>%
    filter(unitscount > 1 & unitscount <= 100)

# Create outcome and shifted running variable
dat <- dat_raw %>%
    mutate(
        outcome = if (use_log) log(.data[[yvar]]) else .data[[yvar]],
        # Shift the running variable so the placebo cutoff is at 0
        signed_distance_shifted = signed_distance - placebo_shift,
        within_bw = abs(signed_distance_shifted) <= bw
    )

# Restrict to parcels within bandwidth of PLACEBO cutoff
dat_bw <- dat %>%
    filter(within_bw, abs(signed_distance_shifted) >= donut)

# Count parcels by zone within each (boundary_year, ward_pair) window
zone_counts <- dat_bw %>%
    group_by(boundary_year, ward_pair, zone_code) %>%
    summarise(
        n        = n(),
        n_sides  = n_distinct(sign(signed_distance_shifted)), # ensure it exists on both sides of PLACEBO
        .groups  = "drop"
    ) %>%
    filter(n_sides == 2) # modal candidate must be present on both sides of placebo

# Pick the modal zone per (boundary_year, ward_pair)
modal_zone <- zone_counts %>%
    arrange(boundary_year, ward_pair, desc(n), zone_code) %>%
    group_by(boundary_year, ward_pair) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(boundary_year, ward_pair, zone_code)

# Keep only parcels with modal zone
dat <- dat %>%
    filter(within_bw, abs(signed_distance_shifted) >= donut) %>%
    mutate(
        side       = as.integer(signed_distance_shifted > 0),
        abs_dist   = abs(signed_distance_shifted),
        cluster_id = paste(boundary_year, ward_pair, zone_code, sep = "_")
    )

if (nrow(dat) == 0) {
    stop("No ward-pair segments with same zoning on both sides inside the placebo bandwidth.")
}

message(sprintf("Sample size after restrictions: %d", nrow(dat)))

# Check balance around placebo cutoff
n_left <- sum(dat$signed_distance_shifted < 0)
n_right <- sum(dat$signed_distance_shifted >= 0)
message(sprintf("Left of placebo cutoff: %d | Right: %d", n_left, n_right))

if (n_left < 10 || n_right < 10) {
    message("WARNING: Few observations on one side of the placebo cutoff.")
}

# -----------------------------------------------------------------------------
# 2. RUN RDROBUST at Placebo Cutoff
# -----------------------------------------------------------------------------
message(sprintf("Running RD at placebo cutoff (original distance = %d ft)", placebo_shift))

rd_result <- rdrobust(
    y       = dat$outcome,
    x       = dat$signed_distance_shifted, # Shifted running variable
    c       = 0, # Cutoff at 0 in shifted coords = placebo_shift in original
    kernel  = kernel,
    p       = 1,
    h       = bw,
    cluster = dat$cluster_id
)

summary(rd_result)

# Extract Results
coef_conv <- rd_result$coef[1]
se_conv <- rd_result$se[1]
pval_conv <- rd_result$pv[1]

coef_robust <- rd_result$coef[3]
se_robust <- rd_result$se[3]
pval_robust <- rd_result$pv[3]

n_obs <- nrow(dat)

# Significance stars
stars_conv <- case_when(
    pval_conv < 0.01 ~ "***",
    pval_conv < 0.05 ~ "**",
    pval_conv < 0.1 ~ "*",
    TRUE ~ ""
)

stars_robust <- case_when(
    pval_robust < 0.01 ~ "***",
    pval_robust < 0.05 ~ "**",
    pval_robust < 0.1 ~ "*",
    TRUE ~ ""
)

annot_text <- sprintf(
    "Conventional: %.3f%s (%.3f)\nRobust: %.3f%s (%.3f)",
    coef_conv, stars_conv, se_conv,
    coef_robust, stars_robust, se_robust
)

# -----------------------------------------------------------------------------
# 3. GENERATE PLOT
# -----------------------------------------------------------------------------
message("Generating placebo RD plot...")

K <- 30
y_axis_label <- if (use_log) "Log(DUPAC)" else "DUPAC"
ylim <- if (use_log) c(2, 6) else c(0, 150)

rd_plot <- rdplot(
    y = dat$outcome,
    x = dat$signed_distance_shifted,
    c = 0,
    h = bw,
    p = 1,
    kernel = kernel,
    binselect = "es",
    nbins = c(K, K),
    title = "", x.label = "", y.label = ""
)

bins <- rd_plot$vars_bins

# Determine title based on shift direction
if (placebo_shift > 0) {
    cutoff_label <- sprintf("Placebo Cutoff: %d ft INTO Stricter Ward (Same Zone)", placebo_shift)
} else if (placebo_shift < 0) {
    cutoff_label <- sprintf("Placebo Cutoff: %d ft INTO Lenient Ward (Same Zone)", abs(placebo_shift))
} else {
    cutoff_label <- "True Cutoff (0 ft)"
}

p <- ggplot() +
    geom_point(
        data = bins,
        aes(x = rdplot_mean_x, y = rdplot_mean_y),
        color = "darkblue", size = 1.5, alpha = 0.7
    ) +
    geom_smooth(
        data = dat %>% filter(signed_distance_shifted < 0),
        aes(x = signed_distance_shifted, y = outcome),
        method = "lm", color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1
    ) +
    geom_smooth(
        data = dat %>% filter(signed_distance_shifted >= 0),
        aes(x = signed_distance_shifted, y = outcome),
        method = "lm", color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1
    ) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    annotate("text",
        x = -Inf, y = ylim[2], label = annot_text,
        hjust = -0.1, vjust = 1.5, size = 3, fontface = "bold"
    ) +
    labs(
        title = cutoff_label,
        subtitle = sprintf("BW = %d ft | N = %s | Kernel = %s", bw, format(n_obs, big.mark = ","), kernel),
        x = "Distance to Placebo Cutoff (ft)",
        y = y_axis_label
    ) +
    coord_cartesian(xlim = c(-bw, bw), ylim = ylim) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold")
    )

# -----------------------------------------------------------------------------
# 4. SAVE OUTPUT
# -----------------------------------------------------------------------------
shift_label <- if (placebo_shift >= 0) sprintf("plus%d", placebo_shift) else sprintf("minus%d", abs(placebo_shift))
output_file <- sprintf("../output/placebo_rd_same_zone_shift_%s_bw%d.pdf", shift_label, bw)

ggsave(output_file, plot = p, width = 8, height = 6)
message(sprintf("✓ Placebo plot saved to: %s", output_file))

# Save summary statistics to CSV
results_df <- data.frame(
    placebo_shift = placebo_shift,
    bandwidth = bw,
    n_obs = n_obs,
    coef_conv = coef_conv,
    se_conv = se_conv,
    pval_conv = pval_conv,
    coef_robust = coef_robust,
    se_robust = se_robust,
    pval_robust = pval_robust
)

results_file <- sprintf("../output/placebo_results_same_zone_shift_%s_bw%d.csv", shift_label, bw)
write_csv(results_df, results_file)
message(sprintf("✓ Results saved to: %s", results_file))
