# spatial_rd_rental_placebo_boundaries.R
# Placebo test: Run spatial RD at artificial boundaries (shifted cutoffs)
# If the true effect is at the ward boundary (distance = 0), we should see NULL effects
# at placebo cutoffs like -500, -250, +250, +500 feet.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/spatial_rd_rental_placebo_boundaries/code")

source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block ---
placebo_shift <- 100  # shift cutoff by 250 ft to the right (positive side)
# =======================================================================================

# args <- commandArgs(trailingOnly = TRUE)
# 
# if (length(args) < 1) {
#     stop("Usage: Rscript spatial_rd_rental_placebo_boundaries.R <placebo_shift>")
# }
# 
# placebo_shift <- as.numeric(args[1]) # e.g., -500, -250, 250, 500

# Fixed parameters for this placebo analysis
bw <- 50 # fixed bandwidth
kernel <- "triangular"
yvar <- "rent_price"
use_log <- F

message(sprintf("=== Placebo Boundary Test: Cutoff shifted by %d ft ===", placebo_shift))

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------
message("Loading rental data...")
df <- read_parquet("../input/rent_with_ward_distances.parquet")

# -----------------------------------------------------------------------------
# 2. PREPARE DATA
# -----------------------------------------------------------------------------
# Create shifted running variable: the "cutoff" is now at placebo_shift instead of 0
# This is equivalent to shifting the cutoff to the right/left
df <- df %>%
    filter(!is.na(rent_price), rent_price > 0) %>%
    filter(!is.na(signed_dist)) %>%
  # filter(building_type_clean == "multi_family") %>%
    # Shift the running variable so the new "cutoff" is at 0
    mutate(signed_dist_shifted = signed_dist - placebo_shift)

# Filter to bandwidth around the PLACEBO cutoff (which is now at 0 in shifted coords)
df_bw <- df %>%
    filter(abs(signed_dist_shifted) <= bw)

# Outcome
if (use_log) {
    df_bw$outcome <- log(df_bw[[yvar]])
    y_lab <- paste0("Log(", yvar, ")")
} else {
    df_bw$outcome <- df_bw[[yvar]]
    y_lab <- yvar
}

df_bw <- df_bw %>%
    filter(!is.na(outcome), !is.na(signed_dist_shifted))

message(sprintf("Observations within bandwidth: %d", nrow(df_bw)))

# Check balance around placebo cutoff
n_left <- sum(df_bw$signed_dist_shifted < 0)
n_right <- sum(df_bw$signed_dist_shifted >= 0)
message(sprintf("Left of placebo cutoff: %d | Right: %d", n_left, n_right))

if (n_left < 20 || n_right < 20) {
    stop("Insufficient observations on one side of the placebo cutoff.")
}

# -----------------------------------------------------------------------------
# 3. RUN RDROBUST at Placebo Cutoff
# -----------------------------------------------------------------------------
message(sprintf("Running RD at placebo cutoff (original distance = %d ft)", placebo_shift))

cluster_var <- if ("ward_pair_id" %in% names(df_bw)) df_bw$ward_pair_id else NULL

est <- rdrobust(
    y = df_bw$outcome,
    x = df_bw$signed_dist_shifted, # Running variable is now shifted
    c = 0, # Cutoff is at 0 (in shifted coordinates = placebo_shift in original)
    h = bw,
    kernel = kernel,
    cluster = cluster_var,
    p = 1
)

summary(est)

# Extract Results - Conventional
coef_conv <- est$coef["Conventional", 1]
se_conv <- est$se["Conventional", 1]
pval_conv <- est$pv["Conventional", 1]

# Extract Results - Robust
coef_robust <- est$coef["Robust", 1]
se_robust <- est$se["Robust", 1]
pval_robust <- est$pv["Robust", 1]

n_obs <- sum(est$N)

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
# 4. GENERATE PLOT
# -----------------------------------------------------------------------------
message("Generating placebo RD plot...")

K <- 30 # bins per side
col_points <- "#4C72B0"
col_lines <- "#C44E52"
col_ci <- "grey85"

# Generate rdplot for binned means
rd_plot <- rdplot(
    y = df_bw$outcome,
    x = df_bw$signed_dist_shifted,
    c = 0,
    h = bw,
    p = 1,
    kernel = kernel,
    binselect = "es",
    nbins = c(K, K),
    title = "", x.label = "", y.label = ""
)

bin_data <- rd_plot$vars_bins
y_quantiles <- quantile(df_bw$outcome, c(0.01, 0.99), na.rm = TRUE)

# Determine title based on shift direction
if (placebo_shift > 0) {
    cutoff_label <- sprintf("Placebo Cutoff: %d ft INTO Stricter Ward", placebo_shift)
} else if (placebo_shift < 0) {
    cutoff_label <- sprintf("Placebo Cutoff: %d ft INTO Lenient Ward", abs(placebo_shift))
} else {
    cutoff_label <- "True Cutoff (0 ft)"
}

p <- ggplot() +
    geom_point(
        data = bin_data, aes(x = rdplot_mean_x, y = rdplot_mean_y),
        color = col_points, alpha = 0.9, size = 2
    ) +
    geom_smooth(
        data = df_bw %>% filter(signed_dist_shifted < 0),
        aes(x = signed_dist_shifted, y = outcome),
        method = "lm", color = col_lines, fill = col_ci, alpha = 0.5, linewidth = 1.2
    ) +
    geom_smooth(
        data = df_bw %>% filter(signed_dist_shifted >= 0),
        aes(x = signed_dist_shifted, y = outcome),
        method = "lm", color = col_lines, fill = col_ci, alpha = 0.5, linewidth = 1.2
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.6) +
    labs(
        title = cutoff_label,
        subtitle = sprintf("BW = %d ft | N = %s | Kernel = %s", bw, format(n_obs, big.mark = ","), kernel),
        x = "Distance to Placebo Cutoff (ft)",
        y = y_lab
    ) +
    scale_y_continuous(limits = y_quantiles) +
    annotate("text",
        x = -Inf, y = -Inf, label = annot_text,
        hjust = -0.1, vjust = -0.5, fontface = "bold", size = 4
    ) +
    theme_minimal(base_size = 14) +
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey92"),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "grey40"),
        axis.title = element_text(face = "bold", size = 12)
    )
p
# -----------------------------------------------------------------------------
# 5. SAVE OUTPUT
# -----------------------------------------------------------------------------
# Create filename based on shift direction
shift_label <- if (placebo_shift >= 0) sprintf("plus%d", placebo_shift) else sprintf("minus%d", abs(placebo_shift))
output_file <- sprintf("../output/placebo_rd_shift_%s_bw%d.pdf", shift_label, bw)

ggsave(output_file, plot = p, width = 8, height = 6)
message(sprintf("✓ Placebo plot saved to: %s", output_file))

# Also save summary statistics to CSV for easy comparison
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

results_file <- sprintf("../output/placebo_results_shift_%s_bw%d.csv", shift_label, bw)
write_csv(results_df, results_file)
message(sprintf("✓ Results saved to: %s", results_file))
