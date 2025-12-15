# spatial_rd_same_zone_only_function_robustness.R
# Robustness check: Run spatial RD with different polynomial orders (p = 1, 2, 3)
# Same-zone restriction applies. Tests whether results are robust to functional form.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/spatial_rd_same_zone_only_function_robustness/code")

source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block ---
poly_order <- 1  # polynomial order: 1=linear, 2=quadratic, 3=cubic
# =======================================================================================

# args <- commandArgs(trailingOnly = TRUE)
# 
# if (length(args) < 1) {
#     stop("Usage: Rscript spatial_rd_same_zone_only_function_robustness.R <poly_order>")
# }
# 
# poly_order <- as.integer(args[1]) # 1, 2, or 3

# Fixed parameters
bw <- 500 # bandwidth in feet
kernel <- "triangular"
yvar <- "density_dupac"
use_log <- TRUE
donut <- 0

# Polynomial order label
poly_labels <- c("1" = "Linear", "2" = "Quadratic", "3" = "Cubic")
poly_label <- poly_labels[as.character(poly_order)]

message(sprintf("=== Function Robustness Check: %s (p = %d) ===", poly_label, poly_order))

# -----------------------------------------------------------------------------
# 1. LOAD & PREPARE DATA
# -----------------------------------------------------------------------------
message("Loading and preparing data...")
dat_raw <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
    filter(arealotsf > 1) %>%
    filter(areabuilding > 1) %>%
    filter(unitscount > 1) %>%
    filter(unitscount > 1 & unitscount <= 100)

# Create outcome
dat <- dat_raw %>%
    mutate(
        outcome = if (use_log) log(.data[[yvar]]) else .data[[yvar]],
        within_bw = abs(signed_distance) <= bw
    )

# Restrict to parcels within bandwidth
dat_bw <- dat %>%
    filter(within_bw, abs(signed_distance) >= donut)

# Count parcels by zone within each (boundary_year, ward_pair) window
zone_counts <- dat_bw %>%
    group_by(boundary_year, ward_pair, zone_code) %>%
    summarise(
        n        = n(),
        n_sides  = n_distinct(sign(signed_distance)),
        .groups  = "drop"
    ) %>%
    filter(n_sides == 2)

# Pick the modal zone per (boundary_year, ward_pair)
modal_zone <- zone_counts %>%
    arrange(boundary_year, ward_pair, desc(n), zone_code) %>%
    group_by(boundary_year, ward_pair) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(boundary_year, ward_pair, zone_code)

# Keep only parcels with modal zone
dat <- dat %>%
    filter(within_bw, abs(signed_distance) >= donut) %>%
    mutate(
        side       = as.integer(signed_distance > 0),
        abs_dist   = abs(signed_distance),
        cluster_id = paste(boundary_year, ward_pair, zone_code, sep = "_")
    )

if (nrow(dat) == 0) {
    stop("No ward-pair segments with same zoning on both sides inside bandwidth.")
}

message(sprintf("Sample size after restrictions: %d", nrow(dat)))

# -----------------------------------------------------------------------------
# 2. RUN RDROBUST with specified polynomial order
# -----------------------------------------------------------------------------
message(sprintf("Running rdrobust with %s polynomial (p = %d)...", poly_label, poly_order))

rd_result <- rdrobust(
    y       = dat$outcome,
    x       = dat$signed_distance,
    c       = 0,
    kernel  = kernel,
    p       = poly_order, # KEY: polynomial order
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
    "Estimate: %.3f%s (%.3f)",
    coef_robust, stars_robust, se_robust
)

# -----------------------------------------------------------------------------
# 3. GENERATE PLOT
# -----------------------------------------------------------------------------
message("Generating RD plot...")

K <- 30
y_axis_label <- if (use_log) "Log(DUPAC)" else "DUPAC"
ylim <- if (use_log) c(2, 6) else c(0, 150)

# Use rdplot for binned means
rd_plot <- rdplot(
    y = dat$outcome,
    x = dat$signed_distance,
    c = 0,
    h = bw,
    p = poly_order, # Match polynomial order
    kernel = kernel,
    binselect = "es",
    nbins = c(K, K),
    title = "", x.label = "", y.label = ""
)

bins <- rd_plot$vars_bins

# Fit polynomial for plotting
fit_left <- lm(outcome ~ poly(signed_distance, poly_order, raw = TRUE),
    data = dat %>% filter(signed_distance < 0)
)
fit_right <- lm(outcome ~ poly(signed_distance, poly_order, raw = TRUE),
    data = dat %>% filter(signed_distance >= 0)
)

# Generate prediction data
pred_left <- data.frame(signed_distance = seq(-bw, 0, length.out = 100))
pred_left$outcome <- predict(fit_left, pred_left)

pred_right <- data.frame(signed_distance = seq(0, bw, length.out = 100))
pred_right$outcome <- predict(fit_right, pred_right)

p <- ggplot() +
    geom_point(
        data = bins,
        aes(x = rdplot_mean_x, y = rdplot_mean_y),
        color = "darkblue", size = 1.5, alpha = 0.7
    ) +
    # Polynomial fits
    geom_line(
        data = pred_left, aes(x = signed_distance, y = outcome),
        color = "#d14949", linewidth = 1
    ) +
    geom_line(
        data = pred_right, aes(x = signed_distance, y = outcome),
        color = "#d14949", linewidth = 1
    ) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    annotate("text",
        x = -Inf, y = ylim[1], label = annot_text,
        hjust = -0.1, vjust = 1.5, size = 3, fontface = "bold"
    ) +
    labs(
        title = sprintf("RD with %s Polynomial (p = %d)", poly_label, poly_order),
        subtitle = sprintf(
            "BW = %d ft | N = %s | Kernel = %s | Same Zone Only",
            bw, format(n_obs, big.mark = ","), kernel
        ),
        x = "Distance to Stricter Ward Boundary (ft)",
        y = y_axis_label
    ) +
    coord_cartesian(xlim = c(-bw, bw), ylim = ylim) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "grey40")
    )

# -----------------------------------------------------------------------------
# 4. SAVE OUTPUT
# -----------------------------------------------------------------------------
output_file <- sprintf(
    "../output/rd_same_zone_%s_p%d_bw%d.pdf",
    tolower(poly_label), poly_order, bw
)

ggsave(output_file, plot = p, width = 8, height = 6)
message(sprintf("✓ RD plot saved to: %s", output_file))

# Save summary statistics to CSV
results_df <- data.frame(
    polynomial = poly_label,
    poly_order = poly_order,
    bandwidth = bw,
    n_obs = n_obs,
    coef_conv = coef_conv,
    se_conv = se_conv,
    pval_conv = pval_conv,
    coef_robust = coef_robust,
    se_robust = se_robust,
    pval_robust = pval_robust
)

results_file <- sprintf(
    "../output/rd_results_same_zone_%s_p%d_bw%d.csv",
    tolower(poly_label), poly_order, bw
)
write_csv(results_df, results_file)
message(sprintf("✓ Results saved to: %s", results_file))
