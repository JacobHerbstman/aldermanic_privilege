# This script runs spatial RD analyses on the dataset from the calculate_ward_boundary_distances task

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# --- 1. ARGUMENT HANDLING ---
# =======================================================================================
# --- Interactive Test Block (comment out if using Make) ---
# cat("--- RUNNING IN INTERACTIVE TEST MODE ---\n")
# yvar    <- "density_dupac"
# use_log <- T
# bw      <- 500             # outer bandwidth in feet
# kernel  <- "triangular"
# =======================================================================================
# --- Command-Line Arguments (backward compatible) ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) {
  stop("FATAL: Script requires 5 args: <yvar> <use_log> <bw> <kernel> <rd_plot_outfile> [donut]", call. = FALSE)
}
yvar <- args[1]
use_log <- as.logical(args[2])
bw <- as.numeric(args[3])
kernel <- args[4]
output_filename_rdplot <- args[5]
# =======================================================================================
if (!exists("donut")) donut <- 0
stopifnot(donut >= 0, donut < bw)

# --- 2) LOAD & PREPARE DATA ----------------------------------------------------
cat("Loading and preparing data...\n")
dat_raw <- read_csv("../input/parcels_with_ward_distances.csv") %>%
  filter(arealotsf > 1) %>%
  filter(areabuilding > 1) %>%
  filter(unitscount > 1) %>%
  filter(unitscount > 1 & unitscount <= 100)

# keep strictly positive outcomes (for logs) and build 'outcome'
dat <- dat_raw %>%
  mutate(
    outcome = if (use_log) log(.data[[yvar]]) else .data[[yvar]],
    within_bw = abs(signed_distance) <= bw
  )
# dat_raw[dat_raw[[yvar]] > 0,
# Restrict to parcels within bandwidth and outside donut with non-missing units
dat_bw <- dat %>%
  filter(within_bw, abs(signed_distance) >= donut)

# Count parcels by zone within each (boundary_year, ward_pair) window
zone_counts <- dat_bw %>%
  group_by(boundary_year, ward_pair, zone_code) %>%
  summarise(
    n        = n(),
    n_sides  = n_distinct(sign(signed_distance)), # ensure it exists on both sides
    .groups  = "drop"
  ) %>%
  filter(n_sides == 2) # modal candidate must be present on both sides

# Pick the modal zone per (boundary_year, ward_pair); deterministic tie-breaker by zone_code
modal_zone <- zone_counts %>%
  arrange(boundary_year, ward_pair, desc(n), zone_code) %>%
  group_by(boundary_year, ward_pair) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(boundary_year, ward_pair, zone_code)

# Keep only parcels in-bandwidth (and outside donut) with that modal zone
dat <- dat %>%
  filter(within_bw, abs(signed_distance) >= donut) %>%
  # inner_join(modal_zone, by = c("boundary_year", "ward_pair", "zone_code")) %>%
  mutate(
    side       = as.integer(signed_distance > 0),
    abs_dist   = abs(signed_distance),
    cluster_id = paste(boundary_year, ward_pair, zone_code, sep = "_")
  )

# appears on both sides

if (nrow(dat) == 0) {
  stop("No ward-pair segments with the same zoning on both sides inside the chosen bandwidth.")
}

cat(
  "Sample size after restrictions:", nrow(dat),
  " | window [", donut, ", ", bw, "] feet.\n"
)

# --- 3) RD ESTIMATION -----------------------------------------------------------
cat("Running rdrobust for:", yvar, "(log =", use_log, ")...\n")
rd_robust_result <- rdrobust(
  y       = dat$outcome,
  x       = dat$signed_distance,
  c       = 0,
  kernel  = kernel,
  p       = 1,
  h       = bw,
  cluster = dat$cluster_id # cluster by (pair × zone), consistent with restriction
)
summary(rd_robust_result)

# Extract Results - Conventional
coef_conv <- rd_robust_result$coef[1]
se_conv <- rd_robust_result$se[1]
pval_conv <- rd_robust_result$pv[1]

# Extract Results - Robust
coef_robust <- rd_robust_result$coef[3]
se_robust <- rd_robust_result$se[3]
pval_robust <- rd_robust_result$pv[3]

# Stars for conventional
stars_conv <- case_when(
  pval_conv < 0.01 ~ "***",
  pval_conv < 0.05 ~ "**",
  pval_conv < 0.1 ~ "*",
  TRUE ~ ""
)

# Stars for robust
stars_robust <- case_when(
  pval_robust < 0.01 ~ "***",
  pval_robust < 0.05 ~ "**",
  pval_robust < 0.1 ~ "*",
  TRUE ~ ""
)

# Two-line annotation
annotation_text <- sprintf(
  "Conventional: %.3f%s (%.3f)\nRobust: %.3f%s (%.3f)",
  coef_conv, stars_conv, se_conv,
  coef_robust, stars_robust, se_robust
)
# --- 4. GENERATE PLOTS ---

# =======================================================================================
# Create better labels and dynamic y-axis limits based on yvar and log status
if (yvar == "density_far") {
  y_axis_label <- "Floor-Area Ratio (FAR)"
  if (use_log) {
    ylim <- c(-2, 2)
  } else {
    ylim <- c(0, 4)
  }
} else if (yvar == "density_lapu") {
  y_axis_label <- "Lot Area Per Unit (LAPU)"
  if (use_log) {
    ylim <- c(6, 8)
  } else {
    ylim <- c(500, 5000)
  }
} else if (yvar == "density_bcr") {
  y_axis_label <- "Building Coverage Ratio (BCR)"
  if (use_log) {
    ylim <- c(-2, 0)
  } else {
    ylim <- c(0, 1)
  }
} else if (yvar == "density_lps") {
  y_axis_label <- "Lot Size Per Story (LPS)"
  if (use_log) {
    ylim <- c(6, 8)
  } else {
    ylim <- c(0, 3000)
  }
} else if (yvar == "density_dupac") {
  y_axis_label <- "Dwelling Units Per Acre (DUPAC)"
  if (use_log) {
    ylim <- c(2, 6)
  } else {
    ylim <- c(0, 150)
  }
} else if (yvar == "unitscount") {
  y_axis_label <- "Units"
  if (use_log) {
    ylim <- c(0, 5)
  } else {
    ylim <- c(2, 20)
  }
} else {
  y_axis_label <- yvar
  ylim <- NULL # Let ggplot decide the limits for unknown variables
}

if (use_log) {
  y_axis_label <- paste("Log(", y_axis_label, ")")
}

plot_title <- "Discontinuity in Development Density at Ward Boundary"
# ================================================================================

# --- 4a) Binned rdplot using the SAME filtered data -----------------------------
cat("Generating binned rdplot...\n")
K <- 30
rd_plot_object <- rdplot(
  y = dat$outcome,
  x = dat$signed_distance,
  c = 0,
  h = bw,
  p = 1,
  kernel = kernel,
  nbins = c(K, K),
  binselect = "es",
  x.lim = c(-bw, bw),
  y.lim = ylim,
  title = "", y.label = "", x.label = ""
)
bins <- rd_plot_object$vars_bins

plot2 <- ggplot() +
  geom_point(
    data = bins,
    aes(x = rdplot_mean_x, y = rdplot_mean_y),
    color = "darkblue", size = 1.5, alpha = 0.7
  ) +
  geom_smooth(
    data = dat %>% filter(signed_distance < 0),
    aes(x = signed_distance, y = outcome),
    method = "lm", formula = y ~ x, se = TRUE, level = 0.95, na.rm = TRUE,
    color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1
  ) +
  geom_smooth(
    data = dat %>% filter(signed_distance >= 0),
    aes(x = signed_distance, y = outcome),
    method = "lm", formula = y ~ x, se = TRUE, span = 0.75, level = 0.95, na.rm = TRUE,
    color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1
  ) +
  geom_vline(xintercept = 0, color = "black", linewidth = .5) +
  geom_vline(
    xintercept = c(-donut, donut), linetype = "dashed",
    linewidth = .4, color = "grey40"
  ) +
  annotate("text",
    x = -Inf, y = if (is.null(ylim)) Inf else ylim[2],
    label = annotation_text, hjust = -0.1, vjust = 1.5, size = 3, fontface = "bold"
  ) +
  labs(
    title = plot_title,
    subtitle = paste0(
      "bw: ", round(bw, 2),
      " ft | kernel: ", kernel,
      " | N = ", nrow(dat)
    ),
    y = y_axis_label,
    x = "Distance to Stricter Ward Boundary (feet)"
  ) +
  coord_cartesian(xlim = c(-bw, bw), ylim = ylim) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12)
  )

plot2

# --- 5) SAVE --------------------------------------------------------------------
if (!exists("output_filename_rdplot")) {
  log_suffix <- if (use_log) "_log" else ""
  output_filename_rdplot <- sprintf("../output/rd_plot%s_%s_bw%d_%s.pdf", log_suffix, yvar, bw, kernel)
}
ggsave(output_filename_rdplot, plot = plot2, width = 8, height = 6, dpi = 300)
cat("✓ RD plot saved to:", output_filename_rdplot, "\n")
