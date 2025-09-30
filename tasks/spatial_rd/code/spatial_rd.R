# This script runs spatial RD analyses on the dataset from the calculate_ward_boundary_distances task

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# --- 1. ARGUMENT HANDLING ---
# =======================================================================================
# --- Interactive Test Block (uncomment to run in RStudio) ---
# cat("--- RUNNING IN INTERACTIVE TEST MODE ---\n")
# yvar                   <- "density_far"
# use_log                 <- F
# bw                     <- 2112
# kernel                 <- "triangular"
# =======================================================================================
# --- Command-Line Arguments (uncomment for Makefile) ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5) {
  stop("FATAL: Script requires 5 arguments: <yvar> <use_log> <bw> <kernel> <rd_plot_outfile>", call. = FALSE)
}
yvar                   <- args[1]
use_log                <- as.logical(args[2])
bw                     <- as.numeric(args[3])
kernel                 <- args[4]
output_filename_rdplot <- args[5]
# # =======================================================================================

# --- 2. LOAD AND PREPARE DATA ---
cat("Loading and preparing data...\n")
parcels_signed <- read_csv("../input/parcels_with_ward_distances.csv")

# Filter out zero values and apply log if specified
parcels_signed <- parcels_signed[parcels_signed[[yvar]] > 0, ]
if (use_log) {
  parcels_signed$outcome <- log(parcels_signed[[yvar]])
  log_suffix <- "_log"
} else {
  parcels_signed$outcome <- parcels_signed[[yvar]]
  log_suffix <- ""
}

parcels_signed <- parcels_signed %>%
  filter(unitscount > 0) 

cat("Data preparation complete.\n")

# --- 3. RUN RDROBUST & EXTRACT ESTIMATES ---
cat("Running rdrobust for:", yvar, "(log =", use_log, ")...\n")
rd_robust_result <- rdrobust(
  y = parcels_signed$outcome, # <-- CHANGE 4: Use the new 'outcome' column
  x = parcels_signed$signed_distance,
  c = 0,
  kernel = kernel,
  p = 1,
  h = bw,
  cluster = parcels_signed$ward_pair
)
summary(rd_robust_result)

# Extract Bias-Corrected coefficient and Standard Error
coef_bc <- rd_robust_result$coef[3]
se_bc   <- rd_robust_result$se[3]
p_bc <- rd_robust_result$pv[3]

# Determine significance stars based on z-value
stars <- case_when(
  p_bc <= .10 & p_bc > .05 ~ "*", # p < 0.10
  p_bc <= .05 & p_bc > .01  ~ "**",  # p < 0.05
  p_bc <= .01 ~ "***",   # p < 0.01
  TRUE ~ ""
)

# Create a formatted string for the plot annotation
annotation_text <- sprintf("Estimate: %.3f%s (%.3f)", coef_bc, stars, se_bc)
annotation_text
# --- 4. GENERATE PLOTS ---

# =======================================================================================
# Create better labels based on the yvar
# Create better labels and dynamic y-axis limits based on yvar and log status
if (yvar == "density_far") {
  y_axis_label <- "Floor-Area Ratio (FAR)"
  if (use_log) {
    ylim <- c(-1, 1)
  } else {
    ylim <- c(0, 2)
  }
} else if (yvar == "density_lapu") {
  y_axis_label <- "Lot Area Per Unit (LAPU)"
  if (use_log) {
    ylim <- c(6, 8)
  } else {
    ylim <- c(0, 2500)
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
} else if (yvar == "density_spu") {
  y_axis_label <- "Square Feet Per Unit (SPU)"
  if (use_log) {
    ylim <- c(6.5, 8)
  } else {
    ylim <- c(0, 3000)
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

## --- Plot 1: Binned rdplot ---
cat("Generating binned rdplot...\n")

df_bw  <- parcels_signed %>% dplyr::filter(abs(signed_distance) <= bw)

K=30
rd_plot_object <- rdplot(
  y = df_bw$outcome,
  x = df_bw$signed_distance,
  c = 0,
  h = bw,
  p = 1,
  # kernel is irrelevant for bins; keep it if you like for consistency
  kernel = kernel,
  # force the same number of bins on each side:
  nbins = c(K,K),
  # pick spacing: "es" = evenly spaced in x, "qs" = quantile spaced
  binselect = "es",
  # keep your limits so bins are inside the RD window
  x.lim = c(-bw, bw),
  y.lim = ylim,
  title = "", y.label = "", x.label = ""
)

## get bins for ggplot
bins   <- rd_plot_object$vars_bins

plot2 <- ggplot() +
  # binned means (points only)
  geom_point(data = bins,
             aes(x = rdplot_mean_x, y = rdplot_mean_y),
             color = "darkblue", size = 1.5, alpha = 0.7) +
  
  # LOESS fits (se=TRUE draws the ribbons around the fitted line)
  geom_smooth(
    data = df_bw %>% dplyr::filter(signed_distance < 0),
    aes(x = signed_distance, y = outcome),
    method = "lm", formula = y ~ x, se = TRUE, level = 0.95, na.rm = TRUE,
    color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1
  ) +
  geom_smooth(
    data = df_bw %>% dplyr::filter(signed_distance >= 0),
    aes(x = signed_distance, y = outcome),
    method = "lm", formula = y ~ x, se = TRUE,
    span = 0.75, level = 0.95, na.rm = TRUE,
    color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1
  ) +
  
  geom_vline(xintercept = 0, color = "black", linewidth = .5) +
  annotate("text", x = -Inf, y = ylim[1], label = annotation_text,
           hjust = -0.1, vjust = 1.5, size = 3, fontface = "bold") +
  labs(
    title    = plot_title,
    subtitle = paste0("bw: ", round(bw/5280, 2),
                      " miles", " | kernel: ", kernel),
    y = y_axis_label,
    x = "Distance to Stricter Ward Boundary (feet)"
  ) +
  coord_cartesian(xlim = c(-bw, bw), ylim = ylim) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12))
plot2

# --- 5. SAVE PLOT ---
if (!exists("output_filename_rdplot")) {
  log_suffix <- if (use_log) "_log" else ""
  output_filename_rdplot <- sprintf( "../output/rd_plot%s_%s_bw%d_%s.pdf", log_suffix, yvar, bw, kernel)
}

ggsave(output_filename_rdplot, plot = plot2, width = 8, height = 6, dpi = 300)
cat("✓ RD plot saved to:", output_filename_rdplot, "\n")
