# This script runs disaggregated spatial RD analyses for each unique ward boundary pair.
# It iterates through each pair, runs an RD, and saves a unique plot.

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# --- 1. ARGUMENT HANDLING ---
# =======================================================================================
# # --- Interactive Test Block (uncomment to run in RStudio) ---
# cat("--- RUNNING IN INTERACTIVE TEST MODE ---\n")
# yvar          <- "density_far"
# use_log       <- FALSE
# bw            <- 2112 # 0.1 miles in feet
# kernel        <- "triangular"
# # =======================================================================================
# --- Command-Line Arguments (uncomment for Makefile) ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("FATAL: Script requires 4 arguments: <yvar> <use_log> <bw> <kernel>", call. = FALSE)
}
yvar       <- args[1]
use_log    <- as.logical(args[2])
bw         <- as.numeric(args[3])
kernel     <- args[4]
# =======================================================================================

# --- 2. LOAD & PREP DATA ------------------------------------------------------
cat("Loading and preparing data...\n")
parcels_signed <- readr::read_csv("../input/parcels_with_ward_distances.csv")

# Keep positive outcomes and build 'outcome' (log if requested)
parcels_signed <- parcels_signed[parcels_signed[[yvar]] > 0, ]
if (use_log) {
  parcels_signed$outcome <- log(parcels_signed[[yvar]])
  log_suffix <- "_log"
} else {
  parcels_signed$outcome <- parcels_signed[[yvar]]
  log_suffix <- ""
}
cat("Data preparation complete.\n")

# --- 3. DYNAMIC LABELS / LIMITS ----------------------------------------------
if (yvar == "density_far") {
  y_axis_label <- "Floor-Area Ratio (FAR)"
  ylim <- if (use_log) c(-1, 1) else c(0, 2)
} else if (yvar == "density_lapu") {
  y_axis_label <- "Lot Area Per Unit (LAPU)"
  ylim <- if (use_log) c(6, 8) else c(0, 2500)
} else if (yvar == "density_bcr") {
  y_axis_label <- "Building Coverage Ratio (BCR)"
  ylim <- if (use_log) c(-2, 0) else c(0, 1)
} else if (yvar == "density_lps") {
  y_axis_label <- "Lot Size Per Story (LPS)"
  ylim <- if (use_log) c(6, 8) else c(0, 3000)
} else if (yvar == "density_spu") {
  y_axis_label <- "Square Feet Per Unit (SPU)"
  ylim <- if (use_log) c(6.5, 8) else c(0, 3000)
} else {
  y_axis_label <- yvar
  ylim <- NULL
}
if (use_log) y_axis_label <- paste0("Log(", y_axis_label, ")")

# --- 4. LOOP OVER PAIRS -------------------------------------------------------
unique_ward_pairs <- unique(na.omit(parcels_signed$ward_pair))
cat(sprintf("Found %d unique ward pairs. Starting analysis loop...\n",
            length(unique_ward_pairs)))

K <- 30  # equal bins per side
pair = unique_ward_pairs[12]  # for testing
for (pair in unique_ward_pairs) {
  
  cat(sprintf("--- Processing pair: %s ---\n", pair))
  
  # Pair slice
  pair_data <- dplyr::filter(parcels_signed, ward_pair == pair)
  
  # Within-window data (what we plot)
  df_bw <- dplyr::filter(pair_data, abs(signed_distance) <= bw)
  
  # Skip if insufficient observations on either side *within the RD window*
  n_left  <- sum(df_bw$signed_distance < 0, na.rm = TRUE)
  n_right <- sum(df_bw$signed_distance >= 0, na.rm = TRUE)
  if (n_left < 10 || n_right < 10) {
    cat("Skipping pair (too few obs within bandwidth on one/both sides).\n")
    next
  }
  
  # rdrobust for annotation (BC estimate)
  rd_robust_result <- tryCatch({
    rdrobust::rdrobust(
      y = pair_data$outcome,
      x = pair_data$signed_distance,
      c = 0, kernel = kernel, p = 1, q = 2, h = bw
    )
  }, error = function(e) {
    cat(sprintf("Error running rdrobust for pair %s: %s\n", pair, e$message))
    return(NULL)
  })
  if (is.null(rd_robust_result)) next
  
  coef_bc <- rd_robust_result$coef[3]
  se_bc   <- rd_robust_result$se[3]
  p_bc    <- rd_robust_result$pv[3]
  stars <- dplyr::case_when(
    p_bc <= .01 ~ "***",
    p_bc <= .05 ~ "**",
    p_bc <= .10 ~ "*",
    TRUE ~ ""
  )
  annotation_text <- sprintf("Estimate: %.3f%s (%.3f)", coef_bc, stars, se_bc)
  
  # rdplot for equal bins per side (points only)
  rd_plot_object <- rdrobust::rdplot(
    y = df_bw$outcome,
    x = df_bw$signed_distance,
    c = 0,
    h = bw,
    p = 1,
    kernel = kernel,     # irrelevant for bins, kept for consistency
    nbins = c(K, K),     # equal number of bins per side
    binselect = "es",
    x.lim = c(-bw, bw),
    y.lim = ylim,
    title = "", y.label = "", x.label = ""
  )
  bins <- rd_plot_object$vars_bins
  
  # Title for this pair
  plot_title <- sprintf("Discontinuity at Boundary: Wards %s",
                        gsub("_", " & ", pair))
  
  # Safe y for annotation if ylim is NULL
  y_annot <- if (is.null(ylim)) min(df_bw$outcome, na.rm = TRUE) else ylim[1]
  
  # Build figure: binned dots + LOESS w/ ribbons on each side
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(x = rdplot_mean_x, y = rdplot_mean_y),
      color = "darkblue", size = 1.5, alpha = 0.7
    ) +
    ggplot2::geom_smooth(
      data = dplyr::filter(df_bw, signed_distance < 0),
      ggplot2::aes(x = signed_distance, y = outcome),
      method = "loess", formula = y ~ x, se = TRUE, level = 0.95, na.rm = TRUE,
      color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1
    ) +
    ggplot2::geom_smooth(
      data = dplyr::filter(df_bw, signed_distance >= 0),
      ggplot2::aes(x = signed_distance, y = outcome),
      method = "loess", formula = y ~ x, se = TRUE, level = 0.95, na.rm = TRUE,
      color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1, span = 0.75
    ) +
    ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    ggplot2::annotate(
      "text", x = -Inf, y = y_annot, label = annotation_text,
      hjust = -0.1, vjust = 1.5, size = 3, fontface = "bold"
    ) +
    ggplot2::labs(
      title    = plot_title,
      subtitle = paste0("bw: ", round(bw / 5280, 2), " miles",
                        " | kernel: ", kernel),
      y = y_axis_label,
      x = "Distance to Stricter Ward Boundary (feet)"
    ) +
    ggplot2::coord_cartesian(xlim = c(-bw, bw), ylim = ylim) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12)
    )
  
  # Save
  out_file <- file.path(
    "../output",
    sprintf("rd_plot_%s%s_%s_bw%d_%s.png", pair, log_suffix, yvar, bw, kernel)
  )
  ggplot2::ggsave(out_file, plot = p, width = 8, height = 6, dpi = 300)
  cat(sprintf("✓ Plot saved to: %s\n", out_file))
}

cat("--- Disaggregated analysis complete. ---\n")