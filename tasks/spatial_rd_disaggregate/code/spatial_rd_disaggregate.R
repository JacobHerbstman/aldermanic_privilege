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
# use_log       <- TRUE
# bw            <- 528 # 0.1 miles in feet
# kernel        <- "epanechnikov"
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

# --- 2. SETUP & DATA PREPARATION ---
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
cat("Data preparation complete.\n")

# --- 3. DYNAMIC PLOT LABELS ---
# Create better labels and dynamic y-axis limits based on yvar and log status
if (yvar == "density_far") {
  y_axis_label <- "Floor-Area Ratio (FAR)"
  if (use_log) {
    ylim <- c(-2, 1.5)
  } else {
    ylim <- c(0, 2.5)
  }
} else if (yvar == "density_lapu") {
  y_axis_label <- "Lot Area Per Unit (LAPU)"
  if (use_log) {
    ylim <- c(6, 9.5)
  } else {
    ylim <- c(0, 5000)
  }
} else if (yvar == "density_bcr") {
  y_axis_label <- "Building Coverage Ratio (BCR)"
  if (use_log) {
    ylim <- c(-3, 0.5)
  } else {
    ylim <- c(0, 1)
  }
} else if (yvar == "density_lps") {
  y_axis_label <- "Lot Size Per Story (LPS)"
  if (use_log) {
    ylim <- c(5, 10)
  } else {
    ylim <- c(0, 5000)
  }
} else if (yvar == "density_spu") {
  y_axis_label <- "Square Feet Per Unit (SPU)"
  if (use_log) {
    ylim <- c(6, 9)
  } else {
    ylim <- c(0, 5000)
  }
} else {
  y_axis_label <- yvar
  ylim <- NULL # Let ggplot decide the limits for unknown variables
}

if (use_log) {
  y_axis_label <- paste("Log(", y_axis_label, ")")
}

plot_title <- "Discontinuity in Development Density at Ward Boundary"

# --- 4. LOOP THROUGH EACH WARD PAIR AND GENERATE PLOTS ---

unique_ward_pairs <- unique(parcels_signed$ward_pair)
unique_ward_pairs <- na.omit(unique_ward_pairs)

cat(sprintf("Found %d unique ward pairs. Starting analysis loop...\n", length(unique_ward_pairs)))

pair = unique_ward_pairs[12]  # for testing
for (pair in unique_ward_pairs) {
  
  cat(sprintf("--- Processing pair: %s ---\n", pair))
  
  # Filter data for the current pair
  pair_data <- parcels_signed %>% filter(ward_pair == pair)
  
  # # Check for sufficient data on both sides of the cutoff
  if (nrow(pair_data %>% filter(signed_distance < 0)) < 10 || nrow(pair_data %>% filter(signed_distance >= 0)) < 10) {
    cat("Skipping pair due to insufficient observations on one or both sides of the boundary.\n")
    next
  }
  
  # Run rdrobust within a tryCatch block to handle potential errors
  rd_robust_result <- tryCatch({
    rdrobust(
      y = pair_data$outcome,
      x = pair_data$signed_distance,
      c = 0, kernel = kernel, p = 1, q = 2, h = bw
    )
  }, error = function(e) {
    cat(sprintf("Error running rdrobust for pair %s: %s\n", pair, e$message))
    return(NULL) # Return NULL on error
  })
  
  # Skip this iteration if rdrobust failed
  if (is.null(rd_robust_result)) {
    next
  }
  
  # --- Extract estimates for plot annotation ---
  coef_bc <- rd_robust_result$coef[3]
  se_bc   <- rd_robust_result$se[3]
  p_bc <- rd_robust_result$pv[3]
  stars <- case_when(
    p_bc <= .01 ~ "***",
    p_bc <= .05 ~ "**",
    p_bc <= .10 ~ "*",
    TRUE ~ ""
  )
  annotation_text <- sprintf("Estimate: %.3f%s (%.3f)", coef_bc, stars, se_bc)
  
  # --- Generate smoothed scatterplot for the pair ---
  plot_title <- sprintf("Discontinuity at Boundary: Wards %s", gsub("_", " & ", pair))
  
  p <- ggplot(data = pair_data, aes(x = signed_distance, y = outcome)) +
    geom_point(color = "grey60", alpha = 0.5, shape = 16) +
    geom_smooth(data = . %>% filter(signed_distance >= 0), method = "loess", se = TRUE, color = "#d14949", fill = "#d14949", alpha = 0.4) +
    geom_smooth(data = . %>% filter(signed_distance < 0), method = "loess", se = TRUE, color = "#496dd1", fill = "#496dd1", alpha = 0.4) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
    annotate("text", x = -Inf, y = ylim[1], label = annotation_text, hjust = -0.1, vjust = -0.5, size = 3, fontface = "bold") +
    labs(
      title = plot_title,
      subtitle = paste0("bw: ", round((bw/5280), 2), " miles | kernel: ", kernel),
      y = y_axis_label, x = "Distance to Stricter Ward Boundary (feet)"
    ) +
    coord_cartesian(xlim = c(-bw, bw), ylim = ylim) +
    theme_bw() +
    theme(panel.grid = element_blank(), plot.title = element_text(size = 12))
  
  # --- Save the plot ---
  output_filename <- file.path("../output", sprintf("rd_scatter_%s%s_%s_bw%d_%s.png", pair, log_suffix, yvar, bw, kernel))
  ggsave(output_filename, plot = p, width = 8, height = 6, dpi = 300)
  cat(sprintf("✓ Plot saved to: %s\n", output_filename))
}

cat("--- Disaggregated analysis complete. ---\n")