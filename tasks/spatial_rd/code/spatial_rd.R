# This script runs spatial RD analyses on the dataset from the calculate_ward_boundary_distances task

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# --- 1. ARGUMENT HANDLING ---
# =======================================================================================
# --- Interactive Test Block (uncomment to run in RStudio) ---
# cat("--- RUNNING IN INTERACTIVE TEST MODE ---\n")
# yvar                   <- "density_far"
# bw                     <- 50
# kernel                 <- "epanechnikov"
# output_filename_rdplot   <- sprintf("../output/TEST_rd_plot_%s_bw%d_%s.png", yvar, bw, kernel)
# output_filename_scatter  <- sprintf("../output/TEST_rd_scatter_%s_bw%d_%s.png", yvar, bw, kernel)
# =======================================================================================
# --- Command-Line Arguments (uncomment for Makefile) ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5) {
  stop("FATAL: Script requires 5 arguments: <yvar> <bw> <kernel> <rd_plot_outfile> <rd_scatter_outfile>", call. = FALSE)
}
yvar                   <- args[1]
bw                     <- as.numeric(args[2])
kernel                 <- args[3]
output_filename_rdplot   <- args[4]
output_filename_scatter  <- args[5]
# =======================================================================================

# --- 2. LOAD AND PREPARE DATA ---

cat("Loading and preparing data...\n")
parcels <- st_read("../input/parcels_with_ward_distances.gpkg")
parcels <- as_tibble(st_drop_geometry(parcels))

parcels_analysis <- parcels %>%
  mutate(
    density_far = if_else(sa_lotsize > 0, sa_sqft / sa_lotsize, NA_real_),
    density_lapu = if_else(sa_nbr_units > 0, sa_lotsize / sa_nbr_units, NA_real_)
  ) %>%
  filter(!is.na(dist_to_boundary) & !is.na(ward_pair) & !is.na(strictness_index))

strictness_lookup <- parcels_analysis %>%
  group_by(ward, boundary_year) %>%
  summarise(strictness_index = mean(strictness_index, na.rm = TRUE), .groups = "drop")

parcels_signed <- parcels_analysis %>%
  mutate(
    wards_in_pair = str_split_fixed(ward_pair, "_", 2),
    ward_a = as.integer(wards_in_pair[, 1]),
    ward_b = as.integer(wards_in_pair[, 2]),
    other_ward = if_else(ward == ward_a, ward_b, ward_a)
  ) %>%
  left_join(strictness_lookup, by = c("other_ward" = "ward", "boundary_year" = "boundary_year")) %>%
  rename(strictness_own = strictness_index.x, strictness_neighbor = strictness_index.y) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_distance = dist_to_boundary * sign
  ) %>%
  filter(!is.na(signed_distance))

cat("Data preparation complete.\n")


# --- 3. RUN RDROBUST & EXTRACT ESTIMATES ---
cat("Running rdrobust for:", yvar, "...\n")
rd_robust_result <- rdrobust(
  y = parcels_signed[[yvar]],
  x = parcels_signed$signed_distance,
  c = 0,
  kernel = kernel,
  p = 1,
  q = 2,
  h = bw,
  cluster = parcels_signed$ward_pair
)
summary(rd_robust_result)

# Extract Bias-Corrected coefficient and Standard Error
coef_bc <- rd_robust_result$coef[3]
se_bc   <- rd_robust_result$se[3]

# Create a formatted string for the plot annotation
annotation_text <- sprintf("Estimate: %.3f (%.3f)", coef_bc, se_bc)


# --- 4. GENERATE PLOTS ---

# =======================================================================================
# Create better labels based on the yvar
if (yvar == "density_far") {
  y_axis_label <- "Floor-Area Ratio (FAR)"
  ylim = c(0,2.5)
} else if (yvar == "density_lapu") {
  y_axis_label <- "Lot Area Per Unit (LAPU)"
  ylim = c(0,5000)
} else {
  y_axis_label <- yvar # Fallback to the raw variable name if it's something else
}

plot_title <- "Discontinuity in Development Density at Ward Boundary"
# ================================================================================

## --- Plot 1: Binned rdplot ---
cat("Generating binned rdplot...\n")
rd_plot_object <- rdplot(
  y = parcels_signed[[yvar]], x = parcels_signed$signed_distance, c = 0,
  h = bw, p = 1, kernel = kernel,
  title = "", y.label = "", x.label = ""
)

plot1 <- ggplot() +
  geom_errorbar(data = rd_plot_object$vars_bins, aes(x = rdplot_mean_x, ymin = rdplot_ci_l, ymax = rdplot_ci_r), width = 10, color = "black", linewidth = 0.8) +
  geom_point(data = rd_plot_object$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y), color = "darkblue", size = 2.5) +
  geom_line(data = rd_plot_object$vars_poly, aes(x = rdplot_x, y = rdplot_y), color = "red", linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.2) +
  annotate("text", x = -Inf, y = 0, label = annotation_text, hjust = -0.1, vjust = 1.5, size = 3, fontface = "bold") +
  labs(
    title = plot_title,
    subtitle = paste0("bw: ", bw, "m | kernel: ", kernel),
    y = y_axis_label, x = "Distance to Stricter Ward Boundary (meters)"
  ) +
  coord_cartesian(xlim = c(-bw, bw), ylim = ylim) +
  theme_bw() + 
  theme(panel.grid.major = element_blank())

## --- Plot 2: Smoothed Scatterplot ---
cat("Generating smoothed scatterplot...\n")
plot2 <- ggplot(data = parcels_signed, aes_string(x = "signed_distance", y = yvar)) +
  geom_point(color = "grey60", alpha = 0.3, shape = 16) +
  geom_smooth(data = . %>% filter(signed_distance >= 0), method = "loess", se = TRUE, color = "#d14949", fill = "#d14949", alpha = 0.4) +
  geom_smooth(data = . %>% filter(signed_distance < 0), method = "loess", se = TRUE, color = "#496dd1", fill = "#496dd1", alpha = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  annotate("text", x = -Inf, y = 0, label = annotation_text, hjust = -0.1, vjust = 1.5, size = 3, fontface = "bold") +
  labs(
    title = plot_title,
    subtitle = paste("Bandwidth:", bw, "m"),
    y = y_axis_label, x = "Distance to Stricter Ward Boundary (meters)"
  ) +
  coord_cartesian(xlim = c(-bw, bw), ylim = ylim) +
  theme_bw() +
  theme(panel.grid = element_blank())

# --- 5. SAVE BOTH PLOTS ---
ggsave(output_filename_rdplot, plot = plot1, width = 8, height = 6, dpi = 300)
cat("✓ Binned plot saved to:", output_filename_rdplot, "\n")

ggsave(output_filename_scatter, plot = plot2, width = 8, height = 6, dpi = 300)
cat("✓ Scatterplot saved to:", output_filename_scatter, "\n")