# spatial_rd_rental_conley.R
# Runs stacked spatial RD on rental data with Conley spatial SEs.
# Combines rdrobust (conventional + bias-corrected) with feols (Conley SEs)
# Supports residualization on border-pair FEs for cleaner within-border comparisons

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/spatial_rd_rental_conley/code")

source("../../setup_environment/code/packages.R")
# -----------------------------------------------------------------------------
# 1. ARGUMENTS
# -----------------------------------------------------------------------------
# =======================================================================================
# --- Interactive Test Block (Uncomment to run in RStudio without Make) ---
input_file <- "../input/rent_with_ward_distances_full.parquet"
yvar <- "rent_price"
use_log <- TRUE
bw <- 250
kernel <- "triangular"
conley_cutoff <- 0.5 # in km
resid_type <- "border_x_month" # "none", "border", "border_month", "border_x_month"
output_file <- "../output/test_plot.pdf"
# =======================================================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 7) {
  stop("Usage: Rscript spatial_rd_rental_conley.R <input_file> <yvar> <use_log> <bw> <kernel> <conley_cutoff_km> <resid_type> [output_file]")
}

input_file <- args[1]
yvar <- args[2]
use_log <- as.logical(args[3])
bw <- as.numeric(args[4])
kernel <- args[5]
conley_cutoff <- as.numeric(args[6])
resid_type <- args[7] # "none", "border", "border_month", "border_x_month"
output_file <- if (length(args) >= 8) args[8] else NULL

# Validate resid_type
valid_resid_types <- c("none", "border", "border_month", "border_x_month")
if (!resid_type %in% valid_resid_types) {
  stop(sprintf("resid_type must be one of: %s", paste(valid_resid_types, collapse = ", ")))
}

# -----------------------------------------------------------------------------
# 2. LOAD DATA
# -----------------------------------------------------------------------------
message(sprintf("Loading rental data from: %s", input_file))
df <- read_parquet(input_file)

# -----------------------------------------------------------------------------
# 3. PREPARE DATA
# -----------------------------------------------------------------------------
# Filter to bandwidth window immediately to save memory
df_bw <- df %>%
  filter(abs(signed_dist) <= bw) %>%
  filter(!is.na(rent_price), rent_price > 0) %>%
  filter(!is.na(latitude), !is.na(longitude)) # Need coords for Conley
# filter(building_type_clean == "multi_family")

# Construct Raw Outcome Variable
if (use_log) {
  df_bw$outcome_raw <- log(df_bw[[yvar]])
  y_lab_base <- paste0("Log(", yvar, ")")
} else {
  df_bw$outcome_raw <- df_bw[[yvar]]
  y_lab_base <- yvar
}

# Remove NAs in outcome or running variable
df_bw <- df_bw %>%
  filter(!is.na(outcome_raw), !is.na(signed_dist), !is.na(ward_pair_id))

# Create year_month for optional FE
df_bw <- df_bw %>%
  mutate(year_month = zoo::as.yearmon(file_date))

n_obs <- nrow(df_bw)

# -----------------------------------------------------------------------------
# 4. RESIDUALIZE OUTCOME (if requested)
# -----------------------------------------------------------------------------
if (resid_type == "none") {
  df_bw$outcome <- df_bw$outcome_raw
  y_lab <- y_lab_base
  resid_label <- "Raw"
  message("Using raw outcome (no residualization)")
} else if (resid_type == "border") {
  # Residualize on ward_pair_id only (border-pair FE)
  message("Residualizing outcome on border-pair (ward_pair_id) FE...")
  fe_model <- feols(outcome_raw ~ 1 | ward_pair_id, data = df_bw)
  df_bw$outcome <- resid(fe_model)
  y_lab <- paste0(y_lab_base, " (Residualized)")
  resid_label <- "Border-Pair FE"
} else if (resid_type == "border_month") {
  # Residualize on ward_pair_id + year_month (additive FEs)
  message("Residualizing outcome on border-pair + month FE (additive)...")
  fe_model <- feols(outcome_raw ~ 1 | ward_pair_id + year_month, data = df_bw)
  df_bw$outcome <- resid(fe_model)
  y_lab <- paste0(y_lab_base, " (Residualized)")
  resid_label <- "Border + Month FE"
} else if (resid_type == "border_x_month") {
  # Residualize on ward_pair_id × year_month (interacted FE, like FE regression)
  message("Residualizing outcome on border-pair × month FE (interacted)...")
  fe_model <- feols(outcome_raw ~ 1 | ward_pair_id^year_month, data = df_bw)
  df_bw$outcome <- resid(fe_model)
  y_lab <- paste0(y_lab_base, " (Residualized)")
  resid_label <- "Border × Month FE"
}

# -----------------------------------------------------------------------------
# 5. RUN RDROBUST (Conventional + Bias-Corrected Estimates)
# -----------------------------------------------------------------------------
message(sprintf(
  "Running RD: %s ~ signed_dist | bw = %.0f | kernel = %s | Conley cutoff = %.2f km | resid = %s",
  y_lab, bw, kernel, conley_cutoff, resid_type
))

# Cluster by ward_pair_id
cluster_var <- df_bw$ward_pair_id

est_rd <- rdrobust(
  y = df_bw$outcome,
  x = df_bw$signed_dist,
  c = 0,
  h = bw,
  kernel = kernel,
  cluster = cluster_var,
  p = 1 # Linear local regression
)

summary(est_rd)

# Extract Conventional estimate
coef_conv <- est_rd$coef["Conventional", 1]
se_conv <- est_rd$se["Conventional", 1]
pval_conv <- est_rd$pv["Conventional", 1]

# Extract Bias-Corrected (Robust) estimate
coef_robust <- est_rd$coef["Robust", 1]
se_robust <- est_rd$se["Robust", 1]
pval_robust <- est_rd$pv["Robust", 1]

# -----------------------------------------------------------------------------
# 6. RUN FEOLS FOR CONLEY SEs (on Conventional estimate)
# -----------------------------------------------------------------------------
# Create kernel weights and treatment indicator to match rdrobust
df_bw <- df_bw %>%
  mutate(
    kern_weight = case_when(
      kernel == "triangular" ~ 1 - abs(signed_dist) / bw,
      kernel == "uniform" ~ 1,
      TRUE ~ 1 - abs(signed_dist) / bw
    ),
    treated = as.numeric(signed_dist >= 0)
  )

# Local linear RD: outcome ~ treated + signed_dist + treated:signed_dist
# Coefficient on 'treated' is the discontinuity at the cutoff
rd_feols <- feols(
  outcome ~ treated * signed_dist,
  data = df_bw,
  weights = ~kern_weight
)

# Verify feols gives same point estimate as rdrobust conventional
coef_feols <- coef(rd_feols)["treated"]
message(sprintf("Point estimate check: rdrobust = %.5f, feols = %.5f", coef_conv, coef_feols))

# Conley spatial SEs (cutoff in km)
vcov_conley_mat <- vcov_conley(rd_feols,
  lat = "latitude", lon = "longitude",
  cutoff = conley_cutoff
)
se_conley <- sqrt(vcov_conley_mat["treated", "treated"])
pval_conley <- 2 * pnorm(-abs(coef_conv / se_conley))

# -----------------------------------------------------------------------------
# 7. PRINT RESULTS COMPARISON
# -----------------------------------------------------------------------------
message("========================================")
message(sprintf("RD ESTIMATES COMPARISON (%s)", resid_label))
message("========================================")
message(sprintf(
  "Conventional:    %.4f (SE = %.4f, t = %6.2f)",
  coef_conv, se_conv, coef_conv / se_conv
))
message(sprintf(
  "Bias-Corrected:  %.4f (SE = %.4f, t = %6.2f)",
  coef_robust, se_robust, coef_robust / se_robust
))
message(sprintf(
  "Conley (%.1fkm):  %.4f (SE = %.4f, t = %6.2f)",
  conley_cutoff, coef_conv, se_conley, coef_conv / se_conley
))
message("========================================")

# Stars function
get_stars <- function(pval) {
  case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.1 ~ "*",
    TRUE ~ ""
  )
}

stars_conv <- get_stars(pval_conv)
stars_robust <- get_stars(pval_robust)
stars_conley <- get_stars(pval_conley)

# Annotation text for plot
annot_text <- sprintf(
  "Conventional:   %.3f%s (%.3f)\nBias-Corrected: %.3f%s (%.3f)\nConley (%.2fkm): %.3f%s (%.3f)",
  coef_conv, stars_conv, se_conv,
  coef_robust, stars_robust, se_robust,
  conley_cutoff, coef_conv, stars_conley, se_conley
)

# -----------------------------------------------------------------------------
# 8. GENERATE BINNED DATA FOR PLOT
# -----------------------------------------------------------------------------
# Use rdplot for consistent binning with original script
rd_plot <- rdplot(
  y = df_bw$outcome,
  x = df_bw$signed_dist,
  c = 0,
  h = bw,
  p = 1,
  kernel = kernel,
  binselect = "es",
  nbins = c(30, 30),
  title = "", x.label = "", y.label = ""
)

bin_data <- rd_plot$vars_bins

# -----------------------------------------------------------------------------
# 9. CREATE PLOT
# -----------------------------------------------------------------------------
col_points <- "#4C72B0"
col_lines <- "#C44E52"
col_ci <- "grey85"

# Title depends on residualization
if (resid_type == "none") {
  plot_title <- sprintf("Rental Price Discontinuity (BW = %d ft)", bw)
  plot_subtitle <- sprintf(
    "Outcome: %s | Kernel: %s | N = %s",
    y_lab_base, kernel, format(n_obs, big.mark = ",")
  )
} else {
  plot_title <- sprintf("Rental Price Discontinuity - %s (BW = %d ft)", resid_label, bw)
  plot_subtitle <- sprintf(
    "Outcome: %s | Kernel: %s | N = %s",
    y_lab, kernel, format(n_obs, big.mark = ",")
  )
}

p <- ggplot() +
  # Binned Means
  geom_point(
    data = bin_data,
    aes(x = rdplot_mean_x, y = rdplot_mean_y),
    fill = "#2C3E50", shape = 21, color = "white", size = 2.5, stroke = 0.3
  ) +
  # Linear Fit (Left - lenient side)
  geom_smooth(
    data = df_bw %>% filter(signed_dist < 0),
    aes(x = signed_dist, y = outcome, weight = kern_weight),
    method = "lm", color = "#4575B4", fill = "#4575B4", alpha = 0.2, linewidth = 1.2
  ) +
  # Linear Fit (Right - strict side)
  geom_smooth(
    data = df_bw %>% filter(signed_dist >= 0),
    aes(x = signed_dist, y = outcome, weight = kern_weight),
    method = "lm", color = "#D73027", fill = "#D73027", alpha = 0.2, linewidth = 1.2
  ) +
  # Cutoff Line
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) +
  # Labels
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Distance to Stricter Ward Boundary (ft)",
    y = y_lab
  ) +
  scale_y_continuous(limits = quantile(df_bw$outcome, c(0.01, 0.99), na.rm = TRUE)) +
  annotate("text",
    x = -Inf, y = -Inf, label = annot_text,
    hjust = -0.05, vjust = -0.5, fontface = "bold", size = 3.5
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey50"),
    axis.title = element_text(face = "bold", size = 12)
  )

print(p)

# -----------------------------------------------------------------------------
# 10. SAVE
# -----------------------------------------------------------------------------
if (!is.null(output_file)) {
  ggsave(output_file, plot = p, width = 9, height = 6)
  message("Saved plot to ", output_file)
}

# Save summary table
summary_df <- tibble(
  yvar = yvar,
  log = use_log,
  bandwidth_ft = bw,
  kernel = kernel,
  conley_cutoff_km = conley_cutoff,
  resid_type = resid_type,
  n_obs = n_obs,
  # Conventional
  coef_conventional = coef_conv,
  se_conventional = se_conv,
  pval_conventional = pval_conv,
  # Bias-Corrected
  coef_bias_corrected = coef_robust,
  se_bias_corrected = se_robust,
  pval_bias_corrected = pval_robust,
  # Conley (same point estimate as conventional)
  se_conley = se_conley,
  pval_conley = pval_conley
)

# if (!is.null(output_file)) {
#   csv_file <- gsub("\\.pdf$", ".csv", output_file)
#   write_csv(summary_df, csv_file)
#   message("Saved summary to ", csv_file)
# }
