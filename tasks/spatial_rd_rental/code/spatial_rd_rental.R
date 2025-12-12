# rental_spatial_rd.R
# Runs stacked spatial RD on rental data.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# -----------------------------------------------------------------------------
# 1. ARGUMENTS
# -----------------------------------------------------------------------------

# =======================================================================================
# --- Interactive Test Block (comment out if using Make) ---
cat("--- RUNNING IN INTERACTIVE TEST MODE ---\n")
yvar    <- "rent_price"
use_log <- T
bw      <- 250             # outer bandwidth in feet
kernel  <- "triangular"
# =======================================================================================

# args <- commandArgs(trailingOnly = TRUE)
# 
# if (length(args) < 4) {
#   stop("Usage: Rscript rental_spatial_rd.R <yvar> <use_log> <bw> <kernel> [output_file]")
# }
# 
# yvar       <- args[1]
# use_log    <- as.logical(args[2])
# bw         <- as.numeric(args[3])
# kernel     <- args[4]
# output_file <- if (length(args) >= 5) args[5] else NULL

# -----------------------------------------------------------------------------
# 2. LOAD DATA
# -----------------------------------------------------------------------------
message("Loading rental data...")
# We use arrow to read the parquet file efficiently
df <- read_parquet("../input/rent_with_ward_distances.parquet")

# -----------------------------------------------------------------------------
# 3. PREPARE DATA
# -----------------------------------------------------------------------------
# Filter to bandwidth window immediately to save memory
df_bw <- df %>% 
  filter(abs(signed_dist) <= bw) %>%
  filter(!is.na(rent_price), rent_price > 0) %>% 
  filter(building_type_clean == "multi_family")

# Construct Outcome Variable
if (use_log) {
  df_bw$outcome <- log(df_bw[[yvar]])
  y_lab <- paste0("Log(", yvar, ")")
} else {
  df_bw$outcome <- df_bw[[yvar]]
  y_lab <- yvar
}

# Remove NAs in outcome or running variable
df_bw <- df_bw %>% 
  filter(!is.na(outcome), !is.na(signed_dist))

# -----------------------------------------------------------------------------
# 4. RUN RDROBUST
# -----------------------------------------------------------------------------
message(sprintf("Running RD: %s ~ signed_dist | bw = %.0f | kernel = %s", 
                y_lab, bw, kernel))

# Cluster by ward_pair_id if available, otherwise just robust
cluster_var <- if ("ward_pair_id" %in% names(df_bw)) df_bw$ward_pair_id else NULL

est <- rdrobust(
  y = df_bw$outcome,
  x = df_bw$signed_dist,
  c = 0,
  h = bw,
  kernel = kernel,
  cluster = cluster_var,
  p = 1 # Linear local regression is standard
)

summary(est)

# Extract Results
coef <- est$coef["Robust", 1]
se   <- est$se["Robust", 1]
pval <- est$pv["Robust", 1]
n_obs <- sum(est$N)

stars <- case_when(
  pval < 0.01 ~ "***",
  pval < 0.05 ~ "**",
  pval < 0.1  ~ "*",
  TRUE        ~ ""
)

annot_text <- sprintf("Estimate: %.3f%s (SE: %.3f)\nN: %s", 
                      coef, stars, se, format(n_obs, big.mark=","))

# -----------------------------------------------------------------------------
# 5. GENERATE PLOT
# -----------------------------------------------------------------------------
# Use rdplot to get binned means (quantile-spaced bins are usually best for skewed data like rents)
# but 'es' (evenly spaced) matches your other plots. Let's stick to 'es'.
rd_plot <- rdplot(
  y = df_bw$outcome,
  x = df_bw$signed_dist,
  c = 0,
  h = bw,
  p = 1,
  kernel = kernel,
  binselect = "es", 
  nbins = c(30, 30), # Adjust bin count as needed
  title = "", x.label = "", y.label = ""
)

bin_data <- rd_plot$vars_bins

p <- ggplot() +
  # Binned Means
  geom_point(data = bin_data, aes(x = rdplot_mean_x, y = rdplot_mean_y),
             color = "darkblue", alpha = 0.7, size = 2) +
  # Linear Fit (Left)
  geom_smooth(data = df_bw %>% filter(signed_dist < 0),
              aes(x = signed_dist, y = outcome),
              method = "lm", color = "#d14949", fill = "grey80", alpha = 0.5) +
  # Linear Fit (Right)
  geom_smooth(data = df_bw %>% filter(signed_dist >= 0),
              aes(x = signed_dist, y = outcome),
              method = "lm", color = "#d14949", fill = "grey80", alpha = 0.5) +
  # Cutoff Line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  # Labels
  labs(
    title = paste0("Rental Price Discontinuity (BW = ", bw, " ft)"),
    subtitle = paste0("Outcome: ", y_lab, " | Kernel: ", kernel),
    x = "Distance to Stricter Ward Boundary (ft)",
    y = y_lab
  ) +
  # Annotation
  annotate("text", x = -Inf, y = Inf, label = annot_text, 
           hjust = -0.1, vjust = 1.5, fontface = "bold", size = 4) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p
# -----------------------------------------------------------------------------
# 6. SAVE
# -----------------------------------------------------------------------------
if (!exists("output_filename_rdplot")) {
  log_suffix <- if (use_log) "_log" else ""
  output_filename_rdplot <- sprintf("../output/rd_plot%s_%s_bw%d_%s.pdf", log_suffix, yvar, bw, kernel)
}
ggsave(output_filename_rdplot, plot = p, width = 8, height = 6, dpi = 300)

