# spatial_rd_rental_disaggregate.R
# Runs disaggregated spatial RD analyses for each unique alderman-pair EPISODE on rental data.
# Produces one RD plot per episode with alderman and ward names in the title.
# An "episode" is a contiguous period where the same two aldermen serve the bordering wards.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/spatial_rd_rental_disaggregate/code")

source("../../setup_environment/code/packages.R")

# -----------------------------------------------------------------------------
# 1. ARGUMENTS
# -----------------------------------------------------------------------------

# =======================================================================================
# --- Interactive Test Block (comment out if using Make) ---
# cat("--- RUNNING IN INTERACTIVE TEST MODE ---\n")
# yvar    <- "rent_price"
# use_log <- TRUE
# bw      <- 250
# kernel  <- "triangular"
# =======================================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4) {
    stop("Usage: Rscript spatial_rd_rental_disaggregate.R <yvar> <use_log> <bw> <kernel>")
}

yvar <- args[1]
use_log <- as.logical(args[2])
bw <- as.numeric(args[3])
kernel <- args[4]

# -----------------------------------------------------------------------------
# 2. LOAD DATA
# -----------------------------------------------------------------------------
message("Loading rental data...")
df <- read_parquet("../input/rent_with_ward_distances.parquet")

# -----------------------------------------------------------------------------
# 3. PREPARE DATA - Basic Filtering
# -----------------------------------------------------------------------------
message("Preparing data...")
df <- df %>%
    filter(!is.na(rent_price), rent_price > 0) %>%
    filter(!is.na(signed_dist), !is.na(ward_pair_id))

# Construct Outcome Variable
if (use_log) {
    df$outcome <- log(df[[yvar]])
    y_lab <- paste0("Log(", yvar, ")")
    log_suffix <- "_log"
} else {
    df$outcome <- df[[yvar]]
    y_lab <- yvar
    log_suffix <- ""
}

# Remove NAs in outcome
df <- df %>% filter(!is.na(outcome))

message(sprintf("Data prepared: %d observations", nrow(df)))

# -----------------------------------------------------------------------------
# 4. EPISODE CREATION (Following land_values_rd_data.R pattern)
# -----------------------------------------------------------------------------
message("Creating alderman-pair episodes...")

# Convert to data.table for efficient processing
DT <- data.table::as.data.table(df)

# 4a. Establish consistent side orientation (lo/hi ward)
DT[, c("ward_lo", "ward_hi") := tstrsplit(ward_pair_id, "-", fixed = TRUE)]
DT[, `:=`(ward_lo = as.integer(ward_lo), ward_hi = as.integer(ward_hi))]
DT[, side_hi := as.integer(ward == ward_hi)]

# 4b. Create month-year for grouping
DT[, month_year := zoo::as.yearmon(file_date)]

# 4c. Determine modal alderman per side per ward-pair × month
# (handles cases where multiple aldermen appear in same month due to transitions)
counts <- DT[, .N, by = .(ward_pair_id, month_year, side_hi, alderman_own)]
data.table::setorder(counts, ward_pair_id, month_year, side_hi, -N, alderman_own)
counts <- counts[, .SD[1], by = .(ward_pair_id, month_year, side_hi)] # modal alderman

# 4d. Pivot to get ald_lo and ald_hi per ward-pair × month
aldpairs <- tidyr::pivot_wider(
    as.data.frame(counts),
    id_cols = c("ward_pair_id", "month_year"),
    names_from = "side_hi",
    values_from = "alderman_own",
    names_prefix = "ald_side_"
) %>%
    dplyr::rename(ald_lo = ald_side_0, ald_hi = ald_side_1) %>%
    data.table::as.data.table()

# Flag months where both sides are present
aldpairs[, both_sides := !is.na(ald_lo) & !is.na(ald_hi)]

# 4e. Create episodes using rleid - detect when alderman pair changes
# Only consider months where both sides are present
months_tbl <- aldpairs[both_sides == TRUE, .(ward_pair_id, month_year, ald_lo, ald_hi)]
data.table::setorder(months_tbl, ward_pair_id, month_year)

# Episode breaks when either alderman changes
months_tbl[, episode_seq := data.table::rleid(ald_lo, ald_hi), by = ward_pair_id]
months_tbl[, episode_id := paste0(ward_pair_id, "_", sprintf("%03d", episode_seq))]

# 4f. Create episode metadata
episode_meta <- months_tbl[, .(
    month_start = min(month_year),
    month_end   = max(month_year),
    ald_lo      = data.table::first(ald_lo),
    ald_hi      = data.table::first(ald_hi),
    ward_pair   = data.table::first(ward_pair_id)
), by = episode_id]

message(sprintf("Created %d unique episodes", nrow(episode_meta)))

# 4g. Merge episode_id back to observations
DT <- months_tbl[, .(ward_pair_id, month_year, episode_id)][DT, on = .(ward_pair_id, month_year)]

# Drop obs without an episode (months with only one side present)
DT <- DT[!is.na(episode_id)]

message(sprintf("Observations with valid episodes: %d", nrow(DT)))

# -----------------------------------------------------------------------------
# 5. DYNAMIC Y-AXIS LIMITS
# -----------------------------------------------------------------------------
y_quantiles <- quantile(DT$outcome, c(0.01, 0.99), na.rm = TRUE)

# -----------------------------------------------------------------------------
# 6. LOOP OVER EPISODES
# -----------------------------------------------------------------------------
unique_episodes <- unique(na.omit(DT$episode_id))
message(sprintf("Found %d unique episodes. Starting analysis loop...", length(unique_episodes)))

# Plotting constants
col_points <- "#4C72B0" # Muted Deep Blue
col_lines <- "#C44E52" # Muted Deep Red/Rose
col_ci <- "grey85" # Light grey for confidence intervals
K <- 30 # bins per side

for (ep in unique_episodes) {
    message(sprintf("--- Processing episode: %s ---", ep))

    # Get episode metadata
    ep_meta <- episode_meta[episode_id == ep]
    ald_lo_name <- ep_meta$ald_lo[1]
    ald_hi_name <- ep_meta$ald_hi[1]
    ward_pair <- ep_meta$ward_pair[1]

    # Parse ward numbers
    wards <- as.integer(strsplit(ward_pair, "-")[[1]])
    ward_lo <- wards[1]
    ward_hi <- wards[2]

    # Extract episode data
    ep_data <- DT[episode_id == ep]

    # Within-window data
    df_bw <- ep_data[abs(signed_dist) <= bw]

    # Skip if insufficient observations on either side
    n_left <- sum(df_bw$signed_dist < 0, na.rm = TRUE)
    n_right <- sum(df_bw$signed_dist >= 0, na.rm = TRUE)
    if (n_left < 10 || n_right < 10) {
        message("Skipping episode (too few obs within bandwidth on one/both sides).")
        next
    }

    # ---------------------------------------------------------------------------
    # Format title with ward numbers and alderman names
    # ---------------------------------------------------------------------------
    # Determine which side is strict (positive signed_dist = strict side)
    # On positive side, the parcel's ward is the "strict" ward
    sample_pos <- df_bw[signed_dist >= 0][1]
    if (sample_pos$side_hi == 1) {
        # Hi-ward is strict (positive distance)
        ald_strict <- ald_hi_name
        ald_lenient <- ald_lo_name
        ward_strict <- ward_hi
        ward_lenient <- ward_lo
    } else {
        # Lo-ward is strict
        ald_strict <- ald_lo_name
        ald_lenient <- ald_hi_name
        ward_strict <- ward_lo
        ward_lenient <- ward_hi
    }

    plot_title <- sprintf("Wards %d & %d", ward_lenient, ward_strict)
    plot_subtitle_alderman <- sprintf(
        "%s vs %s",
        ifelse(is.na(ald_lenient), "Unknown", ald_lenient),
        ifelse(is.na(ald_strict), "Unknown", ald_strict)
    )

    # ---------------------------------------------------------------------------
    # Run rdrobust for annotation
    # ---------------------------------------------------------------------------
    rd_result <- tryCatch(
        {
            rdrobust(
                y = ep_data$outcome,
                x = ep_data$signed_dist,
                c = 0,
                h = bw,
                kernel = kernel,
                p = 1
            )
        },
        error = function(e) {
            message(sprintf("Error running rdrobust for episode %s: %s", ep, e$message))
            return(NULL)
        }
    )

    if (is.null(rd_result)) next

    # Extract results - Conventional
    coef_conv <- rd_result$coef["Conventional", 1]
    se_conv <- rd_result$se["Conventional", 1]
    pval_conv <- rd_result$pv["Conventional", 1]

    # Extract results - Robust
    coef_robust <- rd_result$coef["Robust", 1]
    se_robust <- rd_result$se["Robust", 1]
    pval_robust <- rd_result$pv["Robust", 1]

    n_obs <- sum(rd_result$N)

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
    annot_text <- sprintf(
        "Conventional: %.3f%s (%.3f)\nRobust: %.3f%s (%.3f)",
        coef_conv, stars_conv, se_conv,
        coef_robust, stars_robust, se_robust
    )

    # ---------------------------------------------------------------------------
    # Generate binned RD plot
    # ---------------------------------------------------------------------------
    rd_plot <- tryCatch(
        {
            rdplot(
                y = df_bw$outcome,
                x = df_bw$signed_dist,
                c = 0,
                h = bw,
                p = 1,
                kernel = kernel,
                binselect = "es",
                nbins = c(K, K),
                title = "", x.label = "", y.label = ""
            )
        },
        error = function(e) {
            message(sprintf("Error generating rdplot for episode %s: %s", ep, e$message))
            return(NULL)
        }
    )

    if (is.null(rd_plot)) next

    bin_data <- rd_plot$vars_bins

    # Build the plot
    p <- ggplot() +
        # Binned Means
        geom_point(
            data = bin_data, aes(x = rdplot_mean_x, y = rdplot_mean_y),
            color = col_points, alpha = 0.9, size = 2
        ) +
        # Linear Fit (Left)
        geom_smooth(
            data = as.data.frame(df_bw[signed_dist < 0]),
            aes(x = signed_dist, y = outcome),
            method = "lm", color = col_lines, fill = col_ci, alpha = 0.5, linewidth = 1.2
        ) +
        # Linear Fit (Right)
        geom_smooth(
            data = as.data.frame(df_bw[signed_dist >= 0]),
            aes(x = signed_dist, y = outcome),
            method = "lm", color = col_lines, fill = col_ci, alpha = 0.5, linewidth = 1.2
        ) +
        # Cutoff Line
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.6) +
        # Labels
        labs(
            title = plot_title,
            subtitle = paste0(plot_subtitle_alderman, " | BW = ", bw, " ft | N = ", format(n_obs, big.mark = ",")),
            x = "Distance to Stricter Ward Boundary (ft)",
            y = y_lab
        ) +
        # Y-axis limits based on data quantiles
        scale_y_continuous(limits = y_quantiles) +
        # Annotation
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

    # ---------------------------------------------------------------------------
    # Save plot as PDF (using episode_id in filename)
    # ---------------------------------------------------------------------------
    out_file <- file.path(
        "../output",
        sprintf("rd_plot_%s%s_%s_bw%d_%s.pdf", ep, log_suffix, yvar, bw, kernel)
    )
    ggsave(out_file, plot = p, width = 8, height = 6)
    message(sprintf("✓ Plot saved to: %s", out_file))
}

message("--- Disaggregated rental analysis by episode complete. ---")
