#this code runs regression discontinuities on each alderman pair for the land share variable

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---- Load Data ----
land_values <- read_csv("../input/land_values_rd_data.csv")
episode_meta <- read_csv("../input/episode_metadata.csv")

# -------------------------
# 4) Analysis: preferred rdrobust style
# ---------------------------

# Controls for this run
kernel <- "triangular"
bw     <- 1056


# Helper for stars
stars_from_p <- function(p) {
  if (!is.finite(p)) return(NA_character_)
  if (p <= .01)   return("***")
  if (p <= .05)   return("**")
  if (p <= .10)   return("*")
  ""
}

# ---------------------------
# 4.5) NEW: Plotting Function (MODIFIED)
# ---------------------------
# Added ald_name_lo and ald_name_hi as arguments
make_rd_plot <- function(df, bw, kernel, yvar, use_log, ep_id, 
                         annotation_text, ald_name_lo, ald_name_hi) {
  
  tryCatch({
    
    # --- FIX 3 (Instability): Removed pipe ---
    df_bw <- df[is.finite(df$signed_distance) & abs(df$signed_distance) <= bw, , drop = FALSE]
    
    if (nrow(df_bw) == 0) {
      warning(sprintf("Plotting skipped for %s: No data within bandwidth.", ep_id))
      return()
    }
    
    K <- 15 # Bins from your example
    
    # Run rdplot to get the binned data
    rd_plot_object <- rdplot(
      y = df_bw$outcome,
      x = df_bw$signed_distance,
      c = 0, h = bw, p = 1,
      kernel = kernel,
      nbins = c(K, K),
      binselect = "es",
      x.lim = c(-bw, bw),
      title = "", y.label = "", x.label = ""
    )
    
    bins <- rd_plot_object$vars_bins
    
    # Minimal labels
    y_axis_label <- yvar
    if (use_log) {
      y_axis_label <- paste("Log(", y_axis_label, ")")
    }
    
    # Create plot based on your example style
    plot_out <- ggplot() +
      # Binned means
      geom_point(data = bins,
                 aes(x = rdplot_mean_x, y = rdplot_mean_y),
                 color = "darkblue", size = 1.5, alpha = 0.7) +
      # Linear fits (matches p=1)
      geom_smooth(
        data = df_bw[df_bw$signed_distance < 0, , drop = FALSE],
        aes(x = signed_distance, y = outcome),
        method = "lm", formula = y ~ x, se = TRUE, level = 0.95,
        color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1
      ) +
      geom_smooth(
        data = df_bw[df_bw$signed_distance >= 0, , drop = FALSE],
        aes(x = signed_distance, y = outcome),
        method = "lm", formula = y ~ x, se = TRUE, level = 0.95,
        color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1
      ) +
      geom_vline(xintercept = 0, color = "black", linewidth = .5) +
      
      # Robust annotation: place at bottom-left, just below plot
      annotate("text", x = -Inf, y = -Inf, label = annotation_text,
               hjust = -0.05, vjust = -0.5, size = 3, fontface = "bold") +
      
      labs(
        # --- FIX 2 (New Feature): Added alderman names to title ---
        title = paste(ep_id, ":", ald_name_lo, "vs.", ald_name_hi),
        subtitle = paste0("bw: ", bw, " ft | kernel: ", kernel, " | y: ", y_axis_label),
        y = y_axis_label,
        x = "Distance to Boundary (feet)"
      ) +
      coord_cartesian(xlim = c(-bw, bw)) + # Set x-limits, let y auto-scale
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    # Save the plot
    log_suffix <- if (use_log) "_log" else ""
    out_file <- file.path("../output", 
                          sprintf("rdplot_ep_%s_%s%s.pdf", ep_id, yvar, log_suffix))
    
    ggsave(out_file, plot = plot_out, width = 8, height = 6, dpi = 300)
    
  }, error = function(e) {
    # If plotting fails, just print a warning and continue
    warning(sprintf("Plotting failed for episode %s: %s", ep_id, e$message))
  })
}


# Single-episode runner 
ep_id = "3-12_003"
df = land_values %>%
filter(episode_id == ep_id)

run_one_rd <- function(df, ep_id) {
  df <- df[is.finite(df[[yvar]]) & df[[yvar]] > 0, , drop = FALSE]
  
  if (use_log) df$outcome <- log(as.numeric(df[[yvar]])) else df$outcome <- as.numeric(df[[yvar]])
  
  n_tot <- nrow(df)
  if (n_tot == 0L) {
    return(list(
      # return NA placeholders for BOTH naive and BC so downstream code never breaks
      coef_na = NA_real_, se_na = NA_real_, p_na = NA_real_, z_na = NA_real_, stars_na = NA_character_,
      coef_bc = NA_real_, se_bc = NA_real_, p_bc = NA_real_, z_bc = NA_real_, stars = NA_character_,
      n_obs = 0L, n_left = 0L, n_right = 0L, y_mean = NA_real_,
      bw_used = bw, clustered = NA, status = "no_data", note = "no finite rows"
    ))
  }
  
  n_left  <- sum(df$signed_distance < 0)
  n_right <- sum(df$signed_distance >= 0)
  
  if (!(n_left > 20L && n_right > 20L)) {
    return(list(
      coef_na = NA_real_, se_na = NA_real_, p_na = NA_real_, z_na = NA_real_, stars_na = NA_character_,
      coef_bc = NA_real_, se_bc = NA_real_, p_bc = NA_real_, z_bc = NA_real_, stars = NA_character_,
      n_obs = n_tot, n_left = n_left, n_right = n_right,
      y_mean = mean(df$outcome, na.rm = TRUE),
      bw_used = bw, clustered = NA,
      status = "too_few_sideN",
      note   = sprintf("Skipped: n_left=%d, n_right=%d (need >20 each).", n_left, n_right)
    ))
  }
  
  # labels for plot
  ald_name_lo <- df$ald_lo[1]
  ald_name_hi <- df$ald_hi[1]
  
  fit <- tryCatch(
    rdrobust(y = df$outcome, x = df$signed_distance,
             c = 0, kernel = kernel, p = 1, h = bw,
             vce = "HC1", masspoints = "adjust"),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(status="skip", note=conditionMessage(fit), ep_id=ep_id))
  }
  
  # --- Conventional (naive) ---
  coef_na <- fit$coef[1]
  se_na   <- fit$se[1]
  p_na    <- fit$pv[1]
  z_na    <- if (is.finite(coef_na) && is.finite(se_na) && se_na > 0) coef_na / se_na else NA_real_
  stars_na <- stars_from_p(as.numeric(p_na))
  annotation_text_naive <- sprintf("Conventional: %.3f%s (%.3f)", coef_na, stars_na, se_na)
  
  # --- Bias-corrected with robust SEs (rdrobust index 3) ---
  coef_bc <- fit$coef[3]
  se_bc   <- fit$se[3]
  p_bc    <- fit$pv[3]
  z_bc    <- if (is.finite(coef_bc) && is.finite(se_bc) && se_bc > 0) coef_bc / se_bc else NA_real_
  stars_bc <- stars_from_p(as.numeric(p_bc))
  annotation_text_robust <- sprintf("Bias-Corrected: %.3f%s (%.3f)", coef_bc, stars_bc, se_bc)
  
  total_annotation_text <- paste(annotation_text_robust, annotation_text_naive, sep = "\n")
  
  # Plot
  make_rd_plot(
    df = df, bw = bw, kernel = kernel, yvar = yvar, use_log = use_log, ep_id = ep_id,
    annotation_text = total_annotation_text,
    ald_name_lo = ald_name_lo, ald_name_hi = ald_name_hi
  )
  
  # Return BOTH sets
  list(
    coef_na = as.numeric(coef_na),
    se_na   = as.numeric(se_na),
    p_na    = as.numeric(p_na),
    z_na    = as.numeric(z_na),
    stars_na = stars_na,
    
    coef_bc = as.numeric(coef_bc),
    se_bc   = as.numeric(se_bc),
    p_bc    = as.numeric(p_bc),
    z_bc    = as.numeric(z_bc),
    stars   = stars_bc,
    
    n_obs   = n_tot,
    n_left  = n_left,
    n_right = n_right,
    y_mean  = mean(df$outcome, na.rm = TRUE),
    bw_used = bw,
    clustered = "robust"
  )
}


# ---------------------------
# 5) Loop over all configurations and episodes
# ---------------------------

# Define the grid of analyses to run
analysis_grid <- expand.grid(
  yvar_run = c("land_share_pin10"),
  # yvar_run = c("land_sum"),
  use_log_run = c(FALSE),
  stringsAsFactors = FALSE
)

# Master list to store the results data.table from all runs
all_results_list <- vector("list", nrow(analysis_grid))

# Loop over the analysis grid (e.g., 4 runs)
for (j in 1:nrow(analysis_grid)) {
  
  # ---- Set Controls for this run ----
  yvar    <- analysis_grid$yvar_run[j]
  use_log <- analysis_grid$use_log_run[j]
  
  message(sprintf("\n--- Running analysis %d/%d: yvar=%s, use_log=%s ---", 
                  j, nrow(analysis_grid), yvar, use_log))
  
  # ---- Loop over episodes (This is your original Section 5 loop) ----
  
  eps <- unique(land_values$episode_id)
  eps <- eps[!is.na(eps)]
  
  results_list <- vector("list", length(eps))
  
  pb <- txtProgressBar(min = 0, max = length(eps), style = 3)
  for (i in seq_along(eps)) {
    ep <- eps[i]
    
    # Filter data to just this episode
    df_ep <- land_values[land_values$episode_id == ep, , drop = FALSE]
    
    # Run RD (and plot)
    res <- run_one_rd(df_ep, ep_id = ep)
    
    # --- (Result collection: This section is unchanged) ---
    results_list[[i]] <- data.table::data.table(
      episode_id    = ep,
      outcome       = yvar,
      use_log       = use_log,
      kernel        = kernel,
      bw_used       = res$bw_used,
      n_obs         = res$n_obs,
      n_left        = res$n_left,
      n_right       = res$n_right,
      y_mean        = res$y_mean,
      
      # NEW: conventional (naive) estimates
      coef_na       = res$coef_na,
      se_na         = res$se_na,
      p_na          = res$p_na,
      z_na          = res$z_na,
      stars_na      = res$stars_na,
      
      # Existing: bias-corrected with robust SEs
      coef_bc       = res$coef_bc,
      se_bc         = res$se_bc,
      p_bc          = res$p_bc,
      z_bc          = res$z_bc,
      stars         = res$stars
    )
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  rd_results <- data.table::rbindlist(results_list, use.names = TRUE, fill = TRUE)
  
  # Join metadata (switched to dplyr::left_join for clarity, data.table join is also fine)
  episode_meta <- as.data.frame(episode_meta)  # ensure plain df
  rd_results <- merge(
    rd_results, episode_meta,
    by = "episode_id", all.x = TRUE, sort = FALSE
  )
  
  # Add flags
  rd_results <- as.data.table(rd_results) # Convert back if needed
  rd_results[, `:=`(
    ok_two_sides = (n_left > 0 & n_right > 0),
    ok_min_sideN = (n_left >= 20 & n_right >= 20), # Matched 20 from your function
    episode_len  = (year_end - year_start + 1L)
  )]
  
  # Store this run's results in the master list
  all_results_list[[j]] <- rd_results
  
} # --- End of loop over configurations ---

# -------------------------------------------------------------------
# 6) Combine all runs and save catalog
# -------------------------------------------------------------------

# Combine the results from all 4 analysis runs
final_rd_results <- data.table::rbindlist(all_results_list, use.names = TRUE, fill = TRUE) %>%
  relocate(ald_lo, ald_hi,
           coef_na, se_na, p_na, z_na, stars_na,
           coef_bc, se_bc, p_bc, z_bc, stars,
           ward_pair)

message(sprintf("\nAll analyses complete. Saving %d total results to CSV.", nrow(final_rd_results)))

write_csv(final_rd_results, "../output/rd_alderman_pair_episodes.csv")
