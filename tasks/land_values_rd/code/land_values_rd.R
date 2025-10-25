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
yvar   <- "land_share_pin10"  # change if you like
use_log <- FALSE               # TRUE = log-transform (drops y<=0 first)
kernel <- "triangular"
bw     <- 528                   # feet (~0.1 mi)


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
    df_bw <- dplyr::filter(df, abs(signed_distance) <= bw)
    
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
        # --- FIX 3 (Instability): Removed pipe ---
        data = dplyr::filter(df_bw, signed_distance < 0),
        aes(x = signed_distance, y = outcome),
        method = "lm", formula = y ~ x, se = TRUE, level = 0.95,
        color = "#d14949", fill = "grey", alpha = 0.5, linewidth = 1
      ) +
      geom_smooth(
        # --- FIX 3 (Instability): Removed pipe ---
        data = dplyr::filter(df_bw, signed_distance >= 0),
        aes(x = signed_distance, y = outcome),
        method = "lm", formula = y ~ x, se = TRUE,
        span = 0.75, level = 0.95,
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


# Single-episode runner (MODIFIED)
run_one_rd <- function(df, ep_id) {
  
  # --- FIX 3 (Instability): Removed pipe ---
  df <- dplyr::filter(df, .data[[yvar]] > 0)
  
  if (use_log) {
    # --- FIX 3 (Instability): Removed pipe ---
    df <- dplyr::mutate(df, outcome = log(as.numeric(.data[[yvar]])))
  } else {
    # --- FIX 3 (Instability): Removed pipe ---
    df <- dplyr::mutate(df, outcome = as.numeric(.data[[yvar]]))
  }
  
  n_tot <- nrow(df)
  if (n_tot == 0L) {
    return(list(
      # ... (rest of list is unchanged) ...
      coef_bc = NA_real_, se_bc = NA_real_, p_bc = NA_real_, z_bc = NA_real_,
      stars = NA_character_,
      n_obs = 0L, n_left = 0L, n_right = 0L, y_mean = NA_real_,
      bw_used = bw, clustered = NA, status = "no_data", note = "no finite rows"
    ))
  }
  
  n_left  <- sum(df$signed_distance < 0)
  n_right <- sum(df$signed_distance >= 0)
  
  if (!(n_left > 20L && n_right > 20L)) {
    return(list(
      # ... (rest of list is unchanged) ...
      coef_bc = NA_real_, se_bc = NA_real_, p_bc = NA_real_, z_bc = NA_real_,
      stars = NA_character_,
      n_obs = nrow(df), n_left = n_left, n_right = n_right,
      y_mean = mean(df$outcome, na.rm = TRUE), # Added na.rm
      bw_used = bw, clustered = NA,
      status = "too_few_sideN",
      note   = sprintf("Skipped: n_left=%d, n_right=%d (need >20 each).", n_left, n_right)
    ))
  }
  
  # --- FIX 2 (New Feature): Get alderman names ---
  # Since this df is for one episode, all names are the same
  ald_name_lo <- df$ald_lo[1]
  ald_name_hi <- df$ald_hi[1]
  
  
  # --- (RDrobust call: This section is unchanged) ---
  fit <- rdrobust(
    y = df$outcome, # <-- Use the 'outcome' variable you created
    x = df$signed_distance,
    c = 0, kernel = kernel, p = 1, h = bw,
    cluster = df$tax_year
  )
  
  # --- (Estimate extraction: naive estimates) ---
  coef_na <- suppressWarnings(fit$coef[1]) # Renamed to coef_na
  se_na   <- suppressWarnings(fit$se[1])   # Renamed to se_na
  p_na    <- suppressWarnings(fit$pv[1])   # Renamed to p_na
  z_na    <- if (is.finite(coef_na) && is.finite(se_na) && se_na > 0) coef_na / se_na else NA_real_
  stars_na <- stars_from_p(as.numeric(p_na))
  
  annotation_text_naive <- sprintf("Conventional: %.3f%s (%.3f)", coef_na, stars_na, se_na)
  
  
  # --- (Estimate extraction: Robust SEs and BC estiamtes) ---
  coef_bc <- suppressWarnings(fit$coef[3])
  se_bc   <- suppressWarnings(fit$se[3])
  p_bc    <- suppressWarnings(fit$pv[3])
  z_bc    <- if (is.finite(coef_bc) && is.finite(se_bc) && se_bc > 0) coef_bc / se_bc else NA_real_
  stars_bc <- stars_from_p(as.numeric(p_bc)) # Renamed to stars_bc
  
  annotation_text_robust <- sprintf("Bias-Corrected: %.3f%s (%.3f)", coef_bc, stars_bc, se_bc)
  
  ## total annotation
  total_annotation_text <- paste(annotation_text_robust, annotation_text_naive, sep = "\n")
  
  
  # After we successfully run the RD and get the text, call the plot function.
  make_rd_plot(
    df = df, # The episode's prepared data
    bw = bw, 
    kernel = kernel,
    yvar = yvar, 
    use_log = use_log,
    ep_id = ep_id,
    # --- FIX 1 (Bug): Pass the correct combined text ---
    annotation_text = total_annotation_text,
    # --- FIX 2 (New Feature): Pass alderman names ---
    ald_name_lo = ald_name_lo,
    ald_name_hi = ald_name_hi
  )
  # --- End new step ---
  
  
  # --- (Return results: This section is unchanged) ---
  list(
    coef_bc = as.numeric(coef_bc),
    se_bc   = as.numeric(se_bc),
    p_bc    = as.numeric(p_bc),
    z_bc    = as.numeric(z_bc),
    stars   = stars_bc, # Return robust stars
    n_obs   = n_tot,
    n_left  = n_left,
    n_right = n_right,
    y_mean  = mean(df$outcome, na.rm = TRUE), # Added na.rm
    bw_used = bw, 
    clustered = "tax_year"
    # Note: The returned list only has the BC estimates. 
    # You can add coef_na, se_na etc. here if you want them in your final CSV.
  )
}
# ---------------------------
# 5) Loop over episodes
# ---------------------------

eps <- unique(land_values$episode_id)
eps <- eps[!is.na(eps)]

results_list <- vector("list", length(eps))

pb <- txtProgressBar(min = 0, max = length(eps), style = 3)
for (i in seq_along(eps)) {
  ep <- eps[i]
  
  # Filter data to just this episode
  # --- FIX 3 (Instability): Removed pipe ---
  df_ep  <- dplyr::filter(land_values, episode_id == ep)
  
  # Run RD (and plot)
  res <- run_one_rd(df_ep, ep_id = ep)
  
  # --- (Result collection: This section is unchanged) ---
  results_list[[i]] <- data.table::data.table(
    episode_id    = ep,
    outcome       = yvar, # Use the global yvar
    use_log       = use_log, # Use the global use_log
    kernel        = kernel,
    bw_used       = res$bw_used,
    n_obs         = res$n_obs,
    n_left        = res$n_left,
    n_right       = res$n_right,
    y_mean        = res$y_mean,
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
rd_results <- left_join(rd_results, episode_meta, by = "episode_id")

# Add flags
rd_results <- as.data.table(rd_results) # Convert back if needed
rd_results[, `:=`(
  ok_two_sides = (n_left > 0 & n_right > 0),
  ok_min_sideN = (n_left >= 20 & n_right >= 20), # Matched 20 from your function
  episode_len  = (year_end - year_start + 1L)
)]

# -------------------------------------------------------------------
# 6) Save catalog (ALL episodes kept; NA betas show why)
# -------------------------------------------------------------------
write_csv(rd_results, "../output/rd_alderman_pair_episodes.csv")