#this code runs event studies on each alderman pair 

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---- Load + narrow to RD window (0.1 mi) ----
land_values <- read_csv("../input/land_values_rd_data.csv")

episode_meta <- read_csv("../input/episode_metadata.csv")

# -------------------------
  # 4) Analysis: preferred rdrobust style
# ---------------------------

# Controls for this run
yvar   <- "land_share_pin10"   # change if you like
use_log <- FALSE               # TRUE = log-transform (drops y<=0 first)
kernel <- "triangular"
bw     <- 528                  # feet (~0.1 mi)



# Helper for stars (matches your case_when logic)
stars_from_p <- function(p) {
  if (!is.finite(p)) return(NA_character_)
  if (p <= .01)   return("***")
  if (p <= .05)   return("**")
  if (p <= .10)   return("*")
  ""
}

# Single-episode runner in your preferred style
run_one_rd <- function(df) {
  
  # df <- land_values
  # df <- df %>% filter(episode_id == "24-25_002")
  
  df <- df %>% 
    filter(.data[[yvar]] > 0)
  # Create the outcome variable
  if (use_log) {
    df <- df %>% 
      mutate(outcome = log(as.numeric(.data[[yvar]])))
  } else {
    df <- df %>% 
      mutate(outcome = as.numeric(.data[[yvar]]))
  }
  
  # keep finite, both vars present
  n_tot <- nrow(df)
  if (n_tot == 0L) {
    return(list(
      coef_bc = NA_real_, se_bc = NA_real_, p_bc = NA_real_, z_bc = NA_real_,
      stars = NA_character_,
      n_obs = 0L, n_left = 0L, n_right = 0L, y_mean = NA_real_,
      bw_used = bw, clustered = NA, status = "no_data", note = "no finite rows"
    ))
  }
  
  n_left  <- sum(df$signed_distance < 0)
  n_right <- sum(df$signed_distance >= 0)
  
  # Cluster if and only if we have at least 2 clusters; otherwise use robust EHW
  # cl_vec <- df$ward_pair
  # use_cluster <- data.table::uniqueN(cl_vec) >= 2L
  
  if (!(n_left > 20L && n_right > 20L)) {
    return(list(
      coef_bc = NA_real_, se_bc = NA_real_, p_bc = NA_real_, z_bc = NA_real_,
      stars = NA_character_,
      n_obs = nrow(df), n_left = n_left, n_right = n_right,
      y_mean = mean(df$outcome),
      bw_used = bw, clustered = NA,
      status = "too_few_sideN",
      note   = sprintf("Skipped: n_left=%d, n_right=%d (need >20 each).", n_left, n_right)
    ))
  }
  
  fit <- rdrobust(
    y = df$land_share_pin10, x = df$signed_distance,
    c = 0, kernel = kernel, p = 1, h = bw,
    cluster = df$tax_year
  )
  
  # Extract BC estimate/SE/p-value à la your snippet
  coef_bc <- suppressWarnings(fit$coef[3])
  se_bc   <- suppressWarnings(fit$se[3])
  p_bc    <- suppressWarnings(fit$pv[3])
  z_bc    <- if (is.finite(coef_bc) && is.finite(se_bc) && se_bc > 0) coef_bc / se_bc else NA_real_
  stars   = stars_from_p(as.numeric(p_bc))
  
  annotation_text <- sprintf("Estimate: %.3f%s (%.3f)", coef_bc, stars, se_bc)
  annotation_text
  
  list(
    coef_bc = as.numeric(coef_bc),
    se_bc   = as.numeric(se_bc),
    p_bc    = as.numeric(p_bc),
    z_bc    = as.numeric(z_bc),
    stars   = stars,
    n_obs   = n_tot,
    n_left  = n_left,
    n_right = n_right,
    y_mean  = mean(df$outcome),
    bw_used = bw, 
    clustered = "tax_year"
    # status  = if (use_cluster) "ok_clustered" else "ok_robust",
    # note    = if (!use_cluster) "Only 1 cluster; used robust EHW SEs." else NA_character_
  )
}

# ---------------------------
# 5) Loop over episodes
# ---------------------------

# Get unique, non-NA episode IDs to loop over
eps <- unique(land_values$episode_id)
eps <- eps[!is.na(eps)]

results_list <- vector("list", length(eps))

pb <- txtProgressBar(min = 0, max = length(eps), style = 3)
for (i in seq_along(eps)) {
  ep <- eps[i]
  
  # Filter data to just this episode
  df_ep  <- land_values %>% 
    filter(episode_id == ep)
  
  # Run RD
  res <- run_one_rd(df_ep)
  
  # Save ONLY the episode_id and the RD results.
  # All metadata (ald_lo, year_start, etc.) will be
  # attached in the final join after the loop.
  results_list[[i]] <- data.table::data.table(
    episode_id    = ep,
    outcome       = "land_share_pin10",
    use_log       = F,
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
# Keep your meta join + flags as before
rd_results <- left_join(rd_results, episode_meta, by = "episode_id")

# (The rest of your script remains the same)
rd_results[, `:=`(
  ok_two_sides = (n_left > 0 & n_right > 0),
  ok_min_sideN = (n_left >= 25 & n_right >= 25),
  episode_len  = year_end - year_start + 1L
)]

# -------------------------------------------------------------------
# 6) Save catalog (ALL episodes kept; NA betas show why)
# -------------------------------------------------------------------
write_csv(rd_results, "../output/rd_alderman_pair_episodes.csv")
