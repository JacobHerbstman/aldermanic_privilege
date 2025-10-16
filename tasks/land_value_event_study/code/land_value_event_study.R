#this code assigns wards + distance to nearest ward border for all parcels in the assessor panel 

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---- Load ----
event_study_df   <- read_csv("../input/land_event_study_data.csv") 


## loop and regressions
all_ward_pairs <- unique(event_study_df$ward_pair)

# current_pair <- "13-23"

message(paste("Found", length(all_ward_pairs), "unique ward pairs. Starting analysis loop..."))

for (current_pair in all_ward_pairs) {
  message(paste("--> Processing Ward Pair:", current_pair))
  
  wards <- as.numeric(unlist(strsplit(current_pair, "-|_")))
  ward_a <- wards[1]
  ward_b <- wards[2]
  
  switch1 <- paste(ward_a, "->", ward_b)
  switch2 <- paste(ward_b, "->", ward_a)
  
  border_df <- event_study_df %>%
    filter(ward_pair == current_pair) %>%
    mutate(
      switch_direction = if_else(
        is_treated == 1,
        paste(ward_2003, "->", ward_2015),
        NA_character_
      )
    )
  
  plot_list <- list()
  results_exist <- FALSE
  
  for (current_switch in c(switch1, switch2)) {
    
    message(paste("   ...analyzing switch:", current_switch))
    
    # MODIFIED: Define the correct control group for each switch direction.
    # The control group should consist of non-switching parcels from the switch's "origin" ward.
    origin_ward <- as.numeric(str_extract(current_switch, "^\\d+"))
    
    switch_df <- border_df %>%
      filter(
        switch_direction == current_switch |
          (is_treated == 0 & ward_2003 == origin_ward)
      )
    
    # ADDED: Bin endpoints to improve model stability with sparse data
    switch_df <- switch_df %>%
      mutate(
        time_to_event = case_when(
          time_to_event <= -5 ~ -5, # Bin all pre-periods <= -5
          time_to_event >= 5  ~ 5,  # Bin all post-periods >= 5
          TRUE ~ time_to_event
        )
      )
    
    if (!any(switch_df$switch_direction == current_switch, na.rm = TRUE)) {
      message("      Skipping switch: No parcels made this switch.")
      next
    }
    
    # MODIFIED: More robust pre-flight checks
    if (length(unique(switch_df$is_treated)) < 2 || nrow(switch_df) < 100) {
      message("      Skipping switch: Not enough data or no treatment variation.")
      next
    }
    if(!(-1 %in% switch_df$time_to_event[switch_df$is_treated == 1])) {
      message("      Skipping switch: Baseline period (t=-1) is missing for the treated group.")
      next
    }
    
    # Canonical DiD model with interaction terms.
    res <- feols(
      land_share_pin10 ~ i(time_to_event) + i(time_to_event, is_treated, ref = -1) | block_id,
      data = switch_df,
      cluster = ~block_id
    )
    
    # --- CUSTOM ggplot BLOCK ---
    results_df <- as.data.frame(res$coeftable)
    results_df$term <- rownames(results_df)
    rownames(results_df) <- NULL
    
    # Extract coefficients from the interaction term.
    results_df <- results_df %>%
      rename(
        estimate = Estimate,
        std.error = `Std. Error`
      ) %>%
      filter(str_detect(term, ":is_treated")) %>%
      mutate(
        time_to_event = as.numeric(str_extract(term, "(?<=time_to_event::)-?\\d+")),
        conf.low = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error
      ) %>%
      filter(!is.na(time_to_event))
    
    if(!(-1 %in% results_df$time_to_event)) {
      results_df <- bind_rows(results_df, tibble(
        time_to_event = -1, estimate = 0, std.error = 0, conf.low = 0, conf.high = 0
      ))
    }
    
    p <- ggplot(results_df, aes(x = time_to_event, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray50") +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "gray40") +
      geom_point(color = "midnightblue", size = 2.5) +
      theme_minimal(base_size = 12) +
      labs(
        x = "Years Relative to Remap",
        y = "Coefficient (Land Value Share)",
        title = paste("Switch:", current_switch)
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks())
    
    plot_list[[current_switch]] <- p
    results_exist <- TRUE
  }
  
  if (results_exist) {
    combined_plot <- wrap_plots(plot_list, ncol = length(plot_list)) +
      plot_annotation(
        title = paste("Event Study for Ward Border:", current_pair),
        subtitle = "Effect of 2015 Ward Remapping on Land Value Share, by Switch Direction"
      )
    
    plot_path <- file.path("../output", paste0("event_study_ward_pair_", gsub("/", "-", current_pair), ".pdf"))
    ggsave(plot_path, combined_plot, width = 12, height = 6, bg = "white")
    message(paste("   Successfully saved combined plot to:", plot_path))
  } else {
    message("   No valid results to plot for this ward pair.")
  }
  
  gc()
  Sys.sleep(1)
}

message("Analysis complete. Plots are saved in ../output/")