#this code assigns wards + distance to nearest ward border for all parcels in the assessor panel 

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---- Load ----
land_values   <- sfarrow::st_read_parquet("../input/land_values_aug.parquet", show_col_types = FALSE) 
ward_panel <- st_read("../input/ward_panel.gpkg")


land_values_aug <- land_values %>%
  filter(tax_year >= 2003) %>% 
  mutate(
    ward_map_year = case_when(
      # tax_year < 2003 ~ 1998,
      tax_year >= 2003 & tax_year < 2015 ~ 2003,
      tax_year >= 2015 ~ 2015
    ))

EVENT_YEARS <- c(2015)
MAP_YEAR_LOOKUP <- list(
  # `2003` = list(before = 1998, after = 2003),
  `2015` = list(before = 2003, after = 2015)
)

pins_latest_geom <- land_values_aug %>%
  # Using distinct is faster than group_by/slice for this purpose
  distinct(pin10, .keep_all = TRUE) %>%
  select(pin10, geometry) %>%
  filter(!st_is_empty(geometry))

map_years_to_check <- unique(unlist(MAP_YEAR_LOOKUP))
ward_crosswalk <- pins_latest_geom

for(map_year in map_years_to_check) {
  message(paste("...spatially joining pins to", map_year, "ward map"))
  ward_polygons_year <- ward_panel %>% filter(year == map_year)
  
  # Perform the spatial join
  joined <- st_join(pins_latest_geom, ward_polygons_year, join = st_within) %>%
    st_drop_geometry() %>%
    select(pin10, ward)
  
  # Rename the column to be specific to the year
  names(joined)[names(joined) == "ward"] <- paste0("ward_", map_year)
  
  # Merge into the main crosswalk table, handling pins that might not join
  ward_crosswalk <- ward_crosswalk %>%
    left_join(distinct(joined, pin10, .keep_all = TRUE), by = "pin10")
}
ward_crosswalk <- ward_crosswalk %>% st_drop_geometry()


data_prepared_list <- lapply(EVENT_YEARS, function(event_y) {
  map_y <- MAP_YEAR_LOOKUP[[as.character(event_y)]]
  before_col <- paste0("ward_", map_y$before)
  after_col <- paste0("ward_", map_y$after)
  
  ward_crosswalk %>%
    # A pin is treated if its ward assignment differs and is not NA
    filter(!is.na(.data[[before_col]]) & !is.na(.data[[after_col]]) &
             .data[[before_col]] != .data[[after_col]]) %>%
    mutate(event_year = event_y) %>%
    select(pin10, event_year)
})


data_prepared <- bind_rows(data_prepared_list) %>%
  distinct(pin10, .keep_all = TRUE)

event_study_df <- land_values_aug %>%
  left_join(data_prepared, by = "pin10") %>%
  # A parcel is in the treatment group if it was ever remapped in an event year
  mutate(is_treated = ifelse(!is.na(event_year), 1, 0))



event_study_df <- event_study_df %>%
  mutate(
    # Log of land value (add 1 to avoid log(0))
    # log_land_value = log(land_sum + 1),
    # Time relative to the event. For control units, this will be based on a non-existent event_year,
    # so we set it to a value that sunab() ignores (e.g., -1 or 0 is fine).
    time_to_event = ifelse(is_treated == 1, tax_year - event_year, -1),
    cohort_year = if_else(is_treated == 1, event_year, 0),
    # Census block ID (approximated by first 7 digits of PIN)
    # This is a common and reasonable assumption for Cook County PINs.
    block_id = substr(pin10, 1, 7)
  ) %>%
  # Filter out data far from borders to create a more comparable sample
  # 820 feet is ~250 meters. Adjust as needed.
  filter(dist_to_boundary_ft <= 1056) %>%
  # Remove observations with missing crucial data
  filter(!is.na(ward_pair)) %>% 
  filter(tax_year >= (EVENT_YEARS - 5) & tax_year <= (EVENT_YEARS + 5))





all_ward_pairs <- unique(event_study_df$ward_pair)

message(paste("Found", length(all_ward_pairs), "unique ward pairs. Starting analysis loop..."))

for (current_pair in all_ward_pairs) {
  message(paste("--> Processing Ward Pair:", current_pair))
  
  # Filter data to the specific border being analyzed
  border_df <- event_study_df %>%
    filter(ward_pair == current_pair)
  
  # Ensure there are both treated and untreated observations for this border
  if (length(unique(border_df$is_treated)) < 2 ||
      nrow(border_df) < 100 ||
      sum(border_df$tax_year < 2015) < 20 ||
      sum(border_df$tax_year >= 2015) < 20) {
    message("   Skipping: Not enough data, no treatment variation, or too few pre/post observations.")
    next
  }
  
  # MODIFIED: Use a traditional TWFE event study model.
  # The i() function creates dummies for each value of time_to_event.
  # We set ref = -1, so the year before the event is the omitted baseline category.
  # The control group (where time_to_event is NA) is used by the fixed effects
  # to estimate the counterfactual trend.
  res <- feols(
    land_share_pin10 ~ i(time_to_event, ref = -1) | block_id + tax_year,
    data = border_df,
    cluster = ~block_id
  )
  
  # --- CUSTOM ggplot BLOCK ---
  results_df <- as.data.frame(res$coeftable)
  results_df$term <- rownames(results_df)
  rownames(results_df) <- NULL
  
  results_df <- results_df %>%
    rename(
      estimate = Estimate,
      std.error = `Std. Error`
    ) %>%
    # The term from i() is 'time_to_event::{period}', extract the numeric period
    mutate(
      time_to_event = as.numeric(stringr::str_extract(term, "-?\\d+$")),
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    ) %>%
    filter(!is.na(time_to_event))
  
  # Add the baseline year (t = -1) back in with a coefficient of 0 for plotting
  if(!(-1 %in% results_df$time_to_event)) {
    results_df <- bind_rows(results_df, tibble(
      time_to_event = -1, estimate = 0, std.error = 0, conf.low = 0, conf.high = 0
    ))
  }
  
  plot_title <- paste("Event Study for Ward Border:", current_pair)
  plot_subtitle <- "Effect of Ward Remapping on Land Value Share"
  plot_path <- file.path("../output", paste0("event_study_ward_pair_", current_pair, ".pdf"))
  
  p <- ggplot(results_df, aes(x = time_to_event, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray50") +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.2,
      color = "gray40"
    ) +
    geom_point(color = "midnightblue", size = 3) +
    theme_minimal(base_size = 14) +
    labs(
      x = "Years Relative to Ward Remap",
      y = "Coefficient (Land Value Share)",
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks())
  p
  ggsave(plot_path, p, width = 8, height = 6, bg = "white")
  message(paste("   Successfully saved plot to:", plot_path))
  
  gc()
  
  Sys.sleep(1)
}

message("Analysis complete. Plots are saved in ../output/")