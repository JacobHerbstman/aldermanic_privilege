# Build listed-rent RD attenuation tables and plot from the characteristics panel.

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_rent_attenuation/code")
# bandwidth_ft <- 500

source("../../setup_environment/code/packages.R", local = new.env(parent = globalenv()))

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <bandwidth_ft>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))

message(sprintf("=== Listed-Rent RD Attenuation | bandwidth=%sft ===", bandwidth_label))

rent <- read_parquet(sprintf("../input/rental_rd_characteristics_panel_bw%s.parquet", bandwidth_label)) %>%
  as_tibble()

sample_defs <- tibble::tribble(
  ~sample, ~sample_label, ~filter_column,
  "all", "All", NA_character_,
  "clean_location", "Clean location", "flag_clean_location_sample",
  "no_modal_pair_change", "No modal pair change", "flag_no_modal_pair_change_sample",
  "no_modal_ward_change", "No modal ward change", "flag_no_modal_ward_change_sample",
  "no_questionable_address", "No questionable address", "flag_no_questionable_address_sample"
)

attenuation_rows <- list()
for (i in seq_len(nrow(sample_defs))) {
  sample_name <- sample_defs$sample[[i]]
  sample_label <- sample_defs$sample_label[[i]]
  filter_column <- sample_defs$filter_column[[i]]
  d_sample <- rent
  if (!is.na(filter_column)) {
    if (!filter_column %in% names(d_sample)) {
      stop(sprintf("Missing sample flag column: %s", filter_column), call. = FALSE)
    }
    d_sample <- d_sample %>% filter(.data[[filter_column]])
  }
  d_sample <- d_sample %>%
    filter(
      is.finite(log_sqft),
      is.finite(log_beds),
      is.finite(log_baths),
      if_all(
        all_of(c(
          "nearest_school_dist_kft",
          "nearest_park_dist_kft",
          "nearest_major_road_dist_kft",
          "nearest_cta_stop_dist_kft",
          "lake_michigan_dist_kft"
        )),
        is.finite
      )
    ) %>%
    mutate(strictness_std = strictness_own / sd(strictness_own, na.rm = TRUE))

  if (nrow(d_sample) < 100 || n_distinct(d_sample$segment_id) < 2 || n_distinct(d_sample$strictness_std) < 2) {
    next
  }

  hedonic_rhs <- "strictness_std + log_sqft + log_beds + log_baths"
  if (n_distinct(d_sample$building_type_factor) > 1) {
    hedonic_rhs <- paste0(hedonic_rhs, " + building_type_factor")
  }
  amenity_rhs <- paste(
    hedonic_rhs,
    "nearest_school_dist_kft",
    "nearest_park_dist_kft",
    "nearest_major_road_dist_kft",
    "nearest_cta_stop_dist_kft",
    "lake_michigan_dist_kft",
    sep = " + "
  )

  model_specs <- tibble::tribble(
    ~specification, ~spec_label, ~rhs,
    "no_controls_common", "No controls", "strictness_std",
    "hedonic_common", "Hedonics", hedonic_rhs,
    "hedonic_amenity_common", "Hedonics + amenities", amenity_rhs
  )

  for (j in seq_len(nrow(model_specs))) {
    model <- feols(
      as.formula(paste0("log(rent_price) ~ ", model_specs$rhs[j], " | segment_id^year_month")),
      data = d_sample,
      cluster = ~segment_id
    )
    if (!"strictness_std" %in% names(coef(model))) {
      stop(sprintf("RD attenuation model failed to estimate strictness_std for %s / %s.", sample_name, model_specs$specification[j]), call. = FALSE)
    }
    attenuation_rows[[length(attenuation_rows) + 1]] <- tibble(
      sample = sample_name,
      sample_label = sample_label,
      specification = model_specs$specification[j],
      spec_label = model_specs$spec_label[j],
      estimate = coef(model)[["strictness_std"]],
      std_error = se(model)[["strictness_std"]],
      p_value = pvalue(model)[["strictness_std"]],
      n_obs = model$nobs,
      n_segments = n_distinct(d_sample$segment_id),
      n_ward_pairs = n_distinct(d_sample$ward_pair),
      dep_var_mean = mean(d_sample$rent_price, na.rm = TRUE),
      bandwidth_ft = bandwidth_ft,
      common_sample = TRUE
    )
  }
}

attenuation <- bind_rows(attenuation_rows) %>%
  mutate(
    ci_low = estimate - 1.96 * std_error,
    ci_high = estimate + 1.96 * std_error,
    spec_label = factor(spec_label, levels = c("No controls", "Hedonics", "Hedonics + amenities")),
    sample_label = factor(sample_label, levels = sample_defs$sample_label)
  )
write_csv(attenuation, sprintf("../temp/rental_rd_rent_attenuation_bw%s.csv", bandwidth_label))
message("Saved listed-rent RD attenuation estimates.")
