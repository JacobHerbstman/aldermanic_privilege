# Build listed-rent RD hedonic, quality, and amenity decomposition diagnostics.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_characteristics/code")
# bandwidth_ft <- 500

source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")

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

message(sprintf("=== Listed-Rent RD Characteristics | bandwidth=%sft ===", bandwidth_label))

rent <- read_parquet("../input/rent_with_ward_distances.parquet") %>%
  as_tibble()

if (!"rent_panel_id" %in% names(rent)) {
  stop("Rental input must include rent_panel_id.", call. = FALSE)
}
if (any(is.na(rent$rent_panel_id) | rent$rent_panel_id == "")) {
  stop("Rental input contains missing rent_panel_id values.", call. = FALSE)
}
if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Rental input must be unique by rent_panel_id.", call. = FALSE)
}
if (!all(c("longitude", "latitude") %in% names(rent))) {
  stop("Rental input must include longitude and latitude from the audited geometry task.", call. = FALSE)
}
if (!"signed_dist" %in% names(rent) && "signed_dist_m" %in% names(rent)) {
  rent <- rent %>% mutate(signed_dist = signed_dist_m / 0.3048)
}
if (!"signed_dist" %in% names(rent)) {
  stop("Rental input must include signed_dist in feet or signed_dist_m in meters.", call. = FALSE)
}

for (flag_col in c(
  "flag_location_questionable",
  "flag_modal_assignment_missing",
  "flag_modal_changes_ward",
  "flag_modal_changes_neighbor_ward",
  "flag_modal_changes_pair",
  "flag_modal_dist_diff_gt100ft",
  "flag_rd_location_questionable",
  "flag_address_location_unstable",
  "flag_coordinate_only_generic_pile",
  "flag_building_type_conflict"
)) {
  if (!flag_col %in% names(rent)) {
    rent[[flag_col]] <- FALSE
  }
  rent[[flag_col]] <- coalesce(as.logical(rent[[flag_col]]), FALSE)
}

rent <- rent %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    right = as.integer(signed_dist_ft >= 0),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    is_multifamily = as.integer(building_type_clean == "multi_family"),
    is_single_family = as.integer(building_type_clean == "single_family"),
    is_condo = as.integer(building_type_clean == "condo"),
    is_townhouse = as.integer(building_type_clean == "townhouse"),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(is.finite(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    flag_clean_location_sample = !flag_location_questionable &
      !flag_modal_assignment_missing &
      !flag_modal_changes_ward &
      !flag_modal_changes_neighbor_ward &
      !flag_modal_changes_pair &
      !flag_modal_dist_diff_gt100ft,
    flag_no_modal_pair_change_sample = !flag_modal_assignment_missing & !flag_modal_changes_pair,
    flag_no_modal_ward_change_sample = !flag_modal_assignment_missing &
      !flag_modal_changes_ward &
      !flag_modal_changes_neighbor_ward,
    flag_no_questionable_address_sample = !flag_location_questionable
  ) %>%
  filter(
    !is.na(file_date),
    year >= 2014,
    year <= 2022,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= bandwidth_ft,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    is.finite(longitude),
    is.finite(latitude)
  )

if (nrow(rent) == 0) {
  stop("No rental observations remain in the RD window.", call. = FALSE)
}
if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("RD-window rental panel must remain unique by rent_panel_id.", call. = FALSE)
}

message(sprintf("RD-window rows: %s", format(nrow(rent), big.mark = ",")))
message("Computing amenity distances from audited rental coordinates...")

coords <- build_unique_coordinate_amenity_table(
  rent,
  "longitude",
  "latitude",
  "../input/schools_2015.gpkg",
  "../input/parks.gpkg",
  "../input/major_streets.gpkg",
  "../input/gis_osm_water_a_free_1.shp",
  chunk_n = 100000L,
  distance_units = "feet"
)

cta_stops <- read_amenity_layer("../input/cta_stops.gpkg") %>%
  mutate(
    station_id = as.character(station_id),
    active_from_date = as.Date(active_from_date),
    active_to_date = as.Date(active_to_date)
  )

if (!all(c("active_from_date", "active_to_date") %in% names(cta_stops))) {
  stop("CTA stop layer must include active_from_date and active_to_date.", call. = FALSE)
}
if (any(is.na(cta_stops$active_from_date))) {
  stop("CTA stop layer has missing active_from_date values.", call. = FALSE)
}

coords_month <- rent %>%
  distinct(longitude, latitude, year_month) %>%
  mutate(
    file_month_start = as.Date(paste0(year_month, "-01")),
    file_month_end = lubridate::ceiling_date(file_month_start, "month") - lubridate::days(1)
  )

coords_month_sf <- st_as_sf(
  coords_month,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)

cta_distances <- bind_rows(lapply(split(coords_month_sf, coords_month_sf$year_month), function(month_points) {
  month_start <- unique(month_points$file_month_start)
  month_end <- unique(month_points$file_month_end)
  if (length(month_start) != 1L || length(month_end) != 1L) {
    stop("CTA month split has non-unique month dates.", call. = FALSE)
  }

  active_cta <- cta_stops %>%
    filter(
      active_from_date <= month_end,
      is.na(active_to_date) | active_to_date >= month_start
    )
  if (nrow(active_cta) == 0) {
    stop(sprintf("No active CTA stations for %s.", unique(month_points$year_month)), call. = FALSE)
  }

  nearest_idx <- st_nearest_feature(month_points, active_cta)
  nearest_cta <- active_cta[nearest_idx, ]
  nearest_meta <- st_drop_geometry(nearest_cta)

  st_drop_geometry(month_points) %>%
    transmute(longitude, latitude, year_month) %>%
    mutate(
      nearest_cta_stop_dist_ft = as.numeric(st_distance(month_points, nearest_cta, by_element = TRUE)),
      nearest_cta_station_id = nearest_meta$station_id,
      nearest_cta_station_name = nearest_meta$longname,
      nearest_cta_lines = nearest_meta$lines
    )
}))

if (anyDuplicated(cta_distances[c("longitude", "latitude", "year_month")]) > 0) {
  stop("CTA distance table is not unique by coordinate-month.", call. = FALSE)
}

write_csv(
  cta_distances %>%
    count(year_month, nearest_cta_station_id, nearest_cta_station_name, nearest_cta_lines, name = "n_unique_coordinate_months") %>%
    arrange(year_month, desc(n_unique_coordinate_months)),
  sprintf("../output/rental_rd_cta_station_assignment_bw%s.csv", bandwidth_label)
)

rent <- rent %>%
  left_join(coords, by = c("longitude", "latitude"), relationship = "many-to-one") %>%
  left_join(cta_distances, by = c("longitude", "latitude", "year_month"), relationship = "many-to-one") %>%
  mutate(
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
  )

if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Amenity join expanded rent_panel_id rows.", call. = FALSE)
}

amenity_cols <- c(
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft"
)
amenity_diagnostics <- bind_rows(lapply(amenity_cols, function(metric) {
  x <- rent[[metric]]
  tibble(
    metric = metric,
    n_rows = nrow(rent),
    n_unique_coords = nrow(coords),
    n_nonmissing = sum(!is.na(x)),
    share_nonmissing = mean(!is.na(x)),
    min_distance_ft = min(x, na.rm = TRUE),
    p50_distance_ft = median(x, na.rm = TRUE),
    p90_distance_ft = quantile(x, 0.90, na.rm = TRUE),
    mean_distance_ft = mean(x, na.rm = TRUE),
    max_distance_ft = max(x, na.rm = TRUE)
  )
}))

write_csv(
  amenity_diagnostics,
  sprintf("../output/rental_rd_amenity_distance_diagnostics_bw%s.csv", bandwidth_label)
)
write_parquet(
  as.data.frame(rent),
  sprintf("../output/rental_rd_characteristics_panel_bw%s.parquet", bandwidth_label)
)

sample_defs <- tibble::tribble(
  ~sample, ~sample_label,
  "all", "All",
  "clean_location", "Clean location",
  "no_modal_pair_change", "No modal pair change",
  "no_modal_ward_change", "No modal ward change",
  "no_questionable_address", "No questionable address"
)

filter_sample <- function(df, sample_name) {
  if (sample_name == "all") {
    return(df)
  }
  if (sample_name == "clean_location") {
    return(df %>% filter(flag_clean_location_sample))
  }
  if (sample_name == "no_modal_pair_change") {
    return(df %>% filter(flag_no_modal_pair_change_sample))
  }
  if (sample_name == "no_modal_ward_change") {
    return(df %>% filter(flag_no_modal_ward_change_sample))
  }
  if (sample_name == "no_questionable_address") {
    return(df %>% filter(flag_no_questionable_address_sample))
  }
  stop(sprintf("Unknown sample: %s", sample_name), call. = FALSE)
}

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%$#&{}])", "\\\\\\1", x, perl = TRUE)
  x
}

format_tex_number <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits, big.mark = ",")
  )
}

format_tex_int <- function(x) {
  ifelse(
    is.na(x),
    "",
    formatC(round(x), format = "d", big.mark = ",")
  )
}

tex_stars <- function(p_value) {
  case_when(
    is.na(p_value) ~ "",
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

format_tex_coef <- function(estimate, std_error, p_value, scale = 1, digits = 2) {
  paste0(
    format_tex_number(scale * estimate, digits),
    tex_stars(p_value),
    " (",
    format_tex_number(scale * std_error, digits),
    ")"
  )
}

covariates <- tibble::tribble(
  ~variable, ~label, ~group,
  "beds", "Beds", "Hedonics",
  "baths", "Baths", "Hedonics",
  "sqft", "Sqft", "Hedonics",
  "log_sqft", "Log sqft", "Hedonics",
  "is_multifamily", "Multifamily", "Building type",
  "is_single_family", "Single family", "Building type",
  "is_condo", "Condo", "Building type",
  "is_townhouse", "Townhouse", "Building type",
  "laundry", "Laundry", "Unit amenities",
  "gym", "Gym", "Unit amenities",
  "doorman", "Doorman", "Unit amenities",
  "furnished", "Furnished", "Unit amenities",
  "pool", "Pool", "Unit amenities",
  "year_built", "Year built", "Hedonics",
  "active_days", "Active days in month", "Listing quality",
  "raw_rows_month", "Raw rows in month", "Listing quality",
  "address_missing", "Missing address", "Listing quality",
  "flag_location_questionable", "Questionable location", "Location quality",
  "flag_modal_changes_pair", "Modal pair changes", "Location quality",
  "flag_rd_location_questionable", "RD-location flag", "Location quality",
  "flag_building_type_conflict", "Building-type conflict", "Listing quality",
  "nearest_school_dist_ft", "Dist. to school", "External amenities",
  "nearest_park_dist_ft", "Dist. to park", "External amenities",
  "nearest_major_road_dist_ft", "Dist. to major road", "External amenities",
  "nearest_cta_stop_dist_ft", "Dist. to CTA stop", "External amenities",
  "lake_michigan_dist_ft", "Dist. to Lake Michigan", "External amenities"
) %>%
  filter(variable %in% names(rent))

balance_rows <- list()
for (i in seq_len(nrow(sample_defs))) {
  sample_name <- sample_defs$sample[i]
  sample_label <- sample_defs$sample_label[i]
  d_sample <- filter_sample(rent, sample_name)
  message(sprintf("Balance sample %s: %s rows", sample_name, format(nrow(d_sample), big.mark = ",")))

  for (j in seq_len(nrow(covariates))) {
    variable <- covariates$variable[j]
    d <- d_sample %>%
      mutate(Y = as.numeric(.data[[variable]])) %>%
      filter(is.finite(Y))
    if (nrow(d) < 100 || n_distinct(d$right) < 2 || n_distinct(d$segment_id) < 2) {
      next
    }

    y_sd <- sd(d$Y, na.rm = TRUE)
    if (!is.finite(y_sd) || y_sd <= 0) {
      next
    }

    model <- tryCatch(
      feols(Y ~ right | segment_id^year_month, data = d, cluster = ~segment_id),
      error = function(e) NULL
    )
    if (is.null(model) || !"right" %in% names(coef(model))) {
      next
    }

    balance_rows[[length(balance_rows) + 1]] <- tibble(
      sample = sample_name,
      sample_label = sample_label,
      variable = variable,
      label = covariates$label[j],
      group = covariates$group[j],
      estimate = coef(model)[["right"]],
      std_error = se(model)[["right"]],
      p_value = pvalue(model)[["right"]],
      estimate_std = coef(model)[["right"]] / y_sd,
      std_error_std = se(model)[["right"]] / y_sd,
      n_obs = model$nobs,
      n_segments = n_distinct(d$segment_id),
      n_ward_pairs = n_distinct(d$ward_pair),
      mean_less_stringent = mean(d$Y[d$right == 0], na.rm = TRUE),
      mean_more_stringent = mean(d$Y[d$right == 1], na.rm = TRUE),
      dep_var_sd = y_sd,
      bandwidth_ft = bandwidth_ft
    )
  }
}

balance <- bind_rows(balance_rows)
write_csv(balance, sprintf("../output/rental_rd_covariate_balance_bw%s.csv", bandwidth_label))

external_balance <- balance %>%
  filter(
    sample %in% c("all", "clean_location"),
    group == "External amenities"
  ) %>%
  mutate(cell = format_tex_coef(estimate, std_error, p_value, scale = 1, digits = 1)) %>%
  select(label, sample, cell) %>%
  pivot_wider(names_from = sample, values_from = cell)

external_balance_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "Covariate & All & Clean location \\\\",
  "\\midrule",
  sprintf(
    "%s & %s & %s \\\\",
    latex_escape(external_balance$label),
    external_balance$all,
    external_balance$clean_location
  ),
  "\\bottomrule",
  "\\end{tabular}",
    "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: Each cell reports the residualized stricter-side jump in feet, with standard errors in parentheses. Regressions use listed-rent observations from RentHub within 500ft of ward boundaries, segment-by-month fixed effects, and standard errors clustered by segment. Clean-location rows exclude observations with modal-coordinate ward, pair, or distance-instability flags. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}",
  "\\par\\endgroup"
)
writeLines(
  external_balance_lines,
  sprintf("../output/rental_rd_external_amenity_balance_bw%s.tex", bandwidth_label)
)

plot_balance <- balance %>%
  filter(sample %in% c("all", "clean_location")) %>%
  mutate(
    label = factor(label, levels = rev(unique(label))),
    ci_low = estimate_std - 1.96 * std_error_std,
    ci_high = estimate_std + 1.96 * std_error_std
  )

balance_plot <- ggplot(plot_balance, aes(x = estimate_std, y = label, color = group)) +
  geom_vline(xintercept = 0, color = "gray55", linetype = "dotted") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0, linewidth = 0.45) +
  geom_point(size = 1.8) +
  facet_wrap(~sample_label, ncol = 2) +
  labs(
    title = "Listed-Rent RD Covariate Balance",
    subtitle = sprintf("Standardized stricter-side jumps within %.0fft, segment-by-month FE", bandwidth_ft),
    x = "Standardized jump on stricter side",
    y = NULL,
    color = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(
  sprintf("../output/rental_rd_covariate_balance_bw%s.pdf", bandwidth_label),
  balance_plot,
  width = 10.5,
  height = 8,
  dpi = 300,
  bg = "white"
)

attenuation_rows <- list()
for (i in seq_len(nrow(sample_defs))) {
  sample_name <- sample_defs$sample[i]
  sample_label <- sample_defs$sample_label[i]
  d_sample <- filter_sample(rent, sample_name) %>%
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
    )

  if (nrow(d_sample) < 100 || n_distinct(d_sample$segment_id) < 2 || n_distinct(d_sample$right) < 2) {
    next
  }

  hedonic_rhs <- "right + log_sqft + log_beds + log_baths"
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
    "no_controls_common", "No controls", "right",
    "hedonic_common", "Hedonics", hedonic_rhs,
    "hedonic_amenity_common", "Hedonics + amenities", amenity_rhs
  )

  for (j in seq_len(nrow(model_specs))) {
    model <- feols(
      as.formula(paste0("log(rent_price) ~ ", model_specs$rhs[j], " | segment_id^year_month")),
      data = d_sample,
      cluster = ~segment_id
    )
    if (!"right" %in% names(coef(model))) {
      stop(sprintf("RD attenuation model failed to estimate right for %s / %s.", sample_name, model_specs$specification[j]), call. = FALSE)
    }
    attenuation_rows[[length(attenuation_rows) + 1]] <- tibble(
      sample = sample_name,
      sample_label = sample_label,
      specification = model_specs$specification[j],
      spec_label = model_specs$spec_label[j],
      estimate = coef(model)[["right"]],
      std_error = se(model)[["right"]],
      p_value = pvalue(model)[["right"]],
      n_obs = model$nobs,
      n_segments = n_distinct(d_sample$segment_id),
      n_ward_pairs = n_distinct(d_sample$ward_pair),
      dep_var_mean = mean(log(d_sample$rent_price), na.rm = TRUE),
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
write_csv(attenuation, sprintf("../output/rental_rd_rent_attenuation_bw%s.csv", bandwidth_label))

write_attenuation_table <- function(sample_name, output_path, include_clean_note = FALSE) {
  table_data <- attenuation %>%
    filter(sample == sample_name) %>%
    mutate(
      spec_label = as.character(spec_label),
      coefficient = paste0(format_tex_number(100 * estimate, 2), tex_stars(p_value)),
      std_error_cell = paste0("(", format_tex_number(100 * std_error, 2), ")")
    ) %>%
    arrange(match(spec_label, c("No controls", "Hedonics", "Hedonics + amenities")))

  if (nrow(table_data) != 3L) {
    stop(sprintf("Expected three rent attenuation rows for sample %s.", sample_name), call. = FALSE)
  }

  clean_note <- if (include_clean_note) {
    " The clean-location sample excludes observations with modal-coordinate ward, pair, or distance-instability flags."
  } else {
    ""
  }

  table_lines <- c(
    "\\begingroup",
    "\\centering",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) & (2) & (3) \\\\",
    "\\midrule",
    sprintf(
      "Stricter Side (\\%%) & %s & %s & %s \\\\",
      table_data$coefficient[1],
      table_data$coefficient[2],
      table_data$coefficient[3]
    ),
    sprintf(
      " & %s & %s & %s \\\\",
      table_data$std_error_cell[1],
      table_data$std_error_cell[2],
      table_data$std_error_cell[3]
    ),
    "\\\\",
    sprintf(
      "Observations & %s & %s & %s \\\\",
      format_tex_int(table_data$n_obs[1]),
      format_tex_int(table_data$n_obs[2]),
      format_tex_int(table_data$n_obs[3])
    ),
    sprintf(
      "Dep. Var. Mean & %s & %s & %s \\\\",
      format_tex_number(table_data$dep_var_mean[1], 2),
      format_tex_number(table_data$dep_var_mean[2], 2),
      format_tex_number(table_data$dep_var_mean[3], 2)
    ),
    sprintf(
      "Segments & %s & %s & %s \\\\",
      format_tex_int(table_data$n_segments[1]),
      format_tex_int(table_data$n_segments[2]),
      format_tex_int(table_data$n_segments[3])
    ),
    sprintf(
      "Ward Pairs & %s & %s & %s \\\\",
      format_tex_int(table_data$n_ward_pairs[1]),
      format_tex_int(table_data$n_ward_pairs[2]),
      format_tex_int(table_data$n_ward_pairs[3])
    ),
    "Hedonic Controls & & $\\checkmark$ & $\\checkmark$ \\\\",
    "Amenity Controls & & & $\\checkmark$ \\\\",
    "Segment $\\times$ Month FE & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
    "Cluster Level & Segment & Segment & Segment \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    sprintf(
      "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: Entries are percent log-rent jumps on the stricter side of a ward boundary, with standard errors in parentheses. The sample uses listed-rent floorplan-month observations from 2014--2022 within %sft of ward boundaries. Hedonic controls are log square feet, log bedrooms, log bathrooms, and building type. Amenity controls are distances to the nearest school, CPD park-boundary polygon, major street, CTA stop open by the listing month, and Lake Michigan.%s * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}",
      bandwidth_label,
      clean_note
    ),
    "\\par\\endgroup"
  )

  writeLines(table_lines, output_path)
}

write_attenuation_table(
  "all",
  sprintf("../output/rental_rd_rent_attenuation_bw%s.tex", bandwidth_label),
  include_clean_note = FALSE
)
write_attenuation_table(
  "clean_location",
  sprintf("../output/rental_rd_rent_attenuation_clean_location_bw%s.tex", bandwidth_label),
  include_clean_note = TRUE
)

attenuation_plot <- ggplot(
  attenuation,
  aes(x = spec_label, y = estimate, ymin = ci_low, ymax = ci_high, color = sample_label)
) +
  geom_hline(yintercept = 0, color = "gray55", linetype = "dotted") +
  geom_pointrange(position = position_dodge(width = 0.55), linewidth = 0.45) +
  labs(
    title = "Listed-Rent Jump With Hedonic And Amenity Controls",
    subtitle = sprintf("Common complete-case sample within %.0fft; segment-by-month FE", bandwidth_ft),
    x = NULL,
    y = "Stricter-side jump in log rent",
    color = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(
  sprintf("../output/rental_rd_rent_attenuation_bw%s.pdf", bandwidth_label),
  attenuation_plot,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
ggsave(
  sprintf("../output/rental_rd_rent_attenuation_bw%s.png", bandwidth_label),
  attenuation_plot,
  width = 9,
  height = 5.5,
  dpi = 220,
  bg = "white"
)

message("Saved listed-rent RD characteristics diagnostics.")
