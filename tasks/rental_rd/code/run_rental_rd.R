# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd/code")
# bandwidth_ft <- 500
# bins_per_side <- 15
# start_year <- 2014
# end_year <- 2022

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft, bins_per_side, start_year, end_year)
}
if (length(cli_args) != 4) {
  stop("FATAL: Script requires 4 args: <bandwidth_ft> <bins_per_side> <start_year> <end_year>.", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
bins_per_side <- as.integer(cli_args[2])
start_year <- suppressWarnings(as.integer(cli_args[3]))
end_year <- suppressWarnings(as.integer(cli_args[4]))

if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side <= 0) {
  stop("bins_per_side must be positive.", call. = FALSE)
}
if (!is.finite(start_year) || !is.finite(end_year) || start_year > end_year) {
  stop("start_year and end_year must be valid integer years with start_year <= end_year.", call. = FALSE)
}

rent <- read_parquet(sprintf("../input/rental_rd_characteristics_panel_bw%.0f.parquet", bandwidth_ft)) %>%
  as_tibble()

if (!"rent_panel_id" %in% names(rent)) {
  stop("Rental RD input must include rent_panel_id.", call. = FALSE)
}
if (any(is.na(rent$rent_panel_id) | rent$rent_panel_id == "")) {
  stop("Rental RD input contains missing rent_panel_id values.", call. = FALSE)
}
if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Rental RD input must be unique by rent_panel_id.", call. = FALSE)
}
if (!"signed_dist" %in% names(rent) && "signed_dist_m" %in% names(rent)) {
  rent <- rent %>% mutate(signed_dist = signed_dist_m / 0.3048)
}
if (!"signed_dist" %in% names(rent)) {
  stop("Rental input must include signed_dist in feet or signed_dist_m in meters.", call. = FALSE)
}
if (!"segment_id" %in% names(rent)) {
  stop("Rental input must include segment_id for the main RD fixed effects.", call. = FALSE)
}

rent <- rent %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    right = as.integer(signed_dist_ft >= 0),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(is.finite(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  ) %>%
  filter(
    !is.na(file_date),
    year >= start_year,
    year <= end_year,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= bandwidth_ft,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair)
  )

rent <- rent %>%
  mutate(
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
  ) %>%
  filter(
    !is.na(log_sqft),
    !is.na(log_beds),
    !is.na(log_baths),
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

if (nrow(rent) == 0) {
  stop("No rental observations remain after RD filtering.", call. = FALSE)
}
if (n_distinct(rent$segment_id) < 2) {
  stop("RD sample has fewer than two segments.", call. = FALSE)
}
if (n_distinct(rent$right) < 2) {
  stop("RD sample does not contain both sides of stricter/lenient borders.", call. = FALSE)
}

rhs <- "right + log_sqft + log_beds + log_baths"
if (n_distinct(rent$building_type_factor) > 1) {
  rhs <- paste(rhs, "+ building_type_factor")
}
rhs <- paste(
  rhs,
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft",
  sep = " + "
)
fml <- as.formula(paste0("log(rent_price) ~ ", rhs, " | segment_id^year_month"))

model <- feols(fml, data = rent, cluster = ~segment_id)
if (!"right" %in% names(coef(model))) {
  stop("RD model did not estimate the stricter-side coefficient.", call. = FALSE)
}

estimate <- unname(coef(model)[["right"]])

removed <- model$obs_selection$obsRemoved
keep_idx <- if (is.null(removed)) {
  seq_len(nrow(rent))
} else {
  setdiff(seq_len(nrow(rent)), abs(as.integer(removed)))
}
plot_data <- rent[keep_idx, , drop = FALSE]
plot_data$y_adjusted <- as.numeric(resid(model)) + estimate * plot_data$right

bin_width <- bandwidth_ft / bins_per_side
bins <- plot_data %>%
  mutate(
    bin_id = floor(signed_dist_ft / bin_width),
    bin_center = (bin_id + 0.5) * bin_width
  ) %>%
  group_by(bin_center) %>%
  summarise(
    mean_y = mean(y_adjusted, na.rm = TRUE),
    side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
    .groups = "drop"
  )

plot <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.4) +
  scale_color_manual(
    values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"),
    name = NULL
  ) +
  labs(
    title = "Listed Rents by Side of Ward Boundary",
    x = "Distance to ward boundary (feet; positive = more stringent side)",
    y = "Segment-by-month adjusted log rent"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(
  sprintf("../output/rental_rd_flat_bw%.0f_%d_%d_all_controls.pdf", bandwidth_ft, start_year, end_year),
  plot,
  width = 8.6,
  height = 6,
  dpi = 300,
  bg = "white"
)
