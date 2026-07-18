# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")
# bandwidth_ft <- 500
# bins_per_side <- 10
# start_year <- 2006
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
bins_per_side <- as.numeric(cli_args[2])
start_year <- suppressWarnings(as.integer(cli_args[3]))
end_year <- suppressWarnings(as.integer(cli_args[4]))
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side <= 0 || bins_per_side != floor(bins_per_side)) {
  stop("bins_per_side must be a positive integer.", call. = FALSE)
}
if (!is.finite(start_year) || !is.finite(end_year) || start_year > end_year) {
  stop("start_year and end_year must be valid integer years with start_year <= end_year.", call. = FALSE)
}
bandwidth_label <- cli_args[1]
bins_per_side <- as.integer(bins_per_side)

sales <- read_parquet("../output/sales_with_hedonics_amenities.parquet") %>%
  as_tibble()

if (!"signed_dist_m" %in% names(sales)) {
  stop("Sales input must include signed_dist_m.", call. = FALSE)
}

sales <- sales %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    signed_dist = as.numeric(signed_dist_m) / 0.3048,
    right = as.integer(signed_dist >= 0)
  ) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= start_year,
    year <= end_year,
    !is.na(ward_pair),
    !is.na(segment_id), segment_id != "",
    is.finite(signed_dist),
    abs(signed_dist) <= bandwidth_ft,
    !is.na(strictness_own),
    !is.na(strictness_neighbor)
  )

hedonic_controls <- c(
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage"
)
amenity_controls <- c(
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft"
)
sales <- sales %>%
  filter(if_all(all_of(c(hedonic_controls, amenity_controls)), ~ !is.na(.x)))
rhs <- paste(c("right", hedonic_controls, amenity_controls), collapse = " + ")

if (nrow(sales) == 0) {
  stop("No sales remain after RD filtering.", call. = FALSE)
}
if (n_distinct(sales$ward_pair) < 2) {
  stop("Sales RD plot has fewer than two ward pairs.", call. = FALSE)
}

model <- feols(
  as.formula(paste0("log(sale_price) ~ ", rhs, " | segment_id^year_quarter")),
  data = sales,
  cluster = ~segment_id
)

if (!"right" %in% names(coef(model))) {
  stop("Sales RD plot model did not estimate the stricter-side coefficient.", call. = FALSE)
}
estimate <- unname(coef(model)[["right"]])
model_row <- coeftable(model)[rownames(coeftable(model)) %in% "right", , drop = FALSE]
if (nrow(model_row) != 1L) {
  stop("Could not recover the sales RD point estimate.", call. = FALSE)
}
std_error <- unname(model_row[1, "Std. Error"])
p_value <- unname(model_row[1, "Pr(>|t|)"])
stars <- dplyr::case_when(
  is.finite(p_value) & p_value <= 0.01 ~ "***",
  is.finite(p_value) & p_value <= 0.05 ~ "**",
  is.finite(p_value) & p_value <= 0.10 ~ "*",
  TRUE ~ ""
)
subtitle_label <- sprintf(
  "Jump = %.3f%s (SE %.3f), N = %s",
  estimate,
  stars,
  std_error,
  format(nobs(model), big.mark = ",")
)

removed <- model$obs_selection$obsRemoved
keep_idx <- if (is.null(removed)) {
  seq_len(nrow(sales))
} else {
  setdiff(seq_len(nrow(sales)), abs(as.integer(removed)))
}
plot_data <- sales[keep_idx, , drop = FALSE]
plot_data$y_adjusted <- as.numeric(resid(model)) + estimate * plot_data$right

bin_width <- bandwidth_ft / bins_per_side
bins <- plot_data %>%
  mutate(bin_center = (floor(signed_dist / bin_width) + 0.5) * bin_width) %>%
  group_by(bin_center) %>%
  summarise(
    mean_y = mean(y_adjusted, na.rm = TRUE),
    side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
    .groups = "drop"
  )

plot <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.5) +
  scale_color_manual(
    values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"),
    name = NULL
  ) +
  labs(
    title = "Home Sale Prices by Side of Ward Boundary",
    subtitle = subtitle_label,
    x = "Distance to ward boundary (feet; positive = more stringent side)",
    y = "Segment-by-quarter adjusted log sale price"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(
  sprintf("../output/sales_rd_flat_bw%s_year_quarter_amenity_clust_segment.pdf", bandwidth_label),
  plot,
  width = 8.6,
  height = 6,
  dpi = 300,
  bg = "white"
)

panel_plot <- plot +
  labs(
    x = "Distance to ward boundary (feet)",
    y = "Adjusted log sale price"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

ggsave(
  sprintf("../output/sales_rd_flat_bw%s_year_quarter_amenity_clust_segment_panel.pdf", bandwidth_label),
  panel_plot,
  width = 4.8,
  height = 4.1,
  dpi = 300,
  bg = "white"
)
