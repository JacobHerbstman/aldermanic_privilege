# setwd("tasks/sales_border_pair_fe/code")

source("../../setup_environment/code/packages.R")

bw_ft <- 500L
bins_per_side <- 10L

sales <- read_parquet("../output/sales_with_hedonics_amenities.parquet") %>%
  as_tibble()

if (!"signed_dist" %in% names(sales) && "signed_dist_m" %in% names(sales)) {
  sales <- sales %>% mutate(signed_dist = signed_dist_m / 0.3048)
}
if (!"signed_dist" %in% names(sales)) {
  stop("Sales input must include signed_dist in feet or signed_dist_m in meters.", call. = FALSE)
}

sales <- sales %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    signed_dist = as.numeric(signed_dist),
    right = as.integer(signed_dist >= 0)
  ) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= 2006,
    year <= 2022,
    !is.na(ward_pair),
    !is.na(segment_id), segment_id != "",
    is.finite(signed_dist),
    abs(signed_dist) <= bw_ft,
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

ct <- coeftable(model)
estimate <- ct["right", "Estimate"]
std_error <- ct["right", "Std. Error"]
p_value <- ct["right", "Pr(>|t|)"]
star_text <- case_when(
  !is.finite(p_value) ~ "",
  p_value < 0.01 ~ "***",
  p_value < 0.05 ~ "**",
  p_value < 0.1 ~ "*",
  TRUE ~ ""
)

removed <- model$obs_selection$obsRemoved
keep_idx <- if (is.null(removed)) {
  seq_len(nrow(sales))
} else {
  setdiff(seq_len(nrow(sales)), abs(as.integer(removed)))
}
plot_data <- sales[keep_idx, , drop = FALSE]
plot_data$y_adjusted <- as.numeric(resid(model)) + estimate * plot_data$right

bin_width <- bw_ft / bins_per_side
bins <- plot_data %>%
  mutate(bin_center = (floor(signed_dist / bin_width) + 0.5) * bin_width) %>%
  group_by(bin_center) %>%
  summarise(
    mean_y = mean(y_adjusted, na.rm = TRUE),
    side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
    .groups = "drop"
  )

plot_subtitle <- sprintf(
  "Jump = %.4f%s (SE %.4f), N = %s, %.0fft",
  estimate,
  star_text,
  std_error,
  format(model$nobs, big.mark = ","),
  bw_ft
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
    subtitle = plot_subtitle,
    x = "Distance to ward boundary (feet; positive = more stringent side)",
    y = "Segment-by-quarter adjusted log sale price"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(
  "../output/sales_rd_flat_bw500_year_quarter_amenity_clust_segment.pdf",
  plot,
  width = 8.6,
  height = 6,
  dpi = 300,
  bg = "white"
)
