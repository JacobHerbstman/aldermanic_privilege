# Plot a flat home-sales border RD using the same sample as the main sales table.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")
# bw_ft <- 500
# use_controls <- TRUE
# bins_per_side <- 15
# min_strictness_diff_pctile <- 0
# fe_geo <- "segment"
# cluster_level <- "ward_pair"
# table_mode <- "amenity"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    bw_ft,
    use_controls,
    bins_per_side,
    min_strictness_diff_pctile,
    fe_geo,
    cluster_level,
    table_mode
  )
}
if (length(cli_args) != 7) {
  stop(
    "FATAL: Script requires args: <bw_ft> <use_controls> <bins_per_side> <min_strictness_diff_pctile> <fe_geo> <cluster_level> <table_mode>",
    call. = FALSE
  )
}

bw_ft <- suppressWarnings(as.integer(cli_args[1]))
use_controls <- tolower(cli_args[2]) %in% c("true", "t", "1", "yes")
bins_per_side <- suppressWarnings(as.integer(cli_args[3]))
min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[4]))
fe_geo <- tolower(cli_args[5])
cluster_level <- tolower(cli_args[6])
table_mode <- tolower(cli_args[7])

if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be positive.", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side <= 0) {
  stop("bins_per_side must be positive.", call. = FALSE)
}
if (!is.finite(min_strictness_diff_pctile) || min_strictness_diff_pctile < 0) {
  stop("min_strictness_diff_pctile must be nonnegative.", call. = FALSE)
}
if (!fe_geo %in% c("segment", "ward_pair")) {
  stop("fe_geo must be one of: segment, ward_pair.", call. = FALSE)
}
if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("cluster_level must be one of: segment, ward_pair.", call. = FALSE)
}
if (!table_mode %in% c("baseline", "amenity")) {
  stop("table_mode must be one of: baseline, amenity.", call. = FALSE)
}

stars <- function(p_value) {
  if (!is.finite(p_value)) {
    return("")
  }
  if (p_value < 0.01) {
    return("***")
  }
  if (p_value < 0.05) {
    return("**")
  }
  if (p_value < 0.1) {
    return("*")
  }
  ""
}

message(sprintf(
  "=== Sales flat RD plot | bw=%d | controls=%s | mode=%s ===",
  bw_ft,
  use_controls,
  table_mode
))

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
    is.finite(signed_dist),
    abs(signed_dist) <= bw_ft,
    !is.na(strictness_own),
    !is.na(strictness_neighbor)
  )

if (fe_geo == "segment" || cluster_level == "segment") {
  sales <- sales %>% filter(!is.na(segment_id), segment_id != "")
}

if (min_strictness_diff_pctile > 0) {
  pair_diffs <- sales %>%
    group_by(ward_pair) %>%
    summarise(diff = first(abs(strictness_own - strictness_neighbor)), .groups = "drop")
  cutoff <- quantile(pair_diffs$diff, min_strictness_diff_pctile / 100)
  sales <- sales %>%
    filter(ward_pair %in% pair_diffs$ward_pair[pair_diffs$diff >= cutoff])
}

rhs <- "right"
if (use_controls) {
  hedonic_controls <- c(
    "log_sqft",
    "log_land_sqft",
    "log_building_age",
    "log_bedrooms",
    "log_baths",
    "has_garage"
  )
  sales <- sales %>%
    filter(if_all(all_of(hedonic_controls), ~ !is.na(.x)))
  rhs <- paste(c("right", hedonic_controls), collapse = " + ")
}
if (use_controls && table_mode == "amenity") {
  amenity_controls <- c(
    "nearest_school_dist_ft",
    "nearest_park_dist_ft",
    "nearest_major_road_dist_ft",
    "lake_michigan_dist_ft"
  )
  sales <- sales %>%
    filter(if_all(all_of(amenity_controls), ~ !is.na(.x)))
  rhs <- paste(c(rhs, amenity_controls), collapse = " + ")
}

if (nrow(sales) == 0) {
  stop("No sales remain after RD filtering.", call. = FALSE)
}
if (n_distinct(sales$ward_pair) < 2) {
  stop("Sales RD plot has fewer than two ward pairs.", call. = FALSE)
}

fe_term <- if (fe_geo == "segment") {
  "segment_id^year_quarter"
} else {
  "ward_pair^year_quarter"
}

model <- feols(
  as.formula(paste0("log(sale_price) ~ ", rhs, " | ", fe_term)),
  data = sales,
  cluster = if (cluster_level == "segment") ~segment_id else ~ward_pair
)

ct <- coeftable(model)
estimate <- ct["right", "Estimate"]
std_error <- ct["right", "Std. Error"]
p_value <- ct["right", "Pr(>|t|)"]

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

line_data <- bind_rows(
  tibble(
    x = c(-bw_ft, 0),
    y = mean(plot_data$y_adjusted[plot_data$right == 0], na.rm = TRUE),
    side = "Less Stringent"
  ),
  tibble(
    x = c(0, bw_ft),
    y = mean(plot_data$y_adjusted[plot_data$right == 1], na.rm = TRUE),
    side = "More Stringent"
  )
)

plot_subtitle <- sprintf(
  "Jump = %.4f%s (SE %.4f), N = %s, %.0fft",
  estimate,
  stars(p_value),
  std_error,
  format(model$nobs, big.mark = ","),
  bw_ft
)

plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.5) +
  geom_line(data = line_data, aes(x = x, y = y, color = side), linewidth = 1.1) +
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
  "../output/sales_rd_flat_bw500_year_quarter_amenity_clust_ward_pair.pdf",
  plot,
  width = 8.6,
  height = 6,
  dpi = 300,
  bg = "white"
)

message("Saved: ../output/sales_rd_flat_bw500_year_quarter_amenity_clust_ward_pair.pdf")
