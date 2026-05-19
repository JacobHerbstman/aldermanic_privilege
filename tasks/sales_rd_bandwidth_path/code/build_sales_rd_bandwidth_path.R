# Plot sales RD estimates across bandwidths.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_rd_bandwidth_path/code")
# bandwidths_ft <- "100,200,300,400,500,600,700,800,900,1000"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidths_ft)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <bandwidths_ft>", call. = FALSE)
}

bandwidths_ft <- as.numeric(strsplit(cli_args[1], ",", fixed = TRUE)[[1]])
if (length(bandwidths_ft) == 0 || any(!is.finite(bandwidths_ft)) || any(bandwidths_ft <= 0)) {
  stop("bandwidths_ft must be a comma-separated list of positive numbers.", call. = FALSE)
}

message("=== Sales RD Bandwidth Path ===")
message(sprintf("Bandwidths: %s ft", paste(bandwidths_ft, collapse = ", ")))

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble()

if (!"signed_dist_m" %in% names(sales) && !"signed_dist" %in% names(sales)) {
  stop("Sales input must include signed_dist_m in meters or signed_dist in feet.", call. = FALSE)
}
if (!"segment_id" %in% names(sales)) {
  stop("Sales input must include segment_id for segment fixed effects.", call. = FALSE)
}

if (!"signed_dist" %in% names(sales)) {
  sales <- sales %>% mutate(signed_dist = signed_dist_m / 0.3048)
}

sales <- sales %>%
  mutate(
    sale_date = as.Date(sale_date),
    year = lubridate::year(sale_date),
    year_quarter = paste0(year, "-Q", lubridate::quarter(sale_date)),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    right = as.integer(signed_dist_ft >= 0),
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
  ) %>%
  filter(
    year >= 2006,
    year <= 2022,
    is.finite(sale_price),
    sale_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= max(bandwidths_ft),
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair)
  )

if (nrow(sales) == 0) {
  stop("No sales observations remain after the RD sample restrictions.", call. = FALSE)
}

hedonic_cols <- c(
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage"
)
missing_hedonic_cols <- setdiff(hedonic_cols, names(sales))
if (length(missing_hedonic_cols) > 0) {
  stop(sprintf("Sales input is missing hedonic columns: %s.", paste(missing_hedonic_cols, collapse = ", ")), call. = FALSE)
}

amenity_cols <- c(
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "lake_michigan_dist_kft"
)
missing_amenity_cols <- setdiff(amenity_cols, names(sales))
if (length(missing_amenity_cols) > 0) {
  stop(sprintf("Sales input is missing amenity columns: %s.", paste(missing_amenity_cols, collapse = ", ")), call. = FALSE)
}

fe_specs <- tribble(
  ~fe_spec, ~fe_label, ~fe_terms, ~cluster_var,
  "segment_quarter", "Segment x quarter", "segment_id^year_quarter", "segment_id",
  "segment_quarter_cluster_ward_pair", "Segment x quarter; ward-pair cluster", "segment_id^year_quarter", "ward_pair",
  "segment_year_quarter", "Segment x year + quarter", "segment_id^year + year_quarter", "segment_id",
  "ward_pair_quarter", "Ward-pair x quarter", "ward_pair^year_quarter", "ward_pair"
)

support <- bind_rows(lapply(sort(unique(bandwidths_ft)), function(bw_i) {
  d <- sales %>%
    filter(abs(signed_dist_ft) <= bw_i)
  segment_quarter_support <- d %>%
    group_by(segment_id, year_quarter) %>%
    summarise(
      n = n(),
      has_right = any(right == 1),
      has_left = any(right == 0),
      .groups = "drop"
    )
  pair_quarter_support <- d %>%
    group_by(ward_pair, year_quarter) %>%
    summarise(
      n = n(),
      has_right = any(right == 1),
      has_left = any(right == 0),
      .groups = "drop"
    )

  tibble(
    bandwidth_ft = bw_i,
    n_obs = nrow(d),
    n_right = sum(d$right == 1, na.rm = TRUE),
    n_left = sum(d$right == 0, na.rm = TRUE),
    n_segments = n_distinct(d$segment_id),
    n_ward_pairs = n_distinct(d$ward_pair),
    n_segment_quarters = n_distinct(paste(d$segment_id, d$year_quarter)),
    n_pair_quarters = n_distinct(paste(d$ward_pair, d$year_quarter)),
    n_two_sided_segment_quarters = sum(segment_quarter_support$has_right & segment_quarter_support$has_left),
    n_obs_two_sided_segment_quarters = sum(segment_quarter_support$n[segment_quarter_support$has_right & segment_quarter_support$has_left]),
    n_two_sided_pair_quarters = sum(pair_quarter_support$has_right & pair_quarter_support$has_left),
    n_obs_two_sided_pair_quarters = sum(pair_quarter_support$n[pair_quarter_support$has_right & pair_quarter_support$has_left]),
    hedonic_complete = sum(stats::complete.cases(d[, hedonic_cols])),
    amenity_complete = sum(stats::complete.cases(d[, c(hedonic_cols, amenity_cols)])),
    median_sale_price = median(d$sale_price, na.rm = TRUE)
  )
}))
write_csv(support, "../output/sales_rd_bandwidth_support.csv")

control_specs <- tibble(
  control_set = c("none", "hedonic", "hedonic_amenity"),
  spec_label = c("No controls", "Hedonic controls", "Hedonics + amenities"),
  control_cols = list(character(), hedonic_cols, c(hedonic_cols, amenity_cols))
)

estimate_one <- function(data, bandwidth_ft, control_set, spec_label, control_cols, fe_spec, fe_label, fe_terms, cluster_var, rd_spec = "flat") {
  d <- data %>%
    filter(abs(signed_dist_ft) <= bandwidth_ft) %>%
    mutate(running_100ft = signed_dist_ft / 100)

  if (!rd_spec %in% c("flat", "local_linear")) {
    stop("rd_spec must be flat or local_linear.", call. = FALSE)
  }
  rd_label <- ifelse(rd_spec == "flat", "Flat jump", "Local linear")

  if (length(control_cols) > 0) {
    d <- d %>%
      filter(if_all(all_of(control_cols), ~ is.finite(.x)))
  }

  if (nrow(d) == 0 || n_distinct(d$right) < 2 || n_distinct(d[[cluster_var]]) < 2) {
    return(tibble(
      bandwidth_ft = bandwidth_ft,
      control_set = control_set,
      spec_label = spec_label,
      rd_spec = rd_spec,
      rd_label = rd_label,
      fe_spec = fe_spec,
      fe_label = fe_label,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(d),
      n_segments = n_distinct(d$segment_id),
      n_ward_pairs = n_distinct(d$ward_pair),
      fixed_effects = fe_label,
      cluster = cluster_var
    ))
  }

  rhs <- if (rd_spec == "local_linear") {
    "right + running_100ft + right:running_100ft"
  } else {
    "right"
  }
  if (length(control_cols) > 0) {
    rhs <- paste(c(rhs, control_cols), collapse = " + ")
  }

  fit <- tryCatch(
    feols(
      as.formula(paste0("log(sale_price) ~ ", rhs, " | ", fe_terms)),
      data = d,
      cluster = as.formula(paste0("~", cluster_var))
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    message(sprintf(
      "Skipped %s at %.0fft (%s): %s",
      fe_spec, bandwidth_ft, spec_label, fit$message
    ))
    return(tibble(
      bandwidth_ft = bandwidth_ft,
      control_set = control_set,
      spec_label = spec_label,
      rd_spec = rd_spec,
      rd_label = rd_label,
      fe_spec = fe_spec,
      fe_label = fe_label,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(d),
      n_segments = n_distinct(d$segment_id),
      n_ward_pairs = n_distinct(d$ward_pair),
      fixed_effects = fe_label,
      cluster = cluster_var
    ))
  }

  ct <- coeftable(fit)
  if (!"right" %in% rownames(ct)) {
    return(tibble(
      bandwidth_ft = bandwidth_ft,
      control_set = control_set,
      spec_label = spec_label,
      rd_spec = rd_spec,
      rd_label = rd_label,
      fe_spec = fe_spec,
      fe_label = fe_label,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      n_obs = fit$nobs,
      n_segments = n_distinct(d$segment_id),
      n_ward_pairs = n_distinct(d$ward_pair),
      fixed_effects = fe_label,
      cluster = cluster_var
    ))
  }

  tibble(
    bandwidth_ft = bandwidth_ft,
    control_set = control_set,
    spec_label = spec_label,
    rd_spec = rd_spec,
    rd_label = rd_label,
    fe_spec = fe_spec,
    fe_label = fe_label,
    estimate = unname(ct["right", "Estimate"]),
    std_error = unname(ct["right", "Std. Error"]),
    p_value = unname(ct["right", "Pr(>|t|)"]),
    n_obs = fit$nobs,
    n_segments = n_distinct(d$segment_id),
    n_ward_pairs = n_distinct(d$ward_pair),
    fixed_effects = fe_label,
    cluster = cluster_var
  )
}

results <- bind_rows(lapply(sort(unique(bandwidths_ft)), function(bw_i) {
  bind_rows(lapply(seq_len(nrow(fe_specs)), function(j) {
    bind_rows(lapply(seq_len(nrow(control_specs)), function(k) {
      estimate_one(
        sales, bw_i,
        control_specs$control_set[k],
        control_specs$spec_label[k],
        control_specs$control_cols[[k]],
        fe_specs$fe_spec[j],
        fe_specs$fe_label[j],
        fe_specs$fe_terms[j],
        fe_specs$cluster_var[j]
      )
    }))
  }))
}))

results <- results %>%
  mutate(
    spec_label = factor(spec_label, levels = control_specs$spec_label),
    cluster_label = if_else(cluster == "ward_pair", "Ward-pair cluster", "Segment cluster"),
    pct_change = 100 * (exp(estimate) - 1),
    pct_ci_low = 100 * (exp(estimate - 1.96 * std_error) - 1),
    pct_ci_high = 100 * (exp(estimate + 1.96 * std_error) - 1)
  ) %>%
  arrange(fe_spec, control_set, bandwidth_ft)

write_csv(results, "../output/sales_rd_bandwidth_path.csv")

local_linear_results <- bind_rows(lapply(sort(unique(bandwidths_ft)), function(bw_i) {
  bind_rows(lapply(seq_len(nrow(fe_specs)), function(j) {
    bind_rows(lapply(seq_len(nrow(control_specs)), function(k) {
      estimate_one(
        sales, bw_i,
        control_specs$control_set[k],
        control_specs$spec_label[k],
        control_specs$control_cols[[k]],
        fe_specs$fe_spec[j],
        fe_specs$fe_label[j],
        fe_specs$fe_terms[j],
        fe_specs$cluster_var[j],
        "local_linear"
      )
    }))
  }))
})) %>%
  mutate(
    spec_label = factor(spec_label, levels = control_specs$spec_label),
    cluster_label = if_else(cluster == "ward_pair", "Ward-pair cluster", "Segment cluster"),
    pct_change = 100 * (exp(estimate) - 1),
    pct_ci_low = 100 * (exp(estimate - 1.96 * std_error) - 1),
    pct_ci_high = 100 * (exp(estimate + 1.96 * std_error) - 1)
  ) %>%
  arrange(fe_spec, control_set, bandwidth_ft)

write_csv(local_linear_results, "../output/sales_rd_local_linear_path.csv")

cluster_results <- results %>%
  filter(fe_spec %in% c("segment_quarter", "segment_quarter_cluster_ward_pair"))
write_csv(cluster_results, "../output/sales_rd_cluster_robustness.csv")

plot_data_main <- results %>%
  filter(fe_spec == "segment_quarter")

plot <- ggplot(plot_data_main, aes(x = bandwidth_ft, y = pct_change, color = spec_label, group = spec_label)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "gray55", linewidth = 0.4) +
  geom_vline(xintercept = 1000, linetype = "dashed", color = "gray70", linewidth = 0.35) +
  geom_ribbon(aes(ymin = pct_ci_low, ymax = pct_ci_high, fill = spec_label), color = NA, alpha = 0.13) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = bandwidths_ft) +
  scale_color_manual(values = c("No controls" = "#d95f02", "Hedonic controls" = "#1f77b4", "Hedonics + amenities" = "#009E73")) +
  scale_fill_manual(values = c("No controls" = "#d95f02", "Hedonic controls" = "#1f77b4", "Hedonics + amenities" = "#009E73")) +
  labs(
    title = "Sales RD Effect by Bandwidth",
    subtitle = "Flat stricter-side jump, 2006-2022; main fixed effects are segment x quarter",
    x = "Bandwidth (feet)",
    y = "Sale price jump on stricter side (%)",
    color = NULL,
    fill = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("../output/sales_rd_bandwidth_path.pdf", plot, width = 8.5, height = 5.4, dpi = 300, bg = "white")
ggsave("../output/sales_rd_bandwidth_path.png", plot, width = 8.5, height = 5.4, dpi = 220, bg = "white")

plot_cluster <- ggplot(
  cluster_results,
  aes(x = bandwidth_ft, y = pct_change, color = cluster_label, group = cluster_label)
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "gray55", linewidth = 0.4) +
  geom_errorbar(aes(ymin = pct_ci_low, ymax = pct_ci_high), width = 18, linewidth = 0.35, alpha = 0.45) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.7) +
  facet_wrap(~spec_label, nrow = 1) +
  scale_x_continuous(breaks = bandwidths_ft) +
  scale_color_manual(values = c("Segment cluster" = "#1f77b4", "Ward-pair cluster" = "#d95f02")) +
  labs(
    title = "Sales RD Inference Robustness",
    subtitle = "Flat stricter-side jump with segment x quarter fixed effects",
    x = "Bandwidth (feet)",
    y = "Sale price jump on stricter side (%)",
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("../output/sales_rd_cluster_robustness.pdf", plot_cluster, width = 10.5, height = 5.2, dpi = 300, bg = "white")
ggsave("../output/sales_rd_cluster_robustness.png", plot_cluster, width = 10.5, height = 5.2, dpi = 220, bg = "white")

plot_local <- local_linear_results %>%
  filter(fe_spec == "segment_quarter") %>%
  ggplot(aes(x = bandwidth_ft, y = pct_change, color = spec_label, group = spec_label)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "gray55", linewidth = 0.4) +
  geom_vline(xintercept = 1000, linetype = "dashed", color = "gray70", linewidth = 0.35) +
  geom_ribbon(aes(ymin = pct_ci_low, ymax = pct_ci_high, fill = spec_label), color = NA, alpha = 0.13) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = bandwidths_ft) +
  scale_color_manual(values = c("No controls" = "#d95f02", "Hedonic controls" = "#1f77b4", "Hedonics + amenities" = "#009E73")) +
  scale_fill_manual(values = c("No controls" = "#d95f02", "Hedonic controls" = "#1f77b4", "Hedonics + amenities" = "#009E73")) +
  labs(
    title = "Sales RD Local-Linear Robustness",
    subtitle = "Stricter-side jump at the cutoff with separate linear distance slopes; segment x quarter FE",
    x = "Bandwidth (feet)",
    y = "Sale price jump on stricter side (%)",
    color = NULL,
    fill = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("../output/sales_rd_local_linear_path.pdf", plot_local, width = 8.5, height = 5.4, dpi = 300, bg = "white")
ggsave("../output/sales_rd_local_linear_path.png", plot_local, width = 8.5, height = 5.4, dpi = 220, bg = "white")

plot_data_fe <- results %>%
  filter(control_set == "hedonic_amenity")

plot_fe <- ggplot(plot_data_fe, aes(x = bandwidth_ft, y = pct_change, color = fe_label, group = fe_label)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "gray55", linewidth = 0.4) +
  geom_vline(xintercept = 1000, linetype = "dashed", color = "gray70", linewidth = 0.35) +
  geom_errorbar(aes(ymin = pct_ci_low, ymax = pct_ci_high), width = 18, linewidth = 0.35, alpha = 0.45) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.7) +
  scale_x_continuous(breaks = bandwidths_ft) +
  labs(
    title = "Sales RD Effect by Fixed-Effect Spec",
    subtitle = "Hedonic and amenity controls, 2006-2022",
    x = "Bandwidth (feet)",
    y = "Sale price jump on stricter side (%)",
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("../output/sales_rd_bandwidth_path_fe_specs.pdf", plot_fe, width = 9.2, height = 5.4, dpi = 300, bg = "white")
ggsave("../output/sales_rd_bandwidth_path_fe_specs.png", plot_fe, width = 9.2, height = 5.4, dpi = 220, bg = "white")

message("Saved sales RD bandwidth path outputs.")
