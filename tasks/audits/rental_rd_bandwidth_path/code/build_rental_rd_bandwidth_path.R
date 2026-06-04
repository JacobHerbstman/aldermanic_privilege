# Plot RentHub rental RD estimates across bandwidths.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rental_rd_bandwidth_path/code")
# bandwidths_ft <- "100,200,300,400,500,600,700,800,900,1000"
# samples <- "all,clean_location,no_questionable_address"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidths_ft, samples)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <bandwidths_ft> <samples>", call. = FALSE)
}

bandwidths_ft <- as.numeric(strsplit(cli_args[1], ",", fixed = TRUE)[[1]])
samples <- strsplit(cli_args[2], ",", fixed = TRUE)[[1]]
samples <- trimws(samples)

valid_samples <- c("all", "clean_location", "no_questionable_address")
if (length(bandwidths_ft) == 0 || any(!is.finite(bandwidths_ft)) || any(bandwidths_ft <= 0)) {
  stop("bandwidths_ft must be a comma-separated list of positive numbers.", call. = FALSE)
}
if (!all(samples %in% valid_samples)) {
  stop(sprintf("samples must be drawn from: %s.", paste(valid_samples, collapse = ", ")), call. = FALSE)
}

message("=== RentHub Rental RD Bandwidth Path ===")
message(sprintf("Bandwidths: %s ft", paste(bandwidths_ft, collapse = ", ")))
message(sprintf("Samples: %s", paste(samples, collapse = ", ")))

rent <- read_parquet("../input/rent_with_ward_distances.parquet") %>%
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
  stop("Rental input must include segment_id for the RD fixed effects.", call. = FALSE)
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
    year >= 2014,
    year <= 2022,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= max(bandwidths_ft),
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair)
  )

for (flag_col in c(
  "flag_location_questionable",
  "flag_modal_assignment_missing",
  "flag_modal_changes_ward",
  "flag_modal_changes_neighbor_ward",
  "flag_modal_changes_pair",
  "flag_modal_dist_diff_gt100ft"
)) {
  if (!flag_col %in% names(rent)) {
    rent[[flag_col]] <- FALSE
  }
  rent[[flag_col]] <- coalesce(as.logical(rent[[flag_col]]), FALSE)
}

rent <- rent %>%
  mutate(
    flag_clean_location_sample = !flag_location_questionable &
      !flag_modal_assignment_missing &
      !flag_modal_changes_ward &
      !flag_modal_changes_neighbor_ward &
      !flag_modal_changes_pair &
      !flag_modal_dist_diff_gt100ft,
    flag_no_questionable_address_sample = !flag_location_questionable
  )

sample_filter <- function(data, sample_name) {
  if (sample_name == "all") {
    return(data)
  }
  if (sample_name == "clean_location") {
    return(data %>% filter(flag_clean_location_sample))
  }
  if (sample_name == "no_questionable_address") {
    return(data %>% filter(flag_no_questionable_address_sample))
  }
  stop(sprintf("Unhandled sample: %s", sample_name), call. = FALSE)
}

fe_specs <- tribble(
  ~fe_spec, ~fe_label, ~fe_terms,
  "segment_month", "Segment x month", "segment_id^year_month",
  "ward_month", "Ward x month", "ward^year_month",
  "ward_pair_month", "Ward-pair x month", "ward_pair^year_month",
  "segment_year_month", "Segment x year + month", "segment_id^year + year_month"
)

estimate_one <- function(data, bandwidth_ft, sample_name, use_controls, fe_spec, fe_label, fe_terms, cluster_var = "segment_id", rd_spec = "flat") {
  d <- data %>%
    filter(abs(signed_dist_ft) <= bandwidth_ft) %>%
    mutate(running_100ft = signed_dist_ft / 100)

  if (!cluster_var %in% names(d)) {
    stop(sprintf("Missing cluster variable: %s.", cluster_var), call. = FALSE)
  }
  if (!rd_spec %in% c("flat", "local_linear")) {
    stop("rd_spec must be flat or local_linear.", call. = FALSE)
  }
  rd_label <- ifelse(rd_spec == "flat", "Flat jump", "Local linear")

  if (use_controls) {
    d <- d %>%
      filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths))
  }
  if (nrow(d) == 0 || n_distinct(d[[cluster_var]]) < 2 || n_distinct(d$right) < 2) {
    stop(sprintf("Insufficient RD support for %s at %.0fft.", sample_name, bandwidth_ft), call. = FALSE)
  }

  rhs <- if (rd_spec == "local_linear") {
    "right + running_100ft + right:running_100ft"
  } else {
    "right"
  }
  if (use_controls) {
    rhs <- paste(rhs, "log_sqft", "log_beds", "log_baths", sep = " + ")
    if (n_distinct(d$building_type_factor) > 1) {
      rhs <- paste(rhs, "+ building_type_factor")
    }
  }

  model <- feols(
    as.formula(paste0("log(rent_price) ~ ", rhs, " | ", fe_terms)),
    data = d,
    cluster = as.formula(paste0("~", cluster_var))
  )
  ct <- coeftable(model)
  if (!"right" %in% rownames(ct)) {
    stop(sprintf("Model did not estimate right for %s at %.0fft.", sample_name, bandwidth_ft), call. = FALSE)
  }

  removed <- model$obs_selection$obsRemoved
  keep_idx <- if (is.null(removed)) {
    seq_len(nrow(d))
  } else {
    setdiff(seq_len(nrow(d)), abs(as.integer(removed)))
  }
  model_data <- d[keep_idx, , drop = FALSE]

  tibble(
    bandwidth_ft = bandwidth_ft,
    sample = sample_name,
    use_controls = use_controls,
    rd_spec = rd_spec,
    rd_label = rd_label,
    estimate = unname(ct["right", "Estimate"]),
    std_error = unname(ct["right", "Std. Error"]),
    p_value = unname(ct["right", "Pr(>|t|)"]),
    n_obs = model$nobs,
    n_segments = n_distinct(model_data$segment_id),
    n_ward_pairs = n_distinct(model_data$ward_pair),
    fe_spec = fe_spec,
    fe_label = fe_label,
    fixed_effects = fe_label,
    cluster = cluster_var
  )
}

results <- bind_rows(lapply(samples, function(sample_name) {
  sample_data <- sample_filter(rent, sample_name)
  bind_rows(lapply(sort(unique(bandwidths_ft)), function(bw_i) {
    bind_rows(lapply(seq_len(nrow(fe_specs)), function(j) {
      bind_rows(
        estimate_one(
          sample_data, bw_i, sample_name, FALSE,
          fe_specs$fe_spec[j], fe_specs$fe_label[j], fe_specs$fe_terms[j]
        ),
        estimate_one(
          sample_data, bw_i, sample_name, TRUE,
          fe_specs$fe_spec[j], fe_specs$fe_label[j], fe_specs$fe_terms[j]
        )
      )
    }))
  }))
}))

results <- results %>%
  mutate(
    sample_label = recode(
      sample,
      all = "All",
      clean_location = "Clean location",
      no_questionable_address = "No questionable address"
    ),
    spec_label = if_else(use_controls, "Hedonic controls", "No controls"),
    cluster_label = if_else(cluster == "ward_pair", "Ward-pair cluster", "Segment cluster"),
    pct_change = 100 * (exp(estimate) - 1),
    pct_ci_low = 100 * (exp(estimate - 1.96 * std_error) - 1),
    pct_ci_high = 100 * (exp(estimate + 1.96 * std_error) - 1)
  ) %>%
  arrange(sample, use_controls, bandwidth_ft)

write_csv(results, "../output/rental_rd_bandwidth_path.csv")

cluster_results <- bind_rows(lapply(samples, function(sample_name) {
  sample_data <- sample_filter(rent, sample_name)
  bind_rows(lapply(sort(unique(bandwidths_ft)), function(bw_i) {
    bind_rows(lapply(c("segment_id", "ward_pair"), function(cluster_var) {
      bind_rows(
        estimate_one(
          sample_data, bw_i, sample_name, FALSE,
          "segment_month", "Segment x month", "segment_id^year_month",
          cluster_var, "flat"
        ),
        estimate_one(
          sample_data, bw_i, sample_name, TRUE,
          "segment_month", "Segment x month", "segment_id^year_month",
          cluster_var, "flat"
        )
      )
    }))
  }))
})) %>%
  mutate(
    sample_label = recode(
      sample,
      all = "All",
      clean_location = "Clean location",
      no_questionable_address = "No questionable address"
    ),
    spec_label = if_else(use_controls, "Hedonic controls", "No controls"),
    cluster_label = if_else(cluster == "ward_pair", "Ward-pair cluster", "Segment cluster"),
    pct_change = 100 * (exp(estimate) - 1),
    pct_ci_low = 100 * (exp(estimate - 1.96 * std_error) - 1),
    pct_ci_high = 100 * (exp(estimate + 1.96 * std_error) - 1)
  )

write_csv(cluster_results, "../output/rental_rd_cluster_robustness.csv")

local_linear_results <- bind_rows(lapply(samples, function(sample_name) {
  sample_data <- sample_filter(rent, sample_name)
  bind_rows(lapply(sort(unique(bandwidths_ft)), function(bw_i) {
    bind_rows(
      estimate_one(
        sample_data, bw_i, sample_name, FALSE,
        "segment_month", "Segment x month", "segment_id^year_month",
        "segment_id", "local_linear"
      ),
      estimate_one(
        sample_data, bw_i, sample_name, TRUE,
        "segment_month", "Segment x month", "segment_id^year_month",
        "segment_id", "local_linear"
      )
    )
  }))
})) %>%
  mutate(
    sample_label = recode(
      sample,
      all = "All",
      clean_location = "Clean location",
      no_questionable_address = "No questionable address"
    ),
    spec_label = if_else(use_controls, "Hedonic controls", "No controls"),
    cluster_label = "Segment cluster",
    pct_change = 100 * (exp(estimate) - 1),
    pct_ci_low = 100 * (exp(estimate - 1.96 * std_error) - 1),
    pct_ci_high = 100 * (exp(estimate + 1.96 * std_error) - 1)
  )

write_csv(local_linear_results, "../output/rental_rd_local_linear_path.csv")

plot_data_main <- results %>%
  filter(fe_spec == "segment_month")

plot <- ggplot(plot_data_main, aes(x = bandwidth_ft, y = pct_change, color = spec_label, group = spec_label)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "gray55", linewidth = 0.4) +
  geom_ribbon(aes(ymin = pct_ci_low, ymax = pct_ci_high, fill = spec_label), color = NA, alpha = 0.13) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~sample_label, nrow = 1) +
  scale_x_continuous(breaks = bandwidths_ft) +
  scale_color_manual(values = c("Hedonic controls" = "#1f77b4", "No controls" = "#d95f02")) +
  scale_fill_manual(values = c("Hedonic controls" = "#1f77b4", "No controls" = "#d95f02")) +
  labs(
    title = "RentHub Rental RD Effect by Bandwidth",
    subtitle = "Flat stricter-side jump, 2014-2022; dashed line marks the 500ft main bandwidth",
    x = "Bandwidth (feet)",
    y = "Rent jump on stricter side (%)",
    color = NULL,
    fill = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("../output/rental_rd_bandwidth_path.pdf", plot, width = 10.5, height = 5.8, dpi = 300, bg = "white")
ggsave("../output/rental_rd_bandwidth_path.png", plot, width = 10.5, height = 5.8, dpi = 220, bg = "white")

plot_cluster <- ggplot(
  cluster_results,
  aes(x = bandwidth_ft, y = pct_change, color = cluster_label, group = cluster_label)
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "gray55", linewidth = 0.4) +
  geom_errorbar(aes(ymin = pct_ci_low, ymax = pct_ci_high), width = 18, linewidth = 0.35, alpha = 0.45) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.7) +
  facet_grid(sample_label ~ spec_label) +
  scale_x_continuous(breaks = bandwidths_ft) +
  scale_color_manual(values = c("Segment cluster" = "#1f77b4", "Ward-pair cluster" = "#d95f02")) +
  labs(
    title = "RentHub Rental RD Inference Robustness",
    subtitle = "Flat stricter-side jump with segment x month fixed effects",
    x = "Bandwidth (feet)",
    y = "Rent jump on stricter side (%)",
    color = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("../output/rental_rd_cluster_robustness.pdf", plot_cluster, width = 10.8, height = 7.4, dpi = 300, bg = "white")
ggsave("../output/rental_rd_cluster_robustness.png", plot_cluster, width = 10.8, height = 7.4, dpi = 220, bg = "white")

plot_local <- ggplot(local_linear_results, aes(x = bandwidth_ft, y = pct_change, color = spec_label, group = spec_label)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "gray55", linewidth = 0.4) +
  geom_ribbon(aes(ymin = pct_ci_low, ymax = pct_ci_high, fill = spec_label), color = NA, alpha = 0.13) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~sample_label, nrow = 1) +
  scale_x_continuous(breaks = bandwidths_ft) +
  scale_color_manual(values = c("Hedonic controls" = "#1f77b4", "No controls" = "#d95f02")) +
  scale_fill_manual(values = c("Hedonic controls" = "#1f77b4", "No controls" = "#d95f02")) +
  labs(
    title = "RentHub Rental RD Local-Linear Robustness",
    subtitle = "Stricter-side jump at the cutoff with separate linear distance slopes; segment x month FE",
    x = "Bandwidth (feet)",
    y = "Rent jump on stricter side (%)",
    color = NULL,
    fill = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("../output/rental_rd_local_linear_path.pdf", plot_local, width = 10.5, height = 5.8, dpi = 300, bg = "white")
ggsave("../output/rental_rd_local_linear_path.png", plot_local, width = 10.5, height = 5.8, dpi = 220, bg = "white")

plot_data_fe <- results %>%
  filter(use_controls)

plot_fe <- ggplot(plot_data_fe, aes(x = bandwidth_ft, y = pct_change, color = fe_label, group = fe_label)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "gray55", linewidth = 0.4) +
  geom_errorbar(aes(ymin = pct_ci_low, ymax = pct_ci_high), width = 18, linewidth = 0.35, alpha = 0.45) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.7) +
  facet_wrap(~sample_label, nrow = 1) +
  scale_x_continuous(breaks = bandwidths_ft) +
  scale_color_manual(values = c(
    "Segment x month" = "#1f77b4",
    "Ward x month" = "#9467bd",
    "Ward-pair x month" = "#d95f02",
    "Segment x year + month" = "#1b9e77"
  )) +
  labs(
    title = "RentHub Rental RD Effect by Bandwidth and Fixed Effects",
    subtitle = "Hedonic controls included; dashed line marks the 500ft main bandwidth",
    x = "Bandwidth (feet)",
    y = "Rent jump on stricter side (%)",
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("../output/rental_rd_bandwidth_path_fe_specs.pdf", plot_fe, width = 10.8, height = 5.8, dpi = 300, bg = "white")
ggsave("../output/rental_rd_bandwidth_path_fe_specs.png", plot_fe, width = 10.8, height = 5.8, dpi = 220, bg = "white")

message("Saved rental RD bandwidth path.")
