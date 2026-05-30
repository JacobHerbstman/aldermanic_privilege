# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/nonparametric_rd_density_gap_split/code")
# yvar <- "density_far"
# bandwidth_m <- 152.4
# sample_filter <- "all"
# fe_spec <- "zonegroup_segment_year_additive"
# bins_per_side <- 5
# gap_split <- "above_median"

source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(yvar, bandwidth_m, sample_filter, fe_spec, bins_per_side, gap_split)
}

if (length(cli_args) != 6) {
  stop(
    "Usage: Rscript nonparametric_rd_density_gap_split.R <yvar> <bandwidth_m> <sample_filter> <fe_spec> <bins_per_side> <gap_split>",
    call. = FALSE
  )
}

yvar <- cli_args[1]
bandwidth_m <- as.numeric(cli_args[2])
sample_filter <- cli_args[3]
fe_spec <- cli_args[4]
bins_per_side <- as.integer(cli_args[5])
gap_split <- cli_args[6]

if (!yvar %in% c("density_far", "density_dupac")) {
  stop("yvar must be one of: density_far, density_dupac", call. = FALSE)
}
if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be a positive number.", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample_filter must be one of: all, multifamily", call. = FALSE)
}
if (!fe_spec %in% c("zonegroup_segment_year_additive", "zonegroup_pair_year_additive", "segment_year")) {
  stop("fe_spec must be one of: zonegroup_segment_year_additive, zonegroup_pair_year_additive, segment_year", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side < 2) {
  stop("bins_per_side must be an integer >= 2.", call. = FALSE)
}
if (!gap_split %in% c("above_median", "below_median")) {
  stop("gap_split must be one of: above_median, below_median", call. = FALSE)
}

distance_display <- distance_display_config()
bw_label <- format_distance_label(bandwidth_m, distance_display)

fe_formula <- dplyr::case_when(
  fe_spec == "zonegroup_segment_year_additive" ~ "zone_group + segment_id + construction_year",
  fe_spec == "zonegroup_pair_year_additive" ~ "zone_group + ward_pair + construction_year",
  fe_spec == "segment_year" ~ "segment_id + construction_year",
  TRUE ~ NA_character_
)

pretty_outcome <- dplyr::case_when(
  yvar == "density_far" ~ "Log(FAR)",
  yvar == "density_dupac" ~ "Log(DUPAC)",
  TRUE ~ yvar
)

gap_label <- dplyr::case_when(
  gap_split == "above_median" ~ "above-median gap pairs",
  gap_split == "below_median" ~ "below-median gap pairs",
  TRUE ~ gap_split
)

raw <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  ensure_meter_distance_columns()

dat <- raw %>%
  mutate(zone_group = zone_group_from_code(zone_code)) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    !is.na(ward_pair),
    !is.na(construction_year),
    is.finite(signed_distance_m),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != "",
    abs(signed_distance_m) <= bandwidth_m
  )

if (sample_filter == "all") {
  dat <- dat %>% filter(unitscount > 0)
} else {
  dat <- dat %>% filter(unitscount > 1)
}

dat <- dat %>%
  filter(is.finite(.data[[yvar]]), .data[[yvar]] > 0) %>%
  mutate(
    outcome = log(.data[[yvar]]),
    running_distance = signed_distance_m,
    side = as.integer(running_distance > 0)
  )

if (nrow(dat) == 0) {
  stop("No observations remain after baseline filters.", call. = FALSE)
}

pair_gaps <- dat %>%
  filter(is.finite(strictness_own), is.finite(strictness_neighbor)) %>%
  group_by(ward_pair) %>%
  summarise(
    strictness_gap = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE),
    .groups = "drop"
  )

median_gap <- median(pair_gaps$strictness_gap, na.rm = TRUE)
if (!is.finite(median_gap)) {
  stop("Could not compute a valid median strictness gap.", call. = FALSE)
}

keep_pairs <- if (gap_split == "above_median") {
  pair_gaps %>% filter(strictness_gap >= median_gap) %>% pull(ward_pair)
} else {
  pair_gaps %>% filter(strictness_gap < median_gap) %>% pull(ward_pair)
}

dat <- dat %>% filter(ward_pair %in% keep_pairs)

if (nrow(dat) == 0) {
  stop("No observations remain after applying the median-gap split.", call. = FALSE)
}

controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

fml_resid <- as.formula(sprintf(
  "outcome ~ %s | %s",
  paste(controls, collapse = " + "),
  fe_formula
))
m_resid <- feols(fml_resid, data = dat)

removed <- m_resid$obs_selection$obsRemoved
if (is.null(removed)) {
  keep_idx <- seq_len(nrow(dat))
} else {
  keep_idx <- setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
}

aug <- dat[keep_idx, , drop = FALSE] %>%
  mutate(residualized_outcome = as.numeric(resid(m_resid)))

if (nrow(aug) != nobs(m_resid)) {
  stop("Residualized sample alignment failed.", call. = FALSE)
}

fml_linear <- as.formula(sprintf(
  "outcome ~ side * running_distance + %s | %s",
  paste(controls, collapse = " + "),
  fe_formula
))
m_linear <- feols(fml_linear, data = aug, cluster = ~ward_pair)

linear_row <- coeftable(m_linear)[rownames(coeftable(m_linear)) %in% "side", , drop = FALSE]
if (nrow(linear_row) != 1) {
  stop("Could not recover the local-linear cutoff estimate.", call. = FALSE)
}

cutoff_estimate <- unname(linear_row[1, "Estimate"])
cutoff_se <- unname(linear_row[1, "Std. Error"])
cutoff_p <- unname(linear_row[1, "Pr(>|t|)"])

m_display <- feols(
  residualized_outcome ~ side * running_distance,
  data = aug,
  cluster = ~ward_pair
)

breaks_m <- seq(-bandwidth_m, bandwidth_m, length.out = 2L * bins_per_side + 1L)
bin_width_m <- bandwidth_m / bins_per_side

aug <- aug %>%
  mutate(
    bin_idx = pmin(
      findInterval(running_distance, breaks_m, rightmost.closed = TRUE, all.inside = TRUE),
      length(breaks_m) - 1L
    ),
    bin_left_m = breaks_m[bin_idx],
    bin_center_m = bin_left_m + bin_width_m / 2,
    side_label = if_else(side == 1L, "Strict side", "Lenient side")
  )

bins <- aug %>%
  group_by(bin_idx, bin_center_m, side, side_label) %>%
  summarise(
    n = n(),
    mean_y = mean(residualized_outcome, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(bin_center_m)

coef_names <- names(coef(m_display))
line_df <- tibble(
  running_distance = c(
    seq(-bandwidth_m, 0, length.out = 200),
    seq(0, bandwidth_m, length.out = 200)[-1]
  )
) %>%
  mutate(
    side = as.integer(running_distance > 0),
    side_label = if_else(side == 1L, "Strict side", "Lenient side")
  )

xmat <- model.matrix(~ side * running_distance, data = line_df)
missing_coef_names <- setdiff(coef_names, colnames(xmat))
if (length(missing_coef_names) > 0) {
  stop("Prediction design matrix does not match fitted model.", call. = FALSE)
}
xmat <- xmat[, coef_names, drop = FALSE]

line_vcov <- vcov(m_display)
line_crit <- qt(0.975, df = max(n_distinct(aug$ward_pair) - 1, 1))

line_df <- line_df %>%
  mutate(
    fit = as.numeric(xmat %*% coef(m_display)),
    fit_se = sqrt(pmax(rowSums((xmat %*% line_vcov) * xmat), 0)),
    ci_low = fit - line_crit * fit_se,
    ci_high = fit + line_crit * fit_se
  )

y_min <- min(c(bins$mean_y, line_df$ci_low), na.rm = TRUE)
y_max <- max(c(bins$mean_y, line_df$ci_high), na.rm = TRUE)
y_span <- y_max - y_min
if (!is.finite(y_span) || y_span <= 0) {
  y_span <- 1
}
y_pad <- max(0.15 * y_span, 0.05)
y_limits <- c(y_min - y_pad, y_max + y_pad)

sample_label <- ifelse(sample_filter == "all", "all construction", "multifamily")

x_limits <- c(-bandwidth_m, bandwidth_m) * distance_display$scale
x_label <- sprintf("Distance to ward boundary (%s)", distance_display$unit)

bins <- bins %>%
  mutate(bin_center_display = bin_center_m * distance_display$scale)

line_df <- line_df %>%
  mutate(running_distance_display = running_distance * distance_display$scale)

cutoff_stars <- case_when(
  !is.finite(cutoff_p) ~ "",
  cutoff_p <= 0.01 ~ "***",
  cutoff_p <= 0.05 ~ "**",
  cutoff_p <= 0.10 ~ "*",
  TRUE ~ ""
)

subtitle_label <- sprintf(
  "Jump = %.3f%s (SE %.3f) | %s | %s | bandwidth=%s | N=%d",
  cutoff_estimate,
  cutoff_stars,
  cutoff_se,
  sample_label,
  gap_label,
  bw_label,
  nobs(m_resid)
)

p <- ggplot() +
  geom_ribbon(
    data = line_df,
    aes(x = running_distance_display, ymin = ci_low, ymax = ci_high, fill = factor(side)),
    alpha = 0.16,
    color = NA
  ) +
  geom_line(
    data = line_df,
    aes(x = running_distance_display, y = fit, color = factor(side)),
    linewidth = 1.1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_point(
    data = bins,
    aes(x = bin_center_display, y = mean_y, fill = factor(side)),
    shape = 21,
    color = "white",
    stroke = 0.45,
    size = 2.35,
    alpha = 0.98
  ) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
  scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 7)) +
  coord_cartesian(ylim = y_limits) +
  labs(
    title = paste0("Local-Linear RD: ", pretty_outcome),
    subtitle = subtitle_label,
    x = x_label,
    y = paste("Residualized", pretty_outcome)
  ) +
  theme_bw(base_size = 11)

ggsave(
  sprintf(
    "../output/nonparametric_rd_density_gap_split_log_%s_%s_%s_%s.pdf",
    yvar,
    bw_label,
    sample_filter,
    gap_split
  ),
  plot = p,
  width = 8.6,
  height = 6.0,
  dpi = 300
)
