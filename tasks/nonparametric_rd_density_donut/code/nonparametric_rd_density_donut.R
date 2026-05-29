# --- Interactive Test Block ---
# setwd("tasks/nonparametric_rd_density_donut/code")
# yvar <- "density_far"
# bandwidth_m <- 152.4
# sample_filter <- "all"
# fe_spec <- "zonegroup_segment_year_additive"
# bins_per_side <- 5
# donut_m <- 7.62

source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(yvar, bandwidth_m, sample_filter, fe_spec, bins_per_side, donut_m)
}

if (length(args) != 6) {
  stop(
    "FATAL: Script requires args: <yvar> <bandwidth_m> <sample_filter> <fe_spec> <bins_per_side> <donut_m>",
    call. = FALSE
  )
}

yvar <- args[1]
bandwidth_m <- as.numeric(args[2])
sample_filter <- args[3]
fe_spec <- args[4]
bins_per_side <- as.integer(args[5])
donut_m <- as.numeric(args[6])

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
if (!is.finite(donut_m) || donut_m < 0 || donut_m >= bandwidth_m) {
  stop("donut_m must be non-negative and strictly smaller than bandwidth_m.", call. = FALSE)
}
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

distance_display <- list(unit = "ft", scale = M_TO_FT)
bw_label <- format_distance_label(bandwidth_m, distance_display)
donut_label <- format_distance_label(donut_m, distance_display)
output_pdf <- sprintf(
  "../output/nonparametric_rd_density_donut_log_%s_%s_%s_donut%s.pdf",
  yvar,
  bw_label,
  sample_filter,
  donut_label
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
    abs(signed_distance_m) <= bandwidth_m,
    abs(signed_distance_m) >= donut_m
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
  stop("No observations remain after donut filters.", call. = FALSE)
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
  stop("Could not recover the donut cutoff estimate.", call. = FALSE)
}

cutoff_estimate <- unname(linear_row[1, "Estimate"])
cutoff_se <- unname(linear_row[1, "Std. Error"])
cutoff_p <- unname(linear_row[1, "Pr(>|t|)"])
cutoff_stars <- dplyr::case_when(
  is.finite(cutoff_p) & cutoff_p <= 0.01 ~ "***",
  is.finite(cutoff_p) & cutoff_p <= 0.05 ~ "**",
  is.finite(cutoff_p) & cutoff_p <= 0.1 ~ "*",
  TRUE ~ ""
)

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
    seq(-bandwidth_m, -donut_m, length.out = 160),
    seq(donut_m, bandwidth_m, length.out = 160)
  )
) %>%
  mutate(
    side = as.integer(running_distance > 0),
    side_label = if_else(side == 1L, "Strict side", "Lenient side")
  )

xmat <- matrix(0, nrow = nrow(line_df), ncol = length(coef_names))
colnames(xmat) <- coef_names
if ("(Intercept)" %in% coef_names) xmat[, "(Intercept)"] <- 1
if ("side" %in% coef_names) xmat[, "side"] <- line_df$side
if ("running_distance" %in% coef_names) xmat[, "running_distance"] <- line_df$running_distance
if ("side:running_distance" %in% coef_names) xmat[, "side:running_distance"] <- line_df$side * line_df$running_distance
if ("running_distance:side" %in% coef_names) xmat[, "running_distance:side"] <- line_df$side * line_df$running_distance

line_df <- line_df %>%
  mutate(side_label = if_else(side == 1L, "Strict side", "Lenient side"))

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
if (!is.finite(y_span) || y_span <= 0) y_span <- 1
y_pad <- max(0.15 * y_span, 0.05)
y_limits <- c(y_min - y_pad, y_max + y_pad)

sample_label <- ifelse(sample_filter == "all", "all construction", "multifamily")

x_limits <- c(-bandwidth_m, bandwidth_m) * distance_display$scale
x_label <- sprintf("Distance to ward boundary (%s)", distance_display$unit)

bins <- bins %>%
  mutate(bin_center_display = bin_center_m * distance_display$scale)

line_df <- line_df %>%
  mutate(running_distance_display = running_distance * distance_display$scale)

subtitle_label <- sprintf(
  "Jump = %.3f%s (SE %.3f) | donut >= %s | %s | bandwidth=%s | N=%d",
  cutoff_estimate,
  cutoff_stars,
  cutoff_se,
  donut_label,
  sample_label,
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
    title = paste0("Donut Local-Linear RD: ", pretty_outcome),
    subtitle = subtitle_label,
    x = x_label,
    y = paste("Residualized", pretty_outcome)
  ) +
  theme_bw(base_size = 11)

ggsave(output_pdf, plot = p, width = 8.6, height = 6.0, dpi = 300)
