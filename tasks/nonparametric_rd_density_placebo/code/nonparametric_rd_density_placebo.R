source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/nonparametric_rd_density_placebo/code")
# yvar <- "density_far"
# bandwidth_m <- 100
# sample_filter <- "all"
# fe_spec <- "zonegroup_segment_year_additive"
# bins_per_side <- 5
# placebo_shift_m <- -100
# input_csv <- "../input/parcels_with_ward_distances.csv"
# output_pdf <- "../output/nonparametric_rd_density_placebo_log_density_far_100m_all_shift_neg100m.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(yvar, bandwidth_m, sample_filter, fe_spec, bins_per_side, placebo_shift_m, input_csv, output_pdf)
}

if (!length(args) %in% c(8, 9)) {
  stop(
    "FATAL: Script requires args: <yvar> <bandwidth_m> <sample_filter> <fe_spec> <bins_per_side> <placebo_shift_m> <input_csv> <output_pdf>",
    call. = FALSE
  )
}

yvar <- args[1]
bandwidth_m <- as.numeric(args[2])
sample_filter <- args[3]
fe_spec <- args[4]
bins_per_side <- as.integer(args[5])
placebo_shift_m <- as.numeric(args[6])
input_csv <- args[7]
output_pdf <- args[8]

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
if (!is.finite(placebo_shift_m)) {
  stop("placebo_shift_m must be numeric.", call. = FALSE)
}
fe_formula <- dplyr::case_when(
  fe_spec == "zonegroup_segment_year_additive" ~ "zone_group + segment_id + construction_year",
  fe_spec == "zonegroup_pair_year_additive" ~ "zone_group + ward_pair + construction_year",
  fe_spec == "segment_year" ~ "segment_id + construction_year",
  TRUE ~ NA_character_
)

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.1) return("*")
  ""
}

pretty_outcome <- dplyr::case_when(
  yvar == "density_far" ~ "Log(FAR)",
  yvar == "density_dupac" ~ "Log(DUPAC)",
  TRUE ~ yvar
)

raw <- read_csv(input_csv, show_col_types = FALSE) %>%
  ensure_meter_distance_columns()

dat <- raw %>%
  mutate(
    zone_group = zone_group_from_code(zone_code),
    running_distance = signed_distance_m - placebo_shift_m
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    !is.na(ward_pair),
    !is.na(construction_year),
    is.finite(signed_distance_m),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != "",
    abs(running_distance) <= bandwidth_m
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
    side = as.integer(running_distance > 0)
  )

if (nrow(dat) == 0) {
  stop("No observations remain after placebo filters.", call. = FALSE)
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
  stop("Could not recover the placebo cutoff estimate.", call. = FALSE)
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

x_limits <- c(-bandwidth_m, bandwidth_m)
x_label <- sprintf(
  "Distance to placebo cutoff (m; cutoff shifted %+.0fm)",
  placebo_shift_m
)
bw_label <- sprintf("%dm", as.integer(round(bandwidth_m)))
shift_label <- sprintf("%+.0fm", placebo_shift_m)

bins <- bins %>%
  mutate(bin_center_display = bin_center_m)

line_df <- line_df %>%
  mutate(running_distance_display = running_distance)

subtitle_label <- sprintf(
  "Jump = %.3f%s (SE %.3f) | shift=%s | %s | bandwidth=%s | N=%d",
  cutoff_estimate,
  stars(cutoff_p),
  cutoff_se,
  shift_label,
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
  geom_point(
    data = bins,
    aes(x = bin_center_display, y = mean_y, color = factor(side)),
    size = 1.8,
    alpha = 0.95
  ) +
  geom_line(
    data = line_df,
    aes(x = running_distance_display, y = fit, color = factor(side)),
    linewidth = 1.1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
  scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 7)) +
  coord_cartesian(ylim = y_limits) +
  labs(
    title = paste0("Placebo Local-Linear RD: ", pretty_outcome),
    subtitle = subtitle_label,
    x = x_label,
    y = paste("Residualized", pretty_outcome)
  ) +
  theme_bw(base_size = 11)

ggsave(output_pdf, plot = p, width = 8.6, height = 6.0, dpi = 300)

message(sprintf("Built %s", output_pdf))
