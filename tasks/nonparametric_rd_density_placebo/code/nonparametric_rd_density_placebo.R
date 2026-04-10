source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/nonparametric_rd_density_placebo/code")
# yvar <- "density_far"
# bw_ft <- 250
# sample_filter <- "all"
# fe_spec <- "zonegroup_segment_year_additive"
# bins_per_side <- 5
# placebo_shift_ft <- -250
# output_pdf <- "../output/nonparametric_rd_density_placebo_log_density_far_bw250_all_shift-250.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(yvar, bw_ft, sample_filter, fe_spec, bins_per_side, placebo_shift_ft, output_pdf)
}

if (length(args) != 7) {
  stop(
    "FATAL: Script requires args: <yvar> <bw_ft> <sample_filter> <fe_spec> <bins_per_side> <placebo_shift_ft> <output_pdf>",
    call. = FALSE
  )
}

yvar <- args[1]
bw_ft <- as.numeric(args[2])
sample_filter <- args[3]
fe_spec <- args[4]
bins_per_side <- as.integer(args[5])
placebo_shift_ft <- as.numeric(args[6])
output_pdf <- args[7]

if (!yvar %in% c("density_far", "density_dupac")) {
  stop("yvar must be one of: density_far, density_dupac", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive number.", call. = FALSE)
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
if (!is.finite(placebo_shift_ft)) {
  stop("placebo_shift_ft must be numeric.", call. = FALSE)
}

rd_input_path <- Sys.getenv("RD_INPUT_PATH", "../input/parcels_with_ward_distances.csv")

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

raw <- read_csv(rd_input_path, show_col_types = FALSE)

dat <- raw %>%
  mutate(
    zone_group = zone_group_from_code(zone_code),
    running_distance = signed_distance - placebo_shift_ft
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    !is.na(ward_pair),
    !is.na(construction_year),
    is.finite(signed_distance),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != "",
    abs(running_distance) <= bw_ft
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

breaks_ft <- seq(-bw_ft, bw_ft, length.out = 2L * bins_per_side + 1L)
bin_width_ft <- bw_ft / bins_per_side

aug <- aug %>%
  mutate(
    bin_idx = pmin(
      findInterval(running_distance, breaks_ft, rightmost.closed = TRUE, all.inside = TRUE),
      length(breaks_ft) - 1L
    ),
    bin_left_ft = breaks_ft[bin_idx],
    bin_center_ft = bin_left_ft + bin_width_ft / 2,
    side_label = if_else(side == 1L, "Strict side", "Lenient side")
  )

bins <- aug %>%
  group_by(bin_idx, bin_center_ft, side, side_label) %>%
  summarise(
    n = n(),
    mean_y = mean(residualized_outcome, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(bin_center_ft)

coef_names <- names(coef(m_display))
line_df <- tibble(
  running_distance = c(
    seq(-bw_ft, 0, length.out = 200),
    seq(0, bw_ft, length.out = 200)[-1]
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

subtitle_label <- sprintf(
  "Jump = %.3f%s (SE %.3f) | shift=%+.0f ft | %s | bw=%d ft | N=%d",
  cutoff_estimate,
  stars(cutoff_p),
  cutoff_se,
  placebo_shift_ft,
  sample_filter,
  as.integer(bw_ft),
  nobs(m_resid)
)

x_label <- sprintf("Distance to placebo cutoff shifted %+.0f ft", placebo_shift_ft)

p <- ggplot() +
  geom_ribbon(
    data = line_df,
    aes(x = running_distance, ymin = ci_low, ymax = ci_high, fill = factor(side)),
    alpha = 0.16,
    color = NA
  ) +
  geom_point(
    data = bins,
    aes(x = bin_center_ft, y = mean_y, color = factor(side)),
    size = 1.8,
    alpha = 0.95
  ) +
  geom_line(
    data = line_df,
    aes(x = running_distance, y = fit, color = factor(side)),
    linewidth = 1.1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
  scale_x_continuous(limits = c(-bw_ft, bw_ft), breaks = pretty(c(-bw_ft, bw_ft), n = 7)) +
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
