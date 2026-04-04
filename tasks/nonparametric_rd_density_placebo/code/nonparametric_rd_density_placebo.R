source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/nonparametric_rd_density_placebo/code")
# yvar <- "density_far"
# use_log <- TRUE
# bw_ft <- 500
# sample_filter <- "multifamily"
# gap_split <- "all"
# placebo_shift_ft <- -500
# output_pdf <- "../output/placebo_nonparametric_rd_log_density_far_bw500_multifamily_baseline_fe_shift-500.pdf"
# axis_units <- "feet"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(yvar, use_log, bw_ft, sample_filter, gap_split, placebo_shift_ft, output_pdf, axis_units)
}

if (length(args) != 8) {
  stop(
    "FATAL: Script requires args: <yvar> <use_log> <bw_ft> <sample_filter> <gap_split> <placebo_shift_ft> <output_pdf> <axis_units>",
    call. = FALSE
  )
}

yvar <- args[1]
use_log <- tolower(args[2]) %in% c("true", "t", "1", "yes")
bw_ft <- as.numeric(args[3])
sample_filter <- args[4]
gap_split <- tolower(args[5])
placebo_shift_ft <- as.numeric(args[6])
output_pdf <- args[7]
axis_units <- tolower(args[8])

if (!yvar %in% c("density_far", "density_dupac")) {
  stop("yvar must be one of: density_far, density_dupac", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive number.", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample_filter must be one of: all, multifamily", call. = FALSE)
}
if (!gap_split %in% c("all", "above_median", "below_median")) {
  stop("gap_split must be one of: all, above_median, below_median", call. = FALSE)
}
if (!is.finite(placebo_shift_ft)) {
  stop("placebo_shift_ft must be numeric.", call. = FALSE)
}
if (!axis_units %in% c("feet", "meters")) {
  stop("axis_units must be one of: feet, meters", call. = FALSE)
}

rd_input_path <- Sys.getenv("RD_INPUT_PATH", "../input/parcels_with_ward_distances.csv")

message(sprintf("Input: %s", rd_input_path))

raw <- read_csv(rd_input_path, show_col_types = FALSE)

if (!yvar %in% names(raw)) {
  stop(sprintf("Outcome '%s' not found in input data.", yvar), call. = FALSE)
}

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
  mutate(side = as.integer(running_distance > 0))

if (use_log) {
  dat <- dat %>%
    filter(is.finite(.data[[yvar]]), .data[[yvar]] > 0) %>%
    mutate(outcome = log(.data[[yvar]]))
} else {
  dat <- dat %>%
    filter(is.finite(.data[[yvar]])) %>%
    mutate(outcome = .data[[yvar]])
}

if (nrow(dat) == 0) {
  stop("No observations remain after placebo filters.", call. = FALSE)
}

if (gap_split != "all") {
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
}

if (nrow(dat) == 0) {
  stop("No observations remain after placebo gap split.", call. = FALSE)
}

controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

fml_resid <- as.formula(paste(
  "outcome ~",
  paste(controls, collapse = " + "),
  "| zone_group + segment_id + construction_year"
))

m_resid <- feols(fml_resid, data = dat)

removed <- m_resid$obs_selection$obsRemoved
if (is.null(removed)) {
  keep_idx <- seq_len(nrow(dat))
} else {
  keep_idx <- setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
}

aug <- dat[keep_idx, , drop = FALSE]
if (nrow(aug) != nobs(m_resid)) {
  stop(sprintf("Model/sample alignment failed: kept=%d, nobs=%d", nrow(aug), nobs(m_resid)), call. = FALSE)
}

aug <- aug %>%
  mutate(residualized_outcome = as.numeric(resid(m_resid)))

m_gap <- feols(residualized_outcome ~ side, data = aug, cluster = ~ward_pair)
ct_gap <- coeftable(m_gap)
gap_row <- ct_gap[rownames(ct_gap) %in% "side", , drop = FALSE]

if (nrow(gap_row) != 1) {
  stop("Could not recover pooled placebo side-gap estimate.", call. = FALSE)
}

gap_estimate <- unname(gap_row[1, "Estimate"])
gap_se <- unname(gap_row[1, "Std. Error"])
gap_p <- unname(gap_row[1, "Pr(>|t|)"])

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.1) return("*")
  ""
}

bins_per_side <- 30L
bin_width_ft <- bw_ft / bins_per_side
breaks_ft <- seq(-bw_ft, bw_ft, length.out = 2L * bins_per_side + 1L)

aug <- aug %>%
  mutate(
    bin_idx = pmin(
      findInterval(running_distance, breaks_ft, rightmost.closed = TRUE, all.inside = TRUE),
      length(breaks_ft) - 1L
    ),
    bin_left_ft = breaks_ft[bin_idx],
    bin_center_ft = bin_left_ft + bin_width_ft / 2
  )

bins <- aug %>%
  group_by(bin_idx, bin_center_ft) %>%
  summarise(
    n = n(),
    mean_y = mean(residualized_outcome, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(bin_center_ft)

if (nrow(bins) == 0) {
  stop("No populated placebo bins available for plotting.", call. = FALSE)
}

if (axis_units == "meters") {
  bins <- bins %>% mutate(bin_center_display = bin_center_ft * 0.3048)
  x_limits <- c(-bw_ft, bw_ft) * 0.3048
  x_label <- "Distance to placebo cutoff (meters)"
  bw_label <- sprintf("%d m", as.integer(round(bw_ft * 0.3048)))
} else {
  bins <- bins %>% mutate(bin_center_display = bin_center_ft)
  x_limits <- c(-bw_ft, bw_ft)
  x_label <- "Distance to placebo cutoff (ft)"
  bw_label <- sprintf("%d ft", as.integer(bw_ft))
}

pretty_outcome <- dplyr::case_when(
  yvar == "density_far" ~ "Log(FAR)",
  yvar == "density_dupac" ~ "Log(DUPAC)",
  TRUE ~ yvar
)
if (!use_log) {
  pretty_outcome <- dplyr::case_when(
    yvar == "density_far" ~ "FAR",
    yvar == "density_dupac" ~ "DUPAC",
    TRUE ~ yvar
  )
}

gap_label <- dplyr::case_when(
  gap_split == "above_median" ~ "above-median gap pairs",
  gap_split == "below_median" ~ "below-median gap pairs",
  TRUE ~ "all pairs"
)

placebo_side_label <- dplyr::case_when(
  placebo_shift_ft > 0 ~ "placebo cutoff lies inside the original more-stringent side",
  placebo_shift_ft < 0 ~ "placebo cutoff lies inside the original less-stringent side",
  TRUE ~ "placebo cutoff equals the original boundary"
)

subtitle_bits <- c(
  sprintf("Gap = %.3f%s (SE %.3f)", gap_estimate, stars(gap_p), gap_se),
  sprintf("shift = %+.0f ft", placebo_shift_ft),
  placebo_side_label,
  gap_label,
  paste0("bw = ", bw_label),
  "30 bins/side",
  paste0("N = ", nobs(m_resid))
)

p <- ggplot(bins, aes(x = bin_center_display, y = mean_y)) +
  geom_point(color = "#2f5d8a", size = 2.1, alpha = 0.95) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#6e6e6e", linewidth = 0.6) +
  scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 7)) +
  labs(
    title = paste0("Placebo ", pretty_outcome),
    subtitle = paste(subtitle_bits[!is.na(subtitle_bits) & nzchar(subtitle_bits)], collapse = " | "),
    x = x_label,
    y = paste("Residualized", pretty_outcome)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#d8dce3", linewidth = 0.4),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

ggsave(output_pdf, plot = p, width = 7.4, height = 4.9, dpi = 300)

write_csv(
  bins,
  sub("\\.pdf$", "_bins.csv", output_pdf)
)

write_csv(
  tibble(
    yvar = yvar,
    use_log = use_log,
    bw_ft = bw_ft,
    sample_filter = sample_filter,
    gap_split = gap_split,
    placebo_shift_ft = placebo_shift_ft,
    input_path = rd_input_path,
    output_pdf = output_pdf,
    axis_units = axis_units,
    n_obs = nobs(m_resid),
    n_pairs = n_distinct(aug$ward_pair),
    n_left = sum(aug$side == 0, na.rm = TRUE),
    n_right = sum(aug$side == 1, na.rm = TRUE),
    gap_estimate = gap_estimate,
    gap_se = gap_se,
    gap_p = gap_p
  ),
  sub("\\.pdf$", "_meta.csv", output_pdf)
)

message(sprintf(
  "Built %s | sample=%s | gap_split=%s | y=%s | bw=%d | shift=%+.0f | axis=%s | N=%d",
  output_pdf, sample_filter, gap_split, yvar, as.integer(bw_ft), placebo_shift_ft, axis_units, nobs(m_resid)
))
