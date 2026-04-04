source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

#   "../output/rd_fe_plot_%s%s_bw%d_%s_%s.pdf",
#   ifelse(use_log, "log_", ""), yvar, bw_ft, sample_filter, fe_spec
# )
# source("spatial_rd_fe.R")

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
# arg order: yvar use_log bw_ft sample fe_spec output_pdf [plot_style] [gap_split]
# sample: "all" (unitscount > 0) | "multifamily" (unitscount > 1)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/spatial_rd_fe/code")
# yvar <- "density_far"
# use_log <- TRUE
# bw_ft <- 500
# sample_filter <- "multifamily"
# fe_spec <- "zonegroup_segment_year_additive"
# output_pdf <- "../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.pdf"
# plot_style <- "slope"
# gap_split <- "all"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(yvar, use_log, bw_ft, sample_filter, fe_spec, output_pdf, plot_style, gap_split)
}

if (length(args) < 6) {
  stop("FATAL: Script requires args: <yvar> <use_log> <bw_ft> <sample> <fe_spec> <output_pdf> [<plot_style>] [<gap_split>]", call. = FALSE)
}

yvar <- args[1]
use_log <- tolower(args[2]) %in% c("true", "t", "1", "yes")
bw_ft <- as.numeric(args[3])
sample_filter <- args[4]
fe_spec <- args[5]
output_pdf <- args[6]
plot_style <- if (length(args) >= 7) tolower(args[7]) else "slope"
gap_split <- if (length(args) >= 8) tolower(args[8]) else "all"

if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive number.", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample must be one of: all, multifamily", call. = FALSE)
}

rd_input_path <- Sys.getenv("RD_INPUT_PATH", "../input/parcels_with_ward_distances.csv")

fe_map <- list(
  pair_x_year = list(fe = "ward_pair^construction_year", use_far = FALSE, need_zone = FALSE, need_segment = FALSE),
  pair_year = list(fe = "ward_pair + construction_year", use_far = FALSE, need_zone = FALSE, need_segment = FALSE),
  zone_pair_year_additive = list(fe = "zone_code + ward_pair + construction_year", use_far = FALSE, need_zone = TRUE, need_segment = FALSE),
  zonegroup_pair_year_additive = list(fe = "zone_group + ward_pair + construction_year", use_far = FALSE, need_zone = TRUE, need_segment = FALSE),
  segment_year = list(fe = "segment_id + construction_year", use_far = FALSE, need_zone = FALSE, need_segment = TRUE),
  zone_segment_year_additive = list(fe = "zone_code + segment_id + construction_year", use_far = FALSE, need_zone = TRUE, need_segment = TRUE),
  zonegroup_segment_year_additive = list(fe = "zone_group + segment_id + construction_year", use_far = FALSE, need_zone = TRUE, need_segment = TRUE),
  pair_x_year_far = list(fe = "ward_pair^construction_year", use_far = TRUE, need_zone = FALSE, need_segment = FALSE),
  pair_year_far = list(fe = "ward_pair + construction_year", use_far = TRUE, need_zone = FALSE, need_segment = FALSE)
)

if (!fe_spec %in% names(fe_map)) {
  stop(sprintf("fe_spec must be one of: %s", paste(names(fe_map), collapse = ", ")), call. = FALSE)
}
if (!plot_style %in% c("slope", "level", "level_rd")) {
  stop("plot_style must be one of: slope, level, level_rd", call. = FALSE)
}
if (!gap_split %in% c("all", "above_median", "below_median", "top_quartile", "bottom_quartile")) {
  stop("gap_split must be one of: all, above_median, below_median, top_quartile, bottom_quartile", call. = FALSE)
}

prune_sample_raw <- tolower(Sys.getenv("PRUNE_SAMPLE", "all"))
if (prune_sample_raw %in% c("all", "false", "f", "0", "no", "off")) {
  prune_sample <- "all"
} else if (prune_sample_raw %in% c("pruned", "true", "t", "1", "yes", "on")) {
  prune_sample <- "pruned"
} else {
  stop("PRUNE_SAMPLE must map to one of: all/false/0 or pruned/true/1", call. = FALSE)
}
confound_flags_path <- Sys.getenv("CONFOUND_FLAGS_PATH", "../input/confounded_pair_era_flags.csv")

cluster_level_raw <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
if (cluster_level_raw %in% c("ward_pair", "wardpair", "pair")) {
  cluster_level <- "ward_pair"
} else if (cluster_level_raw %in% c("segment", "segment_id")) {
  cluster_level <- "segment"
} else {
  stop("CLUSTER_LEVEL must be one of: ward_pair, segment", call. = FALSE)
}
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair

weight_style_raw <- tolower(Sys.getenv("RD_WEIGHT_STYLE", "uniform"))
if (weight_style_raw %in% c("uniform", "none", "unweighted")) {
  weight_style <- "uniform"
} else if (weight_style_raw %in% c("triangular", "triangle", "tri")) {
  weight_style <- "triangular"
} else {
  stop("RD_WEIGHT_STYLE must be one of: uniform, triangular", call. = FALSE)
}

control_style_raw <- tolower(Sys.getenv("RD_CONTROL_STYLE", "baseline"))
if (control_style_raw %in% c("baseline", "controls", "demographic_controls")) {
  control_style <- "baseline"
} else if (control_style_raw %in% c("none", "fe_only", "no_controls", "nocontrols")) {
  control_style <- "none"
} else {
  stop("RD_CONTROL_STYLE must be one of: baseline, none", call. = FALSE)
}

donut_ft_raw <- Sys.getenv("DONUT_FT", "0")
donut_ft <- suppressWarnings(as.numeric(donut_ft_raw))
if (!is.finite(donut_ft) || donut_ft < 0) {
  stop("DONUT_FT must be a non-negative number.", call. = FALSE)
}
if (donut_ft >= bw_ft) {
  stop("DONUT_FT must be strictly smaller than bandwidth.", call. = FALSE)
}

placebo_shift_raw <- Sys.getenv("PLACEBO_SHIFT_FT", "0")
placebo_shift_ft <- suppressWarnings(as.numeric(placebo_shift_raw))
if (!is.finite(placebo_shift_ft)) {
  stop("PLACEBO_SHIFT_FT must be numeric.", call. = FALSE)
}

# 1) Load + sample filters aligned with border-pair FE table spec
message(sprintf("Input: %s", rd_input_path))
raw <- read_csv(rd_input_path, show_col_types = FALSE)

if (!yvar %in% names(raw)) {
  stop(sprintf("yvar '%s' not found in data.", yvar), call. = FALSE)
}

strictness_sd <- sd(raw$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("Could not compute a valid strictness standard deviation.", call. = FALSE)
}

dat <- raw %>%
  mutate(
    strictness_own = strictness_own / strictness_sd,
    strictness_neighbor = strictness_neighbor / strictness_sd,
    zone_group = zone_group_from_code(zone_code)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    !is.na(ward_pair),
    !is.na(construction_year),
    is.finite(signed_distance)
  )

if (prune_sample == "pruned") {
  if (!file.exists(confound_flags_path)) {
    stop(sprintf("Missing confound flags file for pruned run: %s", confound_flags_path), call. = FALSE)
  }

  conf_flags <- read_csv(
    confound_flags_path,
    show_col_types = FALSE,
    col_select = c("ward_pair_id_dash", "era", "drop_confound")
  ) %>%
    transmute(
      pair_dash = normalize_pair_dash(ward_pair_id_dash),
      era = as.character(era),
      keep_pair_era = !as.logical(drop_confound)
    ) %>%
    distinct()

  if (anyNA(conf_flags$pair_dash) || anyNA(conf_flags$era)) {
    stop("Confound flags have invalid pair/era keys.", call. = FALSE)
  }
  if (anyDuplicated(conf_flags[, c("pair_dash", "era")]) > 0) {
    stop("Confound flags contain duplicate pair-era keys.", call. = FALSE)
  }

  dat <- dat %>%
    mutate(
      pair_dash = normalize_pair_dash(ward_pair),
      era = era_from_year(construction_year)
    ) %>%
    left_join(conf_flags, by = c("pair_dash", "era"))

  n_missing <- sum(is.na(dat$keep_pair_era))
  if (n_missing > 0) {
    message(sprintf(
      "Pruned run: %d observations have no pair-era pruning flag and will be dropped.",
      n_missing
    ))
    dat <- dat %>% mutate(keep_pair_era = ifelse(is.na(keep_pair_era), FALSE, keep_pair_era))
  }

  n_before_prune <- nrow(dat)
  dat <- dat %>% filter(keep_pair_era)
  message(sprintf("Observations after pair-era pruning: %d -> %d", n_before_prune, nrow(dat)))
}

if (sample_filter == "all") {
  dat <- dat %>% filter(unitscount > 0 )
} else if (sample_filter == "multifamily") {
  dat <- dat %>% filter(unitscount > 1 )
}
message(sprintf("Observations after sample filter (%s): %d", sample_filter, nrow(dat)))

if (fe_map[[fe_spec]]$need_zone) {
  dat <- dat %>% filter(!is.na(zone_code))
}
if (fe_map[[fe_spec]]$need_segment || cluster_level == "segment") {
  dat <- dat %>% filter(!is.na(segment_id), segment_id != "")
}
if (fe_map[[fe_spec]]$use_far) {
  dat <- dat %>% filter(is.finite(floor_area_ratio))
}

dat <- dat %>%
  mutate(running_distance = signed_distance - placebo_shift_ft) %>%
  filter(abs(running_distance) <= bw_ft, abs(running_distance) >= donut_ft)

if (weight_style == "triangular") {
  dat <- dat %>%
    mutate(plot_weight = pmax(0, 1 - abs(running_distance) / bw_ft))
} else {
  dat <- dat %>%
    mutate(plot_weight = 1)
}

message(sprintf(
  "RD config: FE=%s | cluster=%s | bw=%d | donut>=%.0f | placebo_shift=%+.0f | weights=%s | controls=%s | sample=%s | prune=%s | obs=%d",
  fe_spec, cluster_level, as.integer(bw_ft), donut_ft, placebo_shift_ft, weight_style, control_style, sample_filter, prune_sample, nrow(dat)
))

if (use_log) {
  dat <- dat %>%
    filter(is.finite(.data[[yvar]]), .data[[yvar]] > 0) %>%
    mutate(outcome = log(.data[[yvar]]))
} else {
  dat <- dat %>%
    filter(is.finite(.data[[yvar]])) %>%
    mutate(outcome = .data[[yvar]])
}

pair_count_before_gap_split <- dplyr::n_distinct(dat$ward_pair)
median_gap <- NA_real_
gap_q25 <- NA_real_
gap_q75 <- NA_real_
if (gap_split != "all") {
  pair_gaps <- dat %>%
    filter(is.finite(strictness_own), is.finite(strictness_neighbor)) %>%
    group_by(ward_pair) %>%
    summarise(strictness_gap = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE), .groups = "drop")

  median_gap <- median(pair_gaps$strictness_gap, na.rm = TRUE)
  gap_q25 <- as.numeric(quantile(pair_gaps$strictness_gap, probs = 0.25, na.rm = TRUE, type = 7))
  gap_q75 <- as.numeric(quantile(pair_gaps$strictness_gap, probs = 0.75, na.rm = TRUE, type = 7))
  if (!is.finite(median_gap)) {
    stop("Median strictness gap is not finite; cannot split sample.", call. = FALSE)
  }
  if (!is.finite(gap_q25) || !is.finite(gap_q75)) {
    stop("Gap quartiles are not finite; cannot split sample.", call. = FALSE)
  }

  keep_pairs <- dplyr::case_when(
    gap_split == "above_median" ~ list(pair_gaps %>% filter(strictness_gap >= median_gap) %>% pull(ward_pair)),
    gap_split == "below_median" ~ list(pair_gaps %>% filter(strictness_gap < median_gap) %>% pull(ward_pair)),
    gap_split == "top_quartile" ~ list(pair_gaps %>% filter(strictness_gap >= gap_q75) %>% pull(ward_pair)),
    gap_split == "bottom_quartile" ~ list(pair_gaps %>% filter(strictness_gap <= gap_q25) %>% pull(ward_pair)),
    TRUE ~ list(pair_gaps$ward_pair)
  )[[1]]

  dat <- dat %>% filter(ward_pair %in% keep_pairs)
  message(sprintf(
    "Gap split (%s): median=%.3f | q25=%.3f | q75=%.3f | pairs %d -> %d | obs=%d",
    gap_split, median_gap, gap_q25, gap_q75, pair_count_before_gap_split, dplyr::n_distinct(dat$ward_pair), nrow(dat)
  ))
}
if (nrow(dat) == 0) {
  stop("No observations remain after filtering.", call. = FALSE)
}

controls <- c(
  "share_white_own", "share_black_own", "median_hh_income_own",
  "share_bach_plus_own", "homeownership_rate_own"
)
if (fe_map[[fe_spec]]$use_far) {
  controls <- c(controls, "floor_area_ratio")
}
if (control_style == "none") {
  controls <- character(0)
}

# Keep explicit side variable for discontinuity model after placebo shift.
dat <- dat %>% mutate(side = as.integer(running_distance > 0))

rhs_rd <- paste(c("side", "running_distance", "side:running_distance", controls), collapse = " + ")
fml_rd <- as.formula(sprintf("outcome ~ %s | %s", rhs_rd, fe_map[[fe_spec]]$fe))

if (weight_style == "triangular") {
  m_rd <- feols(fml_rd, data = dat, weights = ~plot_weight, cluster = cluster_formula)
} else {
  m_rd <- feols(fml_rd, data = dat, cluster = cluster_formula)
}
ct_rd <- coeftable(m_rd)

get_coef <- function(ct, names_vec) {
  idx <- which(rownames(ct) %in% names_vec)
  if (length(idx) == 0) return(c(estimate = NA_real_, se = NA_real_, p = NA_real_))
  c(estimate = ct[idx[1], "Estimate"], se = ct[idx[1], "Std. Error"], p = ct[idx[1], "Pr(>|t|)"])
}

b_side <- get_coef(ct_rd, c("side"))
b_x <- get_coef(ct_rd, c("running_distance"))
b_int <- get_coef(ct_rd, c("side:running_distance", "running_distance:side"))
slope_right <- b_x["estimate"] + b_int["estimate"]

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.1) return("*")
  ""
}

weighted_mean_safe <- function(x, w) {
  keep <- is.finite(x) & is.finite(w)
  if (!any(keep) || sum(w[keep], na.rm = TRUE) <= 0) {
    return(NA_real_)
  }
  weighted.mean(x[keep], w[keep], na.rm = TRUE)
}

if (plot_style %in% c("level", "level_rd")) {
  # Residualize on FE + controls only (no side or distance terms).
  rhs_resid <- if (length(controls) == 0) "1" else paste(controls, collapse = " + ")
  fml_resid <- as.formula(sprintf("outcome ~ %s | %s", rhs_resid, fe_map[[fe_spec]]$fe))
  if (weight_style == "triangular") {
    m_resid <- feols(fml_resid, data = dat, weights = ~plot_weight, cluster = cluster_formula)
  } else {
    m_resid <- feols(fml_resid, data = dat, cluster = cluster_formula)
  }

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
  aug <- aug %>% mutate(y_adj = as.numeric(resid(m_resid)))

  if (weight_style == "triangular") {
    m_gap <- feols(y_adj ~ side, data = aug, weights = ~plot_weight, cluster = cluster_formula)
  } else {
    m_gap <- feols(y_adj ~ side, data = aug, cluster = cluster_formula)
  }
  b_side_gap <- get_coef(coeftable(m_gap), c("side"))
  b_side_plot <- if (plot_style == "level") b_side_gap else b_side
  n_obs_plot <- nobs(m_resid)
} else {
  # Keep slope terms visible as in the original RD visualization.
  removed <- m_rd$obs_selection$obsRemoved
  if (is.null(removed)) {
    keep_idx <- seq_len(nrow(dat))
  } else {
    keep_idx <- setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
  }

  aug <- dat[keep_idx, , drop = FALSE]
  if (nrow(aug) != nobs(m_rd)) {
    stop(sprintf("Model/sample alignment failed: kept=%d, nobs=%d", nrow(aug), nobs(m_rd)), call. = FALSE)
  }
  aug$.resid <- as.numeric(resid(m_rd))
  aug <- aug %>%
    mutate(
      xb = b_side["estimate"] * side +
        b_x["estimate"] * running_distance +
        b_int["estimate"] * (side * running_distance),
      y_adj = .resid + xb
    )

  b_side_plot <- b_side
  n_obs_plot <- nobs(m_rd)
}

# Binning for visualization
K <- 30
bin_w <- bw_ft / K
bins <- aug %>%
  mutate(bin_id = floor(running_distance / bin_w),
         bin_center = (bin_id + 0.5) * bin_w) %>%
  group_by(bin_center, side) %>%
  summarise(
    n = n(),
    weight_sum = sum(plot_weight, na.rm = TRUE),
    mean_y = weighted_mean_safe(y_adj, plot_weight),
    se_y = ifelse(weight_style == "triangular", NA_real_, sd(y_adj, na.rm = TRUE) / sqrt(n)),
    lo = mean_y - 1.96 * se_y,
    hi = mean_y + 1.96 * se_y,
    .groups = "drop"
  )

line_df <- if (plot_style == "level") {
  mean_left <- weighted_mean_safe(aug$y_adj[aug$side == 0], aug$plot_weight[aug$side == 0])
  mean_right <- weighted_mean_safe(aug$y_adj[aug$side == 1], aug$plot_weight[aug$side == 1])
  bind_rows(
    tibble(running_distance = c(-bw_ft, 0), side = 0, fit = mean_left),
    tibble(running_distance = c(0, bw_ft), side = 1, fit = mean_right)
  )
} else {
  x_left <- seq(-bw_ft, 0, length.out = 200)
  x_right <- seq(0, bw_ft, length.out = 200)
  bind_rows(
    tibble(
      running_distance = x_left,
      side = 0,
      fit = b_x["estimate"] * x_left
    ),
    tibble(
      running_distance = x_right,
      side = 1,
      fit = b_side["estimate"] + (b_x["estimate"] + b_int["estimate"]) * x_right
    )
  )
}

jump_label <- if (plot_style == "level_rd") {
  sprintf(
    "RD jump = %.3f%s (SE %.3f) | pooled gap = %.3f%s (SE %.3f)",
    b_side["estimate"], stars(b_side["p"]), b_side["se"],
    b_side_gap["estimate"], stars(b_side_gap["p"]), b_side_gap["se"]
  )
} else if (plot_style == "level") {
  sprintf(
    "Gap = %.3f%s (SE %.3f)",
    b_side_plot["estimate"], stars(b_side_plot["p"]), b_side_plot["se"]
  )
} else {
  sprintf(
    "Jump = %.3f%s (SE %.3f)",
    b_side_plot["estimate"], stars(b_side_plot["p"]), b_side_plot["se"]
  )
}

placebo_side_label <- dplyr::case_when(
  placebo_shift_ft > 0 ~ "sample lies inside the original more-stringent side",
  placebo_shift_ft < 0 ~ "sample lies inside the original less-stringent side",
  TRUE ~ "right side is the more-stringent side"
)

gap_split_label <- dplyr::case_when(
  gap_split == "above_median" ~ "above-median gap pairs",
  gap_split == "below_median" ~ "below-median gap pairs",
  gap_split == "top_quartile" ~ "top-quartile gap pairs",
  gap_split == "bottom_quartile" ~ "bottom-quartile gap pairs",
  TRUE ~ "all pairs"
)

control_label <- if (control_style == "none") "FE only" else "FE + controls"

distance_label <- if (placebo_shift_ft == 0) {
  "Running distance (ft) relative to cutoff; right side is the more-stringent side"
} else {
  sprintf(
    "Running distance (ft) relative to placebo cutoff shifted %+.0f ft; %s",
    placebo_shift_ft,
    placebo_side_label
  )
}

subtitle_label <- if (placebo_shift_ft == 0) {
  sprintf(
    "%s | %s | %s | %s weights | bw=%d ft | N=%d",
    jump_label,
    gap_split_label,
    control_label,
    weight_style,
    as.integer(bw_ft),
    n_obs_plot
  )
} else {
  sprintf(
    "%s | %s | %s | placebo shift=%+.0f ft | %s weights | bw=%d ft | N=%d",
    jump_label,
    gap_split_label,
    control_label,
    placebo_shift_ft,
    weight_style,
    as.integer(bw_ft),
    n_obs_plot
  )
}

outcome_label <- c(
  density_far = "Floor-Area Ratio (FAR)",
  density_dupac = "Dwelling Units Per Acre (DUPAC)",
  unitscount = "Units"
)

ylab <- ifelse(yvar %in% names(outcome_label), outcome_label[[yvar]], yvar)
if (use_log) ylab <- paste0("Log(", ylab, ")")

p <- ggplot() +
  geom_point(
    data = bins,
    aes(x = bin_center, y = mean_y, color = factor(side)),
    size = 1.6, alpha = 0.9
  ) +
  geom_line(
    data = line_df,
    aes(x = running_distance, y = fit, color = factor(side)),
    linewidth = 1.1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
  labs(
    title = paste0(
      if (plot_style == "level") {
        if (control_style == "none") "Spatial RD (FE-Only Levels): " else "Spatial RD (FE-Adjusted Levels): "
      } else if (plot_style == "level_rd") {
        if (control_style == "none") "Spatial RD (FE-Only Levels + RD Lines): " else "Spatial RD (FE-Adjusted Levels + RD Lines): "
      } else {
        if (control_style == "none") "Spatial RD (FE-Only): " else "Spatial RD (FE-Adjusted): "
      },
      ylab
    ),
    subtitle = subtitle_label,
    x = distance_label,
    y = ylab
  ) +
  theme_bw(base_size = 11)

ggsave(output_pdf, p, width = 8.6, height = 6.0, dpi = 300)

# Save companion diagnostics
out_csv <- sub("\\.pdf$", "_bins.csv", output_pdf)
out_meta <- sub("\\.pdf$", "_meta.csv", output_pdf)
out_fit <- sub("\\.pdf$", "_fit.csv", output_pdf)

write_csv(bins, out_csv)
write_csv(line_df, out_fit)
write_csv(
  tibble(
    yvar = yvar,
    use_log = use_log,
    bw_ft = bw_ft,
    sample_filter = sample_filter,
    donut_ft = donut_ft,
    placebo_shift_ft = placebo_shift_ft,
    prune_sample = prune_sample,
    cluster_level = cluster_level,
    input_path = rd_input_path,
    output_pdf = output_pdf,
    fe_spec = fe_spec,
    plot_style = plot_style,
    weight_style = weight_style,
    control_style = control_style,
    gap_split = gap_split,
    gap_split_label = gap_split_label,
    median_gap = median_gap,
    gap_q25 = gap_q25,
    gap_q75 = gap_q75,
    n_pairs_before_gap_split = pair_count_before_gap_split,
    n_obs = n_obs_plot,
    n_pairs = dplyr::n_distinct(aug$ward_pair),
    n_left = sum(aug$side == 0, na.rm = TRUE),
    n_right = sum(aug$side == 1, na.rm = TRUE),
    weight_sum = sum(aug$plot_weight, na.rm = TRUE),
    jump_estimate = b_side_plot["estimate"],
    jump_se = b_side_plot["se"],
    jump_p = b_side_plot["p"],
    rd_jump_estimate = b_side["estimate"],
    rd_jump_se = b_side["se"],
    rd_jump_p = b_side["p"],
    slope_left = b_x["estimate"],
    slope_right = slope_right,
    slope_diff_right_minus_left = b_int["estimate"]
  ),
  out_meta
)

cat("Saved:\n")
cat(" -", output_pdf, "\n")
cat(" -", out_csv, "\n")
cat(" -", out_meta, "\n")
cat(" -", out_fit, "\n")
