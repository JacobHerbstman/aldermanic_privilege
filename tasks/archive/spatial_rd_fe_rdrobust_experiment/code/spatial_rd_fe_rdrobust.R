source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/spatial_rd_fe/code")
# yvar <- "density_far"
# use_log <- TRUE
# bw_ft <- 500
# sample_filter <- "multifamily"
# fe_spec <- "zonegroup_segment_year_additive"
# output_csv <- "../output/rdrobust_fe_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(yvar, use_log, bw_ft, sample_filter, fe_spec, output_csv)
}

if (length(args) != 6) {
  stop(
    "FATAL: Script requires args: <yvar> <use_log> <bw_ft> <sample> <fe_spec> <output_csv>",
    call. = FALSE
  )
}

yvar <- args[1]
use_log <- tolower(args[2]) %in% c("true", "t", "1", "yes")
bw_ft <- as.numeric(args[3])
sample_filter <- args[4]
fe_spec <- args[5]
output_csv <- args[6]
output_pdf <- sub("\\.csv$", "_plot.pdf", output_csv)
output_png <- sub("\\.csv$", "_plot.png", output_csv)
output_covs_rdplot_pdf <- sub("\\.csv$", "_covs_rdplot.pdf", output_csv)
output_covs_rdplot_png <- sub("\\.csv$", "_covs_rdplot.png", output_csv)

if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive number.", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample must be one of: all, multifamily", call. = FALSE)
}

rd_input_path <- Sys.getenv("RD_INPUT_PATH", "../input/parcels_with_ward_distances.csv")

fe_map <- list(
  pair_x_year = list(
    fe = "ward_pair^construction_year",
    cov_terms = "factor(ward_pair):factor(construction_year)",
    use_far = FALSE,
    need_zone = FALSE,
    need_segment = FALSE
  ),
  pair_year = list(
    fe = "ward_pair + construction_year",
    cov_terms = "factor(ward_pair) + factor(construction_year)",
    use_far = FALSE,
    need_zone = FALSE,
    need_segment = FALSE
  ),
  zone_pair_year_additive = list(
    fe = "zone_code + ward_pair + construction_year",
    cov_terms = "factor(zone_code) + factor(ward_pair) + factor(construction_year)",
    use_far = FALSE,
    need_zone = TRUE,
    need_segment = FALSE
  ),
  zonegroup_pair_year_additive = list(
    fe = "zone_group + ward_pair + construction_year",
    cov_terms = "factor(zone_group) + factor(ward_pair) + factor(construction_year)",
    use_far = FALSE,
    need_zone = TRUE,
    need_segment = FALSE
  ),
  segment_year = list(
    fe = "segment_id + construction_year",
    cov_terms = "factor(segment_id) + factor(construction_year)",
    use_far = FALSE,
    need_zone = FALSE,
    need_segment = TRUE
  ),
  zone_segment_year_additive = list(
    fe = "zone_code + segment_id + construction_year",
    cov_terms = "factor(zone_code) + factor(segment_id) + factor(construction_year)",
    use_far = FALSE,
    need_zone = TRUE,
    need_segment = TRUE
  ),
  zonegroup_segment_year_additive = list(
    fe = "zone_group + segment_id + construction_year",
    cov_terms = "factor(zone_group) + factor(segment_id) + factor(construction_year)",
    use_far = FALSE,
    need_zone = TRUE,
    need_segment = TRUE
  ),
  pair_x_year_far = list(
    fe = "ward_pair^construction_year",
    cov_terms = "factor(ward_pair):factor(construction_year)",
    use_far = TRUE,
    need_zone = FALSE,
    need_segment = FALSE
  ),
  pair_year_far = list(
    fe = "ward_pair + construction_year",
    cov_terms = "factor(ward_pair) + factor(construction_year)",
    use_far = TRUE,
    need_zone = FALSE,
    need_segment = FALSE
  )
)

if (!fe_spec %in% names(fe_map)) {
  stop(sprintf("fe_spec must be one of: %s", paste(names(fe_map), collapse = ", ")), call. = FALSE)
}

cluster_level_raw <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
if (cluster_level_raw %in% c("ward_pair", "wardpair", "pair")) {
  cluster_level <- "ward_pair"
} else if (cluster_level_raw %in% c("segment", "segment_id")) {
  cluster_level <- "segment"
} else {
  stop("CLUSTER_LEVEL must be one of: ward_pair, segment", call. = FALSE)
}
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair

control_style_raw <- tolower(Sys.getenv("RD_CONTROL_STYLE", "baseline"))
if (control_style_raw %in% c("baseline", "controls", "demographic_controls")) {
  control_style <- "baseline"
} else if (control_style_raw %in% c("none", "fe_only", "no_controls", "nocontrols")) {
  control_style <- "none"
} else {
  stop("RD_CONTROL_STYLE must be one of: baseline, none", call. = FALSE)
}

rdrobust_kernel <- tolower(Sys.getenv("RDROBUST_KERNEL", "triangular"))
if (!rdrobust_kernel %in% c("triangular", "uniform", "epanechnikov")) {
  stop("RDROBUST_KERNEL must be one of: triangular, uniform, epanechnikov", call. = FALSE)
}

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

if (sample_filter == "all") {
  dat <- dat %>% filter(unitscount > 0)
} else if (sample_filter == "multifamily") {
  dat <- dat %>% filter(unitscount > 1)
}

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
  mutate(running_distance = signed_distance) %>%
  filter(abs(running_distance) <= bw_ft)

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

message(sprintf(
  "RD robust config: FE=%s | cluster=%s | bw=%d | kernel=%s | controls=%s | sample=%s | obs=%d",
  fe_spec, cluster_level, as.integer(bw_ft), rdrobust_kernel, control_style, sample_filter, nrow(dat)
))

rhs_resid <- if (length(controls) == 0) "1" else paste(controls, collapse = " + ")
fml_resid <- as.formula(sprintf("outcome ~ %s | %s", rhs_resid, fe_map[[fe_spec]]$fe))
m_resid <- feols(fml_resid, data = dat, cluster = cluster_formula)

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
  mutate(
    outcome_resid = as.numeric(resid(m_resid)),
    across(c(ward_pair, segment_id, construction_year, zone_code, zone_group), as.factor)
  )

cluster_vec <- if (cluster_level == "segment") aug$segment_id else aug$ward_pair

run_rdrobust_safe <- function(method, y, covs = NULL) {
  warnings <- character(0)
  fit <- withCallingHandlers(
    tryCatch(
      rdrobust::rdrobust(
        y = y,
        x = aug$running_distance,
        c = 0,
        p = 1,
        h = bw_ft,
        kernel = rdrobust_kernel,
        cluster = cluster_vec,
        covs = covs,
        covs_drop = TRUE
      ),
      error = function(e) e
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  n_left <- sum(aug$running_distance < 0, na.rm = TRUE)
  n_right <- sum(aug$running_distance >= 0, na.rm = TRUE)
  covariate_cols <- if (is.null(covs)) 0L else ncol(covs)

  if (inherits(fit, "error")) {
    return(tibble(
      method = method,
      status = "error",
      note = conditionMessage(fit),
      n_obs = nrow(aug),
      n_left = n_left,
      n_right = n_right,
      n_pairs = dplyr::n_distinct(aug$ward_pair),
      n_clusters = dplyr::n_distinct(cluster_vec),
      covariate_cols = covariate_cols,
      h_left = NA_real_,
      h_right = NA_real_,
      b_left = NA_real_,
      b_right = NA_real_,
      conventional_estimate = NA_real_,
      conventional_se = NA_real_,
      conventional_p = NA_real_,
      bias_corrected_estimate = NA_real_,
      bias_corrected_se = NA_real_,
      bias_corrected_p = NA_real_,
      robust_estimate = NA_real_,
      robust_se = NA_real_,
      robust_p = NA_real_
    ))
  }

  tibble(
    method = method,
    status = if (length(warnings) == 0) "ok" else "warning",
    note = paste(unique(warnings), collapse = " | "),
    n_obs = nrow(aug),
    n_left = n_left,
    n_right = n_right,
    n_pairs = dplyr::n_distinct(aug$ward_pair),
    n_clusters = dplyr::n_distinct(cluster_vec),
    covariate_cols = covariate_cols,
    h_left = as.numeric(fit$bws["h", "left"]),
    h_right = as.numeric(fit$bws["h", "right"]),
    b_left = as.numeric(fit$bws["b", "left"]),
    b_right = as.numeric(fit$bws["b", "right"]),
    conventional_estimate = as.numeric(fit$coef["Conventional", "Coeff"]),
    conventional_se = as.numeric(fit$se["Conventional", "Std. Err."]),
    conventional_p = as.numeric(fit$pv["Conventional", "P>|z|"]),
    bias_corrected_estimate = as.numeric(fit$coef["Bias-Corrected", "Coeff"]),
    bias_corrected_se = as.numeric(fit$se["Bias-Corrected", "Std. Err."]),
    bias_corrected_p = as.numeric(fit$pv["Bias-Corrected", "P>|z|"]),
    robust_estimate = as.numeric(fit$coef["Robust", "Coeff"]),
    robust_se = as.numeric(fit$se["Robust", "Std. Err."]),
    robust_p = as.numeric(fit$pv["Robust", "P>|z|"])
  )
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.1) return("*")
  ""
}

fmt_estimate <- function(results, method_id) {
  row <- results %>% filter(method == method_id) %>% slice(1)
  if (nrow(row) == 0 || row$status == "error" || !is.finite(row$robust_estimate)) {
    return("not estimated")
  }

  sprintf(
    "%.3f%s (SE %.3f, p=%.3f)",
    row$robust_estimate,
    stars(row$robust_p),
    row$robust_se,
    row$robust_p
  )
}

weighted_mean_safe <- function(x, w) {
  keep <- is.finite(x) & is.finite(w)
  if (!any(keep) || sum(w[keep], na.rm = TRUE) <= 0) {
    return(NA_real_)
  }
  weighted.mean(x[keep], w[keep], na.rm = TRUE)
}

cov_terms <- c(controls, fe_map[[fe_spec]]$cov_terms)
covs_formula <- as.formula(sprintf("~ 0 + %s", paste(cov_terms, collapse = " + ")))
covs_matrix <- model.matrix(covs_formula, data = aug)

results_core <- bind_rows(
  run_rdrobust_safe("residualized_outcome", aug$outcome_resid),
  run_rdrobust_safe("covariate_matrix_fe", aug$outcome, covs_matrix)
)

covs_rdplot_warnings <- character(0)
covs_rdplot <- withCallingHandlers(
  rdrobust::rdplot(
    y = aug$outcome,
    x = aug$running_distance,
    c = 0,
    p = 1,
    h = bw_ft,
    kernel = rdrobust_kernel,
    covs = covs_matrix,
    covs_drop = TRUE,
    nbins = c(30, 30),
    binselect = "es",
    hide = TRUE,
    title = "",
    x.label = "",
    y.label = ""
  ),
  warning = function(w) {
    covs_rdplot_warnings <<- c(covs_rdplot_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)

K <- 30
bin_w <- bw_ft / K
plot_data <- aug %>%
  mutate(
    side = as.integer(running_distance >= 0),
    bin_id = floor(running_distance / bin_w),
    bin_center = (bin_id + 0.5) * bin_w,
    rd_weight = pmax(0, 1 - abs(running_distance) / bw_ft)
  )

bins <- plot_data %>%
  group_by(bin_center, side) %>%
  summarise(
    n = n(),
    mean_y = weighted_mean_safe(outcome_resid, rd_weight),
    .groups = "drop"
  )

fit_side <- function(side_value) {
  side_data <- plot_data %>% filter(side == side_value)
  if (nrow(side_data) < 2) {
    return(tibble(running_distance = numeric(0), fit = numeric(0), side = integer(0)))
  }

  m <- lm(outcome_resid ~ running_distance, data = side_data, weights = rd_weight)
  x_grid <- if (side_value == 0) {
    seq(-bw_ft, 0, length.out = 200)
  } else {
    seq(0, bw_ft, length.out = 200)
  }

  tibble(
    running_distance = x_grid,
    fit = as.numeric(predict(m, newdata = tibble(running_distance = x_grid))),
    side = side_value
  )
}

fit_lines <- bind_rows(fit_side(0), fit_side(1))

outcome_label <- c(
  density_far = "Floor-Area Ratio (FAR)",
  density_dupac = "Dwelling Units Per Acre (DUPAC)",
  unitscount = "Units"
)
ylab <- ifelse(yvar %in% names(outcome_label), outcome_label[[yvar]], yvar)
if (use_log) ylab <- paste0("Log(", ylab, ")")

subtitle_text <- sprintf(
  "Residualized: %s | FE dummies in covs: %s",
  fmt_estimate(results_core, "residualized_outcome"),
  fmt_estimate(results_core, "covariate_matrix_fe")
)

caption_text <- str_wrap(
  sprintf(
    "Binned points and lines use the FE-residualized outcome. rdrobust uses a %d ft bandwidth, triangular kernel, and ward-pair clustering; the covs version passes controls and FE dummies directly to rdrobust.",
    as.integer(bw_ft)
  ),
  width = 125
)

p <- ggplot() +
  geom_point(
    data = bins,
    aes(x = bin_center, y = mean_y, color = factor(side), size = n),
    alpha = 0.9
  ) +
  geom_line(
    data = fit_lines,
    aes(x = running_distance, y = fit, color = factor(side)),
    linewidth = 1.05
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
  scale_size_continuous(range = c(1.2, 3.0), guide = "none") +
  labs(
    title = sprintf("RD robust diagnostic: %s, %s", ylab, sample_filter),
    subtitle = subtitle_text,
    x = "Running distance (ft) relative to boundary",
    y = "FE-residualized outcome",
    caption = caption_text
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(size = 7.5, hjust = 0),
    plot.margin = margin(8, 12, 12, 8)
  )

ggsave(output_pdf, p, width = 8.6, height = 6.3, dpi = 300)
ggsave(output_png, p, width = 8.6, height = 6.3, dpi = 180)

covs_rdplot_note <- str_wrap(
  paste(
    "This is rdplot with controls and FE dummies passed through covs= and covs evaluated at means.",
    "rdplot reports that covariate-adjusted global polynomial fits may not line up visually with local binned means."
  ),
  width = 125
)

covs_rdplot_graph <- covs_rdplot$rdplot +
  labs(
    title = sprintf("rdplot with FE dummies in covs: %s, %s", ylab, sample_filter),
    subtitle = sprintf("FE dummies in covs rdrobust: %s", fmt_estimate(results_core, "covariate_matrix_fe")),
    x = "Running distance (ft) relative to boundary",
    y = ylab,
    caption = covs_rdplot_note
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(size = 7.5, hjust = 0),
    plot.margin = margin(8, 12, 12, 8)
  )

ggsave(output_covs_rdplot_pdf, covs_rdplot_graph, width = 8.6, height = 6.3, dpi = 300)
ggsave(output_covs_rdplot_png, covs_rdplot_graph, width = 8.6, height = 6.3, dpi = 180)

results <- results_core %>%
  mutate(
    yvar = yvar,
    use_log = use_log,
    bw_ft = bw_ft,
    sample_filter = sample_filter,
    fe_spec = fe_spec,
    cluster_level = cluster_level,
    control_style = control_style,
    kernel = rdrobust_kernel,
    input_path = rd_input_path,
    output_csv = output_csv,
    output_pdf = output_pdf,
    output_png = output_png,
    output_covs_rdplot_pdf = output_covs_rdplot_pdf,
    output_covs_rdplot_png = output_covs_rdplot_png,
    covs_rdplot_note = paste(unique(covs_rdplot_warnings), collapse = " | "),
    .before = method
  )

write_csv(results, output_csv)
cat("Saved:\n")
cat(" -", output_csv, "\n")
cat(" -", output_pdf, "\n")
cat(" -", output_png, "\n")
cat(" -", output_covs_rdplot_pdf, "\n")
cat(" -", output_covs_rdplot_png, "\n")
