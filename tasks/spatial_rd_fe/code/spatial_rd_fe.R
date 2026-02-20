source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/spatial_rd_fe/code")
# yvar <- "density_far"
# use_log <- TRUE
# bw_ft <- 500
# fe_spec <- "pair_x_year"
# output_pdf <- "../output/rd_fe_plot_log_density_far_bw500_pair_x_year.pdf"
# plot_style <- "slope"
# Rscript spatial_rd_fe.R density_far TRUE 500 pair_x_year ../output/rd_fe_plot_log_density_far_bw500_pair_x_year.pdf slope
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 6) {
  yvar <- args[1]
  use_log <- tolower(args[2]) %in% c("true", "t", "1", "yes")
  bw_ft <- as.numeric(args[3])
  fe_spec <- args[4]
  output_pdf <- args[5]
  plot_style <- tolower(args[6])
} else if (length(args) >= 5) {
  yvar <- args[1]
  use_log <- tolower(args[2]) %in% c("true", "t", "1", "yes")
  bw_ft <- as.numeric(args[3])
  fe_spec <- args[4]
  output_pdf <- args[5]
  plot_style <- "slope"
} else {
  if (!exists("yvar") || !exists("use_log") || !exists("bw_ft") || !exists("fe_spec") || !exists("output_pdf") || !exists("plot_style")) {
    stop("FATAL: Script requires args: <yvar> <use_log> <bw_ft> <fe_spec> <output_pdf> <plot_style>", call. = FALSE)
  }
}

if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive number.", call. = FALSE)
}

fe_map <- list(
  pair_x_year = "ward_pair^construction_year",
  pair_year = "ward_pair + construction_year"
)

if (!fe_spec %in% names(fe_map)) {
  stop("fe_spec must be one of: pair_x_year, pair_year", call. = FALSE)
}
if (!plot_style %in% c("slope", "level")) {
  stop("plot_style must be one of: slope, level", call. = FALSE)
}

# 1) Load + sample filters aligned with border-pair FE table spec
raw <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE)

if (!yvar %in% names(raw)) {
  stop(sprintf("yvar '%s' not found in data.", yvar), call. = FALSE)
}

dat <- raw %>%
  mutate(strictness_own = strictness_own / sd(strictness_own, na.rm = TRUE)) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 1,
    construction_year >= 2006,
    dist_to_boundary <= bw_ft,
    !is.na(ward_pair),
    !is.na(construction_year)
  )

if (use_log) {
  dat <- dat %>%
    filter(is.finite(.data[[yvar]]), .data[[yvar]] > 0) %>%
    mutate(outcome = log(.data[[yvar]]))
} else {
  dat <- dat %>%
    filter(is.finite(.data[[yvar]])) %>%
    mutate(outcome = .data[[yvar]])
}

controls <- c(
  "share_white_own", "share_black_own", "median_hh_income_own",
  "share_bach_plus_own", "homeownership_rate_own"
)

# Keep explicit side variable for discontinuity model
# right side (signed_distance > 0) is stricter side by construction
dat <- dat %>% mutate(side = as.integer(signed_distance > 0))

rhs_rd <- paste(c("side", "signed_distance", "side:signed_distance", controls), collapse = " + ")
fml_rd <- as.formula(sprintf("outcome ~ %s | %s", rhs_rd, fe_map[[fe_spec]]))

m_rd <- feols(fml_rd, data = dat, cluster = ~ward_pair)
ct_rd <- coeftable(m_rd)

get_coef <- function(ct, names_vec) {
  idx <- which(rownames(ct) %in% names_vec)
  if (length(idx) == 0) return(c(estimate = NA_real_, se = NA_real_, p = NA_real_))
  c(estimate = ct[idx[1], "Estimate"], se = ct[idx[1], "Std. Error"], p = ct[idx[1], "Pr(>|t|)"])
}

b_side <- get_coef(ct_rd, c("side"))
b_x <- get_coef(ct_rd, c("signed_distance"))
b_int <- get_coef(ct_rd, c("side:signed_distance", "signed_distance:side"))

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.1) return("*")
  ""
}

if (plot_style == "level") {
  # Residualize on FE + controls only (no side or distance terms).
  rhs_resid <- paste(controls, collapse = " + ")
  fml_resid <- as.formula(sprintf("outcome ~ %s | %s", rhs_resid, fe_map[[fe_spec]]))
  m_resid <- feols(fml_resid, data = dat, cluster = ~ward_pair)

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

  m_gap <- feols(y_adj ~ side, data = aug, cluster = ~ward_pair)
  b_side_plot <- get_coef(coeftable(m_gap), c("side"))
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
        b_x["estimate"] * signed_distance +
        b_int["estimate"] * (side * signed_distance),
      y_adj = .resid + xb
    )

  b_side_plot <- b_side
  n_obs_plot <- nobs(m_rd)
}

# Binning for visualization
K <- 30
bin_w <- bw_ft / K
bins <- aug %>%
  mutate(bin_id = floor(signed_distance / bin_w),
         bin_center = (bin_id + 0.5) * bin_w) %>%
  group_by(bin_center, side) %>%
  summarise(
    n = n(),
    mean_y = mean(y_adj, na.rm = TRUE),
    se_y = sd(y_adj, na.rm = TRUE) / sqrt(n),
    lo = mean_y - 1.96 * se_y,
    hi = mean_y + 1.96 * se_y,
    .groups = "drop"
  )

line_df <- if (plot_style == "level") {
  mean_left <- mean(aug$y_adj[aug$side == 0], na.rm = TRUE)
  mean_right <- mean(aug$y_adj[aug$side == 1], na.rm = TRUE)
  bind_rows(
    tibble(signed_distance = c(-bw_ft, 0), side = 0, fit = mean_left),
    tibble(signed_distance = c(0, bw_ft), side = 1, fit = mean_right)
  )
} else {
  x_left <- seq(-bw_ft, 0, length.out = 200)
  x_right <- seq(0, bw_ft, length.out = 200)
  bind_rows(
    tibble(
      signed_distance = x_left,
      side = 0,
      fit = b_x["estimate"] * x_left
    ),
    tibble(
      signed_distance = x_right,
      side = 1,
      fit = b_side["estimate"] + (b_x["estimate"] + b_int["estimate"]) * x_right
    )
  )
}

jump_label <- sprintf(
  "Jump at cutoff = %.3f%s (SE %.3f)",
  b_side_plot["estimate"], stars(b_side_plot["p"]), b_side_plot["se"]
)

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
    aes(x = signed_distance, y = fit, color = factor(side)),
    linewidth = 1.1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
  labs(
    title = paste0(
      if (plot_style == "level") "Spatial RD (FE-Adjusted Levels): " else "Spatial RD (FE-Adjusted): ",
      ylab
    ),
    subtitle = sprintf(
      "%s | bw=%d ft | FE=%s | N=%d | Ward pairs=%d",
      jump_label, as.integer(bw_ft), fe_spec, n_obs_plot, dplyr::n_distinct(aug$ward_pair)
    ),
    x = "Signed distance to boundary (feet; right is stricter side)",
    y = ylab,
    caption = ifelse(
      plot_style == "level",
      "Points: binned means of outcome residualized on FE+controls only. Lines: side-level means.",
      "Points: binned means of FE+controls-adjusted outcome. Lines: fitted side-jump model component."
    )
  ) +
  theme_bw(base_size = 11)

ggsave(output_pdf, p, width = 8.6, height = 6.0, dpi = 300)

# Save companion diagnostics
out_csv <- sub("\\.pdf$", "_bins.csv", output_pdf)
out_meta <- sub("\\.pdf$", "_meta.csv", output_pdf)

write_csv(bins, out_csv)
write_csv(
  tibble(
    yvar = yvar,
    use_log = use_log,
    bw_ft = bw_ft,
    fe_spec = fe_spec,
    plot_style = plot_style,
    n_obs = n_obs_plot,
    n_pairs = dplyr::n_distinct(aug$ward_pair),
    jump_estimate = b_side_plot["estimate"],
    jump_se = b_side_plot["se"],
    jump_p = b_side_plot["p"],
    rd_jump_estimate = b_side["estimate"],
    rd_jump_se = b_side["se"],
    rd_jump_p = b_side["p"],
    slope_left = b_x["estimate"],
    slope_diff_right_minus_left = b_int["estimate"]
  ),
  out_meta
)

cat("Saved:\n")
cat(" -", output_pdf, "\n")
cat(" -", out_csv, "\n")
cat(" -", out_meta, "\n")
