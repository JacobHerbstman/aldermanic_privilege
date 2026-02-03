source("../../setup_environment/code/packages.R")

# Usage:
# Rscript spatial_rd_fe.R <yvar> <use_log> <bw_ft> <fe_spec> <output_pdf>
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) {
  stop("FATAL: expected 5 args: <yvar> <use_log> <bw_ft> <fe_spec> <output_pdf>", call. = FALSE)
}

yvar <- args[1]
use_log <- tolower(args[2]) %in% c("true", "t", "1", "yes")
bw_ft <- as.numeric(args[3])
fe_spec <- args[4]
output_pdf <- args[5]

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

rhs <- paste(c("side", "signed_distance", "side:signed_distance", controls), collapse = " + ")
fml <- as.formula(sprintf("outcome ~ %s | %s", rhs, fe_map[[fe_spec]]))

m <- feols(fml, data = dat, cluster = ~ward_pair)
ct <- coeftable(m)

get_coef <- function(names_vec) {
  idx <- which(rownames(ct) %in% names_vec)
  if (length(idx) == 0) return(c(estimate = NA_real_, se = NA_real_, p = NA_real_))
  c(estimate = ct[idx[1], "Estimate"], se = ct[idx[1], "Std. Error"], p = ct[idx[1], "Pr(>|t|)"])
}

b_side <- get_coef(c("side"))
b_x <- get_coef(c("signed_distance"))
b_int <- get_coef(c("side:signed_distance", "signed_distance:side"))

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.1) return("*")
  ""
}

# Use model-used sample for plotting (ensures alignment with coefficient sample)
removed <- m$obs_selection$obsRemoved
if (is.null(removed)) {
  keep_idx <- seq_len(nrow(dat))
} else {
  keep_idx <- setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
}

aug <- dat[keep_idx, , drop = FALSE]
if (nrow(aug) != nobs(m)) {
  stop(sprintf("Model/sample alignment failed: kept=%d, nobs=%d", nrow(aug), nobs(m)), call. = FALSE)
}
aug$.resid <- as.numeric(resid(m))

# Frisch-Waugh style adjusted outcome that keeps the X-part visible:
# y_adj = residual + X*beta, where X = {side, x, side*x}
aug <- aug %>%
  mutate(
    xb = b_side["estimate"] * side +
      b_x["estimate"] * signed_distance +
      b_int["estimate"] * (side * signed_distance),
    y_adj = .resid + xb
  )

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

# Fitted lines implied by side-jump model component
x_left <- seq(-bw_ft, 0, length.out = 200)
x_right <- seq(0, bw_ft, length.out = 200)
line_df <- bind_rows(
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

jump_label <- sprintf(
  "Jump at cutoff = %.3f%s (SE %.3f)",
  b_side["estimate"], stars(b_side["p"]), b_side["se"]
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
    title = paste0("Spatial RD (FE-Adjusted): ", ylab),
    subtitle = sprintf(
      "%s | bw=%d ft | FE=%s | N=%d | Ward pairs=%d",
      jump_label, as.integer(bw_ft), fe_spec, nobs(m), dplyr::n_distinct(aug$ward_pair)
    ),
    x = "Signed distance to boundary (feet; right is stricter side)",
    y = ylab,
    caption = "Points: binned means of FE+controls-adjusted outcome. Lines: fitted side-jump model component."
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
    n_obs = nobs(m),
    n_pairs = dplyr::n_distinct(aug$ward_pair),
    jump_estimate = b_side["estimate"],
    jump_se = b_side["se"],
    jump_p = b_side["p"],
    slope_left = b_x["estimate"],
    slope_diff_right_minus_left = b_int["estimate"]
  ),
  out_meta
)

cat("Saved:\n")
cat(" -", output_pdf, "\n")
cat(" -", out_csv, "\n")
cat(" -", out_meta, "\n")
