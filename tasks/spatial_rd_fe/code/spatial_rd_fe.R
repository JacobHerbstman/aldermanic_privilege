source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) {
  stop("Expected args: <yvar> <use_log> <bw_ft> <fe_spec> <output_pdf> [plot_style]")
}

yvar <- args[1]
use_log <- tolower(args[2]) %in% c("true", "t", "1", "yes")
bw_ft <- as.numeric(args[3])
fe_spec <- args[4]
output_pdf <- args[5]
plot_style <- if (length(args) >= 6) tolower(args[6]) else "slope"

if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be positive.")
}
if (!plot_style %in% c("slope", "level")) {
  stop("plot_style must be one of: slope, level")
}

fe_map <- list(
  pair_x_year = "ward_pair^construction_year",
  pair_year = "ward_pair + construction_year"
)
if (!fe_spec %in% names(fe_map)) {
  stop("fe_spec must be one of: pair_x_year, pair_year")
}

raw <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE)
if (!yvar %in% names(raw)) {
  stop("Outcome variable not found: ", yvar)
}

dat <- raw %>%
  mutate(strictness_own = strictness_own / sd(strictness_own, na.rm = TRUE)) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 1,
    unitscount <= 100,
    construction_year >= 1999,
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

if (nrow(dat) == 0) {
  stop("No observations after filters.")
}

controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

dat <- dat %>% mutate(side = as.integer(signed_distance > 0))

rhs_rd <- paste(c("side", "signed_distance", "side:signed_distance", controls), collapse = " + ")
fml_rd <- as.formula(sprintf("outcome ~ %s | %s", rhs_rd, fe_map[[fe_spec]]))

m_rd <- feols(fml_rd, data = dat, cluster = ~ward_pair)
ct_rd <- coeftable(m_rd)

get_coef <- function(ct, names_vec) {
  idx <- which(rownames(ct) %in% names_vec)
  if (length(idx) == 0) {
    return(c(estimate = NA_real_, se = NA_real_, p = NA_real_))
  }
  c(
    estimate = ct[idx[1], "Estimate"],
    se = ct[idx[1], "Std. Error"],
    p = ct[idx[1], "Pr(>|t|)"]
  )
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
  rhs_resid <- paste(controls, collapse = " + ")
  fml_resid <- as.formula(sprintf("outcome ~ %s | %s", rhs_resid, fe_map[[fe_spec]]))
  m_resid <- feols(fml_resid, data = dat, cluster = ~ward_pair)

  removed <- m_resid$obs_selection$obsRemoved
  keep_idx <- if (is.null(removed)) seq_len(nrow(dat)) else setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
  aug <- dat[keep_idx, , drop = FALSE]
  aug <- aug %>% mutate(y_adj = as.numeric(resid(m_resid)))

  m_gap <- feols(y_adj ~ side, data = aug, cluster = ~ward_pair)
  b_side_plot <- get_coef(coeftable(m_gap), c("side"))
  n_obs_plot <- nobs(m_resid)
} else {
  removed <- m_rd$obs_selection$obsRemoved
  keep_idx <- if (is.null(removed)) seq_len(nrow(dat)) else setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
  aug <- dat[keep_idx, , drop = FALSE]
  aug <- aug %>%
    mutate(
      resid_val = as.numeric(resid(m_rd)),
      xb = b_side["estimate"] * side +
        b_x["estimate"] * signed_distance +
        b_int["estimate"] * (side * signed_distance),
      y_adj = resid_val + xb
    )
  b_side_plot <- b_side
  n_obs_plot <- nobs(m_rd)
}

bin_width <- bw_ft / 30
bins <- aug %>%
  mutate(
    bin_id = floor(signed_distance / bin_width),
    bin_center = (bin_id + 0.5) * bin_width
  ) %>%
  group_by(bin_center, side) %>%
  summarise(
    mean_y = mean(y_adj, na.rm = TRUE),
    .groups = "drop"
  )

line_df <- if (plot_style == "level") {
  left_mean <- mean(aug$y_adj[aug$side == 0], na.rm = TRUE)
  right_mean <- mean(aug$y_adj[aug$side == 1], na.rm = TRUE)
  bind_rows(
    tibble(signed_distance = c(-bw_ft, 0), side = 0, fit = left_mean),
    tibble(signed_distance = c(0, bw_ft), side = 1, fit = right_mean)
  )
} else {
  x_left <- seq(-bw_ft, 0, length.out = 200)
  x_right <- seq(0, bw_ft, length.out = 200)
  bind_rows(
    tibble(signed_distance = x_left, side = 0, fit = b_x["estimate"] * x_left),
    tibble(signed_distance = x_right, side = 1, fit = b_side["estimate"] + (b_x["estimate"] + b_int["estimate"]) * x_right)
  )
}

outcome_labels <- c(
  density_far = "Floor-Area Ratio (FAR)",
  density_dupac = "Dwelling Units Per Acre (DUPAC)",
  unitscount = "Units"
)
y_label <- ifelse(yvar %in% names(outcome_labels), outcome_labels[[yvar]], yvar)
if (use_log) {
  y_label <- paste0("Log(", y_label, ")")
}

jump_label <- sprintf(
  "Jump at cutoff = %.3f%s (SE %.3f)",
  b_side_plot["estimate"],
  stars(b_side_plot["p"]),
  b_side_plot["se"]
)

p <- ggplot() +
  geom_point(
    data = bins,
    aes(x = bin_center, y = mean_y, color = factor(side)),
    size = 1.6,
    alpha = 0.9
  ) +
  geom_line(
    data = line_df,
    aes(x = signed_distance, y = fit, color = factor(side)),
    linewidth = 1.1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray35") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
  labs(
    title = ifelse(plot_style == "level", "Spatial RD (FE-Adjusted Levels)", "Spatial RD (FE-Adjusted)"),
    subtitle = sprintf("%s | bw=%d ft | FE=%s | N=%d", jump_label, as.integer(bw_ft), fe_spec, n_obs_plot),
    x = "Signed distance to boundary (feet; right is stricter side)",
    y = y_label
  ) +
  theme_bw(base_size = 11)

ggsave(output_pdf, p, width = 8.6, height = 6.0, dpi = 300)

run_qc <- tibble(
  output_file = basename(output_pdf),
  yvar = yvar,
  use_log = use_log,
  bw_ft = bw_ft,
  fe_spec = fe_spec,
  plot_style = plot_style,
  n_obs = n_obs_plot,
  n_pairs = n_distinct(aug$ward_pair),
  jump_estimate = as.numeric(b_side_plot["estimate"]),
  jump_se = as.numeric(b_side_plot["se"]),
  jump_p = as.numeric(b_side_plot["p"]),
  rd_jump_estimate = as.numeric(b_side["estimate"]),
  rd_jump_se = as.numeric(b_side["se"]),
  rd_jump_p = as.numeric(b_side["p"]),
  run_timestamp = as.character(Sys.time())
)

qc_path <- "../output/spatial_rd_fe_qc.csv"
existing_qc <- if (file.exists(qc_path)) {
  read_csv(qc_path, show_col_types = FALSE)
} else {
  tibble()
}

if (nrow(existing_qc) > 0 && "run_timestamp" %in% names(existing_qc)) {
  existing_qc <- existing_qc %>% mutate(run_timestamp = as.character(run_timestamp))
}

if (nrow(existing_qc) > 0) {
  existing_qc <- existing_qc %>% filter(output_file != basename(output_pdf))
}

updated_qc <- bind_rows(existing_qc, run_qc) %>% arrange(output_file)
write_csv(updated_qc, qc_path)

significant_jump <- sum(updated_qc$jump_p < 0.05, na.rm = TRUE)

txt_lines <- c(
  "spatial rd fe qc summary",
  paste0("rows_in_qc: ", nrow(updated_qc)),
  paste0("jump_p_lt_0_05_count: ", significant_jump),
  paste0("latest_output_file: ", basename(output_pdf)),
  paste0("latest_yvar: ", yvar),
  paste0("latest_plot_style: ", plot_style),
  paste0("latest_bw_ft: ", bw_ft),
  paste0("latest_n_obs: ", n_obs_plot),
  paste0("latest_jump_estimate: ", round(as.numeric(b_side_plot["estimate"]), 4)),
  paste0("latest_jump_p: ", round(as.numeric(b_side_plot["p"]), 4))
)
writeLines(txt_lines, "../output/spatial_rd_fe_qc.txt")

message("Wrote ", output_pdf)
message("Wrote ../output/spatial_rd_fe_qc.csv")
message("Wrote ../output/spatial_rd_fe_qc.txt")
