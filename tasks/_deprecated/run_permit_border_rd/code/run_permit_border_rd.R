source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_permit_border_rd/code")
# Rscript run_permit_border_rd.R cohort_2015 high_discretion issue 500 ../output/permit_border_rd_cohort_2015_high_discretion_issue_bw500_segment_year_clust_pair.pdf
# =======================================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 5) {
  panel_mode <- args[1]
  outcome_family <- args[2]
  date_basis <- args[3]
  bw_ft <- suppressWarnings(as.integer(args[4]))
  output_pdf <- args[5]
} else {
  if (!exists("panel_mode") || !exists("outcome_family") || !exists("date_basis") ||
      !exists("bw_ft") || !exists("output_pdf")) {
    stop(
      "FATAL: Script requires args: <panel_mode> <outcome_family> <date_basis> <bw_ft> <output_pdf>",
      call. = FALSE
    )
  }
}

if (!panel_mode %in% c("cohort_2015", "stacked_implementation")) {
  stop("panel_mode must be one of: cohort_2015, stacked_implementation", call. = FALSE)
}
if (!outcome_family %in% c("high_discretion", "new_construction", "unit_increase")) {
  stop("outcome_family must be one of: high_discretion, new_construction, unit_increase", call. = FALSE)
}
if (!date_basis %in% c("issue", "application")) {
  stop("date_basis must be one of: issue, application", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive integer.", call. = FALSE)
}

outcome_label <- function(outcome_name, basis_name) {
  base <- dplyr::case_when(
    outcome_name == "high_discretion" ~ "High-Discretion Permits",
    outcome_name == "new_construction" ~ "New Construction Permits",
    TRUE ~ "Unit-Increase Permits"
  )
  paste("Issued", base, "Per Block-Year")
}

panel_label <- function(panel_name) {
  dplyr::case_when(
    panel_name == "cohort_2015" ~ "2015 Cohort",
    TRUE ~ "Stacked Implementation"
  )
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

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

input_path <- if (panel_mode == "cohort_2015") {
  "../input/permit_block_year_panel_2015.parquet"
} else {
  "../input/permit_block_year_panel.parquet"
}

outcome_var <- paste0("n_", outcome_family, "_", date_basis)
pair_var <- if (panel_mode == "cohort_2015") "ward_pair_id" else "cohort_ward_pair"
segment_var <- if (panel_mode == "cohort_2015") "segment_id_cohort" else "cohort_segment"
fe_term <- if (panel_mode == "cohort_2015") "segment_id_cohort + year" else "cohort_segment + year"
cluster_formula <- as.formula(paste0("~", pair_var))

message("=== Permit Border RD ===")
message(sprintf("Input: %s", input_path))
message(sprintf("Panel: %s", panel_mode))
message(sprintf("Outcome: %s", outcome_var))
message(sprintf("Bandwidth: %d ft", bw_ft))
message("Window: full pooled sample")
message(sprintf("FE: %s", fe_term))

dat <- read_parquet(input_path) %>%
  as_tibble() %>%
  mutate(side_ward = sub(".*_", "", ward_pair_side)) %>%
  filter(
    !is.na(.data[[outcome_var]]),
    !is.na(dist_ft),
    !is.na(ward_pair_side),
    !is.na(.data[[pair_var]]),
    !is.na(.data[[segment_var]]),
    !is.na(strictness_origin),
    dist_ft <= bw_ft
  )

side_map <- dat %>%
  distinct(.data[[pair_var]], side_ward, strictness_origin) %>%
  group_by(.data[[pair_var]], side_ward) %>%
  summarise(side_strictness = median(strictness_origin, na.rm = TRUE), .groups = "drop") %>%
  group_by(.data[[pair_var]]) %>%
  summarise(
    n_sides = n(),
    strictness_gap = max(side_strictness, na.rm = TRUE) - min(side_strictness, na.rm = TRUE),
    stricter_side = side_ward[which.max(side_strictness)],
    lenient_side = side_ward[which.min(side_strictness)],
    .groups = "drop"
  ) %>%
  filter(n_sides == 2, is.finite(strictness_gap), strictness_gap > 0)

dat <- dat %>%
  inner_join(side_map, by = pair_var) %>%
  mutate(
    right = as.integer(side_ward == stricter_side),
    signed_dist = if_else(right == 1L, dist_ft, -dist_ft)
  ) %>%
  filter(is.finite(signed_dist))

if (nrow(dat) == 0) {
  stop("No observations remain after RD sample construction.", call. = FALSE)
}
if (n_distinct(dat[[pair_var]]) < 2) {
  stop("Need at least two informative border pairs for RD estimation.", call. = FALSE)
}

fml <- as.formula(sprintf(
  "%s ~ right + signed_dist + right:signed_dist | %s",
  outcome_var,
  fe_term
))

m <- feols(fml, data = dat, cluster = cluster_formula)
ct <- coeftable(m)

b_side <- get_coef(ct, c("right"))
b_x <- get_coef(ct, c("signed_dist"))
b_int <- get_coef(ct, c("right:signed_dist", "signed_dist:right"))

if (!is.finite(b_side["estimate"])) {
  stop("RD jump coefficient was not estimated.", call. = FALSE)
}

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
aug <- aug %>%
  mutate(
    xb = b_side["estimate"] * right +
      b_x["estimate"] * signed_dist +
      b_int["estimate"] * (right * signed_dist),
    y_adj = .resid + xb
  )

bins_per_side <- 10L
bin_w <- bw_ft / bins_per_side

bins <- aug %>%
  mutate(
    bin_id = floor(signed_dist / bin_w),
    bin_center = (bin_id + 0.5) * bin_w
  ) %>%
  group_by(bin_center) %>%
  summarise(
    n = n(),
    mean_y = mean(y_adj, na.rm = TRUE),
    se_y = sd(y_adj, na.rm = TRUE) / sqrt(n),
    side = if_else(first(bin_center) >= 0, "More Stringent Side", "Less Stringent Side"),
    .groups = "drop"
  )

x_left <- seq(-bw_ft, 0, length.out = 200)
x_right <- seq(0, bw_ft, length.out = 200)
line_df <- bind_rows(
  tibble(
    signed_dist = x_left,
    side = "Less Stringent Side",
    fit = b_x["estimate"] * x_left
  ),
  tibble(
    signed_dist = x_right,
    side = "More Stringent Side",
    fit = b_side["estimate"] + (b_x["estimate"] + b_int["estimate"]) * x_right
  )
)

jump_label <- sprintf(
  "Jump = %.3f%s (SE %.3f, p = %.3f)\nN = %s | Informative border pairs = %d | Mean outcome = %.3f",
  b_side["estimate"],
  stars(b_side["p"]),
  b_side["se"],
  b_side["p"],
  format(nobs(m), big.mark = ","),
  n_distinct(aug[[pair_var]]),
  mean(aug[[outcome_var]], na.rm = TRUE)
)

p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(
    data = bins,
    aes(x = bin_center, y = mean_y, color = side),
    size = 2.5, alpha = 0.9
  ) +
  geom_line(
    data = line_df,
    aes(x = signed_dist, y = fit, color = side),
    linewidth = 1.1
  ) +
  scale_color_manual(
    values = c("Less Stringent Side" = "#1f77b4", "More Stringent Side" = "#d62728"),
    name = ""
  ) +
  annotate(
    "text",
    x = -bw_ft * 0.95,
    y = max(bins$mean_y, na.rm = TRUE),
    label = jump_label,
    hjust = 0,
    vjust = 1,
    size = 3.3,
    fontface = "bold",
    color = "#1f2d3d"
  ) +
  labs(
    title = paste0("Permit Border RD (FE-Adjusted): ", panel_label(panel_mode)),
    subtitle = sprintf("%s | Full pooled sample | bw = %d ft | FE: segment + year", outcome_label(outcome_family, date_basis), bw_ft),
    x = "Distance to More Stringent Origin-Side Ward Boundary (ft)",
    y = outcome_label(outcome_family, date_basis)
  ) +
  coord_cartesian(xlim = c(-bw_ft, bw_ft)) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray35")
  )

ggsave(output_pdf, p, width = 8.5, height = 6.0, dpi = 300)

message(sprintf(
  "Saved %s | jump=%.3f | se=%.3f | p=%.3f | n=%d | pairs=%d",
  output_pdf,
  b_side["estimate"],
  b_side["se"],
  b_side["p"],
  nobs(m),
  n_distinct(aug[[pair_var]])
))
