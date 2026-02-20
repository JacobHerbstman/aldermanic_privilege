source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_case_study_visuals/code")
# bw_ft <- 500
# borders <- "1_26,11_25"
# bin_ft <- 50
# output_pdf <- "../output/case_study_far_panel_bw500.pdf"
# output_csv <- "../output/case_study_summary_bw500.csv"
# Rscript build_case_study_visuals.R 500 "1_26,11_25" 50 "../output/case_study_far_panel_bw500.pdf" "../output/case_study_summary_bw500.csv"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 5) {
  bw_ft <- as.numeric(cli_args[1])
  borders <- cli_args[2]
  bin_ft <- as.numeric(cli_args[3])
  output_pdf <- cli_args[4]
  output_csv <- cli_args[5]
} else {
  if (!exists("bw_ft") || !exists("borders") || !exists("bin_ft") || !exists("output_pdf") || !exists("output_csv")) {
    stop("FATAL: Script requires 5 args: <bw_ft> <borders> <bin_ft> <output_pdf> <output_csv>", call. = FALSE)
  }
}

bw_ft <- as.numeric(bw_ft)
bin_ft <- as.numeric(bin_ft)
borders <- str_split(borders, ",", simplify = TRUE) %>%
  as.character() %>%
  trimws()
borders <- borders[borders != ""]

if (!is.finite(bw_ft) || bw_ft <= 0) stop("bw_ft must be positive.", call. = FALSE)
if (!is.finite(bin_ft) || bin_ft <= 0) stop("bin_ft must be positive.", call. = FALSE)
if (length(borders) == 0) stop("At least one ward pair must be supplied via --borders.", call. = FALSE)

message("=== Border Case Study Visuals ===")
message(sprintf("bw=%.0fft | bin=%.1fft", bw_ft, bin_ft))
message(sprintf("borders=%s", paste(borders, collapse = ", ")))

df <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    unitscount >= 2,
    unitscount <= 100,
    construction_year >= 2006,
    abs(signed_distance) <= bw_ft,
    density_far > 0,
    density_dupac > 0
  ) %>%
  mutate(
    log_far = log(density_far),
    log_dupac = log(density_dupac),
    side = as.integer(signed_distance >= 0)
  ) %>%
  filter(ward_pair %in% borders)

missing_borders <- setdiff(borders, unique(df$ward_pair))
if (length(missing_borders) > 0) {
  warning(sprintf("No observations for ward pair(s): %s", paste(missing_borders, collapse = ", ")))
}
if (nrow(df) == 0) stop("No observations available for selected borders.", call. = FALSE)

dominant_pair <- df %>%
  group_by(ward_pair, alderman_own, alderman_neighbor) %>%
  summarise(n_pair = n(), .groups = "drop") %>%
  arrange(ward_pair, desc(n_pair), alderman_own, alderman_neighbor) %>%
  group_by(ward_pair) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(facet_label = paste0("Ward Pair ", ward_pair, ": ", alderman_own, " vs ", alderman_neighbor))

sig_stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.10) return("*")
  ""
}

extract_jump <- function(d, outcome_name) {
  model <- tryCatch(
    lm(reformulate(c("side", "signed_distance", "side:signed_distance"), response = outcome_name), data = d),
    error = function(e) NULL
  )
  if (is.null(model)) {
    return(tibble(jump = NA_real_, se = NA_real_, p_value = NA_real_, stars = ""))
  }
  coefs <- summary(model)$coefficients
  if (!"side" %in% rownames(coefs)) {
    return(tibble(jump = NA_real_, se = NA_real_, p_value = NA_real_, stars = ""))
  }
  p <- as.numeric(coefs["side", "Pr(>|t|)"])
  tibble(
    jump = as.numeric(coefs["side", "Estimate"]),
    se = as.numeric(coefs["side", "Std. Error"]),
    p_value = p,
    stars = sig_stars(p)
  )
}

summary_tbl <- df %>%
  group_by(ward_pair) %>%
  group_modify(~{
    far_jump <- extract_jump(.x, "log_far")
    dupac_jump <- extract_jump(.x, "log_dupac")
    tibble(
      n_total = nrow(.x),
      n_strict = sum(.x$side == 1, na.rm = TRUE),
      n_lenient = sum(.x$side == 0, na.rm = TRUE),
      strictness_gap = mean(abs(.x$strictness_own - .x$strictness_neighbor), na.rm = TRUE),
      orientation_match_rate = mean((.x$signed_distance >= 0) == (.x$strictness_own >= .x$strictness_neighbor), na.rm = TRUE),
      jump_log_far = far_jump$jump,
      se_log_far = far_jump$se,
      p_log_far = far_jump$p_value,
      stars_log_far = far_jump$stars,
      jump_log_dupac = dupac_jump$jump,
      se_log_dupac = dupac_jump$se,
      p_log_dupac = dupac_jump$p_value,
      stars_log_dupac = dupac_jump$stars,
      y_annot = quantile(.x$log_far, probs = 0.98, na.rm = TRUE)
    )
  }) %>%
  ungroup() %>%
  left_join(dominant_pair, by = "ward_pair") %>%
  mutate(
    annotation = sprintf(
      "N strict=%d | lenient=%d\nStrictness gap=%.2f\nJump log(FAR)=%.3f%s (SE %.3f)\nJump log(DUPAC)=%.3f%s (SE %.3f)",
      n_strict, n_lenient, strictness_gap, jump_log_far, stars_log_far, se_log_far, jump_log_dupac, stars_log_dupac, se_log_dupac
    )
  )

write_csv(
  summary_tbl %>%
    select(
      ward_pair,
      alderman_own,
      alderman_neighbor,
      n_total,
      n_strict,
      n_lenient,
      strictness_gap,
      orientation_match_rate,
      jump_log_far,
      se_log_far,
      p_log_far,
      stars_log_far,
      jump_log_dupac,
      se_log_dupac,
      p_log_dupac,
      stars_log_dupac
    ),
  output_csv
)

plot_df <- df %>%
  left_join(dominant_pair %>% select(ward_pair, facet_label), by = "ward_pair")

bins_df <- plot_df %>%
  mutate(
    bindex = floor(signed_distance / bin_ft),
    bin_center_ft = (bindex + 0.5) * bin_ft
  ) %>%
  group_by(ward_pair, facet_label, bindex, bin_center_ft) %>%
  summarise(
    mean_log_far = mean(log_far, na.rm = TRUE),
    n_bin = n(),
    .groups = "drop"
  )

ann_df <- summary_tbl %>%
  select(ward_pair, facet_label, annotation, y_annot) %>%
  mutate(x_annot = -0.95 * bw_ft)

p <- ggplot() +
  geom_point(
    data = bins_df,
    aes(x = bin_center_ft, y = mean_log_far),
    color = "#2C3E50",
    size = 2.3,
    alpha = 0.9
  ) +
  geom_smooth(
    data = plot_df %>% filter(side == 0),
    aes(x = signed_distance, y = log_far),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#1f77b4",
    fill = "#1f77b4",
    alpha = 0.22,
    linewidth = 1.1
  ) +
  geom_smooth(
    data = plot_df %>% filter(side == 1),
    aes(x = signed_distance, y = log_far),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#d62728",
    fill = "#d62728",
    alpha = 0.22,
    linewidth = 1.1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  geom_text(
    data = ann_df,
    aes(x = x_annot, y = y_annot, label = annotation),
    hjust = 0,
    vjust = 1,
    size = 3.0
  ) +
  facet_wrap(~facet_label, ncol = 2, scales = "free_y") +
  coord_cartesian(xlim = c(-bw_ft, bw_ft)) +
  labs(
    title = "Case Study Border Contrasts: FAR at 500ft Bandwidth",
    subtitle = "Binned means with side-specific local linear fits; right side is stricter by construction",
    x = "Distance to boundary (feet)",
    y = "Log(Floor-Area Ratio)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(output_pdf, p, width = 11, height = 5.8, dpi = 300)

message("Saved:")
message(sprintf("  - %s", output_pdf))
message(sprintf("  - %s", output_csv))