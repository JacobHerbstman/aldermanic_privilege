source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input_side_panel <- "../output/listing_units_side_panel_bw500_pre_2023_all_pct0_unit_proxy_all.parquet"
# input_bin_cells <- "../output/listing_units_bin_cells_bw500_pre_2023_all_pct0_unit_proxy_bins8_all.parquet"
# output_pdf <- "../output/listing_units_unit_proxy_bw500_pre_2023_all_pct0.pdf"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input_side_panel, input_bin_cells, output_pdf)
}

if (length(cli_args) >= 3) {
  input_side_panel <- cli_args[1]
  input_bin_cells <- cli_args[2]
  output_pdf <- cli_args[3]
} else {
  stop("FATAL: Script requires 3 args: <input_side_panel> <input_bin_cells> <output_pdf>", call. = FALSE)
}

cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("CLUSTER_LEVEL must be one of: segment, ward_pair", call. = FALSE)
}
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair

panel <- read_parquet(input_side_panel) |>
  as_tibble()
bin_cells <- read_parquet(input_bin_cells) |>
  as_tibble()

panel_cols <- c("segment_id", "year_month", "right", "strictness_own", "n_units")
if (cluster_level == "ward_pair") {
  panel_cols <- c(panel_cols, "ward_pair")
}
missing_panel_cols <- setdiff(panel_cols, names(panel))
if (length(missing_panel_cols) > 0) {
  stop(sprintf("Cached side panel missing required columns: %s", paste(missing_panel_cols, collapse = ", ")), call. = FALSE)
}

bin_cols <- c("segment_id", "year_month", "bin_center", "right", "log_n")
if (cluster_level == "ward_pair") {
  bin_cols <- c(bin_cols, "ward_pair")
}
missing_bin_cols <- setdiff(bin_cols, names(bin_cells))
if (length(missing_bin_cols) > 0) {
  stop(sprintf("Cached bin cells missing required columns: %s", paste(missing_bin_cols, collapse = ", ")), call. = FALSE)
}

m <- fepois(n_units ~ right | segment_id^year_month, data = panel, cluster = cluster_formula)
ct <- coeftable(m)
p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)
if (length(p_col) == 0) {
  stop("Could not find p-value column in coeftable output.", call. = FALSE)
}

b_right <- ct["right", "Estimate"]
se_right <- ct["right", "Std. Error"]
p_right <- ct["right", p_col[1]]

message(sprintf(
  "Cached side-level PPML: b=%.4f (SE %.4f, p=%.3f), N cells=%s, %d segments",
  b_right, se_right, p_right, format(nobs(m), big.mark = ","), n_distinct(panel$segment_id)
))

m_bin <- feols(log_n ~ right | segment_id^year_month, data = bin_cells, cluster = cluster_formula)
removed <- m_bin$obs_selection$obsRemoved
keep_idx <- if (is.null(removed)) seq_len(nrow(bin_cells)) else setdiff(seq_len(nrow(bin_cells)), abs(as.integer(removed)))
aug <- bin_cells[keep_idx, , drop = FALSE]
stopifnot(nrow(aug) == nobs(m_bin))
aug$y_adj <- as.numeric(resid(m_bin)) + b_right * aug$right

bins <- aug |>
  group_by(bin_center) |>
  summarise(
    mean_y = mean(y_adj),
    se_y = sd(y_adj) / sqrt(n()),
    n = n(),
    side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
    .groups = "drop"
  )

side_means <- bins |>
  group_by(side) |>
  summarise(
    x_min = min(bin_center),
    x_max = max(bin_center),
    y_mean = mean(mean_y),
    .groups = "drop"
  )

p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.7) +
  geom_segment(
    data = side_means,
    aes(x = x_min, xend = x_max, y = y_mean, yend = y_mean, color = side),
    linewidth = 1.0
  ) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.5) +
  scale_color_manual(values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"), name = "") +
  labs(
    x = "Distance to Ward Boundary (feet; positive = more stringent side)",
    y = "Log(Distinct Listed Units), Residualized"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )

ggsave(output_pdf, p, width = 7, height = 5, dpi = 300, bg = "white")
message(sprintf("Saved: %s", output_pdf))
