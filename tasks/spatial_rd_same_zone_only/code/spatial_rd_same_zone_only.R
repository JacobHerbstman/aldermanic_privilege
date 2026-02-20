source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) {
  stop("Expected args: <yvar> <use_log> <bw> <kernel> <output_pdf>")
}

yvar <- args[1]
use_log <- tolower(args[2]) %in% c("true", "t", "1", "yes")
bw <- as.numeric(args[3])
kernel <- args[4]
output_pdf <- args[5]

if (!is.finite(bw) || bw <= 0) {
  stop("Bandwidth must be positive.")
}

raw <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 1,
    unitscount <= 100,
    construction_year >= 1999
  )

if (!yvar %in% names(raw)) {
  stop("Outcome variable not found: ", yvar)
}

dat <- raw %>%
  filter(abs(signed_distance) <= bw)

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

dat <- dat %>%
  mutate(cluster_id = paste(boundary_year, ward_pair, zone_code, sep = "_"))

rd <- rdrobust(
  y = dat$outcome,
  x = dat$signed_distance,
  c = 0,
  kernel = kernel,
  p = 1,
  h = bw,
  cluster = dat$cluster_id
)

coef_conventional <- rd$coef[1]
se_conventional <- rd$se[1]
p_conventional <- rd$pv[1]
coef_robust <- rd$coef[3]
se_robust <- rd$se[3]
p_robust <- rd$pv[3]

star_label <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

annotation_text <- sprintf(
  "Conventional: %.3f%s (%.3f)\\nRobust:       %.3f%s (%.3f)",
  coef_conventional,
  star_label(p_conventional),
  se_conventional,
  coef_robust,
  star_label(p_robust),
  se_robust
)

rd_bins <- rdplot(
  y = dat$outcome,
  x = dat$signed_distance,
  c = 0,
  h = bw,
  p = 1,
  kernel = kernel,
  nbins = c(30, 30),
  binselect = "es",
  x.lim = c(-bw, bw),
  title = "",
  y.label = "",
  x.label = ""
)

bins <- rd_bins$vars_bins

outcome_labels <- c(
  density_far = "Floor-Area Ratio (FAR)",
  density_dupac = "Dwelling Units Per Acre (DUPAC)",
  unitscount = "Units"
)

y_label <- ifelse(yvar %in% names(outcome_labels), outcome_labels[[yvar]], yvar)
if (use_log) {
  y_label <- paste0("Log(", y_label, ")")
}

plot_data_left <- dat %>% filter(signed_distance < 0)
plot_data_right <- dat %>% filter(signed_distance >= 0)

p <- ggplot() +
  geom_point(
    data = bins,
    aes(x = rdplot_mean_x, y = rdplot_mean_y),
    fill = "#2C3E50",
    shape = 21,
    color = "white",
    size = 2.5,
    stroke = 0.3
  ) +
  geom_smooth(
    data = plot_data_left,
    aes(x = signed_distance, y = outcome),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#4575B4",
    fill = "#4575B4",
    alpha = 0.2,
    linewidth = 1.1
  ) +
  geom_smooth(
    data = plot_data_right,
    aes(x = signed_distance, y = outcome),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#D73027",
    fill = "#D73027",
    alpha = 0.2,
    linewidth = 1.1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray35") +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = annotation_text,
    hjust = -0.05,
    vjust = 1.4,
    size = 3,
    fontface = "bold"
  ) +
  labs(
    title = "Discontinuity in Development Density at Ward Boundary",
    subtitle = paste0("bw: ", bw, " ft | kernel: ", kernel, " | N = ", nrow(dat)),
    y = y_label,
    x = "Distance to Stricter Ward Boundary (feet)"
  ) +
  coord_cartesian(xlim = c(-bw, bw)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave(output_pdf, plot = p, width = 8, height = 6, dpi = 300)

run_qc <- tibble(
  output_file = basename(output_pdf),
  yvar = yvar,
  use_log = use_log,
  bw = bw,
  kernel = kernel,
  n_obs = nrow(dat),
  coef_conventional = as.numeric(coef_conventional),
  se_conventional = as.numeric(se_conventional),
  p_conventional = as.numeric(p_conventional),
  coef_robust = as.numeric(coef_robust),
  se_robust = as.numeric(se_robust),
  p_robust = as.numeric(p_robust),
  run_timestamp = as.character(Sys.time())
)

qc_path <- "../output/rd_run_qc.csv"
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

significant_robust <- sum(updated_qc$p_robust < 0.05, na.rm = TRUE)

qc_lines <- c(
  "spatial rd qc summary",
  paste0("rows_in_qc: ", nrow(updated_qc)),
  paste0("robust_p_lt_0_05_count: ", significant_robust),
  paste0("latest_output_file: ", basename(output_pdf)),
  paste0("latest_yvar: ", yvar),
  paste0("latest_use_log: ", use_log),
  paste0("latest_bw: ", bw),
  paste0("latest_n_obs: ", nrow(dat)),
  paste0("latest_coef_robust: ", round(coef_robust, 4)),
  paste0("latest_p_robust: ", round(p_robust, 4))
)
writeLines(qc_lines, "../output/rd_run_qc.txt")

message("Wrote ", output_pdf)
message("Wrote ../output/rd_run_qc.csv")
message("Wrote ../output/rd_run_qc.txt")
