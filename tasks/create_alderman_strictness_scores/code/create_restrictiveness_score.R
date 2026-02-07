# Create alderman strictness scores from aggregated ward-month permit outcomes.
# Method:
#   1) Residualize log mean processing time on ward fundamentals + month FE.
#   2) Regress residuals on alderman dummies (reference alderman fixed at 0).
#   3) Apply empirical Bayes shrinkage to alderman coefficients.
#   4) Standardize shrunk effects to form the strictness index.

source("../../setup_environment/code/packages.R")
library(optparse)
library(fixest)

option_list <- list(
  make_option("--fe_spec", type = "character", default = "month",
    help = "Stage-1 FE spec: month or ward_month [default: month]"
  ),
  make_option("--ref_alderman", type = "character", default = "Andre Vasquez",
    help = "Reference alderman in stage 2 [default: Andre Vasquez]"
  ),
  make_option("--input_csv", type = "character", default = "../input/ward_monthly_panel_for_alderman_fe.csv",
    help = "Input ward-month panel [default: ../input/ward_monthly_panel_for_alderman_fe.csv]"
  ),
  make_option("--output_csv", type = "character", default = "../output/alderman_restrictiveness_scores_month_FEs.csv",
    help = "Output score CSV [default: ../output/alderman_restrictiveness_scores_month_FEs.csv]"
  ),
  make_option("--output_plot", type = "character", default = "../output/Month_FEs_final_strictness_index.pdf",
    help = "Output score plot [default: ../output/Month_FEs_final_strictness_index.pdf]"
  ),
  make_option("--output_stage1_tex", type = "character", default = "../output/stage1_residualization_models.tex",
    help = "Output stage-1 table [default: ../output/stage1_residualization_models.tex]"
  ),
  make_option("--output_stage2_tex", type = "character", default = "../output/stage2_alderman_dummies_month_FEs.tex",
    help = "Output stage-2 table [default: ../output/stage2_alderman_dummies_month_FEs.tex]"
  )
)
opt <- parse_args(OptionParser(option_list = option_list))

fe_spec <- tolower(opt$fe_spec)
if (!fe_spec %in% c("month", "ward_month")) {
  stop("--fe_spec must be one of: month, ward_month", call. = FALSE)
}

message("=== Create alderman strictness scores ===")
message("Stage-1 FE spec: ", fe_spec)
message("Reference alderman: ", opt$ref_alderman)

# -----------------------------------------------------------------------------
# Load and prepare ward-month panel
# -----------------------------------------------------------------------------

df <- read_csv(opt$input_csv, show_col_types = FALSE) %>%
  mutate(
    month = as.yearmon(month),
    log_mean_processing_time = if_else(mean_processing_time > 0, log(mean_processing_time), NA_real_),
    pop_total_10k = pop_total / 10000,
    median_hh_income_10k = median_hh_income / 10000
  ) %>%
  filter(!is.na(alderman), !is.na(ward), !is.na(month), n_permits_applied > 0)

message("Rows loaded: ", nrow(df))

coverage <- df %>%
  group_by(alderman) %>%
  summarise(n_months = n_distinct(month), .groups = "drop")

keep_aldermen <- coverage %>%
  filter(n_months > 3) %>%
  pull(alderman)

df <- df %>% filter(alderman %in% keep_aldermen)
message("Rows after alderman coverage filter (>3 months): ", nrow(df))
message("Aldermen retained: ", n_distinct(df$alderman))

# -----------------------------------------------------------------------------
# Stage 1: residualize on ward fundamentals + FE
# -----------------------------------------------------------------------------

ca_share_vars <- grep("^ca_share_", names(df), value = TRUE)

candidate_controls <- c(
  "dist_cbd_km", "lakefront_share_1km", "n_rail_stations_800m",
  "homeownership_rate", "pop_total_10k", "median_hh_income_10k",
  "share_black", "share_hisp", "share_white",
  ca_share_vars
)

control_vars <- candidate_controls[candidate_controls %in% names(df)]
if (length(control_vars) == 0) {
  stop("No stage-1 controls found in input panel.", call. = FALSE)
}

stage1_fe_rhs <- if (fe_spec == "month") "month" else "ward + month"
stage1_formula <- as.formula(
  paste0("log_mean_processing_time ~ ", paste(control_vars, collapse = " + "), " | ", stage1_fe_rhs)
)

stage1_model <- feols(
  stage1_formula,
  data = df,
  weights = ~n_permits_applied,
  warn = FALSE
)

# Keep NA for observations dropped by feols so stage-2 sample is explicit.
df$resid_log_mean_processing_time <- resid(stage1_model, na.rm = FALSE)

message("Stage-1 observations used: ", stage1_model$nobs)
message("Stage-1 adjusted R2: ", round(r2(stage1_model, type = "ar2"), 4))

# -----------------------------------------------------------------------------
# Stage 2: plain alderman-dummy regression on residualized outcome
# -----------------------------------------------------------------------------

stage2_df <- df %>%
  filter(!is.na(resid_log_mean_processing_time))

stage2_formula <- as.formula(
  paste0("resid_log_mean_processing_time ~ i(alderman, ref = '", opt$ref_alderman, "')")
)

stage2_model <- feols(
  stage2_formula,
  data = stage2_df,
  vcov = ~ward + month,
  warn = FALSE
)

message("Stage-2 observations used: ", stage2_model$nobs)

coef_dt <- tibble(
  term = names(coef(stage2_model)),
  alderman_fe_raw = as.numeric(coef(stage2_model))
)
se_dt <- tibble(
  term = names(se(stage2_model)),
  alderman_se = as.numeric(se(stage2_model))
)

effects <- coef_dt %>%
  filter(str_detect(term, "^alderman::")) %>%
  mutate(alderman = str_remove(term, "^alderman::")) %>%
  left_join(
    se_dt %>%
      filter(str_detect(term, "^alderman::")) %>%
      mutate(alderman = str_remove(term, "^alderman::")) %>%
      select(alderman, alderman_se),
    by = "alderman"
  ) %>%
  select(alderman, alderman_fe_raw, alderman_se)

if (!opt$ref_alderman %in% effects$alderman) {
  effects <- bind_rows(
    effects,
    tibble(
      alderman = opt$ref_alderman,
      alderman_fe_raw = 0,
      alderman_se = 0
    )
  )
}

# -----------------------------------------------------------------------------
# Empirical Bayes shrinkage
# -----------------------------------------------------------------------------

# tau^2 estimate across aldermen, floored at 0
signal_var <- var(effects$alderman_fe_raw, na.rm = TRUE)
noise_var <- mean(effects$alderman_se^2, na.rm = TRUE)
tau2 <- max(0, signal_var - noise_var)

effects <- effects %>%
  mutate(
    shrinkage_B = tau2 / (tau2 + alderman_se^2),
    alderman_fe_shrunk = alderman_fe_raw * shrinkage_B
  )

standardize <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(rep(0, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / s
}

alderman_stats <- stage2_df %>%
  group_by(alderman) %>%
  summarise(
    n_ward_months = n(),
    total_permits = sum(n_permits_applied, na.rm = TRUE),
    weighted_mean_processing_time = weighted.mean(mean_processing_time, n_permits_applied, na.rm = TRUE),
    .groups = "drop"
  )

scores <- effects %>%
  left_join(alderman_stats, by = "alderman") %>%
  mutate(
    strictness_index = standardize(alderman_fe_shrunk),
    uncertainty_index = strictness_index
  ) %>%
  arrange(strictness_index) %>%
  select(
    alderman,
    n_ward_months,
    total_permits,
    weighted_mean_processing_time,
    alderman_fe_raw,
    alderman_se,
    shrinkage_B,
    alderman_fe_shrunk,
    strictness_index,
    uncertainty_index
  )

# -----------------------------------------------------------------------------
# Output tables
# -----------------------------------------------------------------------------

fe_label <- if (fe_spec == "month") "Month FE" else "Ward + Month FE"
stage1_fixef_group <- if (fe_spec == "month") {
  list("Month FE" = "month")
} else {
  list(
    "Ward FE" = "ward",
    "Month FE" = "month"
  )
}

stage1_dict <- c(
  "dist_cbd_km" = "Distance to CBD (km)",
  "lakefront_share_1km" = "Lakefront Share (1km)",
  "n_rail_stations_800m" = "Rail Stations (800m)",
  "homeownership_rate" = "Homeownership Rate",
  "pop_total_10k" = "Population (10,000s)",
  "median_hh_income_10k" = "Median HH Income ($10,000s)",
  "share_black" = "Black Share",
  "share_hisp" = "Hispanic Share",
  "share_white" = "White Share"
)

etable(
  stage1_model,
  digits = 3,
  se.below = TRUE,
  depvar = FALSE,
  headers = c("Log Mean Processing Time"),
  fitstat = ~ n + r2,
  drop = "^ca_share_",
  dict = stage1_dict,
  file = opt$output_stage1_tex,
  replace = TRUE,
  style.tex = style.tex(
    main = "aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  ),
  fixef.group = stage1_fixef_group
)

insert_ca_controls_row <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (any(grepl("Community Area Share Controls", lines, fixed = TRUE))) return(invisible(NULL))
  anchor_idx <- which(grepl("Month FE", lines, fixed = TRUE))
  if (length(anchor_idx) == 0) anchor_idx <- which(grepl("Stage-1 FE", lines, fixed = TRUE))
  if (length(anchor_idx) == 0) return(invisible(NULL))
  lines <- append(lines, "   Community Area Share Controls    & $\\checkmark$\\\\   ", after = anchor_idx[1])
  writeLines(lines, path)
}
insert_ca_controls_row(opt$output_stage1_tex)

etable(
  stage2_model,
  digits = 3,
  se.below = TRUE,
  depvar = FALSE,
  headers = c("Residualized Processing Time"),
  fitstat = ~ n + r2,
  file = opt$output_stage2_tex,
  replace = TRUE,
  style.tex = style.tex(
    main = "aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  )
)

# -----------------------------------------------------------------------------
# Output chart
# -----------------------------------------------------------------------------

plot_df <- scores %>%
  filter(!is.na(strictness_index)) %>%
  arrange(strictness_index) %>%
  mutate(alderman = factor(alderman, levels = alderman))

p <- ggplot(plot_df, aes(x = strictness_index, y = alderman, fill = strictness_index)) +
  geom_col() +
  scale_fill_distiller(
    palette = "RdYlBu",
    direction = -1,
    name = "Strictness"
  ) +
  labs(
    title = "Alderman Strictness Index",
    subtitle = paste0("Stage 1: ", fe_label, " | Higher = stricter"),
    x = "Strictness Index (standardized EB-shrunk FE)",
    y = NULL
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )

ggsave(opt$output_plot, plot = p, width = 9, height = 13, device = "pdf", bg = "white")

# -----------------------------------------------------------------------------
# Save scores
# -----------------------------------------------------------------------------

write_csv(scores, opt$output_csv)

message("\n=== Strictness score build complete ===")
message("Output CSV: ", opt$output_csv)
message("Stage-1 table: ", opt$output_stage1_tex)
message("Stage-2 table: ", opt$output_stage2_tex)
message("Plot: ", opt$output_plot)
message("Signal variance tau^2: ", round(tau2, 6))
