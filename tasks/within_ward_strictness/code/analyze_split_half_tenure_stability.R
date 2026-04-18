source("../../setup_environment/code/packages.R")
source("../../_lib/alderman_uncertainty_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/within_ward_strictness/code")
# spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"
# permits_input <- "../input/permits_for_uncertainty_index.csv"
# alderman_panel_input <- "../input/chicago_alderman_panel.csv"
# output_scores_csv <- "../output/split_half_stability_scores_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# output_summary_csv <- "../output/split_half_stability_summary_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# output_scatter_pdf <- "../output/split_half_stability_scatter_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(spec, permits_input, alderman_panel_input, output_scores_csv, output_summary_csv, output_scatter_pdf)
}

if (length(args) != 6) {
  stop(
    paste(
      "FATAL: Script requires 6 args:",
      "<spec> <permits_input> <alderman_panel_input>",
      "<output_scores_csv> <output_summary_csv> <output_scatter_pdf>"
    ),
    call. = FALSE
  )
}

spec <- args[1]
permits_input <- args[2]
alderman_panel_input <- args[3]
output_scores_csv <- args[4]
output_summary_csv <- args[5]
output_scatter_pdf <- args[6]

parse_uncertainty_config_from_spec <- function(spec) {
  config <- default_uncertainty_config()
  config$permit_type_fe <- str_detect(spec, "ptfeTRUE")
  config$review_type_fe <- str_detect(spec, "rtfeTRUE")
  config$include_porch <- str_detect(spec, "porchTRUE")
  config$ca_fe <- str_detect(spec, "cafeTRUE")
  config$two_stage <- str_detect(spec, "_2stage")

  volume_match <- str_match(spec, "_vol([^_]+)_([^_]+)")
  if (!is.na(volume_match[1, 1])) {
    config$volume_ctrl <- volume_match[1, 2]
    config$volume_stage <- volume_match[1, 3]
  } else {
    config$volume_ctrl <- "NONE"
    config$volume_stage <- "BOTH"
  }

  config
}

weighted_cor <- function(x, y, w) {
  keep <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(keep) < 2) {
    return(NA_real_)
  }
  x <- x[keep]
  y <- y[keep]
  w <- w[keep] / sum(w[keep])
  cov_xy <- sum(w * (x - sum(w * x)) * (y - sum(w * y)))
  var_x <- sum(w * (x - sum(w * x))^2)
  var_y <- sum(w * (y - sum(w * y))^2)
  cov_xy / sqrt(var_x * var_y)
}

max_permit_year <- str_match(spec, "through([0-9]{4})")[, 2] %>%
  as.integer()
if (!is.finite(max_permit_year)) {
  stop("Could not parse through-year from spec.", call. = FALSE)
}

config <- parse_uncertainty_config_from_spec(spec)

permits <- load_uncertainty_permits(permits_input) %>%
  filter(year <= max_permit_year)

alderman_panel <- read_csv(alderman_panel_input, show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month)) %>%
  filter(month <= as.yearmon(sprintf("%d-12", max_permit_year)))

tenure_ranges <- alderman_panel %>%
  group_by(alderman) %>%
  summarise(
    first_tenure_month = min(month, na.rm = TRUE),
    last_tenure_month = max(month, na.rm = TRUE),
    midpoint_numeric = (as.numeric(first_tenure_month) + as.numeric(last_tenure_month)) / 2,
    .groups = "drop"
  )

permits_split <- permits %>%
  inner_join(tenure_ranges, by = "alderman") %>%
  mutate(
    split_half = if_else(as.numeric(month) <= midpoint_numeric, "first_half", "second_half")
  )

first_result <- build_residualized_uncertainty_index(
  permits = permits_split %>% filter(split_half == "first_half"),
  config = config,
  variant_id = "split_half_first",
  stage1_outcome = "log_processing_time",
  drop_covariates = c("share_bach_plus"),
  construction_rule = "First half of alderman tenure by calendar time"
)

second_result <- build_residualized_uncertainty_index(
  permits = permits_split %>% filter(split_half == "second_half"),
  config = config,
  variant_id = "split_half_second",
  stage1_outcome = "log_processing_time",
  drop_covariates = c("share_bach_plus"),
  construction_rule = "Second half of alderman tenure by calendar time"
)

score_first <- first_result$alderman_index %>%
  transmute(
    alderman,
    n_permits_first = n_permits,
    mean_resid_first = mean_resid,
    uncertainty_index_first = uncertainty_index
  )

score_second <- second_result$alderman_index %>%
  transmute(
    alderman,
    n_permits_second = n_permits,
    mean_resid_second = mean_resid,
    uncertainty_index_second = uncertainty_index
  )

score_compare <- score_first %>%
  inner_join(score_second, by = "alderman") %>%
  mutate(
    harmonic_weight = 2 / ((1 / n_permits_first) + (1 / n_permits_second)),
    rank_first = min_rank(desc(uncertainty_index_first)),
    rank_second = min_rank(desc(uncertainty_index_second)),
    abs_rank_change = abs(rank_first - rank_second)
  ) %>%
  arrange(rank_first)

write_csv(score_compare, output_scores_csv)

summary_rows <- tibble(
  metric = c(
    "pearson_correlation",
    "spearman_correlation",
    "weighted_pearson_harmonic",
    "mean_abs_rank_change",
    "median_abs_rank_change",
    "top20_overlap",
    "n_overlap_aldermen",
    "stage1_nobs_first_half",
    "stage1_nobs_second_half"
  ),
  estimate = c(
    cor(score_compare$uncertainty_index_first, score_compare$uncertainty_index_second, use = "complete.obs"),
    cor(score_compare$uncertainty_index_first, score_compare$uncertainty_index_second, method = "spearman", use = "complete.obs"),
    weighted_cor(score_compare$uncertainty_index_first, score_compare$uncertainty_index_second, score_compare$harmonic_weight),
    mean(score_compare$abs_rank_change, na.rm = TRUE),
    median(score_compare$abs_rank_change, na.rm = TRUE),
    length(intersect(score_compare$alderman[score_compare$rank_first <= 20], score_compare$alderman[score_compare$rank_second <= 20])),
    nrow(score_compare),
    first_result$metadata$stage1_nobs[[1]],
    second_result$metadata$stage1_nobs[[1]]
  )
)

write_csv(summary_rows, output_summary_csv)

plot_pearson <- summary_rows %>% filter(metric == "pearson_correlation") %>% pull(estimate)
annotation_text <- paste0(
  "N = ", nrow(score_compare),
  "\nPearson r = ", formatC(plot_pearson, digits = 3, format = "f")
)

p_scatter <- ggplot(score_compare, aes(x = uncertainty_index_first, y = uncertainty_index_second)) +
  geom_point(color = "#2C7FB8", size = 2.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, color = "#D95F0E", linewidth = 0.7) +
  annotate(
    "text",
    x = Inf,
    y = -Inf,
    label = annotation_text,
    hjust = 1.02,
    vjust = -0.1,
    size = 3.6
  ) +
  labs(
    x = "First-half uncertainty index",
    y = "Second-half uncertainty index",
    title = "Split-Half Stability of Alderman Stringency Index",
    subtitle = "Each half is built from the same residualized score pipeline, split by calendar midpoint of alderman tenure"
  ) +
  theme_minimal(base_size = 12)

ggsave(output_scatter_pdf, p_scatter, width = 7, height = 5)
