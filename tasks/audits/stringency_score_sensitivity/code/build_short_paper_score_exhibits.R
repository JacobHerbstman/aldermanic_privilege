# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")

scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L, spec_id == "controls_drop_income") %>%
  transmute(
    alderman,
    alderman_key = str_squish(str_to_lower(alderman)),
    uncertainty_index = score,
    n_permits
  )

if (anyDuplicated(scores$alderman_key) > 0) {
  stop("The through-2022 no-income score is not unique by alderman.", call. = FALSE)
}

stage1_terms <- read_csv("../input/score_stage1_terms.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L, spec_id == "controls_drop_income")
metadata <- read_csv("../input/score_metadata.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L, spec_id == "controls_drop_income")

if (nrow(metadata) != 1L || nrow(stage1_terms) == 0L) {
  stop("The through-2022 no-income first-stage results are incomplete.", call. = FALSE)
}

term_labels <- c(
  share_black = "Black population share",
  share_hisp = "Hispanic population share",
  share_white = "White population share",
  homeownership_rate = "Homeownership rate",
  pop_total_10k = "Population (10,000s)",
  dist_cbd_km = "Distance to CBD (km)",
  dist_lake_km = "Distance to Lake Michigan (km)",
  n_rail_stations_800m = "Rail stations within 800m",
  n_permits_wm_l1 = "Lagged high-discretion workload"
)

format_stars <- function(p_value) {
  case_when(
    p_value <= 0.01 ~ "***",
    p_value <= 0.05 ~ "**",
    p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )
}

table_rows <- stage1_terms %>%
  mutate(
    label = coalesce(unname(term_labels[term]), term),
    estimate_display = sprintf("%.4f%s", estimate, format_stars(p_value)),
    se_display = sprintf("(%.4f)", std_error)
  ) %>%
  arrange(match(term, names(term_labels)))

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lc}",
  "\\toprule",
  " & Log processing days \\\\",
  "\\midrule"
)
for (row_i in seq_len(nrow(table_rows))) {
  table_lines <- c(
    table_lines,
    sprintf("%s & %s \\\\", table_rows$label[row_i], table_rows$estimate_display[row_i]),
    sprintf(" & %s \\\\", table_rows$se_display[row_i])
  )
}
table_lines <- c(
  table_lines,
  "\\midrule",
  "Permit type FE & Yes \\\\",
  "Review type FE & Yes \\\\",
  "Year $\\times$ month FE & Yes \\\\",
  sprintf("Observations & %s \\\\", format(metadata$stage1_nobs, big.mark = ",", scientific = FALSE)),
  sprintf("$R^2$ & %.3f \\\\", metadata$stage1_r2),
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\par\\vspace{0.5em}\\parbox{0.92\\linewidth}{\\footnotesize Notes: ",
    "The outcome is log processing time among positive-duration high-discretion permits through December 2022. ",
    "Lagged workload counts all high-discretion applications, including same-day permits. ",
    "Median income and bachelor's share are omitted from this score specification. ",
    "The listed controls are included with permit-type, review-type, and year-month fixed effects. ",
    "Regressions are unweighted; standard errors are in parentheses. ",
    "* $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
  ),
  "\\endgroup"
)
writeLines(table_lines, "../output/drop_income_stage1_regression.tex")

score_plot_data <- scores %>%
  arrange(uncertainty_index) %>%
  mutate(alderman = factor(alderman, levels = alderman))
score_plot <- ggplot(
  score_plot_data,
  aes(uncertainty_index, alderman, fill = uncertainty_index)
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7) +
  scale_fill_distiller(
    palette = "RdYlBu",
    direction = -1,
    name = "Stringency Index"
  ) +
  labs(
    title = "Alderman Stringency Index: No-Income Specification",
    x = "Stringency index (standardized mean residual)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )
ggsave(
  "../output/drop_income_score_distribution.pdf",
  score_plot,
  width = 11.5,
  height = 8,
  bg = "white"
)
ggsave(
  "../output/drop_income_score_distribution.png",
  score_plot,
  width = 11.5,
  height = 8,
  dpi = 180,
  bg = "white"
)

alderman_panel <- read_csv(
  "../input/chicago_alderman_panel.csv",
  show_col_types = FALSE
) %>%
  mutate(
    month = as.yearmon(month),
    alderman_key = str_squish(str_to_lower(alderman))
  )
if (anyDuplicated(alderman_panel[c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}

ward_scores <- alderman_panel %>%
  filter(month == as.yearmon("2022-01")) %>%
  select(ward, alderman, alderman_key) %>%
  left_join(
    scores %>% select(alderman_key, uncertainty_index),
    by = "alderman_key",
    relationship = "many-to-one"
  )
if (anyNA(ward_scores$uncertainty_index)) {
  stop("A January 2022 ward alderman is missing a no-income score.", call. = FALSE)
}

ward_map <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  filter(year == 2022L) %>%
  left_join(
    ward_scores %>% select(ward, alderman, uncertainty_index),
    by = "ward",
    relationship = "one-to-one"
  )
map_plot <- ggplot(ward_map) +
  geom_sf(aes(fill = uncertainty_index), color = "grey20", linewidth = 0.2) +
  scale_fill_distiller(
    palette = "RdYlBu",
    direction = -1,
    name = "Stringency Index",
    na.value = "grey90"
  ) +
  labs(title = "Regulatory Stringency Index by Ward (January 2022)") +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )
ggsave("../output/drop_income_score_map.pdf", map_plot, width = 8, height = 10)
ggsave(
  "../output/drop_income_score_map.png",
  map_plot,
  width = 8,
  height = 10,
  dpi = 180,
  bg = "white"
)

turnover_pairs <- alderman_panel %>%
  filter(month >= as.yearmon("2006-01")) %>%
  group_by(ward, alderman, alderman_key) %>%
  summarise(first_month = min(month), .groups = "drop") %>%
  arrange(ward, first_month) %>%
  group_by(ward) %>%
  mutate(
    successor = lead(alderman),
    successor_key = lead(alderman_key),
    predecessor = alderman,
    predecessor_key = alderman_key
  ) %>%
  ungroup() %>%
  filter(!is.na(successor)) %>%
  inner_join(
    scores %>%
      select(alderman_key, predecessor_score = uncertainty_index),
    by = c("predecessor_key" = "alderman_key"),
    relationship = "many-to-one"
  ) %>%
  inner_join(
    scores %>%
      select(alderman_key, successor_score = uncertainty_index),
    by = c("successor_key" = "alderman_key"),
    relationship = "many-to-one"
  )

turnover_plot <- ggplot(
  turnover_pairs,
  aes(predecessor_score, successor_score)
) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#2171B5", alpha = 0.2) +
  geom_point(size = 3, alpha = 0.7, color = "#E41A1C") +
  labs(
    x = "Predecessor stringency index",
    y = "Successor stringency index",
    title = "Stringency Persistence Across Alderman Turnovers"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
ggsave(
  "../output/drop_income_predecessor_successor.pdf",
  turnover_plot,
  width = 6,
  height = 6
)
ggsave(
  "../output/drop_income_predecessor_successor.png",
  turnover_plot,
  width = 6,
  height = 6,
  dpi = 180,
  bg = "white"
)

predecessor_model <- lm(successor_score ~ predecessor_score, data = turnover_pairs)
predecessor_summary <- tibble(
  pairs = nrow(turnover_pairs),
  correlation = cor(
    turnover_pairs$predecessor_score,
    turnover_pairs$successor_score
  ),
  slope = coef(predecessor_model)[["predecessor_score"]],
  slope_se = sqrt(vcov(predecessor_model)["predecessor_score", "predecessor_score"]),
  slope_p = summary(predecessor_model)$coefficients["predecessor_score", "Pr(>|t|)"]
)
write_csv(predecessor_summary, "../output/drop_income_predecessor_successor_summary.csv")

low_discretion <- read_csv(
  "../input/residualized_low_vs_high_processing_alderman_wide.csv",
  show_col_types = FALSE
) %>%
  mutate(alderman_key = str_squish(str_to_lower(alderman))) %>%
  inner_join(
    scores %>% select(alderman_key, uncertainty_index),
    by = "alderman_key",
    relationship = "one-to-one"
  ) %>%
  filter(is.finite(mean_resid_low), is.finite(uncertainty_index))

low_model <- feols(
  mean_resid_low ~ uncertainty_index,
  data = low_discretion,
  se = "hetero",
  warn = FALSE
)
low_table <- coeftable(low_model)
low_result <- tibble(
  estimate = unname(low_table["uncertainty_index", "Estimate"]),
  std_error = unname(low_table["uncertainty_index", "Std. Error"]),
  p_value = unname(low_table["uncertainty_index", "Pr(>|t|)"]),
  aldermen = nobs(low_model)
) %>%
  mutate(stars = format_stars(p_value))
write_csv(low_result, "../output/drop_income_low_discretion_placebo.csv")
writeLines(
  c(
    "\\begingroup",
    "\\centering",
    "\\begin{tabular}{lc}",
    "\\toprule",
    " & Low-discretion log days \\\\",
    "\\midrule",
    sprintf("Stringency index & %.3f%s \\\\", low_result$estimate, low_result$stars),
    sprintf(" & (%.3f) \\\\", low_result$std_error),
    "\\midrule",
    sprintf("Alderman observations & %d \\\\", low_result$aldermen),
    "Heteroskedasticity-robust SEs & Yes \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\endgroup"
  ),
  "../output/drop_income_low_discretion_placebo.tex"
)

write_csv(
  scores %>% select(alderman, uncertainty_index, n_permits),
  "../output/drop_income_short_paper_scores.csv"
)
