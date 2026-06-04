source("../../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_event_study_audit/code")

hedonic_vars <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")
amenity_vars <- c("nearest_school_dist_m", "nearest_park_dist_m", "nearest_major_road_dist_m", "lake_michigan_dist_m")
control_vars <- c(hedonic_vars, amenity_vars)

message("Loading all-valid 2015 sales diagnostic panel...")
data_base <- read_parquet("../input/sales_transaction_panel_2015_all_valid.parquet") %>%
  as_tibble() %>%
  filter(
    relative_year >= -5,
    relative_year <= 5,
    !is.na(strictness_change),
    !is.na(sale_price),
    sale_price > 0,
    !is.na(block_id),
    block_id != "",
    !is.na(ward_origin),
    event_point_origin_mismatch == FALSE
  ) %>%
  mutate(
    ward_origin = as.character(ward_origin),
    ward_pair_id = as.character(ward_pair_id),
    ward_pair_side = as.character(ward_pair_side),
    post_treat = as.integer(relative_year >= 0) * strictness_change,
    weight = 1
  )

fit_sample <- function(df) {
  df %>%
    filter(if_all(all_of(control_vars), ~ is.finite(.x)))
}

summarize_sample <- function(df, sample_name, fe_label) {
  tibble(
    sample = sample_name,
    fe_spec = fe_label,
    n_before_complete_controls = nrow(df),
    n_complete_controls = nrow(fit_sample(df)),
    n_blocks = n_distinct(df$block_id),
    n_pins = n_distinct(df$pin),
    n_switcher_sales = sum(df$treat == 1, na.rm = TRUE),
    n_control_sales = sum(df$treat == 0, na.rm = TRUE),
    n_origin_wards = n_distinct(df$ward_origin),
    n_event_pairs = n_distinct(df$ward_pair_id[!is.na(df$ward_pair_id) & df$ward_pair_id != ""]),
    pct_event_le_1000ft = 100 * mean(df$event_dist_le_1000ft, na.rm = TRUE),
    pct_pre_score_le_1000ft = 100 * mean(df$pre_score_dist_le_1000ft, na.rm = TRUE),
    pct_old_event_1000ft_disagree = 100 * mean(df$old_event_1000ft_disagree, na.rm = TRUE),
    dep_var_mean = mean(df$sale_price, na.rm = TRUE)
  )
}

fit_did <- function(df, sample_name, fe_formula, fe_label) {
  df_model <- fit_sample(df)
  if (nrow(df_model) == 0) {
    stop(sprintf("No complete-control rows for %s.", sample_name), call. = FALSE)
  }

  model <- feols(
    as.formula(sprintf(
      "log(sale_price) ~ post_treat + %s | %s",
      paste(control_vars, collapse = " + "),
      fe_formula
    )),
    data = df_model,
    weights = ~weight,
    cluster = ~block_id
  )

  tibble(
    sample = sample_name,
    fe_spec = fe_label,
    estimate = coef(model)[["post_treat"]],
    std_error = se(model)[["post_treat"]],
    p_value = pvalue(model)[["post_treat"]],
    percent_effect = 100 * (exp(coef(model)[["post_treat"]]) - 1),
    n_obs = nobs(model),
    n_blocks = n_distinct(df_model$block_id),
    n_pins = n_distinct(df_model$pin),
    n_switcher_sales = sum(df_model$treat == 1, na.rm = TRUE),
    n_control_sales = sum(df_model$treat == 0, na.rm = TRUE),
    n_origin_wards = n_distinct(df_model$ward_origin),
    n_event_pairs = n_distinct(df_model$ward_pair_id[!is.na(df_model$ward_pair_id) & df_model$ward_pair_id != ""]),
    dep_var_mean = mean(df_model$sale_price, na.rm = TRUE)
  )
}

fit_event_study <- function(df, sample_name, fe_formula, fe_label) {
  df_model <- fit_sample(df)
  model <- feols(
    as.formula(sprintf(
      "log(sale_price) ~ i(relative_year, strictness_change, ref = -1) + %s | %s",
      paste(control_vars, collapse = " + "),
      fe_formula
    )),
    data = df_model,
    weights = ~weight,
    cluster = ~block_id
  )

  terms <- names(coef(model))
  keep <- grepl("^relative_year::", terms)
  tibble(
    sample = sample_name,
    fe_spec = fe_label,
    term = terms[keep],
    relative_year = as.integer(sub("^relative_year::(-?[0-9]+):.*$", "\\1", terms[keep])),
    estimate = coef(model)[keep],
    std_error = se(model)[keep],
    n_obs = nobs(model)
  )
}

all_valid <- data_base
all_valid_event_pair <- data_base %>%
  filter(
    !is.na(ward_pair_id),
    ward_pair_id != "",
    !is.na(ward_pair_side),
    ward_pair_side != ""
  )
boundary_1000ft_event_pair <- all_valid_event_pair %>%
  filter(event_dist_le_1000ft)
redistricted_only <- data_base %>%
  filter(treat == 1)
redistricted_event_pair <- redistricted_only %>%
  filter(!is.na(ward_pair_id), ward_pair_id != "")

variants <- list(
  list(
    sample_name = "all_valid_origin_year",
    fe_label = "Origin ward FE + year FE",
    fe_formula = "ward_origin + sale_year",
    data = all_valid
  ),
  list(
    sample_name = "all_valid_origin_x_year",
    fe_label = "Origin ward x year FE",
    fe_formula = "ward_origin^sale_year",
    data = all_valid
  ),
  list(
    sample_name = "all_valid_event_pair_x_year",
    fe_label = "Event pair side FE + event pair x year FE, no distance filter",
    fe_formula = "ward_pair_side + ward_pair_id^sale_year",
    data = all_valid_event_pair
  ),
  list(
    sample_name = "all_valid_block_year",
    fe_label = "Block FE + year FE, no distance filter",
    fe_formula = "block_id + sale_year",
    data = all_valid
  ),
  list(
    sample_name = "all_valid_block_event_pair_x_year",
    fe_label = "Block FE + event pair x year FE, no distance filter",
    fe_formula = "block_id + ward_pair_id^sale_year",
    data = all_valid_event_pair
  ),
  list(
    sample_name = "boundary_1000ft_block_event_pair_x_year",
    fe_label = "Block FE + event pair x year FE, event distance <= 1000ft",
    fe_formula = "block_id + ward_pair_id^sale_year",
    data = boundary_1000ft_event_pair
  ),
  list(
    sample_name = "redistricted_only_origin_year",
    fe_label = "Origin ward FE + year FE, switchers only",
    fe_formula = "ward_origin + sale_year",
    data = redistricted_only
  ),
  list(
    sample_name = "redistricted_only_pair_year",
    fe_label = "Origin-destination pair FE + year FE, switchers only",
    fe_formula = "ward_pair_id + sale_year",
    data = redistricted_event_pair
  ),
  list(
    sample_name = "redistricted_only_block_year",
    fe_label = "Block FE + year FE, switchers only",
    fe_formula = "block_id + sale_year",
    data = redistricted_only
  ),
  list(
    sample_name = "redistricted_only_block_pair_x_year",
    fe_label = "Block FE + origin-destination pair x year FE, switchers only",
    fe_formula = "block_id + ward_pair_id^sale_year",
    data = redistricted_event_pair
  )
)

sample_summary <- bind_rows(lapply(variants, function(v) {
  summarize_sample(v$data, v$sample_name, v$fe_label)
}))

did_results <- bind_rows(lapply(variants, function(v) {
  fit_did(v$data, v$sample_name, v$fe_formula, v$fe_label)
}))

event_results <- bind_rows(lapply(variants, function(v) {
  fit_event_study(v$data, v$sample_name, v$fe_formula, v$fe_label)
}))

plot_results <- event_results %>%
  mutate(
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error
  )
reference_rows <- plot_results %>%
  distinct(sample, fe_spec, n_obs) %>%
  mutate(
    term = "reference",
    relative_year = -1L,
    estimate = 0,
    std_error = 0,
    conf_low = 0,
    conf_high = 0
  )
plot_results <- bind_rows(plot_results, reference_rows) %>%
  arrange(sample, relative_year)

make_event_plot <- function(df, title_text) {
  ggplot(df, aes(x = relative_year, y = estimate)) +
    geom_hline(yintercept = 0, color = "gray45", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, color = "gray65", linetype = "dashed", linewidth = 0.35) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.18, color = "#555555") +
    geom_point(size = 2.1, color = "#1f78b4") +
    scale_x_continuous(breaks = -5:5) +
    labs(
      x = "Event year relative to 2015 redistricting",
      y = "Coefficient on stringency change",
      title = title_text,
      subtitle = "Reference year is -1. 95% confidence intervals use block-clustered standard errors."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray30")
    )
}

event_plot <- ggplot(plot_results, aes(x = relative_year, y = estimate)) +
  geom_hline(yintercept = 0, color = "gray45", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, color = "gray65", linetype = "dashed", linewidth = 0.35) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.18, color = "#555555") +
  geom_point(size = 1.7, color = "#1f78b4") +
  facet_wrap(
    vars(fe_spec),
    ncol = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(breaks = -5:5) +
  labs(
    x = "Event year relative to 2015 redistricting",
    y = "Coefficient on stringency change",
    title = "All-Valid 2015 Sales Event-Study Diagnostics",
    subtitle = "Reference year is -1. 95% confidence intervals use block-clustered standard errors."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30")
  )

individual_plot_specs <- plot_results %>%
  distinct(sample, fe_spec) %>%
  arrange(sample)

for (i in seq_len(nrow(individual_plot_specs))) {
  sample_i <- individual_plot_specs$sample[i]
  fe_i <- individual_plot_specs$fe_spec[i]
  plot_i <- make_event_plot(
    plot_results %>% filter(sample == sample_i),
    fe_i
  )
  ggsave(
    sprintf("../output/all_redistricted_sales_2015_event_study_%s.pdf", sample_i),
    plot_i,
    width = 8,
    height = 5,
    bg = "white"
  )
}

write_csv(sample_summary, "../output/all_redistricted_sales_2015_sample_summary.csv")
write_csv(did_results, "../output/all_redistricted_sales_2015_did.csv")
write_csv(event_results, "../output/all_redistricted_sales_2015_event_coefficients.csv")
ggsave("../output/all_redistricted_sales_2015_event_studies.pdf", event_plot, width = 8, height = 12, bg = "white")
ggsave("../output/all_redistricted_sales_2015_event_studies.png", event_plot, width = 8, height = 12, dpi = 180, bg = "white")

message("Saved all-redistricted sales diagnostics.")
print(did_results)
