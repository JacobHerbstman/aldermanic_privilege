source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_disaggregate/code")

bandwidth_m <- 304.8
hedonic_vars <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")
amenity_vars <- c("nearest_school_dist_m", "nearest_park_dist_m", "nearest_major_road_dist_m", "lake_michigan_dist_m")
control_vars <- c(hedonic_vars, amenity_vars)

message("Loading boundary-local origin-destination sales panel...")
data_base <- read_parquet("../input/sales_transaction_panel_2015.parquet") %>%
  as_tibble() %>%
  filter(
    dist_m <= bandwidth_m,
    relative_year >= -5,
    relative_year <= 5,
    !is.na(strictness_change),
    !is.na(sale_price),
    sale_price > 0,
    !is.na(block_id),
    block_id != "",
    !is.na(ward_pair_id),
    ward_pair_id != "",
    !is.na(ward_pair_side),
    ward_pair_side != ""
  ) %>%
  mutate(
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
    n_treated_sales = sum(df$treat == 1, na.rm = TRUE),
    n_control_sales = sum(df$treat == 0, na.rm = TRUE),
    n_event_pairs = n_distinct(df$ward_pair_id),
    dep_var_mean = mean(df$sale_price, na.rm = TRUE),
    mean_dist_m = mean(df$dist_m, na.rm = TRUE),
    max_dist_m = max(df$dist_m, na.rm = TRUE)
  )
}

fit_did <- function(df, sample_name, fe_formula, fe_label) {
  df_model <- fit_sample(df)
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
    n_treated_sales = sum(df_model$treat == 1, na.rm = TRUE),
    n_control_sales = sum(df_model$treat == 0, na.rm = TRUE),
    n_event_pairs = n_distinct(df_model$ward_pair_id),
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

variants <- list(
  list(
    sample_name = "border_pair_side_pair_year",
    fe_label = "Border-pair side FE + border-pair x year FE",
    fe_formula = "ward_pair_side + ward_pair_id^sale_year"
  ),
  list(
    sample_name = "block_pair_year",
    fe_label = "Block FE + border-pair x year FE",
    fe_formula = "block_id + ward_pair_id^sale_year"
  ),
  list(
    sample_name = "block_year",
    fe_label = "Block FE + year FE",
    fe_formula = "block_id + sale_year"
  )
)

sample_summary <- bind_rows(lapply(variants, function(v) {
  summarize_sample(data_base, v$sample_name, v$fe_label)
}))

did_results <- bind_rows(lapply(variants, function(v) {
  fit_did(data_base, v$sample_name, v$fe_formula, v$fe_label)
}))

event_results <- bind_rows(lapply(variants, function(v) {
  fit_event_study(data_base, v$sample_name, v$fe_formula, v$fe_label)
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

combined_plot <- ggplot(plot_results, aes(x = relative_year, y = estimate)) +
  geom_hline(yintercept = 0, color = "gray45", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, color = "gray65", linetype = "dashed", linewidth = 0.35) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.18, color = "#555555") +
  geom_point(size = 1.9, color = "#1f78b4") +
  facet_wrap(vars(fe_spec), ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = -5:5) +
  labs(
    x = "Event year relative to 2015 redistricting",
    y = "Coefficient on stringency change",
    title = "Boundary-Local Origin-Destination Sales Diagnostics",
    subtitle = "2015 sales within 1000ft of the event boundary. Reference year is -1."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30")
  )

for (v in variants) {
  plot_i <- make_event_plot(
    plot_results %>% filter(sample == v$sample_name),
    v$fe_label
  )
  ggsave(
    sprintf("../output/boundary_origin_destination_sales_2015_event_study_%s.pdf", v$sample_name),
    plot_i,
    width = 8,
    height = 5,
    bg = "white"
  )
}

write_csv(sample_summary, "../output/boundary_origin_destination_sales_2015_sample_summary.csv")
write_csv(did_results, "../output/boundary_origin_destination_sales_2015_did.csv")
write_csv(event_results, "../output/boundary_origin_destination_sales_2015_event_coefficients.csv")
ggsave("../output/boundary_origin_destination_sales_2015_event_studies.pdf", combined_plot, width = 8, height = 9, bg = "white")
ggsave("../output/boundary_origin_destination_sales_2015_event_studies.png", combined_plot, width = 8, height = 9, dpi = 180, bg = "white")

message("Saved boundary-local origin-destination sales diagnostics.")
print(did_results)
