# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")
# bandwidth_ft <- 500

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft)
}
if (length(cli_args) != 1) {
  stop("Script requires a bandwidth in feet.", call. = FALSE)
}
bandwidth_ft <- as.integer(cli_args[1])
if (!bandwidth_ft %in% c(500L, 1000L)) {
  stop("Bandwidth must be 500 or 1000 feet.", call. = FALSE)
}
bandwidth_m <- bandwidth_ft * 0.3048
output_suffix <- if_else(bandwidth_ft == 500L, "", paste0("_", bandwidth_ft, "ft"))

scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2014L, spec_id == "controls_drop_income") %>%
  select(alderman, score)

if (anyDuplicated(scores$alderman) > 0) {
  stop("The through-2014 no-income score is not unique by alderman.", call. = FALSE)
}

block_treatment <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(year == 2014L, valid) %>%
  distinct(
    block_id, alderman_origin_2014, alderman_dest_2014,
    ward_origin_corrected = ward_origin, ward_dest_corrected = ward_dest,
    strictness_change_original = strictness_change_frozen
  ) %>%
  left_join(
    scores %>% rename(alderman_origin_2014 = alderman, strictness_origin = score),
    by = "alderman_origin_2014",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_dest_2014 = alderman, strictness_destination = score),
    by = "alderman_dest_2014",
    relationship = "many-to-one"
  ) %>%
  mutate(strictness_change_drop_income = strictness_destination - strictness_origin)

if (anyDuplicated(block_treatment$block_id) > 0) {
  stop("The corrected treatment data are not unique by census block.", call. = FALSE)
}
if (anyNA(block_treatment$strictness_change_drop_income)) {
  stop("The no-income score is missing for a 2014 origin or destination alderman.", call. = FALSE)
}

control_vars <- c(
  "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage",
  "nearest_school_dist_m", "nearest_park_dist_m", "nearest_major_road_dist_m", "lake_michigan_dist_m"
)

panel <- read_parquet("../input/sales_transaction_panel_2015.parquet") %>%
  select(-any_of(c("strictness_change", "treatment_continuous"))) %>%
  left_join(block_treatment, by = "block_id", relationship = "many-to-one") %>%
  filter(
    dist_m <= bandwidth_m,
    relative_year >= -5L,
    relative_year <= 5L,
    sale_price > 0,
    !is.na(strictness_change_drop_income),
    !is.na(ward_pair_id),
    ward_pair_id != "",
    if_all(all_of(control_vars), is.finite)
  ) %>%
  mutate(strictness_change = strictness_change_drop_income)

if (any(panel$ward_origin != panel$ward_origin_corrected, na.rm = TRUE) ||
    any(panel$ward_dest != panel$ward_dest_corrected, na.rm = TRUE)) {
  stop("The sales and corrected block panels disagree on a ward assignment.", call. = FALSE)
}

pre_price <- panel %>%
  filter(relative_year < 0L) %>%
  group_by(block_id) %>%
  summarise(pre_period_log_price = mean(log(sale_price)), .groups = "drop")

panel <- panel %>%
  left_join(pre_price, by = "block_id", relationship = "many-to-one") %>%
  mutate(
    no_pre_period_sale = as.integer(is.na(pre_period_log_price)),
    pre_period_log_price = replace_na(pre_period_log_price, 0)
  )

control_sets <- tribble(
  ~control_set, ~control_label, ~pre_controls,
  "hedonics_amenities", "Hedonics and amenities", "",
  "plus_pre_price", "Plus pre-period price controls",
  " + pre_period_log_price:factor(sale_year) + no_pre_period_sale:factor(sale_year)"
)

event_rows <- list()
summary_rows <- list()
plots <- vector("list", nrow(control_sets))

for (control_i in seq_len(nrow(control_sets))) {
  formula_event <- as.formula(sprintf(
    "log(sale_price) ~ i(relative_year, strictness_change, ref = -1) + %s%s | block_id + ward_pair_id^sale_year",
    paste(control_vars, collapse = " + "),
    control_sets$pre_controls[control_i]
  ))
  event_model <- feols(
    formula_event,
    data = panel,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  event_estimates <- iplot(event_model, .plot = FALSE)[[1]] %>%
    as_tibble() %>%
    transmute(
      control_set = control_sets$control_set[control_i],
      event_time = as.integer(x),
      estimate,
      std_error = if_else(is_ref, 0, (ci_high - estimate) / qnorm(0.975)),
      ci_low,
      ci_high,
      is_reference = is_ref
    ) %>%
    filter(event_time >= -5L, event_time <= 5L)

  pretrend_p_value <- wald(
    event_model,
    "^relative_year::-[2-5]:strictness_change$",
    print = FALSE
  )$p

  pooled_model <- feols(
    as.formula(sprintf(
      "log(sale_price) ~ post_treat + %s%s | block_id + ward_pair_id^sale_year",
      paste(control_vars, collapse = " + "),
      control_sets$pre_controls[control_i]
    )),
    data = panel %>%
      mutate(post_treat = as.integer(relative_year >= 0L) * strictness_change),
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  pooled_estimate <- coef(pooled_model)[["post_treat"]]
  pooled_se <- se(pooled_model)[["post_treat"]]
  pooled_p <- pvalue(pooled_model)[["post_treat"]]
  pooled_stars <- case_when(
    pooled_p <= 0.01 ~ "***",
    pooled_p <= 0.05 ~ "**",
    pooled_p <= 0.10 ~ "*",
    TRUE ~ ""
  )

  plots[[control_i]] <- ggplot(event_estimates, aes(event_time, estimate)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#B8D8CF", color = NA) +
    geom_line(color = "#176B58", linewidth = 0.9) +
    geom_point(color = "#176B58", size = 2.2) +
    scale_x_continuous(breaks = -5:5) +
    labs(
      title = "Home sale prices",
      subtitle = sprintf(
        "%s\nPooled estimate = %.3f%s (SE %.3f)",
        control_sets$control_label[control_i], pooled_estimate, pooled_stars, pooled_se
      ),
      x = "Years relative to the 2015 ward remap",
      y = "Effect of a 1 SD increase in assigned stringency",
      caption = paste0(
        "OLS transaction-level event study with block and ward-pair-by-year fixed effects.\n",
        sprintf(
          "%dft sample; score estimated through 2014 without median income; ward-pair clustering.",
          bandwidth_ft
        )
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0),
      plot.margin = margin(10, 20, 10, 10)
    )

  ggsave(
    file.path(
      "../output",
      paste0("sales_drop_income_event_", control_sets$control_set[control_i], output_suffix, ".png")
    ),
    plots[[control_i]],
    width = 7.6,
    height = 5.2,
    dpi = 220,
    bg = "white"
  )

  event_rows[[control_i]] <- event_estimates %>%
    select(control_set, event_time, estimate, std_error, ci_low, ci_high)
  summary_rows[[control_i]] <- tibble(
    control_set = control_sets$control_set[control_i],
    bandwidth_ft,
    pooled_estimate,
    pooled_se,
    pooled_p,
    proportional_effect = expm1(pooled_estimate),
    observations = nobs(pooled_model),
    sample_blocks = n_distinct(panel$block_id),
    sample_ward_pairs = n_distinct(panel$ward_pair_id),
    pretrend_p_value
  )
}

ggsave(
  paste0("../output/sales_drop_income_event_2panel", output_suffix, ".png"),
  plots[[1]] / plots[[2]],
  width = 7.8,
  height = 10.4,
  dpi = 220,
  bg = "white"
)
write_csv(
  bind_rows(event_rows),
  paste0("../output/sales_drop_income_event_estimates", output_suffix, ".csv")
)
write_csv(
  bind_rows(summary_rows),
  paste0("../output/sales_drop_income_event_summary", output_suffix, ".csv")
)

comparison_rows <- list()
comparison_i <- 1L
for (score_spec in c("original", "drop_income")) {
  for (control_i in seq_len(nrow(control_sets))) {
    comparison_data <- panel %>%
      mutate(
        comparison_change = if (score_spec == "original") {
          strictness_change_original
        } else {
          strictness_change_drop_income
        },
        post_treat = as.integer(relative_year >= 0L) * comparison_change
      )
    comparison_model <- feols(
      as.formula(sprintf(
        "log(sale_price) ~ post_treat + %s%s | block_id + ward_pair_id^sale_year",
        paste(control_vars, collapse = " + "),
        control_sets$pre_controls[control_i]
      )),
      data = comparison_data,
      cluster = ~ward_pair_id,
      notes = FALSE
    )
    comparison_rows[[comparison_i]] <- tibble(
      score_spec,
      control_set = control_sets$control_set[control_i],
      bandwidth_ft,
      estimate = coef(comparison_model)[["post_treat"]],
      std_error = se(comparison_model)[["post_treat"]],
      p_value = pvalue(comparison_model)[["post_treat"]],
      observations = nobs(comparison_model)
    )
    comparison_i <- comparison_i + 1L
  }
}
write_csv(
  bind_rows(comparison_rows),
  paste0("../output/sales_drop_income_score_comparison", output_suffix, ".csv")
)
