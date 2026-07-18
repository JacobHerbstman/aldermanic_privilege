# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")

scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2014L, spec_id == "controls_drop_income") %>%
  select(alderman, score)

if (anyDuplicated(scores$alderman) > 0) {
  stop("The through-2014 no-income score is not unique by alderman.", call. = FALSE)
}

permit_panel <- read_parquet("../input/permit_block_year_panel_2015.parquet")

pre_permit_history <- permit_panel %>%
  filter(year >= 2011L, year <= 2013L) %>%
  group_by(block_id) %>%
  summarise(
    pre_new_construction_count = sum(n_new_construction_issue),
    pre_high_discretion_count = sum(n_high_discretion_application),
    .groups = "drop"
  )

panel <- permit_panel %>%
  filter(
    valid,
    dist_m <= 152.4,
    relative_year >= -4L,
    relative_year <= 5L,
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) %>%
  select(
    block_id, year, relative_year, ward_origin, ward_dest, ward_pair_id, dist_m,
    alderman_origin_2014, alderman_dest_2014,
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
  left_join(pre_permit_history, by = "block_id", relationship = "many-to-one") %>%
  mutate(
    strictness_change = strictness_destination - strictness_origin,
    pre_new_construction_count = replace_na(pre_new_construction_count, 0),
    pre_high_discretion_count = replace_na(pre_high_discretion_count, 0)
  )

if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("The corrected rezoning base is not unique by census block and year.", call. = FALSE)
}
if (anyNA(panel$strictness_change)) {
  stop("The no-income score is missing for a 2014 origin or destination alderman.", call. = FALSE)
}

rezonings <- read_csv(
  "../input/rezoning_census_blocks_20101101_20201231.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(
    matter_id,
    year = as.integer(format(as.Date(matter_passed_date), "%Y")),
    far_pair_status,
    far_change = as.numeric(far_change),
    is_upzone = tolower(is_upzone) == "true"
  )

if (anyDuplicated(rezonings$matter_id) > 0) {
  stop("Rezoning matters are not unique by matter ID.", call. = FALSE)
}

bridge <- read_csv(
  "../input/rezoning_matter_block_bridge_20101101_20201231.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(matter_id, block_id = census_block_id)

if (anyDuplicated(bridge[c("matter_id", "block_id")]) > 0) {
  stop("The rezoning matter-block bridge contains duplicate pairs.", call. = FALSE)
}

rezoning_block_year <- bridge %>%
  left_join(rezonings, by = "matter_id", relationship = "many-to-one") %>%
  filter(!is.na(year)) %>%
  group_by(block_id, year) %>%
  summarise(
    n_rezoning = n_distinct(matter_id),
    n_classified = n_distinct(matter_id[far_pair_status == "resolved_both"]),
    all_rezonings_classified = all(far_pair_status == "resolved_both"),
    n_upzone = n_distinct(matter_id[far_pair_status == "resolved_both" & is_upzone %in% TRUE]),
    far_change_total_observed = sum(far_change[far_pair_status == "resolved_both"], na.rm = TRUE),
    .groups = "drop"
  )

pre_rezoning_history <- rezoning_block_year %>%
  filter(year >= 2011L, year <= 2013L) %>%
  group_by(block_id) %>%
  summarise(
    pre_rezoning_count = sum(n_rezoning),
    pre_upzone_count = sum(n_upzone),
    pre_far_change_total = sum(far_change_total_observed),
    pre_unclassified_count = sum(n_rezoning - n_classified),
    .groups = "drop"
  )

panel <- panel %>%
  left_join(rezoning_block_year, by = c("block_id", "year"), relationship = "one-to-one") %>%
  left_join(pre_rezoning_history, by = "block_id", relationship = "many-to-one") %>%
  mutate(
    across(
      c(pre_rezoning_count, pre_upzone_count, pre_far_change_total, pre_unclassified_count),
      ~replace_na(.x, 0)
    ),
    no_pre_rezonings = as.integer(pre_rezoning_count == 0L),
    any_rezoning = as.integer(coalesce(n_rezoning, 0L) > 0L),
    any_upzone = case_when(
      is.na(n_rezoning) ~ 0,
      n_upzone > 0L ~ 1,
      all_rezonings_classified ~ 0,
      TRUE ~ NA_real_
    ),
    far_change_total = case_when(
      is.na(n_rezoning) ~ 0,
      all_rezonings_classified ~ far_change_total_observed,
      TRUE ~ NA_real_
    )
  )

outcome_specs <- tribble(
  ~outcome, ~title, ~filename,
  "any_rezoning", "Probability of any rezoning", "rezoning_drop_income_event_any_rezoning.png",
  "any_upzone", "Probability of any upzoning", "rezoning_drop_income_event_any_upzone.png",
  "far_change_total", "Total permitted FAR change", "rezoning_drop_income_event_far_change_total.png"
)
control_sets <- tribble(
  ~control_set, ~control_label, ~pre_controls,
  "none", "No pre-period controls", "",
  "pre_history", "Predetermined rezoning and permit history",
  paste0(
    " + pre_rezoning_count:factor(year)",
    " + no_pre_rezonings:factor(year)",
    " + pre_upzone_count:factor(year)",
    " + pre_far_change_total:factor(year)",
    " + pre_unclassified_count:factor(year)",
    " + pre_new_construction_count:factor(year)",
    " + pre_high_discretion_count:factor(year)"
  )
)

event_rows <- list()
summary_rows <- list()
controlled_plots <- vector("list", nrow(outcome_specs))
row_i <- 1L

for (outcome_i in seq_len(nrow(outcome_specs))) {
  outcome_name <- outcome_specs$outcome[outcome_i]

  for (control_i in seq_len(nrow(control_sets))) {
    model_data <- panel %>% filter(!is.na(.data[[outcome_name]]))
    event_model <- feols(
      as.formula(sprintf(
        "%s ~ i(relative_year, strictness_change, ref = -1)%s | block_id + ward_pair_id^year",
        outcome_name,
        control_sets$pre_controls[control_i]
      )),
      data = model_data,
      cluster = ~ward_pair_id,
      notes = FALSE
    )

    event_estimates <- iplot(event_model, .plot = FALSE)[[1]] %>%
      as_tibble() %>%
      transmute(
        outcome = outcome_name,
        control_set = control_sets$control_set[control_i],
        event_time = as.integer(x),
        estimate,
        std_error = if_else(is_ref, 0, (ci_high - estimate) / qnorm(0.975)),
        ci_low,
        ci_high,
        is_reference = is_ref
      ) %>%
      filter(event_time >= -4L, event_time <= 5L)

    pretrend_p_value <- wald(
      event_model,
      "^relative_year::-[2-4]:strictness_change$",
      print = FALSE
    )$p

    pooled_model <- feols(
      as.formula(sprintf(
        "%s ~ post_treat%s | block_id + ward_pair_id^year",
        outcome_name,
        control_sets$pre_controls[control_i]
      )),
      data = model_data %>%
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

    if (control_sets$control_set[control_i] == "pre_history") {
      controlled_plots[[outcome_i]] <- ggplot(event_estimates, aes(event_time, estimate)) +
        geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
        geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
        geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#B8D8CF", color = NA) +
        geom_line(color = "#176B58", linewidth = 0.9) +
        geom_point(color = "#176B58", size = 2.2) +
        scale_x_continuous(breaks = -4:5) +
        labs(
          title = outcome_specs$title[outcome_i],
          subtitle = sprintf("Pooled estimate = %.3f%s (SE %.3f)", pooled_estimate, pooled_stars, pooled_se),
          x = "Years relative to the 2015 ward remap",
          y = "Effect of a 1 SD increase in assigned stringency",
          caption = paste0(
            "OLS census-block event study with block and ward-pair-by-year fixed effects.\n",
            "500ft sample; predetermined history controls; score estimated through 2014 without median income; ward-pair clustering."
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
        file.path("../output", outcome_specs$filename[outcome_i]),
        controlled_plots[[outcome_i]],
        width = 7.6,
        height = 5.2,
        dpi = 220,
        bg = "white"
      )
    }

    event_rows[[row_i]] <- event_estimates %>%
      select(outcome, control_set, event_time, estimate, std_error, ci_low, ci_high)
    summary_rows[[row_i]] <- tibble(
      outcome = outcome_name,
      control_set = control_sets$control_set[control_i],
      pooled_estimate,
      pooled_se,
      pooled_p,
      observations = nobs(pooled_model),
      sample_blocks = n_distinct(model_data$block_id),
      sample_ward_pairs = n_distinct(model_data$ward_pair_id),
      rezoning_block_years = sum(model_data$any_rezoning == 1L),
      pretrend_p_value
    )
    row_i <- row_i + 1L
  }
}

ggsave(
  "../output/rezoning_drop_income_event_3panel.png",
  controlled_plots[[1]] / controlled_plots[[2]] / controlled_plots[[3]],
  width = 7.8,
  height = 15.6,
  dpi = 220,
  bg = "white"
)
write_csv(bind_rows(event_rows), "../output/rezoning_drop_income_event_estimates.csv")
write_csv(bind_rows(summary_rows), "../output/rezoning_drop_income_event_summary.csv")

comparison_rows <- list()
comparison_i <- 1L
for (score_spec in c("original", "drop_income")) {
  for (outcome_i in seq_len(nrow(outcome_specs))) {
    outcome_name <- outcome_specs$outcome[outcome_i]
    for (control_i in seq_len(nrow(control_sets))) {
      comparison_data <- panel %>%
        filter(!is.na(.data[[outcome_name]])) %>%
        mutate(
          comparison_change = if (score_spec == "original") {
            strictness_change_original
          } else {
            strictness_change
          },
          post_treat = as.integer(relative_year >= 0L) * comparison_change
        )
      comparison_model <- feols(
        as.formula(sprintf(
          "%s ~ post_treat%s | block_id + ward_pair_id^year",
          outcome_name,
          control_sets$pre_controls[control_i]
        )),
        data = comparison_data,
        cluster = ~ward_pair_id,
        notes = FALSE
      )
      comparison_rows[[comparison_i]] <- tibble(
        score_spec,
        outcome = outcome_name,
        control_set = control_sets$control_set[control_i],
        estimate = coef(comparison_model)[["post_treat"]],
        std_error = se(comparison_model)[["post_treat"]],
        p_value = pvalue(comparison_model)[["post_treat"]],
        observations = nobs(comparison_model)
      )
      comparison_i <- comparison_i + 1L
    }
  }
}
write_csv(bind_rows(comparison_rows), "../output/rezoning_drop_income_score_comparison.csv")
