# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_alderman_event_study/code")

source("../../../setup_environment/code/packages.R")

panel <- read_parquet("../output/rezoning_alderman_block_year_panel.parquet")

geographies <- tibble(
  sample = c("500ft", "1000ft", "1500ft", "2000ft", "Full 800m panel", "Citywide"),
  sample_type = c(rep("boundary", 5), "citywide"),
  bandwidth = c(152.4, 304.8, 457.2, 609.6, 800, NA_real_)
)
outcomes <- c("any_rezoning", "any_upzone", "far_change_total", "mean_far_change", "upzone_share")

results <- list()
result_index <- 1L

for (geography_index in seq_len(nrow(geographies))) {
  sample_label <- geographies$sample[geography_index]
  sample_type <- geographies$sample_type[geography_index]
  bandwidth <- geographies$bandwidth[geography_index]

  for (outcome in outcomes) {
    conditional_outcome <- outcome %in% c("mean_far_change", "upzone_share")

    control_sets <- if (sample_type == "boundary") {
      c("none", "pre_history", "pre_history_permits")
    } else {
      c("none", "pre_history")
    }

    for (control_set in control_sets) {
      data <- panel %>%
        filter(
          relative_year >= -4L,
          relative_year <= 5L,
          !is.na(.data[[outcome]])
        )

      if (sample_type == "boundary") {
        data <- data %>%
          filter(
            !is.na(ward_pair_id),
            ward_pair_id != "",
            dist_m <= bandwidth
          )
        fixed_effects <- if (conditional_outcome) {
          "ward_pair_id^year"
        } else {
          "block_id + ward_pair_id^year"
        }
      } else {
        fixed_effects <- if (conditional_outcome) {
          "ward_origin^year"
        } else {
          "block_id + ward_origin^year"
        }
      }

      pre_controls <- if (control_set %in% c("pre_history", "pre_history_permits")) {
        paste(
          "i(year, pre_rezoning_count, ref = 2014)",
          "i(year, pre_upzone_count, ref = 2014)",
          "i(year, pre_far_change_total, ref = 2014)",
          "i(year, pre_unclassified_count, ref = 2014)",
          sep = " + "
        )
      } else {
        "1"
      }
      if (control_set == "pre_history_permits") {
        pre_controls <- paste(
          pre_controls,
          "i(year, pre_new_construction_count, ref = 2014)",
          "i(year, pre_high_discretion_count, ref = 2014)",
          sep = " + "
        )
      }

      model <- feols(
        as.formula(sprintf(
          "%s ~ i(relative_year, strictness_change, ref = -1) + %s | %s",
          outcome,
          pre_controls,
          fixed_effects
        )),
        data = data,
        cluster = ~block_id,
        notes = FALSE
      )

      estimates <- iplot(model, i.select = 1, .plot = FALSE)[[1]] %>%
        as_tibble() %>%
        transmute(
          event_time = as.integer(x),
          estimate,
          ci_low,
          ci_high,
          std_error = if_else(is_ref, 0, (ci_high - estimate) / qnorm(0.975)),
          estimate_name_raw = estimate_names_raw,
          is_reference = is_ref
        ) %>%
        filter(event_time >= -4L, event_time <= 5L)

      lead_terms <- estimates %>%
        filter(event_time <= -2L, !is_reference) %>%
        pull(estimate_name_raw)
      pretrend <- if (length(lead_terms) > 0L) {
        wald(model, lead_terms, print = FALSE)
      } else {
        NULL
      }

      results[[result_index]] <- estimates %>%
        transmute(
          outcome,
          sample = sample_label,
          sample_type,
          bandwidth_m = bandwidth,
          control_set,
          conditional_outcome,
          fixed_effects,
          observations = nobs(model),
          blocks = n_distinct(data$block_id),
          pretrend_p_value = if (is.null(pretrend)) NA_real_ else pretrend$p,
          event_time,
          estimate,
          std_error,
          ci_low,
          ci_high
        )
      result_index <- result_index + 1L
    }
  }
}

results <- bind_rows(results) %>%
  mutate(
    sample = factor(sample, levels = geographies$sample),
    control_set = factor(
      control_set,
      levels = c("none", "pre_history", "pre_history_permits"),
      labels = c("Base", "Pre-rezoning history", "Pre-rezoning + permit history")
    ),
    outcome_label = factor(
      outcome,
      levels = outcomes,
      labels = c(
        "Any rezoning",
        "Any upzoning",
        "Total FAR change",
        "Mean FAR change | rezoning",
        "Upzoning share | rezoning"
      )
    )
  )

year_zero <- results %>%
  filter(event_time == 0L, control_set != "Pre-rezoning history") %>%
  mutate(
    estimate_label = if_else(
      conditional_outcome,
      sprintf("%.3f", estimate),
      sprintf("%.4f", estimate)
    ),
    label_vjust = if_else(control_set == "Base", -0.9, 1.8)
  )

plot <- ggplot(year_zero, aes(x = sample, y = estimate, color = control_set, group = control_set)) +
  geom_hline(yintercept = 0, color = "gray45", linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0,
    position = position_dodge(width = 0.45)
  ) +
  geom_point(size = 2.2, position = position_dodge(width = 0.45)) +
  geom_text(
    aes(label = estimate_label, vjust = label_vjust),
    size = 2.7,
    position = position_dodge(width = 0.45),
    show.legend = FALSE
  ) +
  facet_wrap(~outcome_label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Base" = "#176B58", "Pre-rezoning + permit history" = "#B24C3D")) +
  labs(
    title = "Rezoning response in the 2015 reassignment year",
    subtitle = "Effect of a one standard deviation increase in aldermanic stringency",
    x = NULL,
    y = "Estimate",
    color = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave(
  "../output/rezoning_alderman_robustness_year0.pdf",
  plot,
  width = 9,
  height = 8,
  bg = "white"
)

write_csv(
  results %>% mutate(sample = as.character(sample), control_set = as.character(control_set)),
  "../output/rezoning_alderman_robustness_estimates.csv"
)
