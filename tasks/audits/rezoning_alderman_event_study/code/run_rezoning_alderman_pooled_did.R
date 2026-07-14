# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_alderman_event_study/code")

source("../../../setup_environment/code/packages.R")

panel <- read_parquet("../output/rezoning_alderman_block_year_panel.parquet")

geographies <- tibble(
  sample = c("500ft", "1000ft", "1500ft", "2000ft", "Full 800m panel", "Citywide"),
  sample_type = c(rep("boundary", 5), "citywide"),
  bandwidth = c(152.4, 304.8, 457.2, 609.6, 800, NA_real_)
)
outcomes <- c("any_rezoning", "any_upzone", "far_change_total")
post_definitions <- c("include_2015", "exclude_2015")

results <- list()
result_index <- 1L

for (post_definition in post_definitions) {
for (geography_index in seq_len(nrow(geographies))) {
  sample_label <- geographies$sample[geography_index]
  sample_type <- geographies$sample_type[geography_index]
  bandwidth <- geographies$bandwidth[geography_index]

  for (outcome in outcomes) {
    control_sets <- if (post_definition == "exclude_2015") {
      if (sample_type == "boundary") "pre_history_permits" else "pre_history"
    } else if (sample_type == "boundary") {
      c("none", "pre_history_permits")
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
      if (post_definition == "exclude_2015") {
        data <- data %>%
          filter(year != 2015L) %>%
          mutate(post = as.integer(year >= 2016L))
      } else {
        data <- data %>% mutate(post = as.integer(year >= 2015L))
      }

      if (sample_type == "boundary") {
        data <- data %>%
          filter(
            !is.na(ward_pair_id),
            ward_pair_id != "",
            dist_m <= bandwidth
          )
        fixed_effects <- "block_id + ward_pair_id^year"
      } else {
        fixed_effects <- "block_id + ward_origin^year"
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
          "%s ~ post:strictness_change + %s | %s",
          outcome,
          pre_controls,
          fixed_effects
        )),
        data = data,
        cluster = ~block_id,
        notes = FALSE
      )

      coefficient_index <- grep("post.*strictness_change|strictness_change.*post", names(coef(model)))
      if (length(coefficient_index) != 1L) {
        stop("Could not identify the pooled treatment coefficient.", call. = FALSE)
      }

      estimate <- coef(model)[coefficient_index]
      std_error <- se(model)[coefficient_index]
      results[[result_index]] <- tibble(
        outcome,
        sample = sample_label,
        sample_type,
        bandwidth_m = bandwidth,
        post_definition,
        control_set,
        fixed_effects,
        observations = nobs(model),
        blocks = n_distinct(data$block_id),
        estimate,
        std_error,
        ci_low = estimate - qnorm(0.975) * std_error,
        ci_high = estimate + qnorm(0.975) * std_error,
        p_value = 2 * pnorm(-abs(estimate / std_error))
      )
      result_index <- result_index + 1L
      rm(model, data)
      gc(FALSE)
    }
  }
}
}

results <- bind_rows(results) %>%
  mutate(
    sample = factor(sample, levels = geographies$sample),
    control_label = if_else(control_set == "none", "Base", "Predetermined history"),
    outcome_label = factor(
      outcome,
      levels = outcomes,
      labels = c("Any rezoning", "Any upzoning", "Total FAR change")
    ),
    post_label = factor(
      post_definition,
      levels = post_definitions,
      labels = c("Post: 2015-2020", "Post: 2016-2020; 2015 omitted")
    ),
    estimate_label = sprintf("%.4f", estimate),
    label_vjust = if_else(control_label == "Base", -0.9, 1.8)
  )

plot <- ggplot(results, aes(x = sample, y = estimate, color = control_label, group = control_label)) +
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
  facet_wrap(vars(post_label, outcome_label), scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("Base" = "#176B58", "Predetermined history" = "#B24C3D")) +
  labs(
    title = "Pooled rezoning effects after 2015 ward reassignment",
    subtitle = "Full pre-period; one standard deviation increase in stringency",
    x = NULL,
    y = "Post-period effect",
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
  "../output/rezoning_alderman_pooled_did.pdf",
  plot,
  width = 9,
  height = 9,
  bg = "white"
)

write_csv(
  results %>% mutate(sample = as.character(sample)),
  "../output/rezoning_alderman_pooled_did.csv"
)
