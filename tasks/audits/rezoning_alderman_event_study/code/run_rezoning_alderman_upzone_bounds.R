# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_alderman_event_study/code")

source("../../../setup_environment/code/packages.R")

panel <- read_parquet("../output/rezoning_alderman_block_year_panel.parquet") %>%
  mutate(
    any_upzone_lower = replace_na(any_upzone, 0),
    any_upzone_upper = if_else(is.na(any_upzone), 1, any_upzone)
  )

bandwidths <- tibble(
  sample = c("500ft", "1000ft", "1500ft", "2000ft", "Full 800m panel"),
  bandwidth = c(152.4, 304.8, 457.2, 609.6, 800)
)
post_definitions <- c("include_2015", "exclude_2015")
outcomes <- c("any_upzone_lower", "any_upzone_upper")

results <- list()
result_index <- 1L

for (post_definition in post_definitions) {
  for (bandwidth_index in seq_len(nrow(bandwidths))) {
    sample_label <- bandwidths$sample[bandwidth_index]
    bandwidth <- bandwidths$bandwidth[bandwidth_index]

    data <- panel %>%
      filter(
        relative_year >= -4L,
        relative_year <= 5L,
        !is.na(ward_pair_id),
        ward_pair_id != "",
        dist_m <= bandwidth
      )
    if (post_definition == "exclude_2015") {
      data <- data %>%
        filter(year != 2015L) %>%
        mutate(post = as.integer(year >= 2016L))
    } else {
      data <- data %>% mutate(post = as.integer(year >= 2015L))
    }

    for (outcome in outcomes) {
      model <- feols(
        as.formula(sprintf(
          paste(
            "%s ~ post:strictness_change",
            "+ i(year, pre_rezoning_count, ref = 2014)",
            "+ i(year, pre_upzone_count, ref = 2014)",
            "+ i(year, pre_far_change_total, ref = 2014)",
            "+ i(year, pre_unclassified_count, ref = 2014)",
            "+ i(year, pre_new_construction_count, ref = 2014)",
            "+ i(year, pre_high_discretion_count, ref = 2014)",
            "| block_id + ward_pair_id^year"
          ),
          outcome
        )),
        data = data,
        cluster = ~block_id,
        notes = FALSE
      )

      coefficient_index <- grep("post.*strictness_change|strictness_change.*post", names(coef(model)))
      estimate <- coef(model)[coefficient_index]
      std_error <- se(model)[coefficient_index]
      results[[result_index]] <- tibble(
        sample = sample_label,
        bandwidth_m = bandwidth,
        post_definition,
        bound = if_else(outcome == "any_upzone_lower", "unresolved_as_zero", "unresolved_as_one"),
        observations = nobs(model),
        blocks = n_distinct(data$block_id),
        estimate,
        std_error,
        ci_low = estimate - qnorm(0.975) * std_error,
        ci_high = estimate + qnorm(0.975) * std_error
      )
      result_index <- result_index + 1L
      rm(model)
      gc(FALSE)
    }
  }
}

write_csv(bind_rows(results), "../output/rezoning_alderman_upzone_bounds.csv")
