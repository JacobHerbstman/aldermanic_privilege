# setwd("tasks/audits/uea_feedback_checks/code")
# Exploratory (UEA feedback): the high- minus low-discretion difference as one event study. High- and low-discretion
# (excluding signs) application counts are stacked by block, year and permit category; block-by-category and
# comparison-group-by-year-by-category fixed effects absorb each category's own levels and trends, so the event-time
# coefficients on treatment x high-discretion measure how much more high-discretion permits respond than
# low-discretion permits on the same blocks. Poisson pseudo-maximum likelihood, clustered by ward pair, in the paper's
# specification (both sides of the old boundary, 2010-2020) and the same-side comparison (2012-2018).
# Blocks within 500 ft (152.4 m) of the boundary, as in tasks/run_event_study_permit.
bandwidth_m <- 152.4
specs <- tibble::tribble(
  ~spec, ~fixed_effect, ~first_year, ~last_year,
  "both_sides_2010_2020", "ward_pair_id^year^category", 2010L, 2020L,
  "same_side_2012_2018", "ward_pair_side^year^category", 2012L, 2018L
)

source("../../../setup_environment/code/packages.R")
library(patchwork)

panel <- arrow::read_parquet("../input/permit_block_year_panel_2015.parquet") |>
  filter(dist_m <= bandwidth_m, relative_year >= -5L, relative_year <= 5L, !is.na(strictness_change_frozen),
    !is.na(ward_pair_id), ward_pair_id != "", stable_both)
active <- panel |>
  filter(relative_year < 0L) |>
  summarise(pre_permits = sum(n_high_discretion_application), .by = block_id) |>
  filter(pre_permits > 0)
stacked <- panel |>
  filter(block_id %in% active$block_id) |>
  select(block_id, year, relative_year, ward_pair_id, ward_pair_side, strictness_change_frozen,
    high = n_high_discretion_application, low = n_low_discretion_nosigns_application) |>
  tidyr::pivot_longer(c(high, low), names_to = "category", values_to = "permits") |>
  mutate(
    high = as.integer(category == "high"),
    stricter = as.integer(strictness_change_frozen > 0),
    lenient = as.integer(strictness_change_frozen < 0),
    signed = stricter - lenient,
    stricter_high = stricter * high, lenient_high = lenient * high, signed_high = signed * high,
    post = as.integer(year >= 2015L)
  )
stopifnot(!anyDuplicated(stacked[c("block_id", "year", "category")]))

results <- list()
for (s in seq_len(nrow(specs))) {
  d <- filter(stacked, year >= specs$first_year[s], year <= specs$last_year[s])
  fit <- function(rhs) fixest::fepois(as.formula(paste("permits ~", rhs, "| block_id^category +", specs$fixed_effect[s])),
    data = d, cluster = ~ward_pair_id, notes = FALSE)
  event_years <- setdiff(seq(specs$first_year[s], specs$last_year[s]) - 2015L, -1L)
  models <- list(
    combined = list(event = fit("i(relative_year, signed_high, ref = -1) + i(relative_year, signed, ref = -1)"),
      pooled = fit("I(post * signed_high) + I(post * signed)"), arms = "signed_high"),
    separate = list(event = fit(paste("i(relative_year, stricter_high, ref = -1) + i(relative_year, lenient_high, ref = -1) +",
        "i(relative_year, stricter, ref = -1) + i(relative_year, lenient, ref = -1)")),
      pooled = fit("I(post * stricter_high) + I(post * lenient_high) + I(post * stricter) + I(post * lenient)"),
      arms = c("stricter_high", "lenient_high"))
  )
  for (m in models) for (arm in m$arms) {
    terms <- paste0("relative_year::", event_years, ":", arm)
    pre_terms <- terms[event_years < -1L]
    pre_test <- fixest::wald(m$event, keep = paste0("^(", paste(gsub("([:^-])", "\\\\\\1", pre_terms), collapse = "|"), ")$"),
      print = FALSE)
    pooled_term <- paste0("I(post * ", arm, ")")
    results[[length(results) + 1]] <- tibble(spec = specs$spec[s], arm, event_time = c(event_years, -1L),
      estimate = c(coef(m$event)[terms], 0), std_error = c(fixest::se(m$event)[terms], 0),
      pooled_estimate = coef(m$pooled)[[pooled_term]], pooled_std_error = fixest::se(m$pooled)[[pooled_term]],
      pooled_p_value = fixest::pvalue(m$pooled)[[pooled_term]], pretrend_p_value = pre_test$p,
      observations = nobs(m$pooled), blocks = n_distinct(d$block_id))
  }
}
results <- bind_rows(results)
write_csv(results, "../output/discretion_difference_event_study.csv")

stars <- function(p) case_when(p <= 0.01 ~ "***", p <= 0.05 ~ "**", p <= 0.10 ~ "*", TRUE ~ "")
plot_data <- mutate(results, ci_low = estimate - 1.96 * std_error, ci_high = estimate + 1.96 * std_error)
limits <- range(c(plot_data$ci_low, plot_data$ci_high))
colors <- c(signed_high = "#176B58", stricter_high = "#D92D27", lenient_high = "#2478B5")
titles <- c(signed_high = "Combined", stricter_high = "Assigned to more stringent aldermen",
  lenient_high = "Assigned to more lenient aldermen")
panels <- list()
for (s in specs$spec) for (a in names(colors)) {
  p <- filter(plot_data, spec == s, arm == a)
  panels[[length(panels) + 1]] <- ggplot(p, aes(event_time + 2015L, estimate)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_vline(xintercept = 2014.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = colors[[a]], alpha = 0.16) +
    geom_line(color = colors[[a]], linewidth = 0.85) + geom_point(color = colors[[a]], size = 2) +
    coord_cartesian(ylim = limits) + scale_x_continuous(breaks = seq(2010, 2020, 2)) +
    labs(title = paste0(titles[[a]], " (", gsub("_", " ", s), ")"),
      subtitle = sprintf("High minus low, pooled = %.3f%s (SE %.3f); pre-trend p = %.2f", p$pooled_estimate[1],
        stars(p$pooled_p_value[1]), p$pooled_std_error[1], p$pretrend_p_value[1]),
      x = "Application year", y = "High- minus low-discretion effect (log points)") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 9, face = "bold"))
}
ggsave("../output/discretion_difference_event_study.pdf", wrap_plots(panels, ncol = 3), width = 15, height = 8, bg = "white")
