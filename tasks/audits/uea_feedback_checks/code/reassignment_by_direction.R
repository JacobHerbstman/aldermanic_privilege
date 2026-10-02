# setwd("tasks/audits/uea_feedback_checks/code")
# Exploratory (UEA feedback, follow-up to reassignment_cost.R): the two reassignment directions estimated separately
# alongside a term for being reassigned at all. With direction indicators alone the reassignment term is not
# identified (every reassigned block moves one way or the other), so moves whose score change is smaller than a
# cutoff count as reassignment without a change in stringency: "reassigned" is their effect, and "stricter" and
# "lenient" are the additional effects of moves beyond the cutoff. The main cutoff is 0.25 SD, the smallest score
# gap in the density score-robustness table; 0.10 and 0.50 SD are reported as well. Same blocks, samples and PPML
# specification as tasks/run_event_study_permit, in the paper's comparison and the same-side comparison.
bandwidth_m <- 152.4
specs <- tibble::tribble(
  ~spec, ~fixed_effect, ~first_year, ~last_year,
  "both_sides_2010_2020", "ward_pair_id^year", 2010L, 2020L,
  "same_side_2012_2018", "ward_pair_side^year", 2012L, 2018L
)
cutoffs <- c(0.10, 0.25, 0.50)
main_cutoff <- 0.25

source("../../../setup_environment/code/packages.R")
library(patchwork)

panel <- arrow::read_parquet("../input/permit_block_year_panel_2015.parquet") |>
  filter(dist_m <= bandwidth_m, relative_year >= -5L, relative_year <= 5L, !is.na(strictness_change_frozen),
    !is.na(ward_pair_id), ward_pair_id != "", stable_both)
active <- panel |>
  filter(relative_year < 0L) |>
  summarise(pre_permits = sum(n_high_discretion_application), .by = block_id) |>
  filter(pre_permits > 0)
panel <- panel |>
  filter(block_id %in% active$block_id) |>
  mutate(change = strictness_change_frozen, reassigned = as.integer(switched), post = as.integer(year >= 2015L),
    move = if_else(switched, paste0(ward_origin, "->", ward_dest), NA_character_))

pooled <- list()
event <- list()
for (cutoff in cutoffs) {
  data_c <- panel |>
    mutate(stricter = as.integer(change >= cutoff), lenient = as.integer(change <= -cutoff),
      post_reassigned = post * reassigned, post_stricter = post * stricter, post_lenient = post * lenient)
  groups <- data_c |>
    filter(switched) |>
    distinct(block_id, move, stricter, lenient) |>
    summarise(near_zero_blocks = sum(stricter == 0 & lenient == 0), stricter_blocks = sum(stricter),
      lenient_blocks = sum(lenient), near_zero_moves = n_distinct(move[stricter == 0 & lenient == 0]))
  for (s in seq_len(nrow(specs))) {
    d <- filter(data_c, year >= specs$first_year[s], year <= specs$last_year[s])
    fit <- function(rhs) fixest::fepois(as.formula(paste("n_high_discretion_application ~", rhs, "| block_id +",
      specs$fixed_effect[s])), data = d, cluster = ~ward_pair_id, notes = FALSE)
    m <- fit("post_reassigned + post_stricter + post_lenient")
    b <- coef(m)
    v <- vcov(m)
    # Total effects of a move beyond the cutoff (reassignment plus direction), as in the paper's separate panels.
    totals <- tibble(term = c("total: stricter move", "total: lenient move"),
      estimate = c(b[["post_reassigned"]] + b[["post_stricter"]], b[["post_reassigned"]] + b[["post_lenient"]]),
      std_error = c(sqrt(v[1, 1] + v[2, 2] + 2 * v[1, 2]), sqrt(v[1, 1] + v[3, 3] + 2 * v[1, 3])))
    ct <- fixest::coeftable(m)
    pooled[[length(pooled) + 1]] <- bind_rows(
      tibble(term = rownames(ct), estimate = ct[, 1], std_error = ct[, 2]), totals) |>
      mutate(p_value = 2 * pt(-abs(estimate / std_error), fixest::degrees_freedom(m, "t")),
        spec = specs$spec[s], cutoff, observations = nobs(m), .before = 1) |>
      cross_join(groups)
    if (cutoff == main_cutoff) {
      e <- fit("i(relative_year, reassigned, ref = -1) + i(relative_year, stricter, ref = -1) + i(relative_year, lenient, ref = -1)")
      event_years <- setdiff(seq(specs$first_year[s], specs$last_year[s]) - 2015L, -1L)
      for (term in c("reassigned", "stricter", "lenient")) {
        names_t <- paste0("relative_year::", event_years, ":", term)
        pre <- names_t[event_years < -1L]
        pre_p <- fixest::wald(e, keep = paste0("^(", paste(gsub("([:^-])", "\\\\\\1", pre), collapse = "|"), ")$"),
          print = FALSE)$p
        event[[length(event) + 1]] <- tibble(spec = specs$spec[s], term, event_time = c(event_years, -1L),
          estimate = c(coef(e)[names_t], 0), std_error = c(fixest::se(e)[names_t], 0), pretrend_p_value = pre_p)
      }
    }
  }
}
pooled <- bind_rows(pooled)
event <- bind_rows(event)
write_csv(pooled, "../output/reassignment_by_direction_pooled.csv")
write_csv(event, "../output/reassignment_by_direction_event.csv")

stars <- function(p) case_when(p <= 0.01 ~ "***", p <= 0.05 ~ "**", p <= 0.10 ~ "*", TRUE ~ "")
plot_data <- mutate(event, ci_low = estimate - 1.96 * std_error, ci_high = estimate + 1.96 * std_error)
colors <- c(reassigned = "#6B4C9A", stricter = "#D92D27", lenient = "#2478B5")
titles <- c(reassigned = sprintf("Reassigned (change under %.2f SD)", main_cutoff),
  stricter = "Additional: more stringent move", lenient = "Additional: more lenient move")
panels <- list()
for (s in specs$spec) for (term in names(colors)) {
  p <- filter(plot_data, spec == s, term == .env$term)
  q <- filter(pooled, spec == s, cutoff == main_cutoff, term == paste0("post_", .env$term))
  panels[[length(panels) + 1]] <- ggplot(p, aes(event_time + 2015L, estimate)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_vline(xintercept = 2014.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = colors[[term]], alpha = 0.16) +
    geom_line(color = colors[[term]], linewidth = 0.85) + geom_point(color = colors[[term]], size = 2) +
    coord_cartesian(ylim = range(c(plot_data$ci_low, plot_data$ci_high))) +
    scale_x_continuous(breaks = seq(2010, 2020, 2)) +
    labs(title = paste0(titles[[term]], " (", gsub("_", " ", s), ")"),
      subtitle = sprintf("Pooled = %.3f%s (SE %.3f); pre-trend p = %.2f", q$estimate, stars(q$p_value), q$std_error,
        p$pretrend_p_value[1]),
      x = "Application year", y = "Effect on high-discretion permits (log points)") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", size = 10), plot.subtitle = element_text(size = 9, face = "bold"))
}
ggsave("../output/reassignment_by_direction_event_study.pdf", wrap_plots(panels, ncol = 3), width = 15, height = 8,
  bg = "white")
