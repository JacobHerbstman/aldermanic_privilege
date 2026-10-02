# setwd("tasks/audits/uea_feedback_checks/code")
# Exploratory (UEA feedback, follow-up to continuous_stringency.R and lenient_size_by_boundary.R): the permit event
# study with a term for being reassigned at all plus the destination-minus-origin score change (standard deviations of
# the 2006-2014 index; positive = toward a more stringent alderman). Same blocks, samples and PPML specification as
# tasks/run_event_study_permit, in the paper's comparison (both sides of the old boundary, 2010-2020) and the
# same-side comparison (2012-2018). Writes pooled models, implied effects for example moves, event-time paths,
# leave-one-move-out estimates, and the same models for low-discretion permits.
bandwidth_m <- 152.4
specs <- tibble::tribble(
  ~spec, ~fixed_effect, ~first_year, ~last_year,
  "both_sides_2010_2020", "ward_pair_id^year", 2010L, 2020L,
  "same_side_2012_2018", "ward_pair_side^year", 2012L, 2018L
)
outcomes <- c(high_discretion = "n_high_discretion_application", low_discretion = "n_low_discretion_nosigns_application")
# Score changes at which the implied effect of a move is reported.
example_changes <- c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)

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
  mutate(
    change = strictness_change_frozen,
    reassigned = as.integer(switched),
    move = if_else(switched, paste0(ward_origin, "->", ward_dest), NA_character_),
    post = as.integer(year >= 2015L),
    post_reassigned = post * reassigned, post_change = post * change,
    post_stricter_size = post * pmax(change, 0), post_lenient_size = post * pmax(-change, 0),
    post_stricter = post * (change > 0), post_lenient = post * (change < 0),
    signed = sign(change), post_signed = post * signed
  )
stopifnot(all(panel$change[panel$reassigned == 0L] == 0), all(panel$change[panel$reassigned == 1L] != 0))

pooled <- list()
implied <- list()
event <- list()
leave_out <- list()
for (s in seq_len(nrow(specs))) for (o in names(outcomes)) {
  d <- filter(panel, year >= specs$first_year[s], year <= specs$last_year[s])
  fit <- function(rhs, data = d) fixest::fepois(as.formula(paste(outcomes[[o]], "~", rhs, "| block_id +",
    specs$fixed_effect[s])), data = data, cluster = ~ward_pair_id, notes = FALSE)
  key <- tibble(spec = specs$spec[s], outcome = o)

  # Pooled models: reassignment plus one slope; reassignment plus separate slopes for moves toward greater
  # stringency and toward greater leniency (sizes as positive numbers; one common slope means lenient = -stricter);
  # and the paper's two direction indicators for reference.
  models <- list(
    linear = fit("post_reassigned + post_change"),
    separate_slopes = fit("post_reassigned + post_stricter_size + post_lenient_size"),
    direction_indicators = fit("post_stricter + post_lenient"),
    binary_with_reassigned = fit("post_reassigned + post_signed")
  )
  for (m in names(models)) {
    ct <- fixest::coeftable(models[[m]])
    pooled[[length(pooled) + 1]] <- tibble(key, model = m, term = rownames(ct), estimate = ct[, 1], std_error = ct[, 2],
      p_value = ct[, 4], observations = nobs(models[[m]]))
  }
  b <- coef(models$separate_slopes)
  v <- vcov(models$separate_slopes)
  common_slope_se <- sqrt(v["post_stricter_size", "post_stricter_size"] + v["post_lenient_size", "post_lenient_size"] +
    2 * v["post_stricter_size", "post_lenient_size"])
  pooled[[length(pooled) + 1]] <- tibble(key, model = "separate_slopes", term = "test: stricter slope = -lenient slope",
    estimate = b[["post_stricter_size"]] + b[["post_lenient_size"]], std_error = common_slope_se,
    p_value = 2 * pt(-abs(estimate / std_error), fixest::degrees_freedom(models$separate_slopes, "t")),
    observations = nobs(models$separate_slopes))

  # Implied effect of a move of each size under the linear model, and the lenient move that would leave permits
  # unchanged.
  b <- coef(models$linear)
  v <- vcov(models$linear)
  implied[[length(implied) + 1]] <- tibble(key, score_change = example_changes,
    estimate = b[["post_reassigned"]] + b[["post_change"]] * score_change,
    std_error = sqrt(v["post_reassigned", "post_reassigned"] + score_change^2 * v["post_change", "post_change"] +
      2 * score_change * v["post_reassigned", "post_change"]),
    percent_change = 100 * (exp(estimate) - 1),
    break_even_change = -b[["post_reassigned"]] / b[["post_change"]])

  # Event-time paths of both terms: reassignment plus the score change (continuous), or reassignment plus the sign of
  # the change (binary; the paper's combined model with a reassignment term, so reassigned is the average of the two
  # directions' effects and signed is half their difference).
  event_years <- setdiff(seq(specs$first_year[s], specs$last_year[s]) - 2015L, -1L)
  for (version in c("continuous", "binary")) {
    slope <- if (version == "continuous") "change" else "signed"
    e <- fit(paste0("i(relative_year, reassigned, ref = -1) + i(relative_year, ", slope, ", ref = -1)"))
    for (term in c("reassigned", slope)) {
      names_t <- paste0("relative_year::", event_years, ":", term)
      pre <- names_t[event_years < -1L]
      pre_p <- fixest::wald(e, keep = paste0("^(", paste(gsub("([:^-])", "\\\\\\1", pre), collapse = "|"), ")$"),
        print = FALSE)$p
      event[[length(event) + 1]] <- tibble(key, version, term, event_time = c(event_years, -1L),
        estimate = c(coef(e)[names_t], 0), std_error = c(fixest::se(e)[names_t], 0), pretrend_p_value = pre_p)
    }
  }

  # Leaving out each move's reassigned blocks.
  if (o == "high_discretion") {
    for (mv in sort(unique(na.omit(d$move)))) {
      model <- fit("post_reassigned + post_change", filter(d, is.na(move) | move != mv))
      leave_out[[length(leave_out) + 1]] <- tibble(key, move = mv, change = d$change[match(mv, d$move)],
        reassigned_blocks = n_distinct(d$block_id[d$move %in% mv]),
        reassigned_without = coef(model)[["post_reassigned"]], change_without = coef(model)[["post_change"]],
        reassigned_se_without = fixest::se(model)[["post_reassigned"]], change_se_without = fixest::se(model)[["post_change"]])
    }
  }
}
write_csv(bind_rows(pooled), "../output/reassignment_cost_pooled.csv")
write_csv(bind_rows(implied), "../output/reassignment_cost_implied.csv")
write_csv(bind_rows(event), "../output/reassignment_cost_event.csv")
write_csv(bind_rows(leave_out), "../output/reassignment_cost_leave_one_move_out.csv")

# Event-time figures: high-discretion permits, one row per specification, for each version.
stars <- function(p) case_when(p <= 0.01 ~ "***", p <= 0.05 ~ "**", p <= 0.10 ~ "*", TRUE ~ "")
plot_data <- bind_rows(event) |>
  filter(outcome == "high_discretion") |>
  mutate(ci_low = estimate - 1.96 * std_error, ci_high = estimate + 1.96 * std_error)
colors <- c(reassigned = "#6B4C9A", change = "#D92D27", signed = "#D92D27")
titles <- c(reassigned = "Reassigned", change = "Per SD more stringent destination",
  signed = "Direction (+1 stricter, -1 lenient)")
pooled_models <- c(continuous = "linear", binary = "binary_with_reassigned")
for (version in names(pooled_models)) {
  v_data <- filter(plot_data, version == .env$version)
  panels <- list()
  for (s in specs$spec) for (term in unique(v_data$term)) {
    p <- filter(v_data, spec == s, term == .env$term)
    q <- filter(bind_rows(pooled), outcome == "high_discretion", model == pooled_models[[version]], spec == s,
      term == paste0("post_", .env$term))
    panels[[length(panels) + 1]] <- ggplot(p, aes(event_time + 2015L, estimate)) +
      geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
      geom_vline(xintercept = 2014.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = colors[[term]], alpha = 0.16) +
      geom_line(color = colors[[term]], linewidth = 0.85) + geom_point(color = colors[[term]], size = 2) +
      coord_cartesian(ylim = range(c(v_data$ci_low, v_data$ci_high))) +
      scale_x_continuous(breaks = seq(2010, 2020, 2)) +
      labs(title = paste0(titles[[term]], " (", gsub("_", " ", s), ")"),
        subtitle = sprintf("Pooled = %.3f%s (SE %.3f); pre-trend p = %.2f", q$estimate, stars(q$p_value), q$std_error,
          p$pretrend_p_value[1]),
        x = "Application year", y = "Effect on high-discretion permits (log points)") +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 9, face = "bold"))
  }
  ggsave(if (version == "continuous") "../output/reassignment_cost_event_study.pdf" else
    "../output/reassignment_cost_binary_event_study.pdf", wrap_plots(panels, ncol = 2), width = 11.5, height = 8, bg = "white")
}
