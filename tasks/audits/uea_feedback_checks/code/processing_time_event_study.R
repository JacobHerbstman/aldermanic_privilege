# setwd("tasks/audits/uea_feedback_checks/code")
# Exploratory (UEA feedback): does assignment to a more stringent alderman lengthen permit processing, not just reduce
# permit volumes? Permit-level version of the 2015 redistricting event study: log days from application to issue for
# high-discretion permits on the event-study blocks, by application year. Two specifications: the paper's (unchanged
# blocks on both sides of the old boundary, 2010-2020) and the same-side comparison (unchanged blocks in the ward the
# block left only, 2012-2018, before the 2019 election).
# Blocks within 500 ft (152.4 m) of the boundary, as in tasks/run_event_study_permit.
bandwidth_m <- 152.4
specs <- tibble::tribble(
  ~spec, ~fixed_effect, ~first_year, ~last_year,
  "both_sides_2010_2020", "ward_pair_id^year", 2010L, 2020L,
  "same_side_2012_2018", "ward_pair_side^year", 2012L, 2018L
)
# Level fixed effects: each block, or (fewer, to test whether block effects cost the power) each treatment group
# (blocks moved toward greater stringency, toward greater leniency, or unchanged) within each side of each ward pair.
level_fixed_effects <- c(block = "block_id", group = "ward_pair_side^treatment_group")
# Outcomes: log days (OLS; same-day permits, about 40 percent, drop out), days in levels (OLS) and days by Poisson
# pseudo-maximum likelihood (a proportional effect that keeps same-day permits).
outcomes <- tibble::tribble(
  ~outcome, ~variable, ~estimator,
  "log_days", "log_processing_days", "ols",
  "days", "processing_time", "ols",
  "days_ppml", "processing_time", "ppml"
)

source("../../../setup_environment/code/packages.R")
library(patchwork)
sf_use_s2(FALSE)

# Event-study blocks: the paper's stable-incumbent sample with at least one high-discretion permit in 2010-2014.
panel <- arrow::read_parquet("../input/permit_block_year_panel_2015.parquet") |>
  filter(dist_m <= bandwidth_m, relative_year >= -5L, relative_year <= 5L, !is.na(strictness_change_frozen),
    !is.na(ward_pair_id), ward_pair_id != "", stable_both)
active <- panel |>
  filter(relative_year < 0L) |>
  summarise(pre_permits = sum(n_high_discretion_application), .by = block_id) |>
  filter(pre_permits > 0)
blocks <- panel |>
  filter(block_id %in% active$block_id) |>
  distinct(block_id, ward_pair_id, ward_pair_side, switched, strictness_change_frozen) |>
  mutate(stricter = as.integer(strictness_change_frozen > 0), lenient = as.integer(strictness_change_frozen < 0))
stopifnot(!anyDuplicated(blocks$block_id))

# Assign high-discretion permits to the event-study blocks as tasks/create_event_study_permit_data does: point in
# block polygon, then the reviewed block for permits that fall in no block.
block_shapes <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE, col_select = c(the_geom, GEOID10)) |>
  transmute(block_id = as.character(GEOID10), geometry = the_geom) |>
  filter(block_id %in% blocks$block_id) |>
  distinct(block_id, .keep_all = TRUE) |>
  st_as_sf(wkt = "geometry", crs = 4269) |>
  st_make_valid() |>
  st_transform(3435)
permits <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE, query = paste(
  "SELECT id, permit_type, review_type, processing_time, reported_cost, application_start_date_ym, geom",
  "FROM building_permits_clean WHERE high_discretion = 1",
  "AND application_start_date_ym >= '2010-01-01' AND application_start_date_ym < '2021-01-01'",
  "AND (permit_status NOT IN ('CANCELLED', 'REVOKED', 'SUSPENDED') OR permit_status IS NULL)"
)) |>
  mutate(id = as.character(id)) |>
  st_transform(3435)
manual <- read_csv("../input/manual_permit_block_assignments.csv", col_types = cols(.default = col_character())) |>
  filter(block_vintage == "2010", !is.na(reviewed_block_id), reviewed_block_id %in% blocks$block_id) |>
  transmute(id = trimws(id), reviewed_block_id = trimws(reviewed_block_id))
stopifnot(!anyDuplicated(manual$id))
permit_blocks <- st_join(permits, block_shapes, join = st_within) |>
  st_drop_geometry() |>
  left_join(manual, by = "id", relationship = "many-to-one") |>
  mutate(block_id = coalesce(block_id, reviewed_block_id)) |>
  filter(!is.na(block_id))
stopifnot(!anyDuplicated(permit_blocks$id))

# Log days from application to issue; same-day permits (zero days) are dropped from the logged outcome. Reported cost
# enters as a log with an indicator for a missing or zero cost.
permit_data <- permit_blocks |>
  inner_join(blocks, by = "block_id", relationship = "many-to-one") |>
  mutate(
    year = as.integer(substr(application_start_date_ym, 1, 4)),
    relative_year = year - 2015L,
    post = as.integer(year >= 2015L),
    log_processing_days = log(if_else(processing_time > 0, processing_time, NA_real_)),
    cost_missing = as.integer(!is.finite(reported_cost) | reported_cost <= 0),
    log_cost = if_else(cost_missing == 1L, 0, log(pmax(reported_cost, 1))),
    review_type = coalesce(review_type, "unknown"),
    treatment_group = case_when(stricter == 1L ~ "stricter", lenient == 1L ~ "lenient", TRUE ~ "unchanged"),
    post_stricter = post * stricter, post_lenient = post * lenient, post_signed = post * (stricter - lenient)
  )

results <- list()
for (s in seq_len(nrow(specs))) for (l in names(level_fixed_effects)) for (o in seq_len(nrow(outcomes))) {
  spec_rows <- list()
  d <- filter(permit_data, year >= specs$first_year[s], year <= specs$last_year[s], is.finite(.data[[outcomes$variable[o]]]))
  estimator <- if (outcomes$estimator[o] == "ppml") fixest::fepois else fixest::feols
  fit <- function(rhs) estimator(as.formula(paste(outcomes$variable[o], "~", rhs,
    "+ log_cost + cost_missing |", level_fixed_effects[[l]], "+ permit_type^review_type +", specs$fixed_effect[s])),
    data = d, cluster = ~ward_pair_id, notes = FALSE)
  event <- fit("i(relative_year, stricter, ref = -1) + i(relative_year, lenient, ref = -1)")
  pooled <- fit("post_stricter + post_lenient")
  combined <- fit("post_signed")
  event_years <- setdiff(seq(specs$first_year[s], specs$last_year[s]) - 2015L, -1L)
  for (a in c("stricter", "lenient")) {
    terms <- paste0("relative_year::", event_years, ":", a)
    pre_terms <- terms[event_years < -1L]
    pre_test <- fixest::wald(event, keep = paste0("^(", paste(gsub("([:^-])", "\\\\\\1", pre_terms), collapse = "|"), ")$"),
      print = FALSE)
    pooled_term <- paste0("post_", a)
    spec_rows[[a]] <- tibble(spec = specs$spec[s], level_fixed_effect = l, outcome = outcomes$outcome[o], arm = a, event_time = c(event_years, -1L),
      estimate = c(coef(event)[terms], 0), std_error = c(fixest::se(event)[terms], 0), p_value = c(fixest::pvalue(event)[terms], NA),
      pooled_estimate = coef(pooled)[[pooled_term]], pooled_std_error = fixest::se(pooled)[[pooled_term]],
      pooled_p_value = fixest::pvalue(pooled)[[pooled_term]], pretrend_p_value = pre_test$p)
  }
  spec_rows[["combined"]] <- tibble(spec = specs$spec[s], level_fixed_effect = l, outcome = outcomes$outcome[o], arm = "combined", event_time = NA_integer_,
    estimate = NA_real_, std_error = NA_real_, p_value = NA_real_,
    pooled_estimate = coef(combined)[["post_signed"]], pooled_std_error = fixest::se(combined)[["post_signed"]],
    pooled_p_value = fixest::pvalue(combined)[["post_signed"]], pretrend_p_value = NA_real_)
  results[[length(results) + 1]] <- bind_rows(spec_rows) |>
    mutate(permits = nobs(pooled), blocks = n_distinct(d$block_id), mean_processing_days = mean(d$processing_time))
}
results <- bind_rows(results)
write_csv(results, "../output/processing_time_event_study.csv")

stars <- function(p) case_when(p <= 0.01 ~ "***", p <= 0.05 ~ "**", p <= 0.10 ~ "*", TRUE ~ "")
plot_data <- filter(results, arm != "combined", level_fixed_effect == "block") |>
  mutate(ci_low = estimate - 1.96 * std_error, ci_high = estimate + 1.96 * std_error)
y_labels <- c(log_days = "Effect on log processing days", days = "Effect on processing days",
  days_ppml = "Effect on processing days (PPML, log points)")
colors <- c(stricter = "#D92D27", lenient = "#2478B5")
titles <- c(stricter = "Assigned to more stringent aldermen", lenient = "Assigned to more lenient aldermen")
panels <- list()
for (o in outcomes$outcome) for (s in specs$spec) for (a in names(colors)) {
  p <- filter(plot_data, outcome == o, spec == s, arm == a)
  limits <- range(c(filter(plot_data, outcome == o)$ci_low, filter(plot_data, outcome == o)$ci_high))
  panels[[length(panels) + 1]] <- ggplot(p, aes(event_time + 2015L, estimate)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_vline(xintercept = 2014.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = colors[[a]], alpha = 0.16) +
    geom_line(color = colors[[a]], linewidth = 0.85) + geom_point(color = colors[[a]], size = 2) +
    coord_cartesian(ylim = limits) + scale_x_continuous(breaks = seq(2010, 2020, 2)) +
    labs(title = paste0(titles[[a]], " (", gsub("_", " ", s), ")"),
      subtitle = sprintf("Pooled = %.3f%s (SE %.3f); pre-trend p = %.2f; %s permits", p$pooled_estimate[1],
        stars(p$pooled_p_value[1]), p$pooled_std_error[1], p$pretrend_p_value[1], format(p$permits[1], big.mark = ",")),
      x = "Application year", y = y_labels[[o]]) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 9, face = "bold"))
}
ggsave("../output/processing_time_event_study.pdf", wrap_plots(panels, ncol = 4), width = 22, height = 12, bg = "white")
