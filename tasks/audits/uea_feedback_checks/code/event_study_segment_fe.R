# setwd("tasks/audits/uea_feedback_checks/code")
# Exploratory: the paper's permit event study (tasks/run_event_study_permit: high-discretion permits, 2010-2020,
# stable-incumbent blocks within 500 ft, PPML, block fixed effects, clustered by ward pair) with boundary-segment-by-
# year fixed effects in place of ward-pair-by-year fixed effects. Each block is assigned to the nearest 1,320 ft segment
# of its ward pair's boundary on the 2003 map, the boundary the design compares across.
bandwidth_m <- 152.4
segment_search_ft <- 3000
fixed_effects <- c(ward_pair = "ward_pair_id^year", segment = "segment_id^year")

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/canonical_geometry_helpers.R")
library(patchwork)
sf_use_s2(FALSE)

panel <- arrow::read_parquet("../input/permit_block_year_panel_2015.parquet") |>
  filter(dist_m <= bandwidth_m, relative_year >= -5L, relative_year <= 5L, !is.na(strictness_change_frozen),
    !is.na(ward_pair_id), ward_pair_id != "", stable_both)
active <- panel |>
  filter(relative_year < 0L) |>
  summarise(pre_permits = sum(n_high_discretion_application), .by = block_id) |>
  filter(pre_permits > 0)
panel <- panel |>
  filter(block_id %in% active$block_id) |>
  mutate(stricter = as.integer(strictness_change_frozen > 0), lenient = as.integer(strictness_change_frozen < 0),
    signed = stricter - lenient, post = as.integer(year >= 2015L),
    post_signed = post * signed, post_stricter = post * stricter, post_lenient = post * lenient)

# Nearest 2003-map segment of each block's ward pair, from the block's centroid.
blocks <- panel |> distinct(block_id, ward_pair_id)
stopifnot(!anyDuplicated(blocks$block_id))
centroids <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE, col_select = c(the_geom, GEOID10)) |>
  transmute(block_id = as.character(GEOID10), geometry = the_geom) |>
  filter(block_id %in% blocks$block_id) |>
  distinct(block_id, .keep_all = TRUE) |>
  st_as_sf(wkt = "geometry", crs = 4269) |>
  st_make_valid() |>
  st_transform(3435) |>
  st_point_on_surface() |>
  inner_join(blocks, by = "block_id", relationship = "one-to-one")
segments <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg", eras = "2003_2014")
centroids$segment_id <- assign_points_to_nearest_segments(centroids, rep("2003_2014", nrow(centroids)),
  centroids$ward_pair_id, segments, max_distance = units::set_units(segment_search_ft, "ft"))
unassigned <- sum(is.na(centroids$segment_id) | centroids$segment_id == "")
cat("blocks:", nrow(blocks), " assigned to a segment:", nrow(centroids) - unassigned, " unassigned:", unassigned, "\n")
panel <- panel |>
  inner_join(st_drop_geometry(centroids) |> filter(!is.na(segment_id), segment_id != "") |> select(block_id, segment_id),
    by = "block_id", relationship = "many-to-one")

results <- list()
for (fe in names(fixed_effects)) {
  fit <- function(rhs) fixest::fepois(as.formula(paste("n_high_discretion_application ~", rhs, "| block_id +",
    fixed_effects[[fe]])), data = panel, cluster = ~ward_pair_id, notes = FALSE)
  models <- list(
    signed = list(event = fit("i(relative_year, signed, ref = -1)"), pooled = fit("post_signed")),
    separate = list(event = fit("i(relative_year, stricter, ref = -1) + i(relative_year, lenient, ref = -1)"),
      pooled = fit("post_stricter + post_lenient"))
  )
  for (arm in c("signed", "stricter", "lenient")) {
    m <- if (arm == "signed") models$signed else models$separate
    terms <- paste0("relative_year::", setdiff(-5:5, -1), ":", arm)
    pre <- terms[1:4]
    pre_p <- fixest::wald(m$event, keep = paste0("^(", paste(gsub("([:^-])", "\\\\\\1", pre), collapse = "|"), ")$"),
      print = FALSE)$p
    pooled_term <- paste0("post_", arm)
    results[[length(results) + 1]] <- tibble(fixed_effect = fe, arm = .env$arm, event_time = c(setdiff(-5:5, -1), -1L),
      estimate = c(coef(m$event)[terms], 0), std_error = c(fixest::se(m$event)[terms], 0),
      pooled_estimate = coef(m$pooled)[[pooled_term]], pooled_std_error = fixest::se(m$pooled)[[pooled_term]],
      pooled_p_value = fixest::pvalue(m$pooled)[[pooled_term]], pretrend_p_value = pre_p,
      observations = nobs(m$pooled), blocks = n_distinct(panel$block_id))
  }
}
results <- bind_rows(results)
write_csv(results, "../output/event_study_segment_fe.csv")

stars <- function(p) case_when(p <= 0.01 ~ "***", p <= 0.05 ~ "**", p <= 0.10 ~ "*", TRUE ~ "")
plot_data <- mutate(results, ci_low = estimate - 1.96 * std_error, ci_high = estimate + 1.96 * std_error)
colors <- c(signed = "#176B58", stricter = "#D92D27", lenient = "#2478B5")
titles <- c(signed = "Both reassignment directions", stricter = "Assigned to more stringent aldermen",
  lenient = "Assigned to more lenient aldermen")
fe_labels <- c(ward_pair = "ward-pair x year FE (paper)", segment = "segment x year FE")
panels <- list()
for (fe in names(fixed_effects)) for (arm in names(colors)) {
  p <- filter(plot_data, fixed_effect == fe, arm == .env$arm)
  panels[[length(panels) + 1]] <- ggplot(p, aes(event_time, estimate)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = colors[[arm]], alpha = 0.16) +
    geom_line(color = colors[[arm]], linewidth = 0.85) + geom_point(color = colors[[arm]], size = 2) +
    coord_cartesian(ylim = range(c(plot_data$ci_low, plot_data$ci_high))) +
    scale_x_continuous(breaks = -5:5) +
    labs(title = paste0(titles[[arm]], ", ", fe_labels[[fe]]),
      subtitle = sprintf("Pooled Estimate = %.3f%s (SE %.3f); pre-trend p = %.2f", p$pooled_estimate[1],
        stars(p$pooled_p_value[1]), p$pooled_std_error[1], p$pretrend_p_value[1]),
      x = "Years since 2015 redistricting", y = "Effect on annual permits (log points)") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", size = 10), plot.subtitle = element_text(size = 10, face = "bold"))
}
ggsave("../output/event_study_segment_fe.pdf", wrap_plots(panels, ncol = 3), width = 16, height = 8, bg = "white")
