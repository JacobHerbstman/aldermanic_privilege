# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)
bandwidth_m <- 152.4

scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L, spec_id == "controls_drop_income") %>%
  transmute(
    alderman_key = str_squish(str_to_lower(alderman)),
    score
  )
if (anyDuplicated(scores$alderman_key) > 0) {
  stop("The through-2022 no-income score is not unique by alderman.", call. = FALSE)
}

lineage_pins <- read_csv(
  "../input/density_project_lineage.csv",
  show_col_types = FALSE,
  col_types = cols(member_pins = col_character(), .default = col_guess())
) %>%
  left_join(
    read_csv(
      "../input/density_parcel_address_lineage_evidence.csv",
      show_col_types = FALSE,
      col_types = cols(member_pins = col_character(), .default = col_guess())
    ) %>%
      select(project_key, address_audit_recommendation),
    by = "project_key",
    relationship = "one-to-one"
  ) %>%
  filter(
    recommended_action == "candidate_for_recovery",
    address_audit_recommendation != "exclude_address_confirmed_duplicate"
  ) %>%
  select(project_key, member_pins) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  rename(pin = member_pins)

if (anyDuplicated(lineage_pins$pin) > 0) {
  stop("A historical PIN belongs to more than one retained lineage group.", call. = FALSE)
}

recovered <- read_csv(
  "../input/density_historical_recovered_exact_rows.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  inner_join(lineage_pins, by = "pin", relationship = "one-to-one")
address_recovered <- read_csv(
  "../input/density_parcel_address_recovered_model_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    source_class = col_character(),
    .default = col_guess()
  )
)
production <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
)

broad_sample <- bind_rows(production, recovered, address_recovered) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    alderman_own_key = str_squish(str_to_lower(alderman_own)),
    alderman_neighbor_key = str_squish(str_to_lower(alderman_neighbor)),
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code)
  ) %>%
  left_join(
    scores %>% rename(alderman_own_key = alderman_key, score_own = score),
    by = "alderman_own_key",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_neighbor_key = alderman_key, score_neighbor = score),
    by = "alderman_neighbor_key",
    relationship = "many-to-one"
  ) %>%
  mutate(
    signed_distance_new = if_else(
      score_own > score_neighbor,
      abs(dist_to_boundary_m),
      -abs(dist_to_boundary_m)
    )
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006L,
    construction_year <= 2022L,
    !is.na(ward_pair),
    is.finite(signed_distance_new),
    !is.na(zone_group),
    !is.na(segment_id),
    segment_id != ""
  )

if (anyNA(broad_sample$score_own) || anyNA(broad_sample$score_neighbor)) {
  stop("A broad-sample alderman is missing a no-income score.", call. = FALSE)
}

donut_sample <- read_csv(
  "../input/density_score_sensitivity_sample.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess()),
  guess_max = Inf
) %>%
  mutate(
    alderman_own_key = str_squish(str_to_lower(alderman_own)),
    alderman_neighbor_key = str_squish(str_to_lower(alderman_neighbor))
  ) %>%
  left_join(
    scores %>% rename(alderman_own_key = alderman_key, score_own = score),
    by = "alderman_own_key",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_neighbor_key = alderman_key, score_neighbor = score),
    by = "alderman_neighbor_key",
    relationship = "many-to-one"
  ) %>%
  mutate(
    signed_distance_new = if_else(
      score_own > score_neighbor,
      abs(dist_to_boundary_m),
      -abs(dist_to_boundary_m)
    )
  )

placebo_1000_sample <- read_csv(
  "../input/density_placebo_1000ft_sample.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    source_class = col_character(),
    .default = col_guess()
  )
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    alderman_own_key = str_squish(str_to_lower(alderman_own)),
    alderman_neighbor_key = str_squish(str_to_lower(alderman_neighbor)),
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code)
  ) %>%
  left_join(
    scores %>% rename(alderman_own_key = alderman_key, score_own = score),
    by = "alderman_own_key",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_neighbor_key = alderman_key, score_neighbor = score),
    by = "alderman_neighbor_key",
    relationship = "many-to-one"
  ) %>%
  mutate(
    signed_distance_new = if_else(
      score_own > score_neighbor,
      abs(dist_to_boundary_m),
      -abs(dist_to_boundary_m)
    )
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006L,
    construction_year <= 2022L,
    !is.na(ward_pair),
    is.finite(signed_distance_new),
    !is.na(zone_group),
    !is.na(segment_id),
    segment_id != ""
  )

if (anyNA(placebo_1000_sample$score_own) || anyNA(placebo_1000_sample$score_neighbor)) {
  stop("An extended-placebo alderman is missing a no-income score.", call. = FALSE)
}

robustness_specs <- tribble(
  ~check, ~source, ~center_m, ~inner_m,
  "Main cutoff", "donut", 0, 0,
  "Donut 25ft", "donut", 0, 7.62,
  "Donut 50ft", "donut", 0, 15.24,
  "Placebo -500ft", "broad", -152.4, 0,
  "Placebo +500ft", "broad", 152.4, 0,
  "Placebo -1000ft", "placebo_1000", -304.8, 0,
  "Placebo +1000ft", "placebo_1000", 304.8, 0
)

save_rd_plot <- function(model_data, model, check, sample_name, outcome_name, inner_m) {
  residual_model <- feols(
    as.formula(paste0(
      "outcome ~ ",
      paste(controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = model_data,
    notes = FALSE,
    warn = FALSE
  )

  removed <- residual_model$obs_selection$obsRemoved
  keep_idx <- if (is.null(removed)) {
    seq_len(nrow(model_data))
  } else {
    setdiff(seq_len(nrow(model_data)), abs(as.integer(removed)))
  }
  plot_data <- model_data[keep_idx, , drop = FALSE] %>%
    mutate(residualized_outcome = as.numeric(resid(residual_model)))

  if (nrow(plot_data) != nobs(residual_model) || nobs(model) != nobs(residual_model)) {
    stop("The plotted and estimated density samples do not align.", call. = FALSE)
  }

  display_model <- feols(
    residualized_outcome ~ side * running_distance,
    data = plot_data,
    cluster = ~ward_pair,
    notes = FALSE,
    warn = FALSE
  )

  breaks_m <- seq(-bandwidth_m, bandwidth_m, length.out = 11L)
  bin_width_m <- bandwidth_m / 5
  bins <- plot_data %>%
    mutate(
      bin_idx = pmin(
        findInterval(
          running_distance,
          breaks_m,
          rightmost.closed = TRUE,
          all.inside = TRUE
        ),
        length(breaks_m) - 1L
      ),
      bin_center_m = breaks_m[bin_idx] + bin_width_m / 2
    ) %>%
    group_by(bin_idx, bin_center_m, side) %>%
    summarise(mean_y = mean(residualized_outcome), .groups = "drop")

  line_distance <- if (inner_m == 0) {
    c(
      seq(-bandwidth_m, 0, length.out = 200),
      seq(0, bandwidth_m, length.out = 200)[-1]
    )
  } else {
    c(
      seq(-bandwidth_m, -inner_m, length.out = 160),
      seq(inner_m, bandwidth_m, length.out = 160)
    )
  }
  line_data <- tibble(running_distance = line_distance) %>%
    mutate(side = as.integer(running_distance > 0))

  coefficient_names <- names(coef(display_model))
  design_matrix <- model.matrix(~side * running_distance, data = line_data)
  design_matrix <- design_matrix[, coefficient_names, drop = FALSE]
  variance_matrix <- vcov(display_model)
  critical_value <- qt(
    0.975,
    df = max(n_distinct(plot_data$ward_pair) - 1, 1)
  )
  line_data <- line_data %>%
    mutate(
      fit = as.numeric(design_matrix %*% coef(display_model)),
      fit_se = sqrt(pmax(
        rowSums((design_matrix %*% variance_matrix) * design_matrix),
        0
      )),
      ci_low = fit - critical_value * fit_se,
      ci_high = fit + critical_value * fit_se
    )

  coefficient_table <- coeftable(model)
  jump <- unname(coefficient_table["side", "Estimate"])
  jump_se <- unname(coefficient_table["side", "Std. Error"])
  jump_p <- unname(coefficient_table["side", "Pr(>|t|)"])
  stars <- case_when(
    jump_p <= 0.01 ~ "***",
    jump_p <= 0.05 ~ "**",
    jump_p <= 0.10 ~ "*",
    TRUE ~ ""
  )

  check_key <- recode(
    check,
    "Donut 25ft" = "donut25ft",
    "Donut 50ft" = "donut50ft",
    "Placebo -500ft" = "placebo_neg500ft",
    "Placebo +500ft" = "placebo_pos500ft",
    "Placebo -1000ft" = "placebo_neg1000ft",
    "Placebo +1000ft" = "placebo_pos1000ft"
  )
  outcome_key <- recode(outcome_name, density_far = "far", density_dupac = "dupac")
  outcome_label <- recode(
    outcome_name,
    density_far = "Log(FAR)",
    density_dupac = "Log(DUPAC)"
  )
  sample_label <- recode(
    sample_name,
    all = "All construction",
    multifamily = "Multifamily"
  )
  x_label <- if (str_starts(check, "Placebo")) {
    "Distance to placebo cutoff (ft)"
  } else {
    "Distance to ward boundary (ft)"
  }

  bins <- bins %>% mutate(distance_ft = bin_center_m / 0.3048)
  line_data <- line_data %>% mutate(distance_ft = running_distance / 0.3048)

  plot <- ggplot() +
    geom_ribbon(
      data = line_data,
      aes(distance_ft, ymin = ci_low, ymax = ci_high, fill = factor(side)),
      alpha = 0.16,
      color = NA
    ) +
    geom_line(
      data = line_data,
      aes(distance_ft, fit, color = factor(side)),
      linewidth = 0.8
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray55", linewidth = 0.35) +
    geom_point(
      data = bins,
      aes(distance_ft, mean_y, fill = factor(side)),
      shape = 21,
      color = "white",
      stroke = 0.35,
      size = 2
    ) +
    scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_x_continuous(limits = c(-500, 500), breaks = c(-500, -250, 0, 250, 500)) +
    labs(
      title = paste(sample_label, outcome_label, sep = ": "),
      subtitle = sprintf(
        "Jump = %.3f%s (SE %.3f), N = %s",
        jump,
        stars,
        jump_se,
        format(nobs(model), big.mark = ",")
      ),
      x = x_label,
      y = paste("Residualized", outcome_label)
    ) +
    theme_bw(base_size = 8.5) +
    theme(
      plot.title = element_text(face = "bold", size = 9),
      plot.subtitle = element_text(size = 8),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7),
      panel.grid.minor = element_blank()
    )

  ggsave(
    sprintf(
      "../output/preview_density_%s_%s_%s.pdf",
      check_key,
      sample_name,
      outcome_key
    ),
    plot,
    width = 4.8,
    height = 2.8,
    bg = "white"
  )
}

result_rows <- list()
for (spec_i in seq_len(nrow(robustness_specs))) {
  source_name <- robustness_specs$source[spec_i]
  if (source_name == "donut") {
    base_data <- donut_sample
  } else if (source_name == "broad") {
    base_data <- broad_sample
  } else {
    base_data <- placebo_1000_sample
  }
  center_m <- robustness_specs$center_m[spec_i]
  inner_m <- robustness_specs$inner_m[spec_i]
  base_data <- base_data %>%
    mutate(running_distance = signed_distance_new - center_m) %>%
    filter(
      abs(running_distance) <= bandwidth_m,
      abs(running_distance) >= inner_m
    )

  for (sample_name in c("all", "multifamily")) {
    sample_data <- if (sample_name == "all") {
      base_data %>% filter(unitscount > 0)
    } else {
      base_data %>% filter(unitscount > 1)
    }

    for (outcome_name in c("density_far", "density_dupac")) {
      model_data <- sample_data %>%
        filter(is.finite(.data[[outcome_name]]), .data[[outcome_name]] > 0) %>%
        mutate(
          outcome = log(.data[[outcome_name]]),
          side = as.integer(running_distance > 0)
        )

      model <- feols(
        as.formula(paste0(
          "outcome ~ side * running_distance + ",
          paste(controls, collapse = " + "),
          " | zone_group + segment_id + construction_year"
        )),
        data = model_data,
        cluster = ~ward_pair,
        notes = FALSE,
        warn = FALSE
      )
      coefficient_table <- coeftable(model)
      result_rows[[length(result_rows) + 1L]] <- tibble(
        check = robustness_specs$check[spec_i],
        sample = sample_name,
        outcome = outcome_name,
        estimate = unname(coefficient_table["side", "Estimate"]),
        std_error = unname(coefficient_table["side", "Std. Error"]),
        p_value = unname(coefficient_table["side", "Pr(>|t|)"]),
        observations = nobs(model),
        ward_pairs = n_distinct(model_data$ward_pair)
      )

      if (robustness_specs$check[spec_i] != "Main cutoff") {
        save_rd_plot(
          model_data,
          model,
          robustness_specs$check[spec_i],
          sample_name,
          outcome_name,
          inner_m
        )
      }
    }
  }
}

results <- bind_rows(result_rows) %>%
  mutate(
    ci_low = estimate - 1.96 * std_error,
    ci_high = estimate + 1.96 * std_error,
    sample_label = recode(sample, all = "All construction", multifamily = "Multifamily"),
    outcome_label = recode(outcome, density_far = "Log FAR", density_dupac = "Log DUPAC"),
    check = factor(check, levels = robustness_specs$check)
  )
write_csv(results, "../output/drop_income_density_short_paper_robustness.csv")

robustness_plot <- ggplot(
  results,
  aes(estimate, check, xmin = ci_low, xmax = ci_high)
) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = 0.4) +
  geom_errorbarh(height = 0, color = "#176B58", linewidth = 0.7) +
  geom_point(color = "#176B58", size = 2.2) +
  facet_grid(sample_label ~ outcome_label, scales = "free_x") +
  labs(
    title = "Density Robustness with the No-Income Stringency Score",
    subtitle = "Corrected project-lineage sample; 95% confidence intervals",
    x = "Estimated discontinuity",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )
ggsave(
  "../output/drop_income_density_short_paper_robustness.pdf",
  robustness_plot,
  width = 9.2,
  height = 6.6,
  bg = "white"
)
ggsave(
  "../output/drop_income_density_short_paper_robustness.png",
  robustness_plot,
  width = 9.2,
  height = 6.6,
  dpi = 200,
  bg = "white"
)
