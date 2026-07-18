# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

lineage <- read_csv(
  "../output/density_project_lineage.csv",
  show_col_types = FALSE,
  col_types = cols(member_pins = col_character(), .default = col_guess())
) %>%
  left_join(
    read_csv(
      "../output/density_parcel_address_lineage_evidence.csv",
      show_col_types = FALSE,
      col_types = cols(member_pins = col_character(), .default = col_guess())
    ) %>%
      select(project_key, address_audit_recommendation),
    by = "project_key",
    relationship = "one-to-one"
  ) %>%
  mutate(
    final_recovery =
      recommended_action == "candidate_for_recovery" &
      address_audit_recommendation != "exclude_address_confirmed_duplicate"
  )

lineage_pins <- lineage %>%
  select(project_key, member_pins, final_recovery) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  rename(pin = member_pins)

if (anyDuplicated(lineage_pins$pin) > 0) {
  stop("A historical PIN belongs to more than one lineage group.", call. = FALSE)
}

recovered <- read_csv(
  "../input/density_historical_recovered_exact_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    source_class = col_character(),
    .default = col_guess()
  )
) %>%
  left_join(lineage_pins, by = "pin", relationship = "one-to-one") %>%
  filter(final_recovery) %>%
  mutate(sample_source = "recovered_historical_parcel_coordinate")

if (any(is.na(recovered$project_key))) {
  stop("A recovered exact-year row is missing from final project lineage.", call. = FALSE)
}

address_recovered <- read_csv(
  "../output/density_parcel_address_recovered_model_rows.csv",
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
) %>%
  mutate(
    sample_source = "production_current_2025_coordinate",
    project_key = NA_character_
  )

parcels <- bind_rows(production, recovered, address_recovered) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id)
  )

if (anyDuplicated(parcels$pin) > 0) {
  stop("The final lineage sample contains duplicate PINs.", call. = FALSE)
}

production_geometry <- st_read(
  "../input/parcels_with_geometry.gpkg",
  quiet = TRUE
) %>%
  mutate(pin = as.character(pin)) %>%
  select(pin) %>%
  semi_join(production %>% select(pin), by = "pin")
st_geometry(production_geometry) <- "geometry"

if (anyDuplicated(production_geometry$pin) > 0) {
  stop("Production parcel geometry is not unique by PIN.", call. = FALSE)
}

recovered_geometry <- st_as_sf(
  recovered %>% select(pin, longitude, latitude),
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435) %>%
  select(pin)

address_geometry <- st_as_sf(
  address_recovered %>% select(pin, address_x_crs_3435, address_y_crs_3435),
  coords = c("address_x_crs_3435", "address_y_crs_3435"),
  crs = 3435,
  remove = FALSE
) %>%
  select(pin)

all_geometry <- bind_rows(
  production_geometry,
  recovered_geometry,
  address_geometry
)

if (nrow(all_geometry) != nrow(parcels) || anyDuplicated(all_geometry$pin) > 0) {
  stop("Final lineage geometry does not match the final lineage sample.", call. = FALSE)
}
if (any(st_is_empty(all_geometry))) {
  stop("The final lineage sample contains an empty geometry.", call. = FALSE)
}

zoning <- st_read("../input/zoning_data_clean.gpkg", quiet = TRUE) %>%
  select(fresh_zone_code = zone_code) %>%
  st_make_valid() %>%
  st_transform(3435)

zoning_lookup <- all_geometry %>%
  st_transform(st_crs(zoning)) %>%
  st_join(zoning, left = TRUE, largest = TRUE) %>%
  st_drop_geometry()

if (nrow(zoning_lookup) != nrow(parcels) || anyDuplicated(zoning_lookup$pin) > 0) {
  stop("Fresh zoning join changed final lineage row cardinality.", call. = FALSE)
}

scores <- read_csv(
  "../input/permit_zero_day_score_aldermen.csv",
  show_col_types = FALSE
) %>%
  filter(
    cutoff == 2022,
    variant %in% c(
      "official",
      "positive_log_all_permit_volume",
      "positive_log_all_permit_volume_no_porch",
      "nonnegative_unlogged_days"
    )
  ) %>%
  select(variant, alderman, score)

if (anyDuplicated(scores[, c("variant", "alderman")]) > 0) {
  stop("Score audit input is not unique by variant and alderman.", call. = FALSE)
}

score_variants <- list(
  logged_days = scores %>% filter(variant == "official"),
  logged_days_all_permit_volume = scores %>%
    filter(variant == "positive_log_all_permit_volume"),
  logged_days_all_permit_volume_no_porch = scores %>%
    filter(variant == "positive_log_all_permit_volume_no_porch"),
  unlogged_nonnegative_days = scores %>%
    filter(variant == "nonnegative_unlogged_days")
)

parcels <- parcels %>%
  left_join(zoning_lookup, by = "pin", relationship = "one-to-one") %>%
  mutate(zone_group = zone_group_from_code(fresh_zone_code)) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    !is.na(segment_id),
    segment_id != "",
    !is.na(fresh_zone_code)
  )

model_rows <- list()
side_assignments <- list()

for (score_spec in names(score_variants)) {
  score_lookup <- score_variants[[score_spec]] %>% select(alderman, score)

  score_sample <- parcels %>%
    left_join(
      score_lookup %>% rename(alderman_own = alderman, score_own = score),
      by = "alderman_own",
      relationship = "many-to-one"
    ) %>%
    left_join(
      score_lookup %>% rename(alderman_neighbor = alderman, score_neighbor = score),
      by = "alderman_neighbor",
      relationship = "many-to-one"
    )

  if (anyNA(score_sample$score_own) || anyNA(score_sample$score_neighbor)) {
    stop(sprintf("The %s score is missing for a density-sample alderman.", score_spec), call. = FALSE)
  }
  if (any(score_sample$score_own == score_sample$score_neighbor)) {
    stop(sprintf("The %s score produces a tied boundary.", score_spec), call. = FALSE)
  }

  score_sample <- score_sample %>%
    mutate(
      score_spec = score_spec,
      strictness_own_model = score_own,
      side_model = as.integer(score_own > score_neighbor),
      signed_distance_model = if_else(
        side_model == 1L,
        abs(dist_to_boundary_m),
        -abs(dist_to_boundary_m)
      ),
      lenient_dist = abs(signed_distance_model) *
        as.integer(signed_distance_model <= 0),
      strict_dist = abs(signed_distance_model) *
        as.integer(signed_distance_model > 0)
    )

  side_assignments[[score_spec]] <- score_sample %>%
    select(pin, score_spec, side_model)

  for (construction_sample in c("all", "multifamily")) {
    construction_rows <- if (construction_sample == "all") {
      score_sample %>% filter(unitscount > 0)
    } else {
      score_sample %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac", "unitscount")) {
      outcome_scales <- if (outcome == "unitscount") "log" else c("log", "level")
      for (outcome_scale in outcome_scales) {
        outcome_rows <- construction_rows %>%
          filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
          mutate(
            outcome_value = if (outcome_scale == "log") {
              log(.data[[outcome]])
            } else {
              .data[[outcome]]
            }
          )

        for (treatment in c("continuous", "binary")) {
          treatment_variable <- if (treatment == "continuous") {
            "strictness_own_model"
          } else {
            "side_model"
          }

          model <- feols(
            as.formula(paste0(
              "outcome_value ~ ", treatment_variable,
              " + lenient_dist + strict_dist + ",
              paste(demographic_controls, collapse = " + "),
              " | zone_group + segment_id + construction_year"
            )),
            data = outcome_rows,
            cluster = ~ward_pair,
            notes = FALSE
          )

          coefficient_table <- coeftable(model)
          used_rows <- obs(model)
          model_rows[[length(model_rows) + 1L]] <- tibble(
            score_spec,
            construction_sample,
            outcome,
            outcome_scale,
            treatment,
            estimate = unname(coefficient_table[treatment_variable, "Estimate"]),
            se = unname(coefficient_table[treatment_variable, "Std. Error"]),
            p_value = unname(coefficient_table[treatment_variable, "Pr(>|t|)"]),
            n = nobs(model),
            dep_var_mean = mean(outcome_rows[[outcome]][used_rows]),
            dep_var_median = median(outcome_rows[[outcome]][used_rows]),
            dep_var_p95 = quantile(
              outcome_rows[[outcome]][used_rows],
              0.95,
              names = FALSE
            ),
            dep_var_max = max(outcome_rows[[outcome]][used_rows]),
            recovered_rows = sum(
              outcome_rows$sample_source[used_rows] !=
                "production_current_2025_coordinate"
            ),
            ward_pairs = n_distinct(outcome_rows$ward_pair[used_rows])
          )
        }
      }
    }
  }
}

side_changes <- bind_rows(side_assignments) %>%
  left_join(
    bind_rows(side_assignments) %>%
      filter(score_spec == "logged_days") %>%
      select(pin, logged_side = side_model),
    by = "pin",
    relationship = "many-to-one"
  ) %>%
  mutate(side_changed = side_model != logged_side)

side_change_summary <- bind_rows(
  side_changes %>%
    semi_join(
      parcels %>% filter(unitscount > 0) %>% select(pin),
      by = "pin"
    ) %>%
    mutate(construction_sample = "all"),
  side_changes %>%
    semi_join(
      parcels %>% filter(unitscount > 1) %>% select(pin),
      by = "pin"
    ) %>%
    mutate(construction_sample = "multifamily")
) %>%
  group_by(score_spec, construction_sample) %>%
  summarise(
    binary_side_changes = sum(side_changed),
    binary_side_change_share = mean(side_changed),
    .groups = "drop"
  )

model_results <- bind_rows(model_rows) %>%
  left_join(
    bind_rows(model_rows) %>%
      filter(score_spec == "logged_days") %>%
      select(
        construction_sample,
        outcome,
        outcome_scale,
        treatment,
        logged_estimate = estimate,
        logged_se = se,
        logged_p_value = p_value
      ),
    by = c("construction_sample", "outcome", "outcome_scale", "treatment"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    side_change_summary,
    by = c("score_spec", "construction_sample"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    estimate_change_from_logged = estimate - logged_estimate
  ) %>%
  arrange(construction_sample, outcome, outcome_scale, treatment, score_spec)

write_csv(model_results, "../output/density_unlogged_score_models.csv")

current_addresses <- read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(pin, current_property_address = prop_address_full)

if (anyDuplicated(current_addresses$pin) > 0) {
  stop("Current parcel addresses are not unique by PIN.", call. = FALSE)
}

historical_addresses <- read_csv(
  "../output/density_historical_address_records.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  inner_join(
    parcels %>% select(pin, construction_year),
    by = "pin",
    relationship = "many-to-one"
  ) %>%
  filter(!is.na(property_address)) %>%
  mutate(address_year_gap = abs(year - construction_year)) %>%
  arrange(pin, address_year_gap, desc(year), property_address) %>%
  group_by(pin) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    pin,
    historical_property_address = property_address,
    historical_address_year = year
  )

unlogged_scores <- score_variants$unlogged_nonnegative_days %>%
  select(alderman, score)

tail_sample <- parcels %>%
  filter(unitscount > 1) %>%
  left_join(current_addresses, by = "pin", relationship = "many-to-one") %>%
  left_join(historical_addresses, by = "pin", relationship = "one-to-one") %>%
  left_join(
    unlogged_scores %>%
      rename(alderman_own = alderman, unlogged_score_own = score),
    by = "alderman_own",
    relationship = "many-to-one"
  ) %>%
  left_join(
    unlogged_scores %>%
      rename(alderman_neighbor = alderman, unlogged_score_neighbor = score),
    by = "alderman_neighbor",
    relationship = "many-to-one"
  ) %>%
  mutate(
    project_address = case_when(
      sample_source == "recovered_address_coordinate" ~ selected_address,
      sample_source == "recovered_historical_parcel_coordinate" ~
        historical_property_address,
      TRUE ~ current_property_address
    ),
    unlogged_boundary_side = if_else(
      unlogged_score_own > unlogged_score_neighbor,
      "more_stringent",
      "less_stringent"
    ),
    distance_to_boundary_ft = dist_to_boundary_m / 0.3048
  )

tail_projects <- bind_rows(
  tail_sample %>%
    arrange(desc(density_far), pin) %>%
    slice_head(n = 15) %>%
    mutate(tail_metric = "FAR", tail_rank = row_number()),
  tail_sample %>%
    arrange(desc(density_dupac), pin) %>%
    slice_head(n = 15) %>%
    mutate(tail_metric = "DUPAC", tail_rank = row_number())
) %>%
  select(
    tail_metric,
    tail_rank,
    pin,
    project_address,
    construction_year,
    unitscount,
    storiescount,
    arealotsf,
    areabuilding,
    density_far,
    density_dupac,
    ward_pair,
    ward,
    other_ward,
    alderman_own,
    alderman_neighbor,
    unlogged_score_own,
    unlogged_score_neighbor,
    unlogged_boundary_side,
    distance_to_boundary_ft,
    zone_group,
    fresh_zone_code,
    sample_source,
    project_key
  ) %>%
  arrange(tail_metric, tail_rank)

write_csv(
  tail_projects,
  "../output/density_unlogged_outcome_tail_projects.csv"
)

plot_score <- score_variants$logged_days_all_permit_volume %>%
  select(alderman, score)

plot_base <- parcels %>%
  left_join(
    plot_score %>% rename(alderman_own = alderman, score_own = score),
    by = "alderman_own",
    relationship = "many-to-one"
  ) %>%
  left_join(
    plot_score %>% rename(alderman_neighbor = alderman, score_neighbor = score),
    by = "alderman_neighbor",
    relationship = "many-to-one"
  ) %>%
  mutate(
    side = as.integer(score_own > score_neighbor),
    running_distance = if_else(
      side == 1L,
      abs(dist_to_boundary_m),
      -abs(dist_to_boundary_m)
    )
  )

if (anyNA(plot_base$score_own) || anyNA(plot_base$score_neighbor)) {
  stop("The all-permit workload score is missing for a density-sample alderman.", call. = FALSE)
}

distance_display <- distance_display_config("ft")
plot_bandwidth_m <- 152.4
bins_per_side <- 5L
panel_specs <- tribble(
  ~outcome, ~construction_sample, ~title, ~filename,
  "density_far", "all", "All Construction: FAR",
  "density_all_workload_rd_all_far.png",
  "density_far", "multifamily", "Multifamily: FAR",
  "density_all_workload_rd_multifamily_far.png",
  "density_dupac", "all", "All Construction: DUPAC",
  "density_all_workload_rd_all_dupac.png",
  "density_dupac", "multifamily", "Multifamily: DUPAC",
  "density_all_workload_rd_multifamily_dupac.png",
  "unitscount", "all", "All Construction: Units",
  "density_all_workload_rd_all_units.png",
  "unitscount", "multifamily", "Multifamily: Units",
  "density_all_workload_rd_multifamily_units.png"
)

panels <- vector("list", nrow(panel_specs))
for (panel_i in seq_len(nrow(panel_specs))) {
  outcome_i <- panel_specs$outcome[panel_i]
  sample_i <- panel_specs$construction_sample[panel_i]

  plot_data <- if (sample_i == "all") {
    plot_base %>% filter(unitscount > 0)
  } else {
    plot_base %>% filter(unitscount > 1)
  }
  plot_data <- plot_data %>%
    filter(is.finite(.data[[outcome_i]]), .data[[outcome_i]] > 0) %>%
    mutate(outcome_value = log(.data[[outcome_i]]))

  residual_model <- feols(
    as.formula(paste0(
      "outcome_value ~ ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = plot_data,
    notes = FALSE
  )
  removed_rows <- residual_model$obs_selection$obsRemoved
  kept_rows <- if (is.null(removed_rows)) {
    seq_len(nrow(plot_data))
  } else {
    setdiff(seq_len(nrow(plot_data)), abs(as.integer(removed_rows)))
  }
  plot_data <- plot_data[kept_rows, , drop = FALSE] %>%
    mutate(residualized_outcome = as.numeric(resid(residual_model)))

  cutoff_model <- feols(
    as.formula(paste0(
      "outcome_value ~ side * running_distance + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = plot_data,
    cluster = ~ward_pair,
    notes = FALSE
  )
  cutoff_table <- coeftable(cutoff_model)
  cutoff_estimate <- unname(cutoff_table["side", "Estimate"])
  cutoff_se <- unname(cutoff_table["side", "Std. Error"])
  cutoff_p <- unname(cutoff_table["side", "Pr(>|t|)"])

  expected_result <- model_results %>%
    filter(
      score_spec == "logged_days_all_permit_volume",
      construction_sample == sample_i,
      outcome == outcome_i,
      outcome_scale == "log",
      treatment == "binary"
    )
  if (
    nrow(expected_result) != 1L ||
    abs(cutoff_estimate - expected_result$estimate) > 1e-8 ||
    abs(cutoff_se - expected_result$se) > 1e-8
  ) {
    stop(
      sprintf("All-workload plot/model mismatch for %s %s.", sample_i, outcome_i),
      call. = FALSE
    )
  }

  cutoff_stars <- case_when(
    cutoff_p <= 0.01 ~ "***",
    cutoff_p <= 0.05 ~ "**",
    cutoff_p <= 0.10 ~ "*",
    TRUE ~ ""
  )
  display_model <- feols(
    residualized_outcome ~ side * running_distance,
    data = plot_data,
    cluster = ~ward_pair,
    notes = FALSE
  )

  breaks_m <- seq(
    -plot_bandwidth_m,
    plot_bandwidth_m,
    length.out = 2L * bins_per_side + 1L
  )
  bin_width_m <- plot_bandwidth_m / bins_per_side
  bins <- plot_data %>%
    mutate(
      bin = pmin(
        findInterval(
          running_distance,
          breaks_m,
          rightmost.closed = TRUE,
          all.inside = TRUE
        ),
        length(breaks_m) - 1L
      ),
      bin_center_m = breaks_m[bin] + bin_width_m / 2
    ) %>%
    group_by(bin, bin_center_m, side) %>%
    summarise(mean_y = mean(residualized_outcome), .groups = "drop") %>%
    mutate(bin_center = bin_center_m * distance_display$scale)

  line_data <- tibble(
    running_distance = c(
      seq(-plot_bandwidth_m, 0, length.out = 200),
      seq(0, plot_bandwidth_m, length.out = 200)[-1]
    )
  ) %>%
    mutate(side = as.integer(running_distance > 0))
  coefficient_names <- names(coef(display_model))
  design_matrix <- model.matrix(~side * running_distance, data = line_data)
  design_matrix <- design_matrix[, coefficient_names, drop = FALSE]
  display_vcov <- vcov(display_model)
  critical_value <- qt(
    0.975,
    df = max(n_distinct(plot_data$ward_pair) - 1L, 1L)
  )
  line_data <- line_data %>%
    mutate(
      fit = as.numeric(design_matrix %*% coef(display_model)),
      fit_se = sqrt(pmax(
        rowSums((design_matrix %*% display_vcov) * design_matrix),
        0
      )),
      ci_low = fit - critical_value * fit_se,
      ci_high = fit + critical_value * fit_se,
      running_distance_display = running_distance * distance_display$scale
    )

  y_min <- min(c(bins$mean_y, line_data$ci_low), na.rm = TRUE)
  y_max <- max(c(bins$mean_y, line_data$ci_high), na.rm = TRUE)
  y_padding <- max(0.15 * (y_max - y_min), 0.05)

  panels[[panel_i]] <- ggplot() +
    geom_ribbon(
      data = line_data,
      aes(
        x = running_distance_display,
        ymin = ci_low,
        ymax = ci_high,
        fill = factor(side)
      ),
      alpha = 0.16,
      color = NA
    ) +
    geom_line(
      data = line_data,
      aes(x = running_distance_display, y = fit, color = factor(side)),
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray30",
      linewidth = 0.35
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "gray55",
      linewidth = 0.35
    ) +
    geom_point(
      data = bins,
      aes(x = bin_center, y = mean_y, fill = factor(side)),
      shape = 21,
      color = "white",
      stroke = 0.35,
      size = 1.85
    ) +
    scale_fill_manual(
      values = c("0" = "#1f77b4", "1" = "#d62728"),
      guide = "none"
    ) +
    scale_color_manual(
      values = c("0" = "#1f77b4", "1" = "#d62728"),
      guide = "none"
    ) +
    scale_x_continuous(
      limits = c(-500, 500),
      breaks = seq(-500, 500, by = 250)
    ) +
    coord_cartesian(ylim = c(y_min - y_padding, y_max + y_padding)) +
    labs(
      title = panel_specs$title[panel_i],
      subtitle = sprintf(
        "Jump = %.3f%s (SE %.3f)",
        cutoff_estimate,
        cutoff_stars,
        cutoff_se
      ),
      x = "Distance to ward boundary (feet; positive = more stringent side)",
      y = paste0(
        "Residualized Log(",
        case_when(
          outcome_i == "density_far" ~ "FAR",
          outcome_i == "density_dupac" ~ "DUPAC",
          TRUE ~ "Units"
        ),
        ")"
      )
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      panel.grid.minor = element_blank()
    )

  ggsave(
    file.path("../output", panel_specs$filename[panel_i]),
    plot = panels[[panel_i]],
    width = 6.5,
    height = 4.6,
    dpi = 220,
    bg = "white"
  )
}

combined_plot <- (panels[[1]] | panels[[2]]) / (panels[[3]] | panels[[4]]) +
  plot_annotation(
    title = paste(
      "Local-Linear Spatial RD: All-Permit Workload Score",
      "(Lineage-Corrected Sample, 500 ft)"
    )
  ) &
  theme(plot.title = element_text(face = "bold", size = 13))

ggsave(
  "../output/density_all_workload_rd_4panel.png",
  plot = combined_plot,
  width = 12,
  height = 8.6,
  dpi = 220,
  bg = "white"
)
ggsave(
  "../output/density_all_workload_rd_4panel.pdf",
  plot = combined_plot,
  width = 12,
  height = 8.6,
  bg = "white"
)

units_plot <- panels[[5]] | panels[[6]]
units_plot <- units_plot +
  plot_annotation(
    title = paste(
      "Local-Linear Spatial RD: Log Units",
      "(All-Permit Workload Score, 500 ft)"
    )
  ) &
  theme(plot.title = element_text(face = "bold", size = 13))

ggsave(
  "../output/density_all_workload_rd_units_2panel.png",
  plot = units_plot,
  width = 12,
  height = 4.8,
  dpi = 220,
  bg = "white"
)
ggsave(
  "../output/density_all_workload_rd_units_2panel.pdf",
  plot = units_plot,
  width = 12,
  height = 4.8,
  bg = "white"
)

write_csv(parcels, "../output/density_score_sensitivity_sample.csv")
