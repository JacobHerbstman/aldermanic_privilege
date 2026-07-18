# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")

scores <- read_csv("../output/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L, spec_id == "controls_drop_income") %>%
  select(alderman, score)

if (anyDuplicated(scores$alderman) > 0 || any(!is.finite(scores$score))) {
  stop("The no-income score must be unique and finite by alderman.", call. = FALSE)
}

rent <- read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble() %>%
  mutate(current_right = as.integer(signed_dist >= 0)) %>%
  select(-strictness_own, -strictness_neighbor, -sign, -signed_dist_m, -signed_dist) %>%
  left_join(scores, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_own = score) %>%
  left_join(scores, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    file_date = as.Date(file_date),
    year = year(file_date),
    year_month = format(file_date, "%Y-%m"),
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_dist_ft = dist_m * sign / 0.3048,
    right = as.integer(signed_dist_ft >= 0),
    ward_pair = as.character(ward_pair_id),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    beds_factor = factor(beds),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
  ) %>%
  filter(
    !is.na(file_date),
    year >= 2014L,
    year <= 2022L,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    flag_clean_location_sample,
    is.finite(beds),
    beds >= 0,
    !is.na(log_sqft),
    !is.na(log_baths),
    if_all(
      all_of(c(
        "nearest_school_dist_kft",
        "nearest_park_dist_kft",
        "nearest_major_road_dist_kft",
        "nearest_cta_stop_dist_kft",
        "lake_michigan_dist_kft"
      )),
      is.finite
    )
  )

rent_rhs <- "right + log_sqft + beds_factor + log_baths"
rent_current_rhs <- "current_right + log_sqft + beds_factor + log_baths"
if (n_distinct(rent$building_type_factor) > 1) {
  rent_rhs <- paste(rent_rhs, "+ building_type_factor")
  rent_current_rhs <- paste(rent_current_rhs, "+ building_type_factor")
}
rent_controls <- paste(
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft",
  sep = " + "
)

rent_model <- feols(
  as.formula(paste0("log(rent_price) ~ ", rent_rhs, " + ", rent_controls, " | segment_id^year_month")),
  data = rent,
  cluster = ~segment_id
)
rent_current_model <- feols(
  as.formula(paste0("log(rent_price) ~ ", rent_current_rhs, " + ", rent_controls, " | segment_id^year_month")),
  data = rent,
  cluster = ~segment_id
)

rent_estimate <- coef(rent_model)[["right"]]
rent_se <- se(rent_model)[["right"]]
rent_p <- pvalue(rent_model)[["right"]]
rent_stars <- case_when(
  rent_p <= 0.01 ~ "***",
  rent_p <= 0.05 ~ "**",
  rent_p <= 0.10 ~ "*",
  TRUE ~ ""
)
rent_removed <- rent_model$obs_selection$obsRemoved
rent_keep <- if (is.null(rent_removed)) {
  seq_len(nrow(rent))
} else {
  setdiff(seq_len(nrow(rent)), abs(as.integer(rent_removed)))
}
rent_plot_data <- rent[rent_keep, ]
rent_plot_data$y_adjusted <- as.numeric(resid(rent_model)) + rent_estimate * rent_plot_data$right
rent_bins <- rent_plot_data %>%
  mutate(bin_center = (floor(signed_dist_ft / (500 / 15)) + 0.5) * (500 / 15)) %>%
  group_by(bin_center) %>%
  summarise(
    mean_y = mean(y_adjusted),
    side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
    .groups = "drop"
  )
rent_plot <- ggplot(rent_bins, aes(bin_center, mean_y, color = side)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(size = 2.4) +
  scale_color_manual(
    values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"),
    name = NULL
  ) +
  labs(
    title = "Listed Rents by Side of Ward Boundary",
    subtitle = sprintf("Jump = %.3f%s (SE %.3f), N = %s", rent_estimate, rent_stars, rent_se, format(nobs(rent_model), big.mark = ",")),
    x = "Distance to ward boundary (feet; positive = more stringent side)",
    y = "Segment-by-month adjusted log rent"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(current_right = as.integer(signed_dist_m >= 0)) %>%
  select(-strictness_own, -strictness_neighbor, -sign, -signed_dist_m) %>%
  left_join(scores, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_own = score) %>%
  left_join(scores, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_dist = dist_m * sign / 0.3048,
    right = as.integer(signed_dist >= 0),
    ward_pair = as.character(ward_pair_id)
  ) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= 2006L,
    year <= 2022L,
    !is.na(ward_pair),
    !is.na(segment_id),
    segment_id != "",
    is.finite(signed_dist),
    abs(signed_dist) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor)
  )

sales_controls <- c(
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage",
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft"
)
sales <- sales %>%
  filter(if_all(all_of(sales_controls), ~ !is.na(.x)))

sales_model <- feols(
  as.formula(paste0("log(sale_price) ~ right + ", paste(sales_controls, collapse = " + "), " | segment_id^year_quarter")),
  data = sales,
  cluster = ~segment_id
)
sales_current_model <- feols(
  as.formula(paste0("log(sale_price) ~ current_right + ", paste(sales_controls, collapse = " + "), " | segment_id^year_quarter")),
  data = sales,
  cluster = ~segment_id
)

rent_hedonic_controls <- c("log_sqft", "beds_factor", "log_baths", "building_type_factor")
rent_amenity_controls <- c(
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft"
)
sales_hedonic_controls <- c(
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage"
)
sales_amenity_controls <- c(
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft"
)

control_sets <- list(
  full = list(
    rent = c(rent_hedonic_controls, rent_amenity_controls),
    sales = c(sales_hedonic_controls, sales_amenity_controls)
  ),
  no_amenities = list(rent = rent_hedonic_controls, sales = sales_hedonic_controls),
  no_hedonics = list(rent = rent_amenity_controls, sales = sales_amenity_controls),
  no_amenities_or_hedonics = list(rent = character(), sales = character())
)

control_rows <- list()
for (spec_name in names(control_sets)) {
  for (score_name in c("production_through2022", "drop_income_zero_day_workload_through2022")) {
    treatment <- if (score_name == "production_through2022") "current_right" else "right"
    rent_control_model <- feols(
      as.formula(paste0(
        "log(rent_price) ~ ",
        paste(c(treatment, control_sets[[spec_name]]$rent), collapse = " + "),
        " | segment_id^year_month"
      )),
      data = rent,
      cluster = ~segment_id
    )
    sales_control_model <- feols(
      as.formula(paste0(
        "log(sale_price) ~ ",
        paste(c(treatment, control_sets[[spec_name]]$sales), collapse = " + "),
        " | segment_id^year_quarter"
      )),
      data = sales,
      cluster = ~segment_id
    )

    control_rows[[length(control_rows) + 1L]] <- tibble(
      outcome = c("rent", "home_sales"),
      specification = spec_name,
      score = score_name,
      estimate = c(coef(rent_control_model)[[treatment]], coef(sales_control_model)[[treatment]]),
      std_error = c(se(rent_control_model)[[treatment]], se(sales_control_model)[[treatment]]),
      p_value = c(pvalue(rent_control_model)[[treatment]], pvalue(sales_control_model)[[treatment]]),
      observations = c(nobs(rent_control_model), nobs(sales_control_model))
    )
  }
}
bind_rows(control_rows) %>%
  write_csv("../output/drop_income_rent_sales_control_robustness.csv")

rent <- rent %>%
  mutate(involves_reilly = alderman_own == "Brendan Reilly" | alderman_neighbor == "Brendan Reilly")
sales <- sales %>%
  mutate(involves_reilly = alderman_own == "Brendan Reilly" | alderman_neighbor == "Brendan Reilly")

driver_rows <- list()
for (driver_sample in c("full", "excluding_reilly", "reilly_only")) {
  if (driver_sample == "excluding_reilly") {
    rent_driver <- rent %>% filter(!involves_reilly)
    sales_driver <- sales %>% filter(!involves_reilly)
  } else if (driver_sample == "reilly_only") {
    rent_driver <- rent %>% filter(involves_reilly)
    sales_driver <- sales %>% filter(involves_reilly)
  } else {
    rent_driver <- rent
    sales_driver <- sales
  }

  for (score_name in c("production_through2022", "drop_income_zero_day_workload_through2022")) {
    treatment <- if (score_name == "production_through2022") "current_right" else "right"
    rent_driver_model <- feols(
      as.formula(paste0(
        "log(rent_price) ~ ",
        paste(c(treatment, control_sets$full$rent), collapse = " + "),
        " | segment_id^year_month"
      )),
      data = rent_driver,
      cluster = ~segment_id
    )
    sales_driver_model <- feols(
      as.formula(paste0(
        "log(sale_price) ~ ",
        paste(c(treatment, control_sets$full$sales), collapse = " + "),
        " | segment_id^year_quarter"
      )),
      data = sales_driver,
      cluster = ~segment_id
    )
    driver_rows[[length(driver_rows) + 1L]] <- tibble(
      outcome = c("rent", "home_sales"),
      sample = driver_sample,
      score = score_name,
      estimate = c(coef(rent_driver_model)[[treatment]], coef(sales_driver_model)[[treatment]]),
      std_error = c(se(rent_driver_model)[[treatment]], se(sales_driver_model)[[treatment]]),
      p_value = c(pvalue(rent_driver_model)[[treatment]], pvalue(sales_driver_model)[[treatment]]),
      observations = c(nobs(rent_driver_model), nobs(sales_driver_model))
    )
  }
}
bind_rows(driver_rows) %>%
  write_csv("../output/drop_income_rent_sales_reilly_models.csv")

flipped_pairs <- bind_rows(
  rent %>%
    transmute(
      outcome = "rent",
      alderman_1 = pmin(alderman_own, alderman_neighbor),
      alderman_2 = pmax(alderman_own, alderman_neighbor),
      flipped = right != current_right
    ),
  sales %>%
    transmute(
      outcome = "home_sales",
      alderman_1 = pmin(alderman_own, alderman_neighbor),
      alderman_2 = pmax(alderman_own, alderman_neighbor),
      flipped = right != current_right
    )
) %>%
  group_by(outcome, alderman_1, alderman_2) %>%
  summarise(
    observations = n(),
    flipped_observations = sum(flipped),
    flipped_share = mean(flipped),
    .groups = "drop"
  ) %>%
  filter(flipped_observations > 0) %>%
  arrange(outcome, desc(flipped_observations), alderman_1, alderman_2)
write_csv(flipped_pairs, "../output/drop_income_rent_sales_flipped_pairs.csv")

sales_pair_rows <- list()
top_sales_pairs <- flipped_pairs %>%
  filter(outcome == "home_sales") %>%
  slice_head(n = 8)
for (pair_i in seq_len(nrow(top_sales_pairs))) {
  sales_without_pair <- sales %>%
    filter(!(
      pmin(alderman_own, alderman_neighbor) == top_sales_pairs$alderman_1[pair_i] &
        pmax(alderman_own, alderman_neighbor) == top_sales_pairs$alderman_2[pair_i]
    ))
  for (score_name in c("production_through2022", "drop_income_zero_day_workload_through2022")) {
    treatment <- if (score_name == "production_through2022") "current_right" else "right"
    pair_model <- feols(
      as.formula(paste0(
        "log(sale_price) ~ ",
        paste(c(treatment, control_sets$full$sales), collapse = " + "),
        " | segment_id^year_quarter"
      )),
      data = sales_without_pair,
      cluster = ~segment_id
    )
    sales_pair_rows[[length(sales_pair_rows) + 1L]] <- tibble(
      excluded_alderman_1 = top_sales_pairs$alderman_1[pair_i],
      excluded_alderman_2 = top_sales_pairs$alderman_2[pair_i],
      excluded_observations = top_sales_pairs$observations[pair_i],
      score = score_name,
      estimate = coef(pair_model)[[treatment]],
      std_error = se(pair_model)[[treatment]],
      p_value = pvalue(pair_model)[[treatment]],
      observations = nobs(pair_model)
    )
  }
}
bind_rows(sales_pair_rows) %>%
  write_csv("../output/drop_income_sales_pair_leave_out.csv")

sales_estimate <- coef(sales_model)[["right"]]
sales_se <- se(sales_model)[["right"]]
sales_p <- pvalue(sales_model)[["right"]]
sales_stars <- case_when(
  sales_p <= 0.01 ~ "***",
  sales_p <= 0.05 ~ "**",
  sales_p <= 0.10 ~ "*",
  TRUE ~ ""
)
sales_removed <- sales_model$obs_selection$obsRemoved
sales_keep <- if (is.null(sales_removed)) {
  seq_len(nrow(sales))
} else {
  setdiff(seq_len(nrow(sales)), abs(as.integer(sales_removed)))
}
sales_plot_data <- sales[sales_keep, ]
sales_plot_data$y_adjusted <- as.numeric(resid(sales_model)) + sales_estimate * sales_plot_data$right
sales_bins <- sales_plot_data %>%
  mutate(bin_center = (floor(signed_dist / 50) + 0.5) * 50) %>%
  group_by(bin_center) %>%
  summarise(
    mean_y = mean(y_adjusted),
    side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
    .groups = "drop"
  )
sales_plot <- ggplot(sales_bins, aes(bin_center, mean_y, color = side)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"),
    name = NULL
  ) +
  labs(
    title = "Home Sale Prices by Side of Ward Boundary",
    subtitle = sprintf("Jump = %.3f%s (SE %.3f), N = %s", sales_estimate, sales_stars, sales_se, format(nobs(sales_model), big.mark = ",")),
    x = "Distance to ward boundary (feet; positive = more stringent side)",
    y = "Segment-by-quarter adjusted log sale price"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("../output/drop_income_rent_rd.png", rent_plot, width = 8.6, height = 6, dpi = 220, bg = "white")
ggsave("../output/drop_income_sales_rd.png", sales_plot, width = 8.6, height = 6, dpi = 220, bg = "white")
ggsave(
  "../output/drop_income_rent_sales_rd.png",
  rent_plot / sales_plot,
  width = 8.6,
  height = 12,
  dpi = 220,
  bg = "white"
)

bind_rows(
  tibble(
    outcome = "rent",
    score = c("production_through2022", "drop_income_zero_day_workload_through2022"),
    estimate = c(coef(rent_current_model)[["current_right"]], rent_estimate),
    std_error = c(se(rent_current_model)[["current_right"]], rent_se),
    p_value = c(pvalue(rent_current_model)[["current_right"]], rent_p),
    observations = c(nobs(rent_current_model), nobs(rent_model)),
    segments = c(n_distinct(rent$segment_id), n_distinct(rent$segment_id)),
    side_flipped_observations = c(0L, sum(rent$right != rent$current_right))
  ),
  tibble(
    outcome = "home_sales",
    score = c("production_through2022", "drop_income_zero_day_workload_through2022"),
    estimate = c(coef(sales_current_model)[["current_right"]], sales_estimate),
    std_error = c(se(sales_current_model)[["current_right"]], sales_se),
    p_value = c(pvalue(sales_current_model)[["current_right"]], sales_p),
    observations = c(nobs(sales_current_model), nobs(sales_model)),
    segments = c(n_distinct(sales$segment_id), n_distinct(sales$segment_id)),
    side_flipped_observations = c(0L, sum(sales$right != sales$current_right))
  )
) %>%
  write_csv("../output/drop_income_rent_sales_results.csv")
