source("../../../setup_environment/code/packages.R")
source("../../../_lib/amenity_distance_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_border_pair_fe_audit/code")

build_sales_sample <- function(sales, bw_ft, fe_geo, cluster_level, table_mode, estimand) {
  if (!"signed_dist" %in% names(sales) && "signed_dist_m" %in% names(sales)) {
    sales <- sales %>% mutate(signed_dist = signed_dist_m / 0.3048)
  }
  if (!"signed_dist" %in% names(sales)) {
    stop("Sales input must include signed_dist in feet or signed_dist_m in meters.", call. = FALSE)
  }

  sales <- sales %>%
    mutate(
      ward_pair = as.character(ward_pair_id),
      year_factor = as.character(year),
      signed_dist = as.numeric(signed_dist),
      right = as.integer(signed_dist >= 0)
    ) %>%
    filter(
      !is.na(sale_price), sale_price > 0,
      year >= 2006,
      year <= 2022,
      !is.na(ward_pair), is.finite(signed_dist),
      abs(signed_dist) <= bw_ft,
      !is.na(strictness_own),
      !is.na(strictness_neighbor)
    )

  if (fe_geo == "segment" || cluster_level == "segment") {
    sales <- sales %>% filter(!is.na(segment_id), segment_id != "")
  }

  strictness_sd <- sd(sales$strictness_own, na.rm = TRUE)
  stopifnot(is.finite(strictness_sd), strictness_sd > 0)
  sales <- sales %>% mutate(strictness_std = strictness_own / strictness_sd)

  sales_hed <- sales %>%
    filter(!is.na(log_sqft), !is.na(log_land_sqft), !is.na(log_building_age),
           !is.na(log_bedrooms), !is.na(log_baths), !is.na(has_garage))

  if (table_mode == "amenity") {
    sales_amenity <- sales_hed %>%
      filter(
        !is.na(nearest_school_dist_ft),
        !is.na(nearest_park_dist_ft),
        !is.na(nearest_major_road_dist_ft),
        !is.na(lake_michigan_dist_ft)
      )
  } else {
    sales_amenity <- NULL
  }

  list(
    sales = sales,
    sales_hed = sales_hed,
    sales_amenity = sales_amenity,
    treatment_var = ifelse(estimand == "right", "right", "strictness_std")
  )
}

write_sales_spec_sidecars <- function(sales_raw, bw_ft, fe_time, fe_geo, cluster_level, table_mode, estimand) {
  sample <- build_sales_sample(sales_raw, bw_ft, fe_geo, cluster_level, table_mode, estimand)
  sales <- sample$sales
  sales_hed <- sample$sales_hed
  sales_amenity <- sample$sales_amenity
  treatment_var <- sample$treatment_var

  estimand_prefix <- ifelse(estimand == "right", "_right", "")
  output_stem <- sprintf(
    "sales%s_bw%d_%s_%s_clust_%s",
    estimand_prefix,
    bw_ft,
    fe_time,
    table_mode,
    cluster_level
  )

  sales %>%
    group_by(year) %>%
    summarise(
      n = n(),
      median_price = median(sale_price, na.rm = TRUE),
      mean_price = mean(sale_price, na.rm = TRUE),
      coverage_sqft = mean(!is.na(log_sqft)),
      coverage_bedrooms = mean(!is.na(log_bedrooms)),
      coverage_baths = mean(!is.na(log_baths)),
      .groups = "drop"
    ) %>%
    write_csv(file.path("../output", paste0("year_diagnostics_", output_stem, ".csv")))

  fe_var <- switch(fe_time, year = "year_factor", year_quarter = "year_quarter", year_month = "year_month")
  fe_term <- ifelse(fe_geo == "segment", paste0("segment_id^", fe_var), paste0("ward_pair^", fe_var))
  cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair

  m1 <- feols(
    as.formula(paste0("log(sale_price) ~ ", treatment_var, " | ", fe_term)),
    data = sales,
    cluster = cluster_formula
  )

  hedonic_rhs <- paste(
    treatment_var,
    "log_sqft",
    "log_land_sqft",
    "log_building_age",
    "log_bedrooms",
    "log_baths",
    "has_garage",
    sep = " + "
  )
  m2 <- feols(
    as.formula(paste0("log(sale_price) ~ ", hedonic_rhs, " | ", fe_term)),
    data = sales_hed,
    cluster = cluster_formula
  )

  coef_tbl <- bind_rows(
    tibble(
      specification = "no_hedonics",
      estimate = coef(m1)[[treatment_var]],
      std_error = se(m1)[[treatment_var]],
      p_value = pvalue(m1)[[treatment_var]],
      n_obs = m1$nobs,
      dep_var_mean = mean(sales$sale_price, na.rm = TRUE),
      n_segments = n_distinct(sales$segment_id),
      ward_pairs = n_distinct(sales$ward_pair),
      bandwidth_ft = bw_ft,
      fe_time = fe_time,
      fe_geo = fe_geo,
      cluster_level = cluster_level,
      estimand = estimand,
      treatment_var = treatment_var,
      use_zone_group_fe = FALSE,
      use_amenity_controls = FALSE
    ),
    tibble(
      specification = "with_hedonics",
      estimate = coef(m2)[[treatment_var]],
      std_error = se(m2)[[treatment_var]],
      p_value = pvalue(m2)[[treatment_var]],
      n_obs = m2$nobs,
      dep_var_mean = mean(sales_hed$sale_price, na.rm = TRUE),
      n_segments = n_distinct(sales_hed$segment_id),
      ward_pairs = n_distinct(sales_hed$ward_pair),
      bandwidth_ft = bw_ft,
      fe_time = fe_time,
      fe_geo = fe_geo,
      cluster_level = cluster_level,
      estimand = estimand,
      treatment_var = treatment_var,
      use_zone_group_fe = FALSE,
      use_amenity_controls = FALSE
    )
  )

  if (table_mode == "amenity") {
    amenity_rhs <- paste0(
      hedonic_rhs,
      " + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + lake_michigan_dist_ft"
    )
    m3 <- feols(
      as.formula(paste0("log(sale_price) ~ ", amenity_rhs, " | ", fe_term)),
      data = sales_amenity,
      cluster = cluster_formula
    )

    coef_tbl <- bind_rows(
      coef_tbl,
      tibble(
        specification = "with_hedonics_and_amenities",
        estimate = coef(m3)[[treatment_var]],
        std_error = se(m3)[[treatment_var]],
        p_value = pvalue(m3)[[treatment_var]],
        n_obs = m3$nobs,
        dep_var_mean = mean(sales_amenity$sale_price, na.rm = TRUE),
        n_segments = n_distinct(sales_amenity$segment_id),
        ward_pairs = n_distinct(sales_amenity$ward_pair),
        bandwidth_ft = bw_ft,
        fe_time = fe_time,
        fe_geo = fe_geo,
        cluster_level = cluster_level,
        estimand = estimand,
        treatment_var = treatment_var,
        use_zone_group_fe = FALSE,
        use_amenity_controls = TRUE
      )
    )
  }

  write_csv(coef_tbl, file.path("../output", paste0("fe_table_", output_stem, ".csv")))
}

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble()

coords <- sales %>%
  transmute(longitude, latitude) %>%
  filter(is.finite(longitude), is.finite(latitude)) %>%
  distinct()

amenity_distance_diagnostics(sales, coords, "sales") %>%
  rename(n_sales = n_rows) %>%
  write_csv("../output/sales_amenity_distance_diagnostics.csv")

specs <- tribble(
  ~bw_ft, ~fe_time, ~fe_geo, ~cluster_level, ~table_mode, ~estimand,
  500L, "year_quarter", "segment", "ward_pair", "amenity", "strictness_score",
  500L, "year_quarter", "segment", "ward_pair", "amenity", "right",
  500L, "year_quarter", "segment", "segment", "amenity", "right"
)

for (i in seq_len(nrow(specs))) {
  write_sales_spec_sidecars(
    sales,
    specs$bw_ft[i],
    specs$fe_time[i],
    specs$fe_geo[i],
    specs$cluster_level[i],
    specs$table_mode[i],
    specs$estimand[i]
  )
}
