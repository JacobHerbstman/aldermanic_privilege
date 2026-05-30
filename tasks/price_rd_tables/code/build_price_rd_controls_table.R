# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/price_rd_tables/code")

source("../../setup_environment/code/packages.R")

model_row <- function(model, data, term, specification, dep_var) {
  tibble(
    specification = specification,
    estimate = coef(model)[[term]],
    std_error = se(model)[[term]],
    p_value = pvalue(model)[[term]],
    n_obs = model$nobs,
    dep_var_mean = mean(data[[dep_var]], na.rm = TRUE),
    n_segments = n_distinct(data$segment_id),
    n_ward_pairs = n_distinct(data$ward_pair)
  )
}

panel_lines <- function(data, panel_title) {
  if (nrow(data) != 3L) {
    stop(sprintf("Expected three specifications for %s.", panel_title), call. = FALSE)
  }

  data <- data %>%
    mutate(
      estimate_text = paste0(
        formatC(100 * estimate, format = "f", digits = 2, big.mark = ","),
        case_when(
          is.na(p_value) ~ "",
          p_value < 0.01 ~ "$^{***}$",
          p_value < 0.05 ~ "$^{**}$",
          p_value < 0.10 ~ "$^{*}$",
          TRUE ~ ""
        )
      ),
      se_text = paste0("(", formatC(100 * std_error, format = "f", digits = 2, big.mark = ","), ")"),
      n_obs_text = formatC(round(n_obs), format = "d", big.mark = ","),
      dep_var_mean_text = formatC(round(dep_var_mean), format = "d", big.mark = ","),
      n_segments_text = formatC(round(n_segments), format = "d", big.mark = ","),
      n_ward_pairs_text = formatC(round(n_ward_pairs), format = "d", big.mark = ",")
    )

  c(
    sprintf("\\multicolumn{4}{l}{\\textit{%s}} \\\\", panel_title),
    sprintf(
      "Stringency Index & %s & %s & %s \\\\",
      data$estimate_text[1],
      data$estimate_text[2],
      data$estimate_text[3]
    ),
    sprintf(
      " & %s & %s & %s \\\\",
      data$se_text[1],
      data$se_text[2],
      data$se_text[3]
    ),
    "\\addlinespace",
    sprintf(
      "N & %s & %s & %s \\\\",
      data$n_obs_text[1],
      data$n_obs_text[2],
      data$n_obs_text[3]
    ),
    sprintf(
      "Dep. Var. Mean & %s & %s & %s \\\\",
      data$dep_var_mean_text[1],
      data$dep_var_mean_text[2],
      data$dep_var_mean_text[3]
    ),
    sprintf(
      "Segments & %s & %s & %s \\\\",
      data$n_segments_text[1],
      data$n_segments_text[2],
      data$n_segments_text[3]
    ),
    sprintf(
      "Ward Pairs & %s & %s & %s \\\\",
      data$n_ward_pairs_text[1],
      data$n_ward_pairs_text[2],
      data$n_ward_pairs_text[3]
    ),
    "Hedonic Controls & & $\\checkmark$ & $\\checkmark$ \\\\",
    "Amenity Controls & & & $\\checkmark$ \\\\",
    "Segment $\\times$ Time FE & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
    "Cluster Level & Segment & Segment & Segment \\\\"
  )
}

rent <- read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble() %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(is.finite(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
  ) %>%
  filter(
    is.finite(rent_price),
    rent_price > 0,
    is.finite(strictness_own),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    is.finite(log_sqft),
    is.finite(log_beds),
    is.finite(log_baths),
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

rent <- rent %>%
  mutate(strictness_std = strictness_own / sd(strictness_own, na.rm = TRUE))

rent_no_controls <- feols(
  log(rent_price) ~ strictness_std | segment_id^year_month,
  data = rent,
  cluster = ~segment_id
)
rent_hedonics <- feols(
  log(rent_price) ~ strictness_std + log_sqft + log_beds + log_baths + building_type_factor | segment_id^year_month,
  data = rent,
  cluster = ~segment_id
)
rent_amenities <- feols(
  log(rent_price) ~ strictness_std + log_sqft + log_beds + log_baths + building_type_factor +
    nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft +
    nearest_cta_stop_dist_kft + lake_michigan_dist_kft | segment_id^year_month,
  data = rent,
  cluster = ~segment_id
)

rent_panel <- bind_rows(
  model_row(rent_no_controls, rent, "strictness_std", "No controls", "rent_price"),
  model_row(rent_hedonics, rent, "strictness_std", "Hedonics", "rent_price"),
  model_row(rent_amenities, rent, "strictness_std", "Hedonics + amenities", "rent_price")
)

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    signed_dist = signed_dist_m / 0.3048
  ) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= 2006,
    year <= 2022,
    !is.na(ward_pair),
    is.finite(signed_dist),
    abs(signed_dist) <= 500,
    is.finite(strictness_own),
    !is.na(segment_id),
    segment_id != ""
  )

sales <- sales %>%
  mutate(strictness_std = strictness_own / sd(strictness_own, na.rm = TRUE))

sales_hedonic <- sales %>%
  filter(
    !is.na(log_sqft),
    !is.na(log_land_sqft),
    !is.na(log_building_age),
    !is.na(log_bedrooms),
    !is.na(log_baths),
    !is.na(has_garage)
  )

sales_amenity <- sales_hedonic %>%
  filter(
    !is.na(nearest_school_dist_ft),
    !is.na(nearest_park_dist_ft),
    !is.na(nearest_major_road_dist_ft),
    !is.na(lake_michigan_dist_ft)
  )

sales_no_controls <- feols(
  log(sale_price) ~ strictness_std | segment_id^year_quarter,
  data = sales,
  cluster = ~segment_id
)
sales_hedonics <- feols(
  log(sale_price) ~ strictness_std + log_sqft + log_land_sqft + log_building_age +
    log_bedrooms + log_baths + has_garage | segment_id^year_quarter,
  data = sales_hedonic,
  cluster = ~segment_id
)
sales_amenities <- feols(
  log(sale_price) ~ strictness_std + log_sqft + log_land_sqft + log_building_age +
    log_bedrooms + log_baths + has_garage + nearest_school_dist_ft +
    nearest_park_dist_ft + nearest_major_road_dist_ft + lake_michigan_dist_ft | segment_id^year_quarter,
  data = sales_amenity,
  cluster = ~segment_id
)

sales_panel <- bind_rows(
  model_row(sales_no_controls, sales, "strictness_std", "No controls", "sale_price"),
  model_row(sales_hedonics, sales_hedonic, "strictness_std", "Hedonics", "sale_price"),
  model_row(sales_amenities, sales_amenity, "strictness_std", "Hedonics + amenities", "sale_price")
)

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & (1) & (2) & (3) \\\\",
  "\\midrule",
  panel_lines(rent_panel, "Panel A. Listed rents"),
  "\\midrule",
  panel_lines(sales_panel, "Panel B. Home sales"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\vspace{0.5em}\\parbox{0.92\\linewidth}{\\footnotesize Notes: Entries are percent log-price effects of a one-standard-deviation increase in the aldermanic stringency index, with standard errors in parentheses. Panel A uses listed-rent floorplan-month observations from 2014--2022. Panel B uses arm's-length residential sales from 2006--2022. Both panels restrict observations to within 500ft of ward boundaries. Dependent-variable means are in 2022 dollars. Segment-by-time fixed effects are segment-by-month for listed rents and segment-by-quarter for home sales. Hedonic controls are log square feet, log bedrooms or bathrooms, building type for rents, and building area, land area, building age, bedrooms, bathrooms, and garage for sales. Amenity controls are distances to the nearest school, CPD park-boundary polygon, major street, CTA stop for rents, and Lake Michigan. Standard errors are clustered by boundary segment. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}",
  "\\par\\endgroup"
)

writeLines(table_lines, "../output/price_rd_controls_panels_bw500.tex")
