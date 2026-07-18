# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_data_cleaning_audit/code")

source("../../../setup_environment/code/packages.R")

raw <- fread(
  "../input/parcel_sales_city.csv",
  colClasses = list(character = c("pin", "sale_date", "sale_price", "row_id"))
) %>%
  as_tibble() %>%
  mutate(
    pin = gsub("[^0-9]", "", trimws(pin)),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin),
    year = suppressWarnings(as.integer(year)),
    sale_date = coalesce(
      as.Date(as.character(sale_date), format = "%B %d, %Y"),
      as.Date(substr(as.character(sale_date), 1, 10), format = "%Y-%m-%d")
    ),
    sale_price_nominal = suppressWarnings(as.numeric(gsub("[$,]", "", sale_price))),
    buyer_valid = !is.na(sale_buyer_name) &
      !sale_buyer_name %in% c("", "-", "UNKNOWN", ".."),
    seller_normalized = gsub("[^A-Z0-9]", "", toupper(trimws(sale_seller_name))),
    buyer_normalized = gsub("[^A-Z0-9]", "", toupper(trimws(sale_buyer_name))),
    normalized_names_equal = seller_normalized != "" & seller_normalized == buyer_normalized,
    ccao_repeat_sale_flag = tolower(as.character(sale_filter_same_sale_within_365)) %in%
      c("true", "t", "1", "yes")
  ) %>%
  filter(
    class %in% 202:211,
    is.finite(sale_price_nominal),
    sale_price_nominal > 10000,
    !is.na(year),
    year >= 1999,
    year <= 2025,
    sale_deed_type %in% c("Warranty", "Trustee"),
    !is.na(sale_type),
    sale_type != "LAND",
    !sale_seller_name %in% c("", "-", "UNKNOWN", ".."),
    !is.na(sale_seller_name),
    !is.na(sale_buyer_name),
    sale_seller_name != sale_buyer_name,
    num_parcels_sale == 1
  )

if (any(nchar(raw$pin) != 14L)) {
  stop("Eligible raw sales contain an invalid full PIN.", call. = FALSE)
}
if (anyDuplicated(raw$row_id) > 0) {
  stop("Eligible raw sales contain duplicate row_id values.", call. = FALSE)
}

raw_key_counts <- raw %>%
  count(pin, sale_date, sale_price_nominal, name = "n")
if (any(raw_key_counts$n > 1L)) {
  stop("Eligible raw sales are not unique by PIN, sale date, and nominal price.", call. = FALSE)
}

sales_pre <- read_csv(
  "../input/sales_pre_scores_with_segments.csv",
  col_types = cols(pin = col_character(), .default = col_guess()),
  show_col_types = FALSE
) %>%
  mutate(
    pin = gsub("[^0-9]", "", trimws(pin)),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin),
    sale_date = as.Date(sale_date)
  )

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    audit_id = row_number(),
    pin = gsub("[^0-9]", "", trimws(as.character(pin))),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin),
    sale_date = as.Date(sale_date),
    signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
    right_production = as.integer(signed_dist_ft >= 0)
  ) %>%
  left_join(
    raw %>%
      select(
        pin, sale_date, sale_price_nominal, buyer_valid,
        normalized_names_equal, ccao_repeat_sale_flag, sale_document_num
      ),
    by = c("pin", "sale_date", "sale_price_nominal"),
    relationship = "many-to-one"
  )

if (any(is.na(sales$buyer_valid))) {
  stop("A production sales row could not be linked back to its raw transaction.", call. = FALSE)
}

hedonic_controls <- c(
  "log_sqft", "log_land_sqft", "log_building_age",
  "log_bedrooms", "log_baths", "has_garage"
)
amenity_controls <- c(
  "nearest_school_dist_ft", "nearest_park_dist_ft", "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
)

model_base <- sales %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= 2006,
    year <= 2022,
    !is.na(ward_pair_id),
    !is.na(segment_id),
    segment_id != "",
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    if_all(all_of(c(hedonic_controls, amenity_controls)), ~ !is.na(.x))
  )

scores <- read_csv(
  "../input/aldermen_uncertainty_scores_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, exact_score = uncertainty_index)
if (anyDuplicated(scores$alderman) > 0) {
  stop("Score input must be unique by alderman.", call. = FALSE)
}

alderman_environment <- new.env(parent = globalenv())
alderman_environment$panel_end_month <- "2026-04"
alderman_environment$write_csv <- function(...) invisible(NULL)
source(
  "../../../create_alderman_data/code/create_alderman_panel.R",
  local = alderman_environment,
  chdir = TRUE
)

alderman_daily <- alderman_environment$alderman_data %>%
  mutate(
    term_start_date = start_date,
    start_date = pmax(start_date, as.Date("2006-01-01")),
    end_date = pmin(end_date, as.Date("2022-12-31"))
  ) %>%
  filter(start_date <= end_date) %>%
  rowwise() %>%
  mutate(sale_date = list(seq(start_date, end_date, by = "day"))) %>%
  ungroup() %>%
  select(ward, alderman, term_start_date, sale_date) %>%
  tidyr::unnest(sale_date)
alderman_overlap_dates <- alderman_daily %>%
  count(ward, sale_date, name = "n") %>%
  filter(n > 1L)
alderman_daily <- alderman_daily %>%
  arrange(ward, sale_date, desc(term_start_date)) %>%
  distinct(ward, sale_date, .keep_all = TRUE) %>%
  select(-term_start_date)

model_base <- model_base %>%
  left_join(
    alderman_daily %>% rename(ward = ward, alderman_own_exact = alderman),
    by = c("ward", "sale_date"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    alderman_daily %>% rename(neighbor_ward = ward, alderman_neighbor_exact = alderman),
    by = c("neighbor_ward", "sale_date"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_own_exact = alderman, strictness_own_exact = exact_score),
    by = "alderman_own_exact",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_neighbor_exact = alderman, strictness_neighbor_exact = exact_score),
    by = "alderman_neighbor_exact",
    relationship = "many-to-one"
  ) %>%
  mutate(
    sign_exact = case_when(
      strictness_own_exact > strictness_neighbor_exact ~ 1,
      strictness_own_exact < strictness_neighbor_exact ~ -1,
      TRUE ~ NA_real_
    ),
    right_exact = as.integer(dist_m * sign_exact >= 0)
  )

production_lower <- min(sales_pre$sale_price, na.rm = TRUE)
production_upper <- max(sales_pre$sale_price, na.rm = TRUE)
analysis_prices <- sales_pre %>%
  filter(year >= 2006, year <= 2022) %>%
  pull(sale_price_real_2022_raw)
analysis_cutoffs <- quantile(analysis_prices, c(0.01, 0.99), na.rm = TRUE)

model_base <- model_base %>%
  mutate(
    sale_price_winsor_2006_2022 = pmin(
      pmax(sale_price_real_2022_raw, analysis_cutoffs[[1]]),
      analysis_cutoffs[[2]]
    ),
    nearest_cta_stop_dist_ft_exact = nearest_cta_stop_dist_ft
  )

cta_stops <- st_read("../input/cta_stops.gpkg", quiet = TRUE) %>%
  st_transform(3435) %>%
  mutate(
    active_from_date = as.Date(active_from_date),
    active_to_date = as.Date(active_to_date)
  )
cta_transition_months <- cta_stops %>%
  st_drop_geometry() %>%
  filter(
    lubridate::day(active_from_date) != 1L |
      (!is.na(active_to_date) &
        active_to_date != lubridate::ceiling_date(active_to_date, "month") - lubridate::days(1))
  ) %>%
  transmute(year_month = format(coalesce(active_from_date, active_to_date), "%Y-%m")) %>%
  distinct() %>%
  pull(year_month)

cta_check <- model_base %>%
  filter(year_month %in% cta_transition_months) %>%
  select(audit_id, sale_date, longitude, latitude)
if (nrow(cta_check) > 0) {
  cta_points <- st_as_sf(
    cta_check,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(3435)

  cta_exact_rows <- list()
  for (date_i in sort(unique(cta_points$sale_date))) {
    date_points <- cta_points[cta_points$sale_date == date_i, ]
    active_cta <- cta_stops %>%
      filter(
        active_from_date <= date_i,
        is.na(active_to_date) | active_to_date >= date_i
      )
    nearest_idx <- st_nearest_feature(date_points, active_cta)
    cta_exact_rows[[length(cta_exact_rows) + 1L]] <- tibble(
      audit_id = date_points$audit_id,
      nearest_cta_stop_dist_ft_exact = as.numeric(st_distance(
        date_points,
        active_cta[nearest_idx, ],
        by_element = TRUE
      ))
    )
  }
  cta_exact <- bind_rows(cta_exact_rows)
  if (anyDuplicated(cta_exact$audit_id) > 0) {
    stop("Exact-date CTA audit is not unique by sale.", call. = FALSE)
  }
  model_base <- model_base %>%
    select(-nearest_cta_stop_dist_ft_exact) %>%
    left_join(cta_exact, by = "audit_id", relationship = "one-to-one") %>%
    mutate(nearest_cta_stop_dist_ft_exact = coalesce(
      nearest_cta_stop_dist_ft_exact,
      nearest_cta_stop_dist_ft
    ))
}

estimate_sales <- function(data, price_variable, right_variable, cta_variable) {
  model_data <- data %>%
    transmute(
      sale_price_model = .data[[price_variable]],
      right_model = .data[[right_variable]],
      segment_id,
      year_quarter,
      across(all_of(hedonic_controls)),
      nearest_school_dist_ft,
      nearest_park_dist_ft,
      nearest_major_road_dist_ft,
      nearest_cta_stop_dist_ft = .data[[cta_variable]],
      lake_michigan_dist_ft
    ) %>%
    filter(
      is.finite(sale_price_model),
      sale_price_model > 0,
      !is.na(right_model),
      if_all(
        all_of(c(hedonic_controls, amenity_controls)),
        ~ is.finite(.x)
      )
    )

  model <- feols(
    log(sale_price_model) ~ right_model + log_sqft + log_land_sqft +
      log_building_age + log_bedrooms + log_baths + has_garage +
      nearest_school_dist_ft + nearest_park_dist_ft +
      nearest_major_road_dist_ft + nearest_cta_stop_dist_ft +
      lake_michigan_dist_ft | segment_id^year_quarter,
    data = model_data,
    cluster = ~segment_id
  )
  model_row <- coeftable(model)["right_model", , drop = FALSE]
  tibble(
    estimate = unname(model_row[1, "Estimate"]),
    std_error = unname(model_row[1, "Std. Error"]),
    p_value = unname(model_row[1, "Pr(>|t|)"]),
    n = nobs(model)
  )
}

model_specs <- list(
  "Production" = list(
    data = model_base,
    price = "sale_price",
    right = "right_production",
    cta = "nearest_cta_stop_dist_ft"
  ),
  "Require valid buyer name" = list(
    data = filter(model_base, buyer_valid),
    price = "sale_price",
    right = "right_production",
    cta = "nearest_cta_stop_dist_ft"
  ),
  "Exclude normalized seller-buyer name matches" = list(
    data = filter(model_base, !normalized_names_equal),
    price = "sale_price",
    right = "right_production",
    cta = "nearest_cta_stop_dist_ft"
  ),
  "Exclude CCAO repeated-sale flags" = list(
    data = filter(model_base, !ccao_repeat_sale_flag),
    price = "sale_price",
    right = "right_production",
    cta = "nearest_cta_stop_dist_ft"
  ),
  "Winsorize within 2006-2022" = list(
    data = model_base,
    price = "sale_price_winsor_2006_2022",
    right = "right_production",
    cta = "nearest_cta_stop_dist_ft"
  ),
  "No price winsorization" = list(
    data = model_base,
    price = "sale_price_real_2022_raw",
    right = "right_production",
    cta = "nearest_cta_stop_dist_ft"
  ),
  "Exact-date aldermen" = list(
    data = model_base,
    price = "sale_price",
    right = "right_exact",
    cta = "nearest_cta_stop_dist_ft"
  ),
  "Exact-date CTA availability" = list(
    data = model_base,
    price = "sale_price",
    right = "right_production",
    cta = "nearest_cta_stop_dist_ft_exact"
  ),
  "Exact aldermen, valid buyer, 2006-2022 winsorization" = list(
    data = filter(model_base, buyer_valid),
    price = "sale_price_winsor_2006_2022",
    right = "right_exact",
    cta = "nearest_cta_stop_dist_ft_exact"
  )
)

model_results <- bind_rows(lapply(names(model_specs), function(spec_name) {
  spec <- model_specs[[spec_name]]
  estimate_sales(spec$data, spec$price, spec$right, spec$cta) %>%
    mutate(record_type = "model", item = spec_name, value = NA_real_, details = NA_character_)
}))

same_document_counts <- raw %>%
  filter(!is.na(sale_document_num), sale_document_num != "") %>%
  count(sale_document_num, name = "n")

metrics <- tribble(
  ~item, ~value, ~details,
  "Eligible raw sales", nrow(raw), "Production restrictions before spatial assignment",
  "Eligible sales with invalid buyer codes", sum(!raw$buyer_valid), "Appendix says buyer names are valid",
  "Eligible normalized seller-buyer matches", sum(raw$normalized_names_equal), "Additional matches after case and punctuation normalization",
  "Eligible CCAO repeated-sale flags", sum(raw$ccao_repeat_sale_flag), "CCAO same-sale-within-365 indicator",
  "Eligible repeated document numbers", sum(same_document_counts$n > 1L), "Distinct document numbers appearing more than once",
  "Final 500ft complete-case sample", nrow(model_base), "Before fixest collinearity or singleton removal",
  "Final sample with missing garage size coded as no garage", sum(is.na(model_base$garage_size)), "has_garage equals zero when garage_size is missing",
  "Final sample exact own-alderman mismatch", sum(model_base$alderman_own != model_base$alderman_own_exact, na.rm = TRUE), "Monthly versus exact transaction-date assignment",
  "Final sample exact neighboring-alderman mismatch", sum(model_base$alderman_neighbor != model_base$alderman_neighbor_exact, na.rm = TRUE), "Monthly versus exact transaction-date assignment",
  "Final sample missing exact alderman assignment", sum(is.na(model_base$right_exact)), "Vacancies or missing exact-date score",
  "Overlapping alderman ward-dates", nrow(alderman_overlap_dates), "Latest term start retained for audit assignment",
  "Final sample CTA exact-date distance changes", sum(abs(model_base$nearest_cta_stop_dist_ft_exact - model_base$nearest_cta_stop_dist_ft) > 1e-6), "CTA openings or closures within the sale month",
  "Production lower winsor cutoff", production_lower, "2022 dollars",
  "Production upper winsor cutoff", production_upper, "2022 dollars",
  "2006-2022 lower winsor cutoff", analysis_cutoffs[[1]], "2022 dollars",
  "2006-2022 upper winsor cutoff", analysis_cutoffs[[2]], "2022 dollars"
) %>%
  mutate(
    record_type = "metric",
    estimate = NA_real_,
    std_error = NA_real_,
    p_value = NA_real_,
    n = NA_integer_
  )

write_csv(
  bind_rows(model_results, metrics) %>%
    select(record_type, item, estimate, std_error, p_value, n, value, details),
  "../output/sales_data_cleaning_audit.csv"
)
