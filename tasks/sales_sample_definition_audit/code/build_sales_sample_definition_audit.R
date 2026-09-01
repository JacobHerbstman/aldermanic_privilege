# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_sample_definition_audit/code")

source("../../setup_environment/code/packages.R")

sample_order <- c(
  "Legacy names-required",
  "Warranty/trustee, names optional",
  "Production current",
  "Official flags, inclusive",
  "Official flags, exclude nonmarket labels",
  "Official flags, market deed types"
)

nonmarket_mydec_types <- c(
  "Beneficial interest",
  "Corrective Deed",
  "Court Officer's Deed",
  "Deed in lieu of Foreclosure",
  "Executor Deed",
  "Judge's Deed",
  "Judicial Sale",
  "Master's Deed",
  "Mineral Deed",
  "Selling Officer's Deed",
  "Sheriff's Deed",
  "Special Commissioner's Deed",
  "Tax Deed"
)
market_warranty_mydec_types <- c(
  "Special Warranty Deed",
  "Limited Warranty Deed"
)

sales <- fread(
  "../input/parcel_sales_city.csv",
  colClasses = list(character = c("pin", "row_id", "sale_document_num"))
)
sales[, `:=`(
  year = suppressWarnings(as.integer(year)),
  class_numeric = suppressWarnings(as.integer(class)),
  sale_price_numeric = suppressWarnings(as.numeric(sale_price)),
  pin = gsub("[^0-9]", "", trimws(pin)),
  sale_date_use = as.IDate(substr(as.character(sale_date), 1, 10))
)]
sales[nchar(pin) == 13L, pin := paste0("0", pin)]
sales[is.na(sale_date_use) & is.finite(year), sale_date_use := as.IDate(sprintf("%d-06-15", year))]
if (any(nchar(sales$pin) != 14L)) {
  stop("Parcel sales contain an invalid full PIN.", call. = FALSE)
}
if (anyDuplicated(sales$row_id) > 0L) {
  stop("Parcel sales must be unique by row_id.", call. = FALSE)
}

sales[, `:=`(
  seller_valid = !is.na(sale_seller_name) &
    !toupper(trimws(sale_seller_name)) %chin% c("", "-", "UNKNOWN", ".."),
  buyer_valid = !is.na(sale_buyer_name) &
    !toupper(trimws(sale_buyer_name)) %chin% c("", "-", "UNKNOWN", ".."),
  seller_normalized = gsub("[^A-Z0-9]", "", toupper(trimws(fifelse(is.na(sale_seller_name), "", sale_seller_name)))),
  buyer_normalized = gsub("[^A-Z0-9]", "", toupper(trimws(fifelse(is.na(sale_buyer_name), "", sale_buyer_name))))
)]
sales[, same_named_party := seller_valid & buyer_valid &
  seller_normalized != "" & seller_normalized == buyer_normalized]
sales[, common_eligible :=
  year >= 2006L & year <= 2022L &
  class_numeric %in% c(202:211, 234, 278, 295) &
  is.finite(sale_price_numeric) & sale_price_numeric > 10000 &
  (is.na(sale_type) | sale_type != "LAND") &
  is.finite(num_parcels_sale) & num_parcels_sale == 1]
sales[, official_flags_pass :=
  sale_filter_same_sale_within_365 == FALSE &
  sale_filter_less_than_10k == FALSE &
  sale_filter_deed_type == FALSE]
sales[, nonmarket_mydec := mydec_deed_type %chin% nonmarket_mydec_types]
sales[, market_deed_type :=
  sale_deed_type %chin% c("Warranty", "Trustee") |
  mydec_deed_type %chin% market_warranty_mydec_types]

sales[, `Legacy names-required` :=
  common_eligible &
  sale_deed_type %chin% c("Warranty", "Trustee") &
  seller_valid & buyer_valid &
  sale_seller_name != sale_buyer_name]
sales[, `Warranty/trustee, names optional` :=
  common_eligible &
  sale_deed_type %chin% c("Warranty", "Trustee") &
  !same_named_party]
sales[, `Production current` :=
  common_eligible & official_flags_pass &
  sale_deed_type %chin% c("Warranty", "Trustee") &
  !same_named_party]
sales[, `Official flags, inclusive` :=
  common_eligible & official_flags_pass]
sales[, `Official flags, exclude nonmarket labels` :=
  common_eligible & official_flags_pass &
  !nonmarket_mydec & !same_named_party]
sales[, `Official flags, market deed types` :=
  common_eligible & official_flags_pass &
  market_deed_type & !same_named_party]

cpi <- read_csv(
  "../input/fred_cpi_cuura207sa0.csv",
  col_types = cols(.default = "c"),
  show_col_types = FALSE
) %>%
  transmute(
    sale_year_month = format(as.Date(observation_date), "%Y-%m"),
    cpi_value = suppressWarnings(as.numeric(CUURA207SA0))
  ) %>%
  filter(!is.na(sale_year_month), is.finite(cpi_value))
if (anyDuplicated(cpi$sale_year_month) > 0L) {
  stop("CPI input must be unique by month.", call. = FALSE)
}
base_cpi <- mean(cpi$cpi_value[substr(cpi$sale_year_month, 1, 4) == "2022"])
if (!is.finite(base_cpi) || base_cpi <= 0) {
  stop("Unable to construct the 2022 CPI base.", call. = FALSE)
}
cpi <- as.data.table(cpi)
cpi[, sale_price_deflator_to_2022 := base_cpi / cpi_value]
sales[, sale_year_month := format(sale_date_use, "%Y-%m")]
sales <- merge(
  sales,
  cpi[, .(sale_year_month, sale_price_deflator_to_2022)],
  by = "sale_year_month",
  all.x = TRUE,
  sort = FALSE
)
sales[, sale_price_real_2022 := sale_price_numeric * sale_price_deflator_to_2022]
if (sales[common_eligible == TRUE, any(!is.finite(sale_price_real_2022))]) {
  stop("Eligible sales have unresolved CPI deflators.", call. = FALSE)
}

improvements <- as.data.table(read_parquet("../input/residential_improvements_panel.parquet"))
improvements[, `:=`(
  pin = as.character(pin),
  year = suppressWarnings(as.integer(tax_year))
)]
if (anyDuplicated(improvements[, .(pin, year)]) > 0L) {
  stop("Residential improvements must be unique by PIN-year.", call. = FALSE)
}
improvements[, `:=`(
  building_age = year - year_built,
  baths_total = num_full_baths + 0.5 * fifelse(is.na(num_half_baths), 0, num_half_baths),
  has_garage = as.integer(is.finite(garage_size) & garage_size > 0),
  structure_eligible =
    num_buildings == 1 &
    !is_multibuilding &
    is.finite(tieback_proration_rate) &
    abs(tieback_proration_rate - 1) < 0.000001 &
    is.finite(building_sqft) & building_sqft > 0
)]
improvements[, `:=`(
  log_sqft = fifelse(is.finite(building_sqft) & building_sqft > 0, log(building_sqft), NA_real_),
  log_land_sqft = fifelse(is.finite(land_sqft) & land_sqft > 0, log(land_sqft), NA_real_),
  log_building_age = fifelse(is.finite(building_age) & building_age > 0, log(building_age), NA_real_),
  log_bedrooms = fifelse(is.finite(num_bedrooms) & num_bedrooms > 0, log(num_bedrooms), NA_real_),
  log_baths = fifelse(is.finite(baths_total) & baths_total > 0, log(baths_total), NA_real_)
)]
improvements <- improvements[, .(
  pin, year, log_sqft, log_land_sqft, log_building_age,
  log_bedrooms, log_baths, has_garage, structure_eligible
)]
sales <- merge(
  sales,
  improvements,
  by = c("pin", "year"),
  all.x = TRUE,
  sort = FALSE
)
sales[, complete_hedonics :=
  structure_eligible == TRUE &
  is.finite(log_sqft) &
  is.finite(log_land_sqft) &
  is.finite(log_building_age) &
  is.finite(log_bedrooms) &
  is.finite(log_baths) &
  is.finite(has_garage)]
sales[, year_quarter := paste0(year, "-Q", quarter(as.Date(sale_date_use)))]

union_eligible <- Reduce(
  `|`,
  lapply(sample_order, function(sample_i) sales[[sample_i]])
)
hedonic_data <- sales[
  union_eligible == TRUE & complete_hedonics == TRUE &
    !is.na(neighborhood_code) & neighborhood_code != "" &
    !is.na(year_quarter) & is.finite(sale_price_real_2022) & sale_price_real_2022 > 0
]
hedonic_model <- feols(
  log(sale_price_real_2022) ~
    log_sqft + log_land_sqft + log_building_age +
    log_bedrooms + log_baths + has_garage + i(class_numeric) |
    neighborhood_code^year_quarter,
  data = hedonic_data,
  warn = FALSE,
  notes = FALSE
)
hedonic_data[, hedonic_residual := resid(hedonic_model)]
sales <- merge(
  sales,
  hedonic_data[, .(row_id, hedonic_residual)],
  by = "row_id",
  all.x = TRUE,
  sort = FALSE
)

resale_sequence <- sales[
  get("Official flags, inclusive") == TRUE,
  .(row_id, pin, sale_date_use, sale_price_real_2022)
]
setorder(resale_sequence, pin, sale_date_use, row_id)
resale_sequence[, `:=`(
  next_sale_date = shift(sale_date_use, type = "lead"),
  next_sale_price_real_2022 = shift(sale_price_real_2022, type = "lead")
), by = pin]
resale_sequence[, resale_gap_days := as.integer(next_sale_date - sale_date_use)]
resale_sequence[, next_sale_price_ratio := fifelse(
  resale_gap_days >= 30L & resale_gap_days <= 5L * 365L,
  next_sale_price_real_2022 / sale_price_real_2022,
  NA_real_
)]
sales <- merge(
  sales,
  resale_sequence[, .(row_id, resale_gap_days, next_sale_price_ratio)],
  by = "row_id",
  all.x = TRUE,
  sort = FALSE
)

sample_rows <- rbindlist(lapply(sample_order, function(sample_i) {
  data.table(
    sample = sample_i,
    row_id = sales[["row_id"]][sales[[sample_i]]]
  )
}))
sample_rows <- merge(
  sample_rows,
  sales,
  by = "row_id",
  all.x = TRUE,
  allow.cartesian = TRUE,
  sort = FALSE
)
sample_rows[, sample := factor(sample, levels = sample_order)]

production_n <- sample_rows[sample == "Production current", .N]
summary_output <- sample_rows[, .(
  n = .N,
  change_from_production = .N - production_n,
  median_price_real_2022 = median(sale_price_real_2022),
  p10_price_real_2022 = quantile(sale_price_real_2022, 0.10),
  p90_price_real_2022 = quantile(sale_price_real_2022, 0.90),
  pct_price_below_50000_real = 100 * mean(sale_price_real_2022 < 50000),
  pct_invalid_seller_or_buyer = 100 * mean(!seller_valid | !buyer_valid),
  pct_same_named_party = 100 * mean(same_named_party),
  pct_other_deed = 100 * mean(sale_deed_type == "Other", na.rm = TRUE),
  pct_special_or_limited_warranty = 100 * mean(
    mydec_deed_type %chin% market_warranty_mydec_types
  ),
  pct_missing_mydec_subtype = 100 * mean(
    is.na(mydec_deed_type) | mydec_deed_type == ""
  ),
  pct_nonmarket_mydec_label = 100 * mean(nonmarket_mydec),
  pct_complete_hedonics = 100 * mean(complete_hedonics),
  hedonic_residual_sd = sd(hedonic_residual, na.rm = TRUE),
  pct_below_half_hedonic_prediction = 100 * mean(
    hedonic_residual < -log(2),
    na.rm = TRUE
  ),
  pct_above_double_hedonic_prediction = 100 * mean(
    hedonic_residual > log(2),
    na.rm = TRUE
  ),
  resale_within_five_years_n = sum(is.finite(next_sale_price_ratio)),
  median_next_sale_price_ratio = median(
    next_sale_price_ratio,
    na.rm = TRUE
  ),
  pct_next_sale_more_than_double = 100 * mean(
    next_sale_price_ratio > 2,
    na.rm = TRUE
  ),
  count_2015_relative_to_adjacent_years =
    sum(year == 2015L) / mean(c(sum(year == 2014L), sum(year == 2016L)))
), by = sample]
summary_output[, sample := as.character(sample)]

by_year_output <- sample_rows[, .(
  n = .N,
  median_price_real_2022 = median(sale_price_real_2022),
  p10_price_real_2022 = quantile(sale_price_real_2022, 0.10),
  p90_price_real_2022 = quantile(sale_price_real_2022, 0.90),
  pct_other_deed = 100 * mean(sale_deed_type == "Other", na.rm = TRUE),
  pct_missing_mydec_subtype = 100 * mean(
    is.na(mydec_deed_type) | mydec_deed_type == ""
  ),
  pct_nonmarket_mydec_label = 100 * mean(nonmarket_mydec)
), by = .(sample, year)]
by_year_output[, sample := as.character(sample)]

sample_rows[, mydec_group := fcase(
  mydec_deed_type %chin% market_warranty_mydec_types,
  "Special or limited warranty",
  nonmarket_mydec,
  "Nonmarket label",
  is.na(mydec_deed_type) | mydec_deed_type == "",
  "Missing MyDec subtype",
  default = "Other MyDec subtype"
)]
deed_output <- sample_rows[, .N, by = .(
  sample, sale_deed_type, mydec_group
)]
deed_output[, share := N / sum(N), by = sample]
deed_output[, sample := as.character(sample)]
deed_output[, sample_order_index := match(sample, sample_order)]
setorder(deed_output, sample_order_index, -N)
deed_output[, sample_order_index := NULL]

deed_quality_output <- sample_rows[
  sample == "Official flags, inclusive",
  .(
    n = .N,
    median_price_real_2022 = median(sale_price_real_2022),
    pct_below_50000_real = 100 * mean(sale_price_real_2022 < 50000),
    hedonic_residual_sd = sd(hedonic_residual, na.rm = TRUE),
    pct_below_half_hedonic_prediction = 100 * mean(
      hedonic_residual < -log(2),
      na.rm = TRUE
    ),
    resale_within_five_years_n = sum(is.finite(next_sale_price_ratio)),
    median_next_sale_price_ratio = median(
      next_sale_price_ratio,
      na.rm = TRUE
    ),
    pct_next_sale_more_than_double = 100 * mean(
      next_sale_price_ratio > 2,
      na.rm = TRUE
    )
  ),
  by = .(sale_deed_type, mydec_group)
]
setorder(deed_quality_output, -n)

overlap_output <- rbindlist(lapply(sample_order, function(left_sample) {
  rbindlist(lapply(sample_order, function(right_sample) {
    left_ids <- sales[["row_id"]][sales[[left_sample]]]
    right_ids <- sales[["row_id"]][sales[[right_sample]]]
    data.table(
      left_sample = left_sample,
      right_sample = right_sample,
      intersection_n = length(intersect(left_ids, right_ids)),
      union_n = length(union(left_ids, right_ids)),
      jaccard = length(intersect(left_ids, right_ids)) /
        length(union(left_ids, right_ids))
    )
  }))
}))

candidate_output <- sales[union_eligible == TRUE, c(
  "row_id", "sale_document_num", "pin", "year", "sale_date_use",
  "sale_price_numeric", "sale_price_real_2022", "class_numeric",
  "sale_deed_type", "mydec_deed_type", "neighborhood_code",
  "seller_valid", "buyer_valid", "same_named_party",
  "official_flags_pass", "nonmarket_mydec", "market_deed_type",
  "structure_eligible",
  "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms",
  "log_baths", "has_garage", "complete_hedonics",
  sample_order
), with = FALSE]
coordinate_keys_output <- unique(candidate_output[, .(pin, year)])
setorder(coordinate_keys_output, year, pin)

plot_data <- copy(by_year_output)
plot_data[, count_index_2006 := 100 * n / n[year == 2006L], by = sample]
plot_data[, sample := factor(sample, levels = sample_order)]
count_plot <- ggplot(plot_data, aes(year, count_index_2006, color = sample)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  labs(
    x = NULL,
    y = "Sales count (2006 = 100)",
    color = NULL,
    title = "Candidate sales samples over time"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
price_plot <- ggplot(plot_data, aes(year, median_price_real_2022, color = sample)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(
    x = "Sale year",
    y = "Median sale price (2022 dollars)",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

fwrite(summary_output, "../output/sales_sample_definition_summary.csv")
fwrite(by_year_output, "../output/sales_sample_definition_by_year.csv")
fwrite(deed_output, "../output/sales_sample_definition_deed_composition.csv")
fwrite(deed_quality_output, "../output/sales_sample_definition_deed_quality.csv")
fwrite(overlap_output, "../output/sales_sample_definition_overlap.csv")
write_parquet(
  as.data.frame(candidate_output),
  "../output/sales_sample_definition_candidates.parquet"
)
fwrite(
  coordinate_keys_output,
  "../output/sales_sample_definition_coordinate_keys.csv"
)
ggsave(
  "../output/sales_sample_definition_comparison.pdf",
  count_plot / price_plot,
  width = 9,
  height = 8
)
