# setwd("tasks/audits/sales_record_quality_audit/code")
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# bin_width_ft <- 100
# placebo_ft <- 1000
# rent_controls <- "log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft"
# sales_controls <- "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft"
# rent_fixed_effects <- "segment_id^year_month"
# sales_fixed_effects <- "segment_id^year_quarter"
# cluster <- "ward_pair"
# rent_start_year <- 2014

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) cli_args <- c(start_year, end_year, bandwidth_ft, bin_width_ft, placebo_ft, rent_controls, sales_controls, rent_fixed_effects, sales_fixed_effects, cluster, rent_start_year)
stopifnot(length(cli_args) == 11L)
start_year <- as.integer(cli_args[1])
end_year <- as.integer(cli_args[2])
bandwidth_ft <- as.integer(cli_args[3])
bin_width_ft <- as.integer(cli_args[4])
placebo_ft <- as.integer(cli_args[5])
rent_controls <- cli_args[6]
sales_controls <- cli_args[7]
rent_fixed_effects <- cli_args[8]
sales_fixed_effects <- cli_args[9]
cluster <- cli_args[10]
rent_start_year <- as.integer(cli_args[11])
stopifnot(start_year <= end_year, bandwidth_ft > 0, bin_width_ft > 0,
  bandwidth_ft %% bin_width_ft == 0)

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("../../../shared/code/canonical_geometry_helpers.R")
source("../../../shared/code/price_boundary_helpers.R")

rent <- arrow::read_parquet(
  "../input/rental_rd_characteristics_panel_bw1500.parquet"
) |>
  tibble::as_tibble()

if (anyDuplicated(rent$rent_panel_id) > 0L) {
  stop("Rental input must be unique by rent_panel_id.")
}

rent <- rent |>
  dplyr::mutate(
    file_date = as.Date(file_date),
    assignment_date = as.Date(assignment_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    score_tie = strictness_own == strictness_neighbor,
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    era = canonical_era_from_date(
      assignment_date,
      allow_pre_2003 = FALSE
    ),
    log_sqft = dplyr::if_else(
      is.finite(sqft) & sqft > 0,
      log(sqft),
      NA_real_
    ),
    beds_factor = factor(beds),
    log_baths = dplyr::if_else(
      is.finite(baths) & baths > 0,
      log(baths),
      NA_real_
    ),
    building_type_factor = factor(
      dplyr::coalesce(building_type_clean, "other")
    )
  ) |>
  dplyr::filter(
    !is.na(file_date),
    year >= rent_start_year,
    year <= end_year,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) < placebo_ft + bandwidth_ft,
    is.finite(strictness_own),
    is.finite(strictness_neighbor),
    !score_tie,
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != "",
    !is.na(era),
    flag_clean_location_sample,
    is.finite(longitude),
    is.finite(latitude),
    is.finite(beds),
    beds >= 0,
    !is.na(log_sqft),
    !is.na(log_baths),
    dplyr::if_all(
      dplyr::all_of(c(
        "nearest_school_dist_kft",
        "nearest_park_dist_kft",
        "nearest_major_road_dist_kft",
        "nearest_cta_stop_dist_kft",
        "lake_michigan_dist_kft"
      )),
      is.finite
    )
  )

sales <- arrow::read_parquet(
  "../input/clean_sales_with_hedonics_amenities.parquet"
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    sale_date = as.Date(sale_date),
    year = lubridate::year(sale_date),
    year_quarter = paste0(year, "-Q", lubridate::quarter(sale_date)),
    score_tie = strictness_own == strictness_neighbor,
    signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    property_class_factor = factor(class),
    era = canonical_era_from_date(
      sale_date,
      allow_pre_2003 = TRUE
    )
  ) |>
  dplyr::filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= start_year,
    year <= end_year,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) < placebo_ft + bandwidth_ft,
    is.finite(strictness_own),
    is.finite(strictness_neighbor),
    !score_tie,
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != "",
    !is.na(era),
    is.finite(longitude),
    is.finite(latitude),
    dplyr::if_all(
      dplyr::all_of(c(
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
      )),
      is.finite
    )
  )

rent$straight_boundary <- FALSE
sales$straight_boundary <- FALSE

property_type_fits <- list(
  rent_without = estimate_bins(
    data = rent,
    market = "rent",
    cutoff_ft = 0,
    donut_ft = 0,
    straight_only = FALSE,
    property_type_fe = FALSE,
    panel_title = "Listed rents: without building-type FE",
    rent_controls = rent_controls, sales_controls = sales_controls,
      rent_fixed_effects = rent_fixed_effects, sales_fixed_effects = sales_fixed_effects,
      bandwidth_ft = bandwidth_ft, bin_width_ft = bin_width_ft, cluster = cluster
  ),
  rent_with = estimate_bins(
    data = rent,
    market = "rent",
    cutoff_ft = 0,
    donut_ft = 0,
    straight_only = FALSE,
    property_type_fe = TRUE,
    panel_title = "Listed rents: with building-type FE",
    rent_controls = rent_controls, sales_controls = sales_controls,
      rent_fixed_effects = rent_fixed_effects, sales_fixed_effects = sales_fixed_effects,
      bandwidth_ft = bandwidth_ft, bin_width_ft = bin_width_ft, cluster = cluster
  ),
  sales_without = estimate_bins(
    data = sales,
    market = "sales",
    cutoff_ft = 0,
    donut_ft = 0,
    straight_only = FALSE,
    property_type_fe = FALSE,
    panel_title = "Home sales: without property-class FE",
    rent_controls = rent_controls, sales_controls = sales_controls,
      rent_fixed_effects = rent_fixed_effects, sales_fixed_effects = sales_fixed_effects,
      bandwidth_ft = bandwidth_ft, bin_width_ft = bin_width_ft, cluster = cluster
  ),
  sales_with = estimate_bins(
    data = sales,
    market = "sales",
    cutoff_ft = 0,
    donut_ft = 0,
    straight_only = FALSE,
    property_type_fe = TRUE,
    panel_title = "Home sales: with property-class FE",
    rent_controls = rent_controls, sales_controls = sales_controls,
      rent_fixed_effects = rent_fixed_effects, sales_fixed_effects = sales_fixed_effects,
      bandwidth_ft = bandwidth_ft, bin_width_ft = bin_width_ft, cluster = cluster
  )
)

property_type_plot <- patchwork::wrap_plots(
  property_type_fits$rent_without$plot,
  property_type_fits$rent_with$plot,
  property_type_fits$sales_without$plot,
  property_type_fits$sales_with$plot,
  ncol = 2
) +
  patchwork::plot_annotation(
    title = "Price-boundary estimates with and without property-type fixed effects"
  ) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

property_type_estimates <- dplyr::bind_rows(
  property_type_fits$rent_without$summary |>
    dplyr::mutate(specification = "without building-type FE"),
  property_type_fits$rent_with$summary |>
    dplyr::mutate(specification = "with building-type FE"),
  property_type_fits$sales_without$summary |>
    dplyr::mutate(specification = "without property-class FE"),
  property_type_fits$sales_with$summary |>
    dplyr::mutate(specification = "with property-class FE")
) |>
  dplyr::select(
    market,
    specification,
    property_type_fe,
    dplyr::everything()
  )

ggplot2::ggsave(
  "../output/price_boundary_property_type_fe_comparison.pdf",
  property_type_plot,
  width = 12,
  height = 10,
  bg = "white"
)
SaveData(property_type_estimates, c("market", "specification"), "../output/price_boundary_property_type_fe_estimates.csv")
