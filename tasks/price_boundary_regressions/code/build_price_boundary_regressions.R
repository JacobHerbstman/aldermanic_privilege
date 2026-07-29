# setwd("tasks/price_boundary_regressions/code")
# bandwidth_ft <- 500
# bins_per_side <- 10L
# rent_start_year <- 2014L
# rent_end_year <- 2022L
# sales_start_year <- 2006L
# sales_end_year <- 2022L

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0L) {
  cli_args <- c(
    bandwidth_ft,
    bins_per_side,
    rent_start_year,
    rent_end_year,
    sales_start_year,
    sales_end_year
  )
}
if (length(cli_args) != 6L) {
  stop("Expected bandwidth, bins, and rental and sales year ranges.")
}

bandwidth_ft <- as.numeric(cli_args[1])
bins_per_side <- as.integer(cli_args[2])
rent_start_year <- as.integer(cli_args[3])
rent_end_year <- as.integer(cli_args[4])
sales_start_year <- as.integer(cli_args[5])
sales_end_year <- as.integer(cli_args[6])

if (
  !is.finite(bandwidth_ft) ||
    bandwidth_ft <= 0 ||
    is.na(bins_per_side) ||
    bins_per_side < 2L ||
    rent_start_year > rent_end_year ||
    sales_start_year > sales_end_year
) {
  stop("Invalid price-boundary specification.")
}

rent <- arrow::read_parquet(
  sprintf(
    "../input/rental_rd_characteristics_panel_bw%.0f.parquet",
    bandwidth_ft
  )
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    right = as.integer(strictness_own > strictness_neighbor),
    signed_dist_ft = abs(as.numeric(signed_dist)) * dplyr::if_else(
      right == 1L,
      1,
      -1
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
    ),
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
  ) |>
  dplyr::filter(
    !is.na(file_date),
    year >= rent_start_year,
    year <= rent_end_year,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= bandwidth_ft,
    is.finite(strictness_own),
    is.finite(strictness_neighbor),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair_id),
    flag_clean_location_sample,
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

rent_controls <- "log_sqft + beds_factor + log_baths"
if (dplyr::n_distinct(rent$building_type_factor) > 1L) {
  rent_controls <- paste(rent_controls, "+ building_type_factor")
}
rent_controls <- paste(
  rent_controls,
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft",
  sep = " + "
)

rent_model <- fixest::feols(
  stats::as.formula(paste0(
    "log(rent_price) ~ right + ",
    rent_controls,
    " | segment_id^year_month"
  )),
  data = rent,
  cluster = ~segment_id
)

sales <- arrow::read_parquet(
  "../input/sales_with_hedonics_amenities.parquet"
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    right = as.integer(strictness_own > strictness_neighbor),
    signed_dist_ft = abs(as.numeric(signed_dist_m) / 0.3048) *
      dplyr::if_else(right == 1L, 1, -1)
  ) |>
  dplyr::filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= sales_start_year,
    year <= sales_end_year,
    !is.na(ward_pair_id),
    !is.na(segment_id),
    segment_id != "",
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= bandwidth_ft,
    is.finite(strictness_own),
    is.finite(strictness_neighbor)
  )

hedonic_controls <- c(
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage"
)
amenity_controls <- c(
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft"
)
sales <- sales |>
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(c(hedonic_controls, amenity_controls)),
      is.finite
    )
  )

sales_model <- fixest::feols(
  stats::as.formula(paste0(
    "log(sale_price) ~ right + ",
    paste(c(hedonic_controls, amenity_controls), collapse = " + "),
    " | segment_id^year_quarter"
  )),
  data = sales,
  cluster = ~segment_id
)

rent_result <- fixest::coeftable(rent_model)["right", ]
sales_result <- fixest::coeftable(sales_model)["right", ]

format_estimate <- function(result) {
  stars <- dplyr::case_when(
    result[["Pr(>|t|)"]] <= 0.01 ~ "***",
    result[["Pr(>|t|)"]] <= 0.05 ~ "**",
    result[["Pr(>|t|)"]] <= 0.10 ~ "*",
    TRUE ~ ""
  )
  paste0(sprintf("%.3f", result[["Estimate"]]), stars)
}

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcc}",
  "   \\toprule",
  "                    & Listed Rent & Home Sale Price \\\\",
  "                    & (1)         & (2) \\\\",
  "   \\midrule",
  paste0(
    "   More-Stringent Side & ",
    format_estimate(rent_result),
    " & ",
    format_estimate(sales_result),
    " \\\\"
  ),
  sprintf(
    "                    & (%.3f) & (%.3f) \\\\",
    rent_result[["Std. Error"]],
    sales_result[["Std. Error"]]
  ),
  sprintf(
    "   N                & %s & %s \\\\",
    trimws(format(stats::nobs(rent_model), big.mark = ",")),
    trimws(format(stats::nobs(sales_model), big.mark = ","))
  ),
  "   \\midrule",
  "   Hedonic Controls   & $\\checkmark$ & $\\checkmark$ \\\\",
  "   Amenity Controls   & $\\checkmark$ & $\\checkmark$ \\\\",
  "   Segment $\\times$ Period FE & $\\checkmark$ & $\\checkmark$ \\\\",
  "   \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(
  table_lines,
  sprintf(
    "../output/price_boundary_regressions_%.0fft.tex",
    bandwidth_ft
  )
)

rent_removed <- rent_model$obs_selection$obsRemoved
rent_keep <- if (is.null(rent_removed)) {
  seq_len(nrow(rent))
} else {
  setdiff(seq_len(nrow(rent)), abs(as.integer(rent_removed)))
}
rent_plot_data <- rent[rent_keep, , drop = FALSE]
rent_plot_data$adjusted_outcome <- as.numeric(stats::resid(rent_model)) +
  rent_result[["Estimate"]] * rent_plot_data$right

sales_removed <- sales_model$obs_selection$obsRemoved
sales_keep <- if (is.null(sales_removed)) {
  seq_len(nrow(sales))
} else {
  setdiff(seq_len(nrow(sales)), abs(as.integer(sales_removed)))
}
sales_plot_data <- sales[sales_keep, , drop = FALSE]
sales_plot_data$adjusted_outcome <- as.numeric(stats::resid(sales_model)) +
  sales_result[["Estimate"]] * sales_plot_data$right

rent_bins <- rent_plot_data |>
  dplyr::mutate(
    bin_center = (
      floor(signed_dist_ft / (bandwidth_ft / bins_per_side)) + 0.5
    ) * (bandwidth_ft / bins_per_side)
  ) |>
  dplyr::group_by(bin_center) |>
  dplyr::summarise(
    mean_outcome = mean(adjusted_outcome),
    side = dplyr::if_else(
      dplyr::first(bin_center) >= 0,
      "More Stringent",
      "Less Stringent"
    ),
    .groups = "drop"
  )

sales_bins <- sales_plot_data |>
  dplyr::mutate(
    bin_center = (
      floor(signed_dist_ft / (bandwidth_ft / bins_per_side)) + 0.5
    ) * (bandwidth_ft / bins_per_side)
  ) |>
  dplyr::group_by(bin_center) |>
  dplyr::summarise(
    mean_outcome = mean(adjusted_outcome),
    side = dplyr::if_else(
      dplyr::first(bin_center) >= 0,
      "More Stringent",
      "Less Stringent"
    ),
    .groups = "drop"
  )

price_plot <- function(bins, result, title, y_label) {
  stars <- dplyr::case_when(
    result[["Pr(>|t|)"]] <= 0.01 ~ "***",
    result[["Pr(>|t|)"]] <= 0.05 ~ "**",
    result[["Pr(>|t|)"]] <= 0.10 ~ "*",
    TRUE ~ ""
  )

  ggplot2::ggplot(
    bins,
    ggplot2::aes(bin_center, mean_outcome, color = side)
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray30",
      linewidth = 0.6
    ) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::scale_color_manual(
      values = c(
        "Less Stringent" = "#1f77b4",
        "More Stringent" = "#d62728"
      ),
      name = NULL
    ) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf(
        "Jump = %.3f%s (SE %.3f)",
        result[["Estimate"]],
        stars,
        result[["Std. Error"]]
      ),
      x = "Distance to ward boundary (feet)",
      y = y_label
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

ggplot2::ggsave(
  sprintf(
    "../output/price_boundary_rent_%.0fft.pdf",
    bandwidth_ft
  ),
  price_plot(
    rent_bins,
    rent_result,
    "Listed Rents",
    "Adjusted log rent"
  ),
  width = 5.1,
  height = 4.3,
  bg = "white"
)

ggplot2::ggsave(
  sprintf(
    "../output/price_boundary_sales_%.0fft.pdf",
    bandwidth_ft
  ),
  price_plot(
    sales_bins,
    sales_result,
    "Home Sale Prices",
    "Adjusted log sale price"
  ),
  width = 5.1,
  height = 4.3,
  bg = "white"
)
