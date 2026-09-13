# setwd("tasks/price_boundary_results/code")
# official_property_type_fe <- "TRUE"
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# bin_width_ft <- 100
# placebo_ft <- 1000
# donut_inner_ft <- 25
# donut_outer_ft <- 50
# rent_controls <- "log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft"
# sales_controls <- "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft"
# rent_fixed_effects <- "segment_id^year_month"
# sales_fixed_effects <- "segment_id^year_quarter"
# cluster <- "ward_pair"
# rent_start_year <- 2014

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) cli_args <- c(official_property_type_fe, start_year, end_year, bandwidth_ft, bin_width_ft, placebo_ft, donut_inner_ft, donut_outer_ft, rent_controls, sales_controls, rent_fixed_effects, sales_fixed_effects, cluster, rent_start_year)
stopifnot(length(cli_args) == 14L)
official_property_type_fe <- cli_args[1]
start_year <- as.integer(cli_args[2])
end_year <- as.integer(cli_args[3])
bandwidth_ft <- as.integer(cli_args[4])
bin_width_ft <- as.integer(cli_args[5])
placebo_ft <- as.integer(cli_args[6])
donut_inner_ft <- as.integer(cli_args[7])
donut_outer_ft <- as.integer(cli_args[8])
rent_controls <- cli_args[9]
sales_controls <- cli_args[10]
rent_fixed_effects <- cli_args[11]
sales_fixed_effects <- cli_args[12]
cluster <- cli_args[13]
rent_start_year <- as.integer(cli_args[14])
stopifnot(start_year <= end_year, bandwidth_ft > 0, bin_width_ft > 0,
  bandwidth_ft %% bin_width_ft == 0)

stopifnot(official_property_type_fe %in% c("TRUE", "FALSE"))
official_property_type_fe <- official_property_type_fe == "TRUE"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/canonical_geometry_helpers.R")
source("../../shared/code/price_boundary_helpers.R")

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
  "../input/sales_with_hedonics_amenities.parquet"
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

boundaries <- do.call(
  rbind,
  lapply(
    c("2003_2014", "2015_2023"),
    function(layer_name) {
      sf::st_read(
        "../input/ward_pair_boundaries.gpkg",
        layer = layer_name,
        quiet = TRUE
      ) |>
        sf::st_transform(3435) |>
        dplyr::select(era, ward_pair_id)
    }
  )
)

if (
  anyDuplicated(
    sf::st_drop_geometry(boundaries)[c("era", "ward_pair_id")]
  ) > 0L
) {
  stop("Ward-pair boundaries must be unique by era and ward pair.")
}

location_keys <- c(
  "market",
  "longitude",
  "latitude",
  "era",
  "ward_pair",
  "segment_id"
)

locations <- dplyr::bind_rows(
  rent |>
    dplyr::filter(abs(signed_dist_ft) < 500) |>
    dplyr::transmute(
      market = "rent",
      longitude,
      latitude,
      era,
      ward_pair,
      segment_id,
      expected_distance_ft = abs(signed_dist_ft)
    ),
  sales |>
    dplyr::filter(abs(signed_dist_ft) < 500) |>
    dplyr::transmute(
      market = "sales",
      longitude,
      latitude,
      era,
      ward_pair,
      segment_id,
      expected_distance_ft = abs(signed_dist_ft)
    )
) |>
  dplyr::summarise(
    distance_spread_ft = max(expected_distance_ft) -
      min(expected_distance_ft),
    expected_distance_ft = median(expected_distance_ft),
    .by = dplyr::all_of(location_keys)
  )

if (max(locations$distance_spread_ft, na.rm = TRUE) > 1) {
  stop("A location has inconsistent assigned boundary distances.")
}

location_sf <- locations |>
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  sf::st_transform(3435)

location_sf$reconstructed_distance_ft <- NA_real_
location_sf$left_endpoint_distance_m <- NA_real_
location_sf$right_endpoint_distance_m <- NA_real_

location_groups <- interaction(
  location_sf$era,
  location_sf$ward_pair,
  drop = TRUE,
  lex.order = TRUE
)

for (idx in split(seq_len(nrow(location_sf)), location_groups)) {
  boundary_row <- boundaries |>
    dplyr::filter(
      era == location_sf$era[idx[1]],
      ward_pair_id == location_sf$ward_pair[idx[1]]
    )
  if (nrow(boundary_row) != 1L) {
    stop("Could not identify one ward-pair boundary for a price location.")
  }

  repeated_boundary <- sf::st_sfc(
    rep(
      list(sf::st_geometry(boundary_row)[[1]]),
      length(idx)
    ),
    crs = sf::st_crs(boundary_row)
  )
  nearest_lines <- sf::st_nearest_points(
    sf::st_geometry(location_sf[idx, ]),
    repeated_boundary,
    pairwise = TRUE
  )

  point_xy <- matrix(NA_real_, nrow = length(idx), ncol = 2)
  boundary_xy <- matrix(NA_real_, nrow = length(idx), ncol = 2)
  for (j in seq_along(idx)) {
    nearest_coordinates <- sf::st_coordinates(nearest_lines[j])
    point_xy[j, ] <- nearest_coordinates[1, c("X", "Y")]
    boundary_xy[j, ] <- nearest_coordinates[
      nrow(nearest_coordinates),
      c("X", "Y")
    ]
  }

  normal_vector <- boundary_xy - point_xy
  normal_length <- sqrt(rowSums(normal_vector^2))
  if (any(!is.finite(normal_length) | normal_length <= 0)) {
    stop("A price observation lies directly on its assigned boundary.")
  }

  tangent_unit <- cbind(
    -normal_vector[, 2] / normal_length,
    normal_vector[, 1] / normal_length
  )
  half_line_ft <- 50 / 0.3048
  left_xy <- boundary_xy - half_line_ft * tangent_unit
  right_xy <- boundary_xy + half_line_ft * tangent_unit

  left_points <- sf::st_sfc(
    lapply(
      seq_len(nrow(left_xy)),
      function(j) sf::st_point(left_xy[j, ])
    ),
    crs = sf::st_crs(boundary_row)
  )
  right_points <- sf::st_sfc(
    lapply(
      seq_len(nrow(right_xy)),
      function(j) sf::st_point(right_xy[j, ])
    ),
    crs = sf::st_crs(boundary_row)
  )

  location_sf$reconstructed_distance_ft[idx] <- normal_length
  location_sf$left_endpoint_distance_m[idx] <- as.numeric(
    sf::st_distance(left_points, sf::st_geometry(boundary_row))
  ) * 0.3048
  location_sf$right_endpoint_distance_m[idx] <- as.numeric(
    sf::st_distance(right_points, sf::st_geometry(boundary_row))
  ) * 0.3048
}

location_classification <- location_sf |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    straight_boundary = (
      left_endpoint_distance_m <= 15 &
        right_endpoint_distance_m <= 15
    ),
    distance_error_ft = abs(
      reconstructed_distance_ft - expected_distance_ft
    )
  ) |>
  dplyr::select(
    dplyr::all_of(location_keys),
    straight_boundary,
    distance_error_ft
  )

max_distance_error <- max(
  location_classification$distance_error_ft,
  na.rm = TRUE
)
if (max_distance_error > 2) {
  stop(sprintf(
    paste0(
      "Price coordinates do not reproduce assigned boundary distances. ",
      "Maximum error: %.1f feet."
    ),
    max_distance_error
  ))
}

rent <- rent |>
  dplyr::left_join(
    location_classification |>
      dplyr::filter(market == "rent") |>
      dplyr::select(-market),
    by = setdiff(location_keys, "market"),
    relationship = "many-to-one"
  )
sales <- sales |>
  dplyr::left_join(
    location_classification |>
      dplyr::filter(market == "sales") |>
      dplyr::select(-market),
    by = setdiff(location_keys, "market"),
    relationship = "many-to-one"
  )

if (
  anyNA(rent$straight_boundary[abs(rent$signed_dist_ft) < bandwidth_ft]) ||
    anyNA(sales$straight_boundary[abs(sales$signed_dist_ft) < bandwidth_ft])
) {
  stop("Some true-boundary price observations lack a straightness flag.")
}

checks <- tibble::tribble(
  ~check, ~cutoff_ft, ~donut_ft, ~straight_only,
  "main", 0, 0, FALSE,
  "placebo_neg1000ft", -placebo_ft, 0, FALSE,
  "placebo_pos1000ft", placebo_ft, 0, FALSE,
  "straight", 0, 0, TRUE,
  "donut25ft", 0, donut_inner_ft, FALSE,
  "donut50ft", 0, donut_outer_ft, FALSE
)

fits <- list()
for (market_name in c("rent", "sales")) {
  market_data <- if (market_name == "rent") rent else sales
  market_label <- if (market_name == "rent") "Listed rents" else "Home sales"

  for (check_i in seq_len(nrow(checks))) {
    check_name <- checks$check[check_i]
    check_label <- dplyr::case_when(
      check_name == "main" ~ market_label,
      check_name == "placebo_neg1000ft" ~ paste0(
        market_label,
        ": ", format(placebo_ft, big.mark = ","), " ft inside less-stringent side"
      ),
      check_name == "placebo_pos1000ft" ~ paste0(
        market_label,
        ": ", format(placebo_ft, big.mark = ","), " ft inside more-stringent side"
      ),
      check_name == "straight" ~ market_label,
      check_name == "donut25ft" ~ paste0(
        market_label,
        ": exclude nearest ", donut_inner_ft, " ft"
      ),
      TRUE ~ paste0(market_label, ": exclude nearest ", donut_outer_ft, " ft")
    )

    fits[[paste(market_name, check_name, sep = "_")]] <- estimate_bins(
      data = market_data,
      market = market_name,
      cutoff_ft = checks$cutoff_ft[check_i],
      donut_ft = checks$donut_ft[check_i],
      straight_only = checks$straight_only[check_i],
      property_type_fe = official_property_type_fe,
      panel_title = check_label,
      rent_controls = rent_controls, sales_controls = sales_controls,
      rent_fixed_effects = rent_fixed_effects, sales_fixed_effects = sales_fixed_effects,
      bandwidth_ft = bandwidth_ft, bin_width_ft = bin_width_ft, cluster = cluster
    )
  }
}

main_plot <- patchwork::wrap_plots(
  fits$rent_main$plot,
  fits$sales_main$plot,
  ncol = 2
) +
  patchwork::plot_annotation(
    title = "Listed rents and home sale prices at ward boundaries"
  ) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

placebo_plot <- patchwork::wrap_plots(
  fits$rent_placebo_neg1000ft$plot,
  fits$rent_placebo_pos1000ft$plot,
  fits$sales_placebo_neg1000ft$plot,
  fits$sales_placebo_pos1000ft$plot,
  ncol = 2
) +
  patchwork::plot_annotation(
    title = "Prices at artificial boundaries"
  ) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

straight_plot <- patchwork::wrap_plots(
  fits$rent_straight$plot,
  fits$sales_straight$plot,
  ncol = 2
) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

donut_plot <- patchwork::wrap_plots(
  fits$rent_donut25ft$plot,
  fits$rent_donut50ft$plot,
  fits$sales_donut25ft$plot,
  fits$sales_donut50ft$plot,
  ncol = 2
) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  "../output/price_boundary_main.pdf",
  main_plot,
  width = 12,
  height = 5.5,
  bg = "white"
)
ggplot2::ggsave(
  "../output/price_boundary_placebos.pdf",
  placebo_plot,
  width = 12,
  height = 10,
  bg = "white"
)
ggplot2::ggsave(
  "../output/price_boundary_straight.pdf",
  straight_plot,
  width = 12,
  height = 5.5,
  bg = "white"
)
ggplot2::ggsave(
  "../output/price_boundary_donuts.pdf",
  donut_plot,
  width = 12,
  height = 10,
  bg = "white"
)
