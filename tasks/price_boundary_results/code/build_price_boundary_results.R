# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/price_boundary_results/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

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
    year >= 2014L,
    year <= 2022L,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) < 1500,
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
    era = canonical_era_from_date(
      sale_date,
      allow_pre_2003 = TRUE
    )
  ) |>
  dplyr::filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= 2006L,
    year <= 2022L,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) < 1500,
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
    expected_distance_ft = median(expected_distance_ft),
    distance_spread_ft = max(expected_distance_ft) -
      min(expected_distance_ft),
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
  anyNA(rent$straight_boundary[abs(rent$signed_dist_ft) < 500]) ||
    anyNA(sales$straight_boundary[abs(sales$signed_dist_ft) < 500])
) {
  stop("Some true-boundary price observations lack a straightness flag.")
}

rent_controls <- c(
  "log_sqft",
  "beds_factor",
  "log_baths",
  "building_type_factor",
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft"
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

star_string <- function(p_value) {
  dplyr::case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

estimate_bins <- function(
    data,
    market,
    cutoff_ft,
    donut_ft,
    straight_only,
    panel_title) {
  controls <- if (market == "rent") rent_controls else sales_controls
  outcome <- if (market == "rent") "rent_price" else "sale_price"
  fixed_effects <- if (market == "rent") {
    "segment_id^year_month"
  } else {
    "segment_id^year_quarter"
  }

  model_data <- data |>
    dplyr::mutate(
      running_distance_ft = signed_dist_ft - cutoff_ft,
      cutoff_right = as.integer(running_distance_ft >= 0),
      distance_bin = cut(
        running_distance_ft,
        breaks = seq(-500, 500, by = 100),
        labels = sprintf("bin_%02d", 1:10),
        include.lowest = TRUE,
        right = FALSE
      )
    ) |>
    dplyr::filter(
      abs(running_distance_ft) < 500,
      donut_ft == 0 | abs(running_distance_ft) >= donut_ft,
      !straight_only | straight_boundary,
      !is.na(distance_bin)
    )

  formula <- stats::as.formula(sprintf(
    "log(%s) ~ i(distance_bin, ref = 'bin_05') + %s | %s",
    outcome,
    paste(controls, collapse = " + "),
    fixed_effects
  ))
  model <- fixest::feols(
    formula,
    data = model_data,
    cluster = ~segment_id,
    warn = FALSE,
    notes = FALSE
  )
  full_window_formula <- stats::as.formula(sprintf(
    "log(%s) ~ cutoff_right + %s | %s",
    outcome,
    paste(controls, collapse = " + "),
    fixed_effects
  ))
  full_window_model <- fixest::feols(
    full_window_formula,
    data = model_data,
    cluster = ~segment_id,
    warn = FALSE,
    notes = FALSE
  )

  coefficient_table <- fixest::coeftable(model)
  coefficient_rows <- grepl(
    "^distance_bin::",
    rownames(coefficient_table)
  )
  estimates <- tibble::tibble(
    distance_bin = sub(
      "^distance_bin::",
      "",
      rownames(coefficient_table)[coefficient_rows]
    ),
    estimate = coefficient_table[coefficient_rows, "Estimate"],
    std_error = coefficient_table[coefficient_rows, "Std. Error"],
    p_value = coefficient_table[coefficient_rows, "Pr(>|t|)"]
  )

  cluster_count <- dplyr::n_distinct(model_data$segment_id)
  critical_value <- stats::qt(0.975, df = cluster_count - 1L)
  results <- tibble::tibble(
    distance_bin = sprintf("bin_%02d", 1:10),
    bin_start_ft = seq(-500, 400, by = 100),
    bin_end_ft = seq(-400, 500, by = 100),
    bin_center_ft = seq(-450, 450, by = 100)
  ) |>
    dplyr::left_join(
      estimates,
      by = "distance_bin",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      estimate = dplyr::if_else(
        distance_bin == "bin_05",
        0,
        estimate
      ),
      std_error = dplyr::if_else(
        distance_bin == "bin_05",
        NA_real_,
        std_error
      ),
      p_value = dplyr::if_else(
        distance_bin == "bin_05",
        NA_real_,
        p_value
      ),
      ci_low = estimate - critical_value * std_error,
      ci_high = estimate + critical_value * std_error,
      ribbon_low = dplyr::if_else(
        distance_bin == "bin_05",
        0,
        ci_low
      ),
      ribbon_high = dplyr::if_else(
        distance_bin == "bin_05",
        0,
        ci_high
      ),
      side_label = dplyr::case_when(
        cutoff_ft == 0 & bin_center_ft < 0 ~ "Less Stringent",
        cutoff_ft == 0 ~ "More Stringent",
        bin_center_ft < 0 ~ "Below Placebo Cutoff",
        TRUE ~ "Above Placebo Cutoff"
      )
    )

  nearest_above <- results |>
    dplyr::filter(bin_start_ft == 0)
  nearest_stars <- star_string(nearest_above$p_value)
  full_window_table <- fixest::coeftable(full_window_model)
  full_window_estimate <- unname(
    full_window_table["cutoff_right", "Estimate"]
  )
  full_window_std_error <- unname(
    full_window_table["cutoff_right", "Std. Error"]
  )
  full_window_p_value <- unname(
    full_window_table["cutoff_right", "Pr(>|t|)"]
  )
  full_window_stars <- star_string(full_window_p_value)

  plot <- ggplot2::ggplot(
    results,
    ggplot2::aes(
      x = bin_center_ft,
      y = estimate,
      color = side_label,
      group = side_label
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "gray55",
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray35",
      linewidth = 0.4
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = ribbon_low,
        ymax = ribbon_high,
        fill = side_label
      ),
      alpha = 0.16,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::scale_color_manual(
      values = c(
        "Less Stringent" = "#2478B5",
        "More Stringent" = "#D92D27",
        "Below Placebo Cutoff" = "#2478B5",
        "Above Placebo Cutoff" = "#D92D27"
      ),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Less Stringent" = "#2478B5",
        "More Stringent" = "#D92D27",
        "Below Placebo Cutoff" = "#2478B5",
        "Above Placebo Cutoff" = "#D92D27"
      ),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-500, 500),
      breaks = c(-500, -250, 0, 250, 500)
    ) +
    ggplot2::labs(
      title = panel_title,
      subtitle = sprintf(
        paste0(
          "Nearest-bin difference = %.3f%s (SE %.3f)\n",
          "Full 500ft difference = %.3f%s (SE %.3f)"
        ),
        nearest_above$estimate,
        nearest_stars,
        nearest_above$std_error,
        full_window_estimate,
        full_window_stars,
        full_window_std_error
      ),
      x = if (cutoff_ft == 0) {
        "Distance to ward boundary (feet)"
      } else {
        "Distance to placebo cutoff (feet)"
      },
      y = if (cutoff_ft == 0) {
        "Difference from nearest less-stringent bin"
      } else {
        "Difference from nearest below-cutoff bin"
      }
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )

  plot
}

checks <- tibble::tribble(
  ~check, ~cutoff_ft, ~donut_ft, ~straight_only,
  "main", 0, 0, FALSE,
  "placebo_neg1000ft", -1000, 0, FALSE,
  "placebo_pos1000ft", 1000, 0, FALSE,
  "straight", 0, 0, TRUE,
  "donut25ft", 0, 25, FALSE,
  "donut50ft", 0, 50, FALSE
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
        ": 1,000ft inside less-stringent side"
      ),
      check_name == "placebo_pos1000ft" ~ paste0(
        market_label,
        ": 1,000ft inside more-stringent side"
      ),
      check_name == "straight" ~ market_label,
      check_name == "donut25ft" ~ paste0(
        market_label,
        ": exclude nearest 25ft"
      ),
      TRUE ~ paste0(market_label, ": exclude nearest 50ft")
    )

    fits[[paste(market_name, check_name, sep = "_")]] <- estimate_bins(
      data = market_data,
      market = market_name,
      cutoff_ft = checks$cutoff_ft[check_i],
      donut_ft = checks$donut_ft[check_i],
      straight_only = checks$straight_only[check_i],
      panel_title = check_label
    )
  }
}

main_plot <- patchwork::wrap_plots(
  fits$rent_main,
  fits$sales_main,
  ncol = 2
) +
  patchwork::plot_annotation(
    title = "Listed rents and home sale prices at ward boundaries"
  ) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

placebo_plot <- patchwork::wrap_plots(
  fits$rent_placebo_neg1000ft,
  fits$rent_placebo_pos1000ft,
  fits$sales_placebo_neg1000ft,
  fits$sales_placebo_pos1000ft,
  ncol = 2
) +
  patchwork::plot_annotation(
    title = "Shifted-cutoff price placebos"
  ) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

straight_plot <- patchwork::wrap_plots(
  fits$rent_straight,
  fits$sales_straight,
  ncol = 2
) +
  patchwork::plot_annotation(
    title = "Price estimates near locally straight ward boundaries"
  ) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

donut_plot <- patchwork::wrap_plots(
  fits$rent_donut25ft,
  fits$rent_donut50ft,
  fits$sales_donut25ft,
  fits$sales_donut50ft,
  ncol = 2
) +
  patchwork::plot_annotation(
    title = "Price estimates excluding observations nearest the boundary"
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
