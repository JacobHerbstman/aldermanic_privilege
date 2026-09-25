# setwd("tasks/price_boundary_results/code")
start_year <- 2006L
end_year <- 2022L
# RentHub listings are used from 2014 onward.
rent_start_year <- 2014L
bandwidth_ft <- 500L
bin_width_ft <- 100L
placebo_ft <- 1000L
donut_inner_ft <- 25L
donut_outer_ft <- 50L
# Property-type fixed effects (building type for rents, Assessor class for sales) are added to these controls.
rent_controls <- "log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft"
sales_controls <- "log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft"
rent_fixed_effects <- "segment_id^year_month"
sales_fixed_effects <- "segment_id^year_quarter"
cluster <- "ward_pair"
stopifnot(start_year <= end_year, bandwidth_ft %% bin_width_ft == 0)

source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")

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
    dplyr::filter(abs(signed_dist_ft) < bandwidth_ft) |>
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
    dplyr::filter(abs(signed_dist_ft) < bandwidth_ft) |>
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

straightness <- boundary_straightness(location_sf, boundaries)
location_classification <- location_sf |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    straight_boundary = straightness$straight_boundary,
    distance_error_ft = abs(straightness$reconstructed_distance_ft - expected_distance_ft)
  ) |>
  dplyr::select(dplyr::all_of(location_keys), straight_boundary, distance_error_ft)

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

# Estimate one distance-bin model and its average-difference model, and plot the bins.
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
  controls <- c(
    strsplit(if (market == "rent") rent_controls else sales_controls, " + ", fixed = TRUE)[[1]],
    if (market == "rent") "building_type_factor" else "property_class_factor"
  )
  outcome <- if (market == "rent") "rent_price" else "sale_price"
  fixed_effects <- if (market == "rent") {
    rent_fixed_effects
  } else {
    sales_fixed_effects
  }

  bin_edges <- seq(-bandwidth_ft, bandwidth_ft, by = bin_width_ft)
  bin_labels <- sprintf("bin_%02d", seq_len(length(bin_edges) - 1L))
  reference_bin <- bin_labels[bandwidth_ft / bin_width_ft]

  model_data <- data |>
    dplyr::mutate(
      running_distance_ft = signed_dist_ft - cutoff_ft,
      distance_bin = cut(
        running_distance_ft,
        breaks = bin_edges,
        labels = bin_labels,
        include.lowest = TRUE,
        right = FALSE
      )
    ) |>
    dplyr::filter(
      abs(running_distance_ft) < bandwidth_ft,
      donut_ft == 0 | abs(running_distance_ft) >= donut_ft,
      !straight_only | straight_boundary,
      !is.na(distance_bin)
    )

  formula <- stats::as.formula(sprintf(
    "log(%s) ~ i(distance_bin, ref = '%s') + %s | %s",
    outcome,
    reference_bin,
    paste(controls, collapse = " + "),
    fixed_effects
  ))
  model <- fixest::feols(
    formula,
    data = model_data,
    cluster = stats::as.formula(paste("~", cluster)),
    warn = FALSE,
    notes = FALSE
  )
  # Average difference: one indicator for the side at or right of the (placebo) cutoff in place of the distance bins.
  model_data$right_of_cutoff <- as.integer(model_data$running_distance_ft >= 0)
  average <- fixest::coeftable(fixest::feols(
    stats::as.formula(sprintf(
      "log(%s) ~ right_of_cutoff + %s | %s",
      outcome,
      paste(controls, collapse = " + "),
      fixed_effects
    )),
    data = model_data,
    cluster = stats::as.formula(paste("~", cluster)),
    warn = FALSE,
    notes = FALSE
  ))["right_of_cutoff", ]

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

  cluster_count <- dplyr::n_distinct(model_data[[cluster]])
  critical_value <- stats::qt(0.975, df = cluster_count - 1L)
  results <- tibble::tibble(
    distance_bin = bin_labels,
    bin_start_ft = head(bin_edges, -1),
    bin_end_ft = tail(bin_edges, -1),
    bin_center_ft = head(bin_edges, -1) + bin_width_ft / 2
  ) |>
    dplyr::left_join(
      estimates,
      by = "distance_bin",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      estimate = dplyr::if_else(
        distance_bin == reference_bin,
        0,
        estimate
      ),
      std_error = dplyr::if_else(
        distance_bin == reference_bin,
        NA_real_,
        std_error
      ),
      p_value = dplyr::if_else(
        distance_bin == reference_bin,
        NA_real_,
        p_value
      ),
      ci_low = estimate - critical_value * std_error,
      ci_high = estimate + critical_value * std_error,
      ribbon_low = dplyr::if_else(
        distance_bin == reference_bin,
        0,
        ci_low
      ),
      ribbon_high = dplyr::if_else(
        distance_bin == reference_bin,
        0,
        ci_high
      ),
      side_label = dplyr::case_when(
        cutoff_ft == 0 & bin_center_ft < 0 ~ "Less Stringent",
        cutoff_ft == 0 ~ "More Stringent",
        bin_center_ft < 0 ~ "Left of cutoff",
        TRUE ~ "Right of cutoff"
      )
    )

  nearest_above <- results |>
    dplyr::filter(bin_start_ft == 0)
  nearest_stars <- star_string(nearest_above$p_value)

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
        "Left of cutoff" = "#2478B5",
        "Right of cutoff" = "#D92D27"
      ),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Less Stringent" = "#2478B5",
        "More Stringent" = "#D92D27",
        "Left of cutoff" = "#2478B5",
        "Right of cutoff" = "#D92D27"
      ),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-bandwidth_ft, bandwidth_ft),
      breaks = seq(-bandwidth_ft, bandwidth_ft, length.out = 5)
    ) +
    ggplot2::labs(
      title = panel_title,
      subtitle = sprintf(
        "%s = %.3f%s (SE %.3f)\nAverage difference = %.3f%s (SE %.3f)",
        if (cutoff_ft == 0) {
          "Difference across boundary"
        } else {
          "Difference at placebo cutoff"
        },
        round(nearest_above$estimate, 3) + 0,
        nearest_stars,
        nearest_above$std_error,
        round(average[["Estimate"]], 3) + 0,
        star_string(average[["Pr(>|t|)"]]),
        average[["Std. Error"]]
      ),
      x = if (cutoff_ft == 0) {
        "Distance to ward boundary (feet)"
      } else {
        "Distance to placebo cutoff (feet)"
      },
      y = if (market == "rent") {
        "Rent (log difference)"
      } else {
        "Sale price (log difference)"
      }
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )

  plot
}

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
    title = "Prices at artificial boundaries"
  ) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

straight_plot <- patchwork::wrap_plots(
  fits$rent_straight,
  fits$sales_straight,
  ncol = 2
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
