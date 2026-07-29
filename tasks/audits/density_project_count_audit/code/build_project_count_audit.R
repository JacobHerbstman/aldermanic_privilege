# setwd("tasks/audits/density_project_count_audit/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    ward_pair = readr::col_character(),
    segment_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(projects$project_id) > 0L) {
  stop("New-construction data must be unique by project ID.")
}

segments_2003 <- sf::st_read(
  "../input/boundary_segments_1320ft.gpkg",
  layer = "2003_2014",
  quiet = TRUE
) |>
  sf::st_drop_geometry()

segments_2015 <- sf::st_read(
  "../input/boundary_segments_1320ft.gpkg",
  layer = "2015_2023",
  quiet = TRUE
) |>
  sf::st_drop_geometry()

segment_metadata <- dplyr::bind_rows(
  segments_2003,
  segments_2015
) |>
  dplyr::filter(
    valid_segment,
    !is.na(analysis_segment_id),
    analysis_segment_id != "",
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) |>
  dplyr::summarise(
    ward_pair = dplyr::first(ward_pair_id),
    era = dplyr::first(era),
    segment_length_ft = sum(segment_length_ft),
    n_ward_pairs = dplyr::n_distinct(ward_pair_id),
    n_eras = dplyr::n_distinct(era),
    .by = analysis_segment_id
  )

if (
  any(segment_metadata$n_ward_pairs != 1L) ||
    any(segment_metadata$n_eras != 1L)
) {
  stop("An analysis segment maps to multiple ward pairs or eras.")
}

segment_years <- segment_metadata |>
  dplyr::transmute(
    segment_id = analysis_segment_id,
    ward_pair,
    segment_length_ft,
    year_start = dplyr::if_else(era == "2003_2014", 2006L, 2015L),
    year_end = dplyr::if_else(era == "2003_2014", 2014L, 2022L)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    construction_year = list(seq.int(year_start, year_end))
  ) |>
  tidyr::unnest(construction_year) |>
  dplyr::ungroup() |>
  dplyr::select(
    segment_id,
    ward_pair,
    segment_length_ft,
    construction_year
  )

distance_bins <- tibble::tibble(
  distance_bin = factor(
    sprintf("bin_%02d", 1:10),
    levels = sprintf("bin_%02d", 1:10)
  ),
  bin_start_ft = seq(-500, 400, by = 100),
  bin_end_ft = seq(-400, 500, by = 100),
  bin_center_ft = seq(-450, 450, by = 100),
  more_stringent = as.integer(bin_start_ft >= 0)
)

project_sample <- projects |>
  dplyr::filter(
    construction_year >= 2006L,
    construction_year <= 2022L,
    dwelling_units > 0,
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != "",
    is.finite(signed_distance_m),
    abs(signed_distance_m / 0.3048) < 500,
    is.finite(pair_average_score)
  ) |>
  dplyr::mutate(
    running_distance_ft = signed_distance_m / 0.3048,
    distance_bin = cut(
      running_distance_ft,
      breaks = seq(-500, 500, by = 100),
      labels = sprintf("bin_%02d", 1:10),
      include.lowest = TRUE,
      right = FALSE
    )
  ) |>
  dplyr::filter(!is.na(distance_bin))

missing_segments <- setdiff(
  unique(project_sample$segment_id),
  unique(segment_years$segment_id)
)
if (length(missing_segments) > 0L) {
  stop("A project uses a segment absent from the segment universe.")
}

sample_definitions <- tibble::tribble(
  ~sample, ~multifamily_only,
  "All construction", FALSE,
  "Multifamily", TRUE
)

model_rows <- list()
band_rows <- list()
plot_rows <- list()

for (sample_index in seq_len(nrow(sample_definitions))) {
  sample_name <- sample_definitions$sample[sample_index]
  multifamily_only <- sample_definitions$multifamily_only[sample_index]

  sample_projects <- project_sample
  if (multifamily_only) {
    sample_projects <- sample_projects |>
      dplyr::filter(external_multifamily)
  }

  observed_counts <- sample_projects |>
    dplyr::count(
      segment_id,
      construction_year,
      distance_bin,
      name = "project_count"
    )

  count_panel <- tidyr::crossing(
    segment_years,
    distance_bin = distance_bins$distance_bin
  ) |>
    dplyr::left_join(
      observed_counts,
      by = c(
        "segment_id",
        "construction_year",
        "distance_bin"
      ),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      project_count = dplyr::coalesce(project_count, 0L)
    ) |>
    dplyr::left_join(
      distance_bins,
      by = "distance_bin",
      relationship = "many-to-one"
    )

  band_model <- fixest::fepois(
    project_count ~ i(distance_bin, ref = "bin_05") |
      segment_id^construction_year,
    data = count_panel,
    cluster = ~ward_pair,
    warn = FALSE,
    notes = FALSE
  )

  side_panel <- count_panel |>
    dplyr::summarise(
      project_count = sum(project_count),
      .by = c(
        segment_id,
        ward_pair,
        construction_year,
        more_stringent
      )
    )

  full_side_model <- fixest::fepois(
    project_count ~ more_stringent |
      segment_id^construction_year,
    data = side_panel,
    cluster = ~ward_pair,
    warn = FALSE,
    notes = FALSE
  )

  nearest_panel <- count_panel |>
    dplyr::filter(distance_bin %in% c("bin_05", "bin_06")) |>
    dplyr::mutate(
      more_stringent = as.integer(distance_bin == "bin_06")
    )

  nearest_model <- fixest::fepois(
    project_count ~ more_stringent |
      segment_id^construction_year,
    data = nearest_panel,
    cluster = ~ward_pair,
    warn = FALSE,
    notes = FALSE
  )

  for (model_name in c("Nearest 100ft", "Full 500ft")) {
    model <- if (model_name == "Nearest 100ft") {
      nearest_model
    } else {
      full_side_model
    }
    coefficient_table <- fixest::coeftable(model)
    estimate <- coefficient_table["more_stringent", "Estimate"]
    standard_error <- coefficient_table["more_stringent", "Std. Error"]
    p_value <- coefficient_table["more_stringent", "Pr(>|z|)"]

    model_rows[[length(model_rows) + 1L]] <- tibble::tibble(
      sample = sample_name,
      comparison = model_name,
      estimate,
      standard_error,
      p_value,
      percent_difference = 100 * (exp(estimate) - 1),
      n_projects = nrow(sample_projects),
      n_ward_pairs = dplyr::n_distinct(sample_projects$ward_pair),
      n_segments = dplyr::n_distinct(sample_projects$segment_id)
    )
  }

  coefficient_table <- fixest::coeftable(band_model)
  coefficient_rows <- grepl(
    "^distance_bin::",
    rownames(coefficient_table)
  )
  band_estimates <- tibble::tibble(
    distance_bin = sub(
      "^distance_bin::",
      "",
      rownames(coefficient_table)[coefficient_rows]
    ),
    estimate = coefficient_table[coefficient_rows, "Estimate"],
    standard_error = coefficient_table[coefficient_rows, "Std. Error"],
    p_value = coefficient_table[coefficient_rows, "Pr(>|z|)"]
  )

  cluster_count <- dplyr::n_distinct(sample_projects$ward_pair)
  critical_value <- stats::qt(0.975, df = cluster_count - 1L)
  plot_data <- distance_bins |>
    dplyr::mutate(distance_bin = as.character(distance_bin)) |>
    dplyr::left_join(
      band_estimates,
      by = "distance_bin",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      estimate = dplyr::if_else(
        distance_bin == "bin_05",
        0,
        estimate
      ),
      standard_error = dplyr::if_else(
        distance_bin == "bin_05",
        NA_real_,
        standard_error
      ),
      p_value = dplyr::if_else(
        distance_bin == "bin_05",
        NA_real_,
        p_value
      ),
      percent_difference = 100 * (exp(estimate) - 1),
      ci_low = 100 * (
        exp(estimate - critical_value * standard_error) - 1
      ),
      ci_high = 100 * (
        exp(estimate + critical_value * standard_error) - 1
      ),
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
      side = dplyr::if_else(
        bin_center_ft < 0,
        "Less Stringent",
        "More Stringent"
      ),
      sample = sample_name
    )

  plot_rows[[sample_index]] <- plot_data

  raw_band_counts <- sample_projects |>
    dplyr::count(distance_bin, name = "project_count") |>
    dplyr::mutate(distance_bin = as.character(distance_bin))

  band_rows[[sample_index]] <- distance_bins |>
    dplyr::mutate(distance_bin = as.character(distance_bin)) |>
    dplyr::left_join(
      raw_band_counts,
      by = "distance_bin",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      project_count = dplyr::coalesce(project_count, 0L),
      sample = sample_name
    )
}

model_results <- dplyr::bind_rows(model_rows)
readr::write_csv(
  model_results,
  "../output/project_count_models.csv"
)

readr::write_csv(
  dplyr::bind_rows(band_rows),
  "../output/project_counts_by_band.csv"
)

plot_data <- dplyr::bind_rows(plot_rows)
count_plot <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(
    x = bin_center_ft,
    y = percent_difference,
    color = side,
    fill = side,
    group = side
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
    ggplot2::aes(ymin = ribbon_low, ymax = ribbon_high),
    alpha = 0.16,
    color = NA
  ) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::geom_point(size = 2.3) +
  ggplot2::facet_wrap(~sample, ncol = 2, scales = "free_y") +
  ggplot2::scale_color_manual(
    values = c(
      "Less Stringent" = "#2478B5",
      "More Stringent" = "#D92D27"
    ),
    name = NULL
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Less Stringent" = "#2478B5",
      "More Stringent" = "#D92D27"
    ),
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    limits = c(-500, 500),
    breaks = c(-500, -250, 0, 250, 500)
  ) +
  ggplot2::labs(
    x = "Distance to ward boundary (feet)",
    y = "Difference in completed-project count (%)"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    legend.position = "bottom",
    strip.text = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  "../output/project_count_by_band.pdf",
  count_plot,
  width = 10,
  height = 5.8,
  bg = "white"
)
