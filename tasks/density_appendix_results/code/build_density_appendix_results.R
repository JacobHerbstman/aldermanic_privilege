# setwd("tasks/density_appendix_results/code")
# Placebo cutoffs 1,000 ft inside either side of the boundary, and donuts excluding buildings within 25 or 50 ft.
placebo_ft <- 1000L
donut_inner_ft <- 25L
donut_outer_ft <- 50L

source("../../setup_environment/code/packages.R")
source("../../shared/code/density_boundary_helpers.R")

buildings <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE,
  col_types = density_column_types
)
if (anyDuplicated(buildings$building_id) > 0L) {
  stop("New-construction data must be unique by building ID.")
}
buildings <- density_analysis_sample(buildings)

checks <- tibble::tribble(
  ~check, ~cutoff_ft, ~donut_ft,
  sprintf("placebo_neg%dft", placebo_ft), -placebo_ft, 0L,
  sprintf("placebo_pos%dft", placebo_ft), placebo_ft, 0L,
  sprintf("donut%dft", donut_inner_ft), 0L, donut_inner_ft,
  sprintf("donut%dft", donut_outer_ft), 0L, donut_outer_ft
)

for (check_i in seq_len(nrow(checks))) {
  cutoff_ft <- checks$cutoff_ft[check_i]
  # The running distance is measured from the (placebo) cutoff; signed_distance_m is positive on the more-stringent
  # side of the true boundary.
  check_data <- buildings |>
    dplyr::mutate(running_distance_ft = signed_distance_m / 0.3048 - cutoff_ft) |>
    bin_running_distance() |>
    dplyr::filter(abs(running_distance_ft) >= checks$donut_ft[check_i])

  panels <- list()
  for (i in seq_len(nrow(density_samples))) {
    fit <- fit_density_boundary(filter_density_sample(check_data, density_samples$sample[i]))
    panels[[i]] <- plot_density_boundary(fit, density_samples$label[i], cutoff_ft)
  }
  save_density_figure(panels, sprintf("../output/density_%s.pdf", checks$check[check_i]))
}
