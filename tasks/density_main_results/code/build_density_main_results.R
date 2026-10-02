# setwd("tasks/density_main_results/code")
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

# signed_distance_m is positive on the more-stringent side of the boundary.
buildings <- buildings |>
  density_analysis_sample() |>
  dplyr::mutate(running_distance_ft = signed_distance_m / 0.3048) |>
  bin_running_distance()

panels <- list()
for (i in seq_len(nrow(density_samples))) {
  fit <- fit_density_boundary(filter_density_sample(buildings, density_samples$sample[i]))
  panels[[i]] <- plot_density_boundary(fit, density_samples$label[i])
}
save_density_figure(panels, "../output/density_rd.pdf")
