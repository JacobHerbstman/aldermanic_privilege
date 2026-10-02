# setwd("tasks/presentation_figures/code")
# Slide versions of the paper's density boundary-geometry and location-continuity tables
# (tasks/density_boundary_checks), reporting the average difference: the main specification from
# tasks/shared/code/density_boundary_helpers.R; continuity outcomes use segment x joint-service fixed effects only.
source("../../setup_environment/code/packages.R")
source("../../shared/code/density_boundary_helpers.R")

characteristics <- readr::read_csv("../input/density_boundary_characteristics.csv", show_col_types = FALSE,
  col_types = readr::cols(building_id = readr::col_character(), .default = readr::col_guess()))
buildings <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample() |>
  dplyr::mutate(running_distance_ft = signed_distance_m / 0.3048) |>
  bin_running_distance() |>
  dplyr::left_join(characteristics, by = "building_id", relationship = "one-to-one")
stopifnot(!anyDuplicated(buildings$building_id), !anyNA(buildings$straight_boundary))

header <- c("\\begin{tabular}{lccc}", "\\toprule", " & All Construction & Multifamily & Multifamily, 5+ Units \\\\", "\\midrule")
average_rows <- function(label, fits) {
  c(paste0(label, " & ", paste0(sprintf("%.3f", round(sapply(fits, function(f) f$average$estimate), 3) + 0),
      stars(sapply(fits, function(f) f$average$p_value)), collapse = " & "), " \\\\"),
    paste0(" & ", paste0("(", sprintf("%.3f", sapply(fits, function(f) f$average$std_error)), ")", collapse = " & "), " \\\\"))
}

restrictions <- tibble::tribble(
  ~label, ~keep,
  "Main sample", "all",
  "Limited expressway or water overlap", "simple_overlap_keep",
  "Limited physical-feature or arterial overlap", "share_based_keep",
  "Straight boundary segment", "straight_boundary"
)
lines <- header
for (r in seq_len(nrow(restrictions))) {
  restricted <- if (restrictions$keep[r] == "all") buildings else buildings[buildings[[restrictions$keep[r]]], ]
  fits <- lapply(density_samples$sample, function(s) fit_density_boundary(filter_density_sample(restricted, s)))
  lines <- c(lines, average_rows(restrictions$label[r], fits),
    paste0("\\quad Observations & ", paste(format(sapply(fits, `[[`, "observations"), big.mark = ",", trim = TRUE),
      collapse = " & "), " \\\\"),
    if (r < nrow(restrictions)) "\\addlinespace")
}
writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), "../output/density_boundary_robustness_slides.tex")

measures <- c("Distance to downtown (miles)" = "distance_to_cbd_miles", "Distance to nearest school (miles)" = "distance_to_school_miles",
  "Distance to nearest park (miles)" = "distance_to_park_miles", "Distance to Lake Michigan (miles)" = "distance_to_lake_miles")
lines <- header
for (m in names(measures)) {
  fits <- lapply(density_samples$sample, function(s) {
    fit_density_boundary(filter_density_sample(buildings, s), outcome = measures[[m]], controls = NULL,
      fixed_effects = "segment_id^joint_service")
  })
  lines <- c(lines, average_rows(m, fits))
}
observations <- sapply(density_samples$sample, function(s) nrow(filter_density_sample(buildings, s)))
lines <- c(lines, "\\midrule",
  paste0("Observations & ", paste(format(observations, big.mark = ",", trim = TRUE), collapse = " & "), " \\\\"))
writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), "../output/density_location_continuity_slides.tex")
