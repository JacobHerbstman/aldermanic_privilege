# setwd("tasks/density_boundary_checks/code")
# The location-continuity checks compare buildings on the same segment and joint service, without zoning.
continuity_fixed_effects <- "segment_id^joint_service"

source("../../setup_environment/code/packages.R")
source("../../shared/code/density_boundary_helpers.R")

buildings <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE,
  col_types = density_column_types
)
boundary_characteristics <- readr::read_csv(
  "../input/density_boundary_characteristics.csv",
  show_col_types = FALSE,
  col_types = readr::cols(building_id = readr::col_character(), .default = readr::col_guess())
)
if (anyDuplicated(buildings$building_id) > 0L || anyDuplicated(boundary_characteristics$building_id) > 0L) {
  stop("New-construction data and boundary characteristics must be unique by building ID.")
}

# signed_distance_m is positive on the more-stringent side of the boundary.
buildings <- buildings |>
  density_analysis_sample() |>
  dplyr::mutate(running_distance_ft = signed_distance_m / 0.3048) |>
  bin_running_distance() |>
  dplyr::left_join(boundary_characteristics, by = "building_id", relationship = "one-to-one")
if (anyNA(buildings$straight_boundary) || anyNA(buildings$simple_overlap_keep) || anyNA(buildings$share_based_keep)) {
  stop("Some buildings lack a boundary classification.")
}

sample_labels <- c(all = "All Construction", multifamily = "Multifamily", multifamily_5plus = "Multifamily, 5+ Units")
table_header <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  paste0(" & ", paste(sample_labels[density_samples$sample], collapse = " & "), " \\\\"),
  "\\midrule"
)
estimate_row <- function(label, estimates) {
  c(
    paste0(label, " & ", paste0(sprintf("%.3f", estimates$estimate), stars(estimates$p_value), collapse = " & "),
      " \\\\"),
    paste0(" & ", paste0("(", sprintf("%.3f", estimates$std_error), ")", collapse = " & "), " \\\\")
  )
}

# Density estimates on boundary segments without substantial expressway or water overlap, without substantial
# physical-feature or arterial overlap, and on locally straight boundaries.
restrictions <- tibble::tribble(
  ~label, ~keep,
  "Main sample", "all",
  "Limited expressway or water overlap", "simple_overlap_keep",
  "Limited physical-feature or arterial overlap", "share_based_keep",
  "Straight boundary segment", "straight_boundary"
)
robustness_lines <- table_header
for (r in seq_len(nrow(restrictions))) {
  restricted <- if (restrictions$keep[r] == "all") buildings else buildings[buildings[[restrictions$keep[r]]], ]
  fits <- lapply(density_samples$sample, function(sample) {
    fit_density_boundary(filter_density_sample(restricted, sample))
  })
  robustness_lines <- c(
    robustness_lines,
    paste0("\\textit{", restrictions$label[r], "} & & & \\\\"),
    estimate_row("\\quad Difference across boundary", dplyr::bind_rows(lapply(fits, `[[`, "first_bin"))),
    estimate_row("\\quad Average difference", dplyr::bind_rows(lapply(fits, `[[`, "average"))),
    paste0("\\quad Observations & ",
      paste(format(sapply(fits, `[[`, "observations"), big.mark = ",", trim = TRUE), collapse = " & "), " \\\\"),
    if (r < nrow(restrictions)) "\\addlinespace"
  )
}
writeLines(c(robustness_lines, "\\bottomrule", "\\end{tabular}"), "../output/density_boundary_robustness.tex")

# Location characteristics that should not change at the boundary: the first-band difference in each.
location_measures <- tibble::tribble(
  ~variable, ~label,
  "distance_to_cbd_miles", "Distance to downtown (miles)",
  "distance_to_school_miles", "Distance to nearest school (miles)",
  "distance_to_park_miles", "Distance to nearest park (miles)",
  "distance_to_lake_miles", "Distance to Lake Michigan (miles)"
)
continuity_lines <- table_header
for (m in seq_len(nrow(location_measures))) {
  fits <- lapply(density_samples$sample, function(sample) {
    fit_density_boundary(
      filter_density_sample(buildings, sample),
      outcome = location_measures$variable[m],
      controls = NULL,
      fixed_effects = continuity_fixed_effects
    )
  })
  continuity_lines <- c(
    continuity_lines,
    estimate_row(location_measures$label[m], dplyr::bind_rows(lapply(fits, `[[`, "first_bin")))
  )
}
observations <- sapply(density_samples$sample, function(sample) nrow(filter_density_sample(buildings, sample)))
writeLines(
  c(
    continuity_lines,
    "\\midrule",
    paste0("Observations & ", paste(format(observations, big.mark = ",", trim = TRUE), collapse = " & "), " \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  ),
  "../output/density_location_continuity.tex"
)
