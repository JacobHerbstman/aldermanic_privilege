# setwd("tasks/density_main_results/code")
source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
# Summarize the citywide construction sample.
source("../../shared/code/canonical_geometry_helpers.R")
projects <- readr::read_csv("../input/preferred_new_construction_project_ledger.csv", show_col_types = FALSE,
  col_types = readr::cols(class_values = "c", component_pins = "c", .default = readr::col_guess()))
boundaries <- readr::read_csv("../input/preferred_new_construction_boundary_scope.csv", show_col_types = FALSE,
  col_types = readr::cols(ward_pair = "c", .default = readr::col_guess()))
stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(boundaries$project_id),
  setequal(projects$project_id, boundaries$project_id))
projects <- projects |>
  left_join(boundaries |> select(project_id, era, ward_pair, distance_to_boundary_ft),
    by = "project_id", relationship = "one-to-one")
stopifnot(!any(is.na(projects$external_multifamily) & (projects$allow_far | projects$allow_dupac)))

# Check that the descriptive subgroup agrees with the existing analysis classification.
analysis <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE)
check <- projects |> select(project_id, external_multifamily) |>
  inner_join(analysis |> select(project_id, analysis_multifamily = external_multifamily),
    by = "project_id", relationship = "one-to-one")
stopifnot(nrow(check) == nrow(analysis), identical(check$external_multifamily, check$analysis_multifamily))
common <- projects |> filter(construction_year >= 2006L, construction_year <= 2022L,
  allow_far, allow_dupac, is.finite(far), far > 0, is.finite(dupac), dupac > 0,
  is.finite(dwelling_units), dwelling_units > 0)
stopifnot(nrow(common) > 0, !anyNA(common$external_multifamily),
  all(is.finite(common$distance_to_boundary_ft)), all(common$distance_to_boundary_ft >= 0))

# Count nearest boundary segments across the city, without limiting distance.
points <- sf::st_as_sf(common, coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)
segments <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg",
  eras = sort(unique(common$era)))
common$segment_id <- NA_character_
pair_dash <- normalize_pair_dash(common$ward_pair)
stopifnot(!anyNA(common$era), !anyNA(pair_dash))
for (era in names(segments)) {
  for (pair in sort(unique(pair_dash[common$era == era]))) {
    rows <- which(common$era == era & pair_dash == pair)
    boundary <- segments[[era]][segments[[era]]$pair_dash == pair, ]
    stopifnot(nrow(boundary) > 0, sf::st_crs(boundary) == sf::st_crs(points))
    common$segment_id[rows] <- boundary$segment_id[sf::st_nearest_feature(points[rows, ], boundary)]
  }
}
stopifnot(!anyNA(common$segment_id))
segment_check <- common |> select(project_id, segment_id) |>
  inner_join(analysis |> filter(!is.na(segment_id)) |>
    select(project_id, analysis_segment_id = segment_id), by = "project_id", relationship = "one-to-one")
stopifnot(all(segment_check$segment_id == segment_check$analysis_segment_id))

summaries <- bind_rows(common |> mutate(sample = "All New Construction"),
  common |> filter(external_multifamily) |> mutate(sample = "Multifamily")) |>
  summarise(mean_far = mean(far), average_units = mean(dwelling_units), mean_dupac = mean(dupac),
    median_distance = median(distance_to_boundary_ft), ward_pairs = n_distinct(ward_pair),
    segments = n_distinct(segment_id), n = n(), .by = sample)
SaveData(summaries, c("sample"), "../output/density_sample_summary.csv")

# Format the paper table.
summaries <- read.csv("../output/density_sample_summary.csv")
summary_lines <- c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{New Residential Construction in Chicago, 2006--2022}",
  "\\label{tab:summary_stats}", "\\begin{tabular}{lcc}", "\\toprule",
  " & All New Construction & Multifamily \\\\", "\\midrule")
for (field in c("mean_far", "average_units", "mean_dupac", "median_distance", "ward_segments", "n")) {
  label <- switch(field, mean_far = "Average FAR", average_units = "Average Units per Project",
    mean_dupac = "Average DUPAC", median_distance = "Median Distance to Boundary (ft)",
    ward_segments = "Ward Pairs / Boundary Segments", n = "Number of Projects")
  values <- if (field == "n") format(summaries$n, big.mark = ",", trim = TRUE) else
    if (field == "ward_segments") paste0(summaries$ward_pairs, "/", summaries$segments) else
    if (field == "median_distance") sprintf("%.0f", summaries[[field]]) else sprintf("%.2f", summaries[[field]])
  if (field == "n") summary_lines <- c(summary_lines, "\\midrule")
  summary_lines <- c(summary_lines, paste0(label, " & ", paste(values, collapse = " & "), " \\\\"))
}
writeLines(c(summary_lines, "\\bottomrule", "\\end{tabular}",
  "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: The table summarizes projects with recorded building area, lot area, and dwelling units. FAR is building area divided by lot area. DUPAC is dwelling units per acre. Multifamily refers to buildings containing two or more dwelling units.}",
  "\\end{table}"), "../output/density_sample_summary.tex")
