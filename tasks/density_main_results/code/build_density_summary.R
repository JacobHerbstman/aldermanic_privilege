# setwd("tasks/density_main_results/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/canonical_geometry_helpers.R")

# Summarize the citywide construction sample: every building with a measured dwelling count and land area (DUPAC),
# completed 2006-2022.
buildings <- readr::read_csv("../input/permit_construction.csv", show_col_types = FALSE,
  col_types = readr::cols(building_id = "c", permit_number = "c", member_permit_numbers = "c", record_ids = "c",
    ward_pair = "c", .default = readr::col_guess()))
stopifnot(!anyDuplicated(buildings$building_id))
analysis <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = readr::cols(building_id = "c", segment_id = "c", ward_pair = "c", .default = readr::col_guess()))
common <- buildings |> filter(construction_year >= 2006L, construction_year <= 2022L,
  allow_dupac, is.finite(dupac), dupac > 0, is.finite(dwelling_units), dwelling_units > 0)
stopifnot(nrow(common) > 0, !anyNA(common$multifamily),
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
segment_check <- common |> select(building_id, segment_id) |>
  inner_join(analysis |> filter(!is.na(segment_id)) |>
    select(building_id, analysis_segment_id = segment_id), by = "building_id", relationship = "one-to-one")
stopifnot(all(segment_check$segment_id == segment_check$analysis_segment_id))

summaries <- bind_rows(common |> mutate(sample = "All New Construction"),
  common |> filter(multifamily) |> mutate(sample = "Multifamily"),
  common |> filter(multifamily, dwelling_units >= 5) |> mutate(sample = "Multifamily, 5+ Units")) |>
  summarise(average_units = mean(dwelling_units), mean_dupac = mean(dupac),
    median_distance = median(distance_to_boundary_ft), ward_pairs = n_distinct(ward_pair),
    segments = n_distinct(segment_id), n = n(), .by = sample)
SaveData(summaries, c("sample"), "../output/density_sample_summary.csv")

# Format the paper table.
summary_lines <- c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{New Residential Construction in Chicago, 2006--2022}",
  "\\label{tab:summary_stats}", "\\begin{tabular}{lccc}", "\\toprule",
  " & All New Construction & Multifamily & Multifamily, 5+ Units \\\\", "\\midrule")
for (field in c("average_units", "mean_dupac", "median_distance", "ward_segments", "n")) {
  label <- switch(field, average_units = "Average Units per Building",
    mean_dupac = "Average DUPAC", median_distance = "Median Distance to Boundary (ft)",
    ward_segments = "Ward Pairs / Boundary Segments", n = "Number of Buildings")
  values <- if (field == "n") format(summaries$n, big.mark = ",", trim = TRUE) else
    if (field == "ward_segments") paste0(summaries$ward_pairs, "/", summaries$segments) else
    if (field == "median_distance") sprintf("%.0f", summaries[[field]]) else sprintf("%.2f", summaries[[field]])
  if (field == "n") summary_lines <- c(summary_lines, "\\midrule")
  summary_lines <- c(summary_lines, paste0(label, " & ", paste(values, collapse = " & "), " \\\\"))
}
writeLines(c(summary_lines, "\\bottomrule", "\\end{tabular}",
  "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: One row per new residential building with a recorded dwelling count and land area, including condominium buildings; townhouses are counted one home at a time. DUPAC is dwelling units per acre. Multifamily buildings contain two or more dwelling units and are not single-family or townhouse homes.}",
  "\\end{table}"), "../output/density_sample_summary.tex")
