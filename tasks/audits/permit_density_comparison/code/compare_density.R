# setwd("tasks/audits/permit_density_comparison/code")
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# bin_width_ft <- 100
# controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
# fixed_effects <- "zone_group + segment_id + construction_year"
# cluster <- "ward_pair"
# segment_window_ft <- 1500
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("../../../shared/code/canonical_geometry_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(start_year, end_year, bandwidth_ft, bin_width_ft, controls, fixed_effects, cluster, segment_window_ft)
stopifnot(length(args) == 8L)
start_year <- as.integer(args[1])
end_year <- as.integer(args[2])
bandwidth_ft <- as.numeric(args[3])
bin_width_ft <- as.numeric(args[4])
controls <- args[5]
fixed_effects <- args[6]
cluster <- args[7]
segment_window_ft <- as.numeric(args[8])

# One zoning rule for every dataset: the official snapshot in effect before construction.
zone_group <- function(code) {
  code <- str_to_upper(code)
  case_when(
    is.na(code) | str_trim(code) == "" ~ NA_character_,
    str_detect(code, "^RS-?") ~ "Single-Family Residential",
    str_detect(code, "^(RT|RM)-?") ~ "Multi-Family Residential",
    str_detect(code, "^B-?[1-7]-") ~ "Neighborhood Mixed-Use",
    str_detect(code, "^C-?[1-7]-") ~ "Commercial",
    str_detect(code, "^M-?[1-7]-") ~ "Industrial",
    str_detect(code, "^(DX|DR|DS|DC)-") ~ "Downtown",
    str_starts(code, "PD") ~ "Planned Development",
    str_starts(code, "PMD") ~ "Planned Manufacturing",
    str_starts(code, "POS") ~ "Open Space",
    TRUE ~ "Other")
}
zoning_maps <- list(
  `2006` = st_read("../input/historical_zoning_2006_candidate.gpkg", quiet = TRUE) |> transmute(zone = candidate_zone_group_2006),
  `2012` = st_read("/vsizip/../input/zoning_nov2012.zip/Zoning_nov2012.shp", quiet = TRUE) |> transmute(zone = zone_group(ZONE_CLASS)),
  `2014` = st_read("/vsizip/../input/zoning_sep2014.zip/Zoning.shp", quiet = TRUE) |> transmute(zone = zone_group(ZONE_CLASS)),
  `2016` = st_read("/vsizip/../input/zoning_jan2016.zip/zoning_2016_01.shp", quiet = TRUE) |> transmute(zone = zone_group(ZONE_CLASS))) |>
  map(\(x) st_transform(x, 3435))
preceding_zoning <- function(d) {
  snapshot <- case_when(d$construction_year <= 2012 ~ "2006", d$construction_year <= 2014 ~ "2012",
    d$construction_year == 2015 ~ "2014", TRUE ~ "2016")
  points <- st_as_sf(d, coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)
  zone <- rep(NA_character_, nrow(d))
  for (s in names(zoning_maps)) {
    rows <- which(snapshot == s)
    zone[rows] <- st_join(points[rows, "building_id"], zoning_maps[[s]], largest = TRUE)$zone
  }
  zone
}

# Regressors for the permit datasets, following the paper's analysis data: segment, aldermen on the construction
# date, their scores, and ward controls in the construction year.
segments <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg", eras = c("2003_2014", "2015_2023"))
terms <- read_csv("../input/chicago_alderman_terms.csv", col_types = cols(ward = "i", alderman = "c", start_date = "D", end_date = "D"))
scores <- read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE) |>
  select(alderman, score = uncertainty_index)
ward_controls <- read_csv("../input/ward_controls_2006_2022.csv", show_col_types = FALSE) |>
  select(ward, year, share_white, share_black, median_hh_income, share_bach_plus, homeownership_rate)
stopifnot(!anyDuplicated(scores$alderman), !anyDuplicated(ward_controls[c("ward", "year")]))
attach_regressors <- function(d) {
  d <- d |> filter(within_1500ft, construction_year >= start_year, construction_year <= end_year)
  points <- st_as_sf(d, coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)
  d$segment_id <- assign_points_to_nearest_segments(points, d$era, d$ward_pair, segments,
    max_distance = units::set_units(segment_window_ft, "ft"))
  d |>
    left_join(terms, by = join_by(ward, construction_date >= start_date, construction_date <= end_date),
      relationship = "many-to-one") |> rename(alderman_own = alderman) |> select(-start_date, -end_date) |>
    left_join(terms |> rename(alderman_neighbor = alderman),
      by = join_by(neighbor_ward == ward, construction_date >= start_date, construction_date <= end_date),
      relationship = "many-to-one") |> select(-start_date, -end_date) |>
    left_join(scores |> rename(alderman_own = alderman, strictness_own = score), by = "alderman_own", relationship = "many-to-one") |>
    left_join(scores |> rename(alderman_neighbor = alderman, strictness_neighbor = score), by = "alderman_neighbor",
      relationship = "many-to-one") |>
    left_join(ward_controls, by = c("ward", "construction_year" = "year"), relationship = "many-to-one") |>
    rename_with(\(x) paste0(x, "_own"), c(share_white, share_black, median_hh_income, share_bach_plus, homeownership_rate)) |>
    mutate(assigned = is.finite(strictness_own) & is.finite(strictness_neighbor) & strictness_own != strictness_neighbor,
      signed_distance_m = if_else(assigned, distance_to_boundary_ft * 0.3048 * sign(strictness_own - strictness_neighbor), NA_real_),
      density_eligible = allow_far & allow_dupac & far > 0 & dupac > 0)
}

read_permit_ledger <- function(path) {
  read_csv(path, col_types = cols(building_id = "c", ward_pair = "c", construction_date = "D", .default = col_guess())) |>
    attach_regressors()
}
paper <- read_csv("../input/new_construction_analysis_data.csv",
    col_types = cols(project_id = "c", ward_pair = "c", segment_id = "c", construction_date = "D", .default = col_guess())) |>
  rename(building_id = project_id, far = density_far, dupac = density_dupac, multifamily = external_multifamily)
datasets <- list(
  paper_published = paper,
  paper_snapshot_zoning = paper |> mutate(zone_group = preceding_zoning(paper)),
  permit_issue = read_permit_ledger("../input/permit_construction_permit_issue.csv"),
  assessor_year = read_permit_ledger("../input/permit_construction_assessor_year.csv"))
datasets$permit_issue$zone_group <- preceding_zoning(datasets$permit_issue)
datasets$assessor_year$zone_group <- preceding_zoning(datasets$assessor_year)

# The paper's specification: log density on 100 ft distance bands, omitting the nearest lenient-side band.
# Reported: the nearest stringent-side band, from 0 to 100 ft.
bin_edges <- seq(-bandwidth_ft, bandwidth_ft, by = bin_width_ft)
bin_labels <- sprintf("bin_%02d", seq_len(length(bin_edges) - 1L))
reference_bin <- bin_labels[bandwidth_ft / bin_width_ft]
reported_bin <- bin_labels[bandwidth_ft / bin_width_ft + 1L]
results <- expand_grid(dataset = names(datasets), sample = c("all", "multifamily"), outcome = c("far", "dupac")) |>
  pmap(\(dataset, sample, outcome) {
    model_data <- datasets[[dataset]] |>
      filter(construction_year >= start_year, construction_year <= end_year, density_eligible,
        sample == "all" | multifamily %in% TRUE, is.finite(signed_distance_m), abs(signed_distance_m / 0.3048) < bandwidth_ft,
        is.finite(share_white_own), is.finite(share_black_own), is.finite(median_hh_income_own),
        is.finite(share_bach_plus_own), is.finite(homeownership_rate_own),
        !is.na(zone_group), !is.na(segment_id), segment_id != "", !is.na(ward_pair)) |>
      mutate(log_outcome = log(.data[[outcome]]),
        distance_bin = cut(signed_distance_m / 0.3048, breaks = bin_edges, labels = bin_labels, include.lowest = TRUE, right = FALSE))
    model <- feols(as.formula(sprintf("log_outcome ~ i(distance_bin, ref = '%s') + %s | %s", reference_bin, controls, fixed_effects)),
      data = model_data, cluster = as.formula(paste("~", cluster)), warn = FALSE, notes = FALSE)
    band <- coeftable(model)[paste0("distance_bin::", reported_bin), ]
    tibble(dataset, sample, outcome, estimate = band[["Estimate"]], std_error = band[["Std. Error"]],
      p_value = band[["Pr(>|t|)"]], observations = nobs(model), ward_pairs = n_distinct(model_data$ward_pair),
      band_observations = sum(model_data$distance_bin == reported_bin))
  }) |> bind_rows()
SaveData(results, c("dataset", "sample", "outcome"), "../output/density_comparison.csv")
