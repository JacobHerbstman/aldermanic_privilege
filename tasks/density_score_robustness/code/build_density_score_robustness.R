# setwd("tasks/density_score_robustness/code")
workers <- 8L                    # parallel workers for the leave-one-out scores
gap_thresholds <- c(0.25, 0.50)  # score gaps defining the boundary samples

source("../../shared/code/alderman_uncertainty_helpers.R")
source("../../shared/code/density_boundary_helpers.R")

config <- default_uncertainty_config()
permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv") |>
  dplyr::mutate(
    id = as.character(id),
    ward = as.character(ward)
  ) |>
  dplyr::filter(month <= zoo::as.yearmon(as.Date("2022-12-01")))

prepared <- prepare_uncertainty_sample(
  permits,
  include_porch = config$include_porch,
  volume_ctrl = config$volume_ctrl,
  volume_stage = config$volume_stage
)
stage1 <- fit_stage1_model(
  permits = prepared$permits,
  stage1_outcome = "log_processing_time",
  covariates = get_stage1_covariates(
    prepared$place_covariates,
    prepared$include_volume_stage1,
    prepared$volume_var,
    drop_covariates = "share_bach_plus"
  ),
  fe_terms = get_stage1_fe_terms(config),
  variant_id = "paper"
)
stage1_permits <- stage1$permits_for_reg |>
  dplyr::mutate(id = as.character(id))

fit_score <- function(stage1_data) {
  build_two_stage_index(
    permits_for_reg = stage1_data,
    include_volume_stage2 = prepared$include_volume_stage2,
    volume_var = prepared$volume_var,
    stage2_weight = config$stage2_weight
  )$alderman_index |>
    dplyr::select(alderman, score = uncertainty_index)
}

baseline_score <- fit_score(stage1_permits)
published_score <- readr::read_csv(
  "../input/alderman_uncertainty_index_through2022.csv",
  show_col_types = FALSE
) |>
  dplyr::select(alderman, published_score = uncertainty_index)
score_validation <- baseline_score |>
  dplyr::inner_join(published_score, by = "alderman", relationship = "one-to-one") |>
  dplyr::mutate(difference = score - published_score)
if (
  nrow(score_validation) != nrow(baseline_score) ||
    max(abs(score_validation$difference)) > 1e-10
) {
  stop("Reconstructed scores do not match the scores used in the paper.", call. = FALSE)
}

buildings <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE,
  col_types = density_column_types
)
if (anyDuplicated(buildings$building_id) > 0L) {
  stop("New-construction data must be unique by building ID.", call. = FALSE)
}
buildings <- buildings |>
  density_analysis_sample() |>
  dplyr::filter(
    abs(distance_to_boundary_ft) < density_bandwidth_ft,
    !is.na(alderman_own),
    !is.na(alderman_neighbor)
  )

own_scores <- baseline_score |>
  dplyr::rename(alderman_own = alderman, baseline_score_own = score)
neighbor_scores <- baseline_score |>
  dplyr::rename(alderman_neighbor = alderman, baseline_score_neighbor = score)
buildings <- buildings |>
  dplyr::left_join(own_scores, by = "alderman_own", relationship = "many-to-one") |>
  dplyr::left_join(neighbor_scores, by = "alderman_neighbor", relationship = "many-to-one")
if (any(!is.finite(buildings$baseline_score_own)) || any(!is.finite(buildings$baseline_score_neighbor))) {
  stop("Current scores are missing for a construction-sample alderman.", call. = FALSE)
}

# The building's own permits (their city permit ids), removed when its aldermen's scores are re-estimated.
building_permits <- buildings |>
  dplyr::filter(!is.na(permit_ids)) |>
  dplyr::select(building_id, permit_id = permit_ids) |>
  tidyr::separate_longer_delim(permit_id, "/") |>
  dplyr::filter(permit_id %in% stage1_permits$id) |>
  dplyr::distinct(building_id, permit_id)
permit_ids_by_building <- split(building_permits$permit_id, building_permits$building_id)

fit_building_leaveout <- function(building_id) {
  building <- buildings[buildings$building_id == building_id, ]
  leaveout_score <- fit_score(
    stage1_permits |>
      dplyr::filter(!id %in% permit_ids_by_building[[building_id]])
  ) |>
    tibble::deframe()
  tibble::tibble(
    building_id,
    leaveout_score_own = unname(leaveout_score[building$alderman_own]),
    leaveout_score_neighbor = unname(leaveout_score[building$alderman_neighbor])
  )
}

fixest::setFixest_nthreads(1)
physical_cores <- parallel::detectCores(logical = FALSE)
if (!is.finite(physical_cores) || physical_cores < 1L) {
  physical_cores <- workers
}
leaveout_scores <- parallel::mclapply(
  names(permit_ids_by_building),
  fit_building_leaveout,
  mc.cores = max(1L, min(workers, physical_cores)),
  mc.preschedule = TRUE
) |>
  dplyr::bind_rows()

# Buildings without linked permits keep the full-sample scores.
buildings <- buildings |>
  dplyr::left_join(leaveout_scores, by = "building_id", relationship = "one-to-one") |>
  dplyr::mutate(
    leaveout_score_own = dplyr::coalesce(leaveout_score_own, baseline_score_own),
    leaveout_score_neighbor = dplyr::coalesce(leaveout_score_neighbor, baseline_score_neighbor),
    baseline_gap = abs(baseline_score_own - baseline_score_neighbor)
  )
if (any(!is.finite(buildings$leaveout_score_own)) || any(!is.finite(buildings$leaveout_score_neighbor))) {
  stop("Building-specific scores are missing an endpoint.", call. = FALSE)
}

score_versions <- list(
  current_score = buildings |>
    dplyr::mutate(score_own = baseline_score_own, score_neighbor = baseline_score_neighbor),
  building_leaveout = buildings |>
    dplyr::mutate(score_own = leaveout_score_own, score_neighbor = leaveout_score_neighbor)
)
for (threshold in gap_thresholds) {
  score_versions[[sprintf("gap_%0.2f", threshold)]] <- buildings |>
    dplyr::filter(baseline_gap >= threshold) |>
    dplyr::mutate(score_own = baseline_score_own, score_neighbor = baseline_score_neighbor)
}
version_labels <- c(
  current_score = "Full-sample score",
  building_leaveout = "Score excluding the building's permits",
  stats::setNames(sprintf("Score difference at least %.2f SD", gap_thresholds), sprintf("gap_%0.2f", gap_thresholds))
)
sample_labels <- c(all = "All Construction", multifamily = "Multifamily", multifamily_5plus = "Multifamily, 5+ Units")

estimate_row <- function(label, estimates) {
  c(
    paste0("\\quad ", label, " & ",
      paste0(sprintf("%.3f", estimates$estimate), stars(estimates$p_value), collapse = " & "), " \\\\"),
    paste0(" & ", paste0("(", sprintf("%.3f", estimates$std_error), ")", collapse = " & "), " \\\\")
  )
}

# Each score version signs the distance by which alderman has the higher score.
version_rows <- function(version) {
  scored <- score_versions[[version]] |>
    dplyr::mutate(
      running_distance_ft = abs(distance_to_boundary_ft) * sign(score_own - score_neighbor)
    ) |>
    dplyr::filter(score_own != score_neighbor) |>
    bin_running_distance()
  samples <- lapply(density_samples$sample, function(sample) filter_density_sample(scored, sample))
  fits <- lapply(samples, fit_density_boundary)
  c(
    paste0("\\textit{", version_labels[[version]], "} & & & \\\\"),
    estimate_row("Difference across boundary", dplyr::bind_rows(lapply(fits, `[[`, "first_bin"))),
    estimate_row("Average difference", dplyr::bind_rows(lapply(fits, `[[`, "average"))),
    paste0("\\quad Observations & ",
      paste(format(sapply(fits, `[[`, "observations"), big.mark = ",", trim = TRUE), collapse = " & "), " \\\\"),
    paste0("\\quad Ward pairs/segments & ",
      paste(sapply(samples, function(x) paste0(dplyr::n_distinct(x$ward_pair), "/", dplyr::n_distinct(x$segment_id))),
        collapse = " & "), " \\\\")
  )
}

table_lines <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  paste0(" & ", paste(sample_labels[density_samples$sample], collapse = " & "), " \\\\"),
  "\\midrule",
  "\\multicolumn{4}{l}{\\textbf{Panel A: Removing permits linked to each building}} \\\\",
  version_rows("current_score"), "\\addlinespace",
  version_rows("building_leaveout"), "\\addlinespace",
  "\\multicolumn{4}{l}{\\textbf{Panel B: Excluding boundaries with similar scores}} \\\\"
)
for (threshold in gap_thresholds) {
  table_lines <- c(table_lines, version_rows(sprintf("gap_%0.2f", threshold)),
    if (threshold == tail(gap_thresholds, 1)) "\\bottomrule" else "\\addlinespace")
}
writeLines(c(table_lines, "\\end{tabular}"), "../output/density_score_robustness.tex")
