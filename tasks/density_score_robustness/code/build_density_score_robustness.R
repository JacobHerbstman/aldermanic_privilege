# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/density_score_robustness/code")
# workers <- 8
# gap_thresholds <- c(0.25, 0.50)

source("../../_lib/alderman_uncertainty_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(workers, paste(gap_thresholds, collapse = ","))
}
if (length(cli_args) != 2) {
  stop("Script requires the number of workers and comma-separated score-gap thresholds.", call. = FALSE)
}

workers <- as.integer(cli_args[1])
gap_thresholds <- as.numeric(strsplit(cli_args[2], ",", fixed = TRUE)[[1]])
if (!is.finite(workers) || workers < 1L) {
  stop("workers must be a positive integer.", call. = FALSE)
}
if (length(gap_thresholds) == 0L || any(!is.finite(gap_thresholds)) || any(gap_thresholds <= 0)) {
  stop("Score-gap thresholds must be positive numbers.", call. = FALSE)
}

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
  stop("New-construction data must be unique by project ID.", call. = FALSE)
}

projects <- projects |>
  dplyr::filter(
    construction_year >= 2006L,
    construction_year <= 2022L,
    within_500ft,
    dwelling_units > 0,
    allow_far,
    allow_dupac,
    is.finite(density_far),
    density_far > 0,
    is.finite(density_dupac),
    density_dupac > 0,
    is.finite(pair_average_score),
    is.finite(share_white_own),
    is.finite(share_black_own),
    is.finite(median_hh_income_own),
    is.finite(share_bach_plus_own),
    is.finite(homeownership_rate_own),
    !is.na(zone_group),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != "",
    !is.na(alderman_own),
    !is.na(alderman_neighbor),
    abs(distance_to_boundary_ft) < 500
  )

own_scores <- baseline_score |>
  dplyr::rename(alderman_own = alderman, baseline_score_own = score)
neighbor_scores <- baseline_score |>
  dplyr::rename(alderman_neighbor = alderman, baseline_score_neighbor = score)
projects <- projects |>
  dplyr::left_join(own_scores, by = "alderman_own", relationship = "many-to-one") |>
  dplyr::left_join(neighbor_scores, by = "alderman_neighbor", relationship = "many-to-one")

if (
  any(!is.finite(projects$baseline_score_own)) ||
    any(!is.finite(projects$baseline_score_neighbor)) ||
    max(abs(projects$strictness_own - projects$baseline_score_own)) > 1e-10 ||
    max(abs(projects$strictness_neighbor - projects$baseline_score_neighbor)) > 1e-10
) {
  stop("Construction data do not contain the current through-2022 scores.", call. = FALSE)
}
if (!identical(c(nrow(projects), sum(projects$external_multifamily)), c(3692L, 822L))) {
  stop("The baseline density samples do not match the paper.", call. = FALSE)
}

project_permits <- readr::read_csv(
  "../adjudication/project_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character()
  )
)
if (anyDuplicated(project_permits[c("project_id", "permit_id")]) > 0L) {
  stop("Project-permit matches must be unique by project and permit ID.", call. = FALSE)
}

project_permits <- project_permits |>
  dplyr::semi_join(projects, by = "project_id") |>
  dplyr::filter(permit_id %in% stage1_permits$id) |>
  dplyr::distinct(project_id, permit_id)
permit_ids_by_project <- split(project_permits$permit_id, project_permits$project_id)
linked_project_ids <- intersect(names(permit_ids_by_project), projects$project_id)

project_endpoints <- projects |>
  dplyr::select(project_id, alderman_own, alderman_neighbor)

fit_project_leaveout <- function(project_id) {
  endpoint <- project_endpoints[project_endpoints$project_id == project_id, ]
  leaveout_vector <- fit_score(
    stage1_permits |>
      dplyr::filter(!id %in% permit_ids_by_project[[project_id]])
  ) |>
    tibble::deframe()
  tibble::tibble(
    project_id,
    leaveout_score_own = unname(leaveout_vector[endpoint$alderman_own]),
    leaveout_score_neighbor = unname(leaveout_vector[endpoint$alderman_neighbor])
  )
}

fixest::setFixest_nthreads(1)
physical_cores <- parallel::detectCores(logical = FALSE)
if (!is.finite(physical_cores) || physical_cores < 1L) {
  physical_cores <- workers
}
workers <- max(1L, min(workers, physical_cores))

leaveout_scores <- parallel::mclapply(
  linked_project_ids,
  fit_project_leaveout,
  mc.cores = workers,
  mc.preschedule = TRUE
) |>
  dplyr::bind_rows()

projects <- projects |>
  dplyr::left_join(leaveout_scores, by = "project_id", relationship = "one-to-one") |>
  dplyr::mutate(
    leaveout_score_own = dplyr::coalesce(leaveout_score_own, baseline_score_own),
    leaveout_score_neighbor = dplyr::coalesce(
      leaveout_score_neighbor,
      baseline_score_neighbor
    ),
    baseline_gap = abs(baseline_score_own - baseline_score_neighbor)
  )
if (
  any(!is.finite(projects$leaveout_score_own)) ||
    any(!is.finite(projects$leaveout_score_neighbor))
) {
  stop("Project-specific scores are missing an endpoint.", call. = FALSE)
}

score_versions <- list(
  current_score = projects |>
    dplyr::mutate(
      score_own = baseline_score_own,
      score_neighbor = baseline_score_neighbor
    ),
  project_leaveout = projects |>
    dplyr::mutate(
      score_own = leaveout_score_own,
      score_neighbor = leaveout_score_neighbor
    )
)
for (threshold in gap_thresholds) {
  score_versions[[sprintf("gap_%0.2f", threshold)]] <- projects |>
    dplyr::filter(baseline_gap >= threshold) |>
    dplyr::mutate(
      score_own = baseline_score_own,
      score_neighbor = baseline_score_neighbor
    )
}

version_labels <- c(
  current_score = "Current score",
  project_leaveout = "Score excluding the project's permits",
  stats::setNames(
    sprintf("Minimum score gap: %.2f SD", gap_thresholds),
    sprintf("gap_%0.2f", gap_thresholds)
  )
)
sample_labels <- c(all = "All Construction", multifamily = "Multifamily")
outcome_labels <- c(density_far = "Log(FAR)", density_dupac = "Log(DUPAC)")

results <- list()
result_i <- 0L
for (version in names(score_versions)) {
  scored_projects <- score_versions[[version]] |>
    dplyr::mutate(
      score_sign = dplyr::case_when(
        score_own > score_neighbor ~ 1,
        score_own < score_neighbor ~ -1,
        TRUE ~ NA_real_
      ),
      running_distance_ft = abs(distance_to_boundary_ft) * score_sign,
      pair_average_score_model = (score_own + score_neighbor) / 2,
      distance_bin = cut(
        running_distance_ft,
        breaks = seq(-500, 500, by = 100),
        labels = sprintf("bin_%02d", 1:10),
        include.lowest = TRUE,
        right = FALSE
      )
    ) |>
    dplyr::filter(!is.na(distance_bin), is.finite(pair_average_score_model))

  for (sample in names(sample_labels)) {
    model_sample <- scored_projects |>
      dplyr::filter(.env$sample == "all" | external_multifamily)

    for (outcome in names(outcome_labels)) {
      model_data <- model_sample |>
        dplyr::mutate(log_outcome = log(.data[[outcome]]))
      model <- fixest::feols(
        log_outcome ~
          i(distance_bin, ref = "bin_05") +
          pair_average_score_model +
          share_white_own +
          share_black_own +
          median_hh_income_own +
          share_bach_plus_own +
          homeownership_rate_own |
          zone_group + segment_id + construction_year,
        data = model_data,
        cluster = ~ward_pair,
        warn = FALSE,
        notes = FALSE
      )
      coefficient_table <- fixest::coeftable(model)
      result_i <- result_i + 1L
      results[[result_i]] <- tibble::tibble(
        version,
        sample,
        outcome,
        estimate = unname(coefficient_table["distance_bin::bin_06", "Estimate"]),
        std_error = unname(coefficient_table["distance_bin::bin_06", "Std. Error"]),
        p_value = unname(coefficient_table["distance_bin::bin_06", "Pr(>|t|)"]),
        n = stats::nobs(model),
        ward_pairs = dplyr::n_distinct(model_data$ward_pair),
        segments = dplyr::n_distinct(model_data$segment_id)
      )
    }
  }
}
results <- dplyr::bind_rows(results)

format_decimal <- function(value) {
  sub("^(-?)0\\.", "\\1.", sprintf("%.3f", value))
}
format_estimate <- function(estimate, p_value) {
  stars <- dplyr::case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
  paste0(format_decimal(estimate), stars)
}

column_order <- tidyr::expand_grid(
  sample = c("all", "multifamily"),
  outcome = c("density_far", "density_dupac")
)
table_lines <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{All Construction} & \\multicolumn{2}{c}{Multifamily} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & Log(FAR) & Log(DUPAC) & Log(FAR) & Log(DUPAC) \\\\",
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel A: Removing permits linked to each project}} \\\\"
)

for (version in c("current_score", "project_leaveout")) {
  rows <- column_order |>
    dplyr::left_join(
      results |>
        dplyr::filter(.data$version == .env$version),
      by = c("sample", "outcome"),
      relationship = "one-to-one"
    )
  table_lines <- c(
    table_lines,
    paste0(
      version_labels[[version]], " & ",
      paste(format_estimate(rows$estimate, rows$p_value), collapse = " & "),
      " \\\\"
    ),
    paste0(
      " & ",
      paste(paste0("(", format_decimal(rows$std_error), ")"), collapse = " & "),
      " \\\\"
    ),
    paste0(
      "N & ",
      paste(format(rows$n, big.mark = ",", scientific = FALSE), collapse = " & "),
      " \\\\"
    ),
    paste0(
      "Ward pairs/segments & ",
      paste(paste0(rows$ward_pairs, "/", rows$segments), collapse = " & "),
      " \\\\"
    ),
    "\\addlinespace"
  )
}

table_lines <- c(
  table_lines,
  "\\multicolumn{5}{l}{\\textit{Panel B: Excluding boundaries with similar scores}} \\\\"
)
for (threshold in gap_thresholds) {
  version <- sprintf("gap_%0.2f", threshold)
  rows <- column_order |>
    dplyr::left_join(
      results |>
        dplyr::filter(.data$version == .env$version),
      by = c("sample", "outcome"),
      relationship = "one-to-one"
    )
  table_lines <- c(
    table_lines,
    paste0(
      version_labels[[version]], " & ",
      paste(format_estimate(rows$estimate, rows$p_value), collapse = " & "),
      " \\\\"
    ),
    paste0(
      " & ",
      paste(paste0("(", format_decimal(rows$std_error), ")"), collapse = " & "),
      " \\\\"
    ),
    paste0(
      "N & ",
      paste(format(rows$n, big.mark = ",", scientific = FALSE), collapse = " & "),
      " \\\\"
    ),
    paste0(
      "Ward pairs/segments & ",
      paste(paste0(rows$ward_pairs, "/", rows$segments), collapse = " & "),
      " \\\\"
    ),
    if (threshold == tail(gap_thresholds, 1)) "\\bottomrule" else "\\addlinespace"
  )
}
table_lines <- c(table_lines, "\\end{tabular}")

writeLines(table_lines, "../output/density_score_robustness.tex")
