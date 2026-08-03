# setwd("tasks/audits/score_vintage_reflection/code")
# workers <- 8

source("../../../_lib/alderman_uncertainty_helpers.R")
library(arrow)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- workers
}
if (length(cli_args) != 1) {
  stop("Script requires the number of workers.", call. = FALSE)
}
workers <- as.integer(cli_args[1])
if (!is.finite(workers) || workers < 1) {
  stop("workers must be a positive integer.", call. = FALSE)
}

config <- default_uncertainty_config()
permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv") |>
  dplyr::mutate(
    id = as.character(id),
    ward = as.character(ward),
    year = as.integer(year)
  ) |>
  dplyr::filter(month <= zoo::as.yearmon(as.Date("2022-12-01")))

prepare_stage1 <- function(data) {
  prepared <- prepare_uncertainty_sample(
    data,
    include_porch = config$include_porch,
    volume_ctrl = config$volume_ctrl,
    volume_stage = config$volume_stage
  )
  covariates <- get_stage1_covariates(
    prepared$place_covariates,
    prepared$include_volume_stage1,
    prepared$volume_var,
    drop_covariates = "share_bach_plus"
  )
  stage1 <- fit_stage1_model(
    permits = prepared$permits,
    stage1_outcome = "log_processing_time",
    covariates = covariates,
    fe_terms = get_stage1_fe_terms(config),
    variant_id = "paper"
  )
  list(prepared = prepared, stage1 = stage1)
}

fit_score_from_residuals <- function(stage1_data, prepared) {
  build_two_stage_index(
    permits_for_reg = stage1_data,
    include_volume_stage2 = prepared$include_volume_stage2,
    volume_var = prepared$volume_var,
    stage2_weight = config$stage2_weight
  )$alderman_index |>
    dplyr::select(alderman, score = uncertainty_index)
}

full_stage <- prepare_stage1(permits)
full_stage1_data <- full_stage$stage1$permits_for_reg |>
  dplyr::mutate(id = as.character(id), ward = as.character(ward))
baseline_score <- fit_score_from_residuals(full_stage1_data, full_stage$prepared)

published_2022 <- readr::read_csv(
  "../input/alderman_uncertainty_index_through2022.csv",
  show_col_types = FALSE
) |>
  dplyr::transmute(alderman, published_score = uncertainty_index)
baseline_validation <- baseline_score |>
  dplyr::inner_join(published_2022, by = "alderman", relationship = "one-to-one") |>
  dplyr::mutate(difference = score - published_score)
if (
  nrow(baseline_validation) != nrow(baseline_score) ||
    max(abs(baseline_validation$difference)) > 1e-10
) {
  stop("Reconstructed through-2022 score does not match the paper score.", call. = FALSE)
}

score_2006_2014 <- build_residualized_uncertainty_index(
  permits = permits |> dplyr::filter(year <= 2014L),
  config = config,
  variant_id = "2006_2014",
  stage1_outcome = "log_processing_time",
  drop_covariates = "share_bach_plus"
)$alderman_index |>
  dplyr::select(alderman, n_permits_2006_2014 = n_permits, score_2006_2014 = uncertainty_index)

published_2014 <- readr::read_csv(
  "../input/alderman_uncertainty_index_through2014.csv",
  show_col_types = FALSE
) |>
  dplyr::transmute(alderman, published_score_2006_2014 = uncertainty_index)
validation_2014 <- score_2006_2014 |>
  dplyr::inner_join(published_2014, by = "alderman", relationship = "one-to-one") |>
  dplyr::mutate(difference = score_2006_2014 - published_score_2006_2014)
if (
  nrow(validation_2014) != nrow(score_2006_2014) ||
    max(abs(validation_2014$difference)) > 1e-10
) {
  stop("Reconstructed through-2014 score does not match the archived score.", call. = FALSE)
}

score_2015_2022 <- build_residualized_uncertainty_index(
  permits = permits |> dplyr::filter(year >= 2015L),
  config = config,
  variant_id = "2015_2022",
  stage1_outcome = "log_processing_time",
  drop_covariates = "share_bach_plus"
)$alderman_index |>
  dplyr::select(alderman, n_permits_2015_2022 = n_permits, score_2015_2022 = uncertainty_index)

score_vintage_comparison <- score_2006_2014 |>
  dplyr::inner_join(score_2015_2022, by = "alderman", relationship = "one-to-one") |>
  dplyr::mutate(
    rank_2006_2014 = rank(score_2006_2014),
    rank_2015_2022 = rank(score_2015_2022),
    rank_change = rank_2015_2022 - rank_2006_2014
  ) |>
  dplyr::arrange(dplyr::desc(abs(rank_change)))

score_stability_summary <- dplyr::bind_rows(
  score_vintage_comparison |>
    dplyr::summarise(
      sample = "all_common_aldermen",
      n_aldermen = dplyr::n(),
      pearson_correlation = stats::cor(score_2006_2014, score_2015_2022),
      spearman_correlation = stats::cor(score_2006_2014, score_2015_2022, method = "spearman")
    ),
  score_vintage_comparison |>
    dplyr::filter(n_permits_2006_2014 >= 100, n_permits_2015_2022 >= 100) |>
    dplyr::summarise(
      sample = "at_least_100_permits_each_period",
      n_aldermen = dplyr::n(),
      pearson_correlation = stats::cor(score_2006_2014, score_2015_2022),
      spearman_correlation = stats::cor(score_2006_2014, score_2015_2022, method = "spearman")
    ),
  score_vintage_comparison |>
    dplyr::filter(n_permits_2006_2014 >= 250, n_permits_2015_2022 >= 250) |>
    dplyr::summarise(
      sample = "at_least_250_permits_each_period",
      n_aldermen = dplyr::n(),
      pearson_correlation = stats::cor(score_2006_2014, score_2015_2022),
      spearman_correlation = stats::cor(score_2006_2014, score_2015_2022, method = "spearman")
    )
)

construction <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    ward = readr::col_character(),
    neighbor_ward = readr::col_character(),
    .default = readr::col_guess()
  )
)
if (anyDuplicated(construction$project_id)) {
  stop("Construction input is not unique by project ID.", call. = FALSE)
}

project_endpoints <- construction |>
  dplyr::filter(
    within_500ft,
    construction_year >= 2006L,
    construction_year <= 2022L,
    !is.na(alderman_own),
    !is.na(alderman_neighbor)
  ) |>
  dplyr::select(project_id, alderman_own, alderman_neighbor) |>
  dplyr::distinct()

project_permit_ids <- readr::read_csv(
  "../adjudication/final_project_permit_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::semi_join(project_endpoints, by = "project_id") |>
  dplyr::filter(permit_id %in% full_stage1_data$id) |>
  dplyr::distinct(project_id, permit_id)

permit_ids_by_project <- split(
  project_permit_ids$permit_id,
  project_permit_ids$project_id
)
baseline_vector <- stats::setNames(baseline_score$score, baseline_score$alderman)

setFixest_nthreads(1)
physical_cores <- parallel::detectCores(logical = FALSE)
if (!is.finite(physical_cores) || physical_cores < 1) {
  physical_cores <- workers
}
workers <- max(1L, min(workers, physical_cores))

fit_project_leaveout <- function(project_id) {
  endpoint <- project_endpoints[project_endpoints$project_id == project_id, ]
  omitted_ids <- permit_ids_by_project[[project_id]]
  if (is.null(omitted_ids) || length(omitted_ids) == 0L) {
    return(tibble::tibble(
      project_id,
      omitted_permits = 0L,
      score_own = unname(baseline_vector[endpoint$alderman_own]),
      score_neighbor = unname(baseline_vector[endpoint$alderman_neighbor])
    ))
  }
  leaveout_score <- fit_score_from_residuals(
    full_stage1_data |> dplyr::filter(!id %in% omitted_ids),
    full_stage$prepared
  ) |>
    tibble::deframe()
  tibble::tibble(
    project_id,
    omitted_permits = length(omitted_ids),
    score_own = unname(leaveout_score[endpoint$alderman_own]),
    score_neighbor = unname(leaveout_score[endpoint$alderman_neighbor])
  )
}

project_leaveout_scores <- parallel::mclapply(
  project_endpoints$project_id,
  fit_project_leaveout,
  mc.cores = workers,
  mc.preschedule = TRUE
) |>
  dplyr::bind_rows()

rent_contexts <- arrow::read_parquet(
  "../input/rental_rd_characteristics_panel_bw1500.parquet",
  col_select = c(
    "file_date", "ward", "neighbor_ward", "alderman_own",
    "alderman_neighbor", "signed_dist"
  )
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    year = lubridate::year(as.Date(file_date)),
    ward = as.character(ward),
    neighbor_ward = as.character(neighbor_ward),
    distance_ft = abs(as.numeric(signed_dist))
  ) |>
  dplyr::filter(year >= 2014L, year <= 2022L, distance_ft < 500)

sales_contexts <- arrow::read_parquet(
  "../input/sales_with_hedonics_amenities.parquet",
  col_select = c(
    "sale_date", "ward", "neighbor_ward", "alderman_own",
    "alderman_neighbor", "signed_dist_m"
  )
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    year = lubridate::year(as.Date(sale_date)),
    ward = as.character(ward),
    neighbor_ward = as.character(neighbor_ward),
    distance_ft = abs(as.numeric(signed_dist_m)) / 0.3048
  ) |>
  dplyr::filter(year >= 2006L, year <= 2022L, distance_ft < 500)

construction_contexts <- construction |>
  dplyr::transmute(
    year = as.integer(construction_year),
    ward = as.character(ward),
    neighbor_ward = as.character(neighbor_ward),
    alderman_own,
    alderman_neighbor,
    distance_ft = distance_to_boundary_ft
  ) |>
  dplyr::filter(year >= 2006L, year <= 2022L, distance_ft < 500)

ward_year_contexts <- dplyr::bind_rows(
  construction_contexts,
  rent_contexts |> dplyr::select(dplyr::all_of(names(construction_contexts))),
  sales_contexts |> dplyr::select(dplyr::all_of(names(construction_contexts)))
) |>
  dplyr::filter(
    !is.na(ward),
    !is.na(neighbor_ward),
    alderman_own %in% baseline_score$alderman,
    alderman_neighbor %in% baseline_score$alderman
  ) |>
  dplyr::distinct(year, ward, neighbor_ward, alderman_own, alderman_neighbor) |>
  dplyr::mutate(
    context_id = paste(
      year, ward, neighbor_ward, alderman_own, alderman_neighbor,
      sep = "|"
    )
  )

fit_ward_year_leaveout <- function(i) {
  context <- ward_year_contexts[i, ]
  omit <- full_stage1_data$year == context$year &
    full_stage1_data$ward %in% c(context$ward, context$neighbor_ward)
  leaveout_score <- fit_score_from_residuals(
    full_stage1_data[!omit, ],
    full_stage$prepared
  ) |>
    tibble::deframe()
  tibble::tibble(
    context_id = context$context_id,
    omitted_permits = sum(omit),
    score_own = unname(leaveout_score[context$alderman_own]),
    score_neighbor = unname(leaveout_score[context$alderman_neighbor])
  )
}

ward_year_leaveout_scores <- parallel::mclapply(
  seq_len(nrow(ward_year_contexts)),
  fit_ward_year_leaveout,
  mc.cores = workers,
  mc.preschedule = TRUE
) |>
  dplyr::bind_rows()

if (
  anyDuplicated(project_leaveout_scores$project_id) ||
    anyDuplicated(ward_year_leaveout_scores$context_id)
) {
  stop("Leave-out score output has duplicate keys.", call. = FALSE)
}

readr::write_csv(score_vintage_comparison, "../output/score_vintage_comparison.csv")
readr::write_csv(score_stability_summary, "../output/score_stability_summary.csv")
readr::write_csv(
  baseline_validation |>
    dplyr::summarise(
      n_aldermen = dplyr::n(),
      maximum_absolute_difference = max(abs(difference)),
      n_stage1_permits = nrow(full_stage1_data),
      n_projects = nrow(project_endpoints),
      n_projects_with_score_permits = dplyr::n_distinct(project_permit_ids$project_id),
      n_score_permits_linked_to_projects = dplyr::n_distinct(project_permit_ids$permit_id),
      n_project_leaveout_missing_endpoint = sum(
        !stats::complete.cases(
          project_leaveout_scores[c("score_own", "score_neighbor")]
        )
      ),
      n_ward_year_contexts = nrow(ward_year_contexts),
      n_ward_year_leaveout_missing_endpoint = sum(
        !stats::complete.cases(
          ward_year_leaveout_scores[c("score_own", "score_neighbor")]
        )
      )
    ),
  "../output/leaveout_score_diagnostics.csv"
)
saveRDS(
  list(
    baseline_score = baseline_score,
    score_2006_2014 = score_2006_2014,
    score_2015_2022 = score_2015_2022,
    project_leaveout_scores = project_leaveout_scores,
    ward_year_contexts = ward_year_contexts,
    ward_year_leaveout_scores = ward_year_leaveout_scores
  ),
  "../output/score_leaveout_data.rds"
)
