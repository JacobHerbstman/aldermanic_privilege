# setwd("tasks/audits/score_vintage_reflection/code")

source("../../../setup_environment/code/packages.R")
setFixest_notes(FALSE)

regime_scores <- readRDS("../output/regime_scores.rds")
pooled_scores <- readRDS("../output/score_leaveout_data.rds")$baseline_score |>
  dplyr::select(alderman, score)

projects <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    ward = readr::col_character(),
    neighbor_ward = readr::col_character(),
    ward_pair = readr::col_character(),
    segment_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
if (anyDuplicated(projects$project_id)) {
  stop("Construction input is not unique by project ID.", call. = FALSE)
}

add_endpoint_scores <- function(data, scores, prefix) {
  own <- scores |>
    dplyr::transmute(alderman_own = alderman, !!paste0(prefix, "_own") := score)
  neighbor <- scores |>
    dplyr::transmute(
      alderman_neighbor = alderman,
      !!paste0(prefix, "_neighbor") := score
    )
  data |>
    dplyr::left_join(own, by = "alderman_own", relationship = "many-to-one") |>
    dplyr::left_join(
      neighbor,
      by = "alderman_neighbor",
      relationship = "many-to-one"
    )
}

projects <- projects |>
  add_endpoint_scores(pooled_scores, "pooled") |>
  add_endpoint_scores(
    regime_scores$early |> dplyr::select(alderman, score),
    "early"
  ) |>
  add_endpoint_scores(
    regime_scores$transition |> dplyr::select(alderman, score),
    "transition"
  ) |>
  dplyr::mutate(
    period = dplyr::case_when(
      construction_year <= 2014L ~ "2006_2014",
      construction_year <= 2019L ~ "2015_2019",
      TRUE ~ "2020_2022"
    ),
    dated_score_own = dplyr::if_else(
      construction_year <= 2014L,
      early_own,
      transition_own
    ),
    dated_score_neighbor = dplyr::if_else(
      construction_year <= 2014L,
      early_neighbor,
      transition_neighbor
    ),
    pooled_sign = sign(pooled_own - pooled_neighbor),
    dated_sign = sign(dated_score_own - dated_score_neighbor),
    pooled_distance_ft = abs(distance_to_boundary_ft) * pooled_sign,
    dated_distance_ft = abs(distance_to_boundary_ft) * dated_sign,
    pooled_pair_average = (pooled_own + pooled_neighbor) / 2,
    dated_pair_average = (dated_score_own + dated_score_neighbor) / 2,
    complete_pooled = is.finite(pooled_own) & is.finite(pooled_neighbor) & pooled_sign != 0,
    complete_dated = is.finite(dated_score_own) &
      is.finite(dated_score_neighbor) & dated_sign != 0,
    classification_flip = dplyr::if_else(
      complete_pooled & complete_dated,
      pooled_sign != dated_sign,
      NA
    )
  )

validated <- projects |>
  dplyr::filter(is.finite(strictness_own), is.finite(strictness_neighbor))
if (
  max(abs(validated$strictness_own - validated$pooled_own)) > 1e-10 ||
    max(abs(validated$strictness_neighbor - validated$pooled_neighbor)) > 1e-10
) {
  stop("Construction input does not contain the pooled through-2022 score.", call. = FALSE)
}

base_sample <- projects |>
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
    abs(distance_to_boundary_ft) < 500
  )

two_sided_segments <- base_sample |>
  dplyr::filter(complete_pooled) |>
  dplyr::summarise(
    two_sided = dplyr::n_distinct(ward) >= 2L,
    .by = segment_id
  ) |>
  dplyr::filter(two_sided) |>
  dplyr::pull(segment_id)

summarise_flips <- function(data, sample, period, support) {
  selected <- data |>
    dplyr::filter(
      .env$sample == "all" | external_multifamily,
      .env$period == "all_periods" | .data$period == .env$period,
      .env$support == "all_complete" |
        (.env$support == "two_sided_segments" & segment_id %in% two_sided_segments) |
        (.env$support == "nearest_100ft_two_sided" &
          segment_id %in% two_sided_segments &
          abs(distance_to_boundary_ft) < 100),
      complete_pooled,
      complete_dated
    )

  segment_means <- selected |>
    dplyr::summarise(
      flip_share = mean(classification_flip),
      .by = segment_id
    )

  tibble::tibble(
    sample,
    period,
    support,
    n_projects = nrow(selected),
    n_segments = dplyr::n_distinct(selected$segment_id),
    observation_weighted_flip_share = mean(selected$classification_flip),
    equal_segment_weighted_flip_share = mean(segment_means$flip_share)
  )
}

classification_results <- list()
i <- 0L
for (sample in c("all", "multifamily")) {
  for (period in c("all_periods", "2006_2014", "2015_2019", "2020_2022")) {
    for (support in c(
      "all_complete",
      "two_sided_segments",
      "nearest_100ft_two_sided"
    )) {
      i <- i + 1L
      classification_results[[i]] <- summarise_flips(
        base_sample,
        sample,
        period,
        support
      )
    }
  }
}
classification_stability <- dplyr::bind_rows(classification_results)

flip_contributors <- base_sample |>
  dplyr::filter(complete_pooled, complete_dated, classification_flip) |>
  dplyr::mutate(
    endpoint_1 = pmin(alderman_own, alderman_neighbor),
    endpoint_2 = pmax(alderman_own, alderman_neighbor),
    pooled_gap = abs(pooled_own - pooled_neighbor),
    dated_gap = abs(dated_score_own - dated_score_neighbor)
  ) |>
  dplyr::summarise(
    n_projects = dplyr::n(),
    n_multifamily = sum(external_multifamily),
    n_nearest_100ft = sum(abs(distance_to_boundary_ft) < 100),
    mean_pooled_gap = mean(pooled_gap),
    mean_dated_gap = mean(dated_gap),
    .by = c(period, endpoint_1, endpoint_2)
  ) |>
  dplyr::arrange(dplyr::desc(n_projects), dplyr::desc(n_multifamily))

sample_coverage <- dplyr::bind_rows(
  base_sample |>
    dplyr::summarise(
      sample = "all",
      n_projects = dplyr::n(),
      n_complete_pooled = sum(complete_pooled),
      n_complete_dated = sum(complete_dated),
      n_complete_both = sum(complete_pooled & complete_dated),
      dated_coverage_share = mean(complete_dated),
      .by = period
    ),
  base_sample |>
    dplyr::filter(external_multifamily) |>
    dplyr::summarise(
      sample = "multifamily",
      n_projects = dplyr::n(),
      n_complete_pooled = sum(complete_pooled),
      n_complete_dated = sum(complete_dated),
      n_complete_both = sum(complete_pooled & complete_dated),
      dated_coverage_share = mean(complete_dated),
      .by = period
    )
)

extract_result <- function(model, term, sample, outcome, score_method, estimator, data) {
  table <- fixest::coeftable(model)
  tibble::tibble(
    sample,
    outcome,
    score_method,
    estimator,
    estimate = unname(table[term, "Estimate"]),
    std_error = unname(table[term, "Std. Error"]),
    p_value = unname(table[term, "Pr(>|t|)"]),
    n = stats::nobs(model),
    ward_pairs = dplyr::n_distinct(data$ward_pair),
    segments = dplyr::n_distinct(data$segment_id)
  )
}

estimate_density <- function(data, sample, outcome, score_method, distance, pair_average) {
  model_data <- data |>
    dplyr::filter(
      .env$sample == "all" | external_multifamily,
      is.finite(.data[[distance]]),
      is.finite(.data[[pair_average]])
    ) |>
    dplyr::mutate(
      log_outcome = log(.data[[outcome]]),
      side = as.integer(.data[[distance]] >= 0),
      distance_bin = cut(
        .data[[distance]],
        breaks = seq(-500, 500, by = 100),
        labels = sprintf("bin_%02d", 1:10),
        include.lowest = TRUE,
        right = FALSE
      )
    ) |>
    dplyr::filter(!is.na(distance_bin))

  common_rhs <- paste(
    pair_average,
    "+ share_white_own + share_black_own + median_hh_income_own +",
    "share_bach_plus_own + homeownership_rate_own"
  )
  bin_model <- fixest::feols(
    stats::as.formula(paste0(
      "log_outcome ~ i(distance_bin, ref = 'bin_05') + ", common_rhs,
      " | zone_group + segment_id + construction_year"
    )),
    data = model_data,
    cluster = ~ward_pair,
    warn = FALSE,
    notes = FALSE
  )
  flat_model <- fixest::feols(
    stats::as.formula(paste0(
      "log_outcome ~ side + ", common_rhs,
      " | zone_group + segment_id + construction_year"
    )),
    data = model_data,
    cluster = ~ward_pair,
    warn = FALSE,
    notes = FALSE
  )

  dplyr::bind_rows(
    extract_result(
      bin_model,
      "distance_bin::bin_06",
      sample,
      outcome,
      score_method,
      "nearest_100ft_bin",
      model_data
    ),
    extract_result(
      flat_model,
      "side",
      sample,
      outcome,
      score_method,
      "full_500ft",
      model_data
    )
  )
}

common_sample <- base_sample |>
  dplyr::filter(complete_pooled, complete_dated)
model_versions <- list(
  pooled_full_sample = list(
    data = base_sample |> dplyr::filter(complete_pooled),
    distance = "pooled_distance_ft",
    pair_average = "pooled_pair_average"
  ),
  pooled_common_sample = list(
    data = common_sample,
    distance = "pooled_distance_ft",
    pair_average = "pooled_pair_average"
  ),
  dated_sign_pooled_average_common_sample = list(
    data = common_sample,
    distance = "dated_distance_ft",
    pair_average = "pooled_pair_average"
  ),
  dated_common_sample = list(
    data = common_sample,
    distance = "dated_distance_ft",
    pair_average = "dated_pair_average"
  ),
  pooled_prereform_full_sample = list(
    data = base_sample |>
      dplyr::filter(construction_year <= 2019L, complete_pooled),
    distance = "pooled_distance_ft",
    pair_average = "pooled_pair_average"
  ),
  pooled_prereform_common_sample = list(
    data = common_sample |>
      dplyr::filter(construction_year <= 2019L),
    distance = "pooled_distance_ft",
    pair_average = "pooled_pair_average"
  ),
  dated_sign_pooled_average_prereform_common_sample = list(
    data = common_sample |>
      dplyr::filter(construction_year <= 2019L),
    distance = "dated_distance_ft",
    pair_average = "pooled_pair_average"
  ),
  dated_prereform_common_sample = list(
    data = common_sample |>
      dplyr::filter(construction_year <= 2019L),
    distance = "dated_distance_ft",
    pair_average = "dated_pair_average"
  )
)

model_results <- list()
i <- 0L
for (score_method in names(model_versions)) {
  version <- model_versions[[score_method]]
  for (sample in c("all", "multifamily")) {
    for (outcome in c("density_far", "density_dupac")) {
      i <- i + 1L
      model_results[[i]] <- estimate_density(
        version$data,
        sample,
        outcome,
        score_method,
        version$distance,
        version$pair_average
      )
    }
  }
}
regime_density_results <- dplyr::bind_rows(model_results) |>
  dplyr::mutate(
    stars = dplyr::case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    )
  )

readr::write_csv(
  classification_stability,
  "../output/regime_classification_stability.csv"
)
readr::write_csv(flip_contributors, "../output/regime_flip_contributors.csv")
readr::write_csv(sample_coverage, "../output/regime_score_coverage.csv")
readr::write_csv(regime_density_results, "../output/regime_density_results.csv")
