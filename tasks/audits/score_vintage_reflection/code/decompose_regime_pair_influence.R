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
    ward_pair = readr::col_character(),
    segment_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
if (anyDuplicated(projects$project_id)) {
  stop("Construction input is not unique by project ID.", call. = FALSE)
}

add_endpoint_scores <- function(data, scores, prefix) {
  data |>
    dplyr::left_join(
      scores |>
        dplyr::transmute(alderman_own = alderman, !!paste0(prefix, "_own") := score),
      by = "alderman_own",
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      scores |>
        dplyr::transmute(
          alderman_neighbor = alderman,
          !!paste0(prefix, "_neighbor") := score
        ),
      by = "alderman_neighbor",
      relationship = "many-to-one"
    )
}

projects <- projects |>
  add_endpoint_scores(pooled_scores, "pooled") |>
  add_endpoint_scores(regime_scores$early |> dplyr::select(alderman, score), "early") |>
  add_endpoint_scores(
    regime_scores$transition |> dplyr::select(alderman, score),
    "transition"
  ) |>
  dplyr::mutate(
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
    period = dplyr::if_else(construction_year <= 2014L, "2006_2014", "2015_2022"),
    pooled_distance_ft = abs(distance_to_boundary_ft) * sign(pooled_own - pooled_neighbor),
    dated_distance_ft = abs(distance_to_boundary_ft) *
      sign(dated_score_own - dated_score_neighbor),
    pooled_pair_average = (pooled_own + pooled_neighbor) / 2,
    classification_flip = sign(pooled_own - pooled_neighbor) !=
      sign(dated_score_own - dated_score_neighbor),
    endpoint_1 = pmin(alderman_own, alderman_neighbor),
    endpoint_2 = pmax(alderman_own, alderman_neighbor),
    endpoint_pair = paste(endpoint_1, endpoint_2, sep = " | ")
  ) |>
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
    abs(distance_to_boundary_ft) < 500,
    is.finite(pooled_distance_ft),
    is.finite(dated_distance_ft),
    is.finite(pooled_pair_average)
  )

fit_nearest_bin <- function(data, outcome, distance) {
  model_data <- data |>
    dplyr::mutate(
      log_outcome = log(.data[[outcome]]),
      running_distance_ft = .data[[distance]],
      distance_bin = cut(
        running_distance_ft,
        breaks = seq(-500, 500, by = 100),
        labels = sprintf("bin_%02d", 1:10),
        include.lowest = TRUE,
        right = FALSE
      )
    ) |>
    dplyr::filter(!is.na(distance_bin))

  model <- fixest::feols(
    log_outcome ~
      i(distance_bin, ref = "bin_05") +
      pooled_pair_average +
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
  unname(stats::coef(model)["distance_bin::bin_06"])
}

flipped_pairs <- projects |>
  dplyr::filter(classification_flip) |>
  dplyr::distinct(endpoint_pair, endpoint_1, endpoint_2)

results <- list()
result_i <- 0L
for (sample_name in c("all", "multifamily")) {
  sample_data <- projects |>
    dplyr::filter(sample_name == "all" | external_multifamily)

  for (outcome in c("density_far", "density_dupac")) {
    pooled_estimate <- fit_nearest_bin(sample_data, outcome, "pooled_distance_ft")
    dated_estimate <- fit_nearest_bin(sample_data, outcome, "dated_distance_ft")

    for (pair_i in seq_len(nrow(flipped_pairs))) {
      pair <- flipped_pairs[pair_i, ]
      pair_rows <- sample_data$classification_flip &
        sample_data$endpoint_pair == pair$endpoint_pair
      if (!any(pair_rows)) {
        next
      }

      pair_data <- sample_data |>
        dplyr::mutate(
          dated_with_pair_restored = dplyr::if_else(
            pair_rows,
            pooled_distance_ft,
            dated_distance_ft
          ),
          pooled_with_pair_reclassified = dplyr::if_else(
            pair_rows,
            dated_distance_ft,
            pooled_distance_ft
          )
        )
      restored_estimate <- fit_nearest_bin(
        pair_data,
        outcome,
        "dated_with_pair_restored"
      )
      reclassified_estimate <- fit_nearest_bin(
        pair_data,
        outcome,
        "pooled_with_pair_reclassified"
      )

      result_i <- result_i + 1L
      results[[result_i]] <- tibble::tibble(
        sample = sample_name,
        outcome,
        endpoint_1 = pair$endpoint_1,
        endpoint_2 = pair$endpoint_2,
        periods = paste(sort(unique(sample_data$period[pair_rows])), collapse = ";"),
        n_flipped_projects = sum(pair_rows),
        n_flipped_nearest_100ft = sum(
          pair_rows & abs(sample_data$distance_to_boundary_ft) < 100
        ),
        mean_pooled_score_gap = mean(abs(
          sample_data$pooled_own[pair_rows] - sample_data$pooled_neighbor[pair_rows]
        )),
        mean_period_score_gap = mean(abs(
          sample_data$dated_score_own[pair_rows] -
            sample_data$dated_score_neighbor[pair_rows]
        )),
        pooled_estimate,
        dated_estimate,
        total_attenuation = dated_estimate - pooled_estimate,
        restored_estimate,
        change_from_restoring_pooled_pair = restored_estimate - dated_estimate,
        share_of_total_gap_restored =
          (restored_estimate - dated_estimate) / (pooled_estimate - dated_estimate),
        reclassified_estimate,
        change_from_reclassifying_pooled_pair = reclassified_estimate - pooled_estimate
      )
    }
  }
}

pair_influence <- dplyr::bind_rows(results) |>
  dplyr::arrange(
    sample,
    outcome,
    change_from_restoring_pooled_pair
  )

alderman_influence <- dplyr::bind_rows(
  pair_influence |>
    dplyr::transmute(
      sample,
      outcome,
      alderman = endpoint_1,
      pair = paste(endpoint_1, endpoint_2, sep = " | "),
      change_from_restoring_pooled_pair
    ),
  pair_influence |>
    dplyr::transmute(
      sample,
      outcome,
      alderman = endpoint_2,
      pair = paste(endpoint_1, endpoint_2, sep = " | "),
      change_from_restoring_pooled_pair
    )
) |>
  dplyr::summarise(
    n_flipped_pairs = dplyr::n(),
    sum_negative_restoration = sum(pmin(change_from_restoring_pooled_pair, 0)),
    sum_absolute_restoration = sum(abs(change_from_restoring_pooled_pair)),
    largest_absolute_pair_change = max(abs(change_from_restoring_pooled_pair)),
    .by = c(sample, outcome, alderman)
  ) |>
  dplyr::arrange(sample, outcome, sum_negative_restoration)

readr::write_csv(pair_influence, "../output/regime_pair_influence.csv")
readr::write_csv(alderman_influence, "../output/regime_alderman_influence.csv")
