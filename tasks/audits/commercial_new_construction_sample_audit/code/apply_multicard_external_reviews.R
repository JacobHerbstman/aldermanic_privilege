# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

density <- readr::read_csv(
  "../output/multicard_adjudicated_density_model_input.csv",
  show_col_types = FALSE
)
reviews <- readr::read_csv(
  "../output/multicard_external_review_queue.csv",
  show_col_types = FALSE
)
scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(cutoff == 2022L) |>
  dplyr::select(variant, alderman, score)

expected_variants <- c(
  "all_covariates",
  "education_added_back",
  "income_added_back",
  "current_no_income"
)
if (
  anyDuplicated(density$project_id) ||
    anyDuplicated(reviews$project_id) ||
    anyDuplicated(scores[c("variant", "alderman")]) ||
    !setequal(scores$variant, expected_variants)
) {
  stop("The external-review inputs are not uniquely keyed.", call. = FALSE)
}
if (
  any(!reviews$review_status %in% c("complete", "lineage_pending")) ||
    any(
      reviews$review_status == "complete" &
        !reviews$multifamily_disposition %in%
          c("include", "exclude", "suppress")
    ) ||
    any(
      reviews$review_status == "lineage_pending" &
        reviews$multifamily_disposition != "pending"
    )
) {
  stop("The external review contains an incomplete disposition.", call. = FALSE)
}

review_fields <- reviews |>
  dplyr::select(
    project_id,
    pin,
    review_priority,
    review_address,
    review_status,
    external_structure_class,
    multifamily_disposition,
    external_building_count,
    external_unit_count,
    external_building_sqft,
    source_1_url,
    source_2_url,
    supports_building_type,
    supports_final_units,
    reviewer_notes,
    review_date
  )

current_input <- density |>
  dplyr::mutate(
    original_dwelling_units = dwelling_units,
    original_building_sqft = building_sqft,
    externally_reviewed = project_id %in% reviews$project_id,
    external_multifamily = dwelling_units > 1,
    external_value_used = FALSE
  ) |>
  dplyr::left_join(
    review_fields,
    by = "project_id",
    relationship = "one-to-one"
  )

validity_screen <- current_input |>
  dplyr::filter(
    is.na(multifamily_disposition) |
      !multifamily_disposition %in% c("suppress", "pending")
  )

type_classification <- validity_screen |>
  dplyr::mutate(
    external_multifamily = dplyr::case_when(
      multifamily_disposition == "include" ~ TRUE,
      multifamily_disposition == "exclude" ~ FALSE,
      TRUE ~ dwelling_units > 1
    )
  )

external_input <- type_classification |>
  dplyr::mutate(
    dwelling_units = dplyr::if_else(
      multifamily_disposition %in% c("include", "exclude") &
        !is.na(external_unit_count),
      external_unit_count,
      dwelling_units
    ),
    building_sqft = dplyr::if_else(
      multifamily_disposition %in% c("include", "exclude") &
        !is.na(external_building_sqft),
      external_building_sqft,
      building_sqft
    ),
    external_value_used =
      dplyr::coalesce(
        dwelling_units != original_dwelling_units,
        FALSE
      ) |
        dplyr::coalesce(
          building_sqft != original_building_sqft,
          FALSE
        ),
    density_far = building_sqft / land_sqft,
    density_dupac = 43560 * dwelling_units / land_sqft
  )

if (
  anyDuplicated(external_input$project_id) ||
    any(
      external_input$multifamily_disposition == "include" &
        !external_input$external_multifamily,
      na.rm = TRUE
    ) ||
    any(
      external_input$multifamily_disposition == "exclude" &
        external_input$external_multifamily,
      na.rm = TRUE
    ) ||
    any(
      external_input$external_multifamily &
        external_input$dwelling_units <= 1,
      na.rm = TRUE
    )
) {
  stop("The reviewed model input violates its sample rules.", call. = FALSE)
}

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

prepare_variant <- function(data, variant_id) {
  score_map <- scores |>
    dplyr::filter(variant == variant_id) |>
    dplyr::select(alderman, score) |>
    tibble::deframe()

  data |>
    dplyr::mutate(
      score_own = unname(score_map[alderman_own]),
      score_neighbor = unname(score_map[alderman_neighbor]),
      side = as.integer(score_own > score_neighbor),
      dist_to_boundary_m = distance_to_boundary_ft * 0.3048,
      lenient_dist = abs(dist_to_boundary_m) * as.integer(side == 0L),
      strict_dist = abs(dist_to_boundary_m) * as.integer(side == 1L),
      continuous_score_difference = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2
    )
}

fit_models <- function(
  data,
  stage_id,
  variant_id,
  cluster_levels = c("ward_pair", "segment")
) {
  variant_data <- prepare_variant(data, variant_id)
  rows <- list()

  for (sample_name in c("all", "multifamily")) {
    sample_data <- variant_data |>
      dplyr::filter(
        construction_year >= 2006,
        construction_year <= 2022,
        distance_to_boundary_ft <= 500,
        dwelling_units > 0,
        sample_name == "all" | external_multifamily
      )

    for (sample_rule in c("outcome_specific", "common_density")) {
      for (outcome in c("density_far", "density_dupac")) {
        eligibility_field <- if (outcome == "density_far") {
          "allow_far"
        } else {
          "allow_dupac"
        }
        model_data <- sample_data |>
          dplyr::filter(
            .data[[eligibility_field]],
            is.finite(.data[[outcome]]),
            .data[[outcome]] > 0,
            sample_rule == "outcome_specific" | (
              allow_far &
                allow_dupac &
                is.finite(density_far) &
                density_far > 0 &
                is.finite(density_dupac) &
                density_dupac > 0
            ),
            is.finite(score_own),
            is.finite(score_neighbor),
            is.finite(pair_average_score),
            dplyr::if_all(
              dplyr::all_of(demographic_controls),
              is.finite
            ),
            !is.na(zone_group),
            !is.na(segment_id),
            segment_id != ""
          )

        for (treatment in c("continuous", "binary")) {
          treatment_term <- if (treatment == "continuous") {
            "continuous_score_difference"
          } else {
            "side"
          }
          for (cluster_level in cluster_levels) {
            cluster_field <- if (cluster_level == "ward_pair") {
              "ward_pair"
            } else {
              "segment_id"
            }
            model <- fixest::feols(
              stats::as.formula(paste0(
                "log(",
                outcome,
                ") ~ ",
                paste(
                  c(
                    treatment_term,
                    "pair_average_score",
                    "lenient_dist",
                    "strict_dist",
                    demographic_controls
                  ),
                  collapse = " + "
                ),
                " | zone_group + segment_id + construction_year"
              )),
              data = model_data,
              cluster = stats::as.formula(paste0("~", cluster_field))
            )
            coefficient <- fixest::coeftable(model)[treatment_term, ]
            rows[[length(rows) + 1L]] <- tibble::tibble(
              stage = stage_id,
              variant = variant_id,
              sample = sample_name,
              sample_rule,
              outcome,
              treatment,
              cluster_level,
              estimate = unname(coefficient["Estimate"]),
              se = unname(coefficient["Std. Error"]),
              p_value = unname(coefficient["Pr(>|t|)"]),
              n_obs = stats::nobs(model),
              n_clusters = dplyr::n_distinct(
                model_data[[cluster_field]]
              )
            )
          }
        }
      }
    }
  }
  dplyr::bind_rows(rows)
}

stages <- list(
  current_adjudication = current_input,
  validity_screen = validity_screen,
  external_type_classification = type_classification,
  external_values = external_input,
  external_values_old_500_w_66th = external_input |>
    dplyr::mutate(
      dwelling_units = dplyr::if_else(
        project_id == "residential_multicard_20211200290000",
        2,
        dwelling_units
      ),
      building_sqft = dplyr::if_else(
        project_id == "residential_multicard_20211200290000",
        2210,
        building_sqft
      ),
      density_far = building_sqft / land_sqft,
      density_dupac = 43560 * dwelling_units / land_sqft
    )
)

stage_results <- purrr::imap_dfr(
  stages,
  ~ fit_models(
    .x,
    .y,
    "all_covariates",
    cluster_levels = "ward_pair"
  )
)

final_results <- purrr::map_dfr(
  expected_variants,
  ~ fit_models(external_input, "external_values", .x)
)

reviewed_changes <- current_input |>
  dplyr::filter(externally_reviewed) |>
  dplyr::transmute(
    project_id,
    pin,
    construction_year,
    ward_pair,
    distance_to_boundary_ft,
    within_500ft,
    review_address,
    review_status,
    external_structure_class,
    multifamily_disposition,
    original_dwelling_units,
    reviewed_dwelling_units = dplyr::case_when(
      multifamily_disposition %in% c("suppress", "pending") ~ NA_real_,
      !is.na(external_unit_count) ~ external_unit_count,
      TRUE ~ original_dwelling_units
    ),
    original_building_sqft,
    reviewed_building_sqft = dplyr::case_when(
      multifamily_disposition %in% c("suppress", "pending") ~ NA_real_,
      !is.na(external_building_sqft) ~ external_building_sqft,
      TRUE ~ original_building_sqft
    ),
    original_multifamily = original_dwelling_units > 1,
    reviewed_multifamily = multifamily_disposition == "include",
    source_1_url,
    source_2_url,
    reviewer_notes
  )

changed_pairs <- reviewed_changes |>
  dplyr::filter(
    multifamily_disposition %in% c("suppress", "pending") |
      original_multifamily != reviewed_multifamily |
      original_dwelling_units != reviewed_dwelling_units |
      original_building_sqft != reviewed_building_sqft
  ) |>
  dplyr::distinct(ward_pair) |>
  dplyr::pull(ward_pair)

revert_pair <- function(pair_id) {
  pair_review_ids <- reviewed_changes |>
    dplyr::filter(ward_pair == pair_id) |>
    dplyr::pull(project_id)

  reverted <- external_input |>
    dplyr::filter(!project_id %in% pair_review_ids) |>
    dplyr::bind_rows(
      current_input |>
        dplyr::filter(project_id %in% pair_review_ids) |>
        dplyr::mutate(
          external_multifamily = original_dwelling_units > 1,
          dwelling_units = original_dwelling_units,
          building_sqft = original_building_sqft,
          density_far = building_sqft / land_sqft,
          density_dupac = 43560 * dwelling_units / land_sqft
        )
    )

  fit_models(
    reverted,
    "pair_reverted",
    "all_covariates",
    cluster_levels = "ward_pair"
  ) |>
    dplyr::filter(
      sample == "multifamily",
      sample_rule == "common_density"
    ) |>
    dplyr::mutate(reverted_ward_pair = pair_id)
}

pair_reversions <- purrr::map_dfr(changed_pairs, revert_pair)
final_reference <- final_results |>
  dplyr::filter(
    variant == "all_covariates",
    sample == "multifamily",
    sample_rule == "common_density",
    cluster_level == "ward_pair"
  ) |>
  dplyr::select(
    outcome,
    treatment,
    final_estimate = estimate,
    final_se = se,
    final_p_value = p_value,
    final_n_obs = n_obs
  )

pair_influence <- pair_reversions |>
  dplyr::left_join(
    final_reference,
    by = c("outcome", "treatment"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    estimate_change_when_reverted = estimate - final_estimate
  ) |>
  dplyr::arrange(
    outcome,
    treatment,
    dplyr::desc(abs(estimate_change_when_reverted))
  )

duplicate_validation <- tibble::tribble(
  ~metric, ~value, ~passed,
  "reviewed_input_rows", nrow(external_input), TRUE,
  "duplicate_project_ids", anyDuplicated(external_input$project_id),
  anyDuplicated(external_input$project_id) == 0,
  "unresolved_external_reviews_retained",
  sum(
    external_input$multifamily_disposition == "pending",
    na.rm = TRUE
  ),
  !any(
    external_input$multifamily_disposition == "pending",
    na.rm = TRUE
  ),
  "unresolved_retained_duplicate_pairs",
  nrow(readr::read_csv(
    "../output/multicard_retained_duplicate_unresolved.csv",
    show_col_types = FALSE
  )),
  nrow(readr::read_csv(
    "../output/multicard_retained_duplicate_unresolved.csv",
    show_col_types = FALSE
  )) == 0
)

summary <- dplyr::bind_rows(
  reviews |>
    dplyr::count(
      section = "external_review_disposition",
      metric = multifamily_disposition,
      name = "value"
    ),
  tibble::tibble(
    section = "reviewed_model_input",
    metric = c(
      "current_rows",
      "reviewed_rows",
      "removed_spurious_or_unbuilt",
      "removed_pending",
      "final_rows",
      "reviewed_multifamily_included",
      "reviewed_multifamily_excluded",
      "rows_with_external_value_change"
    ),
    value = c(
      nrow(current_input),
      nrow(reviews),
      sum(reviews$multifamily_disposition == "suppress"),
      sum(reviews$multifamily_disposition == "pending"),
      nrow(external_input),
      sum(reviews$multifamily_disposition == "include"),
      sum(reviews$multifamily_disposition == "exclude"),
      sum(external_input$external_value_used)
    )
  )
)

readr::write_csv(
  external_input,
  "../output/multicard_external_reviewed_model_input.csv",
  na = ""
)
readr::write_csv(
  final_results,
  "../output/multicard_external_reviewed_density_results.csv",
  na = ""
)
readr::write_csv(
  stage_results,
  "../output/multicard_external_reviewed_stage_results.csv",
  na = ""
)
readr::write_csv(
  reviewed_changes,
  "../output/multicard_external_reviewed_sample_changes.csv",
  na = ""
)
readr::write_csv(
  pair_influence,
  "../output/multicard_external_reviewed_pair_influence.csv",
  na = ""
)
readr::write_csv(
  duplicate_validation,
  "../output/multicard_external_reviewed_duplicate_validation.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/multicard_external_reviewed_summary.csv",
  na = ""
)
