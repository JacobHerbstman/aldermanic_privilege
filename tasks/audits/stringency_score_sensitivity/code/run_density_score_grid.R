# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

parcels <- read_csv(
  "../input/density_score_sensitivity_sample.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess()),
  guess_max = Inf
)
scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L)
specifications <- read_csv("../input/score_specifications.csv", show_col_types = FALSE)

if (anyDuplicated(parcels$pin) > 0) {
  stop("The fixed density sample contains duplicate PINs.", call. = FALSE)
}
if (max(parcels$dist_to_boundary_m, na.rm = TRUE) > 152.4 + 1e-8) {
  stop("The fixed density sample contains a project beyond 500 feet.", call. = FALSE)
}
if (anyNA(parcels$zone_group) || anyNA(parcels$segment_id) || anyNA(parcels$ward_pair)) {
  stop("The fixed density sample is missing a required fixed effect or cluster.", call. = FALSE)
}

baseline_scores <- scores %>%
  filter(spec_id == "baseline_log_all_workload") %>%
  select(alderman, score)
baseline_sides <- parcels %>%
  left_join(
    baseline_scores %>% rename(alderman_own = alderman, baseline_own = score),
    by = "alderman_own",
    relationship = "many-to-one"
  ) %>%
  left_join(
    baseline_scores %>% rename(alderman_neighbor = alderman, baseline_neighbor = score),
    by = "alderman_neighbor",
    relationship = "many-to-one"
  ) %>%
  transmute(pin, baseline_side = as.integer(baseline_own > baseline_neighbor))

model_rows <- list()
assignment_rows <- list()

for (spec_i in seq_len(nrow(specifications))) {
  spec <- specifications[spec_i, ]
  score_lookup <- scores %>%
    filter(spec_id == spec$spec_id) %>%
    select(alderman, score)

  model_base <- parcels %>%
    left_join(
      score_lookup %>% rename(alderman_own = alderman, score_own = score),
      by = "alderman_own",
      relationship = "many-to-one"
    ) %>%
    left_join(
      score_lookup %>% rename(alderman_neighbor = alderman, score_neighbor = score),
      by = "alderman_neighbor",
      relationship = "many-to-one"
    ) %>%
    left_join(baseline_sides, by = "pin", relationship = "one-to-one") %>%
    filter(!is.na(score_own), !is.na(score_neighbor)) %>%
    mutate(
      side = case_when(
        score_own > score_neighbor ~ 1L,
        score_own < score_neighbor ~ 0L,
        TRUE ~ NA_integer_
      ),
      signed_distance = case_when(
        side == 1L ~ abs(dist_to_boundary_m),
        side == 0L ~ -abs(dist_to_boundary_m),
        TRUE ~ NA_real_
      ),
      lenient_dist = abs(signed_distance) * as.integer(signed_distance <= 0),
      strict_dist = abs(signed_distance) * as.integer(signed_distance > 0)
    )

  assignment_rows[[length(assignment_rows) + 1L]] <- model_base %>%
    summarise(
      spec_id = spec$spec_id,
      projects_with_scores = n(),
      tied_projects = sum(is.na(side)),
      side_changes_from_baseline = sum(side != baseline_side, na.rm = TRUE),
      side_change_share = mean(side != baseline_side, na.rm = TRUE)
    )

  for (construction_sample in c("all", "multifamily")) {
    sample_rows <- if (construction_sample == "all") {
      model_base %>% filter(unitscount > 0)
    } else {
      model_base %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac", "unitscount")) {
      outcome_rows <- sample_rows %>%
        filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
        mutate(outcome_value = log(.data[[outcome]]))

      for (treatment in c("continuous", "binary")) {
        treatment_variable <- if (treatment == "continuous") "score_own" else "side"
        regression_rows <- outcome_rows %>%
          filter(!is.na(.data[[treatment_variable]]), !is.na(signed_distance))

        model <- feols(
          as.formula(paste0(
            "outcome_value ~ ", treatment_variable,
            " + lenient_dist + strict_dist + ",
            paste(demographic_controls, collapse = " + "),
            " | zone_group + segment_id + construction_year"
          )),
          data = regression_rows,
          cluster = ~ward_pair,
          notes = FALSE
        )
        coefficient_table <- coeftable(model)
        used_rows <- obs(model)

        model_rows[[length(model_rows) + 1L]] <- tibble(
          spec_id = spec$spec_id,
          family = spec$family,
          label = spec$label,
          construction_sample,
          outcome,
          treatment,
          estimate = unname(coefficient_table[treatment_variable, "Estimate"]),
          se = unname(coefficient_table[treatment_variable, "Std. Error"]),
          p_value = unname(coefficient_table[treatment_variable, "Pr(>|t|)"]),
          n = nobs(model),
          ward_pairs = n_distinct(regression_rows$ward_pair[used_rows]),
          recovered_projects = sum(
            regression_rows$sample_source[used_rows] !=
              "production_current_2025_coordinate"
          )
        )
      }
    }
  }
}

write_csv(bind_rows(model_rows), "../output/density_score_sensitivity_models.csv")
write_csv(bind_rows(assignment_rows), "../output/density_score_side_assignments.csv")
