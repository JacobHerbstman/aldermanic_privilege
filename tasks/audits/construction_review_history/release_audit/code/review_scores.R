# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/working_paper_release_audit/code")
source("../../shared/code/alderman_uncertainty_helpers.R")
permits <- load_uncertainty_permits("../input/score_permits.csv") |>
  filter(month <= as.yearmon("2022-12"))
prepared <- prepare_uncertainty_sample(permits, TRUE, "LAG1", "BOTH")
covariates <- get_stage1_covariates(prepared$place_covariates, TRUE, prepared$volume_var, "share_bach_plus")
fe_terms <- get_stage1_fe_terms(default_uncertainty_config())
stage1 <- fit_stage1_model(prepared$permits, "log_processing_time", covariates, fe_terms, "baseline")
baseline <- build_two_stage_index(stage1$permits_for_reg, TRUE, prepared$volume_var, "N_PERMITS")
published <- read_csv("../input/scores.csv", show_col_types = FALSE)
stopifnot(isTRUE(all.equal(baseline$alderman_index$uncertainty_index,
                          published$uncertainty_index, tolerance = 1e-9)))

# Same controls, same complete-case observations; joint estimation changes only
# whether alderman indicators participate in estimating the adjustment coefficients.
joint <- feols(as.formula(paste0("log_processing_time ~ ",
                paste(covariates, collapse = " + "), " | alderman + ", paste(fe_terms, collapse = " + "))),
               data = stage1$permits_for_reg, cluster = ~ward, notes = FALSE)
joint_effects <- enframe(fixef(joint)$alderman, name = "alderman", value = "joint_raw")
# Check that redundant nuisance fixed effects do not make alderman rankings
# depend on the absorbed-effect normalization.
reference_alderman <- sort(unique(stage1$permits_for_reg$alderman))[1]
joint_explicit <- feols(as.formula(paste0(
  "log_processing_time ~ i(alderman, ref = '", reference_alderman, "') + ",
  paste(covariates, collapse = " + "), " | ", paste(fe_terms, collapse = " + "))),
  data = stage1$permits_for_reg, cluster = ~ward, notes = FALSE)
explicit_effects <- coef(joint_explicit)[grepl("^alderman::", names(coef(joint_explicit)))]
names(explicit_effects) <- sub("^alderman::", "", names(explicit_effects))
explicit_effects <- c(explicit_effects, setNames(0, reference_alderman))
centered_joint <- joint_effects$joint_raw -
  joint_effects$joint_raw[match(reference_alderman, joint_effects$alderman)]
stopifnot(nobs(joint) == nobs(stage1$model),
  length(explicit_effects) == nrow(joint_effects),
  max(abs(centered_joint - explicit_effects[joint_effects$alderman])) < 1e-5)
write_csv(tibble(term = covariates,
  sequential_coefficient = unname(coef(stage1$model)[covariates]),
  joint_coefficient = unname(coef(joint)[covariates])),
  "../output/score_covariate_comparison.csv")

comparison <- published |>
  select(alderman, alderman_fe_raw, uncertainty_index, alderman_se, shrinkage_B) |>
  inner_join(joint_effects, by = "alderman", relationship = "one-to-one")
stopifnot(nrow(comparison) == nrow(published))
write_csv(comparison, "../output/score_comparison_by_alderman.csv")
projects <- read_csv("../input/projects.csv", show_col_types = FALSE) |>
  filter(abs(signed_distance_m) < 152.4, dwelling_units > 0, allow_far, allow_dupac,
         is.finite(density_far), density_far > 0, is.finite(density_dupac), density_dupac > 0,
         if_all(c(share_white_own, share_black_own, median_hh_income_own,
                  share_bach_plus_own, homeownership_rate_own), is.finite),
         !is.na(zone_group), !is.na(segment_id), segment_id != "", !is.na(ward_pair), ward_pair != "")
raw <- setNames(comparison$alderman_fe_raw, comparison$alderman)
joint_raw <- setNames(comparison$joint_raw, comparison$alderman)
eb <- setNames(comparison$uncertainty_index, comparison$alderman)
raw_sign <- sign(raw[projects$alderman_own] - raw[projects$alderman_neighbor])
joint_sign <- sign(joint_raw[projects$alderman_own] - joint_raw[projects$alderman_neighbor])
eb_sign <- sign(eb[projects$alderman_own] - eb[projects$alderman_neighbor])
fits <- list()
for (version in c("published_eb", "residualized_raw", "joint_raw")) {
  data <- projects |>
    mutate(direction = if (version == "published_eb") eb_sign else if (version == "residualized_raw") raw_sign else joint_sign,
           distance_bin = floor(abs(signed_distance_m) / 0.3048 * direction / 100))
  for (sample in c("all", "multifamily")) {
    for (outcome in c("density_far", "density_dupac")) {
      d <- data |> filter(sample == "all" | external_multifamily) |> mutate(log_outcome = log(.data[[outcome]]))
      fit <- feols(log_outcome ~ i(distance_bin, ref = -1) + share_white_own + share_black_own +
                     median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
                     zone_group + segment_id + construction_year, data = d, cluster = ~ward_pair, notes = FALSE)
      ct <- coeftable(fit)["distance_bin::0", ]
      fits[[paste(version, sample, outcome)]] <- tibble(version, sample, outcome, estimate = ct[1], se = ct[2], p = ct[4], n = nobs(fit))
    }
  }
}
write_csv(bind_rows(fits), "../output/density_joint_adjustment.csv")
write_csv(tibble(check = c("aldermen_compared", "raw_joint_pearson", "raw_joint_spearman", "projects_compared",
                           "raw_vs_joint_orientation_changes", "published_eb_vs_joint_orientation_changes",
                           "min_shrinkage", "max_shrinkage"),
                 value = c(nrow(comparison), cor(comparison$alderman_fe_raw, comparison$joint_raw),
                           cor(comparison$alderman_fe_raw, comparison$joint_raw, method = "spearman"),
                           nrow(projects), sum(raw_sign != joint_sign, na.rm = TRUE),
                           sum(eb_sign != joint_sign, na.rm = TRUE), min(comparison$shrinkage_B),
                           max(comparison$shrinkage_B))), "../output/score_checks.csv")
