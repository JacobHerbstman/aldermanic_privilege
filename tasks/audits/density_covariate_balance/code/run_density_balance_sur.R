# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_covariate_balance/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

covariates <- tribble(
  ~covariate_group, ~covariate,
  "Density model controls", "share_white_own",
  "Density model controls", "share_black_own",
  "Density model controls", "median_hh_income_own",
  "Density model controls", "share_bach_plus_own",
  "Density model controls", "homeownership_rate_own",
  "Block-group characteristics", "percent_white_bg",
  "Block-group characteristics", "percent_black_bg",
  "Block-group characteristics", "percent_hispanic_bg",
  "Block-group characteristics", "homeownership_rate_bg",
  "Block-group characteristics", "median_income_bg",
  "Block-group characteristics", "share_bach_plus_bg",
  "Block-group characteristics", "median_rent_bg",
  "Block-group characteristics", "median_home_value_bg",
  "Block-group characteristics", "avg_household_size_bg",
  "Block-group characteristics", "median_age_bg",
  "Block-group characteristics", "population_density_bg"
)

system_test <- function(estimates, covariance, denominator_df = Inf) {
  covariance <- (covariance + t(covariance)) / 2
  eigenvalues <- eigen(covariance, symmetric = TRUE, only.values = TRUE)$values
  tolerance <- max(abs(eigenvalues)) * 1e-10
  positive_definite <- min(eigenvalues) > tolerance

  if (positive_definite) {
    statistic <- drop(t(estimates) %*% solve(covariance, estimates))
    numerator_df <- length(estimates)
    f_statistic <- statistic / numerator_df
    p_value <- if (is.finite(denominator_df)) {
      pf(f_statistic, numerator_df, denominator_df, lower.tail = FALSE)
    } else {
      pchisq(statistic, numerator_df, lower.tail = FALSE)
    }
  } else {
    statistic <- NA_real_
    numerator_df <- length(estimates)
    f_statistic <- NA_real_
    p_value <- NA_real_
  }

  tibble(
    statistic = statistic,
    f_statistic = f_statistic,
    numerator_df = numerator_df,
    denominator_df = denominator_df,
    p_value = p_value,
    positive_definite = positive_definite,
    minimum_eigenvalue = min(eigenvalues),
    condition_number = if (positive_definite) max(eigenvalues) / min(eigenvalues) else NA_real_
  )
}

spatial_pairs <- function(x, y, maximum_distance_ft) {
  points <- st_as_sf(tibble(x = x, y = y), coords = c("x", "y"), crs = 3435)
  neighbors <- st_is_within_distance(points, dist = maximum_distance_ft)
  pair_rows <- vector("list", length(neighbors))

  for (i in seq_along(neighbors)) {
    j <- neighbors[[i]]
    j <- j[j >= i]
    if (length(j) == 0) {
      next
    }
    pair_rows[[i]] <- tibble(
      i = i,
      j = j,
      distance_ft = sqrt((x[i] - x[j])^2 + (y[i] - y[j])^2)
    )
  }

  bind_rows(pair_rows)
}

spatial_covariance <- function(influence, pairs, bandwidth_ft, hc_scale) {
  usable_pairs <- pairs %>% filter(distance_ft <= bandwidth_ft)
  covariance <- matrix(0, ncol(influence), ncol(influence))

  for (i in unique(usable_pairs$i)) {
    rows_i <- usable_pairs %>% filter(.data$i == .env$i)
    same_row <- rows_i$j == i
    if (any(same_row)) {
      covariance <- covariance + tcrossprod(influence[i, ])
    }

    other_rows <- rows_i[!same_row, , drop = FALSE]
    if (nrow(other_rows) > 0) {
      weights <- 1 - other_rows$distance_ft / bandwidth_ft
      weighted_neighbor_score <- colSums(influence[other_rows$j, , drop = FALSE] * weights)
      covariance <- covariance +
        tcrossprod(influence[i, ], weighted_neighbor_score) +
        tcrossprod(weighted_neighbor_score, influence[i, ])
    }
  }

  covariance * hc_scale
}

geometry <- st_read("../input/parcels_with_geometry.gpkg", quiet = TRUE) %>%
  st_transform(3435)
if (anyDuplicated(geometry$pin) > 0) {
  stop("Parcel geometry must be unique by PIN.", call. = FALSE)
}
coordinates <- st_coordinates(geometry)
geometry <- geometry %>%
  st_drop_geometry() %>%
  transmute(pin = as.character(pin), x = coordinates[, 1], y = coordinates[, 2])

parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  left_join(geometry, by = "pin", relationship = "one-to-one") %>%
  mutate(
    construction_year = as.integer(construction_year),
    ward_pair = as.character(ward_pair),
    segment_id = as.character(segment_id),
    endpoint_own = as.integer(ward),
    endpoint_neighbor = as.integer(other_ward),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2,
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    between(construction_year, 2006, 2022),
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(construction_zone_group),
    !is.na(segment_id),
    segment_id != "",
    is.finite(pair_average_score),
    is.finite(continuous_score_difference),
    is.finite(x),
    is.finite(y)
  )

test_rows <- list()
coefficient_rows <- list()

for (sample_name in c("All construction", "Multifamily")) {
  sample_data <- if (sample_name == "All construction") {
    parcels %>% filter(unitscount > 0)
  } else {
    parcels %>% filter(unitscount > 1)
  }

  for (group_name in unique(covariates$covariate_group)) {
    covariate_names <- covariates %>%
      filter(covariate_group == group_name) %>%
      pull(covariate)
    common_data <- sample_data %>%
      filter(if_all(all_of(covariate_names), ~ is.finite(.x)))
    standardized <- scale(as.matrix(common_data[, covariate_names]))
    common_data[, covariate_names] <- standardized

    distance_pairs <- spatial_pairs(common_data$x, common_data$y, 5280)

    for (treatment_name in c("side", "continuous_score_difference")) {
      models <- vector("list", length(covariate_names))
      estimates <- numeric(length(covariate_names))
      influence <- matrix(NA_real_, nrow(common_data), length(covariate_names))
      pair_scales <- numeric(length(covariate_names))
      hc_scales <- numeric(length(covariate_names))
      directional_scales <- numeric(length(covariate_names))

      for (i in seq_along(covariate_names)) {
        models[[i]] <- feols(
          as.formula(paste0(
            covariate_names[i], " ~ ", treatment_name,
            " + pair_average_score + lenient_dist + strict_dist | segment_id + construction_year"
          )),
          data = common_data,
          warn = FALSE,
          notes = FALSE
        )
        model <- models[[i]]
        if (!identical(obs(model), seq_len(nrow(common_data)))) {
          stop("SUR equations do not use the full common sample.", call. = FALSE)
        }

        coefficient_index <- match(treatment_name, names(coef(model)))
        bread <- model$cov.unscaled
        if (is.null(bread)) {
          bread <- solve(-model$hessian)
        }
        estimates[i] <- coef(model)[coefficient_index]
        influence[, i] <- drop(model$scores %*% bread[, coefficient_index])

        pair_raw_variance <- drop(crossprod(rowsum(
          matrix(influence[, i], ncol = 1),
          common_data$ward_pair
        )))
        pair_fixest_variance <- vcov(
          model,
          cluster = ~ward_pair,
          vcov_fix = FALSE
        )[coefficient_index, coefficient_index]
        pair_scales[i] <- pair_fixest_variance / pair_raw_variance

        hc_raw_variance <- drop(crossprod(influence[, i]))
        hc_fixest_variance <- vcov(
          model,
          vcov = "hetero",
          vcov_fix = FALSE
        )[coefficient_index, coefficient_index]
        hc_scales[i] <- hc_fixest_variance / hc_raw_variance
      }

      scale_ratios <- hc_scales / pair_scales
      if (max(abs(scale_ratios - scale_ratios[1])) > 1e-8) {
        stop("The HC and ward-pair finite-sample corrections are not proportional.", call. = FALSE)
      }

      pair_influence <- sweep(influence, 2, sqrt(pair_scales), "*")
      hc_influence <- sweep(influence, 2, sqrt(hc_scales), "*")
      pair_scores <- rowsum(pair_influence, common_data$ward_pair, reorder = TRUE)
      pair_map <- common_data %>%
        transmute(
          ward_pair,
          ward_a = pmin(endpoint_own, endpoint_neighbor),
          ward_b = pmax(endpoint_own, endpoint_neighbor)
        ) %>%
        distinct()
      if (anyDuplicated(pair_map$ward_pair) > 0) {
        stop("A ward-pair label maps to multiple endpoint sets.", call. = FALSE)
      }
      pair_map <- pair_map[match(rownames(pair_scores), pair_map$ward_pair), ]

      node_scores <- rowsum(
        rbind(pair_scores, pair_scores),
        c(pair_map$ward_a, pair_map$ward_b),
        reorder = TRUE
      )
      own_scores <- rowsum(influence, common_data$endpoint_own, reorder = TRUE)
      neighbor_scores <- rowsum(influence, common_data$endpoint_neighbor, reorder = TRUE)
      directed_pair <- interaction(
        common_data$endpoint_own,
        common_data$endpoint_neighbor,
        drop = TRUE,
        lex.order = TRUE
      )
      directed_pair_scores <- rowsum(influence, directed_pair, reorder = TRUE)
      directional_covariance_raw <-
        crossprod(own_scores) +
        crossprod(neighbor_scores) -
        crossprod(directed_pair_scores)
      for (i in seq_along(models)) {
        coefficient_index <- match(treatment_name, names(coef(models[[i]])))
        directional_variance <- vcov(
          models[[i]],
          cluster = ~endpoint_own + endpoint_neighbor,
          vcov_fix = FALSE
        )[coefficient_index, coefficient_index]
        directional_scales[i] <- directional_variance / directional_covariance_raw[i, i]
      }
      directional_scale_matrix <- outer(
        sqrt(directional_scales),
        sqrt(directional_scales)
      )

      covariance_methods <- list(
        heteroskedastic_robust = list(
          covariance = crossprod(hc_influence),
          denominator_df = Inf,
          clusters = NA_integer_
        ),
        ward_pair_clustered = list(
          covariance = crossprod(pair_scores),
          denominator_df = nrow(pair_scores) - 1,
          clusters = nrow(pair_scores)
        ),
        directional_endpoint_clustered = list(
          covariance = directional_covariance_raw * directional_scale_matrix,
          denominator_df = min(nrow(own_scores), nrow(neighbor_scores)) - 1,
          clusters = min(nrow(own_scores), nrow(neighbor_scores))
        ),
        dyadic_shared_ward = list(
          covariance = crossprod(node_scores) - crossprod(pair_scores),
          denominator_df = nrow(node_scores) - 1,
          clusters = nrow(node_scores)
        )
      )

      for (bandwidth_ft in c(1000, 2640, 5280)) {
        method_name <- paste0("spatial_bartlett_", bandwidth_ft, "ft")
        covariance_methods[[method_name]] <- list(
          covariance = spatial_covariance(
            hc_influence,
            distance_pairs,
            bandwidth_ft,
            1
          ),
          denominator_df = Inf,
          clusters = NA_integer_,
          spatial_pairs = sum(distance_pairs$distance_ft <= bandwidth_ft)
        )
      }

      for (method_name in names(covariance_methods)) {
        method <- covariance_methods[[method_name]]
        test <- system_test(estimates, method$covariance, method$denominator_df)
        test_rows[[length(test_rows) + 1L]] <- test %>%
          mutate(
            sample = sample_name,
            covariate_group = group_name,
            treatment = treatment_name,
            covariance_method = method_name,
            observations = nrow(common_data),
            outcomes = length(covariate_names),
            clusters = method$clusters,
            spatial_pairs = if (is.null(method$spatial_pairs)) NA_integer_ else method$spatial_pairs
          ) %>%
          relocate(sample, covariate_group, treatment, covariance_method)

        standard_errors <- sqrt(pmax(diag(method$covariance), 0))
        coefficient_rows[[length(coefficient_rows) + 1L]] <- tibble(
          sample = sample_name,
          covariate_group = group_name,
          treatment = treatment_name,
          covariance_method = method_name,
          covariate = covariate_names,
          estimate = estimates,
          standard_error = standard_errors,
          observations = nrow(common_data)
        )
      }
    }
  }
}

sur_tests <- bind_rows(test_rows)
sur_coefficients <- bind_rows(coefficient_rows)

expected_coefficients <- read_csv(
  "../output/density_covariate_balance.csv",
  show_col_types = FALSE
) %>%
  select(
    sample, covariate_group, covariate,
    binary_estimate_sd, binary_se,
    continuous_estimate_sd, continuous_se
  ) %>%
  pivot_longer(
    cols = c(binary_estimate_sd, continuous_estimate_sd),
    names_to = "treatment",
    values_to = "expected_estimate"
  ) %>%
  mutate(
    expected_standard_error = if_else(
      treatment == "binary_estimate_sd",
      binary_se,
      continuous_se
    ),
    treatment = if_else(
      treatment == "binary_estimate_sd",
      "side",
      "continuous_score_difference"
    )
  ) %>%
  select(
    sample, covariate_group, treatment, covariate,
    expected_estimate, expected_standard_error
  )

validation <- sur_coefficients %>%
  filter(covariance_method == "ward_pair_clustered") %>%
  left_join(
    expected_coefficients,
    by = c("sample", "covariate_group", "treatment", "covariate"),
    relationship = "one-to-one"
  )
if (anyNA(validation$expected_estimate) ||
    max(abs(validation$estimate - validation$expected_estimate)) > 1e-8 ||
    max(abs(validation$standard_error - validation$expected_standard_error)) > 1e-8) {
  stop("SUR coefficients do not reproduce the equation-by-equation balance results.", call. = FALSE)
}

write_csv(sur_tests, "../output/density_balance_sur_tests.csv")
write_csv(sur_coefficients, "../output/density_balance_sur_coefficients.csv")
