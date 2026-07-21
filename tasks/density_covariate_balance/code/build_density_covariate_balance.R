# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/density_covariate_balance/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

covariates <- tribble(
  ~covariate, ~label,
  "percent_white_bg", "Share White",
  "percent_black_bg", "Share Black",
  "percent_hispanic_bg", "Share Hispanic",
  "homeownership_rate_bg", "Homeownership rate",
  "median_income_bg", "Median household income",
  "share_bach_plus_bg", "Bachelor's degree or higher share",
  "median_rent_bg", "Median gross rent",
  "median_home_value_bg", "Median home value",
  "avg_household_size_bg", "Average household size",
  "median_age_bg", "Median age",
  "population_density_bg", "Population density"
)

parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    construction_year = as.integer(construction_year),
    ward_pair = as.character(ward_pair),
    segment_id = as.character(segment_id),
    side = as.integer(signed_distance_m > 0),
    pair_average_score = (strictness_own + strictness_neighbor) / 2,
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    between(construction_year, 2006, 2022),
    unitscount > 0,
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(construction_zone_group),
    !is.na(segment_id),
    segment_id != "",
    is.finite(pair_average_score)
  )

balance_rows <- list()
joint_rows <- list()

for (sample_name in c("All construction", "Multifamily")) {
  sample_data <- if (sample_name == "All construction") {
    parcels
  } else {
    parcels %>% filter(unitscount > 1)
  }

  common_data <- sample_data %>%
    filter(if_all(all_of(covariates$covariate), is.finite))
  standardized <- scale(as.matrix(common_data[, covariates$covariate]))
  common_data[, covariates$covariate] <- standardized

  estimates <- numeric(nrow(covariates))
  influence <- matrix(NA_real_, nrow(common_data), nrow(covariates))
  cluster_scales <- numeric(nrow(covariates))

  for (i in seq_len(nrow(covariates))) {
    model <- feols(
      as.formula(paste0(
        covariates$covariate[i],
        " ~ side + pair_average_score + lenient_dist + strict_dist | segment_id + construction_year"
      )),
      data = common_data,
      warn = FALSE,
      notes = FALSE
    )
    if (!identical(obs(model), seq_len(nrow(common_data)))) {
      stop("Balance equations do not use the full common sample.", call. = FALSE)
    }

    coefficient_index <- match("side", names(coef(model)))
    bread <- model$cov.unscaled
    if (is.null(bread)) {
      bread <- solve(-model$hessian)
    }
    estimates[i] <- coef(model)[coefficient_index]
    influence[, i] <- drop(model$scores %*% bread[, coefficient_index])

    raw_variance <- drop(crossprod(rowsum(
      matrix(influence[, i], ncol = 1),
      common_data$ward_pair
    )))
    clustered_variance <- vcov(
      model,
      cluster = ~ward_pair,
      vcov_fix = FALSE
    )[coefficient_index, coefficient_index]
    cluster_scales[i] <- clustered_variance / raw_variance
  }

  pair_influence <- sweep(influence, 2, sqrt(cluster_scales), "*")
  pair_scores <- rowsum(pair_influence, common_data$ward_pair, reorder = TRUE)
  covariance <- crossprod(pair_scores)
  covariance <- (covariance + t(covariance)) / 2
  if (min(eigen(covariance, symmetric = TRUE, only.values = TRUE)$values) <= 0) {
    stop("Ward-pair balance covariance matrix is not positive definite.", call. = FALSE)
  }

  statistic <- drop(t(estimates) %*% solve(covariance, estimates))
  joint_p_value <- pf(
    statistic / length(estimates),
    length(estimates),
    nrow(pair_scores) - 1,
    lower.tail = FALSE
  )

  balance_rows[[sample_name]] <- tibble(
    sample = sample_name,
    label = covariates$label,
    estimate = estimates,
    standard_error = sqrt(diag(covariance))
  )
  joint_rows[[sample_name]] <- tibble(
    sample = sample_name,
    joint_p_value,
    observations = nrow(common_data)
  )
}

table_data <- bind_rows(balance_rows) %>%
  pivot_wider(
    names_from = sample,
    values_from = c(estimate, standard_error)
  )
joint_tests <- bind_rows(joint_rows)

table_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Adjusted difference (SD)} \\\\",
  "\\cmidrule(lr){2-3}",
  " & All construction & Multifamily \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(table_data))) {
  table_lines <- c(
    table_lines,
    sprintf(
      "%s & %.3f & %.3f \\\\",
      table_data$label[i],
      table_data$`estimate_All construction`[i],
      table_data$estimate_Multifamily[i]
    ),
    sprintf(
      " & (%.3f) & (%.3f) \\\\",
      table_data$`standard_error_All construction`[i],
      table_data$standard_error_Multifamily[i]
    )
  )
}
table_lines <- c(
  table_lines,
  "\\midrule",
  sprintf(
    "Joint-test $p$-value & %.3f & %.3f \\\\",
    joint_tests$joint_p_value[joint_tests$sample == "All construction"],
    joint_tests$joint_p_value[joint_tests$sample == "Multifamily"]
  ),
  sprintf(
    "Observations & %s & %s \\\\",
    format(
      joint_tests$observations[joint_tests$sample == "All construction"],
      big.mark = ",",
      scientific = FALSE
    ),
    format(
      joint_tests$observations[joint_tests$sample == "Multifamily"],
      big.mark = ",",
      scientific = FALSE
    )
  ),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(table_lines, "../output/density_covariate_balance.tex")
