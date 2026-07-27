# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

covariates <- tibble::tribble(
  ~covariate, ~label,
  "share_white_own", "Share White",
  "share_black_own", "Share Black",
  "median_hh_income_own", "Median household income",
  "share_bach_plus_own", "Bachelor's degree or higher share",
  "homeownership_rate_own", "Homeownership rate"
)

scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    cutoff == 2022L,
    variant == "income_added_back"
  ) |>
  dplyr::select(alderman, score)

if (nrow(scores) == 0L || anyDuplicated(scores$alderman)) {
  stop("The income-only score crosswalk failed validation.")
}

score_map <- tibble::deframe(scores)

projects <- readr::read_csv(
  "../output/provisional_validated_density_input.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(
    score_own = unname(score_map[alderman_own]),
    score_neighbor = unname(score_map[alderman_neighbor]),
    side = as.integer(score_own > score_neighbor),
    pair_average_score = (score_own + score_neighbor) / 2,
    distance_m = distance_to_boundary_ft * 0.3048,
    lenient_dist = distance_m * as.integer(side == 0L),
    strict_dist = distance_m * as.integer(side == 1L)
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
    is.finite(score_own),
    is.finite(score_neighbor),
    is.finite(pair_average_score),
    !is.na(zone_group),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != ""
  )

balance_rows <- list()
joint_rows <- list()

for (sample_name in c("All construction", "Multifamily")) {
  sample_data <- if (sample_name == "All construction") {
    projects
  } else {
    dplyr::filter(projects, external_multifamily)
  }

  common_data <- sample_data |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(covariates$covariate),
        is.finite
      )
    )

  common_data[, covariates$covariate] <- scale(
    as.matrix(common_data[, covariates$covariate])
  )

  estimates <- numeric(nrow(covariates))
  influence <- matrix(
    NA_real_,
    nrow(common_data),
    nrow(covariates)
  )
  cluster_scales <- numeric(nrow(covariates))

  for (i in seq_len(nrow(covariates))) {
    model <- fixest::feols(
      stats::as.formula(paste0(
        covariates$covariate[i],
        " ~ side + pair_average_score + lenient_dist + strict_dist",
        " | zone_group + segment_id + construction_year"
      )),
      data = common_data,
      warn = FALSE,
      notes = FALSE
    )

    if (!identical(fixest::obs(model), seq_len(nrow(common_data)))) {
      stop("Balance equations do not use the full common sample.")
    }

    coefficient_index <- match("side", names(stats::coef(model)))
    bread <- model$cov.unscaled
    if (is.null(bread)) {
      bread <- solve(-model$hessian)
    }

    estimates[i] <- stats::coef(model)[coefficient_index]
    influence[, i] <- drop(
      model$scores %*% bread[, coefficient_index]
    )

    raw_variance <- drop(crossprod(rowsum(
      matrix(influence[, i], ncol = 1),
      common_data$ward_pair
    )))
    clustered_variance <- stats::vcov(
      model,
      cluster = ~ward_pair,
      vcov_fix = FALSE
    )[coefficient_index, coefficient_index]
    cluster_scales[i] <- clustered_variance / raw_variance
  }

  pair_influence <- sweep(
    influence,
    2,
    sqrt(cluster_scales),
    "*"
  )
  pair_scores <- rowsum(
    pair_influence,
    common_data$ward_pair,
    reorder = TRUE
  )
  covariance <- crossprod(pair_scores)
  covariance <- (covariance + t(covariance)) / 2

  if (
    min(eigen(
      covariance,
      symmetric = TRUE,
      only.values = TRUE
    )$values) <= 0
  ) {
    stop("The balance covariance matrix is not positive definite.")
  }

  statistic <- drop(
    t(estimates) %*% solve(covariance, estimates)
  )
  joint_p_value <- stats::pf(
    statistic / length(estimates),
    length(estimates),
    nrow(pair_scores) - 1L,
    lower.tail = FALSE
  )

  balance_rows[[sample_name]] <- tibble::tibble(
    sample = sample_name,
    label = covariates$label,
    estimate = estimates,
    standard_error = sqrt(diag(covariance))
  )
  joint_rows[[sample_name]] <- tibble::tibble(
    sample = sample_name,
    joint_p_value,
    observations = nrow(common_data)
  )
}

table_data <- dplyr::bind_rows(balance_rows) |>
  tidyr::pivot_wider(
    names_from = sample,
    values_from = c(estimate, standard_error)
  )
joint_tests <- dplyr::bind_rows(joint_rows)

table_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Coefficient on More-Stringent Side} \\\\",
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
    joint_tests$joint_p_value[
      joint_tests$sample == "All construction"
    ],
    joint_tests$joint_p_value[
      joint_tests$sample == "Multifamily"
    ]
  ),
  sprintf(
    "Observations & %s & %s \\\\",
    format(
      joint_tests$observations[
        joint_tests$sample == "All construction"
      ],
      big.mark = ","
    ),
    format(
      joint_tests$observations[
        joint_tests$sample == "Multifamily"
      ],
      big.mark = ","
    )
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(
  table_lines,
  "../output/binary_density_balance_income_added_back.tex"
)
