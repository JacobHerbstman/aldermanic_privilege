# setwd("tasks/estimate_alderman_permit_effects/code")
# Alderman effects on block permit applications, identified by changes in who represents a block: the 2015 remap and
# turnover. Poisson regressions of block-year counts on alderman indicators with block fixed effects and either year
# fixed effects (all changes) or 2003-ward-by-year fixed effects (only blocks of the same 2003 ward represented by
# different aldermen, which guards against ward-level trends). Effects are log points relative to the average
# alderman, shrunk toward zero by empirical Bayes; a negative effect means fewer applications.
designs <- c(all_changes = "block_id + year", within_2003_ward = "block_id + ward_2003_map^year")
outcomes <- c(high_discretion = "n_high_discretion", low_discretion = "n_low_discretion_nosigns")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

panel <- arrow::read_parquet("../output/block_alderman_year_panel.parquet")
# The reference alderman is the one with the most block-years; effects are re-expressed relative to the mean below.
reference_alderman <- names(which.max(table(panel$alderman)))

alderman_effects <- function(data, outcome, fixed_effects) {
  model <- fixest::fepois(stats::as.formula(sprintf("%s ~ i(alderman, ref = '%s') | %s", outcome, reference_alderman,
    fixed_effects)), data = data, cluster = ~block_id, notes = FALSE)
  coefficients <- stats::coef(model)
  aldermen <- c(reference_alderman, sub("^alderman::", "", names(coefficients)))
  # Effects relative to the unweighted mean across identified aldermen, with their covariance.
  demean <- diag(length(aldermen)) - 1 / length(aldermen)
  covariance <- rbind(0, cbind(0, stats::vcov(model)))
  tibble(alderman = aldermen, estimate = drop(demean %*% c(0, coefficients)),
    std_error = sqrt(diag(demean %*% covariance %*% t(demean))), observations = stats::nobs(model))
}
shrink <- function(effects) {
  signal_variance <- max(stats::var(effects$estimate) - mean(effects$std_error^2), 0)
  effects |> mutate(shrinkage = signal_variance / (signal_variance + std_error^2),
    shrunk_estimate = shrinkage * estimate, signal_sd = sqrt(signal_variance))
}

exposure <- panel |>
  summarise(block_years = n(), first_year = min(year), last_year = max(year), .by = alderman)
effects <- bind_rows(lapply(names(designs), function(design) {
  bind_rows(lapply(names(outcomes), function(outcome) {
    alderman_effects(panel, outcomes[[outcome]], designs[[design]]) |> shrink() |> mutate(design, outcome, .before = 1)
  }))
})) |>
  left_join(exposure, by = "alderman", relationship = "many-to-one")
SaveData(effects, c("design", "outcome", "alderman"), "../output/alderman_permit_effects.csv")

# Stability of the all-changes high-discretion effects across two splits of the data: odd and even years (over time),
# and blocks with odd and even census block numbers (over space; block numbers interleave within tracts).
split_estimates <- function(split, groups) {
  bind_rows(lapply(names(groups), function(group) {
    alderman_effects(groups[[group]], outcomes[["high_discretion"]], designs[["all_changes"]]) |>
      select(alderman, estimate) |>
      mutate(split, group, .before = 1)
  }))
}
block_number_is_odd <- as.integer(substr(panel$block_id, 15, 15)) %% 2L == 1L
splits <- bind_rows(
  split_estimates("year_parity", list(odd = filter(panel, year %% 2L == 1L), even = filter(panel, year %% 2L == 0L))),
  split_estimates("block_number_parity", list(odd = panel[block_number_is_odd, ], even = panel[!block_number_is_odd, ]))
) |>
  tidyr::pivot_wider(names_from = group, values_from = estimate) |>
  filter(!is.na(odd), !is.na(even))
SaveData(splits, c("split", "alderman"), "../output/alderman_permit_effects_split_samples.csv")
