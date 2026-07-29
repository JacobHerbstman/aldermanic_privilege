# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

scores <- read_csv(
  "../input/low_discretion_alderman_scores.csv",
  show_col_types = FALSE
) %>%
  select(alderman, low_discretion_score = uncertainty_index)

if (anyDuplicated(scores$alderman) > 0) {
  stop("The low-discretion score is not unique by alderman.", call. = FALSE)
}

parcels <- read_csv(
  "../input/density_score_sensitivity_sample.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    .default = col_guess()
  ),
  guess_max = Inf
) %>%
  left_join(
    scores %>% rename(alderman_own = alderman, score_own = low_discretion_score),
    by = "alderman_own",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_neighbor = alderman, score_neighbor = low_discretion_score),
    by = "alderman_neighbor",
    relationship = "many-to-one"
  ) %>%
  filter(
    unitscount > 0,
    !is.na(score_own),
    !is.na(score_neighbor),
    score_own != score_neighbor
  ) %>%
  mutate(
    signed_distance = if_else(
      score_own > score_neighbor,
      abs(dist_to_boundary_m),
      -abs(dist_to_boundary_m)
    ),
    lenient_dist = abs(signed_distance) * as.integer(signed_distance <= 0),
    strict_dist = abs(signed_distance) * as.integer(signed_distance > 0)
  )

model_rows <- list()
for (outcome in c("density_far", "density_dupac")) {
  model_data <- parcels %>%
    filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
    mutate(outcome_value = log(.data[[outcome]]))

  model <- feols(
    as.formula(paste0(
      "outcome_value ~ score_own + lenient_dist + strict_dist + ",
      paste(controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = model_data,
    cluster = ~ward_pair,
    notes = FALSE
  )
  coefficient_table <- coeftable(model)
  used_rows <- obs(model)
  model_rows[[outcome]] <- tibble(
    outcome,
    estimate = unname(coefficient_table["score_own", "Estimate"]),
    standard_error = unname(coefficient_table["score_own", "Std. Error"]),
    p_value = unname(coefficient_table["score_own", "Pr(>|t|)"]),
    observations = nobs(model),
    dependent_variable_mean = mean(model_data[[outcome]][used_rows]),
    ward_pairs = n_distinct(model_data$ward_pair[used_rows])
  )
}

results <- bind_rows(model_rows) %>%
  mutate(
    outcome = factor(outcome, levels = c("density_far", "density_dupac")),
    stars = case_when(
      p_value <= 0.01 ~ "^{***}",
      p_value <= 0.05 ~ "^{**}",
      p_value <= 0.10 ~ "^{*}",
      TRUE ~ ""
    ),
    estimate_text = if_else(
      stars == "",
      sprintf("%.3f", estimate),
      paste0(sprintf("%.3f", estimate), "$", stars, "$")
    )
  ) %>%
  arrange(outcome)

if (nrow(results) != 2L) {
  stop("Expected FAR and DUPAC low-discretion placebo estimates.", call. = FALSE)
}

write_csv(
  results %>% select(-outcome, -stars, -estimate_text) %>% mutate(outcome = as.character(results$outcome), .before = 1),
  "../output/selected_low_discretion_density_placebo.csv"
)

writeLines(
  c(
    "\\begingroup",
    "\\centering",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & ln(FAR) & ln(DUPAC) \\\\ ",
    "\\midrule",
    paste0("Low-discretion score (1 SD) & ", paste(results$estimate_text, collapse = " & "), " \\\\ "),
    paste0(" & (", paste(sprintf("%.3f", results$standard_error), collapse = ") & ("), ") \\\\ "),
    "\\midrule",
    "Zoning Group FE & $\\checkmark$ & $\\checkmark$ \\\\ ",
    "Segment FE & $\\checkmark$ & $\\checkmark$ \\\\ ",
    "Year FE & $\\checkmark$ & $\\checkmark$ \\\\ ",
    paste0("N & ", paste(format(results$observations, big.mark = ",", trim = TRUE), collapse = " & "), " \\\\ "),
    paste0("Dep. Var. Mean & ", paste(sprintf("%.2f", results$dependent_variable_mean), collapse = " & "), " \\\\ "),
    paste0("Ward Pairs & ", paste(results$ward_pairs, collapse = " & "), " \\\\ "),
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\endgroup"
  ),
  "../output/selected_low_discretion_density_placebo.tex"
)
