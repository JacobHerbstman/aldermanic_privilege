# setwd("tasks/density_boundary_checks/code")

source("../../setup_environment/code/packages.R")

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

boundary_characteristics <- readr::read_csv(
  "../input/density_boundary_characteristics.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(projects$project_id) > 0L) {
  stop("New-construction data must be unique by project ID.")
}
if (anyDuplicated(boundary_characteristics$project_id) > 0L) {
  stop("Boundary characteristics must be unique by project ID.")
}

projects <- projects |>
  dplyr::inner_join(
    boundary_characteristics,
    by = "project_id",
    relationship = "one-to-one"
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
    is.finite(pair_average_score),
    is.finite(share_white_own),
    is.finite(share_black_own),
    is.finite(median_hh_income_own),
    is.finite(share_bach_plus_own),
    is.finite(homeownership_rate_own),
    !is.na(zone_group),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != ""
  ) |>
  dplyr::mutate(
    running_distance_ft = signed_distance_m / 0.3048,
    distance_bin = cut(
      running_distance_ft,
      breaks = seq(-500, 500, by = 100),
      labels = sprintf("bin_%02d", 1:10),
      include.lowest = TRUE,
      right = FALSE
    )
  ) |>
  dplyr::filter(
    abs(running_distance_ft) < 500,
    !is.na(distance_bin)
  )

if (nrow(projects) != nrow(boundary_characteristics)) {
  stop("Boundary characteristics do not match the density analysis sample.")
}
if (
  anyNA(projects$straight_boundary) ||
    anyNA(projects$simple_overlap_keep) ||
    anyNA(projects$share_based_keep)
) {
  stop("Some projects lack a boundary classification.")
}

sample_rules <- tibble::tribble(
  ~restriction, ~keep,
  "Main sample", "all",
  "Limited expressway or water overlap", "simple_overlap_keep",
  "Limited physical-feature or arterial overlap", "share_based_keep",
  "Straight boundary segment", "straight_boundary"
)

panel_specs <- tibble::tribble(
  ~sample, ~outcome,
  "all", "density_far",
  "all", "density_dupac",
  "multifamily", "density_far",
  "multifamily", "density_dupac"
)

robustness_rows <- list()

for (rule_i in seq_len(nrow(sample_rules))) {
  keep <- switch(
    sample_rules$keep[rule_i],
    all = rep(TRUE, nrow(projects)),
    simple_overlap_keep = projects$simple_overlap_keep,
    share_based_keep = projects$share_based_keep,
    straight_boundary = projects$straight_boundary
  )

  for (panel_i in seq_len(nrow(panel_specs))) {
    model_data <- projects |>
      dplyr::filter(
        keep,
        panel_specs$sample[panel_i] == "all" | external_multifamily
      ) |>
      dplyr::mutate(
        log_outcome = log(.data[[panel_specs$outcome[panel_i]]]),
        distance_bin = droplevels(distance_bin)
      )

    model <- fixest::feols(
      log_outcome ~
        i(distance_bin, ref = "bin_05") +
        pair_average_score +
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

    coefficient <- fixest::coeftable(model)["distance_bin::bin_06", ]
    robustness_rows[[length(robustness_rows) + 1L]] <- tibble::tibble(
      restriction = sample_rules$restriction[rule_i],
      sample = panel_specs$sample[panel_i],
      outcome = panel_specs$outcome[panel_i],
      estimate = unname(coefficient["Estimate"]),
      std_error = unname(coefficient["Std. Error"]),
      p_value = unname(coefficient["Pr(>|t|)"]),
      observations = stats::nobs(model)
    )
  }
}

robustness <- dplyr::bind_rows(robustness_rows) |>
  dplyr::mutate(
    column = dplyr::case_when(
      sample == "all" & outcome == "density_far" ~ 1L,
      sample == "all" & outcome == "density_dupac" ~ 2L,
      sample == "multifamily" & outcome == "density_far" ~ 3L,
      sample == "multifamily" & outcome == "density_dupac" ~ 4L
    )
  ) |>
  dplyr::arrange(factor(restriction, sample_rules$restriction), column)

stars <- function(p_value) {
  dplyr::case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

robustness_lines <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{All Construction} & \\multicolumn{2}{c}{Multifamily} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & ln(FAR) & ln(DUPAC) & ln(FAR) & ln(DUPAC) \\\\",
  "\\midrule"
)

for (restriction_name in sample_rules$restriction) {
  rows <- robustness |>
    dplyr::filter(restriction == restriction_name) |>
    dplyr::arrange(column)
  robustness_lines <- c(
    robustness_lines,
    paste0(
      restriction_name,
      " & ",
      paste0(
        sprintf("%.3f", rows$estimate),
        stars(rows$p_value),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(
      " & ",
      paste0("(", sprintf("%.3f", rows$std_error), ")", collapse = " & "),
      " \\\\"
    ),
    paste0(
      "Observations & ",
      paste(
        trimws(format(rows$observations, big.mark = ",")),
        collapse = " & "
      ),
      " \\\\"
    ),
    if (restriction_name != tail(sample_rules$restriction, 1L)) {
      "\\addlinespace"
    } else {
      character()
    }
  )
}

writeLines(
  c(
    robustness_lines,
    "\\bottomrule",
    "\\end{tabular}"
  ),
  "../output/density_boundary_robustness.tex"
)

location_measures <- tibble::tribble(
  ~variable, ~label,
  "distance_to_cbd_miles", "Distance to downtown (miles)",
  "distance_to_school_miles", "Distance to nearest school (miles)",
  "distance_to_park_miles", "Distance to nearest park (miles)",
  "distance_to_lake_miles", "Distance to Lake Michigan (miles)"
)

continuity_rows <- list()

for (sample_name in c("all", "multifamily")) {
  sample_data <- projects |>
    dplyr::filter(sample_name == "all" | external_multifamily)

  for (i in seq_len(nrow(location_measures))) {
    model <- fixest::feols(
      stats::as.formula(paste0(
        location_measures$variable[i],
        " ~ i(distance_bin, ref = 'bin_05')",
        " | segment_id + construction_year"
      )),
      data = sample_data,
      cluster = ~ward_pair,
      warn = FALSE,
      notes = FALSE
    )

    coefficient <- fixest::coeftable(model)["distance_bin::bin_06", ]
    continuity_rows[[length(continuity_rows) + 1L]] <- tibble::tibble(
      sample = sample_name,
      variable = location_measures$variable[i],
      label = location_measures$label[i],
      estimate = unname(coefficient["Estimate"]),
      std_error = unname(coefficient["Std. Error"]),
      p_value = unname(coefficient["Pr(>|t|)"]),
      observations = stats::nobs(model)
    )
  }
}

continuity <- dplyr::bind_rows(continuity_rows)
continuity_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & All Construction & Multifamily \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(location_measures))) {
  rows <- continuity |>
    dplyr::filter(variable == location_measures$variable[i]) |>
    dplyr::arrange(factor(sample, c("all", "multifamily")))
  continuity_lines <- c(
    continuity_lines,
    paste0(
      location_measures$label[i],
      " & ",
      paste0(
        sprintf("%.3f", rows$estimate),
        stars(rows$p_value),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(
      " & ",
      paste0("(", sprintf("%.3f", rows$std_error), ")", collapse = " & "),
      " \\\\"
    )
  )
}

continuity_n <- continuity |>
  dplyr::distinct(sample, observations) |>
  dplyr::arrange(factor(sample, c("all", "multifamily")))

writeLines(
  c(
    continuity_lines,
    "\\midrule",
    paste0(
      "Observations & ",
      paste(
        trimws(format(continuity_n$observations, big.mark = ",")),
        collapse = " & "
      ),
      " \\\\"
    ),
    "\\bottomrule",
    "\\end{tabular}"
  ),
  "../output/density_location_continuity.tex"
)
