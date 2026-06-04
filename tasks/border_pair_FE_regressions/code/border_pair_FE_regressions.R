# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_FE_regressions/code")
# bandwidth_m <- 152.4
# bandwidth_label <- "500ft"
# fe_spec <- "zonegroup_segment_year_additive"
# prune_sample <- "all"
# cluster_level <- "ward_pair"
# yvar_1 <- "log(density_far)"
# yvar_2 <- "log(density_dupac)"

source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_m, bandwidth_label, fe_spec, prune_sample, cluster_level, yvar_1, yvar_2)
}

if (length(cli_args) < 7) {
  stop(
    "FATAL: Script requires at least 7 args: <bandwidth_m> <bandwidth_label> <fe_spec> <prune_sample> <cluster_level> <yvar1> [<yvar2> ...].",
    call. = FALSE
  )
}

bandwidth_m <- parse_bw_m(cli_args[1])
bandwidth_label <- cli_args[2]
fe_spec <- cli_args[3]
prune_sample <- tolower(cli_args[4])
cluster_level <- tolower(cli_args[5])
yvars <- cli_args[6:length(cli_args)]

if (!prune_sample %in% c("all", "pruned")) {
  stop("prune_sample must be one of: all, pruned", call. = FALSE)
}
if (!cluster_level %in% c("ward_pair", "segment")) {
  stop("cluster_level must be one of: ward_pair, segment", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}

fe_formulas <- list(
  zonegroup_pair_year_additive = "zone_group + ward_pair + construction_year",
  zonegroup_segment_year_additive = "zone_group + segment_id + construction_year",
  segment_year = "segment_id + construction_year"
)
if (!fe_spec %in% names(fe_formulas)) {
  stop(
    sprintf("Invalid fe_spec '%s'. Must be one of: %s", fe_spec, paste(names(fe_formulas), collapse = ", ")),
    call. = FALSE
  )
}

outcome_label_dict <- c(
  "density_dupac" = "DUPAC",
  "density_far" = "FAR"
)
controls <- c(
  "strictness_own",
  "lenient_dist",
  "strict_dist",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)
needs_segment <- fe_spec %in% c("segment_year", "zonegroup_segment_year_additive") || cluster_level == "segment"
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair
prune_suffix <- if (prune_sample == "pruned") "_pruned" else ""

parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year)),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022
  )

if (prune_sample == "pruned") {
  conf_flags <- read_csv(
    "../input/confounded_pair_era_flags.csv",
    show_col_types = FALSE,
    col_select = c("ward_pair_id_dash", "era", "drop_confound")
  ) %>%
    transmute(
      pair_dash = normalize_pair_dash(ward_pair_id_dash),
      era = as.character(era),
      keep_pair_era = !as.logical(drop_confound)
    ) %>%
    distinct()

  segment_flags <- read_csv(
    "../input/confounded_segment_flags.csv",
    show_col_types = FALSE,
    col_select = c("ward_pair_id_dash", "era", "segment_id", "drop_confound")
  ) %>%
    transmute(
      pair_dash = normalize_pair_dash(ward_pair_id_dash),
      era = as.character(era),
      segment_id = as.character(segment_id),
      keep_segment = !as.logical(drop_confound)
    ) %>%
    distinct()

  if (anyNA(conf_flags$pair_dash) || anyNA(conf_flags$era)) {
    stop("Confound flags have invalid pair/era keys.", call. = FALSE)
  }
  if (anyDuplicated(conf_flags[, c("pair_dash", "era")]) > 0) {
    stop("Confound flags contain duplicate pair-era keys.", call. = FALSE)
  }
  if (anyNA(segment_flags$pair_dash) || anyNA(segment_flags$era) || anyNA(segment_flags$segment_id)) {
    stop("Segment confound flags have invalid pair/era/segment keys.", call. = FALSE)
  }
  if (anyDuplicated(segment_flags[, c("pair_dash", "era", "segment_id")]) > 0) {
    stop("Segment confound flags contain duplicate pair-era-segment keys.", call. = FALSE)
  }
}

summary_rows <- list()
for (sample_filter in c("all", "multifamily")) {
  sample_label <- if (sample_filter == "all") "All Construction" else "Multifamily"

  df_sample <- parcels
  if (sample_filter == "all") {
    df_sample <- df_sample %>% filter(unitscount > 0)
  } else {
    df_sample <- df_sample %>% filter(unitscount > 1)
  }

  if (prune_sample == "pruned") {
    df_sample <- df_sample %>%
      mutate(
        pair_dash = normalize_pair_dash(ward_pair),
        era = era_from_year(construction_year)
      ) %>%
      left_join(conf_flags, by = c("pair_dash", "era"), relationship = "many-to-one") %>%
      left_join(segment_flags, by = c("pair_dash", "era", "segment_id"), relationship = "many-to-one")

    if (anyNA(df_sample$pair_dash) || anyNA(df_sample$era)) {
      stop("Pruned FE sample has invalid ward-pair or era keys before joining confound flags.", call. = FALSE)
    }

    df_sample <- df_sample %>%
      mutate(keep_segment = if_else(is.na(keep_segment), FALSE, keep_segment))

    if (sum(!df_sample$keep_segment) == 0) {
      stop("Pruned FE run would drop zero observations before model filtering.", call. = FALSE)
    }

    df_sample <- df_sample %>% filter(keep_segment)
  }

  for (yvar in yvars) {
    base_var <- gsub("^log\\(|\\)$", "", yvar)
    if (!base_var %in% names(df_sample)) {
      stop(sprintf("Outcome variable '%s' not found.", base_var), call. = FALSE)
    }

    df <- df_sample
    if (is.finite(bandwidth_m)) {
      df <- df %>% filter(dist_to_boundary_m <= bandwidth_m)
    }
    if (needs_segment) {
      df <- df %>% filter(!is.na(segment_id), segment_id != "")
    }
    df <- df %>% filter(is.finite(.data[[base_var]]))
    if (str_detect(yvar, "^log\\(.+\\)$")) {
      df <- df %>% filter(.data[[base_var]] > 0)
    }
    if (nrow(df) == 0) {
      stop(sprintf("No rows remain for '%s' after filtering.", yvar), call. = FALSE)
    }

    y_vals <- if (str_detect(yvar, "^log\\(.+\\)$")) log(df[[base_var]]) else df[[base_var]]
    y_vals <- y_vals[is.finite(y_vals)]
    if (length(unique(y_vals)) <= 1) {
      stop(sprintf("Outcome '%s' is constant after filtering.", yvar), call. = FALSE)
    }

    outcome_label <- if (base_var %in% names(outcome_label_dict)) outcome_label_dict[[base_var]] else base_var
    if (str_detect(yvar, "^log\\(.+\\)$")) {
      outcome_label <- paste0("ln(", outcome_label, ")")
    }

    model <- feols(
      as.formula(paste0(
        yvar,
        " ~ ",
        paste(controls, collapse = " + "),
        " | ",
        fe_formulas[[fe_spec]]
      )),
      data = df,
      cluster = cluster_formula
    )

    coef_table <- coeftable(model)
    if (!"strictness_own" %in% rownames(coef_table)) {
      stop(sprintf("Model failed to estimate strictness_own for '%s'.", yvar), call. = FALSE)
    }

    summary_i <- tibble(
      sample_label = sample_label,
      yvar = yvar,
      outcome_label = outcome_label,
      estimate = unname(coef_table["strictness_own", "Estimate"]),
      se = unname(coef_table["strictness_own", "Std. Error"]),
      p_value = unname(coef_table["strictness_own", "Pr(>|t|)"]),
      n_obs = nobs(model),
      n_ward_pairs = n_distinct(df$ward_pair),
      depvar_mean = mean(df[[base_var]], na.rm = TRUE)
    )
    summary_rows[[length(summary_rows) + 1L]] <- summary_i
  }
}

summaries <- bind_rows(summary_rows) %>%
  mutate(
    outcome_short = case_when(
      outcome_label == "ln(FAR)" ~ "FAR",
      outcome_label == "ln(DUPAC)" ~ "DUPAC",
      TRUE ~ outcome_label
    )
  )

expected_rows <- tidyr::expand_grid(
  sample_label = c("All Construction", "Multifamily"),
  yvar = c("log(density_far)", "log(density_dupac)")
)

summary_keys <- summaries %>%
  count(sample_label, yvar, name = "n")

missing_rows <- anti_join(
  expected_rows,
  summary_keys,
  by = c("sample_label", "yvar")
)

duplicate_rows <- summary_keys %>% filter(n > 1)

if (nrow(missing_rows) > 0) {
  stop("Missing expected density FE summary rows.", call. = FALSE)
}
if (nrow(duplicate_rows) > 0) {
  stop("Duplicate density FE summary rows.", call. = FALSE)
}

ordered <- summaries %>%
  mutate(
    sample_order = match(sample_label, c("All Construction", "Multifamily")),
    outcome_order = match(outcome_short, c("FAR", "DUPAC")),
    star_text = case_when(
      !is.finite(p_value) ~ "",
      p_value <= 0.01 ~ "***",
      p_value <= 0.05 ~ "**",
      p_value <= 0.1 ~ "*",
      TRUE ~ ""
    ),
    estimate_text = if_else(
      star_text == "",
      sprintf("%.3f", estimate),
      sprintf("%.3f$^{%s}$", estimate, star_text)
    ),
    se_text = sprintf("(%.3f)", se)
  ) %>%
  arrange(sample_order, outcome_order)

if (nrow(ordered) != 4) {
  stop("Expected exactly four rows in the combined density FE table.", call. = FALSE)
}

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "   \\toprule",
  "                    & \\multicolumn{2}{c}{All Construction} & \\multicolumn{2}{c}{Multifamily} \\\\",
  "                    & ln(FAR)       & ln(DUPAC)      & ln(FAR)       & ln(DUPAC) \\\\",
  "                    & (1)           & (2)            & (3)           & (4) \\\\",
  "   \\midrule",
  paste0(
    "   Stringency Index & ",
    paste(ordered$estimate_text, collapse = " & "),
    " \\\\"
  ),
  paste0(
    "                    & ",
    paste(ordered$se_text, collapse = " & "),
    " \\\\"
  ),
  "    \\\\",
  "   Zoning Group FE  & $\\checkmark$  & $\\checkmark$   & $\\checkmark$  & $\\checkmark$ \\\\",
  "   Segment FE       & $\\checkmark$  & $\\checkmark$   & $\\checkmark$  & $\\checkmark$ \\\\",
  "   Year FE          & $\\checkmark$  & $\\checkmark$   & $\\checkmark$  & $\\checkmark$ \\\\",
  paste0("   N                & ", paste(trimws(format(ordered$n_obs, big.mark = ",")), collapse = " & "), " \\\\"),
  paste0("   Dep. Var. Mean   & ", paste(sprintf("%.2f", ordered$depvar_mean), collapse = " & "), " \\\\"),
  paste0("   Ward Pairs       & ", paste(trimws(format(ordered$n_ward_pairs, big.mark = ",")), collapse = " & "), " \\\\"),
  "   \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(
  table_lines,
  sprintf(
    "../output/fe_table_%s_all_multifamily_%s_clust_%s%s.tex",
    bandwidth_label,
    fe_spec,
    cluster_level,
    prune_suffix
  )
)
