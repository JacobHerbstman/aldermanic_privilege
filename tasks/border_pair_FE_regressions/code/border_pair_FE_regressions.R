# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_FE_regressions/code")
# bandwidth_m <- 152.4
# sample_filter <- "multifamily"
# fe_spec <- "zonegroup_segment_year_additive"
# prune_sample <- "all"
# cluster_level <- "ward_pair"
# yvar_1 <- "log(density_far)"
# yvar_2 <- "log(density_dupac)"

source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth_m, sample_filter, fe_spec, prune_sample, cluster_level, yvar_1, yvar_2)
}

if (length(args) < 7) {
  stop(
    "Usage: Rscript border_pair_FE_regressions.R <bandwidth_m> <sample> <fe_spec> <prune_sample> <cluster_level> <yvar1> [<yvar2> ...]",
    call. = FALSE
  )
}

bandwidth_m <- parse_bw_m(args[1])
sample_filter <- args[2]
fe_spec <- args[3]
prune_sample <- tolower(args[4])
cluster_level <- tolower(args[5])
yvars <- args[6:length(args)]

if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample must be one of: all, multifamily", call. = FALSE)
}
if (!prune_sample %in% c("all", "pruned")) {
  stop("prune_sample must be one of: all, pruned", call. = FALSE)
}
if (!cluster_level %in% c("ward_pair", "segment")) {
  stop("cluster_level must be one of: ward_pair, segment", call. = FALSE)
}
if (length(yvars) == 0) {
  stop("No yvars provided.", call. = FALSE)
}

distance_display <- distance_display_config()
bandwidth_label <- if (is.finite(bandwidth_m)) {
  format_distance_label(bandwidth_m, distance_display)
} else {
  "all"
}
prune_suffix <- if (prune_sample == "pruned") "_pruned" else ""

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

parcels_fe <- read_csv(
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

if (sample_filter == "all") {
  parcels_fe <- parcels_fe %>% filter(unitscount > 0)
} else {
  parcels_fe <- parcels_fe %>% filter(unitscount > 1)
}

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

  parcels_fe <- parcels_fe %>%
    mutate(
      pair_dash = normalize_pair_dash(ward_pair),
      era = era_from_year(construction_year)
    ) %>%
    left_join(conf_flags, by = c("pair_dash", "era"), relationship = "many-to-one") %>%
    left_join(segment_flags, by = c("pair_dash", "era", "segment_id"), relationship = "many-to-one")

  if (anyNA(parcels_fe$pair_dash) || anyNA(parcels_fe$era)) {
    stop("Pruned FE sample has invalid ward-pair or era keys before joining confound flags.", call. = FALSE)
  }

  parcels_fe <- parcels_fe %>%
    mutate(keep_segment = if_else(is.na(keep_segment), FALSE, keep_segment))

  if (sum(!parcels_fe$keep_segment) == 0) {
    stop("Pruned FE run would drop zero observations before model filtering.", call. = FALSE)
  }

  parcels_fe <- parcels_fe %>% filter(keep_segment)
}

needs_segment <- fe_spec %in% c("segment_year", "zonegroup_segment_year_additive") || cluster_level == "segment"
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair

model_summaries <- list()
for (yvar in yvars) {
  base_var <- gsub("^log\\(|\\)$", "", yvar)
  if (!base_var %in% names(parcels_fe)) {
    stop(sprintf("Outcome variable '%s' not found.", base_var), call. = FALSE)
  }

  df <- parcels_fe
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
      " ~ strictness_own + lenient_dist + strict_dist + share_white_own + ",
      "share_black_own + median_hh_income_own + share_bach_plus_own + ",
      "homeownership_rate_own | ",
      fe_formulas[[fe_spec]]
    )),
    data = df,
    cluster = cluster_formula
  )

  coef_table <- coeftable(model)

  model_summaries[[length(model_summaries) + 1]] <- tibble(
    yvar = yvar,
    outcome_label = outcome_label,
    estimate = unname(coef_table["strictness_own", "Estimate"]),
    se = unname(coef_table["strictness_own", "Std. Error"]),
    p_value = unname(coef_table["strictness_own", "Pr(>|t|)"]),
    n_obs = nobs(model),
    n_ward_pairs = n_distinct(df$ward_pair),
    depvar_mean = mean(df[[base_var]], na.rm = TRUE)
  )
}

write_csv(
  bind_rows(model_summaries),
  sprintf(
    "../temp/fe_summary_%s_%s_%s_clust_%s%s.csv",
    bandwidth_label,
    sample_filter,
    fe_spec,
    cluster_level,
    prune_suffix
  )
)
