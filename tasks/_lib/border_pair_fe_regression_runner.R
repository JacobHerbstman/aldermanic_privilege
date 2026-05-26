# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_FE_regressions/code")
# bandwidth_m <- "152.4"
# sample_filter <- "multifamily"
# fe_spec <- "zonegroup_segment_year_additive"
# prune_sample <- "all"
# cluster_level <- "ward_pair"
# yvar_1 <- "log(density_far)"
# yvar_2 <- "log(density_dupac)"

script_file <- sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
)
tasks_root <- file.path(dirname(script_file), "..")

source(file.path(tasks_root, "setup_environment/code/packages.R"), local = new.env(parent = globalenv()))
source(file.path(tasks_root, "_lib/border_pair_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth_m, sample_filter, fe_spec, prune_sample, cluster_level, yvar_1, yvar_2)
}

new_cli <- length(args) >= 6 &&
  tolower(args[4]) %in% c("all", "pruned", "false", "f", "0", "no", "off", "true", "t", "1", "yes", "on") &&
  tolower(args[5]) %in% c("ward_pair", "wardpair", "pair", "segment", "segment_id")

if (new_cli) {
  bw_arg <- args[1]
  sample_filter <- args[2]
  fe_spec <- args[3]
  prune_sample_raw <- tolower(args[4])
  cluster_level_raw <- tolower(args[5])
  output_filename <- ""
  yvars <- args[6:length(args)]
  if (length(args) == 6 && grepl(",", args[6])) {
    yvars <- strsplit(args[6], ",")[[1]] |> trimws()
  }
} else if (length(args) >= 5) {
  bw_arg <- args[1]
  sample_filter <- args[2]
  fe_spec <- args[3]
  output_filename <- args[4]
  prune_sample_raw <- tolower(Sys.getenv("PRUNE_SAMPLE", "all"))
  cluster_level_raw <- tolower(Sys.getenv("CLUSTER_LEVEL", "ward_pair"))
  yvars <- args[5:length(args)]
  if (length(args) == 5 && grepl(",", args[5])) {
    yvars <- strsplit(args[5], ",")[[1]] |> trimws()
  }
} else {
  stop(
    "FATAL: Script requires args: <bandwidth_m> <sample> <fe_spec> <prune_sample> <cluster_level> <yvar1> [<yvar2> ...]",
    call. = FALSE
  )
}

bandwidth_m <- parse_bw_m(bw_arg)
distance_display <- distance_display_config()
bandwidth_label <- if (is.finite(bandwidth_m)) {
  Sys.getenv("BANDWIDTH_LABEL", format_distance_label(bandwidth_m, distance_display))
} else {
  "all"
}

if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample must be one of: all, multifamily", call. = FALSE)
}
if (length(yvars) == 0) {
  stop("No yvars provided.", call. = FALSE)
}

fe_input_path <- Sys.getenv("FE_INPUT_PATH", "../input/parcels_with_ward_distances.csv")
fe_summary_output_path <- Sys.getenv("FE_SUMMARY_OUTPUT_PATH", "")
write_tex_raw <- tolower(Sys.getenv("WRITE_TEX", ifelse(new_cli, "FALSE", "TRUE")))
write_tex <- !write_tex_raw %in% c("false", "f", "0", "no", "off")
ambiguity_input_path <- Sys.getenv("AMBIGUITY_INPUT_PATH", "")
drop_ambiguous_raw <- tolower(Sys.getenv("DROP_AMBIGUOUS_WITHIN_BW", "FALSE"))
drop_ambiguous_within_bw <- drop_ambiguous_raw %in% c("true", "t", "1", "yes", "on")
min_segment_length_ft_raw <- Sys.getenv("MIN_SEGMENT_LENGTH_FT", "")
min_segment_length_ft <- if (nzchar(min_segment_length_ft_raw)) suppressWarnings(as.numeric(min_segment_length_ft_raw)) else NA_real_
if (nzchar(min_segment_length_ft_raw) && (!is.finite(min_segment_length_ft) || min_segment_length_ft <= 0)) {
  stop("MIN_SEGMENT_LENGTH_FT must be a positive number when supplied.", call. = FALSE)
}

if (prune_sample_raw %in% c("all", "false", "f", "0", "no", "off")) {
  prune_sample <- "all"
} else if (prune_sample_raw %in% c("pruned", "true", "t", "1", "yes", "on")) {
  prune_sample <- "pruned"
} else {
  stop("PRUNE_SAMPLE must map to one of: all/false/0 or pruned/true/1", call. = FALSE)
}
confound_flags_path <- Sys.getenv("CONFOUND_FLAGS_PATH", "../input/confounded_pair_era_flags.csv")
confound_segment_flags_path <- Sys.getenv("CONFOUND_SEGMENT_FLAGS_PATH", "../input/confounded_segment_flags.csv")

if (cluster_level_raw %in% c("ward_pair", "wardpair", "pair")) {
  cluster_level <- "ward_pair"
} else if (cluster_level_raw %in% c("segment", "segment_id")) {
  cluster_level <- "segment"
} else {
  stop("CLUSTER_LEVEL must be one of: ward_pair, segment", call. = FALSE)
}

bandwidth_file_label <- if (nzchar(bandwidth_label)) bandwidth_label else "bwall"
prune_suffix <- if (prune_sample == "pruned") "_pruned" else ""
if (new_cli && !nzchar(fe_summary_output_path)) {
  fe_summary_output_path <- sprintf(
    "../temp/fe_summary_%s_%s_%s_clust_%s%s.csv",
    bandwidth_file_label,
    sample_filter,
    fe_spec,
    cluster_level,
    prune_suffix
  )
}
if (write_tex && !nzchar(output_filename)) {
  output_filename <- sprintf(
    "../output/fe_table_%s_%s_%s_clust_%s%s.tex",
    bandwidth_file_label,
    sample_filter,
    fe_spec,
    cluster_level,
    prune_suffix
  )
}

donut_m <- suppressWarnings(as.numeric(Sys.getenv("DONUT_M", Sys.getenv("DONUT_FT", "0"))))
if (nzchar(Sys.getenv("DONUT_FT", "")) && !nzchar(Sys.getenv("DONUT_M", ""))) {
  donut_m <- donut_m * 0.3048
}
if (!is.finite(donut_m) || donut_m < 0) {
  stop("DONUT_M must be a non-negative number.", call. = FALSE)
}
if (is.finite(bandwidth_m) && donut_m >= bandwidth_m) {
  stop("DONUT_M must be strictly smaller than bandwidth.", call. = FALSE)
}
if (drop_ambiguous_within_bw && !is.finite(bandwidth_m)) {
  stop("DROP_AMBIGUOUS_WITHIN_BW requires a finite bandwidth.", call. = FALSE)
}

message(sprintf("\n=== Border-Pair FE Configuration ==="))
message(sprintf("Bandwidth: %s", bandwidth_label))
message(sprintf("Sample: %s", sample_filter))
message(sprintf("FE Specification: %s", fe_spec))
message(sprintf("Pruning spec: %s", prune_sample))
message(sprintf("Cluster level: %s", cluster_level))
message(sprintf("Donut exclusion: >= %s", format_distance_label(donut_m, distance_display)))
message(sprintf("Drop corner-ambiguous parcels: %s", ifelse(drop_ambiguous_within_bw, "TRUE", "FALSE")))
message(sprintf("Minimum segment length: %s", ifelse(is.finite(min_segment_length_ft), paste0(min_segment_length_ft, "ft"), "none")))
message(sprintf("Write TeX table: %s", ifelse(write_tex, "TRUE", "FALSE")))
message(sprintf("Input: %s", fe_input_path))
message(sprintf("Output: %s", output_filename))
message(sprintf("Y variables: %s", paste(yvars, collapse = ", ")))

parcels_fe <- read_csv(
  fe_input_path,
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

if (drop_ambiguous_within_bw) {
  if (!nzchar(ambiguity_input_path)) {
    stop("DROP_AMBIGUOUS_WITHIN_BW=TRUE requires AMBIGUITY_INPUT_PATH.", call. = FALSE)
  }

  ambiguity_df <- read_csv(
    ambiguity_input_path,
    show_col_types = FALSE,
    col_types = cols(pin = col_character(), .default = col_guess())
  ) %>%
    ensure_meter_distance_columns() %>%
    mutate(
      pin = as.character(pin),
      construction_year = suppressWarnings(as.integer(construction_year))
    ) %>%
    select(pin, construction_year, nearest_other_pair_dist_m, nearest_other_pair_id)

  if (anyDuplicated(ambiguity_df[c("pin", "construction_year")]) > 0) {
    stop("Ambiguity input has duplicate pin-construction_year keys.", call. = FALSE)
  }

  parcels_fe <- parcels_fe %>%
    left_join(ambiguity_df, by = c("pin", "construction_year"), relationship = "many-to-one")
}

if (sample_filter == "all") {
  parcels_fe <- parcels_fe %>% filter(unitscount > 0)
} else {
  parcels_fe <- parcels_fe %>% filter(unitscount > 1)
}

segment_length_input_n <- nrow(parcels_fe)
segment_length_drop_n <- NA_integer_
if (is.finite(min_segment_length_ft)) {
  if (!"segment_length_ft" %in% names(parcels_fe)) {
    stop("MIN_SEGMENT_LENGTH_FT requires segment_length_ft in the FE input.", call. = FALSE)
  }
  parcels_fe <- parcels_fe %>%
    filter(!is.na(segment_length_ft), segment_length_ft >= min_segment_length_ft)
  segment_length_drop_n <- segment_length_input_n - nrow(parcels_fe)
  if (segment_length_drop_n == 0) {
    stop("MIN_SEGMENT_LENGTH_FT dropped zero observations; check the robustness cutoff.", call. = FALSE)
  }
}

parcels_fe_before_prune <- parcels_fe
prune_input_n <- NA_integer_
prune_input_pair_era_n <- NA_integer_
prune_dropped_obs_total <- NA_integer_
prune_dropped_pair_era_n <- NA_integer_
prune_dropped_segment_n <- NA_integer_
prune_missing_obs_n <- NA_integer_
prune_missing_pair_era_n <- NA_integer_
prune_missing_segment_n <- NA_integer_
parcels_fe_prune_joined <- NULL

if (prune_sample == "pruned") {
  conf_flags <- read_csv(
    confound_flags_path,
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
    confound_segment_flags_path,
    show_col_types = FALSE,
    col_select = c("ward_pair_id_dash", "era", "segment_id", "drop_confound", "drop_reason")
  ) %>%
    transmute(
      pair_dash = normalize_pair_dash(ward_pair_id_dash),
      era = as.character(era),
      segment_id = as.character(segment_id),
      keep_segment = !as.logical(drop_confound),
      segment_drop_reason = as.character(drop_reason)
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

  prune_input_n <- nrow(parcels_fe)
  prune_input_pair_era_n <- n_distinct(parcels_fe$pair_dash, parcels_fe$era)
  parcels_fe <- parcels_fe %>%
    mutate(
      missing_pair_pruning_flag = is.na(keep_pair_era),
      missing_segment_pruning_flag = is.na(keep_segment),
      missing_pruning_flag = missing_segment_pruning_flag,
      keep_pair_era = ifelse(missing_pair_pruning_flag, FALSE, keep_pair_era),
      keep_segment = ifelse(missing_segment_pruning_flag, FALSE, keep_segment),
      keep_pruned_sample = keep_segment,
      prune_drop_reason = case_when(
        missing_segment_pruning_flag ~ "missing_segment_pruning_flag",
        !keep_segment ~ paste0("segment_", segment_drop_reason),
        TRUE ~ "kept"
      )
    )

  prune_missing_obs_n <- sum(parcels_fe$missing_pruning_flag)
  prune_missing_pair_era_n <- parcels_fe %>%
    filter(missing_pruning_flag) %>%
    distinct(pair_dash, era) %>%
    nrow()
  prune_missing_segment_n <- parcels_fe %>%
    filter(missing_pruning_flag) %>%
    distinct(pair_dash, era, segment_id) %>%
    nrow()

  if (prune_missing_obs_n > 0) {
    message(
      sprintf(
        "Segment confound flags missing for %s observations across %s segment keys; treating them as dropped.",
        format(prune_missing_obs_n, big.mark = ","),
        format(prune_missing_segment_n, big.mark = ",")
      )
    )
  }

  prune_dropped_obs_total <- sum(!parcels_fe$keep_pruned_sample)
  prune_dropped_pair_era_n <- parcels_fe %>%
    filter(!keep_pruned_sample) %>%
    distinct(pair_dash, era) %>%
    nrow()
  prune_dropped_segment_n <- parcels_fe %>%
    filter(!keep_pruned_sample) %>%
    distinct(pair_dash, era, segment_id) %>%
    nrow()

  if (prune_dropped_obs_total == 0) {
    stop("Pruned FE run would drop zero observations before model filtering.", call. = FALSE)
  }

  parcels_fe_prune_joined <- parcels_fe
  parcels_fe <- parcels_fe %>% filter(keep_pruned_sample)
}

outcome_label_dict <- c(
  "density_dupac" = "DUPAC",
  "density_far" = "FAR",
  "density_lapu" = "Lot Area Per Unit (LAPU)",
  "density_bcr" = "Building Coverage Ratio (BCR)",
  "density_lps" = "Lot Size Per Story (LPS)",
  "density_spu" = "Square Feet Per Unit (SPU)",
  "arealotsf" = "Lot Area (sf)",
  "areabuilding" = "Building Area (sf)",
  "storiescount" = "Stories",
  "unitscount" = "Units",
  "bedroomscount" = "Bedrooms",
  "bathcount" = "Bathrooms"
)
rename_dict <- c(
  "strictness_own" = "Stringency Index",
  "lenient_dist" = "Lenient-Side Distance",
  "strict_dist" = "Strict-Side Distance",
  "zone_group" = "Zoning Group FE",
  "segment_id" = "Segment FE",
  "construction_year" = "Year FE",
  "ward_pair" = "Ward-Pair FE",
  "density_dupac" = "Dwelling Units Per Acre (DUPAC)",
  "density_far" = "Floor Area Ratio (FAR)"
)

fe_formulas <- list(
  zonegroup_pair_year_additive = "zone_group + ward_pair + construction_year",
  zonegroup_segment_year_additive = "zone_group + segment_id + construction_year",
  segment_year = "segment_id + construction_year"
)

fe_labels <- list(
  zonegroup_pair_year_additive = list(
    "Zoning Group FE" = "zone_group",
    "Ward-Pair FE" = "ward_pair",
    "Year FE" = "construction_year"
  ),
  zonegroup_segment_year_additive = list(
    "Zoning Group FE" = "zone_group",
    "Segment FE" = "segment_id",
    "Year FE" = "construction_year"
  ),
  segment_year = list(
    "Segment FE" = "segment_id",
    "Year FE" = "construction_year"
  )
)

if (!fe_spec %in% names(fe_formulas)) {
  stop(
    sprintf("Invalid fe_spec '%s'. Must be one of: %s", fe_spec, paste(names(fe_formulas), collapse = ", ")),
    call. = FALSE
  )
}

fe_formula_str <- fe_formulas[[fe_spec]]
fe_label_list <- fe_labels[[fe_spec]]
need_segment <- fe_spec %in% c("segment_year", "zonegroup_segment_year_additive") || cluster_level == "segment"
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair

model_sample <- function(df, yv, base_var) {
  out <- df %>%
    filter(dist_to_boundary_m >= donut_m)

  if (is.finite(bandwidth_m)) {
    out <- out %>% filter(dist_to_boundary_m <= bandwidth_m)
  }

  ambiguity_drop_n <- 0L
  if (drop_ambiguous_within_bw) {
    n_missing_ambiguity <- sum(is.na(out$nearest_other_pair_dist_m))
    if (n_missing_ambiguity > 0) {
      stop(sprintf("Missing ambiguity distances for %d filtered rows.", n_missing_ambiguity), call. = FALSE)
    }
    ambiguity_drop_n <- sum(out$nearest_other_pair_dist_m <= bandwidth_m, na.rm = TRUE)
    out <- out %>% filter(nearest_other_pair_dist_m > bandwidth_m)
  }

  if (need_segment) {
    out <- out %>% filter(!is.na(segment_id), segment_id != "")
  }

  out <- out %>% filter(is.finite(.data[[base_var]]))

  if (str_detect(yv, "^log\\(.+\\)$")) {
    out <- out %>% filter(.data[[base_var]] > 0)
  }

  list(data = out, ambiguity_drop_n = ambiguity_drop_n)
}

models <- list()
col_headers <- c()
model_summaries <- list()

for (yv in yvars) {
  base_var <- gsub("^log\\(|\\)$", "", yv)
  if (!base_var %in% names(parcels_fe)) {
    warning(sprintf("Skipping '%s' (base var '%s' not found).", yv, base_var))
    next
  }
  outcome_label <- if (base_var %in% names(outcome_label_dict)) outcome_label_dict[[base_var]] else base_var
  if (str_detect(yv, "^log\\(.+\\)$")) {
    outcome_label <- paste0("ln(", outcome_label, ")")
  }

  post_prune <- model_sample(parcels_fe, yv, base_var)
  df <- post_prune$data
  ambiguity_drop_n <- post_prune$ambiguity_drop_n

  pre_prune_n <- NA_integer_
  post_prune_n <- NA_integer_
  prune_drop_n <- NA_integer_
  prune_drop_share <- NA_real_
  common_support_pre_prune_n <- NA_integer_
  common_support_prune_drop_n <- NA_integer_
  common_support_prune_drop_share <- NA_real_
  prune_missing_model_n <- NA_integer_
  prune_flag_drop_model_n <- NA_integer_
  prune_pair_flag_drop_model_n <- NA_integer_
  prune_segment_flag_drop_model_n <- NA_integer_

  if (prune_sample == "pruned") {
    pre_prune <- model_sample(parcels_fe_prune_joined, yv, base_var)
    pre_prune_n <- nrow(pre_prune$data)
    post_prune_n <- nrow(df)
    prune_drop_n <- pre_prune_n - post_prune_n
    prune_drop_share <- ifelse(pre_prune_n > 0, prune_drop_n / pre_prune_n, NA_real_)
    prune_missing_model_n <- sum(pre_prune$data$missing_pruning_flag, na.rm = TRUE)
    prune_flag_drop_model_n <- sum(
      !pre_prune$data$missing_pruning_flag & !pre_prune$data$keep_pruned_sample,
      na.rm = TRUE
    )
    prune_pair_flag_drop_model_n <- sum(
      !pre_prune$data$missing_pair_pruning_flag & !pre_prune$data$keep_pair_era,
      na.rm = TRUE
    )
    prune_segment_flag_drop_model_n <- sum(
      !pre_prune$data$missing_segment_pruning_flag & !pre_prune$data$keep_segment,
      na.rm = TRUE
    )
    common_support_pre_prune_n <- pre_prune_n - prune_missing_model_n
    common_support_prune_drop_n <- common_support_pre_prune_n - post_prune_n
    common_support_prune_drop_share <- ifelse(
      common_support_pre_prune_n > 0,
      common_support_prune_drop_n / common_support_pre_prune_n,
      NA_real_
    )

    if (pre_prune_n == 0) {
      stop(sprintf("Pruned FE run has zero unpruned candidate observations for '%s'.", yv), call. = FALSE)
    }
    if (common_support_pre_prune_n == 0) {
      stop(sprintf("Pruned FE run has zero common-support observations for '%s'.", yv), call. = FALSE)
    }
    if (post_prune_n >= common_support_pre_prune_n) {
      stop(
        sprintf(
          "Pruned FE run did not reduce common-support observations for '%s' after model filters: pre=%s, post=%s.",
          yv,
          format(common_support_pre_prune_n, big.mark = ","),
          format(post_prune_n, big.mark = ",")
        ),
        call. = FALSE
      )
    }
    if (prune_flag_drop_model_n == 0) {
      stop(
        sprintf("Pruned FE run for '%s' dropped observations only because of missing pruning flags.", yv),
        call. = FALSE
      )
    }
  }

  if (nrow(df) == 0) {
    warning(sprintf("Skipping '%s' (no rows after filtering).", yv))
    next
  }

  y_vals <- if (str_detect(yv, "^log\\(.+\\)$")) log(df[[base_var]]) else df[[base_var]]
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(unique(y_vals)) <= 1) {
    warning(sprintf("Skipping '%s' (constant in %s sample).", yv, sample_filter))
    next
  }

  rhs_controls <- c(
    "strictness_own",
    "lenient_dist",
    "strict_dist",
    "share_white_own",
    "share_black_own",
    "median_hh_income_own",
    "share_bach_plus_own",
    "homeownership_rate_own"
  )

  fml_txt <- paste0(yv, " ~ ", paste(rhs_controls, collapse = " + "), " | ", fe_formula_str)
  m <- feols(as.formula(fml_txt), data = df, cluster = cluster_formula)
  m$custom_data <- df

  models[[length(models) + 1]] <- m
  col_headers <- c(col_headers, outcome_label)

  coef_table <- coeftable(m)
  coef_info <- coef_table["strictness_own", c("Estimate", "Std. Error", "Pr(>|t|)"), drop = FALSE]
  lenient_info <- coef_table["lenient_dist", c("Estimate", "Std. Error", "Pr(>|t|)"), drop = FALSE]
  strict_info <- coef_table["strict_dist", c("Estimate", "Std. Error", "Pr(>|t|)"), drop = FALSE]
  strictness_ci <- as.numeric(confint(m, parm = "strictness_own", level = 0.95))

  model_summaries[[length(model_summaries) + 1]] <- tibble(
    yvar = yv,
    outcome_label = outcome_label,
    estimate = unname(coef_info[1, "Estimate"]),
    se = unname(coef_info[1, "Std. Error"]),
    p_value = unname(coef_info[1, "Pr(>|t|)"]),
    ci_low = strictness_ci[1],
    ci_high = strictness_ci[2],
    lenient_slope = unname(lenient_info[1, "Estimate"]),
    lenient_slope_se = unname(lenient_info[1, "Std. Error"]),
    lenient_slope_p = unname(lenient_info[1, "Pr(>|t|)"]),
    strict_slope = unname(strict_info[1, "Estimate"]),
    strict_slope_se = unname(strict_info[1, "Std. Error"]),
    strict_slope_p = unname(strict_info[1, "Pr(>|t|)"]),
    n_obs = nobs(m),
    n_segments = dplyr::n_distinct(df$segment_id),
    n_ward_pairs = dplyr::n_distinct(df$ward_pair),
    depvar_mean = mean(df[[base_var]], na.rm = TRUE),
    bandwidth_m = bandwidth_m,
    bandwidth_label = bandwidth_label,
    sample_filter = sample_filter,
    fe_spec = fe_spec,
    prune_sample = prune_sample,
    pre_prune_n = pre_prune_n,
    post_prune_n = post_prune_n,
    prune_drop_n = prune_drop_n,
    prune_drop_share = prune_drop_share,
    common_support_pre_prune_n = common_support_pre_prune_n,
    common_support_prune_drop_n = common_support_prune_drop_n,
    common_support_prune_drop_share = common_support_prune_drop_share,
    prune_input_n = prune_input_n,
    prune_input_pair_era_n = prune_input_pair_era_n,
    prune_dropped_obs_total = prune_dropped_obs_total,
    prune_dropped_pair_era_n = prune_dropped_pair_era_n,
    prune_dropped_segment_n = prune_dropped_segment_n,
    prune_missing_obs_n = prune_missing_obs_n,
    prune_missing_pair_era_n = prune_missing_pair_era_n,
    prune_missing_segment_n = prune_missing_segment_n,
    prune_missing_model_n = prune_missing_model_n,
    prune_flag_drop_model_n = prune_flag_drop_model_n,
    prune_pair_flag_drop_model_n = prune_pair_flag_drop_model_n,
    prune_segment_flag_drop_model_n = prune_segment_flag_drop_model_n,
    cluster_level = cluster_level,
    donut_m = donut_m,
    drop_ambiguous_within_bw = drop_ambiguous_within_bw,
    ambiguity_drop_n = ambiguity_drop_n,
    min_segment_length_ft = min_segment_length_ft,
    segment_length_input_n = segment_length_input_n,
    segment_length_drop_n = segment_length_drop_n,
    ambiguity_input_path = ambiguity_input_path,
    input_path = fe_input_path,
    table_output = output_filename
  )
}

if (length(models) == 0) {
  stop("No models estimated; check yvars and data.")
}

names(models) <- col_headers

if (write_tex) {
fe_rows <- lapply(names(fe_label_list), function(x) rep("$\\checkmark$", length(models)))
names(fe_rows) <- paste0("_", names(fe_label_list))
fe_rows[["_N"]] <- vapply(models, function(x) format(nobs(x), big.mark = ","), character(1))
fe_rows[["_Dep. Var. Mean"]] <- vapply(
    models,
    function(x) sprintf("%.2f", mean(x$custom_data[[gsub("^log\\(|\\)$", "", all.vars(formula(x))[1])]], na.rm = TRUE)),
    character(1)
  )
  fe_rows[["_Ward Pairs"]] <- vapply(models, function(x) format(n_distinct(x$custom_data$ward_pair), big.mark = ","), character(1))

  etable(
    models,
    keep = "Stringency Index",
    fitstat = NULL,
    style.tex = style.tex(
      "aer",
      model.format = "",
      fixef.title = "",
      fixef.suffix = "",
      yesNo = c("$\\checkmark$", "")
    ),
    depvar = FALSE,
    digits = 2,
    headers = names(models),
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    dict = rename_dict,
    drop.section = "fixef",
    extralines = fe_rows,
    float = FALSE,
    tex = TRUE,
    file = output_filename,
    replace = TRUE
  )

  table_tex <- readLines(output_filename)
  drop_patterns <- c(
    "^\\s*Observations\\s*&",
    "^\\s*R\\$\\^2\\$\\s*&",
    "^\\s*Within R\\$\\^2\\$\\s*&"
  )
  table_tex <- table_tex[!vapply(
    table_tex,
    function(line) any(vapply(drop_patterns, grepl, logical(1), x = line)),
    logical(1)
  )]
  writeLines(table_tex, output_filename)
}

if (nzchar(fe_summary_output_path)) {
  write_csv(bind_rows(model_summaries), fe_summary_output_path)
}
