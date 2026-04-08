source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_FE_regressions/code")
# bw_ft <- "500"
# sample_filter <- "multifamily"
# fe_spec <- "zonegroup_segment_year_additive"
# output_filename <- "../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex"
# yvar_1 <- "log(density_far)"
# yvar_2 <- "log(density_dupac)"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bw_ft, sample_filter, fe_spec, output_filename, yvar_1, yvar_2)
}

if (length(args) >= 5) {
  bw_arg <- args[1]
  sample_filter <- args[2]
  fe_spec <- args[3]
  output_filename <- args[4]
  yvars <- args[5:length(args)]
  if (length(args) == 5 && grepl(",", args[5])) {
    yvars <- strsplit(args[5], ",")[[1]] |> trimws()
  }
} else {
  stop(
    "FATAL: Script requires args: <bw_ft> <sample> <fe_spec> <output_filename> <yvar1> [<yvar2> ...]",
    call. = FALSE
  )
}

bw_ft <- parse_bw_ft(bw_arg)
bw_label <- if (is.finite(bw_ft)) as.character(as.integer(round(bw_ft))) else "all"

if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample must be one of: all, multifamily", call. = FALSE)
}
if (length(yvars) == 0) {
  stop("No yvars provided.", call. = FALSE)
}

fe_input_path <- Sys.getenv("FE_INPUT_PATH", "../input/parcels_with_ward_distances.csv")
fe_summary_output_path <- Sys.getenv("FE_SUMMARY_OUTPUT_PATH", "")

prune_sample_raw <- tolower(Sys.getenv("PRUNE_SAMPLE", "all"))
if (prune_sample_raw %in% c("all", "false", "f", "0", "no", "off")) {
  prune_sample <- "all"
} else if (prune_sample_raw %in% c("pruned", "true", "t", "1", "yes", "on")) {
  prune_sample <- "pruned"
} else {
  stop("PRUNE_SAMPLE must map to one of: all/false/0 or pruned/true/1", call. = FALSE)
}
confound_flags_path <- Sys.getenv("CONFOUND_FLAGS_PATH", "../input/confounded_pair_era_flags.csv")

cluster_level_raw <- tolower(Sys.getenv("CLUSTER_LEVEL", "ward_pair"))
if (cluster_level_raw %in% c("ward_pair", "wardpair", "pair")) {
  cluster_level <- "ward_pair"
} else if (cluster_level_raw %in% c("segment", "segment_id")) {
  cluster_level <- "segment"
} else {
  stop("CLUSTER_LEVEL must be one of: ward_pair, segment", call. = FALSE)
}

donut_ft <- suppressWarnings(as.numeric(Sys.getenv("DONUT_FT", "0")))
if (!is.finite(donut_ft) || donut_ft < 0) {
  stop("DONUT_FT must be a non-negative number.", call. = FALSE)
}
if (is.finite(bw_ft) && donut_ft >= bw_ft) {
  stop("DONUT_FT must be strictly smaller than bandwidth.", call. = FALSE)
}

message(sprintf("\n=== Border-Pair FE Configuration ==="))
message(sprintf("Bandwidth: %s", if (is.finite(bw_ft)) sprintf("%.0f ft", bw_ft) else "all distances"))
message(sprintf("Sample: %s", sample_filter))
message(sprintf("FE Specification: %s", fe_spec))
message(sprintf("Pruning spec: %s", prune_sample))
message(sprintf("Cluster level: %s", cluster_level))
message(sprintf("Donut exclusion: >= %.0f ft", donut_ft))
message(sprintf("Input: %s", fe_input_path))
message(sprintf("Output: %s", output_filename))
message(sprintf("Y variables: %s", paste(yvars, collapse = ", ")))

parcels_fe <- read_csv(fe_input_path, show_col_types = FALSE) %>%
  mutate(
    strictness_own = strictness_own / sd(strictness_own, na.rm = TRUE),
    zone_group = zone_group_from_code(zone_code),
    lenient_dist = abs(signed_distance) * as.integer(signed_distance <= 0),
    strict_dist = abs(signed_distance) * as.integer(signed_distance > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006
  )

if (sample_filter == "all") {
  parcels_fe <- parcels_fe %>% filter(unitscount > 0)
} else {
  parcels_fe <- parcels_fe %>% filter(unitscount > 1)
}

if (prune_sample == "pruned") {
  if (!file.exists(confound_flags_path)) {
    stop(sprintf("Missing confound flags file for pruned run: %s", confound_flags_path), call. = FALSE)
  }

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

  if (anyNA(conf_flags$pair_dash) || anyNA(conf_flags$era)) {
    stop("Confound flags have invalid pair/era keys.", call. = FALSE)
  }
  if (anyDuplicated(conf_flags[, c("pair_dash", "era")]) > 0) {
    stop("Confound flags contain duplicate pair-era keys.", call. = FALSE)
  }

  parcels_fe <- parcels_fe %>%
    mutate(
      pair_dash = normalize_pair_dash(ward_pair),
      era = era_from_year(construction_year)
    ) %>%
    left_join(conf_flags, by = c("pair_dash", "era"))

  n_missing <- sum(is.na(parcels_fe$keep_pair_era))
  if (n_missing > 0) {
    parcels_fe <- parcels_fe %>%
      mutate(keep_pair_era = ifelse(is.na(keep_pair_era), FALSE, keep_pair_era))
  }

  parcels_fe <- parcels_fe %>% filter(keep_pair_era)
}

pretty_label <- function(v) {
  b <- gsub("^log\\(|\\)$", "", v)
  dict <- c(
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
  lab <- ifelse(b %in% names(dict), dict[[b]], b)
  if (str_detect(v, "^log\\(.+\\)$")) paste0("ln(", lab, ")") else lab
}

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

models <- list()
col_headers <- c()
model_summaries <- list()

for (yv in yvars) {
  base_var <- gsub("^log\\(|\\)$", "", yv)
  if (!base_var %in% names(parcels_fe)) {
    warning(sprintf("Skipping '%s' (base var '%s' not found).", yv, base_var))
    next
  }

  df <- parcels_fe %>%
    filter(dist_to_boundary >= donut_ft)

  if (is.finite(bw_ft)) {
    df <- df %>% filter(dist_to_boundary <= bw_ft)
  }
  if (need_segment) {
    df <- df %>% filter(!is.na(segment_id), segment_id != "")
  }
  if (str_detect(yv, "^log\\(.+\\)$")) {
    df <- df %>% filter(.data[[base_var]] > 0)
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
  col_headers <- c(col_headers, pretty_label(yv))

  coef_table <- coeftable(m)
  coef_info <- coef_table["strictness_own", c("Estimate", "Std. Error", "Pr(>|t|)"), drop = FALSE]
  lenient_info <- coef_table["lenient_dist", c("Estimate", "Std. Error", "Pr(>|t|)"), drop = FALSE]
  strict_info <- coef_table["strict_dist", c("Estimate", "Std. Error", "Pr(>|t|)"), drop = FALSE]

  model_summaries[[length(model_summaries) + 1]] <- tibble(
    yvar = yv,
    outcome_label = pretty_label(yv),
    estimate = unname(coef_info[1, "Estimate"]),
    se = unname(coef_info[1, "Std. Error"]),
    p_value = unname(coef_info[1, "Pr(>|t|)"]),
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
    bw_ft = bw_ft,
    bw_label = bw_label,
    sample_filter = sample_filter,
    fe_spec = fe_spec,
    prune_sample = prune_sample,
    cluster_level = cluster_level,
    donut_ft = donut_ft,
    input_path = fe_input_path,
    table_output = output_filename
  )
}

if (length(models) == 0) {
  stop("No models estimated; check yvars and data.")
}

names(models) <- col_headers

fe_rows <- lapply(names(fe_label_list), function(x) rep("$\\checkmark$", length(models)))
names(fe_rows) <- paste0("_", names(fe_label_list))
fe_rows[["_Side-Specific Distance Slopes"]] <- rep("$\\checkmark$", length(models))
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

if (nzchar(fe_summary_output_path)) {
  write_csv(bind_rows(model_summaries), fe_summary_output_path)
}
