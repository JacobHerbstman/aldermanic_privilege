# border_pair_FE_tables_by_bw.R
# One table per bandwidth (in miles) with multiple outcomes as columns.
# Regressions: y ~ homeownership_own | construction_year + ward_pair, clustered by ward_pair

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_FE_regressions/code")

source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_FE_regressions/code")
# bw_ft <- 250
# sample_filter <- "all"  # "all" | "multifamily"
# fe_spec <- "pair_x_year"
# yvars <- c("log(density_far)", "log(density_dupac)", "log(unitscount)")
# output_filename <- sprintf("../output/fe_table_bw%d_%s_%s.tex", bw_ft, sample_filter, fe_spec)
# Rscript border_pair_FE_regressions.R 250 multifamily pair_x_year ../output/fe_table_bw250_multifamily_pair_x_year.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
# arg order: bw_ft sample fe_spec output_filename yvar1 [yvar2 ...]
# sample: "all" (unitscount > 0) | "multifamily" (unitscount > 1)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 5) {
  bw_ft <- suppressWarnings(as.integer(args[1]))
  sample_filter <- args[2]
  fe_spec <- args[3]
  output_filename <- args[4]
  yvars <- args[5:length(args)]
  if (length(args) == 5 && grepl(",", args[5])) {
    yvars <- strsplit(args[5], ",")[[1]] |> trimws()
  }
} else {
  if (!exists("bw_ft") || !exists("sample_filter") || !exists("fe_spec") || !exists("output_filename") || !exists("yvars")) {
    stop("FATAL: Script requires args: <bw_ft> <sample> <fe_spec> <output_filename> <yvar1> [<yvar2> ...]", call. = FALSE)
  }
}

if (!is.finite(bw_ft) || bw_ft <= 0) stop("bw_feet must be a positive integer/numeric.")
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample must be one of: all, multifamily", call. = FALSE)
}
if (length(yvars) == 0) stop("No yvars provided.")

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

donut_ft_raw <- Sys.getenv("DONUT_FT", "0")
donut_ft <- suppressWarnings(as.numeric(donut_ft_raw))
if (!is.finite(donut_ft) || donut_ft < 0) {
  stop("DONUT_FT must be a non-negative number.", call. = FALSE)
}
if (donut_ft >= bw_ft) {
  stop("DONUT_FT must be strictly smaller than bandwidth.", call. = FALSE)
}

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  out <- rep(NA_character_, length(x))
  if (!any(ok)) return(out)
  parts <- strsplit(x[ok], "-", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) return(NA_character_)
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
  out
}

era_from_year <- function(y) {
  y <- as.integer(y)
  ifelse(
    y < 2003L, "1998_2002",
    ifelse(y < 2015L, "2003_2014", ifelse(y < 2023L, "2015_2023", "post_2023"))
  )
}

message(sprintf("\n=== Border-Pair FE Configuration ==="))
message(sprintf("Bandwidth: %d ft", bw_ft))
message(sprintf("Sample: %s", sample_filter))
message(sprintf("FE Specification: %s", fe_spec))
message(sprintf("Pruning spec: %s", prune_sample))
message(sprintf("Cluster level: %s", cluster_level))
message(sprintf("Donut exclusion: >= %.0f ft", donut_ft))
message(sprintf("Output: %s", output_filename))
message(sprintf("Y variables: %s", paste(yvars, collapse = ", ")))

bw_mi <- round(bw_ft / 5280, 2)


# ── 2) DATA ──────────────────────────────────────────────────────────────────
parcels_fe <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(strictness_own = strictness_own / sd(strictness_own, na.rm = T)) %>%
  filter(arealotsf > 1, areabuilding > 1, construction_year >= 2006)

if (sample_filter == "all") {
  parcels_fe <- parcels_fe %>% filter(unitscount > 0)
} else if (sample_filter == "multifamily") {
  parcels_fe <- parcels_fe %>% filter(unitscount > 1)
}
message(sprintf("Observations after sample filter (%s): %d", sample_filter, nrow(parcels_fe)))

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
    message(sprintf(
      "Pruned run: %d observations have no pair-era pruning flag and will be dropped.",
      n_missing
    ))
    parcels_fe <- parcels_fe %>% mutate(keep_pair_era = ifelse(is.na(keep_pair_era), FALSE, keep_pair_era))
  }

  n_before_prune <- nrow(parcels_fe)
  parcels_fe <- parcels_fe %>% filter(keep_pair_era)
  message(sprintf("Observations after pair-era pruning: %d -> %d", n_before_prune, nrow(parcels_fe)))
}


# ── 3) HELPERS ───────────────────────────────────────────────────────────────
is_log_spec <- function(v) str_detect(v, "^log\\(.+\\)$")
base_name <- function(v) gsub("^log\\(|\\)$", "", v)

pretty_label <- function(v) {
  b <- base_name(v)
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
  if (is_log_spec(v)) paste0("ln(", lab, ")") else lab
}

# fitstat: mean of *level* DV for the estimation sample
mean_y_level <- function(x) {
  dat <- x$custom_data
  y_lhs <- deparse(x$fml[[2]])
  y0 <- if (grepl("^log\\(", y_lhs)) gsub("^log\\(|\\)$", "", y_lhs) else y_lhs

  val <- mean(dat[[y0]], na.rm = TRUE)

  # Return a formatted string to force the display you want
  sprintf("%.2f", val)
}
fitstat_register("myo", mean_y_level, alias = "Dep. Var. Mean")

# fitstat: n ward pairs
n_ward_pairs <- function(x) {
  mf <- tryCatch(model.frame(x), error = function(e) NULL)
  if (!is.null(mf) && "ward_pair" %in% names(mf)) {
    return(length(unique(mf$ward_pair)))
  }
  # Fallbacks if needed:
  if (!is.null(x$cluster) && "ward_pair" %in% names(x$cluster)) {
    return(length(unique(x$cluster$ward_pair)))
  }
  if (!is.null(x$custom_data) && "ward_pair" %in% names(x$custom_data)) {
    return(length(unique(stats::na.omit(x$custom_data$ward_pair))))
  }
  NA_integer_
}

fitstat_register("nwp", n_ward_pairs, alias = "Ward Pairs")

rename_dict <- c(
  "strictness_own" = "Uncertainty Index",
  "floor_area_ratio" = "Zoning FAR (Numeric)",
  "zone_code" = "Zoning Code FE",
  "segment_id" = "Segment FE",
  "construction_year" = "Year FE",
  "ward_pair" = "Ward-Pair FE",
  "ward" = "Ward",
  "zone_code^ward_pair" = "Zoning Code $\\times$ Ward-Pair FE",
  "ward_pair^construction_year" = "Ward-Pair $\\times$ Year FE",
  "zone_code^ward_pair^construction_year" = "Zoning $\\times$ Ward-Pair $\\times$ Year FE",
  "density_dupac" = "Dwelling Units Per Acre (DUPAC)",
  "density_far" = "Floor Area Ratio (FAR)",
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

# ── FE SPECIFICATION MAPPINGS ─────────────────────────────────────────────────
fe_formulas <- list(
  zone_pair_year_additive = "zone_code + ward_pair + construction_year",
  zone_segment_year_additive = "zone_code + segment_id + construction_year",
  zone_x_pair_year = "zone_code^ward_pair + construction_year",
  zone_pair_x_year = "zone_code + ward_pair^construction_year",
  triple = "zone_code^ward_pair^construction_year",
  pair_year_only = "ward_pair + construction_year",
  segment_year = "segment_id + construction_year",
  pair_x_year = "ward_pair^construction_year",
  pair_year_far = "ward_pair + construction_year",
  pair_x_year_far = "ward_pair^construction_year"
)

fe_labels <- list(
  zone_pair_year_additive = list(
    "Zoning Code FE" = "zone_code",
    "Ward-Pair FE" = "ward_pair",
    "Year FE" = "construction_year"
  ),
  zone_segment_year_additive = list(
    "Zoning Code FE" = "zone_code",
    "Segment FE" = "segment_id",
    "Year FE" = "construction_year"
  ),
  zone_x_pair_year = list(
    "Zoning Code $\\times$ Ward-Pair FE" = "zone_code\\^ward_pair",
    "Year FE" = "construction_year"
  ),
  zone_pair_x_year = list(
    "Zoning Code FE" = "zone_code",
    "Ward-Pair $\\times$ Year FE" = "ward_pair\\^construction_year"
  ),
  triple = list(
    "Zoning $\\times$ Ward-Pair $\\times$ Year FE" = "zone_code\\^ward_pair\\^construction_year"
  ),
  pair_year_only = list(
    "Ward-Pair FE" = "ward_pair",
    "Year FE" = "construction_year"
  ),
  segment_year = list(
    "Segment FE" = "segment_id",
    "Year FE" = "construction_year"
  ),
  pair_x_year = list(
    "Ward-Pair $\\times$ Year FE" = "ward_pair\\^construction_year"
  ),
  pair_year_far = list(
    "Ward-Pair FE" = "ward_pair",
    "Year FE" = "construction_year"
  ),
  pair_x_year_far = list(
    "Ward-Pair $\\times$ Year FE" = "ward_pair\\^construction_year"
  )
)

# Validate FE spec
if (!fe_spec %in% names(fe_formulas)) {
  stop(sprintf("Invalid fe_spec '%s'. Must be one of: %s", 
    fe_spec, paste(names(fe_formulas), collapse = ", ")), call. = FALSE)
}

fe_formula_str <- fe_formulas[[fe_spec]]
fe_label_list <- fe_labels[[fe_spec]]
message(sprintf("Using FE formula: | %s", fe_formula_str))
use_far_control <- fe_spec %in% c("pair_year_far", "pair_x_year_far")
if (use_far_control) {
  message("Including numeric zoning FAR control: floor_area_ratio")
}
need_segment <- fe_spec %in% c("segment_year", "zone_segment_year_additive") || cluster_level == "segment"
if (need_segment) {
  message("Segment ID required: dropping rows with missing segment_id.")
}
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair


# ── 4) MODELS (ONE PER OUTCOME), SAME BW ─────────────────────────────────────
models <- list()
col_headers <- c()

for (yv in yvars) {
  b <- base_name(yv)
  if (!b %in% names(parcels_fe)) {
    warning(sprintf("Skipping '%s' (base var '%s' not found).", yv, b))
    next
  }

  df <- parcels_fe %>%
    filter(dist_to_boundary <= bw_ft, dist_to_boundary >= donut_ft)

  if (use_far_control) {
    df <- df %>%
      filter(is.finite(floor_area_ratio))
  }
  if (need_segment) {
    df <- df %>%
      filter(!is.na(segment_id), segment_id != "")
  }

  check_df_additive <- df %>%
    mutate(fe_group_id = paste(ward_pair, zone_code, sep = "_"))

  # 2. Calculate variation within each group
  identifying_variation_additive <- check_df_additive %>%
    group_by(fe_group_id) %>%
    summarize(
      n_obs = n(),
      # Check if there is variation in strictness (are there properties on BOTH sides?)
      sd_strictness = sd(strictness_own, na.rm = TRUE)
    ) %>%
    # Filter to keep only useful groups
    filter(n_obs > 1 & !is.na(sd_strictness) & sd_strictness > 0)

  # 3. View Results
  message("--- Additive Specification Checks (Zone^Pair + Year) ---")
  message("Number of useful groups: ", nrow(identifying_variation_additive))
  message("Number of useful observations: ", sum(identifying_variation_additive$n_obs))
  message("Total observations in dataset: ", nrow(df))

  if (nrow(df) == 0) {
    warning(sprintf("Skipping '%s' (no rows after filtering).", yv))
    next
  }

  # Skip outcomes that are constant in this sample
  y_vals <- if (is_log_spec(yv)) log(df[[b]]) else df[[b]]
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(unique(y_vals)) <= 1) {
    warning(sprintf("Skipping '%s' (constant in %s sample).", yv, sample_filter))
    next
  }

  rhs_controls <- c(
    "strictness_own",
    "share_white_own",
    "share_black_own",
    "median_hh_income_own",
    "share_bach_plus_own",
    "homeownership_rate_own"
  )
  if (use_far_control) {
    rhs_controls <- c(rhs_controls, "floor_area_ratio")
  }
  fml_txt <- paste0(yv, " ~ ", paste(rhs_controls, collapse = " + "), " | ", fe_formula_str)
  m <- feols(as.formula(fml_txt), data = df, cluster = cluster_formula)
  m$custom_data <- df

  models[[length(models) + 1]] <- m
  col_headers <- c(col_headers, pretty_label(yv))
}

if (length(models) == 0) stop("No models estimated; check yvars and data.")
names(models) <- col_headers

# ── 5) TITLE & TABLE OUTPUT ──────────────────────────────────────────────────
table_title <- sprintf(
  "Border FE estimates (bw = %.0f ft, donut >= %.0f ft, FE: %s, cluster: %s)",
  bw_ft, donut_ft, fe_spec, cluster_level
)

etable(models,
  keep = "Uncertainty Index",
  fitstat = ~ n + myo + nwp,
  style.tex = style.tex("aer",
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
  fixef.group = fe_label_list,
  float = FALSE,
  tex = TRUE,
  file = output_filename,
  replace = TRUE
)
