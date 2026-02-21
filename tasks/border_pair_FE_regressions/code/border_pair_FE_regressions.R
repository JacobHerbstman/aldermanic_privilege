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
# units_cap <- 40
# fe_spec <- "pair_x_year"
# output_filename <- "../output/fe_table_bw250_pair_x_year.tex"
# yvars <- c("log(density_far)", "log(density_dupac)", "log(unitscount)")
# Rscript border_pair_FE_regressions.R 250 100 pair_x_year ../output/fe_table_bw250_pair_x_year.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 5) {
  bw_ft <- suppressWarnings(as.integer(args[1]))
  units_cap <- suppressWarnings(as.integer(args[2]))
  fe_spec <- args[3]
  output_filename <- args[4]
  yvars <- args[5:length(args)]
  if (length(args) == 5 && grepl(",", args[5])) {
    yvars <- strsplit(args[5], ",")[[1]] |> trimws()
  }
} else {
  if (!exists("bw_ft") || !exists("units_cap") || !exists("fe_spec") || !exists("output_filename") || !exists("yvars")) {
    stop("FATAL: Script requires args: <bw_ft> <units_cap> <fe_spec> <output_filename> <yvar1> [<yvar2> ...]", call. = FALSE)
  }
}

if (!is.finite(bw_ft) || bw_ft <= 0) stop("bw_feet must be a positive integer/numeric.")
if (!is.finite(units_cap)) units_cap <- -1
if (length(yvars) == 0) stop("No yvars provided.")

message(sprintf("\n=== Border-Pair FE Configuration ==="))
message(sprintf("Bandwidth: %d ft", bw_ft))
message(sprintf("FE Specification: %s", fe_spec))
if (units_cap > 0) {
  message(sprintf("Units cap: unitscount <= %d", units_cap))
} else {
  message("Units cap: none")
}
message(sprintf("Output: %s", output_filename))
message(sprintf("Y variables: %s", paste(yvars, collapse = ", ")))

bw_mi <- round(bw_ft / 5280, 2)


# ── 2) DATA ──────────────────────────────────────────────────────────────────
parcels_fe <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(strictness_own = strictness_own / sd(strictness_own, na.rm = T)) %>%
  filter(arealotsf > 1) %>%
  filter(areabuilding > 1) %>%
  # filter(unitscount > 1) %>%
  filter(construction_year >= 2006)

# if (units_cap > 0) {
#   parcels_fe <- parcels_fe %>% filter(unitscount <= units_cap)
# }
# filter(construction_year < 2024)


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
  "zone_code" = "Zoning Code FE",
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
  zone_x_pair_year = "zone_code^ward_pair + construction_year",
  zone_pair_x_year = "zone_code + ward_pair^construction_year",
  triple = "zone_code^ward_pair^construction_year",
  pair_year_only = "ward_pair + construction_year",
  pair_x_year = "ward_pair^construction_year"
)

fe_labels <- list(
  zone_pair_year_additive = list(
    "Zoning Code FE" = "zone_code",
    "Ward-Pair FE" = "ward_pair",
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
  pair_x_year = list(
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
    filter(dist_to_boundary <= bw_ft)

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

  fml_txt <- paste0(yv, " ~ strictness_own + share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own +
  homeownership_rate_own + avg_rent_own | ", fe_formula_str)
  m <- feols(as.formula(fml_txt), data = df, cluster = ~ward_pair)
  m$custom_data <- df

  models[[length(models) + 1]] <- m
  col_headers <- c(col_headers, pretty_label(yv))
}

if (length(models) == 0) stop("No models estimated; check yvars and data.")
names(models) <- col_headers

# ── 5) TITLE & TABLE OUTPUT ──────────────────────────────────────────────────
table_title <- sprintf("Border-Pair FE estimates (bw = %.0f ft)", bw_ft)

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
