# border_pair_FE_regressions_linear_trends.R
# Robustness check: Adding border-pair-specific linear time trends
# Specification: y ~ strictness + controls + ward_pair:year_num | zone_code^ward_pair + construction_year
# This allows for differential linear trends across border pairs while keeping city-wide year FEs

source("../../setup_environment/code/packages.R")

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 3) {
  bw_ft <- suppressWarnings(as.integer(args[1]))
  output_filename <- args[2]
  yvars <- args[3:length(args)]

  if (length(args) == 3 && grepl(",", args[3])) {
    yvars <- strsplit(args[3], ",")[[1]] |> trimws()
  }
} else {
  if (!exists("bw_ft") || !exists("output_filename") || !exists("yvars")) {
    stop("FATAL: need args: <bw_feet> <output_filename> <yvar1> [<yvar2> ...]", call. = FALSE)
  }
}

if (!is.finite(bw_ft) || bw_ft <= 0) stop("bw_feet must be a positive integer/numeric.")
if (length(yvars) == 0) stop("No yvars provided.")

bw_mi <- round(bw_ft / 5280, 2)


# ── 2) DATA ──────────────────────────────────────────────────────────────────
parcels_fe <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(strictness_own = strictness_own / sd(strictness_own, na.rm = T)) %>%
  filter(arealotsf > 1) %>%
  filter(areabuilding > 1) %>%
  filter(unitscount > 1) %>%
  filter(unitscount > 1 & unitscount <= 100) %>% 
  filter(construction_year >= 2006) %>%
  # KEY ADDITION: Create numeric year for linear trends
  mutate(year_num = construction_year - min(construction_year, na.rm = TRUE))


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
  sprintf("%.2f", val)
}
fitstat_register("myo", mean_y_level, alias = "Dep. Var. Mean")

# fitstat: n ward pairs
n_ward_pairs <- function(x) {
  mf <- tryCatch(model.frame(x), error = function(e) NULL)
  if (!is.null(mf) && "ward_pair" %in% names(mf)) {
    return(length(unique(mf$ward_pair)))
  }
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
  "strictness_own" = "Strictness Score",
  "zone_code" = "Zoning Code",
  "construction_year" = "Year FE",
  "ward_pair" = "Ward-Pair",
  "ward" = "Ward",
  "zone_code^ward_pair" = "Zoning Code $\\times$ Ward-Pair FE",
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


# ── 4) MODELS WITH LINEAR TRENDS ─────────────────────────────────────────────
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

  if (nrow(df) == 0) {
    warning(sprintf("Skipping '%s' (no rows after filtering).", yv))
    next
  }

  # Count ward pairs with variation
  check_df <- df %>%
    mutate(fe_group_id = paste(ward_pair, zone_code, sep = "_")) %>%
    group_by(fe_group_id) %>%
    summarize(
      n_obs = n(),
      sd_strictness = sd(strictness_own, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_obs > 1 & !is.na(sd_strictness) & sd_strictness > 0)

  message("--- Linear Trends Specification ---")
  message("Bandwidth: ", bw_ft, " ft")
  message("Outcome: ", yv)
  message("Number of useful groups: ", nrow(check_df))
  message("Total observations in dataset: ", nrow(df))
  message("Number of ward pairs: ", length(unique(df$ward_pair)))

  # KEY CHANGE: Add ward_pair:year_num for border-pair-specific linear trends
  # This estimates one slope per ward_pair while keeping year FEs for city-wide nonlinear time effects
  fml_txt <- paste0(
    yv, " ~ strictness_own + share_white_own + share_black_own + median_hh_income_own + ",
    "share_bach_plus_own + homeownership_rate_own + avg_rent_own + ",
    "ward_pair:year_num | zone_code^ward_pair + construction_year"
  )
  
  m <- tryCatch({
    feols(as.formula(fml_txt), data = df, cluster = ~ward_pair)
  }, error = function(e) {
    warning(sprintf("Model estimation failed for '%s': %s", yv, e$message))
    return(NULL)
  })
  
  if (is.null(m)) next
  
  m$custom_data <- df

  models[[length(models) + 1]] <- m
  col_headers <- c(col_headers, pretty_label(yv))
}

if (length(models) == 0) stop("No models estimated; check yvars and data.")
names(models) <- col_headers

# ── 5) TITLE & TABLE OUTPUT ──────────────────────────────────────────────────
table_title <- sprintf("Border-Pair FE estimates with Linear Trends (bw = %.0f ft)", bw_ft)

etable(models,
  keep = "Strictness Score",
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
  fixef.group = list(
    "Zoning Code $\\times$ Ward-Pair FE" = "zone_code",
    "Year FE" = "construction_year"
  ),
  extralines = list(
    "_Border-Pair Linear Trends" = rep("$\\checkmark$", length(models))
  ),
  float = FALSE,
  tex = TRUE,
  file = output_filename,
  replace = TRUE
)

# Also output a summary comparison CSV
comparison_summary <- data.frame(
  outcome = col_headers,
  coefficient = sapply(models, function(m) coef(m)["strictness_own"]),
  se = sapply(models, function(m) se(m)["strictness_own"]),
  n_obs = sapply(models, function(m) nobs(m)),
  n_ward_pairs = sapply(models, function(m) n_ward_pairs(m))
)

csv_filename <- gsub("\\.tex$", ".csv", output_filename)
write_csv(comparison_summary, csv_filename)
message("Summary saved to: ", csv_filename)
