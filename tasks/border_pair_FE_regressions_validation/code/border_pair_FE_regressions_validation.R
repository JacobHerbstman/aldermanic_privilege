# border_pair_FE_tables_by_bw_validation.R
# Covariate balance check using BLOCK GROUP-LEVEL demographics
# Generates TWO tables (Panel A and Panel B) with 4 columns each

source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
bw_ft <- 250
output_filename <- "../output/fe_validation_table_bw250.tex"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
  bw_ft <- suppressWarnings(as.integer(args[1]))
  output_filename <- args[2]
} else {
  if (!exists("bw_ft") || !exists("output_filename")) {
    message("No args provided. Ensure bw_ft and output_filename are defined in environment.")
  }
}

if (!is.finite(bw_ft) || bw_ft <= 0) stop("bw_feet must be a positive integer/numeric.")

# ── 2) DATA ──────────────────────────────────────────────────────────────────
parcels_fe <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(strictness_own = strictness_own / sd(strictness_own, na.rm = T)) %>%
  filter(arealotsf > 1) %>%
  filter(areabuilding > 1) %>%
  filter(unitscount > 1) %>%
  filter(unitscount > 1 & unitscount <= 100) %>%
  filter(construction_year >= 2006)


# ── 3) HELPERS ───────────────────────────────────────────────────────────────
is_log_spec <- function(v) str_detect(v, "^log\\(.+\\)$")
base_name <- function(v) gsub("^log\\(|\\)$", "", v)

pretty_label <- function(v) {
  b <- base_name(v)
  dict <- c(
    "percent_white_bg" = "\\% White",
    "percent_black_bg" = "\\% Black",
    "median_income_bg" = "Median Income",
    "share_bach_plus_bg" = "\\% Bachelor's+",
    "homeownership_rate_bg" = "Homeownership",
    "median_rent_bg" = "Median Rent",
    "median_home_value_bg" = "Home Value",
    "population_density_bg" = "Pop. Density"
  )
  lab <- ifelse(b %in% names(dict), dict[[b]], b)
  if (is_log_spec(v)) paste0("ln(", lab, ")") else lab
}

mean_y_level <- function(x) {
  dat <- x$custom_data
  y_lhs <- deparse(x$fml[[2]])
  y0 <- if (grepl("^log\\(", y_lhs)) gsub("^log\\(|\\)$", "", y_lhs) else y_lhs
  val <- mean(dat[[y0]], na.rm = TRUE)
  sprintf("%.2f", val)
}
fitstat_register("myo", mean_y_level, alias = "Dep. Var. Mean")

n_ward_pairs <- function(x) {
  mf <- tryCatch(model.frame(x), error = function(e) NULL)
  if (!is.null(mf) && "ward_pair" %in% names(mf)) {
    return(length(unique(mf$ward_pair)))
  }
  if (!is.null(x$custom_data) && "ward_pair" %in% names(x$custom_data)) {
    return(length(unique(stats::na.omit(x$custom_data$ward_pair))))
  }
  NA_integer_
}
fitstat_register("nwp", n_ward_pairs, alias = "Ward Pairs")

rename_dict <- c(
  "strictness_own" = "Strictness Score",
  "zone_code^ward_pair" = "Zoning Code $\\times$ Ward-Pair FE",
  "construction_year" = "Year FE"
)

# ── 4) PANEL DEFINITIONS ──────────────────────────────────────────────────────
# Split into two panels (4 columns each)
balance_vars_panel_a <- c(
  "percent_white_bg",
  "percent_black_bg",
  "median_income_bg",
  "share_bach_plus_bg"
)

balance_vars_panel_b <- c(
  "homeownership_rate_bg",
  "median_rent_bg",
  "median_home_value_bg",
  "population_density_bg"
)

# ── 5) FUNCTION TO RUN ONE PANEL ──────────────────────────────────────────────
run_panel <- function(vars, output_suffix) {
  models <- list()
  col_headers <- c()

  for (yv in vars) {
    b <- base_name(yv)
    if (!b %in% names(parcels_fe)) {
      warning(sprintf("Skipping '%s' (base var '%s' not found).", yv, b))
      next
    }

    df <- parcels_fe %>%
      filter(dist_to_boundary <= bw_ft)

    if (nrow(df) == 0) next

    fml_txt <- paste0(yv, " ~ strictness_own | zone_code^ward_pair + construction_year")
    m <- feols(as.formula(fml_txt), data = df, cluster = ~ward_pair)
    m$custom_data <- df

    models[[length(models) + 1]] <- m
    col_headers <- c(col_headers, pretty_label(yv))
  }

  if (length(models) == 0) {
    return(NULL)
  }
  names(models) <- col_headers

  panel_file <- gsub("\\.tex$", paste0(output_suffix, ".tex"), output_filename)

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
    digits = 3,
    dict = rename_dict,
    headers = names(models),
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    fixef.group = list(
      "Zoning Code $\\times$ Ward-Pair FE" = "zone_code",
      "Year FE" = "construction_year"
    ),
    float = FALSE,
    file = panel_file,
    replace = TRUE
  )

  message(sprintf("Wrote panel to %s", panel_file))
}

# ── 6) GENERATE BOTH PANELS ───────────────────────────────────────────────────
run_panel(balance_vars_panel_a, "_panel_a")
run_panel(balance_vars_panel_b, "_panel_b")
