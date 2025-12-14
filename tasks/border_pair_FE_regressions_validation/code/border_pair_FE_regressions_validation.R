# border_pair_FE_tables_by_bw_validation.R
# One table per bandwidth (in miles) with multiple validation outcomes as columns.
# Regressions: demographic_var ~ strictness_own | construction_year^ward_pair, clustered by ward_pair
# This script is specifically for VALIDATION (Balance Checks)

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
bw_ft <- 250
output_filename <- "../output/fe_validation_table_bw250.tex"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
# Args: <bw_feet> <output_filename>
# Note: We don't need yvars as arguments anymore, as we are pre-defining the validation vars
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
  bw_ft <- suppressWarnings(as.integer(args[1]))
  output_filename <- args[2]
} else {
  # allow interactive testing with objects already defined in the session
  if (!exists("bw_ft") || !exists("output_filename")) {
    # Default fallback if run interactively without args or variables defined
    # stop("FATAL: need args: <bw_feet> <output_filename>", call. = FALSE)
    message("No args provided. Ensure bw_ft and output_filename are defined in environment.")
  }
}

if (!is.finite(bw_ft) || bw_ft <= 0) stop("bw_feet must be a positive integer/numeric.")

bw_mi <- round(bw_ft / 5280, 2)


# ── 2) DATA ──────────────────────────────────────────────────────────────────
# Loading the same dataset, applying same filters to ensure sample consistency
parcels_fe <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(strictness_own = strictness_own / sd(strictness_own, na.rm = T)) %>%
  filter(arealotsf > 1) %>%
  filter(areabuilding > 1) %>%
  filter(unitscount > 1) %>%
  filter(unitscount > 1 & unitscount <= 50)
# filter(construction_year > 2006)


# ── 3) HELPERS ───────────────────────────────────────────────────────────────
# Helper functions for table formatting
is_log_spec <- function(v) str_detect(v, "^log\\(.+\\)$")
base_name <- function(v) gsub("^log\\(|\\)$", "", v)

# Expanded dictionary to include validation variables
pretty_label <- function(v) {
  b <- base_name(v)
  dict <- c(
    # Main Outcomes (kept for reference)
    "density_dupac" = "Dwelling Units Per Acre (DUPAC)",
    "density_far" = "Floor Area Ratio (FAR)",
    "unitscount" = "Units",

    # Validation Variables
    "avg_rent_own" = "Avg. Rent",
    "share_white_own" = "Share White",
    "avg_hh_income_own" = "Avg. HH Income",
    "share_bach_plus_own" = "Share Bachelor's+",
    "avg_home_value_own" = "Avg. Home Value"
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
  "strictness_own" = "Strictness Score",
  "zone_code^ward_pair" = "Zoning Code $\\times$ Ward-Pair FE",
  "construction_year" = "Year FE"
)


# ── 4) MODELS (VALIDATION REGRESSIONS) ───────────────────────────────────────
# Instead of density outcomes, we use the demographic controls as Dependent Variables
# We remove them from the RHS to test for raw balance at the border
balance_vars <- c(
  "share_white_own",
  "avg_hh_income_own",
  "share_bach_plus_own"
)

models <- list()
col_headers <- c()

for (yv in balance_vars) {
  b <- base_name(yv)
  if (!b %in% names(parcels_fe)) {
    warning(sprintf("Skipping '%s' (base var '%s' not found).", yv, b))
    next
  }

  # Filter to bandwidth
  df <- parcels_fe %>%
    filter(dist_to_boundary <= bw_ft)

  if (nrow(df) == 0) {
    warning(sprintf("Skipping '%s' (no rows after filtering).", yv))
    next
  }

  # THE KEY CHANGE:
  # 1. Demographic variable is now LHS (yv)
  # 2. RHS only has strictness + distance + FEs (no other controls)
  fml_txt <- paste0(yv, " ~ strictness_own |
                    zone_code^ward_pair + construction_year")

  m <- feols(as.formula(fml_txt), data = df, cluster = ~ward_pair)
  m$custom_data <- df

  models[[length(models) + 1]] <- m
  col_headers <- c(col_headers, pretty_label(yv))
}

if (length(models) == 0) stop("No models estimated; check balance_vars and data.")
names(models) <- col_headers

# ── 5) TITLE & TABLE OUTPUT ──────────────────────────────────────────────────
table_title <- sprintf("Identification Check: Covariate Balance at Ward Boundaries (bw = %.0f ft)", bw_ft)

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
  file = output_filename,
  replace = TRUE
)
