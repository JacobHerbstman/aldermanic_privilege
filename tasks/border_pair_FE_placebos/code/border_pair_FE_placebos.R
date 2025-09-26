# Border-pair FE placebos: outcomes = distances to amenities
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# --- Defaults for interactive testing (optional) ---
# bw_ft           <- 1056
# yvars           <- c("dist_cta_stop","dist_park","dist_school","dist_major_street")
# output_filename <- "../output/placebo_bw1056.tex"

# --- 1) CMD ARGS: <bw_feet> <output_filename> <yvar1> [<yvar2> ...] -----------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
  bw_ft           <- suppressWarnings(as.integer(args[1]))
  output_filename <- args[2]
  yvars <- if (length(args) >= 3) args[3:length(args)] else
    c("dist_cta_stop","dist_park","dist_school","dist_major_street")
} else {
  if (!exists("bw_ft") || !exists("output_filename") || !exists("yvars")) {
    stop("FATAL: need args: <bw_feet> <output_filename> <yvar1> [<yvar2> ...]", call. = FALSE)
  }
}
if (!is.finite(bw_ft) || bw_ft <= 0) stop("bw_feet must be positive.")
if (length(yvars) == 0) stop("No yvars provided.")

# --- 2) DATA ------------------------------------------------------------------
cat("Loading parcel/distances dataset...\n")
parcels <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(strictness_own_std = strictness_own / sd(strictness_own, na.rm = TRUE))

# --- 3) Helpers ---------------------------------------------------------------
is_log_spec <- function(v) grepl("^log\\(", v)
base_name   <- function(v) gsub("^log\\(|\\)$", "", v)
pretty_label <- function(v) {
  b <- base_name(v)
  dict <- c(
    "dist_cta_stop"     = "Distance to CTA (ft)",
    "dist_major_street" = "Distance to Major St. (ft)",
    "dist_park"         = "Distance to Park (ft)",
    "dist_school"       = "Distance to School (ft)"
  )
  lab <- ifelse(b %in% names(dict), dict[[b]], b)
  if (is_log_spec(v)) paste("Log", lab) else lab
}

# fitstats
mean_y_level <- function(x) {
  md <- x$custom_data
  yn <- deparse(x$fml[[2]])
  if (grepl("^log\\(", yn)) {
    base <- gsub("^log\\(|\\)$", "", yn)
    mean(md[[base]], na.rm = TRUE)
  } else {
    mean(md[[yn]], na.rm = TRUE)
  }
}
fixest::fitstat_register("myo", mean_y_level, alias = "Dep. Var. Mean")
n_ward_pairs <- function(x) x$fixef_sizes["ward_pair"]
fixest::fitstat_register("nwp", n_ward_pairs, alias = "Ward Pairs")

rename_dict <- c(
  "strictness_own_std" = "Restrictiveness Score",
  "dist_cta_stop"      = "Distance to CTA (ft)",
  "dist_major_street"  = "Distance to Major St. (ft)",
  "dist_park"          = "Distance to Park (ft)",
  "dist_school"        = "Distance to School (ft)"
)

# --- 4) MODELS: one bandwidth, multiple outcomes (columns) --------------------
models <- list()
headers <- c()

for (yv in yvars) {
  b <- base_name(yv)
  df <- parcels %>%
    filter(dist_to_boundary <= bw_ft, !is.na(zone_code))
  if (is_log_spec(yv)) df <- df[df[[b]] > 0, ]  # only if log-spec
  
  if (!b %in% names(df)) {
    warning(sprintf("Skipping '%s' (base var '%s' not found).", yv, b))
    next
  }
  if (nrow(df) == 0) {
    warning(sprintf("Skipping '%s' (no rows after filtering).", yv))
    next
  }
  
  fml_txt <- paste0(yv, " ~ strictness_own_std | construction_year + ward_pair")
  m <- fixest::feols(as.formula(fml_txt), data = df, cluster = ~ ward_pair)
  m$custom_data <- df
  models[[length(models) + 1]] <- m
  headers <- c(headers, pretty_label(yv))
}

if (length(models) == 0) stop("No models estimated; check yvars and data.")
names(models) <- headers

# --- 5) TABLE -----------------------------------------------------------------
bw_mi <- bw_ft / 5280
table_title <- sprintf("Border-Pair FE Placebos @ %.2f miles (bw = %d ft)", bw_mi, bw_ft)

fixest::etable(
  models,
  keep        = "Restrictiveness Score",
  fitstat     = ~ n + myo + nwp,
  style.tex   = style.tex("aer", model.format = ""),
  depvar      = FALSE,
  digits      = 2,
  dict        = rename_dict,
  headers     = names(models),
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fixef.group = TRUE,
  title       = table_title,
  file        = output_filename,
  replace     = TRUE
)

cat("✓ Placebo table saved to:", output_filename, "\n")
