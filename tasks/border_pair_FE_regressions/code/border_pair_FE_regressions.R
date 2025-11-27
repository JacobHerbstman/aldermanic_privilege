# border_pair_FE_tables_by_bw.R
# One table per bandwidth (in miles) with multiple outcomes as columns.
# Regressions: y ~ homeownership_own | construction_year + ward_pair, clustered by ward_pair

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# bw_ft <- 250
# yvars <- c(
#   "log(density_dupac)", "log(density_far)", "log(unitscount)"
# )
# output_filename <- "../output/fe_table_bw1056.tex"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
# Args: <bw_feet> <output_filename> <yvar1> [<yvar2> ...]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 3) {
  bw_ft           <- suppressWarnings(as.integer(args[1]))
  output_filename <- args[2]
  # space-separated yvars
  yvars <- args[3:length(args)]

  # backward-compat: if exactly 3 args and the 3rd has commas, split them
  if (length(args) == 3 && grepl(",", args[3])) {
    yvars <- strsplit(args[3], ",")[[1]] |> trimws()
  }
} else {
  # allow interactive testing with objects already defined in the session
  if (!exists("bw_ft") || !exists("output_filename") || !exists("yvars")) {
    stop("FATAL: need args: <bw_feet> <output_filename> <yvar1> [<yvar2> ...]", call. = FALSE)
  }
}

if (!is.finite(bw_ft) || bw_ft <= 0) stop("bw_feet must be a positive integer/numeric.")
if (length(yvars) == 0) stop("No yvars provided.")

bw_mi <- round(bw_ft / 5280, 2)


# ── 2) DATA ──────────────────────────────────────────────────────────────────
parcels_fe <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(strictness_own = strictness_own/sd(strictness_own, na.rm =T)) %>%
  filter(arealotsf > 1) %>%
  filter(areabuilding > 1) %>%
  filter(unitscount > 1) %>% 
  filter(unitscount > 1 & unitscount < 10) 
  # filter(construction_year > 2006)


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
  "strictness_own" = "Strictness Score",
  "zone_code" = "Zoning Code",
  "construction_year" = "Year",
  "ward_pair" = "Ward Pair",
  "ward" = "Ward",
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

  if (nrow(df) == 0) {
    warning(sprintf("Skipping '%s' (no rows after filtering).", yv))
    next
  }

  fml_txt <- paste0(yv, " ~ strictness_own + abs(dist_to_boundary) + avg_rent_own + share_white_own + avg_hh_income_own + share_bach_plus_own | 
                    zone_code + construction_year^ward_pair")
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
       keep = "Strictness Score",
       fitstat = ~ n + myo + nwp,
       style.tex = style.tex("aer", model.format = ""),
       depvar = FALSE,
       digits = 2,
       dict = rename_dict,
       headers = names(models),
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
       fixef.group = list("Ward-pair × Year FE" = "construction_year\\^ward_pair"),
       
       # --- CHANGE THESE LINES ---
       title = NULL,
       float = FALSE,
       # --------------------------
       
       file = output_filename,
       replace = TRUE
)

