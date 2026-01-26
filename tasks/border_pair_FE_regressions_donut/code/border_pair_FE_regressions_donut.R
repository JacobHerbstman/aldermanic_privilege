# border_pair_FE_regressions_donut.R
# Donut robustness check: Same as main FE regressions but excludes parcels within
# specified distance of ward boundary (e.g., 25 or 50 ft)

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_FE_regressions_donut/code")

source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# bw_ft <- 250
# donut_ft <- 25  # set to 25 or 50 for donut robustness
# yvars <- c(
#   "log(density_far)", "log(density_dupac)", "log(unitscount)"
# )
# output_filename <- "../output/fe_table_bw250_donut25.tex"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
# Args: <bw_feet> <donut_feet> <output_filename> <yvar1> [<yvar2> ...]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 4) {
    bw_ft <- suppressWarnings(as.integer(args[1]))
    donut_ft <- suppressWarnings(as.integer(args[2]))
    output_filename <- args[3]
    # space-separated yvars
    yvars <- args[4:length(args)]

    # backward-compat: if exactly 4 args and the 4th has commas, split them
    if (length(args) == 4 && grepl(",", args[4])) {
        yvars <- strsplit(args[4], ",")[[1]] |> trimws()
    }
} else {
    # allow interactive testing with objects already defined in the session
    if (!exists("bw_ft") || !exists("output_filename") || !exists("yvars")) {
        stop("FATAL: need args: <bw_feet> <donut_feet> <output_filename> <yvar1> [<yvar2> ...]", call. = FALSE)
    }
    if (!exists("donut_ft")) donut_ft <- 0
}

if (!is.finite(bw_ft) || bw_ft <= 0) stop("bw_feet must be a positive integer/numeric.")
if (!is.finite(donut_ft) || donut_ft < 0) stop("donut_feet must be a non-negative integer/numeric.")
if (donut_ft == 0) warning("donut_ft is 0; use the main border_pair_FE_regressions task instead.")
if (length(yvars) == 0) stop("No yvars provided.")

bw_mi <- round(bw_ft / 5280, 2)


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
    "zone_code" = "Zoning Code FE",
    "construction_year" = "Year FE",
    "ward_pair" = "Ward-Pair FE",
    "ward" = "Ward",
    "zone_code^ward_pair" = "Zoning Code $\\times$ Ward-Pair FE",
    "ward_pair^construction_year" = "Ward-Pair $\\times$ Year FE",
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

    # DONUT FILTER: exclude parcels within donut_ft of border
    df <- parcels_fe %>%
        filter(dist_to_boundary <= bw_ft & dist_to_boundary >= donut_ft)

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
  homeownership_rate_own + avg_rent_own | ward_pair^construction_year")
    m <- feols(as.formula(fml_txt), data = df, cluster = ~ward_pair)
    m$custom_data <- df

    models[[length(models) + 1]] <- m
    col_headers <- c(col_headers, pretty_label(yv))
}

if (length(models) == 0) stop("No models estimated; check yvars and data.")
names(models) <- col_headers

# ── 5) TITLE & TABLE OUTPUT ──────────────────────────────────────────────────
table_title <- sprintf("Border-Pair FE estimates (bw = %.0f ft, donut = %.0f ft)", bw_ft, donut_ft)

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
        "Ward-Pair $\\times$ Year FE" = "ward_pair"
    ),
    float = FALSE,
    tex = TRUE,
    file = output_filename,
    replace = TRUE
)
