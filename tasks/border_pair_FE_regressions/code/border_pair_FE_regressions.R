# This script runs border-pair FE regressions across multiple bandwidths.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_FE_regressions/code")

source("../../setup_environment/code/packages.R")

# --- 1. PARAMETERS ---
# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# yvar       <- "log(density_far)"
# bandwidths <- c(100, 250, 400, 500, 800) # Define all bandwidths here
# =======================================================================================

# =======================================================================================
# --- Command-Line Arguments (for Makefile) ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("FATAL: Script requires 2 arguments: <yvar> <output_filename>", call. = FALSE)
}
yvar            <- args[1]
output_filename <- args[2]
bandwidths      <- c(100, 250, 400, 500, 800)
# =======================================================================================

# --- 2. DATA PREPARATION ---
# =======================================================================================
cat("Loading and preparing data...\n")
parcels <- st_read("../input/parcels_with_ward_distances.gpkg")
parcels <- as_tibble(st_drop_geometry(parcels))


# --- 3. LOOP THROUGH BANDWIDTHS AND RUN REGRESSIONS ---
# =======================================================================================
cat("Running regressions for yvar:", yvar, "across", length(bandwidths), "bandwidths...\n")

model_list <- lapply(bandwidths, function(bw) {
  parcels_fe <- parcels %>%
    filter(dist_to_boundary <= bw)
  
  fe_model <- feols(
    fml = as.formula(paste0(
      yvar,
      " ~ strictness_index | construction_year + ward_pair"
    )),
    data = parcels_fe,
    cluster = ~ward_pair
  )
  
  fe_model$custom_data <- parcels_fe
  return(fe_model)
})


# --- 4. CREATE AND SAVE RESULTS TABLE ---
# =======================================================================================

# Assign names to the models for clean column headers
names(model_list) <- paste0(bandwidths, "m")

# Helper function for mean of the original (level) dependent variable
mean_y_level <- function(x) {
  model_data <- x$custom_data
  fml <- x$fml
  y_name_str <- deparse(fml[[2]])
  
  if (grepl("log\\(", y_name_str)) {
    y_name_original <- gsub("log|\\(|\\)", "", y_name_str)
    mean(model_data[[y_name_original]], na.rm = TRUE)
  } else {
    mean(model_data[[y_name_str]], na.rm = TRUE)
  }
}
fitstat_register("myo", mean_y_level, alias = "Dep. Var. Mean")

# Helper function for number of ward pairs
n_ward_pairs <- function(x) {
  x$fixef_sizes["ward_pair"]
}
fitstat_register("nwp", n_ward_pairs, alias = "Ward Pairs")


# Define dictionary for renaming
rename_dict <- c(
  "strictness_index" = "Restrictiveness Score",
  "construction_year"      = "Year",
  'ward_pair' = "Ward Pair",
  "density_far" = "Floor Area Ratio (FAR)",
  "density_lapu" = "Lot Area Per Unit (LAPU)",
  "density_bcr" = "Building Coverage Ratio (BCR)",
  "density_lps" = "Lot Size Per Story (LPS)",
  "density_spu" = "Stories Per Unit (SPU)"
)

# --- Create a dynamic title for the table ---
base_yvar <- gsub("log|\\(|\\)", "", yvar)
clean_label <- rename_dict[base_yvar]

table_title <- if (grepl("log\\(", yvar)) {
  paste("Border-Pair FE estimates: Log", clean_label)
} else {
  paste("Border-Pair FE estimates:", clean_label)
}

# Create and save the table
etable(
  model_list,
  keep = "%strictness_index",
  # Formatting options
  fitstat = ~ n + myo + nwp,
  style.tex = style.tex("aer"),
  depvar      = FALSE,
  digits = 2,
  dict        = rename_dict,
  # General options
  title       = table_title,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fixef.group = TRUE,
  file = output_filename, # Save to the specified file
  replace = TRUE
)

cat("✓ Table saved to:", output_filename, "\n")
