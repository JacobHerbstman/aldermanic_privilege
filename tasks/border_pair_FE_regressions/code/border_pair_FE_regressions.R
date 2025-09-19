# This script runs border-pair FE regressions across multiple bandwidths.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# --- 1. PARAMETERS ---
# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# yvar       <- "density_far"
# bandwidths <- c(264, 528, 792, 1056, 1320, 1584, 2112, 2640) # Define all bandwidths here
# =======================================================================================

# =======================================================================================
# --- Command-Line Arguments (for Makefile) ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("FATAL: Script requires 2 arguments: <yvar> <output_filename>", call. = FALSE)
}
yvar            <- args[1]
output_filename <- args[2]
bandwidths      <- c(264, 528, 792, 1056, 1320, 1584, 2640)
# =======================================================================================

# --- 2. DATA PREPARATION ---
# =======================================================================================
cat("Loading and preparing data...\n")
parcels <- read_csv("../input/parcels_with_ward_distances.csv")
parcels <- parcels %>%
  mutate(strictness_own_std = strictness_own / sd(strictness_own, na.rm = TRUE))  ## divide by standard deviation to interpret regressions 

# keep strictly positive outcomes (for logs)
y_name_original <- gsub("log|\\(|\\)", "", yvar)
parcels <- parcels[parcels[[y_name_original]] > 0, ]

# --- Sample restriction helper: keep modal zone that exists on both sides within the bw ---
restrict_to_modal_zone <- function(df, bw) {
  # 1) limit to current bandwidth + non-missing zone
  df_bw <- df %>%
    filter(dist_to_boundary <= bw, !is.na(zone_code))
  
  # 2) grouping keys: use (boundary_year, ward_pair) when available, else ward_pair
  group_keys <- intersect(c("boundary_year", "ward_pair"), names(df_bw))
  
  # 3) count parcels by zone within each group, requiring presence on BOTH sides
  #    prefer signed distance if available; else fall back to ward on each side
  zone_counts <-
    if ("signed_distance" %in% names(df_bw)) {
      df_bw %>%
        group_by(across(all_of(c(group_keys, "zone_code")))) %>%
        summarise(n = n(),
                  n_sides = n_distinct(sign(signed_distance)),
                  .groups = "drop") %>%
        filter(n_sides == 2)
    } else if ("ward" %in% names(df_bw)) {
      df_bw %>%
        group_by(across(all_of(c(group_keys, "zone_code")))) %>%
        summarise(n = n(),
                  n_sides = n_distinct(ward),
                  .groups = "drop") %>%
        filter(n_sides == 2)
    } else {
      # if neither side indicator is present, just compute modal zone
      df_bw %>%
        group_by(across(all_of(c(group_keys, "zone_code")))) %>%
        summarise(n = n(), .groups = "drop")
    }
  
  # 4) pick the modal zone (tie-breaker: alphabetical zone_code)
  modal_zone <- zone_counts %>%
    arrange(across(all_of(group_keys)), desc(n), zone_code) %>%
    group_by(across(all_of(group_keys))) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(all_of(group_keys), zone_code) %>%
    rename(modal_zone_code = zone_code)
  
  # 5) keep parcels in the modal zone for each group
  df_bw %>%
    inner_join(modal_zone, by = group_keys) %>%
    filter(zone_code == modal_zone_code) %>%
    select(-modal_zone_code)
}

parcels_fe <- restrict_to_modal_zone(parcels, 264)


# --- 3. LOOP THROUGH BANDWIDTHS AND RUN REGRESSIONS ---
# =======================================================================================
cat("Running regressions for yvar:", yvar, "across", length(bandwidths), "bandwidths...\n")

model_list <- lapply(bandwidths, function(bw) {
  parcels_fe <- restrict_to_modal_zone(parcels, bw)
  
  fe_model <- feols(
    fml = as.formula(paste0(
      yvar,
      " ~ strictness_own_std | construction_year + ward_pair"
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
names(model_list) <- paste0(round((bandwidths/ 5280), 2), "mi")

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
  "strictness_own_std" = "Restrictiveness Score",
  "construction_year"      = "Year",
  'ward_pair' = "Ward Pair",
  'ward' = "Ward",
  "density_far" = "Floor Area Ratio (FAR)",
  "density_lapu" = "Lot Area Per Unit (LAPU)",
  "density_bcr" = "Building Coverage Ratio (BCR)",
  "density_lps" = "Lot Size Per Story (LPS)",
  "density_spu" = "Sq. Feet Per Unit (SPU)"
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
  keep = "Restrictiveness Score",
  # Formatting options
  fitstat = ~ n + myo + nwp,
  style.tex = style.tex("aer", model.format = ""),
  depvar      = FALSE,
  digits = 2,
  dict        = rename_dict,
  # General options
  headers     = names(model_list),
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fixef.group = TRUE,
  file = output_filename, # Save to the specified file
  replace = TRUE
)

cat("✓ Table saved to:", output_filename, "\n")
