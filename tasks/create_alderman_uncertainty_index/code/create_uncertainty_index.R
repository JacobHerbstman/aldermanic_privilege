## Create Alderman Uncertainty Index
## This script runs permit-level residualization and computes alderman-level moments
## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_alderman_uncertainty_index/code")

source("../../setup_environment/code/packages.R")
library(optparse)
library(fixest)

# -----------------------------------------------------------------------------
# ARGUMENTS FOR MANUAL TESTING (uncomment when running make)
# -----------------------------------------------------------------------------
# PERMIT_TYPE_FE <- FALSE
# REVIEW_TYPE_FE <- TRUE
# INCLUDE_PORCH <- FALSE
# CA_FE <- FALSE
# TWO_STAGE <- TRUE

# -----------------------------------------------------------------------------
# PARSE COMMAND LINE ARGUMENTS
# -----------------------------------------------------------------------------

option_list <- list(
  make_option("--permit_type_fe", type = "character", default = "TRUE",
              help = "Include permit type fixed effects [default: TRUE]"),
  make_option("--review_type_fe", type = "character", default = "TRUE",
              help = "Include review type fixed effects [default: TRUE]"),
  make_option("--include_porch", type = "character", default = "TRUE",
              help = "Include porch permits [default: TRUE]"),
  make_option("--ca_fe", type = "character", default = "FALSE",
              help = "Include community area fixed effects [default: FALSE]"),
  make_option("--two_stage", type = "character", default = "FALSE",
              help = "Use two-stage estimation with ward-month aggregation [default: FALSE]")
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Convert string to logical
PERMIT_TYPE_FE <- toupper(opt$permit_type_fe) == "TRUE"
REVIEW_TYPE_FE <- toupper(opt$review_type_fe) == "TRUE"
INCLUDE_PORCH <- toupper(opt$include_porch) == "TRUE"
CA_FE <- toupper(opt$ca_fe) == "TRUE"
TWO_STAGE <- toupper(opt$two_stage) == "TRUE"

message("=== Creating Alderman Uncertainty Index ===")
message("Parameters:")
message("  PERMIT_TYPE_FE: ", PERMIT_TYPE_FE)
message("  REVIEW_TYPE_FE: ", REVIEW_TYPE_FE)
message("  INCLUDE_PORCH: ", INCLUDE_PORCH)
message("  CA_FE: ", CA_FE)
message("  TWO_STAGE: ", TWO_STAGE)

# Construct output filename
output_suffix <- paste0(
  "ptfe", ifelse(PERMIT_TYPE_FE, "TRUE", "FALSE"),
  "_rtfe", ifelse(REVIEW_TYPE_FE, "TRUE", "FALSE"),
  "_porch", ifelse(INCLUDE_PORCH, "TRUE", "FALSE"),
  "_cafe", ifelse(CA_FE, "TRUE", "FALSE"),
  ifelse(TWO_STAGE, "_2stage", "")
)
output_file <- paste0("../output/alderman_uncertainty_index_", output_suffix, ".csv")

# -----------------------------------------------------------------------------
# LOAD DATA
# -----------------------------------------------------------------------------

message("\nLoading permit data...")
permits <- read_csv("../input/permits_for_uncertainty_index.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))

message("Total permits loaded: ", nrow(permits))

# -----------------------------------------------------------------------------
# OPTIONAL: FILTER OUT PORCH PERMITS
# -----------------------------------------------------------------------------

if (!INCLUDE_PORCH) {
  n_before <- nrow(permits)
  permits <- permits %>% filter(!is_porch)
  n_after <- nrow(permits)
  message("Excluded porch permits: ", n_before - n_after)
  message("Permits after exclusion: ", n_after)
}

# -----------------------------------------------------------------------------
# FILTER: REQUIRE ALDERMEN TO HAVE >3 MONTHS OF DATA
# -----------------------------------------------------------------------------

coverage <- permits %>%
  group_by(alderman) %>%
  summarise(n_months = n_distinct(month), .groups = "drop")

keep_aldermen <- coverage %>%
  filter(n_months > 3) %>%
  pull(alderman)

n_before <- nrow(permits)
n_aldermen_before <- n_distinct(permits$alderman)
permits <- permits %>% filter(alderman %in% keep_aldermen)
n_after <- nrow(permits)
n_aldermen_after <- n_distinct(permits$alderman)

message("\\nAlderman coverage filter (>3 months required):")
message("  Aldermen before: ", n_aldermen_before)
message("  Aldermen after: ", n_aldermen_after)
message("  Aldermen dropped: ", n_aldermen_before - n_aldermen_after)
message("  Permits after: ", n_after)

# -----------------------------------------------------------------------------
# BUILD REGRESSION FORMULA
# -----------------------------------------------------------------------------

# Covariates (ward demographics + place controls)
covariates <- c(
  # Ward demographics
  "median_hh_income", "share_black", "share_hisp", "share_white",
  "homeownership_rate", "share_bach_plus", "pop_total",

  # Legacy place controls from ward geometry (map-version specific)
  "dist_cbd_km", "lakefront_share_1km", "n_rail_stations_800m"
)

# Fixed effects (conditional)
fe_terms <- c("month")  # Always include month FE

if (PERMIT_TYPE_FE) {
  fe_terms <- c(fe_terms, "permit_type_clean")
}

if (REVIEW_TYPE_FE) {
  fe_terms <- c(fe_terms, "review_type_clean")
}

if (CA_FE) {
  fe_terms <- c(fe_terms, "ca_id")
}

# Build formula
covar_part <- paste(covariates, collapse = " + ")
fe_part <- paste(fe_terms, collapse = " + ")
formula_str <- paste0("log_processing_time ~ ", covar_part, " | ", fe_part)
reg_formula <- as.formula(formula_str)

message("\nRegression formula:")
message("  ", formula_str)

# -----------------------------------------------------------------------------
# RUN PERMIT-LEVEL RESIDUALIZATION
# -----------------------------------------------------------------------------

message("\nRunning permit-level regression...")

# First, identify complete cases for the regression
# We need: log_processing_time, all covariates that exist, and FE variables
regression_vars <- c("log_processing_time", covariates, fe_terms, "alderman")

# Filter to complete cases for regression (but keep all columns)
permits_complete <- permits %>%
  filter(!is.na(log_processing_time)) %>%
  # For FE variables, we just need them to be non-NA
  filter(!is.na(month)) %>%
  filter(!is.na(permit_type_clean)) %>%
  filter(!is.na(review_type_clean))

message("Permits with complete outcome and FE vars: ", nrow(permits_complete))

# Run regression with fixest on the complete data
model <- feols(reg_formula, data = permits_complete, warn = FALSE)

# Summary statistics
message("Regression summary:")
message("  Observations used: ", model$nobs)
message("  R-squared: ", round(r2(model, type = "ar2"), 4))

# Note about observations dropped due to covariate NAs
if (model$nobs < nrow(permits_complete)) {
  message("  Note: ", nrow(permits_complete) - model$nobs, 
          " additional obs dropped due to covariate NAs")
}

# Extract residuals - these align with observations that made it through feols
# The fitted values and residuals are for model$nobs observations
# We need to identify which rows those are

# Use the non-NA pattern from fitted values to identify used observations
# feols internally tracks this - we can rebuild by checking which rows had all required data
permits_complete$resid <- NA_real_

# Get the rows that were actually used (feols drops rows with NA in covariates)
# The residuals vector length equals model$nobs, so we assign to first nobs rows that are complete
# Actually, we need to be more careful - let's use an index

# Simplest approach: augment the model with residuals
# feols residuals() returns a named vector with row indices if the data had rownames
# Or we can use the observation sample

# The cleanest approach: run on rows with all required data (no NA or Inf)
required_cols <- c("log_processing_time", covariates)

# Filter to finite values only (no NA, no Inf, no -Inf)
permits_for_reg <- permits_complete %>%
  # Add row index before filtering
  mutate(.row_idx = row_number()) %>%
  # First filter infinite values in log columns
  filter(is.finite(log_processing_time)) %>%
  # Then filter complete cases for the subset of columns that exist
  filter(if_all(all_of(required_cols[required_cols %in% names(.)]), 
                ~ !is.na(.) & is.finite(.)))

# Also filter for non-NA FE variables if CA_FE is TRUE
if (CA_FE) {
  permits_for_reg <- permits_for_reg %>%
    filter(!is.na(ca_id))
}

message("Permits with complete finite covariates: ", nrow(permits_for_reg))

# Run on this subset
model <- feols(reg_formula, data = permits_for_reg, warn = FALSE)
message("Final model observations: ", model$nobs)

# Handle observations dropped by feols (e.g., singleton FE levels)
# feols stores info about removed observations
if (model$nobs < nrow(permits_for_reg)) {
  message("Note: ", nrow(permits_for_reg) - model$nobs, 
          " obs dropped by feols (likely singleton FE levels)")
  
  # Get the rows that were used in the model
  # feols stores the logical vector of kept observations
  resid_vec <- rep(NA_real_, nrow(permits_for_reg))
  kept_obs <- model$obs_selection$obsRemoved
  if (!is.null(kept_obs)) {
    resid_vec[!kept_obs] <- residuals(model)
  } else {
    # Alternative: just assign residuals to first n_obs rows
    # This is a fallback that may not be correct in edge cases
    message("Warning: Using fallback residual assignment")
    resid_vec[1:model$nobs] <- residuals(model)
  }
  permits_for_reg$resid <- resid_vec
} else {
  # Perfect alignment
  permits_for_reg$resid <- residuals(model)
}

# Check how many permits have residuals
n_with_resid <- sum(!is.na(permits_for_reg$resid))
message("Permits with residuals: ", n_with_resid)


# -----------------------------------------------------------------------------
# AGGREGATE RESIDUALS TO ALDERMAN LEVEL
# -----------------------------------------------------------------------------

message("\nAggregating to alderman level...")

if (TWO_STAGE) {
  # -------------------------------------------------------------------------
  # TWO-STAGE APPROACH: Aggregate to ward-month, estimate alderman coefficients
  # with i(alderman), then apply Empirical Bayes shrinkage
  # -------------------------------------------------------------------------
  message("  Using two-stage estimation with EB shrinkage...")

  # Step 1: Aggregate permit residuals to ward-month-alderman level
  ward_month_resid <- permits_for_reg %>%
    filter(!is.na(resid)) %>%
    group_by(ward, month, alderman) %>%
    summarise(
      mean_resid_wm = mean(resid, na.rm = TRUE),
      sd_resid_wm = sd(resid, na.rm = TRUE),
      var_resid_wm = var(resid, na.rm = TRUE),
      n_permits_wm = n(),
      .groups = "drop"
    )

  message("  Ward-month-alderman observations: ", nrow(ward_month_resid))

  # Choose reference alderman (one with many observations for stability)
  ref_alderman <- "Andre Vasquez"
  message("  Reference alderman: ", ref_alderman)

  # Step 2: Run Stage 2 regression with alderman as predictor (not FE absorber)
  # This gives us coefficients and standard errors for EB shrinkage
  stage2_model <- feols(
    mean_resid_wm ~ i(alderman, ref = ref_alderman),
    data = ward_month_resid,
    weights = ~n_permits_wm,
    vcov = ~ward,  # Cluster by ward and month
    warn = FALSE
  )

  message("  Stage 2 observations: ", stage2_model$nobs)
  message("  Stage 2 R-squared: ", round(r2(stage2_model, type = "ar2"), 4))

  # Step 3: Extract coefficients and standard errors
  coefs <- enframe(coef(stage2_model), name = "term", value = "alderman_fe")
  ses <- enframe(se(stage2_model), name = "term", value = "alderman_se")

  # Parse alderman names from coefficient names
  alderman_effects <- coefs %>%
    filter(str_detect(term, "^alderman::")) %>%
    mutate(alderman = str_remove(term, "^alderman::")) %>%
    left_join(
      ses %>%
        filter(str_detect(term, "^alderman::")) %>%
        mutate(alderman = str_remove(term, "^alderman::")) %>%
        select(alderman, alderman_se),
      by = "alderman"
    ) %>%
    select(alderman, alderman_fe, alderman_se)

  # Add reference alderman with FE = 0, SE = 0
  ref_row <- tibble(alderman = ref_alderman, alderman_fe = 0, alderman_se = 0)
  alderman_effects <- bind_rows(alderman_effects, ref_row)

  message("  Aldermen with estimates: ", nrow(alderman_effects))

  # Step 4: Apply Empirical Bayes shrinkage
  message("  Applying Empirical Bayes shrinkage...")

  # Estimate tau^2 (true variance across aldermen)
  tau2 <- max(0, var(alderman_effects$alderman_fe, na.rm = TRUE) -
                 mean(alderman_effects$alderman_se^2, na.rm = TRUE))

  message("    tau^2 (signal variance): ", round(tau2, 6))
  message("    mean(SE^2): ", round(mean(alderman_effects$alderman_se^2, na.rm = TRUE), 6))

  alderman_effects <- alderman_effects %>%
    mutate(
      shrinkage_B = tau2 / (tau2 + alderman_se^2),
      alderman_fe_shrunk = alderman_fe * shrinkage_B
    )

  message("    Mean shrinkage factor: ", round(mean(alderman_effects$shrinkage_B, na.rm = TRUE), 4))

  # Step 5: Create alderman_moments with shrunk FEs as mean_resid
  # Also compute permit-level stats for sd_resid and var_resid
  permit_level_stats <- permits_for_reg %>%
    filter(!is.na(resid)) %>%
    group_by(alderman) %>%
    summarise(
      n_permits = n(),
      sd_resid = sd(resid, na.rm = TRUE),
      var_resid = var(resid, na.rm = TRUE),
      .groups = "drop"
    )

  alderman_moments <- alderman_effects %>%
    select(alderman, mean_resid = alderman_fe_shrunk,
           alderman_fe_raw = alderman_fe, alderman_se, shrinkage_B) %>%
    left_join(permit_level_stats, by = "alderman")

  # Save Stage 2 regression table
  message("\nCreating stage 2 regression table...")
  spec_name_clean <- output_suffix

  etable(
    stage2_model,
    digits = 3,
    se.below = TRUE,
    depvar = FALSE,
    headers = c("Mean Residual"),
    fitstat = ~ n + r2,
    file = paste0("../output/stage2_regression_", spec_name_clean, ".tex"),
    replace = TRUE,
    style.tex = style.tex(
      main = "aer",
      model.format = "",
      fixef.title = "",
      fixef.suffix = "",
      yesNo = c("$\\checkmark$", "")
    )
  )
  message("  Saved: ../output/stage2_regression_", spec_name_clean, ".tex")

} else {
  # -------------------------------------------------------------------------
  # SIMPLE APPROACH: Direct group-by mean of permit residuals
  # -------------------------------------------------------------------------
  message("  Using simple group-by mean...")

  alderman_moments <- permits_for_reg %>%
    filter(!is.na(resid)) %>%
    group_by(alderman) %>%
    summarise(
      n_permits = n(),
      mean_resid = mean(resid, na.rm = TRUE),
      sd_resid = sd(resid, na.rm = TRUE),
      var_resid = var(resid, na.rm = TRUE),
      .groups = "drop"
    )
}

message("Unique aldermen: ", nrow(alderman_moments))
message("Aldermen with 1 permit (NA variance): ", sum(alderman_moments$n_permits == 1, na.rm = TRUE))

# -----------------------------------------------------------------------------
# CREATE UNCERTAINTY INDICES
# -----------------------------------------------------------------------------

message("\nCreating uncertainty indices...")

# Standardization function
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Simplified index: use standardized mean residual (strictness index)
alderman_index <- alderman_moments %>%
  mutate(
    # Standardized mean residual = strictness index
    uncertainty_index = standardize(mean_resid)
  )

# Final column selection - simplified
if (TWO_STAGE) {
  alderman_index <- alderman_index %>%
    select(
      alderman,
      n_permits,
      mean_resid,
      alderman_fe_raw,
      alderman_se,
      shrinkage_B,
      uncertainty_index
    )
} else {
  alderman_index <- alderman_index %>%
    select(
      alderman,
      n_permits,
      mean_resid,
      uncertainty_index
    )
}

# Summary statistics
message("\nIndex summary statistics:")
message("  Strictness index (standardized mean residual):")
message("    Mean: ", round(mean(alderman_index$uncertainty_index, na.rm = TRUE), 4))
message("    SD: ", round(sd(alderman_index$uncertainty_index, na.rm = TRUE), 4))
message("    Min: ", round(min(alderman_index$uncertainty_index, na.rm = TRUE), 4))
message("    Max: ", round(max(alderman_index$uncertainty_index, na.rm = TRUE), 4))

# -----------------------------------------------------------------------------
# STAGE 1 REGRESSION TABLE
# -----------------------------------------------------------------------------

message("\nCreating stage 1 regression table...")

# Clean spec name for filenames
spec_name_clean <- output_suffix

# Create LaTeX table using etable
etable(
  model,
  digits = 3, 
  se.below = TRUE,
  depvar = FALSE,
  headers = c("Log Processing Time"),
  dict = c(
    median_hh_income = "Median HH Income",
    share_black = "Share Black",
    share_hisp = "Share Hispanic",
    share_white = "Share White",
    homeownership_rate = "Homeownership Rate",
    share_bach_plus = "Share Bachelor's+",
    pop_total = "Population",
    dist_cbd_km = "Dist. to CBD (km)",
    lakefront_share_1km = "Lakefront Share (legacy)",
    n_rail_stations_800m = "CTA Stations (legacy 800)"
  ),
  fixef.group = list(
    "Month FE" = "month",
    "Permit Type FE" = "permit_type_clean",
    "Review Type FE" = "review_type_clean"
  ),
  fitstat = ~ n + r2,
  file = paste0("../output/stage1_regression_", spec_name_clean, ".tex"),
  replace = TRUE,
  style.tex = style.tex(
    main = "aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  )
)

message("  Saved: ../output/stage1_regression_", spec_name_clean, ".tex")

# -----------------------------------------------------------------------------
# CREATE PLOT
# -----------------------------------------------------------------------------

message("\nCreating plot...")

plot_data <- alderman_index %>%
  filter(!is.na(uncertainty_index)) %>%
  arrange(uncertainty_index) %>%
  mutate(alderman = factor(alderman, levels = alderman))

p <- ggplot(plot_data, aes(x = uncertainty_index, y = alderman, fill = uncertainty_index)) +
  geom_col() +
  scale_fill_distiller(
    palette = "RdYlBu",
    direction = -1,
    name = "Index"
  ) +
  labs(
    title = "Alderman Strictness Index",
    subtitle = paste0("Higher = longer processing times | Spec: ", gsub("_", " ", output_suffix)),
    x = "Strictness Index (standardized mean residual)",
    y = NULL
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray60")
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7)

ggsave(
  paste0("../output/uncertainty_index_", spec_name_clean, ".pdf"),
  plot = p,
  width = 10, height = 14, device = "pdf", bg = "white"
)
message("  Saved: ../output/uncertainty_index_", spec_name_clean, ".pdf")

# -----------------------------------------------------------------------------
# SAVE OUTPUT
# -----------------------------------------------------------------------------

message("\nSaving CSV output to: ", output_file)
write_csv(alderman_index, output_file)

message("\n=== Strictness index creation complete ===")
message("Output file: ", output_file)
message("Aldermen: ", nrow(alderman_index))
message("Plot saved: 1")
message("Stage 1 table saved: 1")
