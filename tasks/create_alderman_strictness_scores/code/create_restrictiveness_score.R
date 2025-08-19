## this code generates a simple alderman restrictiveness score based on pop. density, homeonwer rates, and incomes at the ward level

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

data <- read_csv("../input/ward_monthly_panel_for_alderman_fe.csv")

# =============================================================================
# 1. DEFINE OUTCOME VARIABLES AND CONTROLS
# =============================================================================

# Primary outcome variables of interest
outcome_vars <- list(
  # Permit outcomes
  "log_n_permits_issued" = "log(n_permits_issued)", 
  
  "permit_approval_rate" = "permit_approval_rate",
  
  # Processing time outcomes (log)
  "log_median_processing_time" = "log(mean_processing_time)", 
  
  # Fee outcomes (log)
  "log_mean_total_fee" = "log(mean_total_fee)"
)

# Control variables (consistent across models)
control_vars <- c(
  "homeownership_rate",
  "population_density", 
  "median_income",
  "percent_black",
  "percent_hispanic"
)

# Fixed effects specification
fe_spec <- "ward+month"  # Start simple to avoid collinearity

# Reference alderman for dummy variables
ref_alderman <- "Andre Vasquez"  # Or choose most common/neutral

permit_outcomes <- c("log_n_permits_issued")

# =============================================================================
# 2. RUN REGRESSIONS FOR ALL OUTCOMES
# =============================================================================

# Initialize storage for results
all_scores <- list()
all_models <- list()

# Loop through outcomes
for (outcome_name in names(outcome_vars)) {
  outcome_var <- outcome_vars[[outcome_name]]
  
  message("Running regression for: ", outcome_name)
  
  if (outcome_name %in% permit_outcomes) {  # Fixed: use outcome_name not outcome_var
    # For permit outcomes: no weights, no n_permits_applied control
    controls_to_use <- control_vars
    weight_formula <- NULL
    weight_str <- ""
  } else {
    # For rate/time/fee outcomes: weight by n_permits_applied, no control
    controls_to_use <- control_vars
    weight_formula <- ~n_permits_applied
    weight_str <- " [weighted]"
  }
  
  # Build formula
  controls_str <- paste(controls_to_use, collapse = " + ")
  alderman_str <- paste0("i(alderman, ref = '", ref_alderman, "')")
  
  formula_str <- paste0(
    outcome_var, " ~ ", 
    controls_str, " + ",
    alderman_str, " | ",
    fe_spec
  )
  
  reg_formula <- as.formula(formula_str)
  
  # Run regression
  model <- feols(
    reg_formula,
    data = data,
    vcov = ~alderman
  )
  
  # Store model
  all_models[[outcome_name]] <- model
  
  # Extract alderman coefficients and standard errors
  alderman_coefs <- coef(model)
  alderman_ses <- se(model)
  
  # Get alderman effects
  alderman_effects <- alderman_coefs[grepl("alderman::", names(alderman_coefs))]
  alderman_se_vals <- alderman_ses[grepl("alderman::", names(alderman_ses))]
  
  # Clean names and add reference alderman
  names(alderman_effects) <- gsub("alderman::", "", names(alderman_effects))
  names(alderman_se_vals) <- gsub("alderman::", "", names(alderman_se_vals))
  
  # Add reference alderman (coefficient = 0, se = 0)
  alderman_scores <- c(setNames(0, ref_alderman), alderman_effects)
  alderman_se_all <- c(setNames(0, ref_alderman), alderman_se_vals)
  
  # Create results data frame for this outcome
  scores_df <- data.frame(
    alderman = names(alderman_scores),
    alderman_fe = as.numeric(alderman_scores),
    alderman_se = as.numeric(alderman_se_all),
    outcome_variable = outcome_name,
    n_obs = nobs(model),
    weighted = !is.null(weight_formula),
    stringsAsFactors = FALSE
  )
  
  # Store in list
  all_scores[[outcome_name]] <- scores_df
  
  message("✓ ", outcome_name, weight_str, " (n=", nobs(model), ")")
}

# Combine all results into one long data frame
all_alderman_scores <- bind_rows(all_scores)

# Create wide format for coefficients
alderman_scores_wide <- all_alderman_scores %>%
  select(alderman, alderman_fe, outcome_variable) %>%
  pivot_wider(
    names_from = outcome_variable,
    values_from = alderman_fe,
    names_prefix = "fe_"
  )


# =============================================================================
# 3. DESCRIPTIVE PLOTS OF FEs
# =============================================================================

outcome_names <- unique(all_alderman_scores$outcome_variable)


# Create a function to make individual bar charts
create_restrictiveness_chart <- function(outcome_var, data) {
  
  # Filter and order data for this outcome
  plot_data <- data %>%
    filter(outcome_variable == outcome_var) %>%
    arrange(alderman_fe) %>%
    mutate(alderman = factor(alderman, levels = alderman))  # Keep order
  
  # Determine color scheme (red for more restrictive, blue for less)
  robust_pattern <- "time|fee|cost"
  
  plot_data <- plot_data %>%
    mutate(
      restrictive_color = case_when(
        alderman_fe == 0 ~ "Reference",
        grepl(robust_pattern, outcome_var) & alderman_fe > 0 ~ "More Restrictive",
        grepl(robust_pattern, outcome_var) & alderman_fe < 0 ~ "Less Restrictive",
        !grepl(robust_pattern, outcome_var) & alderman_fe > 0 ~ "Less Restrictive",
        !grepl(robust_pattern, outcome_var) & alderman_fe < 0 ~ "More Restrictive",
        TRUE ~ "Reference"
      )
    )
  
  # Clean up the title
  clean_title <- outcome_var %>%
    gsub("log_", "Log ", .) %>%      # Handle log_ prefix
    gsub("_", " ", .) %>%            # Replace underscores with spaces
    str_to_title()                   # Title case
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = alderman, y = alderman_fe, fill = restrictive_color)) +
    geom_col() +
    scale_fill_manual(
      values = c("More Restrictive" = "#d73027", 
                 "Less Restrictive" = "#4575b4", 
                 "Reference" = "#999999"),
      name = ""
    ) +
    labs(
      title = paste("Alderman Restrictiveness:", clean_title),
      subtitle = "Ordered from least restrictive (left) to most restrictive (right)",
      x = "Alderman",
      y = "Fixed Effect Coefficient",
      caption = "Reference alderman (Andre Vasquez) set to 0"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  # Very small font
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray60")
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)
  
  return(p)
}

# Create all charts using ACTUAL outcome variable names from data
charts <- map(outcome_names, ~create_restrictiveness_chart(.x, all_alderman_scores))

# Use the actual outcome names for the list names
names(charts) <- outcome_names

# Now access charts with the correct names:
# charts[["permit_approval_rate"]]
# charts[["log_mean_total_fee"]]      # These should match what's actually in your data
# charts[["log_n_permits_issued"]]
# charts[["log_median_processing_time"]]


# =============================================================================
# 4. APPLY EMPIRICAL BAYES SHRINKAGE
# =============================================================================
message("Applying Empirical Bayes shrinkage to alderman fixed effects...")

alderman_scores_shrunk <- all_alderman_scores %>%
  # Group by outcome, as shrinkage must be applied to each set of FEs separately
  group_by(outcome_variable) %>%
  mutate(
    # Step 1: Estimate the variance of the TRUE alderman effects (sigma_alpha^2).
    # This is the average squared FE minus the average estimation variance.
    sigma2_alpha = max(0, mean(alderman_fe^2) - mean(alderman_se^2)),
    
    # Step 2: Calculate the shrinkage factor B for each alderman.
    # B = (true variance) / (true variance + estimation variance)
    shrinkage_factor_B = sigma2_alpha / (sigma2_alpha + alderman_se^2),
    
    # Step 3: Compute the shrunken fixed effect.
    # Shrunken FE = Original FE * Shrinkage Factor
    alderman_fe = alderman_fe * shrinkage_factor_B
  ) %>%
  ungroup() # Always ungroup after a grouped mutation

message("✓ Shrinkage complete.")




charts <- map(outcome_names, ~create_restrictiveness_chart(.x, alderman_scores_shrunk))

# Use the actual outcome names for the list names
names(charts) <- outcome_names

# Now access charts with the correct names:
# charts[["permit_approval_rate"]]
# charts[["log_mean_total_fee"]]      # These should match what's actually in your data
# charts[["log_n_permits_issued"]]
# charts[["log_median_processing_time"]]



# =============================================================================
# 4. USE PCA TO GET FINAL STRICTNESS SCORE
# =============================================================================

sign_flip_pattern <- "fee|time|cost"

pca_data_wide <- alderman_scores_shrunk %>%
  mutate(
    signed_fe = if_else(
      # Condition: If the variable is one where "positive = less strict"
      grepl(sign_flip_pattern, outcome_variable),
      # Action if TRUE: Flip the sign to make "positive = more strict"
      alderman_fe * -1,
      # Action if FALSE: Keep the original sign (it's already correct)
      alderman_fe
    )
  ) %>%
  # Step 2: Pivot to a wide format
  select(alderman, outcome_variable, signed_fe) %>%
  pivot_wider(
    names_from = outcome_variable,
    values_from = signed_fe
  ) 

pca_data_for_prcomp <- pca_data_wide %>%
  column_to_rownames("alderman")

pca_results <- prcomp(pca_data_for_prcomp, scale = TRUE, center = TRUE)

pc1_loadings <- pca_results$rotation
print(pc1_loadings)

# Convert the results matrix (pca_results$x) to a data frame.
# The alderman names will be carried over as row names.
final_scores <- as.data.frame(pca_results$x) %>%
  
  # Use tibble::rownames_to_column() to safely move the row names
  # into a new column called "alderman".
  rownames_to_column("alderman") %>%
  
  # Select and rename the columns you want.
  select(alderman, strictness_index = PC1) %>%
  
  # Now you can safely arrange the results.
  arrange(strictness_index)

message("✓ PCA complete. Final strictness index created robustly.")

write_csv(final_scores, "../output/alderman_restrictiveness_scores.csv")



# permits issued: positive = better
# approval rate: positive = better
# processing time: negative = better
# total fees: negative = better






# # =============================================================================
# # 5. CREATE STRICTNESS INDEX VIA AVERAGING (ALTERNATIVE TO PCA)
# # =============================================================================
# message("PCA is not interpretable. Creating an index by averaging Z-scores...")
# 
# # We'll use the 'pca_data_wide' since it's already correctly signed.
# # Note: The data frame MUST NOT have non-numeric columns (like alderman) for the next step.
# alderman_names <- rownames(pca_data_wide)
# numeric_data <- as.data.frame(scale(pca_data_wide[,-1])) # scale() creates Z-scores
# 
# # Calculate the average Z-score for each alderman
# numeric_data$strictness_index <- rowMeans(numeric_data, na.rm = TRUE)
# 
# # Create the final, clean data frame
# final_scores_avg <- data.frame(
#   alderman = alderman_names,
#   strictness_index = numeric_data$strictness_index
# ) %>%
#   arrange(strictness_index)
# 
# message("✓ Average Z-score index created.")
# 
# # View the results
# head(final_scores_avg)
# tail(final_scores_avg)


