## this code creates alderman strictness scores. The first function, which creates the scores, runs the regressions, 
## applies empirical bayes shrinkage to the raw estimates, and then uses PCA to generate a strictness index. 

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

# Load data and packages
source("../../setup_environment/code/packages.R")
data <- read_csv("../input/ward_monthly_panel_for_alderman_fe.csv")

# ----------------------------
# A) Build outcome columns
# ----------------------------
data <- data %>%
  mutate(
    log_n_permits_issued      = if_else(n_permits_issued > 0, log(n_permits_issued), NA_real_),
    log_mean_processing_time  = if_else(mean_processing_time > 0, log(mean_processing_time), NA_real_),
    log_mean_total_fee        = if_else(mean_total_fee > 0, log(mean_total_fee), NA_real_), 
    log_sum_total_fee         = if_else(sum_total_fee  > 0, log(sum_total_fee),  NA_real_)   
  )

coverage <- data %>%
  dplyr::group_by(alderman) %>%
  dplyr::summarise(n_months = dplyr::n_distinct(month),
                   permits  = sum(n_permits_applied, na.rm = TRUE),
                   .groups = "drop")

keep_aldermen <- coverage %>% dplyr::filter(n_months > 1) %>% dplyr::pull(alderman) ## need > 3 months of data

data <- data %>% dplyr::filter(alderman %in% keep_aldermen)

# ----------------------------
# B) Pre-residualize each outcome on ward fundamentals + month FEs
#     (NO alderman dummies here)
# ----------------------------
# Fundamentals:
geo_vars <- c(
  "dist_cbd_km", "lakefront_share_1km", "n_rail_stations_800m",
  grep("^ca_share_", names(data), value = TRUE)  # community-area shares
)

# You already have these ward controls; include them as predetermined fundamentals
demo_vars <- c("homeownership_rate", "population_density", "median_income",
               "percent_black", "percent_hispanic")

fundamentals <- c(geo_vars, demo_vars)

residualize_var <- function(df, yvar, fundamentals, weight_var = NULL) {
  # keep only rows with all inputs present
  vars_needed <- c(yvar, fundamentals, "month")
  if (!is.null(weight_var)) vars_needed <- c(vars_needed, weight_var)
  ok <- stats::complete.cases(df[, vars_needed])
  
  if (!any(ok)) {
    df[[paste0("resid_", yvar)]] <- NA_real_
    return(df)
  }
  
  rhs <- if (length(fundamentals)) paste(fundamentals, collapse = " + ") else "1"
  fml <- as.formula(paste0(yvar, " ~ ", rhs, " | month"))
  
  w_form <- if (!is.null(weight_var)) as.formula(paste0("~", weight_var)) else NULL
  fit <- feols(fml, data = df[ok, ], weights = w_form)
  
  # in-sample residuals for the rows used; NA elsewhere
  res <- rep(NA_real_, nrow(df))
  res[ok] <- resid(fit)
  df[[paste0("resid_", yvar)]] <- res
  df
}

# Residualize each outcome according to weighting rule:
#  - DO NOT weight "log_n_permits_issued, log_sum_total_fee"
#  - Weight others by n_permits_applied
data <- residualize_var(data, "log_n_permits_issued",     fundamentals, weight_var = NULL)
data <- residualize_var(data, "log_sum_total_fee",        fundamentals, weight_var = NULL)
data <- residualize_var(data, "permit_approval_rate",      fundamentals, weight_var = "n_permits_applied")
data <- residualize_var(data, "log_mean_processing_time",  fundamentals, weight_var = "n_permits_applied")
data <- residualize_var(data, "log_mean_total_fee",        fundamentals, weight_var = "n_permits_applied")



# Build the RHS used in residualization
ca_cols   <- grep("^ca_share_", names(data), value = TRUE)
geo_vars  <- c("dist_cbd_km", "lakefront_share_1km", "n_rail_stations_800m")
demo_vars <- c("homeownership_rate", "population_density", "median_income",
               "percent_black", "percent_hispanic")
fundamentals <- c(geo_vars, demo_vars, ca_cols)

rhs <- if (length(fundamentals)) paste(fundamentals, collapse = " + ") else "1"

# Helper to fit the displayed models (same spec as residualization)
fit_disp <- function(y, w = NULL) {
  fml <- as.formula(paste0(y, " ~ ", rhs, " | month"))
  wgt <- if (is.null(w)) NULL else as.formula(paste0("~", w))
  feols(fml, data = data, weights = wgt)
}

m1 <- fit_disp("log_n_permits_issued")                       # unweighted
m2 <- fit_disp("log_sum_total_fee")          # unweighted
m3 <- fit_disp("permit_approval_rate",     "n_permits_applied")
m4 <- fit_disp("log_mean_processing_time", "n_permits_applied")
m5 <- fit_disp("log_mean_total_fee",       "n_permits_applied")

etable(
  m1, m3, m4,
  drop   = "^ca_share_",                 # hide community-area shares
  digits = 3, se.below = TRUE,
  dict = c(
    `(Intercept)` = "Constant",
    dist_cbd_km = "Dist. to CBD (km)",
    lakefront_share_1km = "Lakefront share (≤1km)",
    n_rail_stations_800m = "CTA stations (≤800m)",
    homeownership_rate = "Homeownership rate",
    population_density = "Population density",
    median_income = "Median income",
    percent_black = "% Black",
    percent_hispanic = "% Hispanic"
  ),
  fitstat = ~ n + r2,
  title = "Stage-1 residualization models (month FE included)",
  notes = "CA shares included in estimation but omitted from display; weights used for cols 2 and 3."
)

etable(
  m1, m3, m4,
  drop   = "^ca_share_",                 # hide community-area shares
  digits = 3, se.below = TRUE,
  dict = c(
    `(Intercept)` = "Constant",
    dist_cbd_km = "Dist. to CBD (km)",
    lakefront_share_1km = "Lakefront share (≤1km)",
    n_rail_stations_800m = "CTA stations (≤800m)",
    homeownership_rate = "Homeownership rate",
    population_density = "Population density",
    median_income = "Median income",
    percent_black = "% Black",
    percent_hispanic = "% Hispanic"
  ),
  fitstat = ~ n + r2,
  title = "Stage-1 residualization models (month FE included)",
  notes = "CA shares included in estimation but omitted from display; weights used for cols 2 and 3.",
  file = "../output/stage1_residualization_models.tex"
)



# =============================================================================
# 1. FUNCTION DEFINITION FOR FEs
# =============================================================================
#' Calculate Alderman Restrictiveness Scores
#'
#' This function runs a series of regressions, applies Empirical Bayes shrinkage,
#' and uses PCA to generate a single alderman restrictiveness index.
#'
#' @param data The input data frame (e.g., your monthly ward panel).
#' @param outcome_vars A named list of outcome variables for the regressions.
#' @param control_vars A character vector of control variables.
#' @param fe_spec A string defining the fixed effects (e.g., "month" or "ward+month").
#' @param ref_alderman The name of the alderman to use as the reference category.
#' @param output_filepath The full path to save the final CSV file.
#' @param permit_outcomes A character vector of outcomes that should not be weighted.
#'
#' @return A data frame with two columns: 'alderman' and 'strictness_index'.


# # ## test vars
# outcome_vars <- list(
#   "log_n_permits_issued"       = "resid_log_n_permits_issued",
#   "permit_approval_rate"       = "resid_permit_approval_rate",
#   "log_mean_processing_time" = "resid_log_mean_processing_time"
#   # "log_mean_total_fee"         = "resid_log_mean_total_fee",
#   # "log_sum_total_fee"        = "resid_log_sum_total_fee"
# )
# outcome_name <- "log_n_permits_issued"
# control_vars <- "1"
# fe_spec <- "month"
# ref_alderman <- "Andre Vasquez"
# permit_outcomes = c("log_n_permits_issued")

calculate_alderman_scores <- function(data,
                                      outcome_vars,
                                      control_vars,
                                      fe_spec,
                                      ref_alderman,
                                      output_filepath,
                                      permit_outcomes = c("log_n_permits_issued")) {
  
  message(paste("\n--- Running analysis for FE spec:", fe_spec, "---"))
  
  # =============================================================================
  # A. RUN REGRESSIONS
  # =============================================================================
  all_scores <- list()
  for (outcome_name in names(outcome_vars)) {
    outcome_var <- outcome_vars[[outcome_name]]
    message("   ...regressing on ", outcome_name)
    
    weight_formula <- if (!outcome_name %in% permit_outcomes) ~n_permits_applied else NULL
    
    formula_str <- paste0(
      outcome_var, " ~ ",
      "i(alderman, ref = '", ref_alderman, "')"
    )
    print(formula_str)
    
    model <- feols(as.formula(formula_str), data = data, vcov = ~ward+month, weights = weight_formula)
    
    coefs <- enframe(coef(model), name = "term", value = "alderman_fe")
    ses <- enframe(se(model), name = "term", value = "alderman_se")
    
    alderman_effects <- coefs %>%
      filter(str_detect(term, "alderman::")) %>%
      mutate(alderman = str_remove(term, "alderman::")) %>%
      left_join(ses %>%
                  filter(str_detect(term, "alderman::")) %>%
                  mutate(alderman = str_remove(term, "alderman::")) %>%
                  select(alderman, alderman_se), by = "alderman")
    
    ref_row <- tibble(term = ref_alderman, alderman_fe = 0, alderman_se = 0, alderman = ref_alderman)
    
    scores_df <- bind_rows(alderman_effects, ref_row) %>%
      mutate(outcome_variable = outcome_name) %>%
      select(alderman, alderman_fe, alderman_se, outcome_variable)
    
    all_scores[[outcome_name]] <- scores_df
  }
  all_alderman_scores <- bind_rows(all_scores)
  
  # =============================================================================
  # B. APPLY EMPIRICAL BAYES SHRINKAGE
  # =============================================================================
  message("   ...applying Empirical Bayes shrinkage.")
  alderman_scores_shrunk <- all_alderman_scores %>%
    dplyr::group_by(outcome_variable) %>%
    dplyr::mutate(
      tau2 = pmax(0, stats::var(alderman_fe, na.rm = TRUE) -
                    mean(alderman_se^2, na.rm = TRUE)),
      shrinkage_factor_B = tau2 / (tau2 + alderman_se^2),
      alderman_fe = alderman_fe * shrinkage_factor_B
    ) %>%
    dplyr::ungroup()

  # =============================================================================
  # C. BUILD STRICTNESS INDEX (reliability-weighted PCA)
  # =============================================================================
  message("   ...computing reliability-weighted PC1 with enforced signs.")
  
  W <- alderman_scores_shrunk %>%
    dplyr::filter(outcome_variable %in% names(outcome_vars)) %>%
    dplyr::select(alderman, outcome_variable, alderman_fe) %>%
    tidyr::pivot_wider(names_from = outcome_variable, values_from = alderman_fe) %>%
    tibble::column_to_rownames("alderman") %>%
    as.matrix()
  
  # plain PCA; PC1 sign is arbitrary by definition
  pca_fit <- prcomp(W, center = TRUE, scale. = TRUE)
  pc1     <- pca_fit$x[, 1]
  
  final_scores <- tibble::tibble(
    alderman = rownames(W),
    strictness_index = pc1
  ) %>% dplyr::arrange(strictness_index)
  
  
  # =============================================================================
  # D. SAVE AND RETURN
  # =============================================================================

  write_csv(final_scores, output_filepath)
  message(paste("✓ Scores saved to:", output_filepath))
  
  # Create a list to return both final scores and the individual shrunken FEs
  results <- list(
    final_scores = final_scores,
    individual_effects = alderman_scores_shrunk 
  )
  
  return(results)
}


# =============================================================================
# 2. PLOTTING FUNCTION 
# =============================================================================

#' Create a suite of charts for alderman restrictiveness scores
#'
#' @param scores_list The list output from `calculate_alderman_scores()`, 
#'   containing `final_scores` and `individual_effects`.
#' @param spec_name A string to identify the model specification in plot titles 
#'   (e.g., "Month FEs" or "Ward + Month FEs").
#'
#' @return A named list of ggplot objects.

create_all_score_charts <- function(scores_list, spec_name) {
  
  spec_name_clean <- gsub(" \\+ ", "_", spec_name) %>% gsub(" ", "_", .)
  
  # =============================================================================
  # PLOT THE FINAL, AGGREGATED STRICTNESS INDEX
  # =============================================================================
  final_scores_df <- scores_list$final_scores
  
  plot_data_final <- final_scores_df %>%
    arrange(strictness_index) %>%
    mutate(
      alderman = factor(alderman, levels = alderman),
      restrictive_color = if_else(strictness_index > 0, "More Strict", "Less Strict")
    )
  
  p_final_index <- ggplot(plot_data_final, aes(x = alderman, y = strictness_index, fill = restrictive_color)) +
    geom_col() +
    scale_fill_manual(
      values = c("More Strict" = "#d73027", "Less Strict" = "#4575b4"),
      name = ""
    ) +
    labs(
      title = "Alderman Strictness Index",
      subtitle = paste("Based on PCA of all outcome variables with", spec_name),
      x = "Alderman",
      y = "Strictness Index"
    ) +
    # ⬇️ split labels onto two rows, slightly smaller angle; trim side padding
    scale_x_discrete(
      guide = guide_axis(angle = 55, n.dodge = 2),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 6, lineheight = 0.8, margin = margin(t = 0)),
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray60")
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)
  
  # =============================================================================
  # PLOT THE INDIVIDUAL FIXED EFFECTS
  # =============================================================================
  individual_effects_df <- scores_list$individual_effects
  outcome_names <- unique(individual_effects_df$outcome_variable)
  
  # Internal helper function for plotting each FE
  create_fe_chart <- function(outcome_var, data) {
    plot_data <- data %>%
      filter(outcome_variable == outcome_var) %>%
      arrange(alderman_fe) %>%
      mutate(alderman = factor(alderman, levels = alderman))
    
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
    
    clean_title <- str_to_title(gsub("_", " ", gsub("log_", "Log ", outcome_var)))
    
    p <- ggplot(plot_data, aes(x = alderman, y = alderman_fe, fill = restrictive_color)) +
      geom_col() +
      scale_fill_manual(
        values = c("More Restrictive" = "#d73027", "Less Restrictive" = "#4575b4", "Reference" = "#999999"),
        name = ""
      ) +
      labs(
        title = paste("Alderman Fixed Effect:", clean_title),
        subtitle = paste("Model includes", spec_name),
        x = "Alderman",
        y = "Shrunken Fixed Effect Coefficient"
      ) +
      # ⬇️ dodge the x labels onto two rows and rotate them
      scale_x_discrete(guide = guide_axis(angle = 45, n.dodge = 2)) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 6),  # no angle here so it doesn't override
        panel.grid = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray60")
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)
    
    return(p)
  }
  
  # Create a list of plots for each individual outcome
  individual_plots <- map(outcome_names, ~create_fe_chart(.x, individual_effects_df))
  names(individual_plots) <- outcome_names
  
  # =============================================================================
  # COMBINE AND RETURN ALL PLOTS
  # =============================================================================
  all_plots <- c(list(final_strictness_index = p_final_index), individual_plots)
  
  message(paste("\n✓ Generated", length(all_plots), "plots for the '", spec_name, "' specification."))
  
  # --- NEW: Use iwalk to loop through the named list of plots and save each one ---
  iwalk(all_plots, function(plot, name) {
    base <- file.path("../output", paste0(spec_name_clean, "_", name))
    # Vector export (best for slides)
    ggsave(paste0(base, ".pdf"), plot = plot,
           width = 14, height = 7.875, device = "pdf", bg = "white")
  })
  
  return(all_plots)
}

# =============================================================================
# 3. DEFINE INPUTS
# =============================================================================

# Use the residualized outcome columns for stage-2 FE estimation
outcome_vars <- list(
  "log_n_permits_issued"       = "resid_log_n_permits_issued",
  "permit_approval_rate"       = "resid_permit_approval_rate",
  "log_mean_processing_time"   = "resid_log_mean_processing_time",
  "log_mean_total_fee"         = "resid_log_mean_total_fee"
  # "log_sum_total_fee"          = "resid_log_sum_total_fee"
)


# After pre-residualization, don’t re-include controls in stage 2
# (set to "1" so the formula parser remains valid with your existing code)
control_vars <- "1"

ref_alderman <- "Andre Vasquez"


# =============================================================================
# 4. CALL THE FUNCTION FOR EACH SPECIFICATION
# =============================================================================

# --- Run model with Month FEs ---
scores_month_fe <- calculate_alderman_scores(
  data = data,
  outcome_vars = outcome_vars,
  control_vars = control_vars,      # now just "1"
  fe_spec = "month",
  ref_alderman = ref_alderman,
  permit_outcomes = c("log_n_permits_issued"),
  output_filepath = "../output/alderman_restrictiveness_scores_month_FEs.csv"
)

charts_month_fe <- create_all_score_charts(scores_month_fe, "Month FEs")


# --- Run model with Ward + Month FEs ---
scores_ward_month_fe <- calculate_alderman_scores(
  data = data,
  outcome_vars = outcome_vars,
  control_vars = control_vars,      # now just "1"
  fe_spec = "ward+month",
  ref_alderman = ref_alderman,
  permit_outcomes = c("log_n_permits_issued"),
  output_filepath = "../output/alderman_restrictiveness_scores_ward_month_FEs.csv"
)

charts_ward_month_fe <- create_all_score_charts(scores_ward_month_fe, "Ward + Month FEs")




