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
    log_sum_total_fee         = if_else(sum_total_fee > 0, log(sum_total_fee), NA_real_)
  )

coverage <- data %>%
  dplyr::group_by(alderman) %>%
  dplyr::summarise(
    n_months = dplyr::n_distinct(month),
    permits = sum(n_permits_applied, na.rm = TRUE),
    .groups = "drop"
  )

keep_aldermen <- coverage %>%
  dplyr::filter(n_months > 3) %>%
  dplyr::pull(alderman) ## need > 3 months of data

data <- data %>% dplyr::filter(alderman %in% keep_aldermen)

# ----------------------------
# B) Pre-residualize each outcome on ward fundamentals + month FEs
#     (NO alderman dummies here)
# ----------------------------
# Fundamentals:
geo_vars <- c(
  "dist_cbd_km", "lakefront_share_1km", "n_rail_stations_800m",
  grep("^ca_share_", names(data), value = TRUE) # community-area shares
)

# You already have these ward controls; include them as predetermined fundamentals
demo_vars <- c(
  "homeownership_rate", "pop_total", "median_hh_income",
  "share_black", "share_hisp"
)

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
data <- residualize_var(data, "log_n_permits_issued", fundamentals, weight_var = NULL)
data <- residualize_var(data, "log_sum_total_fee", fundamentals, weight_var = NULL)
data <- residualize_var(data, "permit_approval_rate", fundamentals, weight_var = "n_permits_applied")
data <- residualize_var(data, "log_mean_processing_time", fundamentals, weight_var = "n_permits_applied")
data <- residualize_var(data, "log_mean_total_fee", fundamentals, weight_var = "n_permits_applied")


# Build the RHS used in residualization
ca_cols <- grep("^ca_share_", names(data), value = TRUE)
geo_vars <- c("dist_cbd_km", "lakefront_share_1km", "n_rail_stations_800m")
demo_vars <- c(
  "homeownership_rate", "pop_total", "median_hh_income",
  "share_black", "share_hisp"
)
fundamentals <- c(geo_vars, demo_vars, ca_cols)

rhs <- if (length(fundamentals)) paste(fundamentals, collapse = " + ") else "1"

# Helper to fit the displayed models (same spec as residualization)
fit_disp <- function(y, w = NULL) {
  fml <- as.formula(paste0(y, " ~ ", rhs, " | month"))
  wgt <- if (is.null(w)) NULL else as.formula(paste0("~", w))
  feols(fml, data = data, weights = wgt)
}

m1 <- fit_disp("log_n_permits_issued") # unweighted
m2 <- fit_disp("log_sum_total_fee") # unweighted
m3 <- fit_disp("permit_approval_rate", "n_permits_applied")
m4 <- fit_disp("log_mean_processing_time", "n_permits_applied")
m5 <- fit_disp("log_mean_total_fee", "n_permits_applied")

# ----------------------------
# Stage-1 table for appendix: scale variables for interpretability
# ----------------------------
# Create scaled versions of population and income (in 10,000s)
data <- data %>%
  mutate(
    pop_10k = pop_total / 10000,
    income_10k = median_hh_income / 10000
  )

# Build formula with scaled variables for display table
ca_cols_disp <- grep("^ca_share_", names(data), value = TRUE)
geo_vars_disp <- c("dist_cbd_km", "lakefront_share_1km", "n_rail_stations_800m")
demo_vars_disp <- c("homeownership_rate", "pop_10k", "income_10k", "share_black", "share_hisp")
fundamentals_disp <- c(geo_vars_disp, demo_vars_disp, ca_cols_disp)
rhs_disp <- paste(fundamentals_disp, collapse = " + ")

# Fit model with scaled variables for display (same regression, just rescaled coefficients)
fml_disp <- as.formula(paste0("log_mean_processing_time ~ ", rhs_disp, " | month + ca_id"))

# We need to create a ca_id variable from the ca_shares (pick the dominant one)
data <- data %>%
  mutate(ca_id = {
    ca_mat <- select(., starts_with("ca_share_"))
    max_col <- max.col(ca_mat, ties.method = "first")
    as.factor(gsub("ca_share_", "", names(ca_mat)[max_col]))
  })

# Fit with month + community area FEs for display
m4_disp <- feols(
  log_mean_processing_time ~ dist_cbd_km + lakefront_share_1km + n_rail_stations_800m +
    homeownership_rate + pop_10k + income_10k + share_black + share_hisp | month + ca_id,
  data = data,
  weights = ~n_permits_applied
)

# Stage-1 table: only log_mean_processing_time (the outcome used for strictness scores)
etable(
  m4_disp,
  digits = 3, se.below = TRUE,
  depvar = FALSE,
  headers = c("Log Processing Time"),
  dict = c(
    dist_cbd_km = "Dist. to CBD (km)",
    lakefront_share_1km = "Lakefront share ($\\leq$1km)",
    n_rail_stations_800m = "CTA stations ($\\leq$800m)",
    homeownership_rate = "Homeownership rate",
    pop_10k = "Population (10,000s)",
    income_10k = "Median HH income (\\$10,000s)",
    share_black = "Share Black",
    share_hisp = "Share Hispanic"
  ),
  fixef.group = list(
    "Month FE" = "month",
    "Community Area FE" = "ca_id"
  ),
  fitstat = ~ n + r2,
  file = "../output/stage1_residualization_models.tex",
  replace = TRUE,
  style.tex = style.tex(
    main = "aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  )
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

    model <- feols(as.formula(formula_str), data = data, vcov = ~ ward + month, weights = weight_formula)

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

  # Check dimensions of W
  if (ncol(W) == 1) {
    message("   ...Only 1 outcome variable present. Skipping PCA and using the single variable as the index.")
    pc1 <- scale(W[, 1], center = TRUE, scale = TRUE)[, 1]
  } else {
    message("   ...Multiple outcome variables present. Running PCA.")
    # plain PCA; PC1 sign is arbitrary by definition
    pca_fit <- prcomp(W, center = TRUE, scale. = TRUE)
    pc1 <- pca_fit$x[, 1]

    # --------------------------------------------------------
    # FIXED LOGIC: Enforce direction of the index
    # We want High Score = STRICT
    # Permits should decrease as Strictness increases.
    # So Correlation(Score, Permits) should be NEGATIVE.
    # If it is positive, we must flip the sign of the score.
    # --------------------------------------------------------

    # Identify the permits column in W if it exists
    permits_col <- grep("permits", colnames(W), value = TRUE)

    if (length(permits_col) > 0) {
      # Use the first match found (e.g. resid_log_n_permits_issued)
      check_col <- permits_col[1]
      cor_check <- cor(pc1, W[, check_col], use = "complete.obs")

      message(paste0("   ...Correlation with ", check_col, ": ", round(cor_check, 3)))

      if (cor_check > 0) {
        message("   ...Correlation is POSITIVE (More Strict = More Permits). Flipping sign to correct.")
        pc1 <- -pc1
      } else {
        message("   ...Correlation is NEGATIVE (More Strict = Fewer Permits). Sign is correct.")
      }
    } else {
      message("   ...WARNING: No 'permits' variable found in W to check sign. Assuming PC1 is correct.")
    }
  } # <--- THIS CLOSING BRACE WAS MISSING

  # =============================================================================
  # C2. CONSTRUCT FINAL DATAFRAME (This block was missing)
  # =============================================================================
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
      alderman = factor(alderman, levels = alderman)
    )

  p_final_index <- ggplot(plot_data_final, aes(x = strictness_index, y = alderman, fill = strictness_index)) +
    geom_col() +
    scale_fill_distiller(
      palette = "RdYlBu",
      direction = -1,
      name = "Strictness Index"
    ) +
    labs(
      title = "Alderman Strictness Index",
      x = NULL,
      y = "Alderman"
    ) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 4),
      axis.text.x = element_text(size = 8),
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold")
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7)

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
        axis.text.x = element_text(size = 6), # no angle here so it doesn't override
        panel.grid = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray60")
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)

    return(p)
  }

  # Create a list of plots for each individual outcome
  individual_plots <- map(outcome_names, ~ create_fe_chart(.x, individual_effects_df))
  names(individual_plots) <- outcome_names

  # =============================================================================
  # COMBINE AND RETURN ALL PLOTS
  # =============================================================================
  all_plots <- c(list(final_strictness_index = p_final_index), individual_plots)

  message(paste("\n✓ Generated", length(all_plots), "plots for the '", spec_name, "' specification."))

  # --- NEW: Use iwalk to loop through the named list of plots and save each one ---
  iwalk(all_plots, function(plot, name) {
    base <- file.path("../output", paste0(spec_name_clean, "_", name))
    # Use taller dimensions for the final strictness index (horizontal bar chart)
    if (name == "final_strictness_index") {
      ggsave(paste0(base, ".pdf"),
        plot = plot,
        width = 8, height = 12, device = "pdf", bg = "white"
      )
    } else {
      # Vector export (best for slides)
      ggsave(paste0(base, ".pdf"),
        plot = plot,
        width = 14, height = 7.875, device = "pdf", bg = "white"
      )
    }
  })

  return(all_plots)
}

# =============================================================================
# 3. DEFINE INPUTS
# =============================================================================

# Use the residualized outcome columns for stage-2 FE estimation
outcome_vars <- list(
  # "log_n_permits_issued"       = "resid_log_n_permits_issued",
  # "permit_approval_rate"       = "resid_permit_approval_rate",
  "log_mean_processing_time" = "resid_log_mean_processing_time"
  # "log_mean_total_fee"         = "resid_log_mean_total_fee"
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
  control_vars = control_vars, # now just "1"
  fe_spec = "month",
  ref_alderman = ref_alderman,
  permit_outcomes = c("log_mean_processing_time"),
  output_filepath = "../output/alderman_restrictiveness_scores_month_FEs.csv"
)

charts_month_fe <- create_all_score_charts(scores_month_fe, "Month FEs")


# --- Run model with Ward + Month FEs ---
scores_ward_month_fe <- calculate_alderman_scores(
  data = data,
  outcome_vars = outcome_vars,
  control_vars = control_vars, # now just "1"
  fe_spec = "ward+month",
  ref_alderman = ref_alderman,
  permit_outcomes = c("log_mean_processing_time"),
  output_filepath = "../output/alderman_restrictiveness_scores_ward_month_FEs.csv"
)

charts_ward_month_fe <- create_all_score_charts(scores_ward_month_fe, "Ward + Month FEs")
