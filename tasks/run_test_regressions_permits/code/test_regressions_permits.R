## this code generates a simple alderman restrictiveness score based on pop. density, homeonwer rates, and incomes at the ward level
## currently using basic restrictiveness score not PCA

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

unbalanced_panel <- read_csv("../input/permit_regression_panel_blocks_unbalanced.csv")

balanced_panel <- read_csv("../input/permit_regression_panel_blocks_balanced.csv")


policy_year <- 2015

prepare_panel <- function(df) {
  df %>%
    mutate(
      # Look ahead to see if this block ever switches in the future
      treat = as.numeric(any(ward_switch == 1, na.rm = TRUE)),
      
      # Create a relative time variable using policy announcement year (2012)
      relative_year = year - policy_year,
      
      ## create an indicator for being post-announcement
      post = ifelse(relative_year >= 0, 1, 0),
      
      # Use the actual implementation year (2015) for score comparison
      score_pre = first(strictness_index[year == 2014]),
      score_post = first(strictness_index[year == 2015]),
      
      # Create a category for the type of switch
      switch_type = case_when(
        treat == 0 ~ "Control",
        score_post > score_pre ~ "Moved to Stricter",
        score_post < score_pre ~ "Moved to Less Strict",
        TRUE ~ "No Change in Score"
      )
    )
}

unbalanced_panel <- unbalanced_panel %>% 
  group_by(block_id) %>%
  prepare_panel() %>%
  ungroup()

  
balanced_panel <- balanced_panel %>% 
  group_by(block_id) %>% 
  prepare_panel() %>% 
  ungroup()


# -----------------------------------------------------------------------------
### Setup Regression Specifications
# -----------------------------------------------------------------------------

# List of panel datasets to use
panel_list <- list(
  unbalanced = unbalanced_panel,
  balanced = balanced_panel
)

# Define all model specifications in a single, clear data frame
controls <- c(
  "homeownership_rate", "population_density", "median_income",
  "percent_black", "percent_hispanic", "avg_household_size"
)

model_specs <- tibble(
  outcome = c(
    "log(n_permits)", # Use +1 to handle zeroes
    "log(avg_processing_time)",
    "log(avg_reported_cost)",
    "log(avg_total_fee)",
    "log(avg_building_fee_paid)",
    "log(avg_zoning_fee_paid)",
    "log(avg_building_fee_subtotal)", 
    "log(avg_zoning_fee_subtotal)",
    "prop_had_zoning_fee_waiver",
    "prop_had_other_fee_waiver",
    "prop_had_any_fee_waiver",
    "prop_corporate_applicant"
  ))

## weights are all n_permits except for n_permits itself
model_specs <- model_specs %>% 
  mutate(weights = c(NA, rep("n_permits", nrow(model_specs) - 1)))



# -----------------------------------------------------------------------------
### Run All Regressions
# -----------------------------------------------------------------------------

# Create a grid of every combination of panel and model spec
regression_grid <- crossing(
  panel_name = names(panel_list),
  model_specs
)

# Define a function to run a single regression
run_feols <- function(panel_name, outcome, weights) {
  
  fml_string <- sprintf("%s ~ strictness_index + %s | block_id + year",
                        outcome, paste(controls, collapse = " + "))
  
  feols(
    fml = as.formula(fml_string),
    data = panel_list[[panel_name]],
    # CORRECTED LINE: Convert the string name into a formula
    weights = if (!is.na(weights)) as.formula(paste0("~", weights)) else NULL,
    vcov = ~block_id
  )
}

# Use pmap to iterate over the grid and run all regressions,
# storing the results in a new list-column called 'model'.
all_models <- regression_grid %>%
  mutate(model = pmap(list(panel_name, outcome, weights), run_feols))


# -----------------------------------------------------------------------------
### Display Results
# -----------------------------------------------------------------------------
# --- Define clean headers to replace the long dependent variable names ---
clean_headers <- c(
  "Log(Permit Count)", 
  "Log(Processing Time)", 
  "Log(Reported Cost)", 
  "Log(Total Fees)",
  "Log(Building Fee Paid)",
  "Log(Zoning Fee Paid)",
  "Log(Building Fee Subtotal)",
  "Log(Zoning Fee Subtotal)",
  "Prop. w/ Zoning Fee Waiver",
  "Prop. w/ Other Fee Waiver",
  "Prop. w/ Any Fee Waiver",
  "Prop. Corporate Applicant"
)


rename_dict <- c(
  "strictness_index" = "Restrictiveness Score",
  "block_id"                  = "Census Block", 
  "year"                      = "Year"
)

unbalanced_models <- all_models %>% 
  filter(panel_name == "unbalanced") %>%
  # Ensure the order matches model_specs by joining
  right_join(model_specs %>% mutate(row_id = row_number()), by = "outcome") %>%
  arrange(row_id) %>%
  pull(model) 

etable(
  unbalanced_models,
  # Formatting options
  headers     = clean_headers,
  keep        = "Restrictiveness Score",
  style.tex = style.tex("aer"),
  fitstat     = ~n,
  depvar      = FALSE,
  digits = 2,
  dict        = rename_dict,
  # General options
  title       = "DiD Estimates (Unbalanced Panel)",
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  # file      = "../output/table_did_unbalanced.tex", 
  replace = T
)

balanced_models <- all_models %>% 
  filter(panel_name == "balanced") %>%
  # Ensure the order matches model_specs by joining
  right_join(model_specs %>% mutate(row_id = row_number()), by = "outcome") %>%
  arrange(row_id) %>%
  pull(model)


# Balanced panel results
etable(
  balanced_models,
  # Formatting options
  headers     = clean_headers,
  keep        = "Restrictiveness Score",
  style.tex = style.tex("aer"),
  fitstat     = ~n,
  depvar      = FALSE,
  digits = 2,
  dict        = rename_dict,
  # General options
  title       = "DiD Estimates (Balanced Panel)",
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  # file      = "../output/table_did_balanced.tex",
  replace = T
)


# -----------------------------------------------------------------------------
### event studies #########
# -----------------------------------------------------------------------------

event_study_specs <- model_specs 

# Now, create every combination of panel and outcome.
event_study_grid <- crossing(
  panel_name = names(panel_list),
  event_study_specs
)


# --- Function to run a pair of event study models ---
run_event_study_pair <- function(panel_name, outcome, weights) {
  
  fml_event <- as.formula(
    sprintf("%s ~ i(relative_year, treat, ref = -1) + %s | block_id + year",
            outcome, paste(controls, collapse = " + "))
  )
  
  panel_data <- panel_list[[panel_name]]
  w_formula <-if (!is.na(weights)) as.formula(paste0("~", weights)) else NULL
  
  # Model 1: Moved to Stricter
  model_stricter <- feols(
    fml = fml_event,
    data = panel_data %>% filter(switch_type %in% c("Control", "Moved to Stricter")),
    weights = w_formula, vcov = ~block_id
  )
  
  # Model 2: Moved to Less Strict
  model_less_strict <- feols(
    fml = fml_event,
    data = panel_data %>% filter(switch_type %in% c("Control", "Moved to Less Strict")),
    weights = w_formula, vcov = ~block_id
  )
  
  # Return both models as a named list
  list(stricter = model_stricter, less_strict = model_less_strict)
}


# --- Run all models and store them in the grid ---
all_event_studies <- event_study_grid %>%
  mutate(model_pair = pmap(
    list(panel_name, outcome, weights),
    run_event_study_pair
  ))



# --- Final, corrected plotting function ---
plot_event_study_ggplot <- function(model_pair, outcome, panel_name, ...) {
  
  # 1. Safely extract data using the iplot() list output
  # iplot() returns a list, so we grab the first element [[1]]
  
  stricter_data <- tryCatch({
    iplot(model_pair$stricter, .plot = FALSE)[[1]] %>% 
      mutate(group = "Moved to Stricter Alderman")
  }, error = function(e) NULL) # Return NULL if it fails
  
  less_strict_data <- tryCatch({
    iplot(model_pair$less_strict, .plot = FALSE)[[1]] %>% 
      mutate(group = "Moved to Less Strict Alderman")
  }, error = function(e) NULL) # Return NULL if it fails
  
  # 2. Combine any data that was successfully extracted
  plot_data <- bind_rows(stricter_data, less_strict_data)
  
  if (nrow(plot_data) > 0) {
    plot_data <- plot_data %>% 
      filter(x != -5) %>%
      # THE FIX: Multiply the correct columns by 100
      mutate(across(c(estimate, ci_low, ci_high), ~ .x * 100))
  }
  
  # 3. Check if we have any data to plot
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    message(sprintf("--> SKIPPING PLOT for '%s' on '%s' panel (no coefficients found).", 
                    outcome, panel_name))
    return(invisible(NULL))
  }
  
  # 4. Build the ggplot
  outcome_to_title <- setNames(
    paste("Effect on", clean_headers),
    model_specs$outcome
  )
  
  main_title <- outcome_to_title[outcome]
  
  p <- ggplot(plot_data, aes(x = factor(x), y = estimate, color = group)) + # Using factor(x) fixes the axis
    geom_vline(xintercept = "-1", linetype = "dashed", color = "gray60") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, alpha = 0.7) +
    geom_point(size = 2.5) +
    facet_wrap(~ group) +
    scale_color_manual(values = c("Moved to Stricter Alderman" = "#D55E00", "Moved to Less Strict Alderman" = "#0072B2")) +
    labs(
      title = main_title,
      subtitle = paste(str_to_title(panel_name), "Panel"),
      x = "Years Relative to Ward Switch",
      y = "Percent Change"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold")
    )
  
  return(p)
}


# generate all plots
for (i in 1:nrow(all_event_studies)) {
  
  params <- all_event_studies[i, ]
  
  message(sprintf("--- Processing spec %d/%d: %s (%s) ---", 
                  i, nrow(all_event_studies), 
                  params$outcome, 
                  params$panel_name))
  
  tryCatch({
    # 1. Generate the ggplot object by calling the function
    my_plot <- plot_event_study_ggplot(
      model_pair = params$model_pair[[1]],
      outcome    = params$outcome,
      panel_name = params$panel_name
    )
    
    # 2. Check if the plot object is valid before proceeding
    if (!is.null(my_plot)) {
      # 3. Display the plot in your R session
      print(my_plot)
      
      # 4. Create a clean filename (e.g., "plots/event_study_unbalanced_avg_total_fee.png")
      # Simple approach: remove log() and parentheses, replace spaces with underscores
      outcome_shortname <- params$outcome %>%
        str_remove("log\\(") %>%
        str_remove("\\)") %>%
        str_replace_all("[^a-zA-Z0-9_]", "_") %>%
        str_remove("^_+|_+$") # remove leading/trailing underscores
      filename <- sprintf("../output/event_study_%s_%s.pdf", 
                          params$panel_name, 
                          outcome_shortname)
      
      # 5. Save the plot to the file
      cowplot::save_plot(filename, my_plot, base_height = 6, base_width = 10, bg = "white")
      
      message(sprintf("      -> Plot saved to %s", filename))
    }
    
  }, error = function(e) {
    message(sprintf("!!! An error occurred for plot %d. Skipping. !!!", i))
    message("The error was:")
    print(e$message)
  })
}





