## this code generates a simple alderman restrictiveness score based on pop. density, homeonwer rates, and incomes at the ward level

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

unbalanced_panel <- read_csv("../input/permit_regression_panel_blocks_unbalanced.csv")

balanced_panel <- read_csv("../input/permit_regression_panel_blocks_balanced.csv")

prepare_panel <- function(df) {
  df %>%
    mutate(
      # Convert NAs for permit counts to 0
      # n_permits = ifelse(is.na(n_permits), 0, n_permits),
      
      # Create an indicator for the treated group (blocks that ever switch)
      treat = as.numeric(max(ward_switch, na.rm = TRUE) == 1),
      
      # Create a relative time variable for the event study (-4, -3, ..., 0, 1, ..., 4)
      # The treatment year is 2015
      relative_year = year - 2015,
      
      ## create an indicator for being post-treatment (to be interacted with treated_unit)
      post = ifelse(relative_year >= 0, 1, 0),
      
      score_pre = first(restrictiveness_score_pca[year == 2014]),
      score_post = first(restrictiveness_score_pca[year == 2015]),
      
      # Create a category for the type of switch
      switch_type = case_when(
        treat == 0 ~ "Control",
        score_post > score_pre ~ "Moved to Stricter",
        score_post < score_pre ~ "Moved to Less Strict",
        TRUE ~ "No Change in Score" # Default case for treated blocks with no score change
      )
    )
}

unbalanced_panel <- unbalanced_panel %>% group_by(block_id) %>% prepare_panel() %>% ungroup()
balanced_panel <- balanced_panel %>% group_by(block_id) %>% prepare_panel() %>% ungroup()


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
    "log(n_permits + 1)", # Use +1 to handle zeroes
    "log(avg_processing_time)",
    "log(avg_reported_cost)",
    "log(avg_total_fee)"
  ),
  weights = c(NA, "n_permits", "n_permits", "n_permits")
)

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
  
  fml_string <- sprintf("%s ~ restrictiveness_score_pca + %s | block_id + year",
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

# Unbalanced panel results
etable(
  all_models %>% filter(panel_name == "unbalanced") %>% pull(model),
  headers = all_models %>% filter(panel_name == "unbalanced") %>% pull(outcome),
  title = "DiD Estimates (Unbalanced Panel)",
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
)

# Balanced panel results
etable(
  all_models %>% filter(panel_name == "balanced") %>% pull(model),
  headers = all_models %>% filter(panel_name == "balanced") %>% pull(outcome),
  title = "DiD Estimates (Balanced Panel)",
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
)




### event studies 

## unbalanced, processing time

event_study_stricter_unbalanced_process_time <- feols(
  log(avg_processing_time) ~ i(relative_year, treat, ref = -1)  | block_id + year,
  data = unbalanced_panel %>% filter(switch_type %in% c("Control", "Moved to Stricter")),
  weights = ~n_permits
)

event_study_less_strict_unbalanced_process_time <- feols(
  log(avg_processing_time) ~ i(relative_year, treat, ref = -1) | block_id + year,
  data = unbalanced_panel %>% filter(switch_type %in% c("Control", "Moved to Less Strict")),
  weights = ~n_permits
)

iplot(
  list(event_study_stricter_unbalanced_process_time, event_study_less_strict_unbalanced_process_time),
  main =  "Effect on Permit Processing Time",
  sub = "Unbalanced Panel",
  xlab = "Years Relative to Ward Switch",
  ylab = "Log(Avg. Processing Time)"
)
legend("bottomleft", col = 1:2, pch = 20, legend = c("Moved to Stricter", "Moved to Less Strict"))


## unbalanced, reported costs

event_study_stricter_unbalanced_reported_cost <- feols(
  log(avg_reported_cost) ~ i(relative_year, treat, ref = -1)| block_id + year,
  data = unbalanced_panel %>% filter(switch_type %in% c("Control", "Moved to Stricter")),
  weights = ~n_permits
)

event_study_less_strict_unbalanced_reported_cost <- feols(
  log(avg_reported_cost) ~ i(relative_year, treat, ref = -1) | block_id + year,
  data = unbalanced_panel %>% filter(switch_type %in% c("Control", "Moved to Less Strict")),
  weights = ~n_permits
)

iplot(
  list(event_study_stricter_unbalanced_reported_cost, event_study_less_strict_unbalanced_reported_cost),
  main =  "Effect on Reported Costs",
  sub = "Unbalanced Panel",
  xlab = "Years Relative to Ward Switch",
  ylab = "Log(Avg. Reported Cost)"
)
legend("bottomleft", col = 1:2, pch = 20, legend = c("Moved to Stricter", "Moved to Less Strict"))


## unbalanced, total fees


event_study_stricter_unbalanced_total_fee <- feols(
  log(avg_total_fee) ~ i(relative_year, treat, ref = -1) | block_id + year,
  data = unbalanced_panel %>% filter(switch_type %in% c("Control", "Moved to Stricter")),
  weights = ~n_permits
)

event_study_less_strict_unbalanced_total_fee <- feols(
  log(avg_total_fee) ~ i(relative_year, treat, ref = -1) | block_id + year,
  data = unbalanced_panel %>% filter(switch_type %in% c("Control", "Moved to Less Strict")),
  weights = ~n_permits
)

iplot(
  list(event_study_stricter_unbalanced_total_fee, event_study_less_strict_unbalanced_total_fee),
  main =  "Effect on Total Fees",
  sub = "Unbalanced Panel",
  xlab = "Years Relative to Ward Switch",
  ylab = "Log(Avg. Total Fee)"
)
legend("bottomleft", col = 1:2, pch = 20, legend = c("Moved to Stricter", "Moved to Less Strict"))



###########################
#### balanced panel #######
###########################



## balanced, processing time

event_study_stricter_balanced_process_time <- feols(
  log(avg_processing_time) ~ i(relative_year, treat, ref = -1) | block_id + year,
  data = balanced_panel %>% filter(switch_type %in% c("Control", "Moved to Stricter")),
  weights = ~n_permits
)

event_study_less_strict_balanced_process_time <- feols(
  log(avg_processing_time) ~ i(relative_year, treat, ref = -1) | block_id + year,
  data = balanced_panel %>% filter(switch_type %in% c("Control", "Moved to Less Strict")),
  weights = ~n_permits
)

iplot(
  list(event_study_stricter_balanced_process_time, event_study_less_strict_balanced_process_time),
  main =  "Effect on Permit Processing Time",
  sub = "balanced Panel",
  xlab = "Years Relative to Ward Switch",
  ylab = "Log(Avg. Processing Time)"
)
legend("bottomleft", col = 1:2, pch = 20, legend = c("Moved to Stricter", "Moved to Less Strict"))


## balanced, reported costs

event_study_stricter_balanced_reported_cost <- feols(
  log(avg_reported_cost) ~ i(relative_year, treat, ref = -1) | block_id + year,
  data = balanced_panel %>% filter(switch_type %in% c("Control", "Moved to Stricter")),
  weights = ~n_permits
)

event_study_less_strict_balanced_reported_cost <- feols(
  log(avg_reported_cost) ~ i(relative_year, treat, ref = -1) | block_id + year,
  data = balanced_panel %>% filter(switch_type %in% c("Control", "Moved to Less Strict")),
  weights = ~n_permits
)

iplot(
  list(event_study_stricter_balanced_reported_cost, event_study_less_strict_balanced_reported_cost),
  main =  "Effect on Reported Costs",
  sub = "balanced Panel",
  xlab = "Years Relative to Ward Switch",
  ylab = "Log(Avg. Reported Cost)"
)
legend("bottomleft", col = 1:2, pch = 20, legend = c("Moved to Stricter", "Moved to Less Strict"))


## balanced, total fees


event_study_stricter_balanced_total_fee <- feols(
  log(avg_total_fee) ~ i(relative_year, treat, ref = -1) | block_id + year,
  data = balanced_panel %>% filter(switch_type %in% c("Control", "Moved to Stricter")),
  weights = ~n_permits
)

event_study_less_strict_balanced_total_fee <- feols(
  log(avg_total_fee) ~ i(relative_year, treat, ref = -1) | block_id + year,
  data = balanced_panel %>% filter(switch_type %in% c("Control", "Moved to Less Strict")),
  weights = ~n_permits
)

iplot(
  list(event_study_stricter_balanced_total_fee, event_study_less_strict_balanced_reported_cost),
  main =  "Effect on Total Fees",
  sub = "balanced Panel",
  xlab = "Years Relative to Ward Switch",
  ylab = "Log(Avg. Total Fee)"
)
legend("bottomleft", col = 1:2, pch = 20, legend = c("Moved to Stricter", "Moved to Less Strict"))




