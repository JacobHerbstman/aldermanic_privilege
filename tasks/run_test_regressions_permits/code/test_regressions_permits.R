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

controls <- c("homeownership_rate", "population_density", "median_income", "percent_black", "percent_hispanic", "avg_household_size") 

# -----------------------------------------------------------------------------
# 3. STANDARD DIFFERENCE-IN-DIFFERENCES ANALYSIS
# -----------------------------------------------------------------------------
# We regress the outcome on 'ward_switch', which is 1 for treated blocks in
# the post-period and 0 otherwise. We include block and year fixed effects.

# Run the regression on the unbalanced panel
did_unbalanced_permits <- feols(
  log(n_permits + 1) ~ restrictiveness_score_pca  | block_id + year,
  data = unbalanced_panel
)

did_unbalanced_process_time <- feols(
  log(avg_processing_time) ~ restrictiveness_score_pca  | block_id + year,
  weights = ~n_permits, 
  data = unbalanced_panel
)

did_unbalanced_reported_cost <- feols(
  log(avg_reported_cost) ~ restrictiveness_score_pca  | block_id + year,
  weights = ~n_permits, 
  data = unbalanced_panel
)

did_unbalanced_total_fee <- feols(
  log(avg_total_fee) ~ restrictiveness_score_pca  | block_id + year,
  weights = ~n_permits, 
  data = unbalanced_panel
)

etable(did_unbalanced_permits, did_unbalanced_process_time, 
       did_unbalanced_reported_cost, did_unbalanced_total_fee,
       headers = list("Permits" = 1, "Processing Times" = 1, "Reported Costs" =1, "Total Fees" = 1),
       title = "DiD Estimate of Ward Switch on Permit Counts", 
       vcov = ~block_id, 
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.1))



# Run the same regression on the balanced panel as a robustness check
did_balanced_permits <- feols(
  log(n_permits + 1) ~  restrictiveness_score_pca | block_id + year,
  data = balanced_panel
)

did_balanced_process_time <- feols(
  log(avg_processing_time) ~ restrictiveness_score_pca | block_id + year,
  weights = ~n_permits, 
  data = balanced_panel
)

did_balanced_reported_cost <- feols(
  log(avg_reported_cost) ~ restrictiveness_score_pca | block_id + year,
  weights = ~n_permits, 
  data = balanced_panel
)

did_balanced_total_fee <- feols(
  log(avg_total_fee) ~ restrictiveness_score_pca  | block_id + year,
  weights = ~n_permits, 
  data = balanced_panel
)

etable(did_balanced_permits,did_balanced_process_time,
       did_balanced_reported_cost, did_balanced_total_fee,
       headers = list("Permits" = 1, "Processing Times" = 1, "Reported Costs" =1, "Total Fees" = 1),
       title = "DiD Estimate of Ward Switch on Permit Counts", 
       vcov = ~block_id, 
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.1))



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




