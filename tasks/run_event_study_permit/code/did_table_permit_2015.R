# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# bandwidth <- 152.4
# bandwidth_label <- "500ft"
# min_period <- -5
# max_period <- 5

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth, bandwidth_label, min_period, max_period)
}
if (length(cli_args) != 4) {
  stop("Script requires bandwidth, bandwidth label, minimum period, and maximum period.", call. = FALSE)
}

bandwidth <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]
min_period <- as.integer(cli_args[3])
max_period <- as.integer(cli_args[4])

if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}
if (!is.finite(min_period) || !is.finite(max_period) || min_period >= max_period) {
  stop("min_period and max_period must define an increasing event window.", call. = FALSE)
}

data <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= min_period,
    relative_year <= max_period,
    stable_both,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) %>%
  mutate(
    strictness_change = strictness_change_frozen,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

if (anyDuplicated(data[c("block_id", "year")]) > 0) {
  stop("Permit DID data must be unique by block and year.", call. = FALSE)
}

outcomes <- c("n_high_discretion_application", "n_low_discretion_nosigns_application")
models <- list()
estimates <- numeric(length(outcomes))
std_errors <- numeric(length(outcomes))
p_values <- numeric(length(outcomes))
n_blocks <- integer(length(outcomes))
n_ward_pairs <- integer(length(outcomes))

for (i in seq_along(outcomes)) {
  outcome_var <- outcomes[i]
  model_data <- data %>% mutate(outcome = .data[[outcome_var]])
  pre_period_controls <- model_data %>%
    filter(relative_year < 0) %>%
    group_by(block_id) %>%
    summarise(
      pre_period_permit_volume = sum(outcome, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

  if (anyDuplicated(pre_period_controls$block_id) > 0) {
    stop("Pre-period permit controls must be unique by block.", call. = FALSE)
  }

  model_data <- model_data %>%
    left_join(pre_period_controls, by = "block_id", relationship = "many-to-one")

  models[[i]] <- fepois(
    outcome ~ post_treat +
      pre_period_permit_volume:factor(year) +
      no_pre_period_permits:factor(year) |
      block_id + ward_pair_id^year,
    data = model_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  estimates[i] <- coef(models[[i]])[["post_treat"]]
  std_errors[i] <- se(models[[i]])[["post_treat"]]
  p_values[i] <- 2 * pnorm(-abs(estimates[i] / std_errors[i]))
  model_rows <- obs(models[[i]])
  n_blocks[i] <- n_distinct(model_data$block_id[model_rows])
  n_ward_pairs[i] <- n_distinct(model_data$ward_pair_id[model_rows])
}

stars <- case_when(
  p_values < 0.01 ~ "***",
  p_values < 0.05 ~ "**",
  p_values < 0.10 ~ "*",
  TRUE ~ ""
)

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\small",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & (1) & (2) \\\\",
  " & High-discretion & Low-discretion \\\\",
  "\\midrule",
  sprintf("Post $\\times$ Stringency $\\Delta$ & %.4f%s & %.4f%s \\\\", estimates[1], stars[1], estimates[2], stars[2]),
  sprintf(" & (%.4f) & (%.4f) \\\\", std_errors[1], std_errors[2]),
  "\\midrule",
  "Block fixed effects & Yes & Yes \\\\",
  "Ward-pair $\\times$ year fixed effects & Yes & Yes \\\\",
  "Pre-period permit controls $\\times$ year & Yes & Yes \\\\",
  sprintf("Observations & %s & %s \\\\", format(nobs(models[[1]]), big.mark = ","), format(nobs(models[[2]]), big.mark = ",")),
  sprintf("Census blocks & %s & %s \\\\", format(n_blocks[1], big.mark = ","), format(n_blocks[2], big.mark = ",")),
  sprintf("Ward pairs & %s & %s \\\\", format(n_ward_pairs[1], big.mark = ","), format(n_ward_pairs[2], big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(
  table_lines,
  sprintf(
    "../output/did_table_permit_2015_application_frozen2014_stable_preperiod_controls_%s_clust_ward_pair.tex",
    bandwidth_label
  )
)
