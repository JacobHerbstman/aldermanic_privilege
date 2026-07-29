# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")
# treatment_specification <- "itt"
# bandwidth <- 152.4
# bandwidth_label <- "500ft"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(treatment_specification, bandwidth, bandwidth_label)
}
if (length(cli_args) != 3) {
  stop("Script requires treatment specification, bandwidth, and bandwidth label.", call. = FALSE)
}

treatment_specification <- cli_args[1]
bandwidth <- as.numeric(cli_args[2])
bandwidth_label <- cli_args[3]
if (!treatment_specification %in% c("itt", "stable", "realized")) {
  stop("Treatment specification must be itt, stable, or realized.", call. = FALSE)
}
if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("Bandwidth must be positive.", call. = FALSE)
}

dose_variable <- if (treatment_specification == "realized") {
  "realized_change_2014"
} else {
  "assigned_change_2014"
}

panel <- read_parquet("../output/corrected_permit_block_year_panel.parquet") %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(.data[[dose_variable]]),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  )
if (treatment_specification == "stable") {
  panel <- panel %>% filter(stable_both)
}
if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("Permit panel must be unique by block and year.", call. = FALSE)
}

data <- bind_rows(
  panel %>% transmute(
    block_id, year, relative_year, ward_pair_id,
    strictness_change = .data[[dose_variable]],
    permit_group = "high",
    outcome = n_high_discretion_application
  ),
  panel %>% transmute(
    block_id, year, relative_year, ward_pair_id,
    strictness_change = .data[[dose_variable]],
    permit_group = "low",
    outcome = n_low_discretion_nosigns_application
  )
)

pre_period_controls <- data %>%
  filter(relative_year < 0L) %>%
  group_by(block_id, permit_group) %>%
  summarise(
    pre_period_permit_volume = sum(outcome, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))
if (anyDuplicated(pre_period_controls[c("block_id", "permit_group")]) > 0) {
  stop("Pre-period controls must be unique by block and permit group.", call. = FALSE)
}

data <- data %>%
  left_join(
    pre_period_controls,
    by = c("block_id", "permit_group"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    high_discretion = as.integer(permit_group == "high"),
    post_treat = as.integer(relative_year >= 0L) * strictness_change,
    pre_volume_high = pre_period_permit_volume * high_discretion,
    pre_volume_low = pre_period_permit_volume * (1L - high_discretion),
    no_pre_permits_high = no_pre_period_permits * high_discretion,
    no_pre_permits_low = no_pre_period_permits * (1L - high_discretion),
    block_permit_group = paste(block_id, permit_group, sep = "_"),
    pair_year_group = paste(ward_pair_id, year, permit_group, sep = "_")
  )

model <- fepois(
  outcome ~ post_treat + post_treat:high_discretion +
    pre_volume_high:factor(year) + no_pre_permits_high:factor(year) +
    pre_volume_low:factor(year) + no_pre_permits_low:factor(year) |
    block_permit_group + pair_year_group,
  data = data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

model_coefficients <- coef(model)
model_vcov <- vcov(model)
difference_name <- "post_treat:high_discretion"
if (!all(c("post_treat", difference_name) %in% names(model_coefficients))) {
  stop("Joint model did not return the expected treatment coefficients.", call. = FALSE)
}

contrasts <- matrix(
  0,
  nrow = 3,
  ncol = length(model_coefficients),
  dimnames = list(
    c("High discretion", "Low discretion", "High minus low"),
    names(model_coefficients)
  )
)
contrasts["High discretion", c("post_treat", difference_name)] <- 1
contrasts["Low discretion", "post_treat"] <- 1
contrasts["High minus low", difference_name] <- 1

estimates <- as.numeric(contrasts %*% model_coefficients)
standard_errors <- sqrt(diag(contrasts %*% model_vcov %*% t(contrasts)))
model_rows <- obs(model)

write_csv(
  tibble(
    contrast = rownames(contrasts),
    treatment_specification,
    estimate_log = estimates,
    std_error_log = standard_errors,
    p_value = 2 * pnorm(-abs(estimates / standard_errors)),
    rate_change = expm1(estimates),
    observations = nobs(model),
    blocks = n_distinct(data$block_id[model_rows]),
    ward_pairs = n_distinct(data$ward_pair_id[model_rows]),
    bandwidth_m = bandwidth,
    bandwidth_label
  ),
  sprintf(
    "../output/corrected_permit_%s_discretion_difference_%s.csv",
    treatment_specification,
    bandwidth_label
  )
)
