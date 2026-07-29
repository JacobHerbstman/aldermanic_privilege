# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")
# bandwidth <- 152.4
# bandwidth_label <- "500ft"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth, bandwidth_label)
}
if (length(cli_args) != 2) {
  stop("Script requires bandwidth and bandwidth label.", call. = FALSE)
}

bandwidth <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]
if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("Bandwidth must be positive.", call. = FALSE)
}

panel <- read_parquet("../output/corrected_permit_block_year_panel.parquet") %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(assigned_change_2014),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  )
if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("Permit panel must be unique by block and year.", call. = FALSE)
}

data <- bind_rows(
  panel %>% transmute(
    block_id, year, relative_year, ward_pair_id,
    strictness_change = assigned_change_2014,
    permit_group = "high",
    outcome = n_high_discretion_application
  ),
  panel %>% transmute(
    block_id, year, relative_year, ward_pair_id,
    strictness_change = assigned_change_2014,
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
    transition_2015 = as.integer(relative_year == 0L) * strictness_change,
    post_2016_2020 = as.integer(relative_year >= 1L) * strictness_change,
    pre_volume_high = pre_period_permit_volume * high_discretion,
    pre_volume_low = pre_period_permit_volume * (1L - high_discretion),
    no_pre_permits_high = no_pre_period_permits * high_discretion,
    no_pre_permits_low = no_pre_period_permits * (1L - high_discretion),
    block_permit_group = paste(block_id, permit_group, sep = "_"),
    pair_year_group = paste(ward_pair_id, year, permit_group, sep = "_")
  )

model <- fepois(
  outcome ~ transition_2015 + post_2016_2020 +
    transition_2015:high_discretion + post_2016_2020:high_discretion +
    pre_volume_high:factor(year) + no_pre_permits_high:factor(year) +
    pre_volume_low:factor(year) + no_pre_permits_low:factor(year) |
    block_permit_group + pair_year_group,
  data = data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

model_coefficients <- coef(model)
model_vcov <- vcov(model)
base_terms <- c("transition_2015", "post_2016_2020")
interaction_terms <- paste0(base_terms, ":high_discretion")
if (!all(c(base_terms, interaction_terms) %in% names(model_coefficients))) {
  stop("Joint model did not return the expected treatment coefficients.", call. = FALSE)
}

periods <- c("2015", "2016-2020")
contrast_names <- paste(
  rep(periods, each = 3L),
  rep(c("High discretion", "Low discretion", "High minus low"), 2L)
)
contrasts <- matrix(
  0,
  nrow = 6,
  ncol = length(model_coefficients),
  dimnames = list(contrast_names, names(model_coefficients))
)
for (period_i in seq_along(periods)) {
  row_i <- (period_i - 1L) * 3L
  contrasts[row_i + 1L, c(base_terms[period_i], interaction_terms[period_i])] <- 1
  contrasts[row_i + 2L, base_terms[period_i]] <- 1
  contrasts[row_i + 3L, interaction_terms[period_i]] <- 1
}

estimates <- as.numeric(contrasts %*% model_coefficients)
standard_errors <- sqrt(diag(contrasts %*% model_vcov %*% t(contrasts)))
model_rows <- obs(model)

write_csv(
  tibble(
    period = rep(periods, each = 3L),
    contrast = rep(c("High discretion", "Low discretion", "High minus low"), 2L),
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
  "../output/corrected_permit_itt_discretion_transition_500ft.csv"
)
