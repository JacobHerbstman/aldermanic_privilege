# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/permit_itt_score_permutation/code")
# bandwidth_m <- 152.4
# bandwidth_label <- "500ft"
# permutations <- 9999
# seed <- 42
# cores <- 8

source("../../../setup_environment/code/packages.R")
library(fixest)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_m, bandwidth_label, permutations, seed, cores)
}
if (length(cli_args) != 5) {
  stop("Script requires bandwidth, bandwidth label, permutation count, seed, and core count.", call. = FALSE)
}

bandwidth_m <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]
permutations <- as.integer(cli_args[3])
seed <- as.integer(cli_args[4])
cores <- as.integer(cli_args[5])

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}
if (!is.finite(permutations) || permutations < 99) {
  stop("at least 99 permutations are required.", call. = FALSE)
}
if (!is.finite(seed)) {
  stop("seed must be an integer.", call. = FALSE)
}
if (!is.finite(cores) || cores < 1) {
  stop("core count must be a positive integer.", call. = FALSE)
}
cores <- min(cores, parallel::detectCores())
setFixest_nthreads(1)

panel <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  mutate(block_id = as.character(block_id)) %>%
  filter(
    cohort == "2015",
    dist_m <= bandwidth_m,
    relative_year >= -5,
    relative_year <= 5,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) %>%
  mutate(outcome = n_high_discretion_application)

if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("Permit panel must be unique by block and year.", call. = FALSE)
}

blocks <- panel %>%
  filter(relative_year == -1) %>%
  distinct(block_id, .keep_all = TRUE) %>%
  transmute(
    block_id,
    alderman_origin_2014,
    alderman_dest_2014,
    strictness_origin_frozen,
    strictness_dest_frozen,
    strictness_change_frozen
  )

if (anyDuplicated(blocks$block_id) > 0) {
  stop("Score-permutation block sample must be unique by block.", call. = FALSE)
}

score_map <- bind_rows(
  blocks %>%
    transmute(
      alderman = alderman_origin_2014,
      score = strictness_origin_frozen
    ),
  blocks %>%
    transmute(
      alderman = alderman_dest_2014,
      score = strictness_dest_frozen
    )
) %>%
  distinct()

if (anyNA(score_map) || anyDuplicated(score_map$alderman) > 0) {
  stop("Frozen scores must define one score for each alderman in the sample.", call. = FALSE)
}

origin_score_position <- match(blocks$alderman_origin_2014, score_map$alderman)
destination_score_position <- match(blocks$alderman_dest_2014, score_map$alderman)

if (anyNA(origin_score_position) || anyNA(destination_score_position)) {
  stop("Every block must match origin and destination aldermen to frozen scores.", call. = FALSE)
}

reconstructed_change <- score_map$score[destination_score_position] -
  score_map$score[origin_score_position]
if (max(abs(reconstructed_change - blocks$strictness_change_frozen)) > 1e-10) {
  stop("Frozen score mapping does not reconstruct the production treatment.", call. = FALSE)
}

pre_period_controls <- panel %>%
  filter(relative_year < 0) %>%
  group_by(block_id) %>%
  summarise(
    pre_period_permit_volume = sum(outcome),
    .groups = "drop"
  ) %>%
  mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

model_data <- panel %>%
  left_join(
    pre_period_controls,
    by = "block_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    block_position = match(block_id, blocks$block_id),
    post = as.integer(relative_year >= 0),
    post_treat = post * blocks$strictness_change_frozen[block_position]
  )

if (anyNA(model_data$block_position)) {
  stop("Every panel row must match the block-level score mapping.", call. = FALSE)
}

observed_model <- fepois(
  outcome ~ post_treat +
    pre_period_permit_volume:factor(year) +
    no_pre_period_permits:factor(year) |
    block_id + ward_pair_id^year,
  data = model_data,
  cluster = ~ward_pair_id,
  notes = FALSE,
  warn = FALSE
)

observed_estimate <- coef(observed_model)[["post_treat"]]
observed_standard_error <- se(observed_model)[["post_treat"]]
observed_p_value <- pvalue(observed_model)[["post_treat"]]

set.seed(seed)
permuted_score_draws <- replicate(permutations, sample(score_map$score))

permuted_estimates <- unlist(parallel::mclapply(
  seq_len(permutations),
  function(permutation_i) {
    permuted_scores <- permuted_score_draws[, permutation_i]
    permuted_change <- permuted_scores[destination_score_position] -
      permuted_scores[origin_score_position]
    permutation_data <- model_data
    permutation_data$post_treat <- permutation_data$post *
      permuted_change[permutation_data$block_position]

    fepois(
      outcome ~ post_treat +
        pre_period_permit_volume:factor(year) +
        no_pre_period_permits:factor(year) |
        block_id + ward_pair_id^year,
      data = permutation_data,
      notes = FALSE,
      warn = FALSE,
      only.coef = TRUE
    )[["post_treat"]]
  },
  mc.cores = cores,
  mc.preschedule = TRUE
))

two_sided_permutation_p <- (
  1 + sum(abs(permuted_estimates) >= abs(observed_estimate))
) / (permutations + 1)
lower_tail_permutation_p <- (
  1 + sum(permuted_estimates <= observed_estimate)
) / (permutations + 1)

message(sprintf(
  paste0(
    "%d aldermen and %d blocks: observed %.3f (SE %.3f); ",
    "two-sided permutation p %.3f; lower-tail p %.3f."
  ),
  nrow(score_map),
  nrow(blocks),
  observed_estimate,
  observed_standard_error,
  two_sided_permutation_p,
  lower_tail_permutation_p
))

plot_data <- tibble(estimate = permuted_estimates)

plot <- ggplot(plot_data, aes(estimate)) +
  geom_histogram(bins = 40, fill = "#AEB8C2", color = "white", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "gray45", linetype = "dashed", linewidth = 0.5) +
  geom_vline(xintercept = observed_estimate, color = "#A33A2B", linewidth = 1.0) +
  labs(
    title = "Permutation of frozen stringency scores across aldermen",
    subtitle = sprintf(
      paste0(
        "Observed = %.3f (SE %.3f); two-sided permutation p = %.3f; ",
        "%s; %s permutations"
      ),
      observed_estimate,
      observed_standard_error,
      two_sided_permutation_p,
      bandwidth_label,
      format(permutations, big.mark = ",")
    ),
    x = "Pooled post-period coefficient on permuted assigned stringency",
    y = "Permutation draws",
    caption = paste0(
      "Ward transitions, permit outcomes, and the score distribution are held fixed.\n",
      "Each draw shuffles the frozen 2006--2014 scores across the 49 aldermen represented in the analysis sample."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

ggsave(
  sprintf(
    "../output/permit_itt_score_permutation_%s_B%d.pdf",
    bandwidth_label,
    permutations
  ),
  plot,
  width = 8.2,
  height = 5.6,
  bg = "white"
)
