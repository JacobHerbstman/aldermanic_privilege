# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")
# bandwidth <- 152.4
# bandwidth_label <- "500ft"

source("../../../setup_environment/code/packages.R")

grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)

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

data <- read_parquet("../output/corrected_permit_block_year_panel.parquet") %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(assigned_change_2022),
    !is.na(assigned_change_2014)
  ) %>%
  mutate(
    post_2022 = as.integer(relative_year >= 0L) * assigned_change_2022,
    post_2014 = as.integer(relative_year >= 0L) * assigned_change_2014
  )

estimate_score_effects <- function(model_data) {
  model_2022 <- fepois(
    n_high_discretion_issue ~ post_2022 | block_id + ward_pair_id^year,
    data = model_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  model_2014 <- fepois(
    n_high_discretion_issue ~ post_2014 | block_id + ward_pair_id^year,
    data = model_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  c(
    estimate_2022_log = coef(model_2022)[["post_2022"]],
    estimate_2014_log = coef(model_2014)[["post_2014"]]
  )
}

baseline <- estimate_score_effects(data)
baseline_recovery <- baseline[["estimate_2014_log"]] - baseline[["estimate_2022_log"]]

block_mapping <- data %>%
  filter(relative_year == -1L) %>%
  distinct(
    block_id, ward_pair_id, switched,
    alderman_origin_2014, alderman_dest_2014,
    assigned_change_2022, assigned_change_2014
  )

alder_blocks <- bind_rows(
  block_mapping %>%
    filter(switched) %>%
    transmute(alderman = alderman_origin_2014, role = "origin", block_id),
  block_mapping %>%
    filter(switched) %>%
    transmute(alderman = alderman_dest_2014, role = "destination", block_id)
) %>%
  filter(!is.na(alderman), alderman != "")

alder_summary <- alder_blocks %>%
  group_by(alderman) %>%
  summarise(
    switched_blocks = n_distinct(block_id),
    roles = paste(sort(unique(role)), collapse = ","),
    .groups = "drop"
  )

alder_results <- vector("list", nrow(alder_summary))
for (i in seq_len(nrow(alder_summary))) {
  blocks_to_drop <- alder_blocks %>%
    filter(alderman == alder_summary$alderman[i]) %>%
    pull(block_id) %>%
    unique()
  leave_one_out <- estimate_score_effects(data %>% filter(!block_id %in% blocks_to_drop))
  leave_one_out_recovery <-
    leave_one_out[["estimate_2014_log"]] - leave_one_out[["estimate_2022_log"]]
  alder_results[[i]] <- tibble(
    alderman = alder_summary$alderman[i],
    leave_one_out_2022_log = leave_one_out[["estimate_2022_log"]],
    leave_one_out_2014_log = leave_one_out[["estimate_2014_log"]],
    recovery_contribution = leave_one_out_recovery - baseline_recovery
  )
}

write_csv(
  alder_summary %>%
    left_join(bind_rows(alder_results), by = "alderman", relationship = "one-to-one") %>%
    mutate(
      baseline_2022_log = baseline[["estimate_2022_log"]],
      baseline_2014_log = baseline[["estimate_2014_log"]],
      baseline_recovery_log = baseline_recovery,
      bandwidth_m = bandwidth,
      bandwidth_label
    ) %>%
    arrange(desc(recovery_contribution)),
  sprintf("../output/frozen_score_alder_recovery_influence_%s.csv", bandwidth_label)
)

pair_summary <- block_mapping %>%
  group_by(ward_pair_id) %>%
  summarise(
    blocks = n_distinct(block_id),
    switched_blocks = n_distinct(block_id[switched]),
    sign_switched_blocks = n_distinct(
      block_id[switched & sign(assigned_change_2022) != sign(assigned_change_2014)]
    ),
    alder_transitions = paste(
      sort(unique(paste(alderman_origin_2014[switched], "to", alderman_dest_2014[switched]))),
      collapse = "; "
    ),
    .groups = "drop"
  )

pair_results <- vector("list", nrow(pair_summary))
for (i in seq_len(nrow(pair_summary))) {
  leave_one_out <- estimate_score_effects(
    data %>% filter(ward_pair_id != pair_summary$ward_pair_id[i])
  )
  leave_one_out_recovery <-
    leave_one_out[["estimate_2014_log"]] - leave_one_out[["estimate_2022_log"]]
  pair_results[[i]] <- tibble(
    ward_pair_id = pair_summary$ward_pair_id[i],
    leave_one_out_2022_log = leave_one_out[["estimate_2022_log"]],
    leave_one_out_2014_log = leave_one_out[["estimate_2014_log"]],
    recovery_contribution = leave_one_out_recovery - baseline_recovery
  )
}

write_csv(
  pair_summary %>%
    left_join(bind_rows(pair_results), by = "ward_pair_id", relationship = "one-to-one") %>%
    mutate(
      baseline_2022_log = baseline[["estimate_2022_log"]],
      baseline_2014_log = baseline[["estimate_2014_log"]],
      baseline_recovery_log = baseline_recovery,
      bandwidth_m = bandwidth,
      bandwidth_label
    ) %>%
    arrange(desc(recovery_contribution)),
  sprintf("../output/frozen_score_pair_recovery_influence_%s.csv", bandwidth_label)
)
