# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")
# bandwidth <- 304.8
# bandwidth_label <- "1000ft"

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

data <- read_parquet("../output/corrected_permit_block_year_panel.parquet") %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(assigned_change_2022)
  ) %>%
  mutate(
    stricter_dose = pmax(assigned_change_2022, 0),
    lenient_dose = pmax(-assigned_change_2022, 0),
    post_stricter = as.integer(relative_year >= 0L) * stricter_dose,
    post_lenient = as.integer(relative_year >= 0L) * lenient_dose
  )

estimate_lenient_effect <- function(model_data) {
  model <- fepois(
    n_high_discretion_issue ~ post_stricter + post_lenient |
      block_id + ward_pair_id^year,
    data = model_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  c(
    estimate_log = coef(model)[["post_lenient"]],
    std_error_log = se(model)[["post_lenient"]]
  )
}

baseline <- estimate_lenient_effect(data)
lenient_blocks <- data %>%
  filter(relative_year == -1L, assigned_change_2022 < 0) %>%
  select(
    block_id, ward_pair_id, alderman_origin_2014, alderman_dest_2014,
    assigned_change_2022
  )
total_lenient_blocks <- n_distinct(lenient_blocks$block_id)

alder_groups <- bind_rows(
  lenient_blocks %>% transmute(
    role = "destination",
    alderman = alderman_dest_2014,
    block_id,
    ward_pair_id,
    assigned_change_2022
  ),
  lenient_blocks %>% transmute(
    role = "origin",
    alderman = alderman_origin_2014,
    block_id,
    ward_pair_id,
    assigned_change_2022
  )
) %>%
  filter(!is.na(alderman), alderman != "")

group_summary <- alder_groups %>%
  group_by(role, alderman) %>%
  summarise(
    lenient_blocks = n_distinct(block_id),
    ward_pairs = n_distinct(ward_pair_id),
    mean_assigned_change = mean(assigned_change_2022),
    .groups = "drop"
  )

influence_results <- vector("list", nrow(group_summary))
for (i in seq_len(nrow(group_summary))) {
  role_i <- group_summary$role[i]
  alderman_i <- group_summary$alderman[i]
  blocks_to_drop <- alder_groups %>%
    filter(role == role_i, alderman == alderman_i) %>%
    pull(block_id) %>%
    unique()
  leave_one_out <- estimate_lenient_effect(data %>% filter(!block_id %in% blocks_to_drop))
  influence_results[[i]] <- tibble(
    role = role_i,
    alderman = alderman_i,
    leave_one_out_estimate_log = leave_one_out[["estimate_log"]],
    leave_one_out_std_error_log = leave_one_out[["std_error_log"]]
  )
}

write_csv(
  group_summary %>%
    left_join(
      bind_rows(influence_results),
      by = c("role", "alderman"),
      relationship = "one-to-one"
    ) %>%
    mutate(
      total_lenient_blocks = total_lenient_blocks,
      share_of_lenient_blocks = lenient_blocks / total_lenient_blocks,
      baseline_estimate_log = baseline[["estimate_log"]],
      baseline_std_error_log = baseline[["std_error_log"]],
      baseline_effect = expm1(baseline_estimate_log),
      leave_one_out_effect = expm1(leave_one_out_estimate_log),
      upward_contribution = baseline_estimate_log - leave_one_out_estimate_log,
      bandwidth_m = bandwidth,
      bandwidth_label
    ) %>%
    arrange(role, desc(upward_contribution)),
  sprintf("../output/corrected_permit_lenient_alder_influence_%s.csv", bandwidth_label)
)
