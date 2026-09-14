# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_alderman_data/code")
# panel_end_month <- "2022-12"

source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) {
  cli_args <- c(panel_end_month)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <panel_end_month>.", call. = FALSE)
}

panel_end_month <- cli_args[1]
panel_end_date <- as.Date(paste0(panel_end_month, "-01"))
if (is.na(panel_end_date)) {
  stop("panel_end_month must be YYYY-MM.", call. = FALSE)
}
current_panel_end_date <- seq(panel_end_date, by = "month", length.out = 2)[2] - 1

alderman_data <- read_csv(
  "../input/alderman_terms.csv",
  col_types = cols(ward = col_double(), alderman = col_character(),
                   start_date = col_date(), end_date = col_date())
) %>%
  filter(start_date <= current_panel_end_date) %>%
  mutate(end_date = pmin(end_date, current_panel_end_date))

term_overlaps <- alderman_data %>%
  arrange(ward, start_date) %>%
  group_by(ward) %>%
  mutate(next_start_date = lead(start_date)) %>%
  ungroup() %>%
  filter(!is.na(next_start_date), next_start_date <= end_date)
if (nrow(term_overlaps) > 0) {
  stop("Alderman terms overlap within a ward.", call. = FALSE)
}

SaveData(alderman_data, c("ward", "start_date"), "../output/chicago_alderman_terms.csv")

panel_months <- as.yearmon(seq(as.Date("1998-01-01"), panel_end_date, by = "months"))

panel_grid <- expand_grid(
  month = panel_months,
  ward = 1:50
)

alderman_months <- alderman_data %>%
  mutate(
    term_order = row_number(),
    start_month = as.yearmon(start_date) + if_else(
      lubridate::days_in_month(start_date) - lubridate::day(start_date) + 1L >
        lubridate::days_in_month(start_date) / 2,
      0,
      1 / 12
    ),
    end_month = as.yearmon(end_date) - if_else(
      lubridate::day(end_date) > lubridate::days_in_month(end_date) / 2,
      0,
      1 / 12
    )
  ) %>%
  filter(start_month <= end_month) %>%
  rowwise() %>%
  # Preserve the panel's existing transition-month convention.
  mutate(month = list(panel_months[
    round(panel_months, 4) >= round(start_month, 4) &
      round(panel_months, 4) <= round(end_month, 4)
  ])) %>%
  ungroup() %>%
  select(ward, alderman, term_order, month) %>%
  tidyr::unnest(month) %>%
  arrange(ward, month, term_order) %>%
  distinct(ward, month, .keep_all = TRUE) %>%
  select(ward, month, alderman)

final_panel <- panel_grid %>%
  left_join(alderman_months, by = c("ward", "month"), relationship = "one-to-one")

coverage <- final_panel %>%
  summarise(
    n_wards = n_distinct(ward),
    n_missing_alderman = sum(is.na(alderman) | alderman == ""),
    n_unique_aldermen = n_distinct(alderman, na.rm = TRUE),
    .by = month
  ) %>%
  arrange(month)

if (coverage$n_missing_alderman[coverage$month == max(coverage$month)] > 0) {
  stop("Alderman panel has missing assignments in its final month.", call. = FALSE)
}

SaveData(final_panel, c("ward", "month"), "../output/chicago_alderman_panel.csv")
