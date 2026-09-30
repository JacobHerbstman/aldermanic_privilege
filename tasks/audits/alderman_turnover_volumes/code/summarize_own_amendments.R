# setwd("tasks/audits/alderman_turnover_volumes/code")
# Aldermen's own zoning map amendments (tasks/audits/zoning_stalls_and_delays), one row per alderman and period: the
# Journals for introductions of 2000--2010 and eLMS for 2011 to the end of its download. For each, the years the
# alderman held a ward within the period (create_alderman_data/adjudication/alderman_terms.csv), the wards held, the
# number of own amendments in all and by kind of change (direction: down, up, same floor-area ratio, to a planned
# development, unknown), those per year in office, and the share passed. An amendment introduced in a vacancy has no
# alderman and is left out.
period_from <- as.Date(c(journals = "2000-01-01", elms = "2011-01-01"))
period_to <- as.Date(c(journals = "2010-12-31", elms = "2026-09-23"))

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

periods <- tibble(source = names(period_from), period = c("2000-2010 Journals", "2011-2026 eLMS"),
  from = period_from, to = period_to[names(period_from)])

own <- read_csv("../input/zoning_amendments.csv", show_col_types = FALSE) |>
  filter(filer == "alderman", !is.na(alderman)) |>
  inner_join(periods, by = "source", relationship = "many-to-one") |>
  filter(introduction_date >= from, introduction_date <= to)
terms <- read_csv("../input/alderman_terms.csv", show_col_types = FALSE)
service <- bind_rows(lapply(seq_len(nrow(periods)), function(i) {
  terms |>
    mutate(period = periods$period[i], days = as.numeric(pmin(end_date, periods$to[i]) -
      pmax(start_date, periods$from[i])) + 1) |>
    filter(days > 0)
})) |>
  summarise(years_in_office = sum(days) / 365.25, wards = paste(sort(unique(ward)), collapse = ";"),
    .by = c(period, alderman))
stopifnot(all(paste(own$period, own$alderman) %in% paste(service$period, service$alderman)))

counts <- own |>
  summarise(own_amendments = n(), downzonings = sum(direction == "down"), upzonings = sum(direction == "up"),
    same_far = sum(direction == "same_far"), to_planned_development = sum(direction == "to_planned_development"),
    unknown_kind = sum(direction == "unknown"), passed_share = mean(outcome == "passed"), .by = c(period, alderman))
own_by_alderman <- service |>
  left_join(counts, by = c("period", "alderman"), relationship = "one-to-one") |>
  mutate(across(c(own_amendments, downzonings, upzonings, same_far, to_planned_development, unknown_kind),
    ~ coalesce(.x, 0L)),
    own_amendments_per_year = own_amendments / years_in_office, downzonings_per_year = downzonings / years_in_office) |>
  arrange(period, desc(downzonings_per_year))
SaveData(own_by_alderman, c("period", "alderman"), "../output/own_amendments_by_alderman.csv")
