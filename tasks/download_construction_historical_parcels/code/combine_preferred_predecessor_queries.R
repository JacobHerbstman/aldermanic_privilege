# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)

original <- read_csv("../input/preferred_predecessor_source_queries.csv", show_col_types = FALSE)
additional <- read_csv("../input/preferred_predecessor_queries_2026-09-07.csv", show_col_types = FALSE)
address_correction <- read_csv("../input/preferred_predecessor_troy_queries_2026-09-07.csv", show_col_types = FALSE)
lake_park <- read_csv("../input/lake_park_predecessor_queries_2026-09-08.csv", show_col_types = FALSE)
campbell <- read_csv("../input/campbell_predecessor_queries_2026-09-08.csv", show_col_types = FALSE)
queries <- bind_rows(original, additional, address_correction, lake_park, campbell) |>
  arrange(target_year, reference_x_3435, reference_y_3435)
stopifnot(!anyNA(queries), !anyDuplicated(queries))
write_csv(queries, "../output/preferred_predecessor_source_queries.csv")
