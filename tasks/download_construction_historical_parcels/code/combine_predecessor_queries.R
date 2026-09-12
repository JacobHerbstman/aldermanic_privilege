source("../../shared/code/save_data.R")
# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)

original <- read_csv("../input/historical_predecessor_queries_2026-07-27.csv", show_col_types = FALSE)
additional <- read_csv("../input/historical_predecessor_queries_2026-09-07.csv", show_col_types = FALSE)
queries <- bind_rows(original, additional) |> arrange(target_year, reference_x_3435, reference_y_3435)
stopifnot(!anyNA(queries), !anyDuplicated(queries), all(queries$target_year %in% 2006:2022))
SaveData(queries, c("target_year", "reference_x_3435", "reference_y_3435"), "../output/historical_predecessor_source_queries.csv")
