# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)

points <- read_csv("../input/preferred_predecessor_reference_points.csv",
  col_types = cols(target_year = col_integer(), reference_x_3435 = col_double(),
    reference_y_3435 = col_double(), .default = col_skip())) |>
  filter(is.finite(reference_x_3435), is.finite(reference_y_3435)) |>
  distinct()
queried <- read_csv("../output/preferred_predecessor_source_queries.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(queried), !anyNA(queried))
additional <- anti_join(points, queried, by = names(points)) |>
  arrange(target_year, reference_x_3435, reference_y_3435)
write_csv(additional, "../output/preferred_predecessor_spatial_queries_download.csv")
