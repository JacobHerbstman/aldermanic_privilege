# setwd("tasks/download_construction_historical_parcels/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

# Initial parcel requests
original <- read_csv("../input/historical_project_parcel_queries_2026-07-27.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
additional <- read_csv("../input/historical_project_parcel_queries_2026-09-07.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
latest <- read_csv("../input/historical_project_parcel_queries_2026-09-08.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
queries <- bind_rows(original, additional, latest) |> arrange(target_year, pin10)
stopifnot(!anyNA(queries), !anyDuplicated(queries), all(grepl("^[0-9]{10}$", queries$pin10)))
initial_queries <- queries

# Initial parcel polygons
original <- st_read("../input/historical_project_parcel_source_2026-07-27.gpkg", quiet = TRUE)
additional <- st_read("../input/historical_project_parcel_source_2026-09-07.gpkg", quiet = TRUE)
latest <- st_read("../input/historical_project_parcel_source_2026-09-08.gpkg", quiet = TRUE)
queries <- initial_queries
stopifnot(st_crs(original)$epsg == 3435, st_crs(additional)$epsg == 3435,
          nrow(inner_join(st_drop_geometry(original) |> distinct(target_year, pin10),
                         st_drop_geometry(additional) |> distinct(target_year, pin10),
                         by = c("target_year", "pin10"), relationship = "one-to-one")) == 0L)
parcels <- bind_rows(original, additional, latest) |> arrange(target_year, pin10, pin14, object_id)
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          nrow(distinct(st_drop_geometry(parcels), target_year, pin10)) ==
            nrow(distinct(st_drop_geometry(original), target_year, pin10)) +
            nrow(distinct(st_drop_geometry(additional), target_year, pin10)) +
            nrow(distinct(st_drop_geometry(latest), target_year, pin10)),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)),
          nrow(anti_join(st_drop_geometry(parcels), queries, by = c("target_year", "pin10"))) == 0L)
SaveData(parcels, c("target_year", "object_id"), "../output/historical_project_parcel_source.gpkg", layer = "historical_project_parcels", delete_dsn = TRUE, quiet = TRUE)
initial_parcels <- parcels

# Predecessor requests
original <- read_csv("../input/historical_predecessor_queries_2026-07-27.csv", show_col_types = FALSE)
additional <- read_csv("../input/historical_predecessor_queries_2026-09-07.csv", show_col_types = FALSE)
queries <- bind_rows(original, additional) |> arrange(target_year, reference_x_3435, reference_y_3435)
stopifnot(!anyNA(queries), !anyDuplicated(queries), all(queries$target_year %in% 2006:2022))

# Predecessor polygons
original <- st_read("../input/historical_predecessor_parcels_2026-07-27.gpkg", quiet = TRUE)
additional <- st_read("../input/historical_predecessor_parcels_2026-09-07.gpkg", quiet = TRUE)
stopifnot(st_crs(original)$epsg == 3435, st_crs(additional)$epsg == 3435)
parcels <- bind_rows(original, additional) |>
  arrange(target_year, predecessor_pin10, predecessor_pin14, object_id)
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
SaveData(parcels, c("target_year", "object_id"), "../output/historical_predecessor_parcel_source.gpkg", layer = "historical_project_predecessor_parcels", delete_dsn = TRUE, quiet = TRUE)

# Additional parcel requests
original <- read_csv("../input/preferred_historical_parcel_source_queries.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
initial <- initial_queries
additional <- read_csv("../input/preferred_parcel_supplement_queries_2026-09-07.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
latest <- read_csv("../input/preferred_parcel_supplement_queries_2026-09-08.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
evidence <- read_csv("../input/preferred_parcel_evidence_queries_2026-09-08.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
lake_park <- read_csv("../input/lake_park_direct_queries_2026-09-08.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
east_64th <- read_csv("../input/east_64th_queries_2026-09-08.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
geneva_maud <- read_csv("../input/geneva_maud_queries_2026-09-08.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
campbell <- read_csv("../input/campbell_queries_2026-09-08.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
reviewed <- read_csv("reviewed_parcel_queries.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
stopifnot(!anyDuplicated(original), !anyDuplicated(initial), !anyDuplicated(additional))
queries <- bind_rows(original, anti_join(initial, original, by = c("target_year", "pin10")), additional, latest, evidence, lake_park, east_64th, geneva_maud, campbell, reviewed) |>
  arrange(target_year, pin10)
stopifnot(!anyNA(queries), !anyDuplicated(queries))
preferred_queries <- queries

# Combine the recorded parcel coverage
original <- st_read("../input/preferred_historical_parcel_source.gpkg", quiet = TRUE)
latest <- st_read("../input/preferred_parcel_supplement_2026-09-08.gpkg", quiet = TRUE)
evidence <- st_read("../input/preferred_parcel_evidence_2026-09-08.gpkg", quiet = TRUE)
lake_park <- st_read("../input/lake_park_direct_parcels_2026-09-08.gpkg", quiet = TRUE)
east_64th <- st_read("../input/east_64th_parcels_2026-09-08.gpkg", quiet = TRUE)
geneva_maud <- st_read("../input/geneva_maud_parcels_2026-09-08.gpkg", quiet = TRUE)
campbell <- st_read("../input/campbell_parcels_2026-09-08.gpkg", quiet = TRUE)
reviewed_queries <- read_csv("reviewed_parcel_queries.csv", show_col_types = FALSE)
reviewed <- lapply(sort(unique(reviewed_queries$target_year)), function(year)
  st_read(paste0("../sources/reviewed_parcels_additional_", year, ".gpkg"), quiet = TRUE)) |> bind_rows()
original_queries <- read_csv("../input/preferred_historical_parcel_source_queries.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
initial <- initial_parcels |>
  anti_join(original_queries, by = c("target_year", "pin10"))
additional <- st_read("../input/preferred_parcel_supplement_2026-09-07.gpkg", quiet = TRUE)
queries <- preferred_queries
stopifnot(st_crs(original)$epsg == 3435, st_crs(initial)$epsg == 3435, st_crs(additional)$epsg == 3435)
parcels <- bind_rows(original, initial, additional, latest, evidence, lake_park, east_64th, geneva_maud, campbell, reviewed) |> arrange(target_year, pin10, pin14, object_id)
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)),
          nrow(anti_join(st_drop_geometry(parcels), queries, by = c("target_year", "pin10"))) == 0L)
SaveData(parcels, c("target_year", "object_id"), "../output/preferred_historical_parcel_source.gpkg", layer = "historical_parcels", delete_dsn = TRUE, quiet = TRUE)

# Additional predecessor requests
original <- read_csv("../input/preferred_predecessor_source_queries.csv", show_col_types = FALSE)
additional <- read_csv("../input/preferred_predecessor_queries_2026-09-07.csv", show_col_types = FALSE)
address_correction <- read_csv("../input/preferred_predecessor_troy_queries_2026-09-07.csv", show_col_types = FALSE)
lake_park <- read_csv("../input/lake_park_predecessor_queries_2026-09-08.csv", show_col_types = FALSE)
campbell <- read_csv("../input/campbell_predecessor_queries_2026-09-08.csv", show_col_types = FALSE)
reviewed <- read_csv("reviewed_predecessor_queries.csv", show_col_types = FALSE)
queries <- bind_rows(original, additional, address_correction, lake_park, campbell, reviewed) |>
  arrange(target_year, reference_x_3435, reference_y_3435)
stopifnot(!anyNA(queries), !anyDuplicated(queries))

# Combine predecessor coverage
original <- st_read("../input/preferred_predecessor_parcel_source.gpkg", quiet = TRUE)
additional <- st_read("../input/preferred_predecessor_parcels_2026-09-07.gpkg", quiet = TRUE) |>
  select(-geometry_valid)
address_correction <- st_read("../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg", quiet = TRUE) |>
  select(-geometry_valid)
lake_park <- st_read("../input/lake_park_predecessor_parcels_2026-09-08.gpkg", quiet = TRUE) |> select(-geometry_valid)
campbell <- st_read("../input/campbell_predecessor_parcels_2026-09-08.gpkg", quiet = TRUE) |> select(-geometry_valid)
stopifnot(st_crs(original)$epsg == 3435, st_crs(additional)$epsg == 3435,
  st_crs(address_correction)$epsg == 3435)
parcels <- bind_rows(original, additional, address_correction, lake_park, campbell) |>
  arrange(target_year, predecessor_pin10, predecessor_pin14, object_id)
reviewed <- st_read("../sources/reviewed_predecessor_parcels.gpkg", quiet = TRUE) |> select(-geometry_valid)
for (i in seq_len(nrow(reviewed))) {
  same <- which(parcels$target_year == reviewed$target_year[i] & parcels$object_id == reviewed$object_id[i])
  if (length(same)) stopifnot(length(same) == 1L,
    parcels$predecessor_pin14[same] == reviewed$predecessor_pin14[i],
    lengths(st_equals(reviewed[i, ], parcels[same, ])) == 1L)
}
reviewed <- reviewed |> anti_join(st_drop_geometry(parcels) |> select(target_year, object_id),
  by = c("target_year", "object_id"))
parcels <- bind_rows(parcels, reviewed) |> arrange(target_year, predecessor_pin10, predecessor_pin14, object_id)
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
SaveData(parcels, c("target_year", "object_id"), "../output/preferred_predecessor_parcel_source.gpkg", layer = "historical_project_predecessor_parcels", delete_dsn = TRUE, quiet = TRUE)

# Exact-parcel coordinates
history <- bind_rows(
  read_csv("../input/geocoding_parcel_history_2026-09-07.csv", col_types = cols(.default = col_character())),
  read_csv("../sources/reviewed_parcel_history.csv", col_types = cols(.default = col_character()))) |>
  arrange(pin, year, row_id)
stopifnot(!anyNA(history$row_id), !anyDuplicated(history$row_id), !anyDuplicated(history[c("pin", "year")]))
SaveData(history, c("pin", "year"), "../output/geocoding_parcel_history.csv")
