# setwd("tasks/audits/zoning_record_validation/code")
# Whether the districts after that the eLMS reader finds (tasks/clean_zoning_map_amendments) are those on the City's
# current zoning map, which gives for each district the ordinance record that set it (clerk_docn, O2017-7341; a
# substitute's record SO2017-7341 is taken as the same number). One row per passed amendment whose records appear on
# the map: its districts and direction, the map's district classes set by those records, and whether they agree (the
# map has a district after or, for a change to a planned development, a planned development). A district may have
# been rezoned again since, so agreement is a lower bound.

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

map_records <- st_read("../input/zoning_districts_current.geojson", quiet = TRUE) |>
  st_drop_geometry() |>
  transmute(record = str_remove(str_squish(clerk_docn), "^S"), zone_class) |>
  filter(!is.na(record), record != "") |>
  summarise(map_classes = paste(sort(unique(zone_class)), collapse = ";"), .by = record)

amendments <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  filter(outcome == "passed")
on_map <- amendments |>
  select(matter_id, record_numbers) |>
  tidyr::separate_longer_delim(record_numbers, ";") |>
  mutate(record = str_remove(record_numbers, "^S")) |>
  inner_join(map_records, by = "record", relationship = "many-to-one") |>
  summarise(map_classes = paste(sort(unique(unlist(strsplit(map_classes, ";")))), collapse = ";"), .by = matter_id)

elms_districts_vs_zoning_map <- amendments |>
  inner_join(on_map, by = "matter_id", relationship = "one-to-one") |>
  mutate(map_planned_development = str_detect(map_classes, "(^|;)PD"),
    agrees = case_when(
      direction == "to_planned_development" ~ map_planned_development,
      direction != "unknown" ~ mapply(function(to, map) any(strsplit(to, ";")[[1]] %in% strsplit(map, ";")[[1]]),
        coalesce(to_districts, ""), map_classes))) |>
  select(matter_id, record_number, filed_by_alderman, from_districts, to_districts, to_source, direction, map_classes,
    agrees)
SaveData(elms_districts_vs_zoning_map, "matter_id", "../output/elms_districts_vs_zoning_map.csv")
