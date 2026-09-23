# Rules shared by measure_buildings.R and build_construction_buildings.R.

# Commercial apartment valuations (2021 onward): every valuation reporting dwelling units, with its parcels.
# Hotel rooms, care beds and parking spaces are not dwelling units.
read_commercial_valuations <- function() {
  read_csv("../input/commercial_valuation_data.csv", col_types = cols(.default = col_character()),
      col_select = c(keypin, pins, year, class_es, tot_units, bldgsf, landsf, yearbuilt, property_type_use)) |>
    transmute(record_id = str_remove_all(keypin, "-"), year = as.numeric(year), classes = class_es,
      units = as.numeric(tot_units), building_sqft = as.numeric(bldgsf), land_sqft = as.numeric(landsf),
      year_built = as.numeric(yearbuilt), property_type_use,
      pin10s = map_chr(str_extract_all(str_remove_all(pins, "-"), "[0-9]{14}"), \(x) paste(sort(unique(substr(x, 1, 10))), collapse = "/"))) |>
    group_by(record_id) |>
    filter(!any(str_detect(str_to_upper(coalesce(property_type_use, "")), "HOTEL|MOTEL|NURSING|HOSP|HEALTH CARE|TREATMENT|PARKING|BOAT"))) |>
    ungroup() |> filter(units > 0) |> select(-property_type_use)
}

# Single-house permits must match the Assessor exactly; multifamily counts may differ by revisions.
units_agree <- function(units, permit_units, unit_tolerance) {
  if_else(permit_units <= 1L, units == permit_units, abs(units - permit_units) <= pmax(1, unit_tolerance * permit_units))
}
