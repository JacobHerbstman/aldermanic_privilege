# Rules shared by measure_buildings.R and build_construction_buildings.R.

# Settings shared by both scripts.
assessor_year_lead <- 2L         # years the Assessor's year built may precede the permit (many 2013 and 2016 permits)
max_build_lag_years <- 4L        # years from permit to reported year built (99.5 percent of measured buildings)
unit_tolerance <- 0.2            # share by which a multifamily building's units may differ from its permit
min_sqft_per_unit <- 300         # less floor area per unit is a shop-only record
max_land_sqft_per_unit <- 43560  # more land per unit (an acre) is development-wide land
min_land_sqft <- 100             # less land is a recording error
measurement_years <- 3L          # a building is measured on the record it holds most often in its first years

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

# Parcel succession: a condominium declaration or subdivision retires a parcel number and creates new ones. Each new
# parcel descends from the nearest older parcel on its tax block last assessed in the year before, or the year of, its
# first assessment. `parcels` has pin10, first_year, last_year, x_3435 and y_3435. Returns each ancestor with its
# descendants ("a/b/c"), following chains of successive divisions.
parcel_descendants <- function(parcels) {
  retired <- parcels |> filter(last_year < max(last_year))
  born <- parcels |> filter(first_year > min(first_year))
  children <- map(sort(unique(born$first_year)), \(year) {
    kids <- born |> filter(first_year == year)
    parents <- retired |> filter(last_year %in% c(year - 1, year), first_year < year)
    if (nrow(parents) == 0) return(NULL)
    nearest <- st_nearest_feature(st_as_sf(kids, coords = c("x_3435", "y_3435"), crs = 3435),
      st_as_sf(parents, coords = c("x_3435", "y_3435"), crs = 3435))
    tibble(ancestor = parents$pin10[nearest], descendant = kids$pin10)
  }) |> bind_rows() |> filter(substr(ancestor, 1, 7) == substr(descendant, 1, 7))
  # Each parcel has one parent, so walking up from every pair reaches all of its ancestors.
  pairs <- children
  step <- children
  repeat {
    step <- step |> inner_join(children |> rename(next_ancestor = ancestor), by = c("ancestor" = "descendant"),
      relationship = "many-to-one") |> transmute(ancestor = next_ancestor, descendant)
    if (nrow(step) == 0) break
    pairs <- bind_rows(pairs, step)
  }
  pairs |> distinct() |> group_by(ancestor) |> summarise(descendants = paste(sort(descendant), collapse = "/"), .groups = "drop")
}

# The assessment year whose measurement a building holds most often in its first `years` assessment years, the
# earliest when tied. First-year records are often partial: a building still under construction, duplicate cards, or
# a record before a condominium declaration. `signature` summarizes a year's measurement; a year without a complete
# measurement (NA) does not count.
stable_year <- function(tax_year, signature, years) {
  early <- tax_year < min(tax_year) + years & !is.na(signature)
  if (!any(early)) return(min(tax_year))
  counts <- table(signature[early])
  min(tax_year[early & signature %in% names(counts)[counts == max(counts)]])
}
