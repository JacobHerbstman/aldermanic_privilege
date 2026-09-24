# Footprint floor area check

The Assessor records no floor area for condominium buildings of 20 or more units (the first of `SIZE_BREAKS`), so
their FAR cannot be measured. This audit asks whether 2022 footprint area times height could stand in for floor area.

Buildings in `prepare_permit_construction/output/permit_construction.csv` built through `LAST_CONSTRUCTION_YEAR`
are matched to the 2022 footprints (`tasks/download_building_footprints_2022`) containing their measured parcels'
centroids, ignoring structures lower than `MIN_HEIGHT_FT`; buildings sharing a footprint are not compared.
Floor area per cubic foot of footprint volume is calibrated on rentals with Assessor floor area, and for buildings of
20 or more units also on the other large rentals alone (leave one out). Errors are reported by size in units, split
at `SIZE_BREAKS`.

- `output/footprint_floor_area_buildings.csv`: each matched building, its Assessor and footprint floor area and error.
- `output/footprint_floor_area_summary.csv`: error by calibration, building type (condominium, rental,
  single_family) and size.
