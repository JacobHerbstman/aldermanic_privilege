# New-construction analysis data

`build_new_construction_analysis_data.R` prepares the building-level data used by
the density tasks from `prepare_permit_construction/output/permit_construction.csv`.
It keeps buildings within 1,500 ft of a ward-pair boundary and attaches:

- the building's nearest 1,320-ft boundary segment on its ward pair;
- the aldermen of its own and the neighboring ward on the construction date, their
  through-2022 stringency scores, and the signed distance to the boundary
  (positive on the side of the higher-scoring alderman);
- the joint-service period: the era, ward pair and the two aldermen's terms;
- own-ward demographic controls for the construction year;
- the city permit ids behind each building, used by the leave-own-permits score check;
- the zoning group in force at construction, from the preserved zoning history
  (`construction_zoning_history`) and the official 2012, 2014, 2016 and 2025 maps.

Buildings are dated by their first permit's issue date, or June 15 of the reported
year built when no permit is linked. Chicago geometry is in EPSG:3435. A building
without a recorded alderman term or score keeps missing values and no signed
distance. Run `make` in `code/`.
