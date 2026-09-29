# Land prices at ward boundaries (exploratory, branch `explore-alderman-measures`)

Land prices capitalize the development an alderman is expected to allow, so unlike the density of what gets built
they do not depend on which projects go ahead. Run `make` in `code/`.

- `build_land_sales.R`: market sales of land in Chicago, 2006–2022, sold alone, passing the county's sale-quality
  flags, over $10,000, not by or to a government body or land bank, on lots of at least 1,000 square feet (2021 lot
  areas from `tasks/download_parcel_polygons_2021`). Two kinds: vacant parcels (Assessor class 100 in the year of
  sale), and teardowns, the last sale of a parcel in the two years before a demolition permit naming it (not
  demolitions of garages, sheds or other structures alone, and not the City's emergency demolitions of derelict
  buildings). Demolition permits name parcels only from about 2009, so teardowns start then. A teardown is
  redeveloped when a new-construction permit names the parcel within three years of the demolition (unknown for
  demolitions after 2019, whose three years run past the permit data). Each sale gets its
  zoning group on the sale date and, on the ward map in force, its ward, the nearest other ward, its distance to it
  and the aldermen serving both wards that day. 5,877 vacant-lot sales (1,841 within 500 ft of a boundary; median
  $16 per square foot) and 2,531 teardown sales (792 within 500 ft; median $125 per square foot), of which 1,447
  were redeveloped (437 within 500 ft), 786 were not (260) and 298 are unknown.
- `estimate_land_boundary.R`: log price per square foot across boundaries, within each boundary and pair of
  serving aldermen, with sale-year and zoning-group fixed effects, for vacant, teardown and all sales (with a
  fixed effect for the kind of sale), and for teardowns that were and were not redeveloped, by each alderman measure of
  `tasks/explore_alderman_measures`: per standard deviation by which a side's alderman is stricter (gap), and
  stricter against more lenient side (stricter_side). Repeated at placebo lines 500, 750 and 1,000 ft inside either
  ward. `plot_land_boundary.R` draws the stricter-side estimates within 500 ft for the processing-time index and the
  stall rate.
- `estimate_teardown_redevelopment.R`: whether teardowns on the stricter side are less often redeveloped (linear
  probability, same fixed effects).
- `estimate_land_remap.R`: vacant-lot and teardown sales of 2010–2020 in the permit event study's blocks near 2015 boundaries, before and
  after 2015, moved against unmoved blocks of the same ward pair, by the direction of the change in the 2006–2014
  index.

## Findings (September 28, 2026)

- **Vacant lots are not cheaper on the stricter side.** By the processing-time index the difference is 0.009 per
  standard deviation (SE 0.017) within 500 ft and 0.011 (SE 0.016) within 250 ft, which rules out differences
  larger than about 3–4 percent per standard deviation; stricter against more lenient side, 0.026 (SE 0.045). By the
  stall rate it is positive and within the range of the placebo lines.
- **Teardown land is about 8 percent cheaper on the index's stricter side.** Within 500 ft the stricter side's price
  per square foot is lower by 0.086 log points (SE 0.040; 792 sales), below all six placebo lines (−0.039 to
  +0.044); within 250 ft, −0.067 (SE 0.041). Dropping any one ward pair leaves it between −0.064 and −0.101; without
  any Ward 1 boundary it is −0.135 (SE 0.047), and without Moreno −0.113. Per standard deviation of the index gap it
  is weaker (−0.023, SE 0.017). By the stall rate the stricter side's teardowns are pricier, not cheaper (0.034,
  SE 0.045; by the stall rate away from boundaries 0.078 and, within 250 ft, 0.117, SE 0.043). Pooling both kinds
  of sale, no measure shows a difference.
- **The teardown discount is in teardowns not followed by a new building.** Among redeveloped teardowns the stricter
  side's price is no lower (−0.015, SE 0.043; 437 sales); among the others it is lower by 0.140 (SE 0.079; 260
  sales). Development value, which redevelopment realizes, is therefore not what is discounted. Nor are teardowns on
  the stricter side less often redeveloped: by the index the share is 5.7 points higher (SE 3.3) within 500 ft and
  1.9 points lower (SE 5.2) within 250 ft, and no measure gives a significant difference.
- **The remap design is too thin.** Only 173 of 979 sales fall in blocks the 2015 map moved; the estimate is −0.050
  (SE 0.149).
