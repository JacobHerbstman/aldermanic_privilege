# Historical Sale Coordinates

This task retrieves parcel coordinates from the Cook County Assessor's
historical parcel universe for the residential PIN-year observations that pass
the initial sales filters. The coordinates come from the sale year, so a later
parcel split or condominium conversion does not force the sale to use a current
parcel location.

The output is
`output/historical_sale_parcel_coordinates_2006_2022.csv`, with one row per
PIN and year.
