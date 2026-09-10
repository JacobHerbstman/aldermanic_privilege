# Historical Sale Coordinates

This task retrieves parcel coordinates from the Cook County Assessor's
historical parcel universe for the residential PIN-year observations that pass
the initial sales filters. The coordinates come from the sale year, so a later
parcel split or condominium conversion does not force the sale to use a current
parcel location.

The output is
`output/historical_sale_parcel_coordinates_2006_2022.csv`, with one row per
PIN and year.

Requests run in batches of 24. Unsuccessful requests are retried after the other
batches finish, for at most three attempts. Successful requests are not repeated.
Any remaining failure stops the build before publishing the output; there is no
fallback to current-year locations or an incomplete download.
