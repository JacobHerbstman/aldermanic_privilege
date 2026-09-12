# Historical Sale Coordinates

This task supplies recorded parcel coordinates from the Cook County Assessor's
historical parcel universe for the residential PIN-year observations that pass
the initial sales filters. The coordinates come from the sale year, so a later
parcel split or condominium conversion does not force the sale to use a current
parcel location.

The output is
`output/historical_sale_parcel_coordinates_2006_2022.csv`, with one row per
PIN and year.

Ordinary Make restores the recorded CSV from [`sources/`](sources/README.md).
`make download-current` queries the current sales population into a separate
file for comparison before replacing the recorded response.

During deliberate acquisition, requests run in batches of six, matching curl's per-host connection limit so
requests do not exhaust connection timeouts waiting inside the pool.
Unsuccessful requests are retried after the other
batches finish, for at most five attempts. Each request allows 120 seconds to
connect and 300 seconds overall. Successful requests are not repeated.
Any remaining failure stops the build before publishing the output; there is no
fallback to current-year locations or an incomplete download.
