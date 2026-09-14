# Recorded sale-parcel coordinates

The gzip archive preserves the existing 2006–2022 coordinate response, captured
unchanged on September 12, 2026. It contains parcel numbers, source years and
coordinates from Cook County's Parcel Universe endpoint:
https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json.

`../code/download_historical_sale_parcel_coordinates.R` records the original
query: the parcel/year pairs in the cleaned 2006–2022 sales, requesting longitude,
latitude and EPSG:3435 coordinates. The response covers that recorded query
population. Ordinary Make restores it; distance calculations remain downstream.
A different sales source or period may require additional queries.

`make download-current` queries the current sales population into a separate
`_current.csv` file and writes its report. Review that response before adopting
it as a replacement source. Decompression of the committed archive reproduces
the exact CSV bytes used before the reporting cleanup.
