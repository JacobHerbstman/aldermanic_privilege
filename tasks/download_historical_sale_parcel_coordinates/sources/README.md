# Recorded sale-parcel coordinates

The gzip archive preserves the 2006–2022 coordinate response queried on
September 24, 2026, when the sales cleaning began following the school-closures
project's rules (county sale-quality flags without a separate deed-type
restriction; foreclosure auctions and transfers to lenders removed). That added
46,360 parcel-years; the 231,210 parcel-years shared with the September 12
response have identical coordinates. It contains parcel numbers, source years and
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
