# Recorded Census inputs

These gzip files contain the unchanged Census extracts used by the current ward
controls. They were preserved from the existing task outputs on September 12,
2026. Decompression reproduces the exact original CSV and GeoPackage bytes.
The counts and geography are inputs; `create_ward_controls.R` still constructs
the ward controls from them on every required rebuild.

The source queries are recorded in `../code/download_census_controls.R`:
Cook County, Illinois; 2000 Census SF3 block groups, 2010 Census SF1 block-group
counts, 2010 and 2020 TIGER block-group polygons, and 2013–2022 ACS five-year
block-group estimates. The script lists the requested Census variable codes.
Publishers: https://api.census.gov/data.html and https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html.

Ordinary Make reads these recorded inputs. `make download-current` uses a
Census API key and writes separate `_current` files and their data reports.
Review a new extract before replacing these source archives.
