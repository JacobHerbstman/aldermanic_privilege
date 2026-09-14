# Recorded block-group inputs

These gzip files preserve the task's existing 2014 ACS counts and 2019 block-group
geography, byte for byte, as captured on September 12, 2026. The ordinary build
reads these inputs and recalculates block-group controls in
`create_block_group_controls.R`.

`../code/download_block_group_controls.R` records the Cook County, Illinois
queries and Census variable codes. Publishers:
https://api.census.gov/data.html and
https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html.

`make download-current` writes newly acquired counts and polygons to separate
`_current` files, with reports. Compare those responses before replacing the
recorded source archives.
