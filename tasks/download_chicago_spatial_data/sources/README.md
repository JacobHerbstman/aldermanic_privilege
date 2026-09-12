# Recorded major streets

`major_streets.geojson.gz` preserves the existing major-street source, captured
byte for byte on September 12, 2026. Its query and transformation are recorded in
`../code/download_major_streets.R`. Boundary construction reads this street
geometry downstream.
The publisher is the Chicago Data Portal, dataset
https://data.cityofchicago.org/d/ueqs-5wr6.

Ordinary Make decompresses the recorded file. `make download-current` writes a
separate `major_streets_current.geojson` for comparison before source adoption.
