# Block-Group Characteristics

This task creates the 2014 block-group characteristics used in the permit
balance table. Population density uses 2019 block-group geometry, matching the
submitted analysis. The output is `output/block_group_controls.csv`.

The Makefile restores the 2014 ACS counts and 2019 block-group polygons from
[`sources/`](sources/README.md). `create_block_group_controls.R` calculates the
controls and writes their report when saving the data. `make download-current`
runs the acquisition script with `CENSUS_API_KEY`, saving separate current
responses for review before adoption.
