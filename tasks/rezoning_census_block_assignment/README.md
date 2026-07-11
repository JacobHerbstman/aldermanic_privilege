# Rezoning Census-Block Assignment

Assigns each geocoded rezoning matter to a 2010 census block for the 2015 ward
redistricting event-study design.

## Inputs

- `tasks/rezoning_geocode_external_merge/output/rezoning_geocode_with_external_20101101_20201231.csv`
- `tasks/rezoning_geocode_stage1_parcel/output/rezoning_parcel_matches_20101101_20201231.csv`
- `tasks/rezoning_block_hand_adjudications/output/reviewed_rezoning_block_assignments.csv`
- `tasks/download_chicago_spatial_data/output/census_blocks_2010.csv`

## Output

- `tasks/rezoning_census_block_assignment/output/rezoning_census_blocks_20101101_20201231.csv`
- `tasks/rezoning_census_block_assignment/output/rezoning_matter_block_bridge_20101101_20201231.csv`

Longitude and latitude are interpreted in EPSG:4326 and transformed immediately
to EPSG:3435. The task requires unique block IDs, unique rezoning IDs, and exactly
one containing block for every valid coordinate. Matters without a defensible
point coordinate remain in the output with `block_assignment_status` equal to
`missing_coordinate`; they are not silently dropped or assigned to a guessed
block.

The first output preserves one representative block per matter. The bridge uses
all exact-address parcels and all matched parcels inside ordinance address ranges,
so a multi-parcel rezoning may correctly appear in more than one block. Matters
without exact parcel matches fall back to the reviewed representative point.
Nine unusual ordinances use committed ordinance-geometry reviews in place of a
point assignment.
