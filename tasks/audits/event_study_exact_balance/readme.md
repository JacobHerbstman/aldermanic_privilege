# event_study_exact_balance

Purpose: Audit task that builds exact-distance parcel baselines for 2010 census blocks and uses them to compare treated and control blocks in the 2015 permit and home-sales event-study samples.

Produces: `output/block_parcel_baselines_2014.csv`, exact-balance summary tables, and redistricting sample-support tables.

Approximate runtime: 5-15 minutes, depending on whether the parcel-to-amenity baseline needs to be rebuilt from scratch.
