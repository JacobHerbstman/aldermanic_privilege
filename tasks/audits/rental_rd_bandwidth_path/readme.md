# rental_rd_bandwidth_path

Purpose: Plots listed-rent RD estimates across bandwidths from 100ft to 1000ft using the current cleaned rental panel and audited ward-boundary distances.

Inputs:
- `input/rent_with_ward_distances.parquet`: cleaned RentHub floorplan-month panel with corrected ward-pair distances, segment IDs, score side assignment, and location-quality flags.

Outputs:
- `output/rental_rd_bandwidth_path.csv`: coefficient path for the flat stricter-side rent jump.
- `output/rental_rd_bandwidth_path.pdf`: bandwidth-path plot for the main segment-by-month fixed-effects specification.
- `output/rental_rd_bandwidth_path.png`: rendered copy for quick inspection.
- `output/rental_rd_bandwidth_path_fe_specs.pdf`: hedonic-control bandwidth path comparing segment-by-month, ward-by-month, ward-pair-by-month, and segment-by-year plus month fixed effects.
- `output/rental_rd_bandwidth_path_fe_specs.png`: rendered copy of the fixed-effects comparison.

The main model matches `rental_rd`: log listed rent in 2022 dollars, segment-by-month fixed effects, standard errors clustered by segment, and optional hedonic controls for square footage, bedrooms, bathrooms, and building type. The fixed-effects comparison keeps the same outcome and clustering while varying the fixed-effects structure. The plot includes the all, clean-location, and no-questionable-address samples.
