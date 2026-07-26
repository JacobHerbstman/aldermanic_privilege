# Area-weighted block-group assignment audit

Production assigns each Census block group to the ward containing its centroid. This audit instead allocates each block group's counts across every intersecting ward in proportion to polygon area. Ward demographic shares are calculated from the allocated counts. Median household income is the household-weighted average of block-group medians after households are area-allocated.

Some historical ward polygons contain small overlapping slivers. Ward-specific intersections are therefore scaled to equal the block group's area inside the union of all ward polygons. This prevents overlapping ward geometry from allocating the same area twice while preserving the portion of a Cook County block group that lies within Chicago.

The exercise uses the same Census variables, years, ward maps, score estimator, and downstream specifications as production. It changes only the block-group-to-ward assignment rule. Area weighting assumes that residents and households are uniformly distributed within each block group, so it is a sensitivity check rather than an unambiguously superior measure.

Run `make` from `code/`. The main comparison is `output/area_weighted_main_results.csv`; score correlations and alderman-level movements are in the two accompanying score files.
The four-panel density display is `output/density_rd_area_weighted_4panel.pdf`.

## Results

The score is insensitive to the assignment rule. The centroid and area-weighted scores have correlations of 0.996 through 2014 and 0.999 through 2022. Their rank correlations are 0.990 and 0.998, respectively.

| Outcome | Centroid | Area-weighted |
| --- | ---: | ---: |
| Multifamily log FAR, continuous | -0.113 (0.077) | -0.136 (0.080) |
| Multifamily log DUPAC, continuous | -0.092 (0.090) | -0.161 (0.081) |
| Multifamily log FAR, binary | -0.258 (0.105) | -0.270 (0.103) |
| Multifamily log DUPAC, binary | -0.199 (0.112) | -0.238 (0.094) |
| High-discretion permit ITT | -0.102 (0.032) | -0.105 (0.032) |
| Rent, continuous | 0.0228 (0.0062) | 0.0229 (0.0063) |
| Sale price, continuous | 0.0173 (0.0071) | 0.0171 (0.0072) |

Sample sizes are identical under both assignment rules. Area weighting modestly strengthens the multifamily density estimates and has negligible effects on the permit and price estimates. The production centroid method is therefore not driving the paper's results. Area weighting is not necessarily preferable because it assumes population and households are uniformly distributed within each block group.
