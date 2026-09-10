# Density boundary characteristics

This task recalculates the boundary and location measures for every project in
the connected construction analysis data. It replaces the old fixed project list.

`build_boundary_features.R` measures each existing boundary segment's overlap
with expressways, water, parks, cemeteries and arterials, using the original
30-metre corridor and recorded September 2025 OSM layers. It saves
`boundary_feature_measurements.csv`.

`build_density_boundary_characteristics.R` applies those measures to each
project's assigned segment. The simple restriction requires less than 50 percent
expressway or water overlap. The share-based restriction requires less than
50 percent physical-feature overlap, 40 percent expressway overlap, and
75 percent arterial overlap. The straightness check projects a 100-metre tangent
at the nearest ward-pair boundary point; each endpoint must lie within 15 metres
of that boundary. These reproduce the original analysis rules.

Distances to the downtown reference point (-87.6313, 41.8837), the nearest
2015 school, park and Lake Michigan shoreline use the finished project locations.
Lake shoreline simplification remains 50 feet. Spatial calculations use EPSG:3435.
The output contains one row per project and is consumed by density boundary checks.
Run `make` in `code/`; standard reports describe both saved datasets.
