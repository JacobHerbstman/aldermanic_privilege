# Recorded parcel searches

These files preserve the Cook County responses already used in the construction
review. They were copied byte for byte from this task's existing outputs on
September 12, 2026. They contain parcel geometry or coordinate history, not
hand-entered building measurements. Git records the saved source files.

- `reviewed_parcels_additional_2006.gpkg` through `2022.gpkg`: construction-year
  parcel searches, including empty responses. The request table is
  `../code/reviewed_parcel_queries.csv`; the year-to-layer table is the task's
  `input/historical_project_parcel_layers.csv`. The source is Cook County's
  `https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer`.
- `reviewed_predecessor_parcels.gpkg`: polygons intersecting the recorded points
  in `../code/reviewed_predecessor_queries.csv`, using the same historical service
  and EPSG:3435 coordinates.
- `reviewed_parcel_history.csv`: the 54 recorded rows for the three parcel numbers
  in `../code/reviewed_history_queries.csv`, years 1999–2025, from
  `https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json`.

Acquisition history and the original query scripts are preserved in
`tasks/audits/construction_review_history/source_acquisition/`. Ordinary
production reads and combines these saved responses. Adopting refreshed sources
requires comparing and updating the committed inputs deliberately.
