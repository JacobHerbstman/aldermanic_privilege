# Parcel history

`download_parcel_history.R` downloads Cook County Clerk parcel polygons from
`gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer` and
saves `output/parcel_history.gpkg` (EPSG 3435) with its data report:

- layer 19, "Parcel History 2000-2023": one polygon per 10-digit parcel in the
  Clerk's maps of 2000–2023, with its first map year (`in_gis`) and last taxed
  year (`last_taxed`), for every one-mile tile holding a Chicago parcel centroid
  (`tasks/download_parcel_centroids`), so parcels in neighboring suburbs within
  those tiles are included;
- the yearly layers 2018–2025, for the Chicago parcels missing from layer 19.

The maps were not updated for 2011: one parcel has `in_gis` 2011 and three were
last taxed in 2010, so the 2010–2011 parcel changes carry 2012 map years. The
service's statistics endpoint reports years only through 2017; the features
themselves run through 2023. Each row is keyed by `layer` and the service's
`object_id`.

This is a live service: the Clerk may revise these maps, so a later run can
differ from the recorded report. Run `make` in `code/`.
