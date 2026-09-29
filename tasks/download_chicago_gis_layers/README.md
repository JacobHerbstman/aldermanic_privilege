# City of Chicago map layers for placing zoning amendments

`download_chicago_gis_layers.R` downloads three layers from the City of Chicago's data portal
(https://data.cityofchicago.org), saved as received in `output/`, and records each one's dataset, URL, file, bytes,
SHA-256 and retrieval date in `output/chicago_gis_layers.csv`. Run `make` in `code/`.

- `zoning_districts_current.geojson` (dataset dj47-wfun): the zoning districts in force. A district set by an
  amendment since about 2002 carries the amendment's application number (`ordinance`, "15599" or "A4818") and
  passage date (`ordinance_1`); 14,986 districts, of which about 100 were set in 2002, 280 in 2003 and 300 to 450 a
  year since. The source is live, so a rerun is a deliberate refresh.
- `zoning_districts_2012.zip` (dataset p8va-airx, "deprecated October 2014"): the zoning districts of November 2012,
  a shapefile (`Zoning_nov2012.shp`), with the same fields (`ORDINANCE_`, `ORDINANCE1`); it keeps districts amended
  again since, 3,344 of its 11,294 districts with an amendment's number.
- `street_centerlines_2013.zip` (dataset xy4z-b6aa, "deprecated July 2013"): the street centerlines, a shapefile
  (`Transportation.shp`, 55,747 segments, Illinois East state plane in feet) with each segment's direction, name and
  type, end nodes and address ranges. It is the latest centerline file the portal serves as data; the later versions
  are map views without data behind them.

Downloaded September 28, 2026. Consumers: `tasks/place_journal_zoning_amendments` (centerlines) and
`tasks/audits/zoning_record_validation` (zoning districts).
